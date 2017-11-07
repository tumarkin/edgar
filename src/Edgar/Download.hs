{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

module Edgar.Download
  ( download
  , config
  , Config(..)
  )
  where

import           Control.Concurrent           (forkIO)
import qualified Data.ByteString.Lazy.Char8   as L8
import qualified Hasql.Decoders               as D
import qualified Hasql.Encoders               as E
import           Hasql.Query
import           Hasql.Session
import           Network.HTTP.Simple
import           Options.Applicative
import           System.Console.AsciiProgress
import           System.Directory
import           System.FilePath

import           Edgar.Common


download :: Config -> IO ()
download c@Config{..} = do
  conn <- connectTo psql

  -- Get form filepaths
  forms <- case mode of
    QueryMode conditions ->  getQueryQueue conn conditions
    IdMode    x          ->  mapM (formFilename conn) x

  putStrLn $ "Requested forms: " <> tshow (length forms)

  forms' <- filterM (notDownloaded dir) forms

  let nToDownload = length forms'

  putStrLn $ "Not downloaded: " <> tshow nToDownload

  queue <- newMVar forms'
  dc    <- newMVar 0

  threadId <- spawnThreads concurrentDLs (conn, dir, queue, dc)

  renderProgressBarUntilComplete nToDownload dc

spawnThreads :: Int -> (Connection, FilePath, Queue, DownloadCounter) -> IO [ThreadId]
spawnThreads n (conn, basedir, q, dc) =
  replicateM n $ forkIO (downloadThread conn basedir q dc)

downloadThread :: Connection -> FilePath -> Queue -> DownloadCounter -> IO ()
downloadThread conn basedir q dc =
  nextForm q >>= \case
    Nothing -> return ()
    Just nf -> downloadAndSaveForm conn basedir nf
              >> incrementCounter dc
              >> downloadThread conn basedir q dc

downloadAndSaveForm :: Connection -> FilePath -> Text -> IO ()
downloadAndSaveForm conn basedir ffn = do
  source <- downloadUrl $ "https://www.sec.gov/Archives/" <> ffn

  let localPath = basedir </> unpack ffn

  createDirectoryIfMissing True $ dropFileName localPath
  L8.writeFile localPath source

notDownloaded :: FilePath -> Text -> IO Bool
notDownloaded basedir ffn = not <$> doesFileExist (basedir </> unpack ffn)

-- Asynchronous queue and counter
type Queue           = MVar [Text]
type DownloadCounter = MVar Integer -- A counter for the number downloaded and not yet included in progress bar


nextForm :: Queue -> IO (Maybe Text)
nextForm q = do
  queue <- takeMVar q
  case fromNullable queue of
    Nothing -> putMVar q queue >> return Nothing
    Just x  -> putMVar q (tail x) >> return (Just $ head x)

incrementCounter :: DownloadCounter -> IO ()
incrementCounter dc = modifyMVar_ dc (return . (+1))

renderProgressBarUntilComplete :: Int -> DownloadCounter -> IO ()
renderProgressBarUntilComplete nForms dc = displayConsoleRegions $ do
  pg <- newProgressBar def { pgWidth       = 100
                          , pgOnCompletion = Just "Done: :percent"
                          , pgTotal        = fromIntegral nForms
                          }
  loopProgressBar dc pg

loopProgressBar :: DownloadCounter -> ProgressBar -> IO ()
loopProgressBar dc pg =
  unlessM (isComplete pg) $ do
    n <- takeMVar dc
    putMVar dc 0
    tickNI pg n
    threadDelay $ 100 * 1000
    loopProgressBar dc pg


-- Database functions
downloadUrl :: (MonadThrow m, MonadIO m) => Text -> m L8.ByteString
downloadUrl url =
  getResponseBody <$> (httpLBS =<< parseRequest (unpack url))

formFilename :: Connection -> Int64 -> IO Text
formFilename conn i = run (query i formFilenameQ) conn >>= \case
  Left e -> error $ show e
  Right url -> return url


formFilenameQ :: Query Int64 Text
formFilenameQ = statement sql encoder decoder True
  where
    sql     = "select filename from forms where id = $1"
    encoder = E.value E.int8
    decoder = D.singleRow (D.value D.text)


getQueryQueue :: Connection -> Conditions -> IO [Text]
getQueryQueue conn cd@Conditions{..} =
  run (query () (queryQueueQ cd)) conn >>= \case
    Left e -> error $ show e
    Right r -> return r

queryQueueQ :: Conditions -> Query () [Text]
queryQueueQ Conditions{..} = statement (textToBS sql) encoder decoder True
  where
    sql       = "select filename from forms where "
                 ++ (unwords . intersperse "and" . catMaybes $ conditions)
                 ++ " order by random()"

    conditions = [cikCond, conameCond, typeCond, startCond, endCond]

    cikCond    = ("cik " ++ )          <$> inConditionInt cik
    conameCond = ("company_name " ++ ) <$> inConditionText companyName
    typeCond   = ("form_type " ++ )    <$> inConditionText formType
    startCond  = ((++) "date_filed >= " . apostrophize True . pack . formatTime defaultTimeLocale "%F") <$> startDate
    endCond    = ((++) "date_filed <= " . apostrophize True . pack . formatTime defaultTimeLocale "%F") <$> endDate

    encoder = E.unit
    decoder = D.rowsList (D.value D.text)


inConditionInt :: [Int64] -> Maybe Text
inConditionInt is = inConditionGen False (map tshow is)

inConditionText :: [Text] -> Maybe Text
inConditionText = inConditionGen True

inConditionGen :: Bool -> [Text] -> Maybe Text
inConditionGen _ [] = Nothing
inConditionGen addAppos as = Just . ("in " ++ ) . parenthesize . unwords . intersperse "," . map (apostrophize addAppos) $ as

parenthesize :: Text -> Text
parenthesize x  = "(" <> x <> ")"

apostrophize :: Bool -> Text -> Text
apostrophize True  x = "'" <> x <> "'"
apostrophize False x = x

textToBS :: Text -> ByteString
textToBS = L8.toStrict . L8.pack . unpack


-- Config
data Config = Config
  { mode          :: !Mode
  , psql          :: !ByteString
  , dir           :: !FilePath
  , concurrentDLs :: !Int
  }

data Mode
  = QueryMode Conditions
  | IdMode [Int64]

data Conditions = Conditions
  { cik         :: ![Int64]
  , companyName :: ![Text]
  , formType    :: ![Text]
  , startDate   :: !(Maybe Day)
  , endDate     :: !(Maybe Day)
  }

config = subconcat
  [ command "query" (infoHelper (config' queryMode) (fpDesc "Download documents satisfying specified conditions"))
  , command "id"    (infoHelper (config' idMode)    (fpDesc "Download documents with given ids"))
  ]

config' :: Options.Applicative.Parser Mode -> Options.Applicative.Parser Config
config' m = Config
    <$> m
    <*> option   auto (short 'p' <> long "postgres" <> value "postgresql://localhost/edgar" <> showDefault <> help "Postgres path")
    <*> option   auto (short 'd' <> long "directory" <> value "." <> showDefault <> help "Archive root directory")
    <*> option   auto (short 'n' <> value 4 <> showDefault <> help "Number of concurrent downloads")


idMode :: Options.Applicative.Parser Mode
idMode = IdMode <$> many (argument auto (metavar "FORM-ID"))

queryMode :: Options.Applicative.Parser Mode
queryMode = QueryMode <$> conditions

conditions :: Options.Applicative.Parser Conditions
conditions = Conditions
    <$> many     (option auto (short 'c' <> long "cik" <> metavar "INT" <> help "CIKs to download"))
    <*> many     (textOption  (short 'n' <> long "name" <> metavar "TEXT" <> help "Company names"))
    <*> many     (textOption  (short 't' <> long "form-type" <> metavar "TEXT" <> help "Form types to download"))
    <*> optional (option auto (short 's' <> long "start" <> metavar "DATE" <> help "Start date (YYYY-MM-DD)"))
    <*> optional (option auto (short 'e' <> long "end" <> metavar "DATE" <> help "End date (YYYY-MM-DD)"))




