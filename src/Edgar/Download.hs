module Edgar.Download
  ( download
  , Config(..)
  , Mode(..)
  , Conditions(..)
  )
  where

import           Conduit                      (MonadThrow)
import           Control.Concurrent           (forkIO, modifyMVar_, threadDelay)
import qualified Hasql.Decoders               as D
import qualified Hasql.Encoders               as E
import           Hasql.Session
import           Hasql.Statement
import           Network.HTTP.Simple
import           Options.Applicative
import           System.Console.AsciiProgress

import           Edgar.Common
import           Edgar.Concurrent


download ∷ Config → IO ()
download c@Config{..} = do
    conn <- connectTo $ encodeUtf8 psql

    -- Get form filepaths
    forms <- case mode of
      QueryMode conditions →  getQueryQueue conn conditions
      IdMode    x          →  mapM (formFilename conn) x

    putStrLn $ "Requested forms: " <> show (length forms)

    forms' <- filterM (notDownloaded dir) forms

    let nToDownload = length forms'

    putStrLn $ "Not downloaded: " <> show nToDownload
    runConcurrent concurrentDLs (downloadAndSaveForm conn dir) forms'

downloadAndSaveForm ∷ Connection → FilePath → Text → IO ()
downloadAndSaveForm conn basedir ffn = do
    source <- downloadUrl $ "https://www.sec.gov/Archives/" <> ffn
    createDirectoryIfMissing True $ dropFileName localPath
    writeFileLBS localPath source
  where
    localPath = basedir </> toString ffn

notDownloaded ∷ FilePath → Text → IO Bool
notDownloaded basedir ffn = not <$> doesFileExist (basedir </> toString ffn)

--------------------------------------------------------------------------------
-- Database functions                                                         --
--------------------------------------------------------------------------------
downloadUrl ∷ (MonadIO m, MonadThrow m) => Text → m LByteString
downloadUrl url =
    getResponseBody <$> (httpLBS =<< parseRequest (toString url))

formFilename ∷ Connection → Int64 → IO Text
formFilename conn i = run (statement i formFilenameQ) conn >>= \case
    Left e → error $ show e
    Right url → return url
  where
    formFilenameQ ∷ Statement Int64 Text
    formFilenameQ = Statement sql encoder decoder True

    sql     = "select filename from forms where id = $1"
    encoder = E.param $ E.nonNullable E.int8
    decoder = D.singleRow (D.column $ D.nonNullable D.text)


getQueryQueue ∷ Connection → Conditions → IO [Text]
getQueryQueue conn cd@Conditions{..} =
    run (statement () (queryQueueQ cd)) conn >>= \case
      Left e → error $ show e
      Right r → return r
  where
    queryQueueQ ∷ Conditions → Statement () [Text]
    queryQueueQ Conditions{..} = Statement (encodeUtf8 sql) encoder decoder True

    sql       = "select filename from forms where "
                 <> (unwords . intersperse "and" . catMaybes $ conditions)
                 <> " order by random()"

    conditions = [cikCond, conameCond, typeCond, startCond, endCond]

    cikCond    = ("cik " <> )          <$> inConditionInt cik
    conameCond = ("company_name " <> ) <$> inConditionText companyName
    typeCond   = ("form_type " <> )    <$> inConditionText formType
    startCond  = (<>) "date_filed >= " . apostrophize True . toText . formatTime defaultTimeLocale "%F" <$> startDate
    endCond    = (<>) "date_filed <= " . apostrophize True . toText . formatTime defaultTimeLocale "%F" <$> endDate

    encoder = E.noParams
    decoder = D.rowList (D.column $ D.nonNullable D.text)


inConditionInt ∷ [Int64] → Maybe Text
inConditionInt is = inConditionGen False (map show is)

inConditionText ∷ [Text] → Maybe Text
inConditionText = inConditionGen True

inConditionGen ∷ Bool → [Text] → Maybe Text
inConditionGen _ [] = Nothing
inConditionGen addAppos as = Just . ("in " <> ) . parenthesize . unwords . intersperse "," . map (apostrophize addAppos) $ as

parenthesize ∷ Text → Text
parenthesize x  = "(" <> x <> ")"

apostrophize ∷ Bool → Text → Text
apostrophize True  x = "'" <> x <> "'"
apostrophize False x = x

--------------------------------------------------------------------------------
-- Config and CLI                                                             --
--------------------------------------------------------------------------------
data Config = Config
  { mode          ∷ !Mode
  , psql          ∷ !String
  , dir           ∷ !FilePath
  , concurrentDLs ∷ !Int
  }

data Mode
  = QueryMode Conditions
  | IdMode [Int64]

data Conditions = Conditions
  { cik         ∷ ![Int64]
  , companyName ∷ ![Text]
  , formType    ∷ ![Text]
  , startDate   ∷ !(Maybe Day)
  , endDate     ∷ !(Maybe Day)
  }


