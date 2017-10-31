{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Edgar.Download
  ( download
  -- , opts
  , config
  , Config(..)
  )
  where

import           ClassyPrelude
-- import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as L8
-- import           Data.Char                  (ord)
-- import           Data.Csv hiding (header)
-- import           Data.Functor.Contravariant
import           Data.Time.Calendar
import           Data.Time.Format
import  System.FilePath
import  System.Directory
import qualified Hasql.Decoders             as D
import qualified Hasql.Encoders             as E
import           Hasql.Query
import           Hasql.Session
import           Network.HTTP.Simple
import           Options.Applicative
import           Options.Applicative.Helper

import Edgar.Common


download :: Config -> IO ()
download c@Config{..} = do
  conn <- connectTo psql

  forms <- case mode of
    QueryMode conditions -> getQueryQueue conn conditions
    IdMode    x -> return x

  mapM_ (downloadAndSaveForm conn dir) forms




downloadAndSaveForm :: Connection -> FilePath -> Int64 -> IO ()
downloadAndSaveForm conn basedir i = do
  ffn <- formFilename conn i
  
  source <- downloadUrl $ "https://www.sec.gov/Archives/" <> ffn 

  let localPath = basedir </> unpack ffn

  createDirectoryIfMissing True $ dropFileName localPath
  L8.writeFile localPath source


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


getQueryQueue :: Connection -> Conditions -> IO [Int64]
getQueryQueue conn cd@Conditions{..} = 
  run (query batchSize (queryQueueQ cd)) conn >>= \case
    Left e -> error $ show e
    Right r -> return r

queryQueueQ :: Conditions -> Query Int64 [Int64]
queryQueueQ Conditions{..} = statement sql encoder decoder True 
  where
    sql        = traceShow sql' $ textToBS sql'
    sql'       = "select id from forms where " 
                 ++ (unwords . intersperse "and" . catMaybes $ conditions) 
                 ++ " order by random() limit $1"

    conditions = [cikCond, conameCond, typeCond, startCond, endCond]

    cikCond    = ("cik " ++ )          <$> inConditionInt cik
    conameCond = ("company_name " ++ ) <$> inConditionText companyName
    typeCond   = ("form_type " ++ )    <$> inConditionText formType
    startCond  = ((++) "date_filed >= " . apostrophize True . pack . formatTime defaultTimeLocale "%F") <$> startDate
    endCond    = ((++) "date_filed <= " . apostrophize True . pack . formatTime defaultTimeLocale "%F") <$> endDate

    encoder = E.value E.int8
    decoder = D.rowsList (D.value D.int8)


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
  { mode        :: !Mode
  , psql        :: !ByteString
  , dir         :: !FilePath
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
  , batchSize   :: !Int64
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


idMode :: Options.Applicative.Parser Mode
idMode = IdMode <$> many (argument auto (metavar "FORM-ID"))

queryMode :: Options.Applicative.Parser Mode
queryMode = QueryMode <$> conditions

conditions :: Options.Applicative.Parser Conditions
conditions = Conditions
    <$> many (option auto (short 'c' <> long "cik" <> metavar "INT" <> help "CIKs to download"))
    <*> many (option auto (short 'n' <> long "name" <> metavar "TEXT" <> help "Company names"))
    <*> many (option auto (short 't' <> long "form-type" <> metavar "TEXT" <> help "Form types to download"))
    <*> optional (option auto (short 's' <> long "start" <> metavar "DATE" <> help "Start date (YYYY-MM-DD)"))
    <*> optional (option auto (short 'e' <> long "end" <> metavar "DATE" <> help "End date (YYYY-MM-DD)"))
    <*> option auto (short 'b' <> long "batch-size" <> value 1 <> showDefault <> metavar "Int" <> help "Number of forms to download")




