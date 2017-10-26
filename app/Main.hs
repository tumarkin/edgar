{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           ClassyPrelude
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char                  (ord)
import           Data.Csv hiding (header)
import           Data.Functor.Contravariant
import           Data.Time.Calendar
import           Data.Time.Format
import           Hasql.Connection
import qualified Hasql.Decoders             as D
import qualified Hasql.Encoders             as E
import           Hasql.Query
import           Hasql.Session
import           Network.HTTP.Simple
import           Options.Applicative

main :: IO ()
main = do
  Config{..} <- execParser opts
  updateDbWithIndex (year, qtr)
  


updateDbWithIndex :: (Year, Quarter) -> IO ()
updateDbWithIndex yq = do
  i <- downloadIndex yq
  let efs = case toEdgarForms i of
                Left e  -> error e
                Right r -> r

  c <- connectTo "postgresql://localhost/edgar"

  mapM_ (insertEdgarForm c) efs 



toEdgarForms :: L8.ByteString -> Either String (Vector EdgarForm)
toEdgarForms = decodeWith pipeDelimited NoHeader . dropHeader
  where
    dropHeader = L8.unlines . unsafeDrop 11 . L8.lines

pipeDelimited :: DecodeOptions
pipeDelimited =
    defaultDecodeOptions{ decDelimiter = fromIntegral (ord '|')}

downloadIndex :: (MonadThrow m, MonadIO m) => (Year, Quarter) -> m L8.ByteString
downloadIndex (y, q) =
    getResponseBody <$> (httpLBS =<< parseRequest url)
  where
    url = "https://www.sec.gov/Archives/edgar/full-index/" <> show y <> "/QTR" <> show q <> "/master.idx"

-- Database
insertEdgarForm :: Connection -> EdgarForm -> IO ()
insertEdgarForm c ef = run (query ef insertQ) c >>= \case
  Left e  -> error $ show e
  Right _ -> return ()

insertQ :: Query EdgarForm ()
insertQ = statement sql encoder decoder True
  where
    sql     = "insert into forms (cik, company_name, form_type, date_filed, filename) values ($1, $2, $3, $4, $5)"
    encoder = contramap cik (E.value E.text)
           <> contramap companyName (E.value E.text)
           <> contramap formType (E.value E.text)
           <> contramap dateFiled (E.value E.date)
           <> contramap filename (E.value E.text)
    decoder = D.unit

connectTo :: ByteString -> IO Connection
connectTo b = acquire b >>= \case
  Left e  -> error "Unable to connect to database"
  Right c -> return c



data EdgarForm = EdgarForm
  { cik         :: !Text
  , companyName :: !Text
  , formType    :: !Text
  , dateFiled   :: !Day
  , filename    :: !Text
  } deriving (Generic, Show)

instance FromRecord EdgarForm
instance ToRecord   EdgarForm

instance ToField Day where
  toField = error "Day is not intended to be converted to a CSV field"

instance FromField Day where
  -- parseField :: ByteString -> Parser Day
  parseField = parseTimeM True defaultTimeLocale "%Y-%m-%d" . L8.unpack . L8.fromStrict

type Year = Int
type Quarter = Int

-- Config
data Config = Config
  { year :: !Int
  , qtr :: !Int
  , psql :: !String
  }


config :: Options.Applicative.Parser Config
config = Config
    <$> argument auto (metavar "YEAR")
    <*> argument auto (metavar "QUARTER")
    <*> strOption ( short 'p' <> long "postgres" <> value "postgresql://localhost/edgar" <> showDefault <> help "Postgres path")


opts :: ParserInfo Config
opts = info (config <**> helper)
  ( fullDesc
  <> progDesc "Download edgar index files into DB"
  <> header "edgar - an archiver for SEC corporate filings data" )

