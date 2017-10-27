{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Edgar.Update
  ( updateDbWithIndex
  , opts
  , config
  , Config(..)
  )
  where

import           ClassyPrelude
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char                  (ord)
import           Data.Csv hiding (header)
import           Data.Functor.Contravariant
import qualified Hasql.Decoders             as D
import qualified Hasql.Encoders             as E
import           Hasql.Query
import           Hasql.Session
import           Network.HTTP.Simple
import           Options.Applicative

import Edgar.Common


updateDbWithIndex :: Config -> IO ()
updateDbWithIndex Config{..} = do
  i <- downloadIndex (year, qtr)
  let efs = case toEdgarForms i of
              Left e  -> error e
              Right r -> r

  c <- connectTo psql

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
insertQ = statement sql encodeEdgarForm D.unit True
  where
    sql     = "insert into forms (cik, company_name, form_type, date_filed, filename) values ($1, $2, $3, $4, $5)"

type Year    = Int
type Quarter = Int

-- Config
data Config = Config
  { year :: !Year
  , qtr :: !Quarter
  , psql :: !ByteString
  }


config :: Options.Applicative.Parser Config
config = Config
    <$> argument auto (metavar "YEAR")
    <*> argument auto (metavar "QUARTER")
    <*> option   auto (short 'p' <> long "postgres" <> value "postgresql://localhost/edgar" <> showDefault <> help "Postgres path")


opts :: ParserInfo Config
opts = info (config <**> helper)
  ( fullDesc
  <> progDesc "Download edgar index files into DB"
  <> header "edgar - an archiver for SEC corporate filings data" )

