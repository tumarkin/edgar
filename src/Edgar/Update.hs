{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE RankNTypes   #-}

module Edgar.Update
  ( updateDbWithIndex
  , opts
  , config
  , Config(..)
  )
  where

import           ClassyPrelude
import           Control.Monad.Trans.Resource
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy.Char8   as L8
import           Data.Char                    (ord)
import           Data.Conduit                 (Conduit, Producer, Consumer, (.|))
import qualified Data.Conduit                 as C
import qualified Data.Conduit.Binary          as C
import qualified Data.Conduit.Zlib            as C
import           Data.Csv                     hiding (header)
import           Data.Functor.Contravariant
import qualified Hasql.Decoders               as D
import qualified Hasql.Encoders               as E
import           Hasql.Query
import           Hasql.Session
import           Network.HTTP.Simple
import           Options.Applicative

import           Edgar.Common




updateDbWithIndex :: Config -> IO ()
updateDbWithIndex c@ Config{..} = do
  conn <- connectTo psql
  runResourceT $  C.runConduit (myConduit c conn)


myConduit :: Config -> Connection -> C.ConduitM a c (ResourceT IO) ()
myConduit c@Config{..} conn
    =  indexSourceC c
    .| C.ungzip
    .| C.lines
    .| dropHeaderC
    .| lazifyBSC
    .| toEdgarFormC
    .| storeFormC conn


indexSourceC :: Config -> C.Producer (ResourceT IO) ByteString
indexSourceC Config{..} =
    httpSource url getResponseBody
  where
    url = parseRequest_ $ "https://www.sec.gov/Archives/edgar/full-index/" <> show year <> "/QTR" <> show qtr <> "/master.gz"


dropHeaderC :: Conduit ByteString (ResourceT IO) ByteString
dropHeaderC = ignoreC 11

ignoreC :: Int -> Conduit ByteString (ResourceT IO) ByteString
ignoreC 0 = C.awaitForever C.yield
ignoreC i = do
    C.await >>= \case
      Nothing -> return ()
      Just _  -> ignoreC (i-1)


lazifyBSC :: Conduit ByteString (ResourceT IO) L8.ByteString
lazifyBSC = C.awaitForever $ C.yield . L8.fromStrict

toEdgarFormC :: Conduit L8.ByteString (ResourceT IO) EdgarForm
toEdgarFormC = C.awaitForever $ C.yield . toEdgarForm

storeFormC :: Connection -> Consumer EdgarForm (ResourceT IO) ()
storeFormC conn = C.awaitForever $ \ef -> liftIO (insertEdgarForm conn ef)








toEdgarForms :: L8.ByteString -> Either String (Vector EdgarForm)
toEdgarForms = decodeWith pipeDelimited NoHeader . dropHeader
  where
    dropHeader = L8.unlines . unsafeDrop 11 . L8.lines

toEdgarForm :: L8.ByteString -> EdgarForm
toEdgarForm b = 
  case decodeWith pipeDelimited NoHeader b of
    Left e  -> error e
    Right r -> unsafeHead r


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
  , qtr  :: !Quarter
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

