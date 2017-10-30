{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleContexts   #-}

module Edgar.Update
  ( updateDbWithIndex
  , opts
  , config
  , Config(..)
  )
  where

import           ClassyPrelude
import           Control.Monad.Trans.Resource
import           Control.Monad.State.Strict
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
import Control.Lens (makeLenses)
import Control.Lens.Setter ((+=))

import           Edgar.Common


-- Types
type UpdateM = StateT FormCounter (ResourceT IO)

data FormCounter = FormCounter 
  { _inserted  :: !Int
  , _malformed :: !Int
  , _duplicate :: !Int
  } deriving (Show)

makeLenses ''FormCounter

nullFormCounter :: FormCounter
nullFormCounter = FormCounter 0 0 0

type Year    = Int
type Quarter = Int

-- Application
updateDbWithIndex :: Config -> IO ()
updateDbWithIndex c@ Config{..} = do
    conn <- connectTo psql
    (_, fc) <- runResourceT $  runStateT (C.runConduit (myConduit c conn)) nullFormCounter
    putStrLn $ finalMsg fc
  where
    finalMsg FormCounter{..} = 
        tshow _inserted <> " new forms inserted into DB" <>
          if _duplicate > 0 
          then " (" <> tshow _duplicate <> " known forms found)" 
          else ""

myConduit :: Config -> Connection -> C.ConduitM a c UpdateM ()
myConduit c@Config{..} conn
    =  indexSourceC c
    .| C.ungzip
    .| C.lines
    .| dropHeaderC
    .| lazifyBSC
    .| toEdgarFormC
    .| storeFormC conn

indexSourceC :: Config -> C.Producer UpdateM ByteString
indexSourceC Config{..} =
    httpSource url getResponseBody
  where
    url = parseRequest_ $ "https://www.sec.gov/Archives/edgar/full-index/" <> show year <> "/QTR" <> show qtr <> "/master.gz"



dropHeaderC :: Conduit ByteString UpdateM ByteString
dropHeaderC = ignoreC 11

ignoreC :: Int -> Conduit ByteString UpdateM ByteString
ignoreC 0 = C.awaitForever C.yield
ignoreC i =
    C.await >>= \case
      Nothing -> return ()
      Just _  -> ignoreC (i-1)


lazifyBSC :: Conduit ByteString UpdateM L8.ByteString
lazifyBSC = C.awaitForever $ C.yield . L8.fromStrict

toEdgarFormC :: (MonadIO m, MonadState FormCounter m) 
              => Conduit L8.ByteString m EdgarForm
toEdgarFormC = C.awaitForever $ \bs ->
    case toEdgarForm bs of
      Left e   -> do
                  malformed += 1
                  liftIO . L8.putStrLn $ "Error reading form: " ++ bs
      Right ef -> C.yield ef

storeFormC :: Connection -> Consumer EdgarForm UpdateM ()
storeFormC conn = C.awaitForever $ \ef -> insertEdgarForm conn ef


toEdgarForm :: L8.ByteString -> Either String EdgarForm
toEdgarForm b = unsafeHead <$> decodeWith pipeDelimited NoHeader b

pipeDelimited :: DecodeOptions
pipeDelimited =
    defaultDecodeOptions{ decDelimiter = fromIntegral (ord '|')}

-- Database
insertEdgarForm :: (MonadIO m, MonadState FormCounter m) 
                 => Connection -> EdgarForm -> m ()
insertEdgarForm c ef = liftIO (run (query ef insertQ) c) >>= \case
  Left e  -> if isDuplicateError e
            then duplicate += 1
            else error $ show e
  Right _ -> inserted += 1


insertQ :: Query EdgarForm ()
insertQ = statement sql encodeEdgarForm D.unit True
  where
    sql     = "insert into forms (cik, company_name, form_type, date_filed, filename) values ($1, $2, $3, $4, $5)"

isDuplicateError :: Error -> Bool
isDuplicateError (ResultError (ServerError _ msg _ _)) = "duplicate key value" `isPrefixOf` msg
isDuplicateError _ = False

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

