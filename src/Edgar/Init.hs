{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Edgar.Init
  ( initDb
  , opts
  , config
  , Config(..)
  )
  where

import           ClassyPrelude
import qualified Hasql.Decoders             as D
import qualified Hasql.Encoders             as E
import           Hasql.Query
import           Hasql.Session
import           Options.Applicative
import Options.Applicative.Helper

import Edgar.Common


initDb :: Config -> IO ()
initDb Config{..} = do
  c <- connectTo psql
  run (query () formTypeQ) c >>= \case
    Left e  -> error $ show e
    Right _ -> putStrLn "Enumerated form type created."

  run (query () initQ) c >>= \case
    Left e  -> error $ show e
    Right _ -> putStrLn "Forms table created."

-- Database
initQ :: Query () ()
initQ = statement sql encoder decoder True
  where
    sql     = "create table forms (" <> 
      "  id             serial primary key," <>
      "  cik            integer," <>
      "  company_name   text," <>
      "  form_type      form_type," <>
      "  date_filed     date," <>
      "  filename       text," <>
      "  unique (cik, company_name, form_type, date_filed, filename)" <>
      "  )"
    encoder = E.unit
    decoder = D.unit

formTypeQ :: Query () ()
formTypeQ = statement sql encoder decoder True
  where
    sql     = "create type form_type as enum ()"
    encoder = E.unit
    decoder = D.unit


-- Config
data Config = Config
  { psql :: !ByteString
  }


config :: Options.Applicative.Parser Config
config = Config
    <$> option auto (short 'p' <> long "postgres" <> value "postgresql://localhost/edgar" <> showDefault <> help "Postgres path")


opts :: ParserInfo Config
opts = infoHelper config (fpDesc "Download edgar index files into DB")

