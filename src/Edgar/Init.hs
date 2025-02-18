module Edgar.Init
  ( initDb
  , Config(..)
  )
  where

import qualified Hasql.Decoders             as D
import qualified Hasql.Encoders             as E
import           Hasql.Statement
import           Hasql.Session

import           Edgar.Common


initDb ∷ Config → IO ()
initDb Config{..} = do
  c <- connectTo psql
  run (statement () formTypeQ) c >>= \case
    Left e  → error $ show e
    Right _ → putStrLn "Enumerated form type created."

  run (statement () initQ) c >>= \case
    Left e  → error $ show e
    Right _ → putStrLn "Forms table created."

--------------------------------------------------------------------------------
-- Database queries                                                           --
--------------------------------------------------------------------------------
initQ ∷ Statement () ()
initQ = Statement sql encoder decoder True
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
    encoder = E.noParams
    decoder = D.noResult

formTypeQ ∷ Statement () ()
formTypeQ = Statement sql encoder decoder True
  where
    sql     = "create type form_type as enum ()"
    encoder = E.noParams
    decoder = D.noResult


--------------------------------------------------------------------------------
-- Config and CLI                                                             --
--------------------------------------------------------------------------------
newtype Config = Config {psql ∷ String }


