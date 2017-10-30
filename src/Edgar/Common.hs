{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}

module Edgar.Common
  ( 
  -- * Types
    EdgarForm(..)
  , Day

  -- * Hasql
  , connectTo
  , Connection

  , encodeEdgarForm

  ) where

import           ClassyPrelude
import           Data.Time.Calendar
import           Data.Time.Format
import           Hasql.Connection
import           Data.Csv
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Functor.Contravariant
import qualified Hasql.Decoders             as D
import qualified Hasql.Encoders             as E

connectTo :: ByteString -> IO Connection
connectTo b = acquire b >>= \case
  Left e  -> error "Unable to connect to database"
  Right c -> return c


data EdgarForm = EdgarForm
  { cik         :: !Int64
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

encodeEdgarForm = contramap cik (E.value E.int8)
           <> contramap companyName (E.value E.text)
           <> contramap formType (E.value E.text)
           <> contramap dateFiled (E.value E.date)
           <> contramap filename (E.value E.text)
