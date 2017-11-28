{-# LANGUAGE DeriveGeneric #-}

module Edgar.Common
  ( 
  -- * Types
    EdgarForm(..)
  , YearQtr
  , yearQtr
  , year
  , qtr
  -- , Day

  -- * Hasql
  , connectTo
  , Connection

  , encodeEdgarForm

  -- * Optparse Applicative
  , textOption

  -- * Reexported modules
  , module X

  ) where

import Data.Char (isDigit)
import           ClassyPrelude as X
import           Data.Time.Calendar
import           Data.Time.Format
import           Hasql.Connection
import           Data.Csv
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Functor.Contravariant
import qualified Hasql.Decoders             as D
import qualified Hasql.Encoders             as E
import           Options.Applicative.Helper as X
import qualified Options.Applicative        as Opt
import Data.Ix
import GHC.Read
-- import Text.Parsec 
import Prelude (read)
import qualified Text.ParserCombinators.ReadP as RP
import qualified Text.ParserCombinators.ReadPrec as RP
import Text.ParserCombinators.Parsec hiding ((<|>))

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

textOption :: Opt.Mod Opt.OptionFields String -> Opt.Parser Text
textOption ms = pack <$> Opt.strOption ms


data YearQtr = YearQtr
  { year :: !Int
  , qtr  :: !Int
  } deriving (Show, Eq, Ord)

yearQtr :: Int -> Int -> YearQtr
yearQtr y q = 
    if | y < 1994      -> error "Edgar index starts in 1994"
       | q < 1 || q > 4 -> error "Quarter must be between 1 and 4"
       | otherwise     -> YearQtr y q

instance Read YearQtr where
  readsPrec _ s = [(parseYq s, "")]
  readPrec  = RP.lift readYqP

parseYq :: String -> YearQtr
parseYq s = 
  case Text.ParserCombinators.Parsec.runParser parseYq' () "YearQtr" s of
    Left  e -> error $ show e
    Right r -> r


parseYq' :: Text.ParserCombinators.Parsec.Parser YearQtr
parseYq' = do
  y <- many1 digit 
  char 'q' <|> char 'Q'
  q <- digit
  return $ yearQtr (read y) (read $ q : "")


readYqP :: RP.ReadP YearQtr
readYqP = do
  y <- RP.count 4 $ RP.satisfy isDigit
  _ <- RP.satisfy (\c -> c == 'q' || c == 'Q')
  q <- RP.satisfy isDigit
  _ <- RP.eof
  return $ yearQtr (read y) (read $ q : "")













  





instance Ix YearQtr where
  range   (s, e)   = map fromInt $ range (toInt s, toInt e)
  index   (s, e) y = Data.Ix.index (toInt s, toInt e) $ toInt y
  inRange (s, e) y = (s <= y && y <= e) || (s >= y && y >= e)



toInt :: YearQtr -> Int
toInt yq = year yq * 4 + qtr yq

fromInt :: Int -> YearQtr
fromInt i = YearQtr y m
  where

    m = if m' == 0 then 4 else m'
    y = if m' == 0 then y' - 1 else y'

    y' = quot i 4
    m' = mod i 4



