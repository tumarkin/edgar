{-# LANGUAGE LambdaCase #-}

module Edgar.Common
  ( connectTo
  , Connection
  )
  where
import           ClassyPrelude
import           Hasql.Connection

connectTo :: ByteString -> IO Connection
connectTo b = acquire b >>= \case
  Left e  -> error "Unable to connect to database"
  Right c -> return c
