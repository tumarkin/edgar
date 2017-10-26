{-# LANGUAGE LambdaCase        #-}

import           ClassyPrelude
import           Options.Applicative
import           Options.Applicative.Helper

import qualified Edgar.Init        as Init
import qualified Edgar.Update        as Update


data Command
  = Init Init.Config
  | Update Update.Config
  | Download

opts :: Options.Applicative.Parser Command
opts = subconcat
  [ command "init"   (infoHelper (Init   <$> Init.config)   (fpDesc "Initialize database"))
  , command "update" (infoHelper (Update <$> Update.config) (fpDesc "Update form index"))
  ]

main :: IO ()
main = 
  helperExecParser opts (fpDesc "edgar" <> header "edgar - an archiver for SEC corporate filings data")  >>= \case
    Init c   -> Init.initDb c
    Update c -> Update.updateDbWithIndex c
    _      -> error "Command not implemented"
