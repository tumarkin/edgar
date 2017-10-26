{-# LANGUAGE LambdaCase        #-}

import           ClassyPrelude
import qualified Edgar.Update        as Update
import           Options.Applicative
import           Options.Applicative.Helper

data Command
  = Init
  | Update Update.Config
  | Download

opts :: Options.Applicative.Parser Command
opts = subconcat
  [ command "update" (infoHelper (Update <$> Update.config) (fpDesc "Update form index"))
  ]

main :: IO ()
main = do
  helperExecParser opts (fpDesc "edgar")  >>= \case
    Update c -> Update.updateDbWithIndex c
    _      -> error "Command not implemented"
