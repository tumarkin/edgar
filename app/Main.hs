import           Options.Applicative
import           Options.Applicative.Helper

import qualified Edgar.Download             as Download
import qualified Edgar.Init                 as Init
import qualified Edgar.Update               as Update
import Edgar.Common


data Command
  = Init Init.Config
  | Update Update.Config
  | Download Download.Config

opts :: Options.Applicative.Parser Command
opts = subconcat
  [ command "init"     (infoHelper (Init     <$> Init.config)     (fpDesc "Initialize database"))
  , command "update"   (infoHelper (Update   <$> Update.config)   (fpDesc "Update form index"))
  , command "download" (infoHelper (Download <$> Download.config) (fpDesc "Download forms"))
  ]

main :: IO ()
main =
  helperExecParser opts (fpDesc "edgar" <> header "edgar - an archiver for SEC corporate filings data")  >>= \case
    Init c     -> Init.initDb c
    Update c   -> Update.updateDbWithIndex c
    Download c -> Download.download c
    -- _      -> error "Command not implemented"
