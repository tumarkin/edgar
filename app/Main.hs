import           Control.Monad.Writer       (Writer)
import           Options.Applicative
import           Options.Applicative.Simple

import           Edgar.Common
import qualified Edgar.Download             as Download
import qualified Edgar.Init                 as Init
import qualified Edgar.Update               as Update


data Command
    = Init Init.Config
    | Update Update.Config
    | Download Download.Config

commands ∷ ExceptT Command (Writer (Mod CommandFields Command)) ()
commands = do
    addCommand     "init"     "Initialize database" Init     Init.config
    addCommand     "update"   "Update form index"   Update   Update.config
    addSubCommands "download" "Download forms"      do
      addCommand "query" "Download documents satisfying specified conditions" Download Download.configQueryMode
      addCommand "id"    "Download documents with given ids"                  Download Download.configIdMode



main ∷ IO ()
main = do
    command <- snd <$> simpleOptions
                         "v0.1.3"
                         "edgar - an archiver for SEC corporate filings data"
                         "See --help for details"
                         (pure ())
                         commands
    case command of
      Init c     → Init.initDb c
      Update c   → Update.updateDbWithIndex c
      Download c → Download.download c

