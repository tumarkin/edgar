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
    addCommand     "init"     "Initialize database" Init     initConf
    addCommand     "update"   "Update form index"   Update   updateConf
    addSubCommands "download" "Download forms"      do
      addCommand "query" "Download documents satisfying specified conditions" Download downloadQueryMode
      addCommand "id"    "Download documents with given ids"                  Download downloadIdMode




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


--------------------------------------------------------------------------------
-- Command Parsers                                                            --
--------------------------------------------------------------------------------
initConf ∷ Parser Init.Config
initConf = Init.Config <$> postgres

updateConf ∷ Parser Update.Config
updateConf = Update.Config
    <$> argument auto (metavar "START"<> help "Start year quarter specified as YYYYqQ (e.g. 1999q1)")
    <*> optional (argument auto (metavar "END" <> help "End year quarter specified as YYYYqQ (OPTIONAL - Downloads only START when omitted)"))
    <*> postgres
    <*> userEmail

downloadQueryMode = downloadConfig queryMode
downloadIdMode    = downloadConfig idMode

idMode ∷ Parser Download.Mode
idMode = Download.IdMode <$> many (argument auto (metavar "FORM-ID"))

queryMode ∷ Parser Download.Mode
queryMode = Download.QueryMode <$> conditions


downloadConfig ∷ Parser Download.Mode → Parser Download.Config
downloadConfig modeParser = Download.Config
    <$> modeParser
    <*> postgres
    <*> option   auto (short 'd' <> long "directory"            <> value "." <> showDefault <> help "Archive root directory")
    <*> option   auto (short 'n' <> long "concurrent-downloads" <> value 4   <> showDefault <> help "Number of concurrent downloads")
    <*> userEmail
    <*> many     (strOption   (short 'z' <> long "zip-extension" <> metavar "EXT" <> help "Extensions of zipped files in archive"))
    <*> optional (option auto (short 'l' <> long "limit"                 <> metavar "INT" <> help "Limit to N forms"))


conditions ∷ Parser Download.Conditions
conditions = Download.Conditions
    <$> many     (option auto (short 'c' <> long "cik" <> metavar "INT" <> help "CIKs to download"))
    <*> many     (textOption  (short 'n' <> long "name" <> metavar "TEXT" <> help "Company names"))
    <*> many     (textOption  (short 't' <> long "form-type" <> metavar "TEXT" <> help "Form types to download"))
    <*> optional (option auto (short 's' <> long "start" <> metavar "DATE" <> help "Start date (YYYY-MM-DD)"))
    <*> optional (option auto (short 'e' <> long "end" <> metavar "DATE" <> help "End date (YYYY-MM-DD)"))


userEmail :: Parser String
userEmail = strOption (metavar "EMAIL" <> long "email" <> short 'e' <> help "User email address (required by Edgar)")

--------------------------------------------------------------------------------
-- Individual option  parsers                                                 --
--------------------------------------------------------------------------------
postgres ∷ Parser String
postgres = strOption (short 'p' <> long "postgres" <> value "postgresql://localhost/edgar" <> showDefault <> help "Postgres path postgresql::/username:password@host:port/database")

