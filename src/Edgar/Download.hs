{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Edgar.Download
  ( download
  , opts
  , config
  , Config(..)
  )
  where

import           ClassyPrelude
-- import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as L8
-- import           Data.Char                  (ord)
-- import           Data.Csv hiding (header)
-- import           Data.Functor.Contravariant
-- import           Data.Time.Calendar
-- import           Data.Time.Format
import  System.FilePath
import  System.Directory
import qualified Hasql.Decoders             as D
import qualified Hasql.Encoders             as E
import           Hasql.Query
import           Hasql.Session
import           Network.HTTP.Simple
import           Options.Applicative

import Edgar.Common


download :: Config -> IO ()
download Config{..} = do
  c <- connectTo psql

  mapM_ (downloadAndSaveForm c dir) forms



downloadAndSaveForm :: Connection -> FilePath -> Int64 -> IO ()
downloadAndSaveForm conn basedir i = do
  ffn <- formFilename conn i
  
  source <- downloadUrl $ "https://www.sec.gov/Archives/" <> ffn 

  let localPath = basedir </> unpack ffn

  createDirectoryIfMissing True $ dropFileName localPath
  L8.writeFile localPath source


downloadUrl :: (MonadThrow m, MonadIO m) => Text -> m L8.ByteString
downloadUrl url = 
  getResponseBody <$> (httpLBS =<< parseRequest (unpack url))

formFilename :: Connection -> Int64 -> IO Text
formFilename conn i = run (query i formFilenameQ) conn >>= \case
  Left e -> error $ show e
  Right url -> return url


formFilenameQ :: Query Int64 Text
formFilenameQ = statement sql encoder decoder True
  where
    sql     = "select filename from forms where id = $1"
    encoder = E.value E.int8 
    decoder = D.singleRow (D.value D.text)





-- Config
data Config = Config
  { forms :: ![Int64]
  , psql  :: !ByteString
  , dir   :: !FilePath
  }


config :: Options.Applicative.Parser Config
config = Config
    <$> many (argument auto (metavar "FORM-ID"))
    <*> option   auto (short 'p' <> long "postgres" <> value "postgresql://localhost/edgar" <> showDefault <> help "Postgres path")
    <*> option   auto (short 'd' <> long "directory" <> value "." <> showDefault <> help "Archive root directory")


opts :: ParserInfo Config
opts = info (config <**> helper)
  ( fullDesc
  <> progDesc "Download edgar index files into DB"
  <> header "edgar - an archiver for SEC corporate filings data" )

