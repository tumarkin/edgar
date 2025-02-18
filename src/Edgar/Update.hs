module Edgar.Update
  ( updateDbWithIndex
  , Config(..)
  )
  where

import           Control.Monad.Trans.Resource
import qualified Data.ByteString.Char8        as BS
import qualified Data.ByteString.Lazy.Char8   as BSL
import           Data.Conduit                 (ConduitT, (.|))
import qualified Data.Conduit                 as C
import qualified Data.Conduit.Binary          as C
import qualified Data.Conduit.Zlib            as C
import           Data.Csv                     hiding (header)
import qualified Data.Vector                  as Partial
import qualified Hasql.Decoders               as D
import qualified Hasql.Encoders               as E
import           Hasql.Session
import           Hasql.Statement
import           Lens.Micro.Platform          (makeLenses, (+=))
import           Network.HTTP.Simple


import           Edgar.Common



--------------------------------------------------------------------------------
-- Types                                                                      --
--------------------------------------------------------------------------------
type UpdateM = StateT FormCounter (ResourceT IO)

data FormCounter = FormCounter
  { _inserted  ∷ !Int
  , _malformed ∷ !Int
  , _duplicate ∷ !Int
  } deriving (Show)

makeLenses ''FormCounter

nullFormCounter ∷ FormCounter
nullFormCounter = FormCounter 0 0 0

type Year    = Int
type Quarter = Int

--------------------------------------------------------------------------------
-- Main functions                                                             --
--------------------------------------------------------------------------------
updateDbWithIndex ∷ Config → IO ()
updateDbWithIndex c@Config{..} =  do
    conn <- connectTo psql
    Edgar.Common.mapM_ (updateDbYearQtr c conn) [startYq..fromMaybe startYq endYq]



updateDbYearQtr ∷ Config → Connection → YearQtr → IO ()
updateDbYearQtr c conn yq = do
    putStrLn $ "Updating " <> show (year yq) <> " quarter " <> show (qtr yq) <> ".."
    (_, fc) <- runResourceT $  runStateT (C.runConduit (myConduit c conn yq)) nullFormCounter
    putStr "  "
    putStrLn $ finalMsg fc
  where
    finalMsg FormCounter{..} =
        show _inserted <> " new forms inserted into DB" <>
          if _duplicate > 0
          then " (" <> show _duplicate <> " known forms found)"
          else ""

myConduit ∷ Config → Connection → YearQtr → C.ConduitM a c UpdateM ()
myConduit c@Config{..} conn yq
    =  indexSourceC c yq
    .| C.ungzip
    .| C.lines
    .| dropHeaderC
    .| lazifyBSC
    .| toEdgarFormC
    .| storeFormC conn

indexSourceC ∷ Config → YearQtr → ConduitT i ByteString UpdateM ()
indexSourceC Config{..} yq =
    httpSource url' getResponseBody
  where
    url = parseRequest_ $ "https://www.sec.gov/Archives/edgar/full-index/" <> show (year yq) <> "/QTR" <> show (qtr yq) <> "/master.gz"
    url' = addRequestHeader "user-agent" (BS.pack emailAddress) url
    -- url' = ur


dropHeaderC ∷ ConduitT ByteString ByteString UpdateM ()
dropHeaderC = ignoreC 11

ignoreC ∷ Int → ConduitT ByteString ByteString UpdateM ()
ignoreC 0 = C.awaitForever C.yield
ignoreC i =
    C.await >>= \case
      Nothing → return ()
      Just _  → ignoreC (i-1)

lazifyBSC ∷ ConduitT ByteString LByteString UpdateM ()
lazifyBSC = C.awaitForever $ C.yield . toLazy

toEdgarFormC ∷ (MonadIO m, MonadState FormCounter m) => ConduitT LByteString EdgarForm m ()
toEdgarFormC = C.awaitForever yieldForm
  where
    yieldForm bs =
       case toEdgarForm (stripQuotes bs) of
         Left _   → do
                     malformed += 1
                     liftIO . BSL.putStrLn $ "Error reading form: " <> bs
                     pure ()
         Right ef → C.yield ef

    toEdgarForm b =
        Partial.head <$> decodeWith pipeDelimited NoHeader b

    stripQuotes = BSL.filter (/= '\"')

storeFormC ∷ Connection → ConduitT EdgarForm o UpdateM ()
storeFormC conn = C.awaitForever $ \ef → insertEdgarForm conn ef

pipeDelimited ∷ DecodeOptions
pipeDelimited =
    defaultDecodeOptions{ decDelimiter = fromIntegral (ord '|')}

--------------------------------------------------------------------------------
-- Database functions                                                         --
--------------------------------------------------------------------------------
insertEdgarForm ∷ (MonadIO m, MonadState FormCounter m)
                 => Connection → EdgarForm → m ()
insertEdgarForm c ef = liftIO (run (statement ef insertQ) c) >>= \case
  Left e → if | isDuplicateError e → duplicate += 1
              | isEnumError e      → addEnumFormType c (formType ef) >> insertEdgarForm c ef
              | otherwise          → error $ show e
  Right _ → inserted += 1


insertQ ∷ Statement EdgarForm ()
insertQ = Statement sql encodeEdgarForm D.noResult True
  where
    sql     = "insert into forms (cik, company_name, form_type, date_filed, filename) values ($1, $2, $3::form_type, $4, $5)"

isDuplicateError ∷ SessionError → Bool
isDuplicateError (QueryError _ _ (ResultError (ServerError _ msgBs _ _ _ ))) = "duplicate key value" `isPrefixOf` msg
  where msg = decodeUtf8 msgBs
isDuplicateError _ = False

isEnumError ∷ SessionError → Bool
isEnumError (QueryError _ _ (ResultError (ServerError _ msgBs _ _ _))) = "invalid input value for enum" `isPrefixOf` msg
  where msg = decodeUtf8 msgBs
isEnumError _ = False

addEnumFormType ∷ MonadIO m
                => Connection → Text → m ()
addEnumFormType c t =
  liftIO (run (statement () (formTypeQ t)) c) >>= \case
    Left e  → error $ show e
    Right _ → return () -- putStrLn "Forms table created."


formTypeQ ∷ Text → Statement () ()
formTypeQ t = Statement sql encoder decoder True
  where
    sql     = "alter type form_type add value '" <> encodeUtf8 t <> "'"
    encoder = E.noParams
    decoder = D.noResult

--------------------------------------------------------------------------------
-- Config and CLI                                                             --
--------------------------------------------------------------------------------
data Config = Config
  { startYq      ∷ !YearQtr
  , endYq        ∷ !(Maybe YearQtr)
  , psql         ∷ !String
  , emailAddress ∷ !String
  }


debug ∷ IO ()
debug = updateDbWithIndex conf
  where
    conf = Config (yearQtr 2011 4) Nothing "postgresql://localhost/edgar" "test_email@domainname.com"
