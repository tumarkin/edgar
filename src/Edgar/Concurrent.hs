module Edgar.Concurrent
  ( runConcurrent
  ) where


import ClassyPrelude
import Control.Concurrent (forkIO)
import System.Console.AsciiProgress


runConcurrent :: Int             -- | Number of processed to run
              -> (a -> IO ()) -- | Form processing function
              -> [a]         -- | List of forms to process
              -> IO ()
runConcurrent nConcurrent f forms = do
    queue <- newMVar forms
    dc    <- newMVar 0
    threadId <- spawnThreads nConcurrent f (queue, dc)

    renderProgressBarUntilComplete (length forms) dc

spawnThreads :: Int -> (a -> IO ()) -> (Queue a, ProgressCounter) -> IO [ThreadId]
spawnThreads nConcurrent f qc =
    replicateM nConcurrent $ forkIO (singleThread f qc)

singleThread :: (a -> IO ()) -- | Form processing function
             -> (Queue a, ProgressCounter) 
             -> IO ()
singleThread f (q, dc) =
  nextForm q >>= \case
    Nothing -> return ()
    Just nf -> f nf >> incrementCounter dc >> singleThread f (q, dc)

-- -- Asynchronous queue and counter
type Queue a         = MVar [a]
type ProgressCounter = MVar Integer -- A counter for the number downloaded and not yet included in progress bar


nextForm :: Queue a -> IO (Maybe a)
nextForm q = do
  queue <- takeMVar q
  case fromNullable queue of
    Nothing -> putMVar q queue >> return Nothing
    Just x  -> putMVar q (tail x) >> return (Just $ head x)

incrementCounter :: ProgressCounter -> IO ()
incrementCounter dc = modifyMVar_ dc (return . (+1))

renderProgressBarUntilComplete :: Int -> ProgressCounter -> IO ()
renderProgressBarUntilComplete nForms dc = displayConsoleRegions $ do
  pg <- newProgressBar def { pgWidth       = 100
                          , pgOnCompletion = Just "Done: :percent"
                          , pgTotal        = fromIntegral nForms
                          }
  loopProgressBar dc pg

loopProgressBar :: ProgressCounter -> ProgressBar -> IO ()
loopProgressBar dc pg =
  unlessM (isComplete pg) $ do
    n <- takeMVar dc
    putMVar dc 0
    tickNI pg n
    threadDelay $ 100 * 1000
    loopProgressBar dc pg



