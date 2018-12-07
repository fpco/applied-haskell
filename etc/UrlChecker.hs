#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE NoImplicitPrelude #-}
import RIO
import Conduit
import Control.Concurrent.STM.TBMQueue
import System.Environment
import Network.HTTP.Simple
import Network.HTTP.Client (parseUrlThrow)
import qualified RIO.Text as T
import Prelude (putStrLn)

withWorkers :: MonadUnliftIO m => Int -> ((m () -> m ()) -> m a) -> m a
withWorkers count inner = do
  queue <- liftIO $ newTBMQueueIO (count * 8)
  let schedule = atomically . writeTBMQueue queue
      worker = fix $ \loop -> do
        mnext <- atomically $ readTBMQueue queue
        case mnext of
          Nothing -> pure ()
          Just next -> next *> loop
  runConcurrently $
    Concurrently (replicateConcurrently_ count worker) *>
    Concurrently (inner schedule `finally` atomically (closeTBMQueue queue))

main :: IO ()
main = withWorkers 8 $ \schedule -> getArgs >>= mapM_ (goFile schedule)

goFile :: (IO () -> IO ()) -> FilePath -> IO ()
goFile schedule fp =
  withSourceFile fp $ \src ->
  runConduit $ src .| linesUnboundedAsciiC .| mapM_C goLine
  where
    goLine bs =
      case decodeUtf8' bs of
        Left e -> throwIO e
        Right text -> do
          req <- parseUrlThrow $ T.unpack text
          putStrLn $ show req
          void $ httpNoBody req
