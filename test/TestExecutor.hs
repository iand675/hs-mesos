{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Control.Monad
import           Data.Monoid
import           System.Exit
import           System.Mesos.Executor
import           System.Mesos.TaskStatus
import           System.Mesos.Types

data TestExecutor = TestExecutor
instance ToExecutor TestExecutor where
  registered _ _ _ _ s = print $ "Registered executor on " <> slaveInfoHostname s
  reRegistered _ _ s = print $ "Re-registered executor on " <> slaveInfoHostname s
  launchTask _ d t = void $ do
    print $ "Starting task " <> fromTaskID (taskID t)
    sendStatusUpdate d $ taskStatus (taskID t) TaskRunning
    -- this is where one would perform the requested task
    sendStatusUpdate d $ taskStatus (taskID t) Finished

main = do
  r <- withExecutorDriver TestExecutor $ \e -> do
    run e
  if r == Stopped
    then exitSuccess
    else exitFailure
