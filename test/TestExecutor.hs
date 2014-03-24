module Main where
import Data.Monoid
import System.Exit
import System.Mesos.Executor

data TestExecutor = TestExecutor
instance Executor TestExecutor where
  registered _ _ _ s = print $ "Registered executor on " <> hostname s
  reRegistered _ s = print $ "Re-registered executor on " <> hostname s
  launchTask d t = do
    print $ "Starting task " <> fromTaskID (taskID t)
    sendStatusUpdate d $ taskStatus (taskID t) TaskRunning
    -- this is where one would perform the requested task
    sendStatusUpdate d $ taskStatus (taskID t) Finished

main = do
  result <- withExecutor TestExecutor run
  if result == Stopped
    then exitSuccess
    else exitFailure
