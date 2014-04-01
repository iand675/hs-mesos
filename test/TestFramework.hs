{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Applicative
import Control.Lens
import Control.Monad
import qualified Data.ByteString.Char8 as C
import Data.IORef
import Data.Monoid ((<>))
import System.Exit
import System.Mesos.Resources
import System.Mesos.Scheduler
import System.Mesos.Types
import Text.Groom

cpusPerTask :: Double
cpusPerTask = 1

memPerTask :: Double
memPerTask = 32

requiredResources :: [Resource]
requiredResources = (cpusPerTask ^. re cpus) <> (memPerTask ^. re mem)

data TestScheduler = TestScheduler
  { role          :: C.ByteString
  , totalTasks    :: Int
  , tasksLaunched :: IORef Int
  , tasksFinished :: IORef Int
  }

executorInfo fid = ExecutorInfo
  { executorInfoExecutorID = ExecutorID "default"
  , executorInfoFrameworkID = fid
  , executorInfoCommandInfo = CommandInfo [] Nothing "/vagrant/hs-mesos/dist/build/test-executor/test-executor"
  , executorInfoResources = requiredResources
  , executorName = Just "Test Executor (Haskell)"
  , executorSource = Nothing
  , executorData = Nothing
  }

instance ToScheduler TestScheduler where
  registered _ _ _ _ = putStrLn "Registered!"

  resourceOffers s driver offers = do
    forM_ offers $ \offer -> do
      putStrLn ("Starting task on " <> show (offerHostname offer))
      status <- launchTasks
        driver
        [offerID offer]
        [ TaskInfo "Task foo" (TaskID "foo") (offerSlaveID offer) requiredResources
            (TaskExecutor $ executorInfo $ offerFrameworkID offer)
            Nothing
        ]
        (Filters Nothing)
      putStrLn "Launched tasks"
    return ()

  statusUpdate s driver status = do
    putStrLn $ "Task " <> show (taskStatusTaskID status) <> " is in state " <> show (taskStatusState status)
    putStrLn $ groom status
    when (taskStatusState status == Finished) $ do
      count <- atomicModifyIORef' (tasksFinished s) (\x -> let x' = succ x in (x', x'))
      when (count == totalTasks s) $ void $ do
        getLine
        stop driver False

  errorMessage _ _ message = C.putStrLn message

main = do
  role <- return "*" -- role to use when registering
  master <- return "127.0.1.1:5050" -- ip:port of master to connect to
  let info = (frameworkInfo "" "Test Framework (Haskell)") { frameworkRole = Just role }
  scheduler <- TestScheduler "master" 5 <$> newIORef 0 <*> newIORef 0
  status <- withSchedulerDriver scheduler info master Nothing $ \d -> do
    status <- run d
    -- Ensure that the driver process terminates
    stop d False
  if status /= Stopped
    then exitFailure
    else exitSuccess
