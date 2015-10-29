{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Control.Applicative
import           Control.Lens
import           Control.Monad
import qualified Data.ByteString.Char8  as C
import           Data.IORef
import           Data.Monoid            ((<>))
import           System.Exit
import           System.Mesos.Lens hiding (executorInfo)
import           System.Mesos.Resources
import           System.Mesos.Scheduler
import           System.Mesos.Types     hiding (commandInfo)
import qualified System.Mesos.Types     as T
-- import Text.Groom

cpusPerTask :: Double
cpusPerTask = 1

memPerTask :: Double
memPerTask = 32

requiredResources :: [Resource]
requiredResources = [cpusPerTask ^. re cpus, memPerTask ^. re mem]

data TestScheduler = TestScheduler
  { schedulerRole :: C.ByteString
  , totalTasks    :: Int
  , tasksLaunched :: IORef Int
  , tasksFinished :: IORef Int
  }

executorSettings fid = e & name ?~ "Test Executor (Haskell)"
  where e = executorInfo (ExecutorID "default") fid (T.commandInfo $ ShellCommand "/Users/ian/Code/personal/hs-mesos/dist/build/test-executor/test-executor") requiredResources

instance ToScheduler TestScheduler where
  registered _ _ _ _ = putStrLn "Registered!"

  resourceOffers s driver offers = do
    finishedCount <- readIORef $ tasksFinished s
    if totalTasks s == finishedCount
      then forM_ offers $ \offer -> declineOffer driver (offer^.id') filters
      else forM_ offers $ \offer -> do
             putStrLn ("Starting task on " <> show (offer^.hostname))
             status <- launchTasks
               driver
               [ offer^.id' ]
               [ taskInfo "Task foo" (TaskID "foo") (offer^.slaveId) requiredResources
                   (TaskExecutor $ executorSettings $ (offer^.frameworkId))
               ]
              (Filters Nothing)
             putStrLn "Launched tasks"
    return ()

  statusUpdate s driver status = do
    putStrLn $ "Task " <> show (status^.taskId) <> " is in state " <> show (status^.state)
    print status
    when ((status^.state) == Finished) $ do
      count <- atomicModifyIORef' (tasksFinished s) (\x -> let x' = succ x in (x', x'))
      when (count == totalTasks s) $ void $ do
        getLine
        stop driver False

  errorMessage _ _ message = C.putStrLn message

main = do
  r <- return "*" -- role to use when registering
  master <- return "127.0.0.1:5050" -- ip:port of master to connect to
  let info = (frameworkInfo "" "Test Framework (Haskell)") & role ?~ r
  scheduler <- TestScheduler "master" 5 <$> newIORef 0 <*> newIORef 0
  status <- withSchedulerDriver scheduler info master Nothing $ \d -> do
    status <- run d
    -- Ensure that the driver process terminates
    stop d False
  if status /= Stopped
    then exitFailure
    else exitSuccess
