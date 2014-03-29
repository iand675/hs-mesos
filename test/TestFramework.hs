{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Char8 as C
import Data.IORef
import Data.Monoid ((<>))
import System.Exit
import System.Mesos.Scheduler
import System.Mesos.Types

cpusPerTask :: Int
cpusPerTask = 1

memPerTask :: Int
memPerTask = 32

data TestScheduler = TestScheduler
  { role          :: C.ByteString
  , totalTasks    :: Int
  , tasksLaunched :: IORef Int
  , tasksFinished :: IORef Int
  }

instance ToScheduler TestScheduler where
  registered _ _ _ _ = putStrLn "Registered!"

  resourceOffers s driver offers = do
    forM_ offers $ \offer -> do
      putStrLn ("Starting task on " <> show (offerHostname offer))
      status <- launchTasks
        driver
        [offerID offer]
        [ TaskInfo "Task foo" (TaskID "foo") (offerSlaveID offer) (offerResources offer)
            Nothing
            (Just $ CommandInfo [] Nothing "echo hello")
            Nothing
        ]
        (Filters Nothing)
      print status
      putStrLn "Launched tasks"
    return ()

  statusUpdate s driver status = do
    putStrLn $ "Task " <> show (taskStatusTaskID status) <> " is in state " <> show (taskStatusState status)
    when (taskStatusState status == Finished) $ do
      count <- atomicModifyIORef' (tasksFinished s) (\x -> let x' = succ x in (x', x'))
      when (count == totalTasks s) $ void $ stop driver False

  errorMessage _ _ message = C.putStrLn message

frameworkInfo :: C.ByteString -> C.ByteString -> FrameworkInfo
frameworkInfo u n = FrameworkInfo u n Nothing Nothing Nothing Nothing Nothing

main = do
  role <- return "*" -- role to use when registering
  master <- return "127.0.1.1:5050" -- ip:port of master to connect to
  let info = (frameworkInfo "" "Test Framework (Haskell)") { frameworkRole = Just role }
  scheduler <- TestScheduler "master" 5 <$> newIORef 0 <*> newIORef 0
  status <- withSchedulerDriver scheduler info master Nothing $ \d -> do
    status <- run d
    stop d False
    return status
  if status /= Stopped
    then exitFailure
    else exitSuccess
