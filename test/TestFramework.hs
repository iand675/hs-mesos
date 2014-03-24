module Main where
import System.Mesos.Scheduler

cpusPerTask :: Int
cpusPerTask = 1

memPerTask :: Int
memPerTask = 32

data TestScheduler = TestScheduler { role :: ByteString }

instance Scheduler TestScheduler where
  registered _ _ _ = putStrLn "Registered!"
  resourceOffers driver offers = do
    putStr "."
    forM_ offers $ \offer -> do
      let resources = parseResource ("cpus:" <> show cpusPerTask <> ";mem:" <> show memPerTask)
  statusUpdate driver status = do
    print $ "Task " <> taskID status <> " is in state " <> taskState status
    if (taskState status == Finished)
      then tasksFinished++
      else return()
    if (tasksFinished == totalTasks)
      then return ()
      else stop driver
  errorMessage _ m = putStrLn m

