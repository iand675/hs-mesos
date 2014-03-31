module System.Mesos.Types (
  SchedulerDriver,
  Scheduler,
  Status(..),
  FrameworkID(..),
  SlaveID(..),
  OfferID(..),
  TaskID(..),
  ExecutorID(..),
  ContainerID(..),
  FrameworkInfo(..),
  CommandURI(..),
  CommandInfo(..),
  ExecutorInfo(..),
  MasterInfo(..),
  SlaveInfo(..),
  Value(..),
  Resource(..),
  ResourceStatistics(..),
  ResourceUsage(..),
  Request(..),
  Offer(..),
  TaskInfo(..),
  TaskState(..),
  isTerminal,
  TaskStatus(..),
  Filters(..),
  Credential(..)
) where
import System.Mesos.Internal

isTerminal :: TaskState -> Bool
isTerminal s = case s of
  Finished -> True
  Failed -> True
  Killed -> True
  Lost -> True
  _ -> False
