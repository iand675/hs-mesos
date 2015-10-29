module System.Mesos.TaskStatus where
import           System.Mesos.Types

taskStatus :: TaskID -> TaskState -> TaskStatus
taskStatus tid ts = TaskStatus tid ts Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
