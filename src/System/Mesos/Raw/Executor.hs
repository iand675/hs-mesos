module System.Mesos.Raw.Executor where
import           System.Mesos.Internal
import           System.Mesos.Raw.ExecutorId
import           System.Mesos.Raw.ExecutorInfo
import           System.Mesos.Raw.FrameworkId
import           System.Mesos.Raw.FrameworkInfo
import           System.Mesos.Raw.SlaveId
import           System.Mesos.Raw.SlaveInfo
import           System.Mesos.Raw.TaskId
import           System.Mesos.Raw.TaskInfo
import           System.Mesos.Raw.TaskStatus

type ExecutorPtr = Ptr Executor

-- | A data structure of the underlying executor & the callbacks that are triggered via the Mesos C++ API.
data Executor = Executor
  { executorImpl                :: Ptr Executor
  , rawExecutorRegistered       :: FunPtr RawExecutorRegistered
  , rawExecutorReRegistered     :: FunPtr RawExecutorReRegistered
  , rawExecutorDisconnected     :: FunPtr RawExecutorDisconnected
  , rawExecutorLaunchTask       :: FunPtr RawExecutorLaunchTask
  , rawExecutorTaskKilled       :: FunPtr RawExecutorTaskKilled
  , rawExecutorFrameworkMessage :: FunPtr RawExecutorFrameworkMessage
  , rawExecutorShutdown         :: FunPtr RawExecutorShutdown
  , rawExecutorErrorCallback    :: FunPtr RawExecutorError
  }

type ExecutorDriverPtr = Ptr ExecutorDriver

-- | A handle that allows an Executor to trigger lifecycle & status update events (e.g. starting & stopping the executor and sending messages to the Scheduler that invoked the executor).
newtype ExecutorDriver = ExecutorDriver { fromExecutorDriver :: ExecutorDriverPtr }

type RawExecutorRegistered = ExecutorDriverPtr -> ExecutorInfoPtr -> FrameworkInfoPtr -> SlaveInfoPtr -> IO ()

type RawExecutorReRegistered = ExecutorDriverPtr -> SlaveInfoPtr -> IO ()

type RawExecutorDisconnected = ExecutorDriverPtr -> IO ()

type RawExecutorLaunchTask = ExecutorDriverPtr -> TaskInfoPtr -> IO ()

type RawExecutorTaskKilled = ExecutorDriverPtr -> TaskIDPtr -> IO ()

type RawExecutorFrameworkMessage = ExecutorDriverPtr -> Ptr CChar -> CInt -> IO ()

type RawExecutorShutdown = ExecutorDriverPtr -> IO ()

type RawExecutorError = ExecutorDriverPtr -> Ptr CChar -> CInt -> IO ()

foreign import ccall "wrapper" wrapExecutorRegistered
  :: RawExecutorRegistered
  -> IO (FunPtr RawExecutorRegistered)

foreign import ccall "wrapper" wrapExecutorReRegistered
  :: RawExecutorReRegistered
  -> IO (FunPtr RawExecutorReRegistered)

foreign import ccall "wrapper" wrapExecutorDisconnected
  :: RawExecutorDisconnected
  -> IO (FunPtr RawExecutorDisconnected)

foreign import ccall "wrapper" wrapExecutorLaunchTask
  :: RawExecutorLaunchTask
  -> IO (FunPtr RawExecutorLaunchTask)

foreign import ccall "wrapper" wrapExecutorTaskKilled
  :: RawExecutorTaskKilled
  -> IO (FunPtr RawExecutorTaskKilled)

foreign import ccall "wrapper" wrapExecutorFrameworkMessage
  :: RawExecutorFrameworkMessage
  -> IO (FunPtr RawExecutorFrameworkMessage)

foreign import ccall "wrapper" wrapExecutorShutdown
  :: RawExecutorShutdown
  -> IO (FunPtr RawExecutorShutdown)

foreign import ccall "wrapper" wrapExecutorError
  :: RawExecutorError
  -> IO (FunPtr RawExecutorError)

foreign import ccall safe "createExecutor" c_createExecutor
  :: FunPtr RawExecutorRegistered
  -> FunPtr RawExecutorReRegistered
  -> FunPtr RawExecutorDisconnected
  -> FunPtr RawExecutorLaunchTask
  -> FunPtr RawExecutorTaskKilled
  -> FunPtr RawExecutorFrameworkMessage
  -> FunPtr RawExecutorShutdown
  -> FunPtr RawExecutorError
  -> IO ExecutorPtr

foreign import ccall safe "destroyExecutor" c_destroyExecutor
  :: ExecutorPtr
  -> IO ()

foreign import ccall safe "ext/executor.h createExecutorDriver" c_createExecutorDriver
  :: ExecutorPtr
  -> IO ExecutorDriverPtr

foreign import ccall safe "ext/executor.h destroyExecutorDriver" c_destroyExecutorDriver
  :: ExecutorDriverPtr
  -> IO ()

foreign import ccall safe "ext/executor.h startExecutorDriver" c_startExecutorDriver
  :: ExecutorDriverPtr
  -> IO CInt

foreign import ccall safe "ext/executor.h stopExecutorDriver" c_stopExecutorDriver
  :: ExecutorDriverPtr
  -> IO CInt

foreign import ccall safe "ext/executor.h abortExecutorDriver" c_abortExecutorDriver
  :: ExecutorDriverPtr
  -> IO CInt

foreign import ccall safe "ext/executor.h joinExecutorDriver" c_joinExecutorDriver
  :: ExecutorDriverPtr
  -> IO CInt

foreign import ccall safe "ext/executor.h runExecutorDriver" c_runExecutorDriver
  :: ExecutorDriverPtr
  -> IO CInt

foreign import ccall safe "ext/executor.h sendExecutorDriverStatusUpdate" c_sendExecutorDriverStatusUpdate
  :: ExecutorDriverPtr
  -> TaskStatusPtr
  -> IO CInt

foreign import ccall safe "ext/executor.h sendExecutorDriverFrameworkMessage" c_sendExecutorDriverFrameworkMessage
  :: ExecutorDriverPtr
  -> Ptr CChar
  -> CInt
  -> IO CInt

