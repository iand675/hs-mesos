
-- | Mesos executor interface and executor driver. An executor is
-- responsible for launching tasks in a framework specific way (i.e.,
-- creating new threads, new processes, etc). One or more executors
-- from the same framework may run concurrently on the same
-- machine. Note that we use the term "executor" fairly loosely to
-- refer to the code that implements the Executor interface (see
-- below) as well as the program that is responsible for instantiating
-- a new MesosExecutorDriver (also below). In fact, while a Mesos
-- slave is responsible for (forking and) executing the "executor",
-- there is no reason why whatever the slave executed might itself
-- actually execute another program which actually instantiates and
-- runs the MesosSchedulerDriver. The only contract with the slave is
-- that the program that it invokes does not exit until the "executor"
-- has completed. Thus, what the slave executes may be nothing more
-- than a script which actually executes (or forks and waits) the
-- "real" executor.
module System.Mesos.Executor (
  -- * Creating an executor
  ToExecutor(..),
  Executor,
  withExecutor,
  withExecutorDriver,
  createExecutor,
  destroyExecutor,
  ExecutorDriver,
  createDriver,
  destroyDriver,
  -- * Interacting with Mesos
  start,
  stop,
  abort,
  await,
  run,
  sendStatusUpdate,
  sendFrameworkMessage
) where
import Data.ByteString (ByteString, packCStringLen)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Foreign.C
import Foreign.Marshal.Safe
import Foreign.Ptr
import Foreign.Storable
import System.Mesos.Internal
import System.Mesos.Types

withExecutor :: ToExecutor a => a -> (Executor -> IO b) -> IO b
withExecutor e f = do
  executor <- createExecutor e
  result <- f executor
  destroyExecutor executor
  return result

withExecutorDriver :: ToExecutor a => a -> (ExecutorDriver -> IO b) -> IO b
withExecutorDriver e f = withExecutor e $ \executor -> do
  driver <- createDriver executor
  result <- f driver
  destroyDriver driver
  return result

-- | Callback interface to be implemented by frameworks' executors. Note
-- that only one callback will be invoked at a time, so it is not
-- recommended that you block within a callback because it may cause a
-- deadlock.
class ToExecutor a where
  -- | Invoked once the executor driver has been able to successfully
  -- connect with Mesos. In particular, a scheduler can pass some
  -- data to its executors through the FrameworkInfo.ExecutorInfo's
  -- data field.
  registered :: a -> ExecutorDriver -> ExecutorInfo -> FrameworkInfo -> SlaveInfo -> IO ()
  registered _ _ _ _ _ = return ()

  -- | Invoked when the executor re-registers with a restarted slave.
  reRegistered :: a -> ExecutorDriver -> SlaveInfo -> IO ()
  reRegistered _ _ _ = return ()

  -- | Invoked when the executor becomes "disconnected" from the slave
  -- (e.g., the slave is being restarted due to an upgrade).
  disconnected :: a -> ExecutorDriver -> IO ()
  disconnected _ _ = return ()

  -- | Invoked when a task has been launched on this executor (initiated
  -- via @launchTasks@). Note that this task can be realized
  -- with a thread, a process, or some simple computation, however, no
  -- other callbacks will be invoked on this executor until this
  -- callback has returned.
  launchTask :: a -> ExecutorDriver -> TaskInfo -> IO ()
  launchTask _ _ _ = return ()

  -- | Invoked when a task running within this executor has been killed
  -- (via @killTask@). Note that no status update will
  -- be sent on behalf of the executor, the executor is responsible
  -- for creating a new @TaskStatus@ (i.e., with @Killed@) and
  -- invoking @sendStatusUpdate@.

  taskKilled :: a -> ExecutorDriver -> TaskID -> IO ()
  taskKilled _ _ _ = return ()

  -- | Invoked when a framework message has arrived for this
  -- executor. These messages are best effort; do not expect a
  -- framework message to be retransmitted in any reliable fashion.
  frameworkMessage :: a -> ExecutorDriver -> ByteString -> IO ()
  frameworkMessage _ _ _ = return ()

  -- | Invoked when the executor should terminate all of its currently
  -- running tasks. Note that after a Mesos has determined that an
  -- executor has terminated any tasks that the executor did not send
  -- terminal status updates for (e.g., @Killed@, @Finished@,
  -- @Failed@, etc) a @Lost@ status update will be created.
  shutdown :: a -> ExecutorDriver -> IO ()
  shutdown _ _ = return ()

  -- | Invoked when a fatal error has occured with the executor and/or
  -- executor driver. The driver will be aborted BEFORE invoking this
  -- callback.
  errorMessage :: a -> ExecutorDriver -> ByteString -> IO ()
  errorMessage _ _ _ = return ()

createExecutor :: ToExecutor a => a -> IO Executor
createExecutor c = do
  registeredFun <- wrapExecutorRegistered $ \edp eip fip sip -> do
    ei <- unmarshal eip
    fi <- unmarshal fip
    si <- unmarshal sip
    registered c (ExecutorDriver edp) ei fi si
  reRegisteredFun <- wrapExecutorReRegistered $ \edp sip -> do
    si <- unmarshal sip
    reRegistered c (ExecutorDriver edp) si
  disconnectedFun <- wrapExecutorDisconnected $ \edp -> disconnected c (ExecutorDriver edp)
  launchTaskFun <- wrapExecutorLaunchTask $ \edp tip -> do
    ti <- unmarshal tip
    launchTask c (ExecutorDriver edp) ti
  taskKilledFun <- wrapExecutorTaskKilled $ \edp tip -> do
    ti <- unmarshal tip
    taskKilled c (ExecutorDriver edp) ti
  frameworkMessageFun <- wrapExecutorFrameworkMessage $ \edp mcp mlp -> do
    bs <- packCStringLen (mcp, fromIntegral mlp)
    frameworkMessage c (ExecutorDriver edp) bs
  shutdownFun <- wrapExecutorShutdown $ \edp -> shutdown c (ExecutorDriver edp)
  errorCallback <- wrapExecutorError $ \edp mcp mlp -> do
    bs <- packCStringLen (mcp, fromIntegral mlp)
    errorMessage c (ExecutorDriver edp) bs
  e <- c_createExecutor registeredFun reRegisteredFun disconnectedFun launchTaskFun taskKilledFun frameworkMessageFun shutdownFun errorCallback
  return $ Executor e registeredFun reRegisteredFun disconnectedFun launchTaskFun taskKilledFun frameworkMessageFun shutdownFun errorCallback

destroyExecutor :: Executor -> IO ()
destroyExecutor e = do
  c_destroyExecutor $ executorImpl e
  freeHaskellFunPtr $ rawExecutorRegistered e
  freeHaskellFunPtr $ rawExecutorReRegistered e
  freeHaskellFunPtr $ rawExecutorDisconnected e
  freeHaskellFunPtr $ rawExecutorLaunchTask e
  freeHaskellFunPtr $ rawExecutorTaskKilled e
  freeHaskellFunPtr $ rawExecutorFrameworkMessage e
  freeHaskellFunPtr $ rawExecutorShutdown e
  freeHaskellFunPtr $ rawExecutorErrorCallback e

createDriver :: Executor -> IO ExecutorDriver
createDriver = fmap ExecutorDriver . c_createExecutorDriver . executorImpl

destroyDriver :: ExecutorDriver -> IO ()
destroyDriver = c_destroyExecutorDriver . fromExecutorDriver

-- | Starts the executor driver. This needs to be called before any
-- other driver calls are made.
start :: ExecutorDriver -> IO Status
start = fmap toStatus . c_startExecutorDriver . fromExecutorDriver

-- | Stops the @ExecutorDriver@.
stop :: ExecutorDriver -> IO Status
stop = fmap toStatus . c_stopExecutorDriver . fromExecutorDriver

-- | Aborts the driver so that no more callbacks can be made to the
-- executor. The semantics of abort and stop have deliberately been
-- separated so that code can detect an aborted driver (i.e., via
-- the return status of @abort@, see below), and
-- instantiate and start another driver if desired (from within the
-- same process ... although this functionality is currently not
-- supported for executors).
abort :: ExecutorDriver -> IO Status
abort = fmap toStatus . c_abortExecutorDriver . fromExecutorDriver

-- | Waits for the driver to be stopped or aborted, possibly
 -- *blocking* the current thread indefinitely. The return status of
 -- this function can be used to determine if the driver was aborted
 -- (see mesos.proto for a description of Status).
await :: ExecutorDriver -> IO Status
await = fmap toStatus . c_joinExecutorDriver . fromExecutorDriver

-- | Starts and immediately @await@s (i.e., blocks on) the driver.
run :: ExecutorDriver -> IO Status
run = fmap toStatus . c_runExecutorDriver . fromExecutorDriver

-- | Sends a status update to the framework scheduler, retrying as
-- necessary until an acknowledgement has been received or the
 -- executor is terminated (in which case, a @Lost@ status update
 -- will be sent). See @System.Mesos.Scheduler.statusUpdate@ for more information
 -- about status update acknowledgements.
sendStatusUpdate :: ExecutorDriver -> TaskStatus -> IO Status
sendStatusUpdate (ExecutorDriver d) s = do
  sp <- marshal s
  result <- c_sendExecutorDriverStatusUpdate d sp
  destroy sp
  return $ toStatus result

-- | Sends a message to the framework scheduler. These messages are
-- best effort; do not expect a framework message to be
-- retransmitted in any reliable fashion.
sendFrameworkMessage :: ExecutorDriver -> ByteString -> IO Status
sendFrameworkMessage (ExecutorDriver d) s = unsafeUseAsCStringLen s $ \(sp, sl) -> do
  result <- c_sendExecutorDriverFrameworkMessage d sp (fromIntegral sl)
  return $ toStatus result
