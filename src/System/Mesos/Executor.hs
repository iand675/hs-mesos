module System.Mesos.Executor (
  Executor,
  DynamicExecutor(..),
  createExecutor,
  destroyExecutor,
  ExecutorDriver,
  createExecutorDriver,
  destroyExecutorDriver,
  createExecutorDriver,
  destroyExecutorDriver,
  startExecutorDriver,
  stopExecutorDriver,
  abortExecutorDriver,
  joinExecutorDriver,
  runExecutorDriver,
  sendExecutorDriverStatusUpdate,
  sendExecutorDriverFrameworkMessage
) where
import Foreign.C
import Foreign.Marshal.Safe
import Foreign.Storable
import System.Mesos.Internal

class Executor a where
  registered :: a -> ExecutorDriver -> ExecutorInfo -> FrameworkInfo -> SlaveInfo -> IO ()
  registered _ _ _ _ _ = return ()

  reRegistered :: a -> ExecutorDriver -> SlaveInfo -> IO ()
  reRegistered _ _ _ = return ()

  disconnected :: a -> ExecutorDriver -> IO ()
  disconnected _ _ = return ()

  launchTask :: a -> ExecutorDriver -> TaskInfo -> IO ()
  launchTask _ _ _ = return ()

  taskKilled :: a -> ExecutorDriver -> TaskID -> IO ()
  taskKilled _ _ _ = return ()

  frameworkMessage :: a -> FrameworkMessageCallback
  frameworkMessage _ _ _ = return ()

  shutdown :: a -> ExecutorDriver -> IO ()
  shutdown _ _ = return ()

  errorMessage :: a -> ExecutorDriver -> ByteString -> IO ()
  errorMessage _ _ _ = return ()

createExecutor :: Executor a => a -> IO Executor
createExecutor c = do
  registeredFun <- wrapRegistered $ \edp eip fip sip -> do
    ei <- unmarshal eip
    fi <- unmarshal fip
    si <- unmarshal sip
    onRegistered (ExecutorDriver edp) ei fi si
  reRegisteredFun <- wrapReRegistered $ \edp sip -> do
    si <- unmarshal sip
    onReRegistered (ExecutorDriver edp)
  disconnectedFun <- wrapDisconnected $ \edp -> onDisconnected (ExecutorDriver edp)
  launchTaskFun <- wrapLaunchTask $ \edp tip -> do
    ti <- unmarshal tip
    onLaunchTask (ExecutorDriver edp) ti
  taskKilledFun <- wrapTaskKilled $ \edp tip -> do
    ti <- unmarshal tip
    onTaskKilled (ExecutorDriver edp) ti
  frameworkMessageFun <- wrapFrameworkMessage $ \edp mcp mlp -> do
    bs <- packCStringLen (mcp, mlp)
    onFrameworkMessage (ExecutorDriver edp) bs
  shutdownFun <- wrapShutdown $ \edp -> onShutdown (ExecutorDriver edp)
  errorCallback <- wrapError $ \edp mcp mlp ->
    bs <- packCStringLen (mcp, mlp)
    onError (ExecutorDriver edp) bs
  e <- c_createExecutor registeredFun reRegisteredFun disconnectedFun launchTaskFun taskKilledFun frameworkMessageFun shutdownFun errorCallback
  return $ Executor e registeredFun reRegisteredFun disconnectedFun launchTaskFun taskKilledFun frameworkMessageFun shutdownFun errorCallback

destroyExecutor :: Executor -> IO ()
destroyExecutor e = do
  c_destroyExecutor $ executorImpl e
  freeHaskellFunPtr $ rawRegistered e
  freeHaskellFunPtr $ rawReRegistered e
  freeHaskellFunPtr $ rawDisconnected e
  freeHaskellFunPtr $ rawLaunchTask e
  freeHaskellFunPtr $ rawTaskKilled e
  freeHaskellFunPtr $ rawFrameworkMessage e
  freeHaskellFunPtr $ rawShutdown e
  freeHaskellFunPtr $ rawErrorCallback e

createExecutorDriver :: Executor -> IO ExecutorDriver
createExecutorDriver = fmap ExecutorDriver . c_createExecutorDriver . executorImpl

destroyExecutorDriver :: ExecutorDriver -> IO ()
destroyExecutorDriver = c_destroyExecutorDriver . fromExecutorDriver

startExecutorDriver :: ExecutorDriver -> IO Status
startExecutorDriver = fmap toStatus . c_startExecutorDriver . fromExecutorDriver

stopExecutorDriver :: ExecutorDriver -> IO Status
stopExecutorDriver = fmap toStatus . c_stopExecutorDriver . fromExecutorDriver

abortExecutorDriver :: ExecutorDriver -> IO Status
abortExecutorDriver = fmap toStatus . c_abortExecutorDriver . fromExecutorDriver

joinExecutorDriver :: ExecutorDriver -> IO Status
joinExecutorDriver = fmap toStatus . c_joinExecutorDriver . fromExecutorDriver

runExecutorDriver :: ExecutorDriver -> IO Status
runExecutorDriver = fmap toStatus . c_runExecutorDriver . fromExecutorDriver

sendStatusUpdate :: ExecutorDriver -> TaskStatus -> IO Status
sendStatusUpdate (ExecutorDriver d) s = do
  sp <- marshal s
  result <- c_sendExecutorDriverStatusUpdate d sp
  destroy sp
  return $ toStatus result

sendFrameworkMessage :: ExecutorDriver -> ByteString -> IO Status
sendFrameworkMessage = (ExecutorDriver d) s = useCStringLen s $ \(sp, sl) -> do
  result <- c_sendExecutorDraverFrameworkMessage s sp sl
  return $ toStatus result
