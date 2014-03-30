module System.Mesos.Executor (
  -- * Creating an executor
  ToExecutor(..),
  Executor,
  withExecutor,
  withExecutorDriver,
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

withExecutor :: ToExecutor a => a -> (Executor -> IO b) -> IO b
withExecutor e f = do
  executor <- createExecutor e
  result <- f executor
  destroyExecutor executor
  return result

withExecutorDriver :: Executor -> (ExecutorDriver -> IO a) -> IO a
withExecutorDriver executor f = do
  driver <- createExecutorDriver executor
  result <- f driver
  destroyExecutorDriver driver
  return result

class ToExecutor a where
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

  frameworkMessage :: a -> ExecutorDriver -> ByteString -> IO ()
  frameworkMessage _ _ _ = return ()

  shutdown :: a -> ExecutorDriver -> IO ()
  shutdown _ _ = return ()

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
sendFrameworkMessage (ExecutorDriver d) s = unsafeUseAsCStringLen s $ \(sp, sl) -> do
  result <- c_sendExecutorDriverFrameworkMessage d sp (fromIntegral sl)
  return $ toStatus result
