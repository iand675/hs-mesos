module System.Mesos.Scheduler (
  Scheduler,
  createScheduler,
  destroyScheduler,
  SchedulerDriver,
  create,
  destroyDriver,
  start,
  stop,
  abort,
  joinDriver,
  run,
  requestResources,
  launchTasks,
  killTask,
  declineOffer,
  reviveOffers,
  sendFrameworkMessage,
  reconcileTasks
) where
import Data.ByteString (ByteString, packCStringLen)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Foreign.C
import Foreign.Ptr
import System.Mesos.Internal

class ToScheduler a where
  registered :: a -> SchedulerDriver -> FrameworkID -> MasterInfo -> IO ()
  registered _ _ _ _ = return ()

  reRegistered :: a -> SchedulerDriver -> MasterInfo -> IO ()
  reRegistered _ _ _ = return ()

  disconnected :: a -> SchedulerDriver -> IO ()
  disconnected _ _ = return ()

  resourceOffers :: a -> SchedulerDriver -> [Offer] -> IO ()
  resourceOffers _ _ _ = return ()

  offerRescinded :: a -> SchedulerDriver -> OfferID -> IO ()
  offerRescinded _ _ _ = return ()

  statusUpdate :: a -> SchedulerDriver -> TaskStatus -> IO ()
  statusUpdate _ _ _ = return ()

  frameworkMessage :: a -> SchedulerDriver -> ExecutorID -> SlaveID -> ByteString -> IO ()
  frameworkMessage  _ _ _ _ _ = return ()

  slaveLost :: a -> SchedulerDriver -> SlaveID -> IO ()
  slaveLost _ _ _ = return ()

  executorLost :: a -> SchedulerDriver -> ExecutorID -> SlaveID -> Status -> IO ()
  executorLost _ _ _ _ _ = return ()

  errorMessage :: a -> SchedulerDriver -> ByteString -> IO ()
  errorMessage _ _ _ = return ()

createScheduler :: ToScheduler a => a -> IO Scheduler
createScheduler s = do
  registeredFun <- wrapSchedulerRegistered $ \sdp fp mp -> do
    let sd = SchedulerDriver sdp
    f <- unmarshal fp
    m <- unmarshal mp
    (registered s) sd f m
  reRegisteredFun <- wrapSchedulerReRegistered $ \sdp mip -> do
    let sd = SchedulerDriver sdp
    mi <- unmarshal mip
    (reRegistered s) sd mi
  disconnectedFun <- wrapSchedulerDisconnected $ \sdp -> do
    let sd = SchedulerDriver sdp
    (disconnected s) sd
  resourceOffersFun <- wrapSchedulerResourceOffers $ \sdp os c -> do
    let sd = SchedulerDriver sdp
    (resourceOffers s) sd []
  offerRescindedFun <- wrapSchedulerOfferRescinded $ \sdp oidp -> do
    let sd = SchedulerDriver sdp
    oid <- unmarshal oidp
    (offerRescinded s) sd oid
  statusUpdateFun <- wrapSchedulerStatusUpdate $ \sdp tsp -> do
    let sd = SchedulerDriver sdp
    ts <- unmarshal tsp
    (statusUpdate s) sd ts
  frameworkMessageFun <- wrapSchedulerFrameworkMessage $ \sdp eip sip ptr c -> do
    let sd = SchedulerDriver sdp
    ei <- unmarshal eip
    si <- unmarshal sip
    bs <- packCStringLen (ptr, fromIntegral c)
    (frameworkMessage s) sd ei si bs
  slaveLostFun <- wrapSchedulerSlaveLost $ \sdp sip -> do
    let sd = SchedulerDriver sdp
    si <- unmarshal sip
    (slaveLost s) sd si
  executorLostFun <- wrapSchedulerExecutorLost $ \sdp eip sip st -> do
    let sd = SchedulerDriver sdp
    ei <- unmarshal eip
    si <- unmarshal sip
    (executorLost s) sd ei si (toEnum $ fromIntegral st)
  errorFun <- wrapSchedulerError $ \sdp ptr c -> do
    let sd = SchedulerDriver sdp
    bs <- packCStringLen (ptr, fromIntegral c)
    (errorMessage s) sd bs
  schedulerPtr <- c_createScheduler registeredFun reRegisteredFun disconnectedFun resourceOffersFun offerRescindedFun statusUpdateFun frameworkMessageFun slaveLostFun executorLostFun errorFun
  return $ Scheduler schedulerPtr registeredFun reRegisteredFun disconnectedFun resourceOffersFun offerRescindedFun statusUpdateFun frameworkMessageFun slaveLostFun executorLostFun errorFun

destroyScheduler :: Scheduler -> IO ()
destroyScheduler s = do
  c_destroyScheduler $ schedulerImpl s
  freeHaskellFunPtr $ rawSchedulerRegistered s
  freeHaskellFunPtr $ rawSchedulerReRegistered s
  freeHaskellFunPtr $ rawSchedulerDisconnected s
  freeHaskellFunPtr $ rawSchedulerResourceOffers s
  freeHaskellFunPtr $ rawSchedulerOfferRescinded s
  freeHaskellFunPtr $ rawSchedulerStatusUpdate s
  freeHaskellFunPtr $ rawSchedulerFrameworkMessage s
  freeHaskellFunPtr $ rawSchedulerSlaveLost s
  freeHaskellFunPtr $ rawSchedulerExecutorLost s
  freeHaskellFunPtr $ rawSchedulerError s

excerciseMethods :: Scheduler -> IO ()
excerciseMethods = c_exerciseMethods . schedulerImpl

withDriver :: (SchedulerDriverPtr -> IO CInt) -> SchedulerDriver -> IO Status
withDriver f (SchedulerDriver p) = fmap (toEnum . fromIntegral) $ f p

create :: Scheduler -> FrameworkInfo -> ByteString -> Maybe Credential -> IO SchedulerDriver
create = undefined

destroyDriver :: SchedulerDriver -> IO ()
destroyDriver = c_destroySchedulerDriver . fromSchedulerDriver

start :: SchedulerDriver -> IO Status
start = withDriver c_startSchedulerDriver

stop :: SchedulerDriver -> Bool -> IO Status
stop d f = withDriver (\p -> c_stopSchedulerDriver p fi) d
  where
    fi = if f then 1 else 0

abort :: SchedulerDriver -> IO Status
abort = withDriver c_abortSchedulerDriver

joinDriver :: SchedulerDriver -> IO Status
joinDriver = withDriver c_joinSchedulerDriver

run :: SchedulerDriver -> IO Status
run = withDriver c_runSchedulerDriver

-- TODO destroy marshalled values after calling C function
requestResources :: SchedulerDriver -> [Request] -> IO Status
requestResources (SchedulerDriver p) rs = do
  fmap (toEnum . fromIntegral) $ withMarshal rs $ \l rp -> do
    c_requestResources p rp $ fromIntegral l

launchTasks :: SchedulerDriver -> [OfferID] -> [TaskInfo] -> Filters -> IO Status
launchTasks (SchedulerDriver p) os ts f = do
  fp <- marshal f
  res <- withMarshal os $ \ol op ->
    withMarshal ts $ \tl tp -> c_launchTasks p op (fromIntegral ol) tp (fromIntegral tl) fp
  destroy fp
  return $ toEnum $ fromIntegral res

killTask :: SchedulerDriver -> TaskID -> IO Status
killTask (SchedulerDriver p) t = do
  tid <- marshal t
  res <- c_killTask p tid
  destroy tid
  return $ toEnum $ fromIntegral res

declineOffer :: SchedulerDriver -> OfferID -> Filters -> IO Status
declineOffer (SchedulerDriver p) o f = do
  oid <- marshal o
  fp <- marshal f
  res <- c_declineOffer p oid fp
  destroy oid
  destroy fp
  return $ toEnum $ fromIntegral res

reviveOffers :: SchedulerDriver -> IO Status
reviveOffers = withDriver c_reviveOffers

sendFrameworkMessage :: SchedulerDriver -> ExecutorID -> SlaveID -> ByteString -> IO Status
sendFrameworkMessage (SchedulerDriver p) e s bs = do
  ep <- marshal e
  sp <- marshal s
  res <- unsafeUseAsCStringLen bs $ \(strp, l) ->
    c_sendFrameworkMessage p ep sp strp (fromIntegral l)
  destroy ep
  destroy sp
  return $ toEnum $ fromIntegral res

reconcileTasks :: SchedulerDriver -> [TaskStatus] -> IO Status
reconcileTasks (SchedulerDriver p) ts = do
  res <- withMarshal ts $ \l tp -> c_reconcileTasks p tp (fromIntegral l)
  return $ toEnum $ fromIntegral res

