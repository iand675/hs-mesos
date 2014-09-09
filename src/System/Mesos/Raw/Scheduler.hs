module System.Mesos.Raw.Scheduler where
import           System.Mesos.Internal
import           System.Mesos.Raw.Credential
import           System.Mesos.Raw.ExecutorId
import           System.Mesos.Raw.Filters
import           System.Mesos.Raw.FrameworkId
import           System.Mesos.Raw.FrameworkInfo
import           System.Mesos.Raw.MasterInfo
import           System.Mesos.Raw.Offer
import           System.Mesos.Raw.OfferId
import           System.Mesos.Raw.Request
import           System.Mesos.Raw.SlaveId
import           System.Mesos.Raw.TaskId
import           System.Mesos.Raw.TaskInfo
import           System.Mesos.Raw.TaskStatus
type SchedulerPtr = Ptr Scheduler

data Scheduler = Scheduler
  { schedulerImpl                :: SchedulerPtr
  , rawSchedulerRegistered       :: FunPtr RawSchedulerRegistered
  , rawSchedulerReRegistered     :: FunPtr RawSchedulerReRegistered
  , rawSchedulerDisconnected     :: FunPtr RawSchedulerDisconnected
  , rawSchedulerResourceOffers   :: FunPtr RawSchedulerResourceOffers
  , rawSchedulerOfferRescinded   :: FunPtr RawSchedulerOfferRescinded
  , rawSchedulerStatusUpdate     :: FunPtr RawSchedulerStatusUpdate
  , rawSchedulerFrameworkMessage :: FunPtr RawSchedulerFrameworkMessage
  , rawSchedulerSlaveLost        :: FunPtr RawSchedulerSlaveLost
  , rawSchedulerExecutorLost     :: FunPtr RawSchedulerExecutorLost
  , rawSchedulerError            :: FunPtr RawSchedulerError
  }

-- | Type representing the connection from a scheduler to Mesos. This
-- handle is used both to manage the scheduler's lifecycle (start
-- it, stop it, or wait for it to finish) and to interact with Mesos
-- (e.g., launch tasks, kill tasks, etc.).
newtype SchedulerDriver = SchedulerDriver { fromSchedulerDriver :: SchedulerDriverPtr }
  deriving (Show, Eq)

type SchedulerDriverPtr = Ptr SchedulerDriver

type RawSchedulerRegistered = SchedulerDriverPtr -> FrameworkIDPtr -> MasterInfoPtr -> IO ()

type RawSchedulerReRegistered = SchedulerDriverPtr -> MasterInfoPtr -> IO ()

type RawSchedulerDisconnected = SchedulerDriverPtr -> IO ()

type RawSchedulerResourceOffers = SchedulerDriverPtr -> Ptr OfferPtr -> CInt -> IO ()

type RawSchedulerOfferRescinded = SchedulerDriverPtr -> OfferIDPtr -> IO ()

type RawSchedulerStatusUpdate = SchedulerDriverPtr -> TaskStatusPtr -> IO ()

type RawSchedulerFrameworkMessage = SchedulerDriverPtr -> ExecutorIDPtr -> SlaveIDPtr -> Ptr CChar -> Int -> IO ()

type RawSchedulerSlaveLost = SchedulerDriverPtr -> SlaveIDPtr -> IO ()

type RawSchedulerExecutorLost = SchedulerDriverPtr -> ExecutorIDPtr -> SlaveIDPtr -> CInt -> IO ()

type RawSchedulerError = SchedulerDriverPtr -> Ptr CChar -> CInt -> IO ()

foreign import ccall "wrapper" wrapSchedulerRegistered
  :: RawSchedulerRegistered
  -> IO (FunPtr RawSchedulerRegistered)

foreign import ccall "wrapper" wrapSchedulerReRegistered
  :: RawSchedulerReRegistered
  -> IO (FunPtr RawSchedulerReRegistered)

foreign import ccall "wrapper" wrapSchedulerDisconnected
  :: RawSchedulerDisconnected
  -> IO (FunPtr RawSchedulerDisconnected)

foreign import ccall "wrapper" wrapSchedulerResourceOffers
  :: RawSchedulerResourceOffers
  -> IO (FunPtr RawSchedulerResourceOffers)

foreign import ccall "wrapper" wrapSchedulerOfferRescinded
  :: RawSchedulerOfferRescinded
  -> IO (FunPtr RawSchedulerOfferRescinded)

foreign import ccall "wrapper" wrapSchedulerStatusUpdate
  :: RawSchedulerStatusUpdate
  -> IO (FunPtr RawSchedulerStatusUpdate)

foreign import ccall "wrapper" wrapSchedulerFrameworkMessage
  :: RawSchedulerFrameworkMessage
  -> IO (FunPtr RawSchedulerFrameworkMessage)

foreign import ccall "wrapper" wrapSchedulerSlaveLost
  :: RawSchedulerSlaveLost
  -> IO (FunPtr RawSchedulerSlaveLost)

foreign import ccall "wrapper" wrapSchedulerExecutorLost
  :: RawSchedulerExecutorLost
  -> IO (FunPtr RawSchedulerExecutorLost)

foreign import ccall "wrapper" wrapSchedulerError
  :: RawSchedulerError
  -> IO (FunPtr RawSchedulerError)

foreign import ccall safe "ext/scheduler.h createScheduler" c_createScheduler
  :: FunPtr RawSchedulerRegistered
  -> FunPtr RawSchedulerReRegistered
  -> FunPtr RawSchedulerDisconnected
  -> FunPtr RawSchedulerResourceOffers
  -> FunPtr RawSchedulerOfferRescinded
  -> FunPtr RawSchedulerStatusUpdate
  -> FunPtr RawSchedulerFrameworkMessage
  -> FunPtr RawSchedulerSlaveLost
  -> FunPtr RawSchedulerExecutorLost
  -> FunPtr RawSchedulerError
  -> IO SchedulerPtr

foreign import ccall safe "ext/scheduler.h destroyScheduler" c_destroyScheduler
  :: SchedulerPtr
  -> IO ()

foreign import ccall safe "ext/scheduler.h createSchedulerDriver" c_createSchedulerDriver
  :: SchedulerPtr
  -> FrameworkInfoPtr
  -> Ptr CChar
  -> CInt
  -> IO SchedulerDriverPtr

foreign import ccall safe "ext/scheduler.h createSchedulerDriverWithCredentials"  c_createSchedulerDriverWithCredentials
  :: SchedulerPtr
  -> FrameworkInfoPtr
  -> Ptr CChar
  -> CInt
  -> CredentialPtr
  -> IO SchedulerDriverPtr

foreign import ccall safe "ext/scheduler.h destroySchedulerDriver" c_destroySchedulerDriver
  :: SchedulerDriverPtr
  -> IO ()

foreign import ccall safe "ext/scheduler.h startSchedulerDriver" c_startSchedulerDriver
  :: SchedulerDriverPtr
  -> IO CInt

foreign import ccall safe "ext/scheduler.h stopSchedulerDriver" c_stopSchedulerDriver
  :: SchedulerDriverPtr
  -> CInt
  -> IO CInt

foreign import ccall safe "ext/scheduler.h abortSchedulerDriver" c_abortSchedulerDriver
  :: SchedulerDriverPtr
  -> IO CInt

foreign import ccall safe "ext/scheduler.h joinSchedulerDriver" c_joinSchedulerDriver
  :: SchedulerDriverPtr
  -> IO CInt

foreign import ccall safe "ext/scheduler.h runSchedulerDriver" c_runSchedulerDriver
  :: SchedulerDriverPtr
  -> IO CInt

foreign import ccall safe "ext/scheduler.h requestResources" c_requestResources
  :: SchedulerDriverPtr
  -> Ptr RequestPtr
  -> CInt
  -> IO CInt

foreign import ccall safe "ext/scheduler.h launchTasks" c_launchTasks
  :: SchedulerDriverPtr
  -> Ptr OfferIDPtr
  -> CInt
  -> Ptr TaskInfoPtr
  -> CInt
  -> FiltersPtr
  -> IO CInt

foreign import ccall safe "ext/scheduler.h killTask" c_killTask
  :: SchedulerDriverPtr
  -> TaskIDPtr
  -> IO CInt

foreign import ccall safe "ext/scheduler.h declineOffer" c_declineOffer
  :: SchedulerDriverPtr
  -> OfferIDPtr
  -> FiltersPtr
  -> IO CInt

foreign import ccall safe "ext/scheduler.h reviveOffers" c_reviveOffers
  :: SchedulerDriverPtr
  -> IO CInt

foreign import ccall safe "ext/scheduler.h schedulerDriverSendFrameworkMessage" c_sendFrameworkMessage
  :: SchedulerDriverPtr
  -> ExecutorIDPtr
  -> SlaveIDPtr
  -> Ptr CChar
  -> CInt
  -> IO CInt

foreign import ccall safe "ext/scheduler.h reconcileTasks" c_reconcileTasks
  :: SchedulerDriverPtr
  -> Ptr TaskStatusPtr
  -> CInt
  -> IO CInt
