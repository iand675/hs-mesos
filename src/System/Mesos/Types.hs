{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.Mesos.Types (
  -- * Core Framework & Executor types
  -- ** Masters & Slaves
  MasterInfo(..),
  masterInfo,
  SlaveInfo(..),
  slaveInfo,
  -- ** Frameworks & Executors
  ExecutorInfo(..),
  executorInfo,
  FrameworkInfo(..),
  frameworkInfo,
  -- ** Resource allocation
  Offer(..),
  Request(..),
  Filters(..),
  filters,
  -- ** Launching Tasks
  TaskInfo(..),
  TaskExecutionInfo(..),
  CommandInfo(..),
  CommandURI(..),
  commandURI,
  CommandValue (..),
  Value(..),
  Resource(..),
  resource,
  -- ** Task & Executor Status Updates
  Status(..),
  TaskStatus(..),
  TaskState(..),
  isTerminal,
  -- ** Identifiers
  FrameworkID(..),
  SlaveID(..),
  OfferID(..),
  TaskID(..),
  ExecutorID(..),
  ContainerID(..),
  -- ** Containerization Support
  ContainerInfo (..),
  Volume (..),
  Mode (..),
  ContainerType (..),
  -- ** Health Checks
  HealthCheck (..),
  HealthCheckStrategy (..),
  -- ** Resource Usage & Performance Statistics
  ResourceStatistics(..),
  ResourceUsage(..),
  PerformanceStatistics(..),
  -- ** Task Status
  -- ** Credentials & ACLs
  Credential(..),
  credential
) where
import           Data.ByteString (ByteString)
import           Data.String
import           Data.Word

isTerminal :: TaskState -> Bool
isTerminal s = case s of
  Finished -> True
  Failed -> True
  Killed -> True
  Lost -> True
  _ -> False

-- | Describes possible task states. IMPORTANT: Mesos assumes tasks that
-- enter terminal states (see below) imply the task is no longer
-- running and thus clean up any thing associated with the task
-- (ultimately offering any resources being consumed by that task to
-- another task).
data TaskState
  = Staging -- ^ Initial state. Framework status updates should not use.
  | Starting
  | TaskRunning
  | Finished -- ^ TERMINAL.
  | Failed -- ^ TERMINAL.
  | Killed -- ^ TERMINAL.
  | Lost -- ^ TERMINAL.
  deriving (Show, Eq)

instance Enum TaskState where
  fromEnum Staging = 6
  fromEnum Starting = 0
  fromEnum TaskRunning = 1
  fromEnum Finished = 2
  fromEnum Failed = 3
  fromEnum Killed = 4
  fromEnum Lost = 5
  toEnum 0 = Starting
  toEnum 1 = TaskRunning
  toEnum 2 = Finished
  toEnum 3 = Failed
  toEnum 4 = Killed
  toEnum 5 = Lost
  toEnum 6 = Staging
  toEnum _ = error "Unsupported task state"

-- | Indicates the state of the scheduler and executor driver
-- after function calls.
data Status = NotStarted | Running | Aborted | Stopped
  deriving (Show, Eq)

instance Enum Status where
  fromEnum Running = 2
  fromEnum NotStarted = 1
  fromEnum Aborted = 3
  fromEnum Stopped = 4
  toEnum 1 = NotStarted
  toEnum 2 = Running
  toEnum 3 = Aborted
  toEnum 4 = Stopped
  toEnum _ = error "Unsupported status"

-- | A unique ID assigned to a framework. A framework can reuse this ID in order to do failover.
newtype FrameworkID = FrameworkID { fromFrameworkID :: ByteString }
  deriving (Show, Eq, IsString)

-- | A unique ID assigned to an offer.
newtype OfferID = OfferID { fromOfferID :: ByteString }
  deriving (Show, Eq, IsString)

-- |  A unique ID assigned to a slave. Currently, a slave gets a new ID whenever it (re)registers with Mesos. Framework writers shouldn't assume any binding between a slave ID and and a hostname.
newtype SlaveID = SlaveID { fromSlaveID :: ByteString }
  deriving (Show, Eq, IsString)

-- |  A framework generated ID to distinguish a task. The ID must remain
-- unique while the task is active. However, a framework can reuse an
-- ID _only_ if a previous task with the same ID has reached a
-- terminal state (e.g., 'Finished', 'Lost', 'Killed', etc.). See 'isTerminal' for a utility function to simplify checking task state.
newtype TaskID = TaskID { fromTaskID :: ByteString }
  deriving (Show, Eq, IsString)

-- |  A framework generated ID to distinguish an executor. Only one
-- executor with the same ID can be active on the same slave at a
-- time.
newtype ExecutorID = ExecutorID { fromExecutorID :: ByteString }
  deriving (Show, Eq, IsString)


-- |  A slave generated ID to distinguish a container. The ID must be unique
-- between any active or completed containers on the slave. In particular,
-- containers for different runs of the same (framework, executor) pair must be
-- unique.
newtype ContainerID = ContainerID { fromContainerID :: ByteString }
  deriving (Show, Eq, IsString)

-- | Describes a framework. If the user field is set to an empty string
-- Mesos will automagically set it to the current user. Note that the
-- ID is only available after a framework has registered, however, it
-- is included here in order to facilitate scheduler failover (i.e.,
-- if it is set then the 'System.Mesos.Scheduler.SchedulerDriver' expects the scheduler is
-- performing failover). The amount of time that the master will wait
-- for the scheduler to failover before removing the framework is
-- specified by 'frameworkFailoverTimeout'.
-- If 'frameworkCheckpoint' is set, framework pid, executor pids and status updates
-- are checkpointed to disk by the slaves.
-- Checkpointing allows a restarted slave to reconnect with old executors
-- and recover status updates, at the cost of disk I/O.
-- The 'frameworkRole' field is used to group frameworks for allocation decisions,
-- depending on the allocation policy being used.
-- If the 'frameworkHostname' field is set to an empty string Mesos will
-- automagically set it to the current hostname.
data FrameworkInfo = FrameworkInfo
  { frameworkUser            :: !ByteString
  , frameworkName            :: !ByteString
  , frameworkID              :: !(Maybe FrameworkID)
  , frameworkFailoverTimeout :: !(Maybe Double)
  , frameworkCheckpoint      :: !(Maybe Bool)
  , frameworkRole            :: !(Maybe ByteString)
  , frameworkHostname        :: !(Maybe ByteString)
  , frameworkPrincipal       :: !(Maybe ByteString)
  }
  deriving (Show, Eq)

frameworkInfo :: ByteString -> ByteString -> FrameworkInfo
frameworkInfo u n = FrameworkInfo u n Nothing Nothing Nothing Nothing Nothing Nothing

data HealthCheckStrategy
   = HTTPCheck
     { httpCheckPort     :: !Word32 -- ^ Port to send the HTTP request.
     , httpCheckPath     :: !(Maybe ByteString) -- ^ HTTP request path. (defaults to @"/"@.
     , httpCheckStatuses :: ![Word32] -- ^ Expected response statuses. Not specifying any statuses implies that any returned status is acceptable.
     }
   | CommandCheck
     { commandCheckCommand :: !CommandInfo -- ^ Command health check.
     } deriving (Show, Eq)

data HealthCheck = HealthCheck
  { healthCheckStrategy            :: !HealthCheckStrategy
  , healthCheckDelaySeconds        :: !(Maybe Double) -- ^ Amount of time to wait until starting the health checks.
  , healthCheckIntervalSeconds     :: !(Maybe Double) -- ^ Interval between health checks.
  , healthCheckTimeoutSeconds      :: !(Maybe Double) -- ^ Amount of time to wait for the health check to complete.
  , healthCheckConsecutiveFailures :: !(Maybe Word32) -- ^ Number of consecutive failures until considered unhealthy.
  , healthCheckGracePeriodSeconds  :: !(Maybe Double) -- ^ Amount of time to allow failed health checks since launch.
  } deriving (Show, Eq)

-- | Describes a command, executed via:
--
-- > /bin/sh -c value
--
-- Any URIs specified are fetched before executing the command.  If the executable field for an
-- uri is set, executable file permission is set on the downloaded file.
-- Otherwise, if the downloaded file has a recognized archive extension
-- (currently [compressed] tar and zip) it is extracted into the executor's
-- working directory.  In addition, any environment variables are set before
-- executing the command (so they can be used to "parameterize" your command).
data CommandInfo = CommandInfo
  { commandInfoURIs    :: ![CommandURI]
  , commandEnvironment :: !(Maybe [(ByteString, ByteString)])
  , commandValue       :: !CommandValue
  -- TODO: Existing field in 0.20, but doesn't actually work
  -- , commandContainer   :: !(Maybe ContainerInfo)

  -- | Enables executor and tasks to run as a specific user. If the user
  -- field is present both in 'FrameworkInfo' and here, the 'CommandInfo'
  -- user value takes precedence.
  , commandUser        :: !(Maybe ByteString)
  }
  deriving (Show, Eq)

data CommandURI = CommandURI
  { commandURIValue      :: !ByteString
  , commandURIExecutable :: !(Maybe Bool)
  , commandURIExtract    :: !(Maybe Bool)
  }
  deriving (Show, Eq)

commandURI :: ByteString -> CommandURI
commandURI v = CommandURI v Nothing Nothing

{-
TODO: not yet supported (0.20)
data CommandContainerInfo = CommandContainerInfo
  { commandContainerInfoImage   :: !ByteString
  , commandContainerInfoOptions :: ![ByteString]
  }
-}

data CommandValue = ShellCommand !ByteString
                  | RawCommand !ByteString ![ByteString]
                  deriving (Show, Eq)

-- Describes information about an executor. The 'executorData' field can be
-- used to pass arbitrary bytes to an executor.
data ExecutorInfo = ExecutorInfo
  { executorInfoExecutorID    :: !ExecutorID
  , executorInfoFrameworkID   :: !FrameworkID
  , executorInfoCommandInfo   :: !CommandInfo
  , executorInfoContainerInfo :: !(Maybe ContainerInfo)
  -- ^ Executor provided with a container will launch the container
  -- with the executor's 'CommandInfo' and we expect the container to
  -- act as a Mesos executor.
  , executorInfoResources     :: ![Resource]
  , executorName              :: !(Maybe ByteString)
  , executorSource            :: !(Maybe ByteString)
  -- ^ Source is an identifier style string used by frameworks to track
  -- the source of an executor. This is useful when it's possible for
  -- different executor ids to be related semantically.
  --
  -- NOTE: Source is exposed alongside the resource usage of the
  -- executor via JSON on the slave. This allows users to import
  -- usage information into a time series database for monitoring.
  , executorData              :: !(Maybe ByteString)
  }
  deriving (Show, Eq)

executorInfo :: ExecutorID -> FrameworkID -> CommandInfo -> [Resource] -> ExecutorInfo
executorInfo eid fid ci rs = ExecutorInfo eid fid ci Nothing rs Nothing Nothing Nothing

-- | Describes a master. This will probably have more fields in the
-- future which might be used, for example, to link a framework web UI
-- to a master web UI.
data MasterInfo = MasterInfo
  { masterInfoID       :: !ByteString
  , masterInfoIP       :: !Word32
  , masterInfoPort     :: !(Maybe Word32) -- ^ Defaults to 5050
  , masterInfoPID      :: !(Maybe ByteString)
  , masterInfoHostname :: !(Maybe ByteString)
  }
  deriving (Show, Eq)

masterInfo :: ByteString -> Word32 -> MasterInfo
masterInfo id ip = MasterInfo id ip Nothing Nothing Nothing

-- | Describes a slave. Note that the 'slaveInfoSlaveID' field is only available after
-- a slave is registered with the master, and is made available here
-- to facilitate re-registration.  If checkpoint is set, the slave is
-- checkpointing its own information and potentially frameworks'
-- information (if a framework has checkpointing enabled).
data SlaveInfo = SlaveInfo
  { slaveInfoHostname   :: !ByteString
  , slaveInfoPort       :: !(Maybe Word32)
  , slaveInfoResources  :: ![Resource]
  , slaveInfoAttributes :: ![(ByteString, Value)]
  , slaveInfoSlaveID    :: !(Maybe SlaveID)
  , slaveInfoCheckpoint :: !(Maybe Bool)
  }
  deriving (Show, Eq)

slaveInfo :: ByteString -> [Resource] -> [(ByteString, Value)] -> SlaveInfo
slaveInfo hn rs as = SlaveInfo hn Nothing rs as Nothing Nothing

newtype Filters = Filters
  { refuseSeconds :: Maybe Double
  -- ^ Time to consider unused resources refused. Note that all unused
  -- resources will be considered refused and use the default value
  -- (below) regardless of whether Filters was passed to
  -- SchedulerDriver::launchTasks. You MUST pass Filters with this
  -- field set to change this behavior (i.e., get another offer which
  -- includes unused resources sooner or later than the default).
  --
  -- Defaults to 5.0 if not set.
  }
  deriving (Show, Eq)

filters :: Filters
filters = Filters Nothing

credential :: ByteString -> Credential
credential p = Credential p Nothing

data Value
  = Scalar !Double
  | Ranges ![(Word64, Word64)]
  | Set ![ByteString]
  | Text !ByteString
  deriving (Show, Eq)

-- | Describes a resource on a machine. A resource can take on one of
-- three types: scalar (double), a list of finite and discrete ranges
-- (e.g., [1-10, 20-30]), or a set of items.
--
-- N.B. there is a slight deviation from the C++ API: the Haskell bindings convert 'Text' values
-- into a single element 'Set' value in order to avoid having to expose yet another data type.
data Resource = Resource
  { resourceName  :: !ByteString
  , resourceValue :: !Value
  , resourceRole  :: !(Maybe ByteString)
  }
  deriving (Show, Eq)

resource :: ByteString -> Value -> Resource
resource n v = Resource n v Nothing

data ResourceStatistics = ResourceStatistics
  { resourceStatisticsTimestamp          :: !Double
  , resourceStatisticsCPUsUserTimeSecs   :: !(Maybe Double)
  -- ^ Total CPU time spent in user mode
  , resourceStatisticsCPUsSystemTimeSecs :: !(Maybe Double)
  -- ^ Total CPU time spent in kernel mode.
  , resourceCPUsLimit                    :: !Double
  -- ^ Number of CPUs allocated.
  , resourceCPUsPeriods                  :: !(Maybe Word32)
  -- ^ cpu.stat on process throttling (for contention issues).
  , resourceCPUsThrottled                :: !(Maybe Word32)
  -- ^ cpu.stat on process throttling (for contention issues).
  , resourceCPUsThrottledTimeSecs        :: !(Maybe Double)
  -- ^ cpu.stat on process throttling (for contention issues).
  , resourceMemoryResidentSetSize        :: !(Maybe Word64)
  -- ^ Resident set size
  , resourceMemoryLimitBytes             :: !(Maybe Word64)
  -- ^ Amount of memory resources allocated.
  , resourceMemoryFileBytes              :: !(Maybe Word64)
  , resourceMemoryAnonymousBytes         :: !(Maybe Word64)
  , resourceMemoryMappedFileBytes        :: !(Maybe Word64)
  , resourcePerformanceStatistics        :: !(Maybe PerformanceStatistics)
  , resourceNetRxPackets                 :: !(Maybe Word64)
  , resourceNetRxBytes                   :: !(Maybe Word64)
  , resourceNetRxErrors                  :: !(Maybe Word64)
  , resourceNetRxDropped                 :: !(Maybe Word64)
  , resourceNetTxPackets                 :: !(Maybe Word64)
  , resourceNetTxBytes                   :: !(Maybe Word64)
  , resourceNetTxErrors                  :: !(Maybe Word64)
  , resourceNetTxDropped                 :: !(Maybe Word64)
  }
  deriving (Show, Eq)

-- | Describes a snapshot of the resource usage for an executor.
--
-- Resource usage is for an executor. For tasks launched with
-- an explicit executor, the executor id is provided. For tasks
-- launched without an executor, our internal executor will be
-- used. In this case, we provide the task id here instead, in
-- order to make this message easier for schedulers to work with.
data ResourceUsage = ResourceUsage
  { resourceUsageSlaveID      :: !SlaveID
  , resourceUsageFrameworkID  :: !FrameworkID
  , resourceUsageExecutorID   :: !(Maybe ExecutorID) -- ^ If present, this executor was explicitly specified.
  , resourceUsageExecutorName :: !(Maybe ByteString) -- ^ If present, this executor was explicitly specified.
  , resourceUsageTaskID       :: !(Maybe TaskID) -- ^ If present, this task did not have an executor.
  , resourceUsageStatistics   :: !(Maybe ResourceStatistics)
  -- ^  If missing, the isolation module cannot provide resource usage.
  }
  deriving (Show, Eq)

data PerformanceStatistics = PerformanceStatistics
  { performanceStatisticsTimestamp              :: !Double -- ^ Start of sample interval, in seconds since the Epoch.
  , performanceStatisticsDuration               :: !Double -- ^ Duration of sample interval, in seconds.

  -- | Hardware events
  , performanceStatisticsCycles                 :: !(Maybe Word64)
  , performanceStatisticsStalledCyclesFrontend  :: !(Maybe Word64)
  , performanceStatisticsStalledCyclesBackend   :: !(Maybe Word64)
  , performanceStatisticsInstructions           :: !(Maybe Word64)
  , performanceStatisticsCacheReferences        :: !(Maybe Word64)
  , performanceStatisticsCacheMisses            :: !(Maybe Word64)
  , performanceStatisticsBranches               :: !(Maybe Word64)
  , performanceStatisticsBranchMisses           :: !(Maybe Word64)
  , performanceStatisticsBusCycles              :: !(Maybe Word64)
  , performanceStatisticsRefCycles              :: !(Maybe Word64)

  -- | Software events
  , performanceStatisticsCpuClock               :: !(Maybe Double)
  , performanceStatisticsTaskClock              :: !(Maybe Double)
  , performanceStatisticsPageFaults             :: !(Maybe Word64)
  , performanceStatisticsMinorFaults            :: !(Maybe Word64)
  , performanceStatisticsMajorFaults            :: !(Maybe Word64)
  , performanceStatisticsContextSwitches        :: !(Maybe Word64)
  , performanceStatisticsCpuMigrations          :: !(Maybe Word64)
  , performanceStatisticsAlignmentFaults        :: !(Maybe Word64)
  , performanceStatisticsEmulationFaults        :: !(Maybe Word64)

  -- | Hardware cache events
  , performanceStatisticsL1DcacheLoads          :: !(Maybe Word64)
  , performanceStatisticsL1DcacheLoadMisses     :: !(Maybe Word64)
  , performanceStatisticsL1DcacheStores         :: !(Maybe Word64)
  , performanceStatisticsL1DcacheStoreMisses    :: !(Maybe Word64)
  , performanceStatisticsL1DcachePrefetches     :: !(Maybe Word64)
  , performanceStatisticsL1DcachePrefetchMisses :: !(Maybe Word64)
  , performanceStatisticsL1IcacheLoads          :: !(Maybe Word64)
  , performanceStatisticsL1IcacheLoadMisses     :: !(Maybe Word64)
  , performanceStatisticsL1IcachePrefetches     :: !(Maybe Word64)
  , performanceStatisticsL1IcachePrefetchMisses :: !(Maybe Word64)
  , performanceStatisticsLlcLoads               :: !(Maybe Word64)
  , performanceStatisticsLlcLoadMisses          :: !(Maybe Word64)
  , performanceStatisticsLlcStores              :: !(Maybe Word64)
  , performanceStatisticsLlcStoreMisses         :: !(Maybe Word64)
  , performanceStatisticsLlcPrefetches          :: !(Maybe Word64)
  , performanceStatisticsLlcPrefetchMisses      :: !(Maybe Word64)
  , performanceStatisticsDtlbLoads              :: !(Maybe Word64)
  , performanceStatisticsDtlbLoadMisses         :: !(Maybe Word64)
  , performanceStatisticsDtlbStores             :: !(Maybe Word64)
  , performanceStatisticsDtlbStoreMisses        :: !(Maybe Word64)
  , performanceStatisticsDtlbPrefetches         :: !(Maybe Word64)
  , performanceStatisticsDtlbPrefetchMisses     :: !(Maybe Word64)
  , performanceStatisticsItlbLoads              :: !(Maybe Word64)
  , performanceStatisticsItlbLoadMisses         :: !(Maybe Word64)
  , performanceStatisticsBranchLoads            :: !(Maybe Word64)
  , performanceStatisticsBranchLoadMisses       :: !(Maybe Word64)
  , performanceStatisticsNodeLoads              :: !(Maybe Word64)
  , performanceStatisticsNodeLoadMisses         :: !(Maybe Word64)
  , performanceStatisticsNodeStores             :: !(Maybe Word64)
  , performanceStatisticsNodeStoreMisses        :: !(Maybe Word64)
  , performanceStatisticsNodePrefetches         :: !(Maybe Word64)
  , performanceStatisticsNodePrefetchMisses     :: !(Maybe Word64)
  } deriving (Show, Eq)

-- | Describes a request for resources that can be used by a framework
-- to proactively influence the allocator.
data Request = Request
  { requestSlaveID :: !(Maybe SlaveID) -- ^ If value is provided, then this request is assumed to only apply to resources on the given slave.
  , reqResources   :: ![Resource]
  }
  deriving (Show, Eq)

-- | Describes some resources available on a slave. An offer only
-- contains resources from a single slave.
data Offer = Offer
  { offerID          :: !OfferID
  , offerFrameworkID :: !FrameworkID
  , offerSlaveID     :: !SlaveID
  , offerHostname    :: !ByteString
  , offerResources   :: ![Resource]
  , offerAttributes  :: ![(ByteString, Value)]
  , offerExecutorIDs :: ![ExecutorID]
  }
  deriving (Show, Eq)

data TaskExecutionInfo
  = TaskCommand !CommandInfo
  | TaskExecutor !ExecutorInfo
  deriving (Eq, Show)

-- | Describes a task. Passed from the scheduler all the way to an
-- executor (see SchedulerDriver::launchTasks and
-- Executor::launchTask).
--
-- A different executor can be used to launch this task, and subsequent tasks
-- meant for the same executor can reuse the same ExecutorInfo struct.
data TaskInfo = TaskInfo
  { taskInfoName       :: !ByteString
  , taskID             :: !TaskID
  , taskSlaveID        :: !SlaveID
  , taskResources      :: ![Resource]
  , taskImplementation :: !TaskExecutionInfo
  , taskData           :: !(Maybe ByteString)
  -- | Task provided with a container will launch the container as part
  -- of this task paired with the task's CommandInfo.
  , taskContainer      :: !(Maybe ContainerInfo)
  -- | A health check for the task (currently in *alpha* and initial
  -- support will only be for TaskInfo's that have a CommandInfo).
  , taskHealthCheck    :: !(Maybe HealthCheck)
  }
  deriving (Show, Eq)

-- | Describes the current status of a task.
data TaskStatus = TaskStatus
  { taskStatusTaskID     :: !TaskID
  , taskStatusState      :: !TaskState
  , taskStatusMessage    :: !(Maybe ByteString) -- ^ Possible message explaining state.
  , taskStatusData       :: !(Maybe ByteString)
  , taskStatusSlaveID    :: !(Maybe SlaveID)
  , taskStatusExecutorID :: !(Maybe ExecutorID)
  , taskStatusTimestamp  :: !(Maybe Double)
  -- | Describes whether the task has been determined to be healthy
  -- (true) or unhealthy (false) according to the HealthCheck field in
  -- the command info.
  , taskStatusHealthy    :: !(Maybe Bool)
  }
  deriving (Show, Eq)

-- | Credential used for authentication.
--
-- NOTE: 'credentialPrincipal' is used for authenticating the framework with
-- the master. This is different from 'frameworkUser'
-- which is used to determine the user under which the framework's
-- executors/tasks are run.
data Credential = Credential
  { credentialPrincipal :: !ByteString
  , credentialSecret    :: !(Maybe ByteString)
  }
  deriving (Show, Eq)


-- | Entity is used to describe a subject(s) or an object(s) of an ACL.
-- NOTE:
-- To allow everyone access to an Entity set its type to 'ANY'.
-- To deny access to an Entity set its type to 'NONE'.
data ACLEntity = Some ![ByteString]
               | Any
               | None
               deriving (Show, Eq)
data RegisterFrameworkACL = RegisterFrameworkACL { registerFrameworkPrincipals :: ACLEntity, registerFrameworkRoles :: ACLEntity } deriving (Show, Eq)
data RunTaskACL = RunTaskACL { runTaskPrincipals :: ACLEntity, runTaskUsers :: ACLEntity } deriving (Show, Eq)
data ShutdownFrameworkACL = ShutdownFrameworkACL { shutdownFrameworkPrincipals :: ACLEntity, shutdownFrameworkFrameworkPrincipals :: ACLEntity } deriving (Show, Eq)

data ACLSettings = ACLSettings
  { aclSettingsPermissive         :: !(Maybe Bool)
  , aclSettingsRegisterFrameworks :: ![RegisterFrameworkACL]
  , aclSettingsRunTasks           :: ![RunTaskACL]
  , aclSettingsShutdownFramework  :: ![ShutdownFrameworkACL]
  } deriving (Show, Eq)

data RateLimit = RateLimit
  { rateLimitQPS       :: !(Maybe Double)
  , rateLimitPrincipal :: !ByteString
  , rateLimitCapacity  :: !(Maybe Word64)
  } deriving (Show, Eq)

data RateLimits = RateLimits
  { rateLimits                         :: ![RateLimit]
  , rateLimitsAggregateDefaultQPS      :: !(Maybe Double)
  , rateLimitsAggregateDefaultCapacity :: !(Maybe Double)
  } deriving (Show, Eq)

data Mode = ReadWrite -- ^ Mount the volume in R/W mode
          | ReadOnly  -- ^ Mount the volume as read-only
  deriving (Show, Eq)

instance Enum Mode where
  fromEnum ReadWrite = 1
  fromEnum ReadOnly = 2
  toEnum 1 = ReadWrite
  toEnum 2 = ReadOnly
  toEnum _ = error "Unsupported volume mode"

data Volume = Volume
  { volumeContainerPath :: !ByteString
  , volumeHostPath      :: !(Maybe ByteString)
  , volumeMode          :: !Mode
  } deriving (Show, Eq)

data ContainerType = Docker { dockerImage :: ByteString }
                   | Unknown Int -- ^ Not technically a container type. Represents the 'type' enum field if we get a container type that isn't Docker (e.g. from Mesos releases > 0.20)
  deriving (Show, Eq)

data ContainerInfo = ContainerInfo
  { containerInfoContainerType :: !ContainerType
  , containerInfoVolumes       :: ![Volume]
  } deriving (Show, Eq)
