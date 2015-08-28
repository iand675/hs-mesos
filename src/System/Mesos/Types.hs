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
  taskInfo,
  TaskExecutionInfo(..),
  CommandInfo(..),
  commandInfo,
  CommandURI(..),
  commandURI,
  CommandValue (..),
  Value(..),
  Resource(..),
  resource,
  -- ** Traffic Control Statistics
  TrafficControlStatistics(..),
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
  ResourceUsageExecutor(..),
  PerformanceStatistics(..),
  -- ** Task Status
  -- ** Credentials & ACLs
  Credential(..),
  credential,
  -- ** Port
  Port(..),
  -- ** Discovery Info
  DiscoveryInfo(..)
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
newtype FrameworkID = FrameworkID { frameworkIDId' :: ByteString }
  deriving (Show, Eq, IsString)

-- | A unique ID assigned to an offer.
newtype OfferID = OfferID { offerIDId' :: ByteString }
  deriving (Show, Eq, IsString)

-- |  A unique ID assigned to a slave. Currently, a slave gets a new ID whenever it (re)registers with Mesos. Framework writers shouldn't assume any binding between a slave ID and and a hostname.
newtype SlaveID = SlaveID { slaveIDId' :: ByteString }
  deriving (Show, Eq, IsString)

-- |  A framework generated ID to distinguish a task. The ID must remain
-- unique while the task is active. However, a framework can reuse an
-- ID _only_ if a previous task with the same ID has reached a
-- terminal state (e.g., 'Finished', 'Lost', 'Killed', etc.). See 'isTerminal' for a utility function to simplify checking task state.
newtype TaskID = TaskID { taskIDId' :: ByteString }
  deriving (Show, Eq, IsString)

-- |  A framework generated ID to distinguish an executor. Only one
-- executor with the same ID can be active on the same slave at a
-- time.
newtype ExecutorID = ExecutorID { executorIDId' :: ByteString }
  deriving (Show, Eq, IsString)


-- |  A slave generated ID to distinguish a container. The ID must be unique
-- between any active or completed containers on the slave. In particular,
-- containers for different runs of the same (framework, executor) pair must be
-- unique.
newtype ContainerID = ContainerID { containerIDId' :: ByteString }
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
  { frameworkInfoUser            :: !ByteString
  , frameworkInfoName            :: !ByteString
  , frameworkInfoId'             :: !(Maybe FrameworkID)
  , frameworkInfoFailoverTimeout :: !(Maybe Double)
  , frameworkInfoCheckpoint      :: !(Maybe Bool)
  , frameworkInfoRole            :: !(Maybe ByteString)
  , frameworkInfoHostname        :: !(Maybe ByteString)
  , frameworkInfoPrincipal       :: !(Maybe ByteString)
  -- TODO:
  -- , frameworkInfoWebUIURL       :: !(Maybe ByteString)
  }
  deriving (Show, Eq)

frameworkInfo :: ByteString -> ByteString -> FrameworkInfo
frameworkInfo u n = FrameworkInfo u n Nothing Nothing Nothing Nothing Nothing Nothing

data HealthCheckStrategy
   = HTTPCheck -- ^ Describes an HTTP health check. This is not fully implemented and not recommended for use - see MESOS-2533.
     { healthCheckStrategyPort     :: !Word32 -- ^ Port to send the HTTP request.
     , healthCheckStrategyPath     :: !(Maybe ByteString) -- ^ HTTP request path. (defaults to @"/"@.
     , healthCheckStrategyStatuses :: ![Word32] -- ^ Expected response statuses. Not specifying any statuses implies that any returned status is acceptable.
     }
   | CommandCheck
     { healthCheckStrategyCommand :: !CommandInfo -- ^ Command health check.
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
  { commandInfoUris        :: ![CommandURI]
  , commandInfoEnvironment :: !(Maybe [(ByteString, ByteString)])
  , commandInfoValue       :: !CommandValue
  -- TODO: Existing field in 0.20, but doesn't actually work
  -- , commandContainer   :: !(Maybe ContainerInfo)

  , commandInfoUser        :: !(Maybe ByteString)
  -- ^ Enables executor and tasks to run as a specific user. If the user
  -- field is present both in 'FrameworkInfo' and here, the 'CommandInfo'
  -- user value takes precedence.
  }
  deriving (Show, Eq)

commandInfo :: CommandValue -> CommandInfo
commandInfo v = CommandInfo [] Nothing v Nothing

data CommandURI = CommandURI
  { commandURIValue      :: !ByteString
  , commandURIExecutable :: !(Maybe Bool)

  , commandURIExtract    :: !(Maybe Bool)
    -- ^ In case the fetched file is recognized as an archive, extract
    -- its contents into the sandbox. Note that a cached archive is
    -- not copied from the cache to the sandbox in case extraction
    -- originates from an archive in the cache.
  , commandURICache      :: !(Maybe Bool)
    -- ^ If this field is "true", the fetcher cache will be used. If not,
    -- fetching bypasses the cache and downloads directly into the
    -- sandbox directory, no matter whether a suitable cache file is
    -- available or not. The former directs the fetcher to download to
    -- the file cache, then copy from there to the sandbox. Subsequent
    -- fetch attempts with the same URI will omit downloading and copy
    -- from the cache as long as the file is resident there. Cache files
    -- may get evicted at any time, which then leads to renewed
    -- downloading.
  }
  deriving (Show, Eq)

commandURI :: ByteString -> CommandURI
commandURI v = CommandURI v Nothing Nothing Nothing

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

-- | Describes information about an executor. The 'executorData' field can be
-- used to pass arbitrary bytes to an executor.
data ExecutorInfo = ExecutorInfo
  { executorInfoExecutorId    :: !ExecutorID
  , executorInfoFrameworkId   :: !FrameworkID
  , executorInfoCommandInfo   :: !CommandInfo
  , executorInfoContainerInfo :: !(Maybe ContainerInfo)
  -- ^ Executor provided with a container will launch the container
  -- with the executor's 'CommandInfo' and we expect the container to
  -- act as a Mesos executor.
  , executorInfoResources         :: ![Resource]
  , executorInfoName              :: !(Maybe ByteString)
  , executorInfoSource            :: !(Maybe ByteString)
  -- ^ Source is an identifier style string used by frameworks to track
  -- the source of an executor. This is useful when it's possible for
  -- different executor ids to be related semantically.
  --
  -- NOTE: Source is exposed alongside the resource usage of the
  -- executor via JSON on the slave. This allows users to import
  -- usage information into a time series database for monitoring.
  , executorInfoDiscover      :: !(Maybe DiscoveryInfo)
  -- ^ Service discovery information for the executor. It is not
  -- interpreted or acted upon by Mesos. It is up to a service
  -- discovery system to use this information as needed and to handle
  -- executors without service discovery information.
  }
  deriving (Show, Eq)

executorInfo :: ExecutorID -> FrameworkID -> CommandInfo -> [Resource] -> ExecutorInfo
executorInfo eid fid ci rs = ExecutorInfo eid fid ci Nothing rs Nothing Nothing Nothing

-- | Describes a master. This will probably have more fields in the
-- future which might be used, for example, to link a framework web UI
-- to a master web UI.
data MasterInfo = MasterInfo
  { masterInfoId'      :: !ByteString
  , masterInfoIp       :: !Word32
  , masterInfoPort     :: !(Maybe Word32) -- ^ Defaults to 5050
  , masterInfoPid      :: !(Maybe ByteString)
  , masterInfoHostname :: !(Maybe ByteString)
  , masterInfoVersion  :: !(Maybe ByteString)
  }
  deriving (Show, Eq)

masterInfo :: ByteString -> Word32 -> MasterInfo
masterInfo mid ip = MasterInfo mid ip Nothing Nothing Nothing Nothing

-- | Describes a slave. Note that the 'slaveInfoSlaveID' field is only available after
-- a slave is registered with the master, and is made available here
-- to facilitate re-registration.  If checkpoint is set, the slave is
-- checkpointing its own information and potentially frameworks'
-- information (if a framework has checkpointing enabled).
data SlaveInfo = SlaveInfo
  { slaveInfoHostname   :: !ByteString
  , slaveInfoPort       :: !(Maybe Word32) -- ^Defaults to 5051
  , slaveInfoResources  :: ![Resource]
  , slaveInfoAttributes :: ![(ByteString, Value)]
  , slaveInfoSlaveId    :: !(Maybe SlaveID)
  , slaveInfoCheckpoint :: !(Maybe Bool)
  }
  deriving (Show, Eq)

slaveInfo :: ByteString -> [Resource] -> [(ByteString, Value)] -> SlaveInfo
slaveInfo hn rs as = SlaveInfo hn Nothing rs as Nothing Nothing

newtype Filters = Filters
  { filtersRefuseSeconds :: Maybe Double
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
  { resourceName                     :: !ByteString
  , resourceValue                    :: !Value
  , resourceRole                     :: !(Maybe ByteString)
  , resourceReservationInfoPrincipal :: !(Maybe ByteString)
  -- ^ Describes a dynamic reservation. A dynamic reservation is
  -- acquired by an operator via the '/reserve' HTTP endpoint or by
  -- a framework via the offer cycle by sending back an
  -- 'Offer::Operation::Reserve' message.
  -- NOTE: We currently do not allow frameworks with role "*" to
  -- make dynamic reservations.

  -- | Describes a persistent disk volume.
  -- A persistent disk volume will not be automatically garbage
  -- collected if the task/executor/slave terminates, but is
  -- re-offered to the framework(s) belonging to the 'role'.
  -- A framework can set the ID (if it is not set yet) to express
  -- the intention to create a new persistent disk volume from a
  -- regular disk resource. To reuse a previously created volume, a
  -- framework can launch a task/executor when it receives an offer
  -- with a persistent volume, i.e., ID is set.
  -- NOTE: Currently, we do not allow a persistent disk volume
  -- without a reservation (i.e., 'role' should not be '*').
  , resourceDiskInfoPersistence      :: !(Maybe ByteString)
  -- ^ A unique ID for the persistent disk volume.
  -- NOTE: The ID needs to be unique per role on each slave.
  , resourceDiskInfoVolume           :: !(Maybe Volume)
  -- ^ Describes how this disk resource will be mounted in the
  -- container. If not set, the disk resource will be used as the
  -- sandbox. Otherwise, it will be mounted according to the
  -- 'container_path' inside 'volume'. The 'host_path' inside
  -- 'volume' is ignored.
  -- NOTE: If 'volume' is set but 'persistence' is not set, the
  -- volume will be automatically garbage collected after
  -- task/executor terminates. Currently, if 'persistence' is set,
  -- 'volume' must be set.
  }
  deriving (Show, Eq)

resource :: ByteString -> Value -> Resource
resource n v = Resource n v Nothing Nothing Nothing Nothing

-- | When the network bandwidth caps are enabled and the container
-- is over its limit, outbound packets may be either delayed or
-- dropped completely either because it exceeds the maximum bandwidth
-- allocation for a single container (the cap) or because the combined
-- network traffic of multiple containers on the host exceeds the
-- transmit capacity of the host (the share). We can report the
-- following statistics for each of these conditions exported directly
-- from the Linux Traffic Control Queueing Discipline.
--
-- id         : name of the limiter, e.g. 'tx_bw_cap'
-- backlog    : number of packets currently delayed
-- bytes      : total bytes seen
-- drops      : number of packets dropped in total
-- overlimits : number of packets which exceeded allocation
-- packets    : total packets seen
-- qlen       : number of packets currently queued
-- rate_bps   : throughput in bytes/sec
-- rate_pps   : throughput in packets/sec
-- requeues   : number of times a packet has been delayed due to
--              locking or device contention issues
--
-- More information on the operation of Linux Traffic Control can be
-- found at http://www.lartc.org/lartc.html.
data TrafficControlStatistics = TrafficControlStatistics
  { trafficControlStatisticsIDId'      :: !ByteString
  , trafficControlStatisticsBacklog    :: !(Maybe Word64)
  , trafficControlStatisticsBytes      :: !(Maybe Word64)
  , trafficControlStatisticsDrops      :: !(Maybe Word64)
  , trafficControlStatisticsOverlimits :: !(Maybe Word64)
  , trafficControlStatisticsPackets    :: !(Maybe Word64)
  , trafficControlStatisticsQlen       :: !(Maybe Word64)
  , trafficControlStatisticsRatebps    :: !(Maybe Word64)
  , trafficControlStatisticsRatepps    :: !(Maybe Word64)
  , trafficControlStatisticsRequeues   :: !(Maybe Word64)
  } deriving (Show, Eq)


-- | A snapshot of resource usage statistics.
data ResourceStatistics = ResourceStatistics
  { resourceStatisticsTimestamp          :: !Double
  , resourceStatisticsProcesses          :: !(Maybe Word32)
  , resourceStatisticsThreads            :: !(Maybe Word32)
  , resourceStatisticsCpusUserTimeSecs   :: !(Maybe Double)
  -- ^ Total CPU time spent in user mode
  , resourceStatisticsCpusSystemTimeSecs :: !(Maybe Double)
  -- ^ Total CPU time spent in kernel mode.
  , resourceStatisticsCpusLimit                    :: !Double
  -- ^ Number of CPUs allocated.
  , resourceStatisticsCpusPeriods                  :: !(Maybe Word32)
  -- ^ cpu.stat on process throttling (for contention issues).
  , resourceStatisticsCpusThrottled                :: !(Maybe Word32)
  -- ^ cpu.stat on process throttling (for contention issues).
  , resourceStatisticsCpusThrottledTimeSecs        :: !(Maybe Double)
  -- ^ cpu.stat on process throttling (for contention issues).
  , resourceStatisticsMemoryTotalBytes             :: !(Maybe Word64)
  -- ^ MemoryTotalBytes was added in 0.23.0 to represent the total memory
  -- of a process in RAM (as opposed to in Swap). This was previously
  -- reported as mem_rss_bytes, which was also changed in 0.23.0 to
  -- represent only the anonymous memory usage, to keep in sync with
  -- Linux kernel's (arguably erroneous) use of terminology.
  , resourceStatisticsMemoryTotalMemSwBytes        :: !(Maybe Word64)
  -- ^ Total memory + swap usage. This is set if swap is enabled.
  , resourceStatisticsMemoryLimitBytes             :: !(Maybe Word64)
  -- ^ Amount of memory resources allocated.
  , resourceStatisticsMemorySoftLimitBytes         :: !(Maybe Word64)
  -- ^ Soft memory limit for a container.
  , resourceStatisticsMemoryFileBytes              :: !(Maybe Word64)
  -- ^ Deprecated in 0.23.0, will be removed in 0.24.0
  , resourceStatisticsMemoryAnonymousBytes         :: !(Maybe Word64)
  -- ^ Deprecated in 0.23.0, will be removed in 0.24.0
  , resourceStatisticsMemoryCacheBytes             :: !(Maybe Word64)
  -- ^ mem_cache_bytes is added in 0.23.0 to represent page cache usage.
  , resourceStatisticsMemoryRssBytes               :: !(Maybe Word64)
  -- ^ Since 0.23.0, mem_rss_bytes is changed to represent only
  -- anonymous memory usage. Note that neither its requiredness, type,
  -- name nor numeric tag has been changed.
  , resourceStatisticsMemoryMappedFileBytes        :: !(Maybe Word64)
  , resourceStatisticsMemorySwapBytes              :: !(Maybe Word64)
  -- ^ This is only set if swap is enabled.
  , resourceStatisticsMemoryLowPressureCounter     :: !(Maybe Word64)
  -- ^ Number of occurrences of different levels of memory pressure
  -- events reported by memory cgroup. Pressure listening (re)starts
  -- with these values set to 0 when slave (re)starts. See
  -- https://www.kernel.org/doc/Documentation/cgroups/memory.txt for
  -- more details.
  , resourceStatisticsMemoryMediumPressureCounter  :: !(Maybe Word64)
  , resourceStatisticsMemoryCriticalPressureCounter :: !(Maybe Word64)
  , resourceStatisticsDiskLimitBytes               :: !(Maybe Word64)
  -- ^ Disk Usage Information for executor working directory.
  , resourceStatisticsDiskUsedBytes                :: !(Maybe Word64)
  -- ^ Disk Usage Information for executor working directory.
  , resourceStatisticsPerformanceStatistics        :: !(Maybe PerformanceStatistics)
  , resourceStatisticsNetRxPackets                 :: !(Maybe Word64)
  , resourceStatisticsNetRxBytes                   :: !(Maybe Word64)
  , resourceStatisticsNetRxErrors                  :: !(Maybe Word64)
  , resourceStatisticsNetRxDropped                 :: !(Maybe Word64)
  , resourceStatisticsNetTxPackets                 :: !(Maybe Word64)
  , resourceStatisticsNetTxBytes                   :: !(Maybe Word64)
  , resourceStatisticsNetTxErrors                  :: !(Maybe Word64)
  , resourceStatisticsNetTxDropped                 :: !(Maybe Word64)

  , resourceStatisticsNetTcpRttMicroSecsP50        :: !(Maybe Double)
  -- ^ The kernel keeps track of RTT (round-trip time) for its TCP
  -- sockets. RTT is a way to tell the latency of a container.
  , resourceStatisticsNetTcpRttMicroSecsP90        :: !(Maybe Double)
  , resourceStatisticsNetTcpRttMicroSecsP95        :: !(Maybe Double)
  , resourceStatisticsNetTcpRttMicroSecsP99        :: !(Maybe Double)

  , resourceStatisticsNetTcpActiveConnections      :: !(Maybe Double)
  , resourceStatisticsNetTcpTimeWaiConnections     :: !(Maybe Double)

  , resourceStatisticsNetTrafficControlStats       :: ![TrafficControlStatistics]
  -- ^ Network traffic flowing into or out of a container can be delayed
  -- or dropped due to congestion or policy inside and outside the
  -- container.
  }
  deriving (Show, Eq)

-- | Describes a snapshot of the resource usage for an executor.
data ResourceUsage = ResourceUsage
  { resourceUsageExecutors :: ![ResourceUsageExecutor]
  }
  deriving (Show, Eq)

data ResourceUsageExecutor = ResourceUsageExecutor
  { resourceUsageExecutorExecutorInfo :: !ExecutorInfo
  , resourceUsageExecutorAllocated    :: ![Resource]
  -- ^ This includes resources used by the executor itself
  -- as well as its active tasks.
  , resourceUsageExecutorStatistics   :: !(Maybe ResourceStatistics)
  -- ^ Current resource usage. If absent, the containerizer
  -- cannot provide resource usage.
  } deriving (Show, Eq)

-- |Describes a sample of events from "perf stat". Only available on
-- Linux.
--
-- NOTE: Each optional field matches the name of a perf event (see
-- "perf list") with the following changes:
-- 1. Names are downcased.
-- 2. Hyphens ('-') are replaced with underscores ('_').
-- 3. Events with alternate names use the name "perf stat" returns,
--    e.g., for the event "cycles OR cpu-cycles" perf always returns
--    cycles.
data PerformanceStatistics = PerformanceStatistics
  { performanceStatisticsTimestamp              :: !Double
  -- ^ Start of sample interval, in seconds since the Epoch.
  , performanceStatisticsDuration               :: !Double
  -- ^ Duration of sample interval, in seconds.

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
  { requestSlaveId :: !(Maybe SlaveID) -- ^ If value is provided, then this request is assumed to only apply to resources on the given slave.
  , requestResources   :: ![Resource]
  }
  deriving (Show, Eq)

-- | Describes some resources available on a slave. An offer only
-- contains resources from a single slave.
data Offer = Offer
  { offerId'         :: !OfferID
  , offerFrameworkId :: !FrameworkID
  , offerSlaveId     :: !SlaveID
  , offerHostname    :: !ByteString
  , offerResources   :: ![Resource]
  , offerAttributes  :: ![(ByteString, Value)]
  , offerExecutorIds :: ![ExecutorID]
  }
  deriving (Show, Eq)

-- Not used ?!
{- data OfferOperation = OfferLaunch ![TaskInfo]
                    | OfferReserve ![Resource] -- resources
                    | OfferUnreserver ![Resource] -- resources
                    | OfferCreate ![Resource] -- volumes
                    | OfferDestroy ![Resource] -- volumes
                    deriving (Show, Eq)
-}

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
  { taskInfoName           :: !ByteString
  , taskInfoId'            :: !TaskID
  , taskInfoSlaveId        :: !SlaveID
  , taskInfoResources      :: ![Resource]
  , taskInfoImplementation :: !TaskExecutionInfo
  , taskInfoData_          :: !(Maybe ByteString)
  , taskInfoContainer      :: !(Maybe ContainerInfo)
  -- ^ Task provided with a container will launch the container as part
  -- of this task paired with the task's CommandInfo.
  , taskInfoHealthCheck    :: !(Maybe HealthCheck)
  -- ^ A health check for the task (currently in *alpha* and initial
  -- support will only be for TaskInfo's that have a CommandInfo).
  , taskInfoLabels         :: ![(ByteString, Maybe ByteString)]
  -- ^ Labels are free-form key value pairs which are exposed through
  -- master and slave endpoints. Labels will not be interpreted or
  -- acted upon by Mesos itself. As opposed to the data field, labels
  -- will be kept in memory on master and slave processes. Therefore,
  -- labels should be used to tag tasks with light-weight meta-data.
  , taskInfoDiscovery      :: !(Maybe DiscoveryInfo)
  }
  deriving (Show, Eq)

taskInfo :: ByteString -> TaskID -> SlaveID -> [Resource] -> TaskExecutionInfo -> TaskInfo
taskInfo n t s rs i = TaskInfo n t s rs i Nothing Nothing Nothing [] Nothing

-- | Describes the current status of a task.
data TaskStatus = TaskStatus
  { taskStatusTaskId     :: !TaskID
  , taskStatusState      :: !TaskState
  , taskStatusMessage    :: !(Maybe ByteString) -- ^ Possible message explaining state.
  , taskStatusSource     :: !(Maybe TaskStatusSource)
  , taskStatusReason     :: !(Maybe TaskStatusReason)
  , taskStatusData_      :: !(Maybe ByteString)
  , taskStatusSlaveId    :: !(Maybe SlaveID)
  , taskStatusExecutorId :: !(Maybe ExecutorID)
  , taskStatusTimestamp  :: !(Maybe Double)
  , taskStatusUUID       :: !(Maybe ByteString)
  , taskStatusHealthy    :: !(Maybe Bool)
  -- ^ Describes whether the task has been determined to be healthy
  -- (true) or unhealthy (false) according to the HealthCheck field in
  -- the command info.
  }
  deriving (Show, Eq)

-- | Describes the source of the task status update.
data TaskStatusSource = SourceMaster
                      | SourceSlave
                      | SourceExecutor
                      deriving (Show, Eq)

instance Enum TaskStatusSource where
  fromEnum SourceMaster   = 0
  fromEnum SourceSlave    = 1
  fromEnum SourceExecutor = 2
  toEnum 0 = SourceMaster
  toEnum 1 = SourceSlave
  toEnum 2 = SourceExecutor
  toEnum _ = error "value not supported for TaskStatusSource"

-- | Detailed reason for the task status update.
data TaskStatusReason = ReasonCommandExecutorFailed
                      | ReasonExecutorPreempted
                      | ReasonExecutorTerminated
                      | ReasonExecutorUnregistered
                      | ReasonFrameworkRemoved
                      | ReasonGCError
                      | ReasonInvalidFrameworkId
                      | ReasonInvalidOffers
                      | ReasonMasterDisconnected
                      | ReasonMemoryLimit
                      | ReasonReconcilation
                      | ReasonResourcesUnkown
                      | ReasonSlaveDisconnected
                      | ReasonSlaveRemoved
                      | ReasonSlaveRestarted
                      | ReasonSlaveUnkown
                      | ReasonTaskInvalid
                      | ReasonTaskUnauthorized
                      | ReasonTaskUnknown
                      deriving (Show, Eq)

instance Enum TaskStatusReason where
  fromEnum ReasonCommandExecutorFailed = 0
  fromEnum ReasonExecutorPreempted     = 17
  fromEnum ReasonExecutorTerminated    = 1
  fromEnum ReasonExecutorUnregistered  = 2
  fromEnum ReasonFrameworkRemoved      = 3
  fromEnum ReasonGCError               = 4
  fromEnum ReasonInvalidFrameworkId    = 5
  fromEnum ReasonInvalidOffers         = 6
  fromEnum ReasonMasterDisconnected    = 7
  fromEnum ReasonMemoryLimit           = 8
  fromEnum ReasonReconcilation         = 9
  fromEnum ReasonResourcesUnkown       = 18
  fromEnum ReasonSlaveDisconnected     = 10
  fromEnum ReasonSlaveRemoved          = 11
  fromEnum ReasonSlaveRestarted        = 12
  fromEnum ReasonSlaveUnkown           = 13
  fromEnum ReasonTaskInvalid           = 14
  fromEnum ReasonTaskUnauthorized      = 15
  fromEnum ReasonTaskUnknown           = 16
  toEnum 0  = ReasonCommandExecutorFailed
  toEnum 17 = ReasonExecutorPreempted
  toEnum 1  = ReasonExecutorTerminated
  toEnum 2  = ReasonExecutorUnregistered
  toEnum 3  = ReasonFrameworkRemoved
  toEnum 4  = ReasonGCError
  toEnum 5  = ReasonInvalidFrameworkId
  toEnum 6  = ReasonInvalidOffers
  toEnum 7  = ReasonMasterDisconnected
  toEnum 8  = ReasonMemoryLimit
  toEnum 9  = ReasonReconcilation
  toEnum 18 = ReasonResourcesUnkown
  toEnum 10 = ReasonSlaveDisconnected
  toEnum 11 = ReasonSlaveRemoved
  toEnum 12 = ReasonSlaveRestarted
  toEnum 13 = ReasonSlaveUnkown
  toEnum 14 = ReasonTaskInvalid
  toEnum 15 = ReasonTaskUnauthorized
  toEnum 16 = ReasonTaskUnknown

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

data RegisterFrameworkACL = RegisterFrameworkACL
  { registerFrameworkACLPrincipals :: ACLEntity
  , registerFrameworkACLRoles :: ACLEntity
  } deriving (Show, Eq)

data RunTaskACL = RunTaskACL
  { runTaskACLPrincipals :: ACLEntity
  , runTaskACLUsers :: ACLEntity
  } deriving (Show, Eq)

data ShutdownFrameworkACL = ShutdownFrameworkACL
  { shutdownFrameworkACLPrincipals :: ACLEntity
  , shutdownFrameworkACLFrameworkPrincipals :: ACLEntity
  } deriving (Show, Eq)

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
  { rateLimitsRateLimits               :: ![RateLimit]
  , rateLimitsAggregateDefaultQPS      :: !(Maybe Double)
  , rateLimitsAggregateDefaultCapacity :: !(Maybe Word64)
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


-- | Named port used for service discovery.
data Port = Port
  { portNumber   :: !Word32
  , portName     :: !(Maybe ByteString)
  , portProtocal :: !(Maybe ByteString)
  } deriving (Show, Eq)

-- |  Service discovery information.
-- The visibility field restricts discovery within a framework
-- (FRAMEWORK), within a Mesos cluster (CLUSTER), or  places no
-- restrictions (EXTERNAL).
-- The environment, location, and version fields provide first class
-- support for common attributes used to differentiate between
-- similar services. The environment may receive values such as
-- PROD/QA/DEV, the location field may receive values like
-- EAST-US/WEST-US/EUROPE/AMEA, and the version field may receive
-- values like v2.0/v0.9. The exact use of these fields is up to each
-- service discovery system.
data DiscoveryInfo = DiscoveryInfo
  { discoveryInfoVisibility  :: !Visibility
  , discoveryInfoName        :: !(Maybe ByteString)
  , discoveryInfoEnvironment :: !(Maybe [(ByteString, ByteString)])
  , discoveryInfoLocation    :: !(Maybe ByteString)
  , discoveryInfoVersion     :: !(Maybe ByteString)
  , discoveryInfoPorts       :: ![Port]
  , discoveryInfoLabels      :: !(Maybe [(ByteString, Maybe ByteString)])
  } deriving (Show, Eq)

data Visibility = VisibilityFramework
                | VisibilityCluster
                | VisibilityExternal
                deriving (Show, Eq)

instance Enum Visibility where
  fromEnum VisibilityFramework = 0
  fromEnum VisibilityCluster   = 1
  fromEnum VisibilityExternal  = 2
  toEnum 0 = VisibilityFramework
  toEnum 1 = VisibilityCluster
  toEnum 2 = VisibilityExternal
