{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}
-- | Currently just raw TH-generated 'Lens'es and 'Prism's. FUNCTION & CLASS NAMES IN THIS FILE ARE EXTREMELY LIKELY TO CHANGE IN FUTURE RELEASES.
module System.Mesos.Lens where

import           Control.Lens.TH
import           System.Mesos.Internal (makePrefixFields)
import           System.Mesos.Types

makePrisms ''TaskState
makePrisms ''Status
makeFields ''FrameworkID
makeFields ''OfferID
makeFields ''SlaveID
makeFields ''TaskID
makeFields ''ExecutorID
makeFields ''ContainerID
makeFields ''FrameworkInfo
makeFields ''HealthCheckStrategy
makePrisms ''HealthCheckStrategy
makePrefixFields "healthCheck" ''HealthCheck
makePrefixFields "command" ''CommandInfo
makePrefixFields "commandURI" ''CommandURI
makePrisms ''CommandValue
makePrefixFields "executorInfo" ''ExecutorInfo
makePrefixFields "masterInfo" ''MasterInfo
makePrefixFields "slaveInfo" ''SlaveInfo
makeLenses ''Filters
makePrisms ''Value
makeFields ''Resource
makePrefixFields "resource" ''ResourceStatistics
makeFields ''ResourceUsage
makePrefixFields "performanceStatistics" ''PerformanceStatistics
makeFields ''Request
makeFields ''Offer
makePrisms ''TaskExecutionInfo
makeFields ''TaskInfo
makePrefixFields "taskStatus" ''TaskStatus
makeFields ''Credential
makePrisms ''Mode
makeFields ''Volume
makeFields ''ContainerType
makePrisms ''ContainerType
makePrefixFields "containerInfo" ''ContainerInfo
