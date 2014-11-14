{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}
-- | Currently just raw TH-generated 'Lens'es and 'Prism's. FUNCTION & CLASS NAMES IN THIS FILE ARE EXTREMELY LIKELY TO CHANGE IN FUTURE RELEASES.
module System.Mesos.Lens where

import           Control.Lens.TH
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
makeFields ''HealthCheck
makeFields ''CommandInfo
makeFields ''CommandURI
makePrisms ''CommandValue
makeFields ''ExecutorInfo
makeFields ''MasterInfo
makeFields ''SlaveInfo
makeFields ''Filters
makePrisms ''Value
makeFields ''Resource
makeFields ''ResourceStatistics
makeFields ''ResourceUsage
makeFields ''PerformanceStatistics
makeFields ''Request
makeFields ''Offer
makePrisms ''TaskExecutionInfo
makeFields ''TaskInfo
makeFields ''TaskStatus
makeFields ''Credential
makePrisms ''Mode
makeFields ''Volume
makeFields ''ContainerType
makePrisms ''ContainerType
makeFields ''ContainerInfo
