{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
module Main where
import           Control.Applicative
import           Control.Monad.Managed
import qualified Data.ByteString              as ByteString
import           Data.IORef
import           Foreign.C.Types
import           System.Mesos.Internal
import           System.Mesos.Raw.Attribute
import           System.Mesos.Raw.Environment
import           System.Mesos.Raw.Label
import           System.Mesos.Raw.Parameter
import           System.Mesos.Raw.Parameters
import           System.Mesos.Scheduler
import           System.Mesos.Types
import           Test.Tasty
import qualified Test.Tasty.HUnit             as Unit
import           Test.Tasty.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Monadic

markCalled r = modifyIORef r (+1)

beforeAndAfter :: (Show a, CPPValue a) => a -> IO a
beforeAndAfter x = do
  print x
  x' <- with (cppValue x >>= unmarshal) return
  print x'
  return x'

main :: IO ()
main = defaultMain $ testGroup "Mesos" [testIDs, executorTests, schedulerTests]

instance Arbitrary CUInt where
  arbitrary = CUInt <$> arbitrary
instance Arbitrary CULong where
  arbitrary = CULong <$> arbitrary
instance Arbitrary ByteString.ByteString where
  arbitrary = ByteString.pack <$> arbitrary
instance CoArbitrary ByteString.ByteString where
  coarbitrary = coarbitrary . ByteString.unpack
deriving instance Arbitrary FrameworkID
deriving instance Arbitrary OfferID
deriving instance Arbitrary SlaveID
deriving instance Arbitrary TaskID
deriving instance Arbitrary ExecutorID
deriving instance Arbitrary ContainerID
instance Arbitrary FrameworkInfo where
  arbitrary = FrameworkInfo
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary CommandURI where
  arbitrary = CommandURI
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary CommandValue where
  arbitrary = oneof
    [ ShellCommand <$> arbitrary
    , RawCommand <$> arbitrary <*> arbitrary
    ]

instance Arbitrary CommandInfo where
  arbitrary = CommandInfo <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ExecutorInfo where
  arbitrary = ExecutorInfo
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary MasterInfo where
  arbitrary = MasterInfo
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary SlaveInfo where
  arbitrary = SlaveInfo
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary Value where
  arbitrary = oneof
    [ Scalar <$> arbitrary
    , Ranges <$> listOf range
    , Set <$> listOf arbitrary
    , Text <$> arbitrary
    ]
    where range = arbitrary >>= \l -> arbitrary >>= \r -> return (l, r)

instance Arbitrary Attribute where
  arbitrary = Attribute <$> arbitrary <*> arbitrary

instance Arbitrary Resource where
  arbitrary = Resource
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary PerformanceStatistics where
  arbitrary = PerformanceStatistics
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary ResourceStatistics where
  arbitrary = ResourceStatistics
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary ResourceUsage where
  arbitrary = ResourceUsage
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary Request where
  arbitrary = Request
    <$> arbitrary
    <*> arbitrary

instance Arbitrary Offer where
  arbitrary = Offer
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary TaskExecutionInfo where
  arbitrary = oneof
    [ TaskCommand <$> arbitrary
    , TaskExecutor <$> arbitrary
    ]

instance Arbitrary Mode where
  arbitrary = oneof [pure ReadWrite, pure ReadOnly]

instance Arbitrary Volume where
  arbitrary = Volume <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ContainerType where
  arbitrary = Docker <$> arbitrary

instance Arbitrary ContainerInfo where
  arbitrary = ContainerInfo <$> arbitrary <*> arbitrary

instance Arbitrary HealthCheckStrategy where
  arbitrary = oneof
    [ CommandCheck <$> arbitrary
    , HTTPCheck <$> arbitrary <*> arbitrary <*> arbitrary
    ]

instance Arbitrary HealthCheck where
  arbitrary = HealthCheck
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary TaskInfo where
  arbitrary = TaskInfo
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary TaskState where
  arbitrary = elements
    [ Staging
    , Starting
    , TaskRunning
    , Finished
    , Failed
    , Killed
    , Lost
    ]

instance Arbitrary TaskStatusSource where
  arbitrary = elements [SourceExecutor, SourceMaster, SourceSlave]

instance Arbitrary TaskStatusReason where
  arbitrary = elements
    [ ReasonCommandExecutorFailed
    , ReasonExecutorPreempted
    , ReasonExecutorTerminated
    , ReasonExecutorUnregistered
    , ReasonFrameworkRemoved
    , ReasonGCError
    , ReasonInvalidFrameworkId
    , ReasonInvalidOffers
    , ReasonMasterDisconnected
    , ReasonMemoryLimit
    , ReasonReconcilation
    , ReasonResourcesUnkown
    , ReasonSlaveDisconnected
    , ReasonSlaveRemoved
    , ReasonSlaveRestarted
    , ReasonSlaveUnkown
    , ReasonTaskInvalid
    , ReasonTaskUnauthorized
    , ReasonTaskUnknown
    ]

instance Arbitrary TaskStatus where
  arbitrary = TaskStatus
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary Filters where
  arbitrary = Filters <$> arbitrary
instance Arbitrary Environment where
  arbitrary = Environment <$> arbitrary
instance Arbitrary Parameter where
  arbitrary = Parameter <$> arbitrary <*> arbitrary
instance Arbitrary Parameters where
  arbitrary = Parameters <$> arbitrary
instance Arbitrary Credential where
  arbitrary = Credential <$> arbitrary <*> arbitrary

instance Arbitrary Port where
  arbitrary = Port <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Label where
  arbitrary = Label <$> arbitrary

instance Arbitrary DiscoveryInfo where
  arbitrary = DiscoveryInfo
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary Visibility where
  arbitrary = elements
    [ VisibilityFramework
    , VisibilityCluster
    , VisibilityExternal
    ]

idempotentMarshalling :: (Show a, Eq a, CPPValue a) => a -> IO a
idempotentMarshalling x = with (cppValue x >>= unmarshal) return

prop_idempotentMarshalling :: (Show a, Eq a, CPPValue a) => Gen a -> Property
prop_idempotentMarshalling g = monadicIO $ forAllM g $ \x -> do
  res <- Test.QuickCheck.Monadic.run $ idempotentMarshalling x
  -- Test.QuickCheck.Monadic.run $ print x
  if equalExceptDefaults x res
    then assert True
    else do
      Test.QuickCheck.Monadic.run $ do
        putStrLn "Original"
        print x
        putStrLn "Unmarshalled"
        print res
      assert False

qcIM :: (Show a, Eq a, CPPValue a) => String -> Gen a -> TestTree
qcIM n = testProperty n . prop_idempotentMarshalling

testIDs = testGroup "Marshalling"
  [ qcIM "ExecutorInfo" (arbitrary :: Gen ExecutorInfo)
  , qcIM "TaskInfo" (arbitrary :: Gen TaskInfo)
  , qcIM "Volume" (arbitrary :: Gen Volume)
  , qcIM "HealthCheck" (arbitrary :: Gen HealthCheck)
  , qcIM "ContainerInfo" (arbitrary :: Gen ContainerInfo)
  , qcIM "Value" (arbitrary :: Gen Value)
  , qcIM "FrameworkInfo" (arbitrary :: Gen FrameworkID)
  , qcIM "OfferID" (arbitrary :: Gen OfferID)
  , qcIM "SlaveID" (arbitrary :: Gen SlaveID)
  , qcIM "TaskID" (arbitrary :: Gen TaskID)
  , qcIM "ExecutorID" (arbitrary :: Gen ExecutorID)
  , qcIM "ContainerID" (arbitrary :: Gen ContainerID)
  , qcIM "Filters" (arbitrary :: Gen Filters)
  , qcIM "Environment" (arbitrary :: Gen Environment)
  , qcIM "FrameworkInfo" (arbitrary :: Gen FrameworkInfo)
  , qcIM "CommandURI" (arbitrary :: Gen CommandURI)
  , qcIM "Credential" (arbitrary :: Gen Credential)
  , qcIM "TaskStatus" (arbitrary :: Gen TaskStatus)
  , qcIM "ResourceUsage" (arbitrary :: Gen ResourceUsage)
  , qcIM "ResourceStatistics" (arbitrary :: Gen ResourceStatistics)
  , qcIM "Parameters" (arbitrary :: Gen Parameters)
  , qcIM "Attribute" (arbitrary :: Gen Attribute)
  , qcIM "Resource" (arbitrary :: Gen Resource)
  , qcIM "CommandInfo" (arbitrary :: Gen CommandInfo)
  , qcIM "MasterInfo" (arbitrary :: Gen MasterInfo)
  , qcIM "Request" (arbitrary :: Gen Request)
  , qcIM "Offer" (arbitrary :: Gen Offer)
  , qcIM "SlaveInfo" (arbitrary :: Gen SlaveInfo)
  , qcIM "DiscoveryInfo" (arbitrary :: Gen DiscoveryInfo)
  , qcIM "Port" (arbitrary :: Gen Port)
  , qcIM "Label" (arbitrary :: Gen Label)
  ]

executorTests = testGroup "Executor"
  [
  ]

schedulerTests = testGroup "Scheduler"
  [
  ]
