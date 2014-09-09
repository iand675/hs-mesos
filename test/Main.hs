{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where
import           Control.Applicative
import           Control.Monad.Managed
import qualified Data.ByteString              as ByteString
import           Data.IORef
import           Foreign.C.Types
import           System.Mesos.Internal
import           System.Mesos.Raw.Attribute
import           System.Mesos.Raw.Environment
import           System.Mesos.Raw.Parameter
import           System.Mesos.Raw.Parameters
import           System.Mesos.Scheduler
import           System.Mesos.Types
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
main = testIDs

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
  arbitrary = CommandURI <$> arbitrary <*> arbitrary <*> arbitrary

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
  arbitrary = Resource <$> arbitrary <*> arbitrary <*> arbitrary

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

instance Arbitrary TaskState where
  arbitrary = elements [Staging, Starting, TaskRunning, Finished, Failed, Killed, Lost]

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

qcIM :: (Show a, Eq a, CPPValue a) => Gen a -> IO ()
qcIM = quickCheck . prop_idempotentMarshalling

testIDs = do
  putStrLn "Testing ExecutorInfo"
  qcIM (arbitrary :: Gen ExecutorInfo)
  putStrLn "Testing TaskInfo"
  qcIM (arbitrary :: Gen TaskInfo)
  putStrLn "Testing Volume"
  qcIM (arbitrary :: Gen Volume)
  putStrLn "Testing HealthCheck"
  qcIM (arbitrary :: Gen HealthCheck)
  putStrLn "Testing ContainerInfo"
  qcIM (arbitrary :: Gen ContainerInfo)
  putStrLn "Testing Value"
  qcIM (arbitrary :: Gen Value)
  putStrLn "Testing FrameworkID"
  qcIM (arbitrary :: Gen FrameworkID)
  putStrLn "Testing OfferID"
  qcIM (arbitrary :: Gen OfferID)
  putStrLn "Testing SlaveID"
  qcIM (arbitrary :: Gen SlaveID)
  putStrLn "Testing TaskID"
  qcIM (arbitrary :: Gen TaskID)
  putStrLn "Testing ExecutorID"
  qcIM (arbitrary :: Gen ExecutorID)
  putStrLn "Testing ContainerID"
  qcIM (arbitrary :: Gen ContainerID)
  putStrLn "Testing Filters"
  qcIM (arbitrary :: Gen Filters)
  putStrLn "Testing Environment"
  qcIM (arbitrary :: Gen Environment)
  putStrLn "Testing FrameworkInfo"
  qcIM (arbitrary :: Gen FrameworkInfo)
  putStrLn "Testing CommandURI"
  qcIM (arbitrary :: Gen CommandURI)
  putStrLn "Testing Credential"
  qcIM (arbitrary :: Gen Credential)
  putStrLn "Testing TaskStatus"
  qcIM (arbitrary :: Gen TaskStatus)
  putStrLn "Testing ResourceUsage"
  qcIM (arbitrary :: Gen ResourceUsage)
  putStrLn "Testing ResourceStatistics"
  qcIM (arbitrary :: Gen ResourceStatistics)
  putStrLn "Testing Parameters"
  qcIM (arbitrary :: Gen Parameters)
  putStrLn "Testing Attribute"
  qcIM (arbitrary :: Gen Attribute)
  putStrLn "Testing Resource"
  qcIM (arbitrary :: Gen Resource)
  putStrLn "Testing CommandInfo"
  qcIM (arbitrary :: Gen CommandInfo)
  putStrLn "Testing MasterInfo"
  qcIM (arbitrary :: Gen MasterInfo)
  putStrLn "Testing Request"
  qcIM (arbitrary :: Gen Request)
  putStrLn "Testing Offer"
  qcIM (arbitrary :: Gen Offer)
  putStrLn "Testing SlaveInfo"
  qcIM (arbitrary :: Gen SlaveInfo)

