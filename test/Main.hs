{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main where
import Control.Applicative
import qualified Data.ByteString as ByteString
import Data.IORef
import Foreign.C.Types
import System.Mesos.Scheduler
import System.Mesos.Types
import System.Mesos.Internal
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Monadic

markCalled r = modifyIORef r (+1)

beforeAndAfter x = do
  print x
  p <- marshal x
  x' <- unmarshal p
  destroy p
  print x'

main :: IO ()
main = do
  -- beforeAndAfter $ SlaveInfo {slaveInfoHostname = "\NUL\STX\ETX\EOT\EOT", slaveInfoPort = Just 2, slaveInfoResources = [Resource {resourceName = "\EOT\SOH\SOH\EOT\SOH", resourceValue = Text "", resourceRole = Just "\STX"},Resource {resourceName = "\NUL\NUL\SOH\EOT", resourceValue = Scalar 0.17900899705823084, resourceRole = Nothing},Resource {resourceName = "\NUL\ETX\ETX", resourceValue = Scalar 4.503865622328922, resourceRole = Just "\STX\ETX"}], slaveInfoAttributes = [("",Set ["\SOH\STX\ETX\NUL","\ETX\STX\ETX"]),("",Scalar (-2.1180957294956677)),("\ETX\ETX\ETX\NUL",Set ["\STX\STX\ETX","\ETX\EOT\ETX","\NUL\SOH\ETX","\STX\STX","\ETX\STX\ETX"]),("\ETX\SOH\STX\EOT\ETX",Text "\EOT\EOT\SOH\EOT")], slaveInfoSlaveID = Just (SlaveID {fromSlaveID = "\SOH\ETX\EOT"}), slaveInfoCheckpoint = Nothing}
  testIDs
  {-
  ref <- newIORef 0
  let called = markCalled ref
  let config = SchedulerConfig
                { schedulerRegistered       = \_ _ _ -> called
                , schedulerReRegistered     = \_ _ -> called
                , schedulerDisconnected     = \_ -> called
                , schedulerResourceOffers   = \_ _ -> called
                , schedulerOfferRescinded   = \_ _ -> called
                , schedulerStatusUpdate     = \_ _ -> called
                , schedulerFrameworkMessage = \_ _ _ _ -> called
                , schedulerSlaveLost        = \_ _ -> called
                , schedulerExecutorLost     = \_ _ _ _ -> called
                , schedulerError            = \_ _ -> called
                }
  scheduler <- createScheduler config
  excerciseMethods scheduler
  destroyScheduler scheduler
  count <- readIORef ref
  putStrLn "should be called 10 times: "
  print count
  -}

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
instance Arbitrary CommandURI where
  arbitrary = CommandURI <$> arbitrary <*> arbitrary
instance Arbitrary CommandInfo where
  arbitrary = CommandInfo <$> arbitrary <*> arbitrary <*> arbitrary
instance Arbitrary ExecutorInfo where
  arbitrary = ExecutorInfo
    <$> arbitrary
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
instance Arbitrary TaskInfo where
  arbitrary = TaskInfo
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
instance Arbitrary TaskState where
  arbitrary = elements [Staging, Starting, TaskRunning, Finished, Failed, Killed, Lost]
instance Arbitrary TaskStatus where
  arbitrary = TaskStatus <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
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
idempotentMarshalling x = do
  p <- marshal x
  u <- unmarshal p
  destroy p
  return u

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
  putStrLn "Testing ExecutorInfo"
  qcIM (arbitrary :: Gen ExecutorInfo)
  putStrLn "Testing MasterInfo"
  qcIM (arbitrary :: Gen MasterInfo)
  putStrLn "Testing Request"
  qcIM (arbitrary :: Gen Request)
  putStrLn "Testing Offer"
  qcIM (arbitrary :: Gen Offer)
  putStrLn "Testing TaskInfo"
  qcIM (arbitrary :: Gen TaskInfo)
  putStrLn "Testing SlaveInfo"
  qcIM (arbitrary :: Gen SlaveInfo)
