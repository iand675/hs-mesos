{-# LANGUAGE OverloadedStrings #-}
module System.Mesos.Internal where
import Control.Applicative
import Data.ByteString (ByteString, packCStringLen)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Word
import Foreign.C
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import System.Mesos.Types

type CBool = CUChar

toCBool :: Bool -> CBool
toCBool b = if b then 1 else 0

fromCBool :: CBool -> Bool
fromCBool b = b /= 0

toStatus :: CInt -> Status
toStatus = toEnum . fromIntegral

type RawRegistered = SchedulerDriverPtr -> FrameworkIDPtr -> MasterInfoPtr -> IO ()
type RawReRegistered = SchedulerDriverPtr -> MasterInfoPtr -> IO ()
type RawDisconnected = SchedulerDriverPtr -> IO ()
type RawResourceOffers = SchedulerDriverPtr -> Ptr OfferPtr -> CInt -> IO ()
type RawOfferRescinded = SchedulerDriverPtr -> OfferIDPtr -> IO ()
type RawStatusUpdate = SchedulerDriverPtr -> TaskStatusPtr -> IO ()
type RawFrameworkMessage = SchedulerDriverPtr -> ExecutorIDPtr -> SlaveIDPtr -> Ptr CChar -> Int -> IO ()
type RawSlaveLost = SchedulerDriverPtr -> SlaveIDPtr -> IO ()
type RawExecutorLost = SchedulerDriverPtr -> ExecutorIDPtr -> SlaveIDPtr -> CInt -> IO ()
type RawError = SchedulerDriverPtr -> Ptr CChar -> CInt -> IO ()

withMarshal :: CPPValue a => [a] -> (Int -> Ptr (Ptr a) -> IO b) -> IO b
withMarshal l f = do
  lp <- mapM marshal l
  val <- withArrayLen lp f
  mapM_ destroy lp
  return val

peekMaybe :: (Storable a) => Ptr (Ptr a) -> IO (Maybe a)
peekMaybe p = do
  pInner <- peek p
  if pInner == nullPtr
    then return Nothing
    else peek pInner >>= return . Just

peekMaybeBS :: Ptr (Ptr CChar) -> Ptr CInt -> IO (Maybe ByteString)
peekMaybeBS sp slp = do
  sl <- peek slp
  spInner <- peek sp
  if spInner == nullPtr
    then return Nothing
    else packCStringLen (spInner, fromIntegral sl) >>= return . Just

peekMaybePrim :: Storable a => Ptr a -> Ptr CBool -> IO (Maybe a)
peekMaybePrim p vsp = do
  set <- peek vsp
  if set /= 0
    then fmap Just $ peek p
    else return Nothing

maybeUnsafeUseAsCStringLen (Just bs) = unsafeUseAsCStringLen bs
maybeUnsafeUseAsCStringLen Nothing = ($ (nullPtr, 0))

defEq :: Eq a => a -> Maybe a -> Maybe a -> Bool
defEq d x x' = x == x' || ((x == Nothing || x == Just d) && (x' == Nothing || x' == Just d))

data Executor = Executor
  { executorImpl                :: Ptr Executor
  , rawExecutorRegistered       :: FunPtr RawExecutorRegistered
  , rawExecutorReRegistered     :: FunPtr RawExecutorReRegistered
  , rawExecutorDisconnected     :: FunPtr RawExecutorDisconnected
  , rawExecutorLaunchTask       :: FunPtr RawExecutorLaunchTask
  , rawExecutorTaskKilled       :: FunPtr RawExecutorTaskKilled
  , rawExecutorFrameworkMessage :: FunPtr RawExecutorFrameworkMessage
  , rawExecutorShutdown         :: FunPtr RawExecutorShutdown
  , rawExecutorErrorCallback    :: FunPtr RawExecutorError
  }

type ExecutorDriverPtr = Ptr ExecutorDriver
newtype ExecutorDriver = ExecutorDriver { fromExecutorDriver :: ExecutorDriverPtr }

data Scheduler = Scheduler
  { schedulerImpl :: SchedulerPtr
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
-- (e.g., launch tasks, kill tasks, etc.). See @MesosSchedulerDriver@
-- below for a concrete example of a @SchedulerDriver@.
newtype SchedulerDriver = SchedulerDriver { fromSchedulerDriver :: SchedulerDriverPtr }
  deriving (Show, Eq)

toEnvVar :: (ByteString, ByteString) -> EnvironmentVariable
toEnvVar (k, v) = EnvironmentVariable k v

fromEnvVar :: EnvironmentVariable -> (ByteString, ByteString)
fromEnvVar (EnvironmentVariable k v) = (k, v)

toAttribute (k, v) = Attribute k v
fromAttribute (Attribute k v) = (k, v)

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

type RawExecutorRegistered = ExecutorDriverPtr -> ExecutorInfoPtr -> FrameworkInfoPtr -> SlaveInfoPtr -> IO ()
type RawExecutorReRegistered = ExecutorDriverPtr -> SlaveInfoPtr -> IO ()
type RawExecutorDisconnected = ExecutorDriverPtr -> IO ()
type RawExecutorLaunchTask = ExecutorDriverPtr -> TaskInfoPtr -> IO ()
type RawExecutorTaskKilled = ExecutorDriverPtr -> TaskIDPtr -> IO ()
type RawExecutorFrameworkMessage = ExecutorDriverPtr -> Ptr CChar -> CInt -> IO ()
type RawExecutorShutdown = ExecutorDriverPtr -> IO ()
type RawExecutorError = ExecutorDriverPtr -> Ptr CChar -> CInt -> IO ()

type SchedulerPtr = Ptr Scheduler
type FrameworkIDPtr = Ptr FrameworkID
type MasterInfoPtr = Ptr MasterInfo
type SchedulerDriverPtr = Ptr SchedulerDriver
type SlaveIDPtr = Ptr SlaveID
type ExecutorIDPtr = Ptr ExecutorID
type OfferPtr = Ptr Offer
type OfferIDPtr = Ptr OfferID
type TaskStatusPtr = Ptr TaskStatus
type FrameworkInfoPtr = Ptr FrameworkInfo
type CredentialPtr = Ptr Credential
type RequestPtr = Ptr Request
type TaskInfoPtr = Ptr TaskInfo
type TaskIDPtr = Ptr TaskID
type ContainerIDPtr = Ptr ContainerID
type EnvironmentPtr = Ptr Environment
type EnvironmentVariablePtr = Ptr EnvironmentVariable
type ExecutorInfoPtr = Ptr ExecutorInfo
type SlaveInfoPtr = Ptr SlaveInfo
type ExecutorPtr = Ptr Executor
type ResourcePtr = Ptr Resource
type AttributePtr = Ptr Attribute
type ValuePtr = Ptr Value
type CommandInfoPtr = Ptr CommandInfo
type ResourceUsagePtr = Ptr ResourceUsage
type ResourceStatisticsPtr = Ptr ResourceStatistics
type ParameterPtr = Ptr Parameter
type ParametersPtr = Ptr Parameters

class CPPValue a where
  marshal :: a -> IO (Ptr a)
  unmarshal :: Ptr a -> IO a
  destroy :: Ptr a -> IO ()
  equalExceptDefaults :: Eq a => a -> a -> Bool
  equalExceptDefaults = (==)

type ToID a = Ptr CChar -> CInt -> IO a
type FromID a = a -> Ptr (Ptr CChar) -> IO CInt

-- *****************************************************************************************************************
-- 
-- *****************************************************************************************************************
foreign import ccall "ext/types.h toFrameworkID" c_toFrameworkID :: ToID FrameworkIDPtr
foreign import ccall "ext/types.h fromFrameworkID" c_fromFrameworkID :: FromID FrameworkIDPtr
foreign import ccall "ext/types.h destroyFrameworkID" c_destroyFrameworkID :: FrameworkIDPtr -> IO ()
instance CPPValue FrameworkID where
  marshal x = unsafeUseAsCStringLen (fromFrameworkID x) $ \(strp, l) -> c_toFrameworkID strp (fromIntegral l)
  unmarshal p = fmap FrameworkID $ do
    alloca $ \ptrPtr -> do
      len <- c_fromFrameworkID p ptrPtr
      ptrVal <- peek ptrPtr
      packCStringLen (ptrVal, fromIntegral len)
  destroy = c_destroyFrameworkID

-- *****************************************************************************************************************
-- 
-- *****************************************************************************************************************
foreign import ccall "ext/types.h toOfferID" c_toOfferID :: ToID OfferIDPtr
foreign import ccall "ext/types.h fromOfferID" c_fromOfferID :: FromID OfferIDPtr
foreign import ccall "ext/types.h destroyOfferID" c_destroyOfferID :: OfferIDPtr -> IO ()
instance CPPValue OfferID where
  marshal x = unsafeUseAsCStringLen (fromOfferID x) $ \(strp, l) -> c_toOfferID strp (fromIntegral l)
  unmarshal p = fmap OfferID $ do
    alloca $ \ptrPtr -> do
      len <- c_fromOfferID p ptrPtr
      ptrVal <- peek ptrPtr
      packCStringLen (ptrVal, fromIntegral len)
  destroy = c_destroyOfferID

-- *****************************************************************************************************************
-- 
-- *****************************************************************************************************************
foreign import ccall "ext/types.h toSlaveID" c_toSlaveID :: ToID SlaveIDPtr
foreign import ccall "ext/types.h fromSlaveID" c_fromSlaveID :: FromID SlaveIDPtr
foreign import ccall "ext/types.h destroySlaveID" c_destroySlaveID :: SlaveIDPtr -> IO ()
instance CPPValue SlaveID where
  marshal x = unsafeUseAsCStringLen (fromSlaveID x) $ \(strp, l) -> c_toSlaveID strp (fromIntegral l)
  unmarshal p = fmap SlaveID $ do
    alloca $ \ptrPtr -> do
      len <- c_fromSlaveID p ptrPtr
      ptrVal <- peek ptrPtr
      packCStringLen (ptrVal, fromIntegral len)
  destroy = c_destroySlaveID

-- *****************************************************************************************************************
-- 
-- *****************************************************************************************************************
foreign import ccall "ext/types.h toTaskID" c_toTaskID :: ToID TaskIDPtr
foreign import ccall "ext/types.h fromTaskID" c_fromTaskID :: FromID TaskIDPtr
foreign import ccall "ext/types.h destroyTaskID" c_destroyTaskID :: TaskIDPtr -> IO ()
instance CPPValue TaskID where
  marshal x = unsafeUseAsCStringLen (fromTaskID x) $ \(strp, l) -> c_toTaskID strp (fromIntegral l)
  unmarshal p = fmap TaskID $ do
    alloca $ \ptrPtr -> do
      len <- c_fromTaskID p ptrPtr
      ptrVal <- peek ptrPtr
      packCStringLen (ptrVal, fromIntegral len)
  destroy = c_destroyTaskID

-- *****************************************************************************************************************
-- 
-- *****************************************************************************************************************
foreign import ccall "ext/types.h toExecutorID" c_toExecutorID :: ToID ExecutorIDPtr
foreign import ccall "ext/types.h fromExecutorID" c_fromExecutorID :: FromID ExecutorIDPtr
foreign import ccall "ext/types.h destroyExecutorID" c_destroyExecutorID :: ExecutorIDPtr -> IO ()
instance CPPValue ExecutorID where
  marshal x = unsafeUseAsCStringLen (fromExecutorID x) $ \(strp, l) -> c_toExecutorID strp (fromIntegral l)
  unmarshal p = fmap ExecutorID $ do
    alloca $ \ptrPtr -> do
      len <- c_fromExecutorID p ptrPtr
      ptrVal <- peek ptrPtr
      packCStringLen (ptrVal, fromIntegral len)
  destroy = c_destroyExecutorID

-- *****************************************************************************************************************
-- 
-- *****************************************************************************************************************
foreign import ccall "ext/types.h toContainerID" c_toContainerID :: ToID ContainerIDPtr
foreign import ccall "ext/types.h fromContainerID" c_fromContainerID :: FromID ContainerIDPtr
foreign import ccall "ext/types.h destroyContainerID" c_destroyContainerID :: ContainerIDPtr -> IO ()
instance CPPValue ContainerID where
  marshal x = unsafeUseAsCStringLen (fromContainerID x) $ \(strp, l) -> c_toContainerID strp (fromIntegral l)
  unmarshal p = fmap ContainerID $ do
    alloca $ \ptrPtr -> do
      len <- c_fromContainerID p ptrPtr
      ptrVal <- peek ptrPtr
      packCStringLen (ptrVal, fromIntegral len)
  destroy = c_destroyContainerID

-- *****************************************************************************************************************
-- 
-- *****************************************************************************************************************

newtype StdString = StdString ByteString
type StdStringPtr = Ptr StdString
foreign import ccall "ext/types.h toStdString" c_toStdString
  :: Ptr CChar
  -> CInt
  -> IO StdStringPtr
foreign import ccall "ext/types.h fromStdString" c_fromStdString
  :: StdStringPtr
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> IO ()
foreign import ccall "ext/types.h destroyStdString" c_destroyStdString
  :: StdStringPtr
  -> IO ()
instance CPPValue StdString where
  marshal (StdString bs) = unsafeUseAsCStringLen bs $ \(sp, sl) -> c_toStdString sp (fromIntegral sl)
  unmarshal p = alloca $ \spp -> alloca $ \slp -> do
    c_fromStdString p spp slp
    sp <- peek spp
    sl <- peek slp
    StdString <$> packCStringLen (sp, fromIntegral sl)
  destroy = c_destroyStdString

foreign import ccall "ext/types.h toFrameworkInfo" c_toFrameworkInfo
  :: Ptr CChar
  -> CInt
  -> Ptr CChar
  -> CInt
  -> Ptr FrameworkIDPtr
  -> Ptr CDouble
  -> Ptr CBool
  -> Ptr CChar
  -> CInt
  -> Ptr CChar
  -> CInt
  -> IO FrameworkInfoPtr
foreign import ccall "ext/types.h fromFrameworkInfo" c_fromFrameworkInfo
  :: FrameworkInfoPtr
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> Ptr FrameworkIDPtr
  -> Ptr CBool
  -> Ptr CDouble
  -> Ptr CBool
  -> Ptr CBool
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> IO ()
foreign import ccall "ext/types.h destroyFrameworkInfo" c_destroyFrameworkInfo
  :: FrameworkInfoPtr
  -> IO ()
instance CPPValue FrameworkInfo where
  marshal fi = unsafeUseAsCStringLen (frameworkUser fi) $ \(up, ul) ->
    unsafeUseAsCStringLen (frameworkName fi) $ \(np, nl) ->
    let roleFun f = case frameworkRole fi of
          Nothing -> f (nullPtr, 0)
          Just r -> unsafeUseAsCStringLen r f
    in
    roleFun $ \(rp, rl) ->
    let hostnameFun f = case frameworkHostname fi of
          Nothing -> f (nullPtr, 0)
          Just r -> unsafeUseAsCStringLen r f
    in
    hostnameFun $ \(hp, hl) ->
    alloca $ \fp ->
    alloca $ \cp -> do
      fp' <- maybe (return nullPtr) (\x -> poke fp (CDouble x) >> return fp) $ frameworkFailoverTimeout fi
      cp' <- maybe (return nullPtr) (\x -> poke cp (if x then 1 else 0) >> return cp) $ frameworkCheckpoint fi
      let fidFun f = case frameworkID fi of
            Nothing -> f nullPtr
            Just r -> alloca $ \p -> do
              fidp <- marshal r
              poke p fidp
              final <- f p
              destroy fidp
              return final
      fidFun $ \fidp -> c_toFrameworkInfo up
        (fromIntegral ul)
        np
        (fromIntegral nl)
        fidp
        fp'
        cp'
        rp
        (fromIntegral rl)
        hp
        (fromIntegral hl)
  unmarshal fp = alloca $ \up ->
    alloca $ \ul ->
    alloca $ \np ->
    alloca $ \nl ->
    alloca $ \idp ->
    alloca $ \tps ->
    alloca $ \tp ->
    alloca $ \cps ->
    alloca $ \cp ->
    alloca $ \rp ->
    alloca $ \rl ->
    alloca $ \hp ->
    alloca $ \hl -> do
      poke up nullPtr
      poke ul 0
      poke np nullPtr
      poke nl 0
      poke idp nullPtr
      poke rp nullPtr
      poke rl 0
      poke hp nullPtr
      poke hl 0
      c_fromFrameworkInfo fp up ul np nl idp tps tp cps cp rp rl hp hl
      ulv <- peek ul
      ubs <- peek up >>= \upi -> packCStringLen (upi, fromIntegral ulv)
      nlv <- peek nl
      nbs <- peek np >>= \npi -> packCStringLen (npi, fromIntegral nlv)
      mid <- do
        midp <- peek idp
        if midp == nullPtr
          then return Nothing
          else fmap Just $ unmarshal midp
      mt <- fmap (fmap (\(CDouble d) -> d)) $ peekMaybePrim tp tps
      mc <- fmap (fmap (== 1)) $ peekMaybePrim cp cps
      mr <- peekMaybeBS rp rl
      mh <- peekMaybeBS hp hl
      return $ FrameworkInfo ubs nbs mid mt mc mr mh
  destroy = c_destroyFrameworkInfo
  equalExceptDefaults (FrameworkInfo u n i ft cp r h) (FrameworkInfo u' n' i' ft' cp' r' h') =
    u == u' && n == n' && i == i' && defEq 0 ft ft' && defEq False cp cp' && defEq "*" r r' && h == h'

-- *****************************************************************************************************************
-- 
-- *****************************************************************************************************************
type CommandURIPtr = Ptr CommandURI

foreign import ccall "ext/types.h toCommandURI" c_toCommandURI
  :: Ptr CChar
  -> CInt
  -> Ptr CBool
  -> IO CommandURIPtr
foreign import ccall "ext/types.h fromCommandURI" c_fromCommandURI
  :: CommandURIPtr
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> Ptr CBool
  -> Ptr CBool
  -> IO ()
foreign import ccall "ext/types.h destroyCommandURI" c_destroyCommandURI
  :: CommandURIPtr
  -> IO ()
instance CPPValue CommandURI where
  marshal cu = unsafeUseAsCStringLen (commandURIValue cu) $ \(vp, vl) ->
    let call = c_toCommandURI vp (fromIntegral vl) in
    alloca $ \xp -> case commandURIExecutable cu of
      Nothing -> call nullPtr
      Just x -> poke xp (toCBool x) >> call xp
  unmarshal cup = alloca $ \uriPP ->
    alloca $ \uriLenP ->
    alloca $ \exeSetP ->
    alloca $ \exeP -> do
      c_fromCommandURI cup uriPP uriLenP exeSetP exeP
      uriLen <- peek uriLenP
      uriP <- peek uriPP
      uri <- packCStringLen (uriP, fromIntegral uriLen)
      mset <- peekMaybePrim exeP exeSetP
      return $ CommandURI uri (fmap fromCBool mset)
  destroy = c_destroyCommandURI

-- *****************************************************************************************************************
-- 
-- *****************************************************************************************************************
type FiltersPtr = Ptr Filters

foreign import ccall "ext/types.h toFilters" c_toFilters
  :: Ptr CDouble
  -> IO FiltersPtr
foreign import ccall "ext/types.h fromFilters" c_fromFilters
  :: FiltersPtr
  -> Ptr CBool
  -> Ptr CDouble
  -> IO ()
foreign import ccall "ext/types.h destroyFilters" c_destroyFilters
  :: FiltersPtr
  -> IO ()
instance CPPValue Filters where
  marshal f = case refuseSeconds f of
    Nothing -> c_toFilters nullPtr
    Just s -> alloca $ \sp -> do
      poke sp (CDouble s) >> c_toFilters sp
  unmarshal fp = alloca $ \rsc -> alloca $ \rsp -> do
    c_fromFilters fp rsc rsp
    ms <- peekMaybePrim rsp rsc
    return $ Filters $ fmap (\(CDouble d) -> d) ms
  destroy = c_destroyFilters
  equalExceptDefaults (Filters f) (Filters f') = defEq 5.0 f f'

-- *****************************************************************************************************************
-- 
-- *****************************************************************************************************************
foreign import ccall "ext/types.h toCredential" c_toCredential
  :: Ptr CChar
  -> CInt
  -> Ptr CChar
  -> CInt
  -> IO CredentialPtr
foreign import ccall "ext/types.h fromCredential" c_fromCredential
  :: CredentialPtr
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> IO ()
foreign import ccall "ext/types.h destroyCredential" c_destroyCredential
  :: CredentialPtr
  -> IO ()
instance CPPValue Credential where
  marshal c = do
    unsafeUseAsCStringLen (credentialPrincipal c) $ \(pp, pl) ->
      let call = c_toCredential pp (fromIntegral pl) in case credentialSecret c of
        Nothing -> call nullPtr 0
        Just s -> unsafeUseAsCStringLen s $ \(sp, sl) -> call sp (fromIntegral sl)
  unmarshal cp = alloca $ \pp ->
    alloca $ \plp ->
    alloca $ \sp ->
    alloca $ \slp -> do
      c_fromCredential cp pp plp sp slp
      p <- peek pp
      pl <- peek plp
      ps <- packCStringLen (p, fromIntegral pl)
      s <- peek sp
      ss <- if s == nullPtr
        then return Nothing
        else do
          sl <- peek slp
          fmap Just $ packCStringLen (s, fromIntegral sl)
      return $ Credential ps ss
  destroy = c_destroyCredential

-- *****************************************************************************************************************
-- 
-- *****************************************************************************************************************
foreign import ccall "ext/types.h toMasterInfo" c_toMasterInfo
  :: Ptr CChar
  -> CInt
  -> CUInt
  -> CUInt
  -> Ptr CChar
  -> CInt
  -> Ptr CChar
  -> CInt
  -> IO MasterInfoPtr

foreign import ccall "ext/types.h fromMasterInfo" c_fromMasterInfo
  :: MasterInfoPtr
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> Ptr CUInt
  -> Ptr CUInt
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> IO ()

foreign import ccall "ext/types.h destroyMasterInfo" c_destroyMasterInfo
  :: MasterInfoPtr
  -> IO ()

instance CPPValue MasterInfo where
  marshal i = unsafeUseAsCStringLen (masterInfoID i) $ \(idp, idl) ->
    maybeUnsafeUseAsCStringLen (masterInfoPID i) $ \(pidp, pidl) ->
    maybeUnsafeUseAsCStringLen (masterInfoHostname i) $ \(hnp, hnl) ->
      c_toMasterInfo idp (fromIntegral idl) (CUInt $ masterInfoIP i) (CUInt $ masterInfoPort i) pidp (fromIntegral pidl) hnp (fromIntegral hnl)
  unmarshal i = alloca $ \idpP ->
    alloca $ \idlP ->
    alloca $ \ipP ->
    alloca $ \portP ->
    alloca $ \pidpP ->
    alloca $ \pidlP ->
    alloca $ \hnpP ->
    alloca $ \hnlP -> do
      poke pidpP nullPtr
      poke hnpP nullPtr
      c_fromMasterInfo i idpP idlP ipP portP pidpP pidlP hnpP hnlP
      idp <- peek idpP
      idl <- peek idlP
      mID <- packCStringLen (idp, fromIntegral idl)
      (CUInt ip) <- peek ipP
      (CUInt port) <- peek portP
      pid <- peekMaybeBS pidpP pidlP
      hn <- peekMaybeBS hnpP hnlP
      return $ MasterInfo mID ip port pid hn
  destroy = c_destroyMasterInfo
-- *****************************************************************************************************************
-- 
-- *****************************************************************************************************************
foreign import ccall "ext/types.h toSlaveInfo" c_toSlaveInfo
  :: Ptr CChar
  -> CInt
  -> Ptr CUInt
  -> Ptr ResourcePtr
  -> CInt
  -> Ptr AttributePtr
  -> CInt
  -> SlaveIDPtr
  -> Ptr CBool
  -> IO SlaveInfoPtr
foreign import ccall "ext/types.h fromSlaveInfo" c_fromSlaveInfo
  :: SlaveInfoPtr
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> Ptr CBool
  -> Ptr CUInt
  -> Ptr (Ptr ResourcePtr)
  -> Ptr CInt
  -> Ptr (Ptr AttributePtr)
  -> Ptr CInt
  -> Ptr SlaveIDPtr
  -> Ptr CBool
  -> Ptr CBool
  -> IO ()
foreign import ccall "ext/types.h destroySlaveInfo" c_destroySlaveInfo
  :: SlaveInfoPtr
  -> IO ()

instance CPPValue SlaveInfo where
  marshal i = unsafeUseAsCStringLen (slaveInfoHostname i) $ \(hp, hl) ->
    alloca $ \pp -> alloca $ \cp -> do
      rs <- mapM marshal $ slaveInfoResources i
      as <- mapM (marshal . toAttribute) $ slaveInfoAttributes i
      withArrayLen rs $ \rl rp -> withArrayLen as $ \al ap -> do
        maybe (return ()) (poke pp . CUInt) $ slaveInfoPort i
        maybe (return ()) (poke cp . toCBool) $ slaveInfoCheckpoint i
        sidp <- maybe (return nullPtr) marshal $ slaveInfoSlaveID i
        p <- c_toSlaveInfo hp (fromIntegral hl) (maybe nullPtr (const pp) $ slaveInfoPort i) rp (fromIntegral rl) ap (fromIntegral al) sidp (maybe nullPtr (const cp) $ slaveInfoCheckpoint i)
        destroy sidp
        mapM_ destroy rs
        mapM_ destroy as
        return p
  unmarshal i = alloca $ \hpp ->
    alloca $ \hlp ->
    alloca $ \pps ->
    alloca $ \pp ->
    alloca $ \rpp ->
    alloca $ \rlp ->
    alloca $ \app ->
    alloca $ \alp ->
    alloca $ \ipp ->
    alloca $ \cps ->
    alloca $ \cp -> do
      poke ipp nullPtr
      c_fromSlaveInfo i hpp hlp pps pp rpp rlp app alp ipp cps cp
      hp <- peek hpp
      hl <- peek hlp
      h <- packCStringLen (hp, fromIntegral hl)
      p <- peekMaybePrim pp pps
      rp <- peek rpp
      rl <- peek rlp
      rs <- mapM unmarshal =<< peekArray (fromIntegral rl) rp
      ap <- peek app
      al <- peek alp
      as <- mapM unmarshal =<< peekArray (fromIntegral al) ap
      ip <- peek ipp
      sid <- if ip == nullPtr
        then return Nothing
        else fmap Just $ unmarshal ip
      c <- peekMaybePrim cp cps
      return $ SlaveInfo h (fromIntegral <$> p) rs (map fromAttribute as) sid $ fmap fromCBool c
  destroy = c_destroySlaveInfo
  equalExceptDefaults (SlaveInfo hn p rs as sid cp) (SlaveInfo hn' p' rs' as' sid' cp') =
    (hn == hn') &&
    (p == p' || p == Just 5051) &&
    (and $ zipWith equalExceptDefaults rs rs') &&
    (and $ zipWith (\(k, v) (k', v') -> k == k' && equalExceptDefaults v v') as as') &&
    (sid == sid') &&
    defEq False cp cp'
-- *****************************************************************************************************************
-- 
-- *****************************************************************************************************************
data ValueType
  = SCALAR
  | RANGES
  | SET
  | TEXT

instance Enum ValueType where
  fromEnum SCALAR = 0
  fromEnum RANGES = 1
  fromEnum SET = 2
  fromEnum TEXT = 3
  toEnum 0 = SCALAR
  toEnum 1 = RANGES
  toEnum 2 = SET
  toEnum 3 = TEXT

data ValueRange = ValueRange Word64 Word64
type ValueRangePtr = Ptr ValueRange

foreign import ccall "ext/types.h toRange" c_toRange
  :: CULong
  -> CULong
  -> IO ValueRangePtr

foreign import ccall "ext/types.h fromRange" c_fromRange
  :: ValueRangePtr
  -> Ptr CULong
  -> Ptr CULong
  -> IO ()
foreign import ccall "ext/types.h destroyRange" c_destroyRange
  :: ValueRangePtr
  -> IO ()
instance CPPValue ValueRange where
  marshal (ValueRange l h) = c_toRange (CULong l) (CULong h)
  unmarshal p = alloca $ \l -> alloca $ \h -> do
    c_fromRange p l h
    (CULong l') <- peek l
    (CULong r') <- peek h
    return $ ValueRange l' r'
  destroy = c_destroyRange

foreign import ccall "ext/types.h toValue" c_toValue
  :: CInt
  -> CDouble
  -> Ptr ValueRangePtr
  -> CInt
  -> Ptr StdStringPtr
  -> CInt
  -> Ptr CChar
  -> CInt
  -> IO ValuePtr
foreign import ccall "ext/types.h fromValue" c_fromValue
  :: ValuePtr
  -> Ptr CInt
  -> Ptr CDouble
  -> Ptr (Ptr ValueRangePtr)
  -> Ptr CInt
  -> Ptr (Ptr StdStringPtr)
  -> Ptr CInt
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> IO ()
foreign import ccall "ext/types.h destroyValue" c_destroyValue
  :: ValuePtr
  -> IO ()
instance CPPValue Value where
  marshal (Scalar x) = c_toValue (fromIntegral $ fromEnum SCALAR) (CDouble x) nullPtr 0 nullPtr 0 nullPtr 0
  marshal (Ranges rs) = do
    ranges <- mapM (marshal . (uncurry ValueRange)) rs
    result <- withArrayLen ranges $ \rLen rp -> do
      c_toValue (fromIntegral $ fromEnum RANGES) 0 rp (fromIntegral rLen) nullPtr 0 nullPtr 0
    mapM_ destroy ranges
    return result
  marshal (Set ts) = do
    sps <- mapM (marshal . StdString) ts
    result <- withArrayLen sps $ \sl sp -> 
      c_toValue (fromIntegral $ fromEnum SET) 0 nullPtr 0 sp (fromIntegral sl) nullPtr 0
    mapM destroy sps
    return result
  marshal (Text t) = unsafeUseAsCStringLen t $ \(tp, tl) ->
    c_toValue (fromIntegral $ fromEnum TEXT) 0 nullPtr 0 nullPtr 0 tp (fromIntegral tl)
  unmarshal vp = alloca $ \typeP ->
    alloca $ \scalarP ->
    alloca $ \rangePP ->
    alloca $ \rangeLenP ->
    alloca $ \setStrPP ->
    alloca $ \setSizeP ->
    alloca $ \textP ->
    alloca $ \textLenP -> do
      c_fromValue vp typeP scalarP rangePP rangeLenP setStrPP setSizeP textP textLenP
      t <- fmap (toEnum . fromIntegral) $ peek typeP
      case t of
        SCALAR -> peek scalarP >>= \(CDouble d) -> return $ Scalar d
        RANGES -> do
          rangeLen <- fmap fromIntegral $ peek rangeLenP
          rangeP <- peek rangePP
          rangePs <- peekArray (fromIntegral rangeLen) rangeP
          rs <- mapM unmarshal rangePs
          return $ Ranges $ map (\(ValueRange l h) -> (l, h)) rs
        SET -> do
          setSize <- fmap fromIntegral $ peek setSizeP
          setStrs <- mapM unmarshal =<< peekArray setSize =<< peek setStrPP
          return $! Set $ map (\(StdString x) -> x) setStrs
        TEXT -> do
          text <- peek textP
          textLen <- peek textLenP
          fmap Text $ packCStringLen (text, fromIntegral textLen)

  destroy = c_destroyValue
-- *****************************************************************************************************************
-- 
-- *****************************************************************************************************************
foreign import ccall "ext/types.h toResource" c_toResource
  :: Ptr CChar
  -> CInt
  -> ValuePtr
  -> Ptr CChar
  -> CInt
  -> IO ResourcePtr
foreign import ccall "ext/types.h fromResource" c_fromResource
  :: ResourcePtr
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> Ptr ValuePtr
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> IO ()
foreign import ccall "ext/types.h destroyResource" c_destroyResource
  :: ResourcePtr
  -> IO ()
instance CPPValue Resource where
  marshal r = unsafeUseAsCStringLen (resourceName r) $ \(np, nl) ->
    maybeUnsafeUseAsCStringLen (resourceRole r) $ \(rp, rl) -> do
      vp <- marshal $ resourceValue r
      p <- c_toResource np (fromIntegral nl) vp rp (fromIntegral rl)
      destroy vp
      return p
  unmarshal r = 
    alloca $ \npp -> 
    alloca $ \nlp -> 
    alloca $ \vpp -> 
    alloca $ \rpp -> 
    alloca $ \rlp -> do
      c_fromResource r npp nlp vpp rpp rlp
      np <- peek npp
      nl <- peek nlp
      n <- packCStringLen (np, fromIntegral nl)
      vp <- peek vpp
      v <- unmarshal vp
      rp <- peek rpp
      r <- if r == nullPtr
        then return Nothing
        else fmap Just $ do
          rl <- peek rlp
          packCStringLen (rp, fromIntegral rl)
      return $ Resource n v r
  destroy = c_destroyResource
  equalExceptDefaults (Resource n v r) (Resource n' v' r') = (n == n') &&
    textToSet v v' &&
    defEq "*" r r'
    where
      textToSet (Text t) (Set s) = [t] == s
      textToSet (Set s) (Text t) = [t] == s
      textToSet v v' = equalExceptDefaults v v'

-- *****************************************************************************************************************
-- 
-- *****************************************************************************************************************
foreign import ccall "ext/types.h toTaskStatus" c_toTaskStatus
  :: TaskIDPtr
  -> CInt
  -> Ptr CChar
  -> CInt
  -> Ptr CChar
  -> CInt
  -> SlaveIDPtr
  -> Ptr CDouble
  -> IO TaskStatusPtr
foreign import ccall "ext/types.h fromTaskStatus" c_fromTaskStatus
  :: TaskStatusPtr
  -> Ptr TaskIDPtr
  -> Ptr CInt
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> Ptr SlaveIDPtr
  -> Ptr CBool
  -> Ptr CDouble
  -> IO ()
foreign import ccall "ext/types.h destroyTaskStatus" c_destroyTaskStatus
  :: TaskStatusPtr
  -> IO ()
instance CPPValue TaskStatus where
  marshal s = do
    tidP <- marshal $ taskStatusTaskID s
    sidP <- maybe (return nullPtr) marshal $ taskStatusSlaveID s
    ts <- maybeUnsafeUseAsCStringLen (taskStatusMessage s) $ \(tmp, tml) ->
      maybeUnsafeUseAsCStringLen (taskStatusData s) $ \(tsd, tsl) -> do
        alloca $ \tsp -> do
          tsp' <- maybe (return nullPtr) (\x -> poke tsp (CDouble x) >> return tsp) $ taskStatusTimestamp s
          c_toTaskStatus tidP (fromIntegral $ fromEnum $ taskStatusState s) tmp (fromIntegral tml) tsd (fromIntegral tsl) sidP tsp'
    destroy tidP
    destroy sidP
    return ts
  unmarshal s = alloca $ \tidp ->
    alloca $ \sp ->
    alloca $ \mpp ->
    alloca $ \mlp ->
    alloca $ \dpp ->
    alloca $ \dlp ->
    alloca $ \sidp ->
    alloca $ \tssp ->
    alloca $ \tsp -> do
      poke mpp nullPtr
      poke dpp nullPtr
      poke sidp nullPtr
      c_fromTaskStatus s tidp sp mpp mlp dpp dlp sidp tssp tsp
      tid <- unmarshal =<< peek tidp
      state <- (toEnum . fromIntegral) <$> peek sp
      msg <- peekMaybeBS mpp mlp
      dat <- peekMaybeBS dpp dlp
      mSidP <- peek sidp
      sid <- if mSidP == nullPtr
        then return Nothing
        else Just <$> unmarshal mSidP
      ts <- fmap (\(CDouble d) -> d) <$> peekMaybePrim tsp tssp
      return $ TaskStatus tid state msg dat sid ts
  destroy = c_destroyTaskStatus

-- *****************************************************************************************************************
-- 
-- *****************************************************************************************************************
newtype Environment = Environment
  { environmentVariables :: [(ByteString, ByteString)]
  }
  deriving (Show, Eq)

fromEnvironment :: Environment -> [(ByteString, ByteString)]
fromEnvironment = environmentVariables

toEnvironment :: [(ByteString, ByteString)] -> Environment
toEnvironment = Environment

foreign import ccall "ext/types.h toEnvironment" c_toEnvironment
  :: Ptr EnvironmentVariablePtr
  -> CInt
  -> IO EnvironmentPtr
foreign import ccall "ext/types.h fromEnvironment" c_fromEnvironment
  :: EnvironmentPtr
  -> Ptr (Ptr EnvironmentVariablePtr)
  -> Ptr CInt
  -> IO ()
foreign import ccall "ext/types.h destroyEnvironment" c_destroyEnvironment
  :: EnvironmentPtr
  -> IO ()
instance CPPValue Environment where
  marshal e = do
    es <- mapM (marshal . toEnvVar) $ environmentVariables e
    p <- withArrayLen es $ \eLen esp -> c_toEnvironment esp (fromIntegral eLen)
    mapM_ destroy es
    return p
  unmarshal ep = alloca $ \evpp -> alloca $ \evlp -> do
    c_fromEnvironment ep evpp evlp
    evp <- peek evpp
    evl <- peek evlp
    evs <- peekArray (fromIntegral evl) evp
    l <- mapM unmarshal evs
    return $ Environment $ map fromEnvVar l
  destroy = c_destroyEnvironment
-- *****************************************************************************************************************
-- 
-- *****************************************************************************************************************

foreign import ccall "ext/types.h toCommandInfo" c_toCommandInfo
  :: Ptr CommandURIPtr
  -> CInt
  -> EnvironmentPtr
  -> Ptr CChar
  -> CInt
  -> IO CommandInfoPtr
foreign import ccall "ext/types.h fromCommandInfo" c_fromCommandInfo
  :: CommandInfoPtr
  -> Ptr (Ptr CommandURIPtr)
  -> Ptr CInt
  -> Ptr EnvironmentPtr
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> IO CommandInfoPtr
foreign import ccall "ext/types.h destroyCommandInfo" c_destroyCommandInfo
  :: CommandInfoPtr
  -> IO ()
instance CPPValue CommandInfo where
  marshal i = do
    envP <- maybe (return nullPtr) (marshal . toEnvironment) $ commandEnvironment i
    uriPs <- mapM marshal $ commandInfoURIs i
    r <- withArrayLen uriPs $ \upl upp ->
      unsafeUseAsCStringLen (commandValue i) $ \(vp, vl) ->
        c_toCommandInfo upp (fromIntegral upl) envP vp (fromIntegral vl)
    destroy envP
    mapM_ destroy uriPs
    return r
  unmarshal i = alloca $ \upp ->
    alloca $ \ulp ->
    alloca $ \epp ->
    alloca $ \vpp ->
    alloca $ \vlp -> do
      poke epp nullPtr
      c_fromCommandInfo i upp ulp epp vpp vlp
      up <- peek upp
      ul <- peek ulp
      ups <- peekArray (fromIntegral ul) up
      us <- mapM unmarshal ups
      ep <- peek epp
      e <- if ep == nullPtr
        then return Nothing
        else fmap Just $ unmarshal ep
      vp <- peek vpp
      vl <- peek vlp
      v <- packCStringLen (vp, fromIntegral vl)
      return $ CommandInfo us (fmap fromEnvironment e) v
  destroy = c_destroyCommandInfo
-- *****************************************************************************************************************
-- 
-- *****************************************************************************************************************
foreign import ccall "ext/types.h toExecutorInfo" c_toExecutorInfo
  :: ExecutorIDPtr
  -> FrameworkIDPtr
  -> CommandInfoPtr
  -> Ptr ResourcePtr
  -> CInt
  -> Ptr CChar
  -> CInt
  -> Ptr CChar
  -> CInt
  -> Ptr CChar
  -> CInt
  -> IO ExecutorInfoPtr
foreign import ccall "ext/types.h fromExecutorInfo" c_fromExecutorInfo
  :: ExecutorInfoPtr
  -> Ptr ExecutorIDPtr
  -> Ptr FrameworkIDPtr
  -> Ptr CommandInfoPtr
  -> Ptr (Ptr ResourcePtr)
  -> Ptr CInt
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> IO ()
foreign import ccall "ext/types.h destroyExecutorInfo" c_destroyExecutorInfo
  :: ExecutorInfoPtr
  -> IO ()

instance CPPValue ExecutorInfo where
  marshal i = do
    eidP <- marshal $ executorInfoExecutorID i
    fidP <- marshal $ executorInfoFrameworkID i
    ciP <- marshal $ executorInfoCommandInfo i
    rps <- mapM marshal $ executorInfoResources i
    ip <- maybeUnsafeUseAsCStringLen (executorName i) $ \(np, nl) ->
      maybeUnsafeUseAsCStringLen (executorSource i) $ \(sp, sl) ->
      maybeUnsafeUseAsCStringLen (executorData i) $ \(dp, dl) ->
      withArrayLen rps $ \rLen rs -> do
        c_toExecutorInfo eidP fidP ciP rs (fromIntegral rLen) np (fromIntegral nl) sp (fromIntegral sl) dp (fromIntegral dl)
    destroy eidP
    destroy fidP
    destroy ciP
    mapM_ destroy rps
    return ip
  unmarshal ip = alloca $ \eidP ->
    alloca $ \fidP ->
    alloca $ \ciP ->
    alloca $ \rpP ->
    alloca $ \rpL ->
    alloca $ \enP ->
    alloca $ \enL ->
    alloca $ \esP ->
    alloca $ \esL ->
    alloca $ \edP ->
    alloca $ \edL -> do
      poke enP nullPtr
      poke esP nullPtr
      poke edP nullPtr
      c_fromExecutorInfo ip eidP fidP ciP rpP rpL enP enL esP esL edP edL
      eid <- unmarshal =<< peek eidP
      fid <- unmarshal =<< peek fidP
      ci <- unmarshal =<< peek ciP
      rl <- peek rpL
      rps <- peekArray (fromIntegral rl) =<< peek rpP
      rs <- mapM unmarshal rps
      en <- peekMaybeBS enP enL
      es <- peekMaybeBS esP esL
      ed <- peekMaybeBS edP edL
      return $ ExecutorInfo eid fid ci rs en es ed
  destroy = c_destroyExecutorInfo
  equalExceptDefaults (ExecutorInfo id fid ci rs n s d) (ExecutorInfo id' fid' ci' rs' n' s' d') =
    id == id' && fid == fid' && equalExceptDefaults ci ci' && and (zipWith equalExceptDefaults rs rs') && n == n' && s == s' && d == d'

-- *****************************************************************************************************************
-- 
-- *****************************************************************************************************************
foreign import ccall "ext/types.h toResourceStatistics" c_toResourceStatistics
  :: CDouble
  -> Ptr CDouble
  -> Ptr CDouble
  -> CDouble
  -> Ptr CUInt
  -> Ptr CUInt
  -> Ptr CDouble
  -> Ptr CULong
  -> Ptr CULong
  -> Ptr CULong
  -> Ptr CULong
  -> Ptr CULong
  -> IO ResourceStatisticsPtr
foreign import ccall "ext/types.h fromResourceStatistics" c_fromResourceStatistics
  :: ResourceStatisticsPtr
  -> Ptr CDouble
  -> Ptr CDouble
  -> Ptr CBool
  -> Ptr CDouble
  -> Ptr CBool
  -> Ptr CDouble
  -> Ptr CUInt
  -> Ptr CBool
  -> Ptr CUInt
  -> Ptr CBool
  -> Ptr CDouble
  -> Ptr CBool
  -> Ptr CULong
  -> Ptr CBool
  -> Ptr CULong
  -> Ptr CBool
  -> Ptr CULong
  -> Ptr CBool
  -> Ptr CULong
  -> Ptr CBool
  -> Ptr CULong
  -> Ptr CBool
  -> IO ()
foreign import ccall "ext/types.h destroyResourceStatistics" c_destroyResourceStatistics
  :: ResourceStatisticsPtr
  -> IO ()

instance CPPValue ResourceStatistics where
  marshal s = alloca $ \cpuUTS ->
    alloca $ \cpuSTS ->
    alloca $ \cpuPs ->
    alloca $ \cpuT ->
    alloca $ \cpuTTS ->
    alloca $ \memRSS ->
    alloca $ \memLB ->
    alloca $ \memFB ->
    alloca $ \memAB ->
    alloca $ \memMB -> do
      cpuUTS' <- maybe (return nullPtr) (\x -> poke cpuUTS (CDouble x) >> return cpuUTS) $ resourceStatisticsCPUsUserTimeSecs s
      cpuSTS' <- maybe (return nullPtr) (\x -> poke cpuSTS (CDouble x) >> return cpuSTS) $ resourceStatisticsCPUsSystemTimeSecs s
      cpuPs' <- maybe (return nullPtr) (\x -> poke cpuPs (CUInt x) >> return cpuPs) $ resourceCPUsPeriods s
      cpuT' <- maybe (return nullPtr) (\x -> poke cpuT (CUInt x) >> return cpuT) $ resourceCPUsThrottled s
      cpuTTS' <- maybe (return nullPtr) (\x -> poke cpuTTS (CDouble x) >> return cpuTTS) $ resourceCPUsThrottledTimeSecs s
      memRSS' <- maybe (return nullPtr) (\x -> poke memRSS (CULong x) >> return memRSS) $ resourceMemoryResidentSetSize s
      memLB' <- maybe (return nullPtr) (\x -> poke memLB (CULong x) >> return memLB) $ resourceMemoryLimitBytes s
      memFB' <- maybe (return nullPtr) (\x -> poke memFB (CULong x) >> return memFB) $ resourceMemoryFileBytes s
      memAB' <- maybe (return nullPtr) (\x -> poke memAB (CULong x) >> return memAB) $ resourceMemoryAnonymousBytes s
      memMB' <- maybe (return nullPtr) (\x -> poke memMB (CULong x) >> return memMB) $ resourceMemoryMappedFileBytes s
      c_toResourceStatistics (CDouble $ resourceStatisticsTimestamp s)
        cpuUTS'
        cpuSTS'
        (CDouble $ resourceCPUsLimit s)
        cpuPs'
        cpuT'
        cpuTTS'
        memRSS'
        memLB'
        memFB'
        memAB'
        memMB'
  unmarshal s = alloca $ \tsP ->
    alloca $ \cpuLP ->
    alloca $ \cpuUTSP ->
    alloca $ \cpuUTSSP ->
    alloca $ \cpuSTSP ->
    alloca $ \cpuSTSSP ->
    alloca $ \cpuPsP ->
    alloca $ \cpuPsSP ->
    alloca $ \cpuTP ->
    alloca $ \cpuTSP ->
    alloca $ \cpuTTSP ->
    alloca $ \cpuTTSSP ->
    alloca $ \memRSSP ->
    alloca $ \memRSSSP ->
    alloca $ \memLBP ->
    alloca $ \memLBSP ->
    alloca $ \memFBP ->
    alloca $ \memFBSP ->
    alloca $ \memABP ->
    alloca $ \memABSP ->
    alloca $ \memMBP ->
    alloca $ \memMBSP -> do
      c_fromResourceStatistics s
        tsP
        cpuUTSP
        cpuUTSSP
        cpuSTSP
        cpuSTSSP
        cpuLP
        cpuPsP
        cpuPsSP
        cpuTP
        cpuTSP
        cpuTTSP
        cpuTTSSP
        memRSSP
        memRSSSP
        memLBP
        memLBSP
        memFBP
        memFBSP
        memABP
        memABSP
        memMBP
        memMBSP
      (CDouble ts) <- peek tsP
      cpuUTS <- toDouble <$> peekMaybePrim cpuUTSP cpuUTSSP
      cpuSTS <- toDouble <$> peekMaybePrim cpuSTSP cpuSTSSP
      (CDouble l) <- peek cpuLP
      cpuPs <- toWord32 <$> peekMaybePrim cpuPsP cpuPsSP
      cpuT <- toWord32 <$> peekMaybePrim cpuTP cpuTSP
      cpuTTS <- toDouble <$> peekMaybePrim cpuTTSP cpuTTSSP
      memRSS <- toWord64 <$> peekMaybePrim memRSSP memRSSSP
      memLB <- toWord64 <$> peekMaybePrim memLBP memLBSP
      memFB <- toWord64 <$> peekMaybePrim memFBP memFBSP
      memAB <- toWord64 <$> peekMaybePrim memABP memABSP
      memMB <- toWord64 <$> peekMaybePrim memMBP memMBSP
      return $ ResourceStatistics ts cpuUTS cpuSTS l cpuPs cpuT cpuTTS memRSS memLB memFB memAB memMB
      where
        toDouble mx = case mx of
                        Nothing -> Nothing
                        Just (CDouble x) -> Just x
        toWord32 mx = case mx of
                        Nothing -> Nothing
                        Just (CUInt x) -> Just x
        toWord64 mx = case mx of
                        Nothing -> Nothing
                        Just (CULong x) -> Just x
  destroy = c_destroyResourceStatistics
-- *****************************************************************************************************************
-- 
-- *****************************************************************************************************************
foreign import ccall "ext/types.h toResourceUsage" c_toResourceUsage
  :: SlaveIDPtr
  -> FrameworkIDPtr
  -> ExecutorIDPtr
  -> Ptr CChar
  -> CInt
  -> TaskIDPtr
  -> ResourceStatisticsPtr
  -> IO ResourceUsagePtr
foreign import ccall "ext/types.h fromResourceUsage" c_fromResourceUsage
  :: ResourceUsagePtr
  -> Ptr SlaveIDPtr
  -> Ptr FrameworkIDPtr
  -> Ptr ExecutorIDPtr
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> Ptr TaskIDPtr
  -> Ptr ResourceStatisticsPtr
  -> IO ()
foreign import ccall "ext/types.h destroyResourceUsage" c_destroyResourceUsage
  :: ResourceUsagePtr
  -> IO ()

instance CPPValue ResourceUsage where
  marshal (ResourceUsage sid fid eid en tid rs) = do
    sidP <- marshal sid
    fidP <- marshal fid
    eidP <- maybe (return nullPtr) marshal eid
    tidP <- maybe (return nullPtr) marshal tid
    rsP <- maybe (return nullPtr) marshal rs
    result <- maybeUnsafeUseAsCStringLen en $ \(enP, enL) -> do
      c_toResourceUsage sidP fidP eidP enP (fromIntegral enL) tidP rsP
    destroy sidP
    destroy fidP
    destroy eidP
    destroy tidP
    destroy rsP
    return result
  unmarshal up = alloca $ \sidPP ->
    alloca $ \fidPP ->
    alloca $ \eidPP ->
    alloca $ \enPP ->
    alloca $ \enLP ->
    alloca $ \tidPP ->
    alloca $ \rsPP -> do
      poke eidPP nullPtr
      poke enPP nullPtr
      poke tidPP nullPtr
      poke rsPP nullPtr
      c_fromResourceUsage up sidPP fidPP eidPP enPP enLP tidPP rsPP
      sid <- unmarshal =<< peek sidPP
      fid <- unmarshal =<< peek fidPP
      eidP <- peek eidPP
      eid <- if eidP == nullPtr
               then return Nothing
               else Just <$> unmarshal eidP
      en <- peekMaybeBS enPP enLP
      tidP <- peek tidPP
      tid <- if tidP == nullPtr
               then return Nothing
               else Just <$> unmarshal tidP
      rsP <- peek rsPP
      rs <- if rsP == nullPtr
              then return Nothing
              else Just <$> unmarshal rsP
      return $ ResourceUsage sid fid eid en tid rs
  destroy = c_destroyResourceUsage
-- *****************************************************************************************************************
-- 
-- *****************************************************************************************************************
foreign import ccall "ext/types.h toRequest" c_toRequest
  :: SlaveIDPtr
  -> Ptr ResourcePtr
  -> CInt
  -> IO RequestPtr
foreign import ccall "ext/types.h fromRequest" c_fromRequest
  :: RequestPtr
  -> Ptr SlaveIDPtr
  -> Ptr (Ptr ResourcePtr)
  -> Ptr CInt
  -> IO ()
foreign import ccall "ext/types.h destroyRequest" c_destroyRequest
  :: RequestPtr
  -> IO ()
instance CPPValue Request where
  marshal r = do
    sp <- maybe (return nullPtr) marshal $ requestSlaveID r
    rps <- mapM marshal $ reqResources r
    p <- withArrayLen rps $ \rl rpp -> c_toRequest sp rpp (fromIntegral rl)
    destroy sp
    mapM_ destroy rps
    return p
  unmarshal r = alloca $ \spp -> alloca $ \rpp -> alloca $ \rlp -> do
    poke spp nullPtr
    c_fromRequest r spp rpp rlp
    sp <- peek spp
    rl <- peek rlp
    rps <- peekArray (fromIntegral rl) =<< peek rpp
    rs <- mapM unmarshal rps
    s <- if sp == nullPtr
      then return Nothing
      else fmap Just $ unmarshal sp
    return $ Request s rs
  destroy = c_destroyRequest
  equalExceptDefaults (Request sid rs) (Request sid' rs') = sid == sid' && and (zipWith equalExceptDefaults rs rs')
-- *****************************************************************************************************************
-- 
-- *****************************************************************************************************************
foreign import ccall "ext/types.h toOffer" c_toOffer
  :: OfferIDPtr
  -> FrameworkIDPtr
  -> SlaveIDPtr
  -> Ptr CChar
  -> CInt
  -> Ptr ResourcePtr
  -> CInt
  -> Ptr AttributePtr
  -> CInt
  -> Ptr ExecutorIDPtr
  -> CInt
  -> IO OfferPtr
foreign import ccall "ext/types.h fromOffer" c_fromOffer
  :: OfferPtr
  -> Ptr OfferIDPtr
  -> Ptr FrameworkIDPtr
  -> Ptr SlaveIDPtr
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> Ptr (Ptr ResourcePtr)
  -> Ptr CInt
  -> Ptr (Ptr AttributePtr)
  -> Ptr CInt
  -> Ptr (Ptr ExecutorIDPtr)
  -> Ptr CInt
  -> IO ()
foreign import ccall "ext/types.h destroyOffer" c_destroyOffer
  :: OfferPtr
  -> IO ()

instance CPPValue Offer where
  marshal (Offer oid fid sid hn rs as es) = do
    oidP <- marshal oid
    fidP <- marshal fid
    sidP <- marshal sid
    rPs <- mapM marshal rs
    aPs <- mapM (marshal . uncurry Attribute) as
    ePs <- mapM marshal es
    result <- unsafeUseAsCStringLen hn $ \(hnp, hnl) ->
      withArrayLen rPs $ \rLen rPP ->
      withArrayLen aPs $ \aLen aPP ->
      withArrayLen ePs $ \eLen ePP ->
        c_toOffer oidP fidP sidP hnp (fromIntegral hnl) rPP (fromIntegral rLen) aPP (fromIntegral aLen) ePP (fromIntegral eLen)
    destroy oidP
    destroy fidP
    destroy sidP
    mapM_ destroy rPs
    mapM_ destroy aPs
    mapM_ destroy ePs
    return result
  unmarshal op = alloca $ \oidPP ->
    alloca $ \fidPP ->
    alloca $ \sidPP ->
    alloca $ \hnPP ->
    alloca $ \hLenP ->
    alloca $ \rPPP ->
    alloca $ \rLenP ->
    alloca $ \aPPP ->
    alloca $ \aLenP ->
    alloca $ \ePPP ->
    alloca $ \eLenP -> do
      c_fromOffer op oidPP fidPP sidPP hnPP hLenP rPPP rLenP aPPP aLenP ePPP eLenP
      oid <- unmarshal =<< peek oidPP
      fid <- unmarshal =<< peek fidPP
      sid <- unmarshal =<< peek sidPP
      hnP <- peek hnPP
      hLen <- peek hLenP
      hn <- packCStringLen (hnP, fromIntegral hLen)
      rLen <- peek rLenP
      rPP <- peek rPPP
      rs <- mapM unmarshal =<< peekArray (fromIntegral rLen) rPP
      aLen <- peek aLenP
      aPP <- peek aPPP
      as <- mapM unmarshal =<< peekArray (fromIntegral aLen) aPP
      eLen <- peek eLenP
      ePP <- peek ePPP
      es <- mapM unmarshal =<< peekArray (fromIntegral eLen) ePP
      return $ Offer oid fid sid hn rs (map (\(Attribute k v) -> (k, v)) as) es
  destroy = c_destroyOffer
  equalExceptDefaults (Offer oid fid sid hn rs as es) (Offer oid' fid' sid' hn' rs' as' es') =
    oid == oid' && fid == fid' && sid == sid' && hn == hn' &&
      and (zipWith equalExceptDefaults rs rs') &&
      and (zipWith (\l r -> equalExceptDefaults (uncurry Attribute l) (uncurry Attribute r)) as as') &&
      es == es'
-- *****************************************************************************************************************
-- 
-- *****************************************************************************************************************

foreign import ccall "ext/types.h toTaskInfo" c_toTaskInfo
  :: Ptr CChar
  -> CInt
  -> TaskIDPtr
  -> SlaveIDPtr
  -> Ptr ResourcePtr
  -> CInt
  -> ExecutorInfoPtr
  -> CommandInfoPtr
  -> Ptr CChar
  -> CInt
  -> IO TaskInfoPtr
foreign import ccall "ext/types.h fromTaskInfo" c_fromTaskInfo
  :: TaskInfoPtr
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> Ptr TaskIDPtr
  -> Ptr SlaveIDPtr
  -> Ptr (Ptr ResourcePtr)
  -> Ptr CInt
  -> Ptr ExecutorInfoPtr
  -> Ptr CommandInfoPtr
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> IO ()
foreign import ccall "ext/types.h destroyTaskInfo" c_destroyTaskInfo
  :: TaskInfoPtr
  -> IO ()

instance CPPValue TaskInfo where
  marshal t = unsafeUseAsCStringLen (taskInfoName t) $ \(np, nl) -> do
    rps <- mapM marshal (taskResources t)
    tid <- marshal $ taskID t
    sid <- marshal $ taskSlaveID t
    eip <- maybe (return nullPtr) marshal $ case taskImplementation t of
                                              TaskExecutor e -> Just e
                                              _ -> Nothing
    cip <- maybe (return nullPtr) marshal $ case taskImplementation t of
                                              TaskCommand c -> Just c
                                              _ -> Nothing
    p <- maybeUnsafeUseAsCStringLen (taskData t) $ \(tdp, tdl) ->
      withArrayLen rps $ \rl rpp ->
        c_toTaskInfo np (fromIntegral nl) tid sid rpp (fromIntegral rl) eip cip tdp (fromIntegral tdl)
    mapM_ destroy rps
    destroy tid
    destroy sid
    destroy eip
    destroy cip
    return p
  unmarshal t = alloca $ \npp ->
    alloca $ \nlp ->
    alloca $ \tpp ->
    alloca $ \spp ->
    alloca $ \rpp ->
    alloca $ \rlp ->
    alloca $ \epp ->
    alloca $ \cpp ->
    alloca $ \dpp ->
    alloca $ \dlp -> do
      poke epp nullPtr
      poke cpp nullPtr
      poke dpp nullPtr
      c_fromTaskInfo t npp nlp tpp spp rpp rlp epp cpp dpp dlp
      np <- peek npp
      nl <- peek nlp
      n <- packCStringLen (np, fromIntegral nl)
      tp <- peek tpp
      t <- unmarshal tp
      sp <- peek spp
      s <- unmarshal sp
      rp <- peek rpp
      rl <- peek rlp
      rs <- mapM unmarshal =<< peekArray (fromIntegral rl) rp
      ep <- peek epp
      e <- if ep == nullPtr
        then return Nothing
        else fmap Just $ unmarshal ep
      cp <- peek cpp
      c <- if cp == nullPtr
        then return Nothing
        else fmap Just $ unmarshal cp
      let ei = maybe (maybe (error "FATAL: TaskInfo must have CommandInfo or ExecutorInfo") TaskCommand c) TaskExecutor e
      d <- peekMaybeBS dpp dlp
      return $ TaskInfo n t s rs ei d
  destroy = c_destroyTaskInfo
  equalExceptDefaults (TaskInfo n id sid rs ei d) (TaskInfo n' id' sid' rs' ei' d') =
    n == n' && id == id' && sid == sid' && and (zipWith equalExceptDefaults rs rs') && case (ei, ei') of
      (TaskCommand c, TaskCommand c') -> equalExceptDefaults c c'
      (TaskExecutor e, TaskExecutor e') -> equalExceptDefaults e e'
      _ -> False
    where
      mEq ml mr = case equalExceptDefaults <$> ml <*> mr of
        Nothing -> ml == mr
        Just b -> b
-- *****************************************************************************************************************
-- 
-- *****************************************************************************************************************
data Attribute = Attribute
  { attributeName  :: ByteString
  , attributeValue :: Value
  }
  deriving (Show, Eq)
foreign import ccall "ext/types.h toAttribute" c_toAttribute
  :: Ptr CChar
  -> CInt
  -> ValuePtr
  -> IO AttributePtr
foreign import ccall "ext/types.h fromAttribute" c_fromAttribute
  :: AttributePtr
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> Ptr ValuePtr
  -> IO ()
foreign import ccall "ext/types.h destroyAttribute" c_destroyAttribute
  :: AttributePtr
  -> IO ()

instance CPPValue Attribute where
  marshal a = unsafeUseAsCStringLen (attributeName a) $ \(np, nl) -> do
    vp <- marshal $ attributeValue a
    p <- c_toAttribute np (fromIntegral nl) vp
    destroy vp
    return p
  unmarshal ap = alloca $ \npp -> alloca $ \nlp -> alloca $ \vpp -> do
    c_fromAttribute ap npp nlp vpp
    np <- peek npp
    nl <- peek nlp
    vp <- peek vpp
    n <- packCStringLen (np, fromIntegral nl)
    v <- unmarshal vp
    return $ Attribute n v
  destroy = c_destroyAttribute

-- *****************************************************************************************************************
-- 
-- *****************************************************************************************************************
data EnvironmentVariable = EnvironmentVariable
  { environmentVariableKey :: ByteString
  , environmentVariableValue :: ByteString
  }
  deriving (Show, Eq)

foreign import ccall "ext/types.h toEnvironmentVariable" c_toEnvironmentVariable
  :: Ptr CChar
  -> CInt
  -> Ptr CChar
  -> CInt
  -> IO EnvironmentVariablePtr
foreign import ccall "ext/types.h fromEnvironmentVariable" c_fromEnvironmentVariable
  :: EnvironmentVariablePtr
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> IO ()
foreign import ccall "ext/types.h destroyEnvironmentVariable" c_destroyEnvironmentVariable
  :: EnvironmentVariablePtr
  -> IO ()

instance CPPValue EnvironmentVariable where
  marshal e = unsafeUseAsCStringLen (environmentVariableKey e) $ \(kp, kl) ->
    unsafeUseAsCStringLen (environmentVariableValue e) $ \(vp, vl) ->
      c_toEnvironmentVariable kp (fromIntegral kl) vp (fromIntegral vl)
  unmarshal e = alloca $ \kpp ->
    alloca $ \klp ->
    alloca $ \vpp ->
    alloca $ \vlp -> do
      c_fromEnvironmentVariable e kpp klp vpp vlp
      kp <- peek kpp
      kl <- peek klp
      vp <- peek vpp
      vl <- peek vlp
      k <- packCStringLen (kp, fromIntegral kl)
      v <- packCStringLen (vp, fromIntegral vl)
      return $ EnvironmentVariable k v
  destroy = c_destroyEnvironmentVariable

-- *****************************************************************************************************************
-- 
-- *****************************************************************************************************************

data Parameter = Parameter ByteString ByteString
  deriving (Eq, Show)

foreign import ccall "ext/types.h toParameter" c_toParameter
  :: Ptr CChar
  -> CInt
  -> Ptr CChar
  -> CInt
  -> IO ParameterPtr

foreign import ccall "ext/types.h fromParameter" c_fromParameter
  :: ParameterPtr
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> IO ()

foreign import ccall "ext/types.h destroyParameter" c_destroyParameter
  :: ParameterPtr
  -> IO ()

instance CPPValue Parameter where
  marshal (Parameter key value) = unsafeUseAsCStringLen key $ \(kp, kl) ->
    unsafeUseAsCStringLen value $ \(vp, vl) -> c_toParameter kp (fromIntegral kl) vp (fromIntegral vl)
  unmarshal p = alloca $ \kpp -> alloca $ \klp -> alloca $ \vpp -> alloca $ \vlp -> do
    c_fromParameter p kpp klp vpp vlp
    kp <- peek kpp
    kl <- peek klp
    vp <- peek vpp
    vl <- peek vlp
    k <- packCStringLen (kp, fromIntegral kl)
    v <- packCStringLen (vp, fromIntegral vl)
    return $ Parameter k v
  destroy = c_destroyParameter

-- *****************************************************************************************************************
-- 
-- *****************************************************************************************************************

data Parameters = Parameters [Parameter]
  deriving (Eq, Show)

foreign import ccall "ext/types.h toParameters" c_toParameters
  :: Ptr ParameterPtr
  -> CInt
  -> IO ParametersPtr

foreign import ccall "ext/types.h fromParameters" c_fromParameters
  :: ParametersPtr
  -> Ptr (Ptr ParameterPtr)
  -> Ptr CInt
  -> IO ()

foreign import ccall "ext/types.h destroyParameters" c_destroyParameters
  :: ParametersPtr
  -> IO ()

instance CPPValue Parameters where
  marshal (Parameters ps) = do
    pps <- mapM marshal ps
    p <- withArrayLen pps $ \pl pp -> c_toParameters pp (fromIntegral pl)
    mapM_ destroy pps
    return p

  unmarshal p = do
    alloca $ \ppp -> alloca $ \plp -> do
      c_fromParameters p ppp plp
      pp <- peek ppp
      pl <- peek plp
      ps <- peekArray (fromIntegral pl) pp
      fmap Parameters $ mapM unmarshal ps

  destroy = c_destroyParameters

-- *****************************************************************************************************************
-- 
-- *****************************************************************************************************************
foreign import ccall "wrapper" wrapExecutorRegistered
  :: RawExecutorRegistered
  -> IO (FunPtr RawExecutorRegistered)
foreign import ccall "wrapper" wrapExecutorReRegistered
  :: RawExecutorReRegistered
  -> IO (FunPtr RawExecutorReRegistered)
foreign import ccall "wrapper" wrapExecutorDisconnected
  :: RawExecutorDisconnected
  -> IO (FunPtr RawExecutorDisconnected)
foreign import ccall "wrapper" wrapExecutorLaunchTask
  :: RawExecutorLaunchTask
  -> IO (FunPtr RawExecutorLaunchTask)
foreign import ccall "wrapper" wrapExecutorTaskKilled
  :: RawExecutorTaskKilled
  -> IO (FunPtr RawExecutorTaskKilled)
foreign import ccall "wrapper" wrapExecutorFrameworkMessage
  :: RawExecutorFrameworkMessage
  -> IO (FunPtr RawExecutorFrameworkMessage)
foreign import ccall "wrapper" wrapExecutorShutdown
  :: RawExecutorShutdown
  -> IO (FunPtr RawExecutorShutdown)
foreign import ccall "wrapper" wrapExecutorError
  :: RawExecutorError
  -> IO (FunPtr RawExecutorError)

foreign import ccall "createExecutor" c_createExecutor
  :: FunPtr RawExecutorRegistered
  -> FunPtr RawExecutorReRegistered
  -> FunPtr RawExecutorDisconnected
  -> FunPtr RawExecutorLaunchTask
  -> FunPtr RawExecutorTaskKilled
  -> FunPtr RawExecutorFrameworkMessage
  -> FunPtr RawExecutorShutdown
  -> FunPtr RawExecutorError
  -> IO ExecutorPtr
foreign import ccall "destroyExecutor" c_destroyExecutor
  :: ExecutorPtr
  -> IO ()
foreign import ccall "ext/executor.h createExecutorDriver" c_createExecutorDriver
  :: ExecutorPtr
  -> IO ExecutorDriverPtr
foreign import ccall "ext/executor.h destroyExecutorDriver" c_destroyExecutorDriver
  :: ExecutorDriverPtr
  -> IO ()
foreign import ccall "ext/executor.h startExecutorDriver" c_startExecutorDriver
  :: ExecutorDriverPtr
  -> IO CInt
foreign import ccall "ext/executor.h stopExecutorDriver" c_stopExecutorDriver
  :: ExecutorDriverPtr
  -> IO CInt
foreign import ccall "ext/executor.h abortExecutorDriver" c_abortExecutorDriver
  :: ExecutorDriverPtr
  -> IO CInt
foreign import ccall "ext/executor.h joinExecutorDriver" c_joinExecutorDriver
  :: ExecutorDriverPtr
  -> IO CInt
foreign import ccall "ext/executor.h runExecutorDriver" c_runExecutorDriver
  :: ExecutorDriverPtr
  -> IO CInt
foreign import ccall "ext/executor.h sendExecutorDriverStatusUpdate" c_sendExecutorDriverStatusUpdate
  :: ExecutorDriverPtr
  -> TaskStatusPtr
  -> IO CInt
foreign import ccall "ext/executor.h sendExecutorDriverFrameworkMessage" c_sendExecutorDriverFrameworkMessage
  :: ExecutorDriverPtr
  -> Ptr CChar
  -> CInt
  -> IO CInt

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

foreign import ccall "ext/scheduler.h createScheduler" c_createScheduler
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
foreign import ccall "ext/scheduler.h destroyScheduler" c_destroyScheduler
  :: SchedulerPtr
  -> IO ()

foreign import ccall "ext/scheduler.h createSchedulerDriver" c_createSchedulerDriver
  :: SchedulerPtr
  -> FrameworkInfoPtr
  -> Ptr CChar
  -> CInt
  -> IO SchedulerDriverPtr
foreign import ccall "ext/scheduler.h createSchedulerDriverWithCredentials"  c_createSchedulerDriverWithCredentials
  :: SchedulerPtr
  -> FrameworkInfoPtr
  -> Ptr CChar
  -> CInt
  -> CredentialPtr
  -> IO SchedulerDriverPtr
foreign import ccall "ext/scheduler.h destroySchedulerDriver" c_destroySchedulerDriver
  :: SchedulerDriverPtr
  -> IO ()
foreign import ccall "ext/scheduler.h startSchedulerDriver" c_startSchedulerDriver
  :: SchedulerDriverPtr
  -> IO CInt
foreign import ccall "ext/scheduler.h stopSchedulerDriver" c_stopSchedulerDriver
  :: SchedulerDriverPtr
  -> CInt
  -> IO CInt
foreign import ccall "ext/scheduler.h abortSchedulerDriver" c_abortSchedulerDriver
  :: SchedulerDriverPtr
  -> IO CInt
foreign import ccall "ext/scheduler.h joinSchedulerDriver" c_joinSchedulerDriver
  :: SchedulerDriverPtr
  -> IO CInt
foreign import ccall "ext/scheduler.h runSchedulerDriver" c_runSchedulerDriver
  :: SchedulerDriverPtr
  -> IO CInt
foreign import ccall "ext/scheduler.h requestResources" c_requestResources
  :: SchedulerDriverPtr
  -> Ptr RequestPtr
  -> CInt
  -> IO CInt
foreign import ccall "ext/scheduler.h launchTasks" c_launchTasks
  :: SchedulerDriverPtr
  -> Ptr OfferIDPtr
  -> CInt
  -> Ptr TaskInfoPtr
  -> CInt
  -> FiltersPtr
  -> IO CInt
foreign import ccall "ext/scheduler.h killTask" c_killTask
  :: SchedulerDriverPtr
  -> TaskIDPtr
  -> IO CInt
foreign import ccall "ext/scheduler.h declineOffer" c_declineOffer
  :: SchedulerDriverPtr
  -> OfferIDPtr
  -> FiltersPtr
  -> IO CInt
foreign import ccall "ext/scheduler.h reviveOffers" c_reviveOffers
  :: SchedulerDriverPtr
  -> IO CInt
foreign import ccall "ext/scheduler.h schedulerDriverSendFrameworkMessage" c_sendFrameworkMessage
  :: SchedulerDriverPtr
  -> ExecutorIDPtr
  -> SlaveIDPtr
  -> Ptr CChar
  -> CInt
  -> IO CInt
foreign import ccall "ext/scheduler.h reconcileTasks" c_reconcileTasks
  :: SchedulerDriverPtr
  -> Ptr TaskStatusPtr
  -> CInt
  -> IO CInt

foreign import ccall "ext/scheduler.h exerciseMethods" c_exerciseMethods :: SchedulerPtr -> IO ()
