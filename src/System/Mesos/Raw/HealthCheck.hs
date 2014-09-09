{-# LANGUAGE OverloadedStrings #-}
module System.Mesos.Raw.HealthCheck where
import qualified Foreign.Marshal.Array        as A
import           System.Mesos.Internal
import           System.Mesos.Raw.CommandInfo
type HealthCheckPtr = Ptr HealthCheck

foreign import ccall unsafe "ext/types.h toHealthCheck" c_toHealthCheck
  :: CBool -- ^ has_http
  -> CInt -- ^ HealthCheck_HTTP.port
  -> Ptr CChar -- ^ HealthCheck_HTTP.path
  -> CInt -- ^ HealthCheck_HTTP.path size
  -> Ptr CUInt -- ^ HealthCheck_HTTP.statuses
  -> CInt -- ^ HealthCheck_HTTP.statuses size
  -> Ptr CDouble -- ^ delay_seconds
  -> Ptr CDouble -- ^ interval_seconds
  -> Ptr CDouble -- ^ timeout_seconds
  -> Ptr CUInt -- ^ consecutive_failures
  -> Ptr CDouble -- ^ grace_period_seconds
  -> CommandInfoPtr -- ^ command
  -> IO HealthCheckPtr

foreign import ccall unsafe "ext/types.h fromHealthCheck" c_fromHealthCheck
  :: HealthCheckPtr
  -> Ptr CBool       -- ^ has_http
  -> Ptr CInt        -- ^ HealthCheck_HTTP.port
  -> Ptr (Ptr CChar) -- ^ HealthCheck_HTTP.path
  -> Ptr CInt        -- ^ HealthCheck_HTTP.path size
  -> Ptr (Ptr CUInt)  -- ^ HealthCheck_HTTP.statuses
  -> Ptr CInt        -- ^ HealthCheck_HTTP.statuses size
  -> Ptr CDouble     -- ^ delay_seconds
  -> Ptr CBool       -- ^ delay_seconds is set?
  -> Ptr CDouble     -- ^ interval_seconds
  -> Ptr CBool       -- ^ interval_seconds is set?
  -> Ptr CDouble     -- ^ timeout_seconds
  -> Ptr CBool       -- ^ timeout_seconds is set?
  -> Ptr CUInt       -- ^ consecutive_failures
  -> Ptr CBool       -- ^ consecutive_failures is set?
  -> Ptr CDouble         -- ^ grace_period_seconds
  -> Ptr CBool           -- ^ grace_period_seconds is set?
  -> Ptr CommandInfoPtr  -- ^ command
  -> IO ()

foreign import ccall unsafe "ext/types.h destroyHealthCheck" c_destroyHealthCheck
  :: HealthCheckPtr -> IO ()

instance CPPValue HealthCheck where

  marshal hc = do
    ds <- allocMaybe $ fmap CDouble $ healthCheckDelaySeconds hc
    is <- allocMaybe $ fmap CDouble $ healthCheckIntervalSeconds hc
    ts <- allocMaybe $ fmap CDouble $ healthCheckTimeoutSeconds hc
    cf <- allocMaybe $ fmap fromIntegral $ healthCheckConsecutiveFailures hc
    gs <- allocMaybe $ fmap CDouble $ healthCheckGracePeriodSeconds hc
    case healthCheckStrategy hc of
      CommandCheck ci -> do
        cp <- cppValue ci
        liftIO $ c_toHealthCheck 0 0 nullPtr 0 nullPtr 0 ds is ts cf gs cp
      HTTPCheck prt path ss -> do
        (pathP, pathLen) <- maybeCString path
        (ssP, ssLen) <- arrayLen $ map fromIntegral ss
        liftIO $ c_toHealthCheck 1 (fromIntegral prt) pathP (fromIntegral pathLen) ssP (fromIntegral ssLen) ds is ts cf gs nullPtr

  unmarshal p = do
    isHttpP <- alloc
    portP <- alloc
    (pathP, pathL) <- arrayPair
    ssPP <- alloc
    ssL <- alloc
    ds <- alloc
    dss <- alloc
    is <- alloc
    iss <- alloc
    ts <- alloc
    tss <- alloc
    cf <- alloc
    cfs <- alloc
    gs <- alloc
    gss <- alloc
    cmdP <- alloc
    liftIO $ c_fromHealthCheck p isHttpP portP pathP pathL ssPP ssL ds dss is iss ts tss cf cfs gs gss cmdP
    isHttp <- peek isHttpP
    dSecs <- peekMaybePrim ds dss
    iSecs <- peekMaybePrim is iss
    tSecs <- peekMaybePrim ts tss
    cFails <- peekMaybePrim cf cfs
    gSecs <- peekMaybePrim gs gss
    strat <- if fromCBool isHttp
      then do
        port <- peek portP
        path <- peekMaybeCString (pathP, pathL)
        sl <- peek ssL
        ssP <- peek ssPP
        s <- liftIO $ A.peekArray (fromIntegral sl) ssP
        return $ HTTPCheck (fromIntegral port) path $ map (\(CUInt w) -> w) s
      else do
        cmd <- peekCPP =<< peek cmdP
        return $ CommandCheck cmd
    return $ HealthCheck strat (toDouble <$> dSecs) (toDouble <$> iSecs) (toDouble <$> tSecs) (fromIntegral <$> cFails) (toDouble <$> gSecs)
    where
      toDouble (CDouble x) = x

  destroy = c_destroyHealthCheck

  equalExceptDefaults (HealthCheck (HTTPCheck port path ss) ds is ts cf gp) (HealthCheck (HTTPCheck port' path' ss') ds' is' ts' cf' gp') = port == port' && defEq "" path path' && ss == ss' && ds == ds' && is == is' && ts == ts' && cf == cf' && gp == gp'
  equalExceptDefaults (HealthCheck (CommandCheck c) ds is ts cf gp) (HealthCheck (CommandCheck c') ds' is' ts' cf' gp') = ds == ds' && is == is' && ts == ts' && cf == cf' && gp == gp' && equalExceptDefaults c c'
  equalExceptDefaults _ _ = False
