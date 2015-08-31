{-# LANGUAGE OverloadedStrings #-}
module System.Mesos.Raw.Resource where
import           System.Mesos.Internal
import           System.Mesos.Raw.Value
import           System.Mesos.Raw.Volume

type ResourcePtr = Ptr Resource

foreign import ccall unsafe "ext/types.h toResource" c_toResource
  :: Ptr CChar -- ^ name
  -> CInt -- ^ nameLen
  -> ValuePtr -- ^ value
  -> Ptr CChar -- ^ role
  -> CInt -- ^ roleLen
  -> Ptr CChar -- ^ reservationPrinciple
  -> CInt -- ^ reservationPrincipleLen
  -> Ptr CChar -- ^ diskInfoPersistence
  -> CInt -- ^ diskInfoPersistenceLen
  -> VolumePtr -- ^ diskInfoVolume
  -> IO ResourcePtr
foreign import ccall unsafe "ext/types.h fromResource" c_fromResource
  :: ResourcePtr -- ^ resource
  -> Ptr (Ptr CChar) -- ^ name
  -> Ptr CInt -- ^ nameLen
  -> Ptr ValuePtr -- ^ value
  -> Ptr (Ptr CChar) -- ^ role
  -> Ptr CInt -- ^ roleLen
  -> Ptr (Ptr CChar) -- ^ reservationPrincipal
  -> Ptr CInt -- ^ reservationPrincipalLen
  -> Ptr (Ptr CChar) -- ^ diskInfoPersistence
  -> Ptr CInt -- ^ diskInfoPersistenceLen
  -> Ptr VolumePtr -- ^ diskInfoVolume
  -> IO ()
foreign import ccall unsafe "ext/types.h destroyResource" c_destroyResource
  :: ResourcePtr
  -> IO ()

instance CPPValue Resource where
  marshal r = do
    (np, nl) <- cstring $ resourceName r
    (rp, rl) <- maybeCString $ resourceRole r
    vp <- cppValue $ resourceValue r
    (principalp, principall) <- maybeCString $ resourceReservationInfoPrincipal r
    (persp, persl) <- maybeCString $ resourceDiskInfoPersistence r
    volp <- maybe (return nullPtr) cppValue $ resourceDiskInfoVolume r

    liftIO $ c_toResource np (fromIntegral nl) vp rp (fromIntegral rl)
               principalp (fromIntegral principall) persp (fromIntegral persl)
               volp

  unmarshal r = do
    (npp, nlp) <- arrayPair
    vpp <- alloc
    (rpp, rlp) <- arrayPair
    (resprinpp, resprinlp) <- arrayPair
    (perspp, perslp) <- arrayPair
    volpp <- alloc
    poke npp nullPtr
    poke rpp nullPtr
    poke resprinpp nullPtr
    poke perspp nullPtr
    poke volpp nullPtr
    liftIO $ c_fromResource r npp nlp vpp rpp rlp resprinpp resprinlp perspp perslp volpp
    n <- peekCString (npp, nlp)
    vp <- peek vpp
    v <- unmarshal vp
    r' <- peekMaybeBS rpp rlp
    resprin <- peekMaybeBS resprinpp resprinlp
    pers <- peekMaybeBS perspp perslp
    vol <- peekMaybeCPP volpp
    return $ Resource n v r' resprin pers vol

  destroy = c_destroyResource

  equalExceptDefaults (Resource n v r prin pers vol) (Resource n' v' r' prin' pers' vol') = (n == n') &&
    textToSet v v' &&
    defEq "*" r r' &&
    prin == prin' &&
    pers == pers' &&
    vol == vol'
    where
      textToSet (Text t) (Set s) = [t] == s
      textToSet (Set s) (Text t) = [t] == s
      textToSet x y = equalExceptDefaults x y
