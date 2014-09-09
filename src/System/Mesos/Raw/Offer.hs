module System.Mesos.Raw.Offer where
import           System.Mesos.Internal
import           System.Mesos.Raw.Attribute
import           System.Mesos.Raw.ExecutorId
import           System.Mesos.Raw.FrameworkId
import           System.Mesos.Raw.OfferId
import           System.Mesos.Raw.Resource
import           System.Mesos.Raw.SlaveId

type OfferPtr = Ptr Offer

foreign import ccall unsafe "ext/types.h toOffer" c_toOffer
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

foreign import ccall unsafe "ext/types.h fromOffer" c_fromOffer
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

foreign import ccall unsafe "ext/types.h destroyOffer" c_destroyOffer
  :: OfferPtr
  -> IO ()

instance CPPValue Offer where

  marshal (Offer oid fid sid hn rs as es) = do
    oidP <- cppValue oid
    fidP <- cppValue fid
    sidP <- cppValue sid
    rPs <- mapM cppValue rs
    aPs <- mapM (cppValue . uncurry Attribute) as
    ePs <- mapM cppValue es
    (hnp, hnl) <- cstring hn
    (rPP, rLen) <- arrayLen rPs
    (aPP, aLen) <- arrayLen aPs
    (ePP, eLen) <- arrayLen ePs
    liftIO $ c_toOffer oidP fidP sidP hnp (fromIntegral hnl) rPP (fromIntegral rLen) aPP (fromIntegral aLen) ePP (fromIntegral eLen)

  unmarshal op = do
    oidPP <- alloc
    fidPP <- alloc
    sidPP <- alloc
    hnPP <- alloc
    hLenP <- alloc
    rPPP <- alloc
    rLenP <- alloc
    aPPP <- alloc
    aLenP <- alloc
    ePPP <- alloc
    eLenP <- alloc
    liftIO $ c_fromOffer op oidPP fidPP sidPP hnPP hLenP rPPP rLenP aPPP aLenP ePPP eLenP
    oid <- unmarshal =<< peek oidPP
    fid <- unmarshal =<< peek fidPP
    sid <- unmarshal =<< peek sidPP
    hn <- peekCString (hnPP, hLenP)
    rPP <- peek rPPP
    rs <- mapM unmarshal =<< peekArray (rPP, rLenP)
    aPP <- peek aPPP
    as <- mapM unmarshal =<< peekArray (aPP, aLenP)
    ePP <- peek ePPP
    es <- mapM unmarshal =<< peekArray (ePP, eLenP)
    return $ Offer oid fid sid hn rs (map (\(Attribute k v) -> (k, v)) as) es

  destroy = c_destroyOffer

  equalExceptDefaults (Offer oid fid sid hn rs as es) (Offer oid' fid' sid' hn' rs' as' es') =
    oid == oid' && fid == fid' && sid == sid' && hn == hn' &&
      and (zipWith equalExceptDefaults rs rs') &&
      and (zipWith (\l r -> equalExceptDefaults (uncurry Attribute l) (uncurry Attribute r)) as as') &&
      es == es'
