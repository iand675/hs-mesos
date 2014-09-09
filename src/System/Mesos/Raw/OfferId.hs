module System.Mesos.Raw.OfferId where
import           System.Mesos.Internal

type OfferIDPtr = Ptr OfferID

foreign import ccall unsafe "ext/types.h toOfferID" c_toOfferID :: ToID OfferIDPtr

foreign import ccall unsafe "ext/types.h fromOfferID" c_fromOfferID :: FromID OfferIDPtr

foreign import ccall unsafe "ext/types.h destroyOfferID" c_destroyOfferID :: OfferIDPtr -> IO ()

instance CPPValue OfferID where

  marshal x = do
    (strp, l) <- cstring $ fromOfferID x
    liftIO $ c_toOfferID strp $ fromIntegral l

  unmarshal p = fmap OfferID $ do
    ptrPtr <- alloc
    len <- liftIO $ c_fromOfferID p ptrPtr
    peekCString' (ptrPtr, len)

  destroy = c_destroyOfferID
