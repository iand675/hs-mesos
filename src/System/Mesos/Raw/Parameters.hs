module System.Mesos.Raw.Parameters where
import           System.Mesos.Internal
import           System.Mesos.Raw.Parameter

type ParametersPtr = Ptr Parameters

newtype Parameters = Parameters [Parameter]
  deriving (Eq, Show)

foreign import ccall unsafe "ext/types.h toParameters" c_toParameters
  :: Ptr ParameterPtr
  -> CInt
  -> IO ParametersPtr

foreign import ccall unsafe "ext/types.h fromParameters" c_fromParameters
  :: ParametersPtr
  -> Ptr (Ptr ParameterPtr)
  -> Ptr CInt
  -> IO ()

foreign import ccall unsafe "ext/types.h destroyParameters" c_destroyParameters
  :: ParametersPtr
  -> IO ()

instance CPPValue Parameters where

  marshal (Parameters ps) = do
    pps <- mapM cppValue ps
    (pp, pl) <- arrayLen pps
    liftIO $ c_toParameters pp (fromIntegral pl)

  unmarshal p = do
    ppp <- alloc
    plp <- alloc
    liftIO $ c_fromParameters p ppp plp
    pp <- peek ppp
    pl <- peek plp
    ps <- peekArray' (pp, fromIntegral pl)
    fmap Parameters $ mapM unmarshal ps

  destroy = c_destroyParameters
