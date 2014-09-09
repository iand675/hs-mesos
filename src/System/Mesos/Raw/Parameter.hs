module System.Mesos.Raw.Parameter where
import           System.Mesos.Internal

type ParameterPtr = Ptr Parameter

data Parameter = Parameter ByteString ByteString
  deriving (Eq, Show)

foreign import ccall unsafe "ext/types.h toParameter" c_toParameter
  :: Ptr CChar
  -> CInt
  -> Ptr CChar
  -> CInt
  -> IO ParameterPtr

foreign import ccall unsafe "ext/types.h fromParameter" c_fromParameter
  :: ParameterPtr
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> IO ()

foreign import ccall unsafe "ext/types.h destroyParameter" c_destroyParameter
  :: ParameterPtr
  -> IO ()

instance CPPValue Parameter where

  marshal (Parameter key value) = do
    k@(kp, kl) <- cstring key
    v@(vp, vl) <- cstring value
    liftIO $ c_toParameter kp (fromIntegral kl) vp (fromIntegral vl)

  unmarshal p = do
    kp@(kpp, klp) <- arrayPair
    vp@(vpp, vlp) <- arrayPair
    liftIO $ c_fromParameter p kpp klp vpp vlp
    k <- peekCString kp
    v <- peekCString vp
    return $ Parameter k v

  destroy = c_destroyParameter
