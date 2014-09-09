module System.Mesos.Raw.EnvironmentVariable where
import           System.Mesos.Internal

toEnvVar :: (ByteString, ByteString) -> EnvironmentVariable
toEnvVar (k, v) = EnvironmentVariable k v

fromEnvVar :: EnvironmentVariable -> (ByteString, ByteString)
fromEnvVar (EnvironmentVariable k v) = (k, v)

type EnvironmentVariablePtr = Ptr EnvironmentVariable

data EnvironmentVariable = EnvironmentVariable
  { environmentVariableKey   :: !ByteString
  , environmentVariableValue :: !ByteString
  } deriving (Show, Eq)

foreign import ccall unsafe "ext/types.h toEnvironmentVariable" c_toEnvironmentVariable
  :: Ptr CChar
  -> CInt
  -> Ptr CChar
  -> CInt
  -> IO EnvironmentVariablePtr

foreign import ccall unsafe "ext/types.h fromEnvironmentVariable" c_fromEnvironmentVariable
  :: EnvironmentVariablePtr
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> IO ()

foreign import ccall unsafe "ext/types.h destroyEnvironmentVariable" c_destroyEnvironmentVariable
  :: EnvironmentVariablePtr
  -> IO ()

instance CPPValue EnvironmentVariable where

  marshal e = do
    (kp, kl) <- cstring $ environmentVariableKey e
    (vp, vl) <- cstring $ environmentVariableValue e
    liftIO $ c_toEnvironmentVariable kp (fromIntegral kl) vp (fromIntegral vl)

  unmarshal e = do
    kp@(kpp, klp) <- arrayPair
    vp@(vpp, vlp) <- arrayPair
    liftIO $ c_fromEnvironmentVariable e kpp klp vpp vlp
    k <- peekCString kp
    v <- peekCString vp
    return $ EnvironmentVariable k v

  destroy = c_destroyEnvironmentVariable
