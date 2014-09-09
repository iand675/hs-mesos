module System.Mesos.Raw.Environment where
import           System.Mesos.Internal
import           System.Mesos.Raw.EnvironmentVariable

type EnvironmentPtr = Ptr Environment

newtype Environment = Environment
  { environmentVariables :: [(ByteString, ByteString)]
  }
  deriving (Show, Eq)

fromEnvironment :: Environment -> [(ByteString, ByteString)]
fromEnvironment = environmentVariables

toEnvironment :: [(ByteString, ByteString)] -> Environment
toEnvironment = Environment

foreign import ccall unsafe "ext/types.h toEnvironment" c_toEnvironment
  :: Ptr EnvironmentVariablePtr
  -> CInt
  -> IO EnvironmentPtr

foreign import ccall unsafe "ext/types.h fromEnvironment" c_fromEnvironment
  :: EnvironmentPtr
  -> Ptr (Ptr EnvironmentVariablePtr)
  -> Ptr CInt
  -> IO ()

foreign import ccall unsafe "ext/types.h destroyEnvironment" c_destroyEnvironment
  :: EnvironmentPtr
  -> IO ()

instance CPPValue Environment where
  marshal e = do
    es <- mapM (cppValue . toEnvVar) $ environmentVariables e
    (esp, eLen) <- arrayLen es
    liftIO $ c_toEnvironment esp (fromIntegral eLen)

  unmarshal ep = Environment <$> do
    evpp <- alloc
    evlp <- alloc
    liftIO $ c_fromEnvironment ep evpp evlp
    ev <- peek evpp
    evs <- peekArray (ev, evlp)
    l <- mapM peekCPP evs
    return $ map fromEnvVar l

  destroy = c_destroyEnvironment
