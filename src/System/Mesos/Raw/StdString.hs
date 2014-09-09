module System.Mesos.Raw.StdString where
import           System.Mesos.Internal

newtype StdString = StdString { fromStdString :: ByteString }

type StdStringPtr = Ptr StdString

foreign import ccall unsafe "ext/types.h toStdString" c_toStdString
  :: Ptr CChar
  -> CInt
  -> IO StdStringPtr
foreign import ccall unsafe "ext/types.h fromStdString" c_fromStdString
  :: StdStringPtr
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> IO ()
foreign import ccall unsafe "ext/types.h destroyStdString" c_destroyStdString
  :: StdStringPtr
  -> IO ()

instance CPPValue StdString where

  marshal (StdString bs) = do
    (sp, sl) <- cstring bs
    liftIO $ c_toStdString sp $ fromIntegral sl

  unmarshal p = do
    spp <- alloc
    slp <- alloc
    liftIO $ c_fromStdString p spp slp
    StdString <$> peekCString (spp, slp)

  destroy = c_destroyStdString
