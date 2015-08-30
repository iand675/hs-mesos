module System.Mesos.Raw.Port where
import           System.Mesos.Internal

type PortPtr = Ptr Port

foreign import ccall unsafe "ext/types.h toPort" c_toPort
  :: CUInt -- ^ number
  -> Ptr CChar  -- ^ name
  -> CInt -- ^ name length
  -> Ptr CChar -- ^ protocol
  -> CInt -- ^ protocol length
  -> IO PortPtr

foreign import ccall unsafe "ext/types.h fromPort" c_fromPort
  :: PortPtr
  -> Ptr CUInt
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> IO ()

foreign import ccall unsafe "ext/types.h destroyPort" c_destroyPort
  :: PortPtr
  -> IO ()

instance CPPValue Port where
  marshal p = do
    (namep, namel) <- maybeCString $ portName p
    (protocol, protocolL) <- maybeCString $ portProtocol p
    liftIO $ c_toPort (fromIntegral (portNumber p))
               namep (fromIntegral namel)
               protocol (fromIntegral protocolL)

  unmarshal p = do
    numberP <- alloc
    (namePP, nameLP) <- arrayPair
    (protocolPP, protocolLP) <- arrayPair
    poke namePP nullPtr
    poke protocolPP nullPtr
    liftIO $ c_fromPort p numberP namePP nameLP protocolPP protocolLP
    (CUInt number) <- peek numberP
    name <- peekMaybeBS namePP nameLP
    protocol <- peekMaybeBS protocolPP protocolLP
    return $ Port number name protocol

  destroy = c_destroyPort
