module System.Mesos.Raw.Credential where
import           System.Mesos.Internal

type CredentialPtr = Ptr Credential

foreign import ccall unsafe "ext/types.h toCredential" c_toCredential
  :: Ptr CChar
  -> CInt
  -> Ptr CChar
  -> CInt
  -> IO CredentialPtr

foreign import ccall unsafe "ext/types.h fromCredential" c_fromCredential
  :: CredentialPtr
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> IO ()

foreign import ccall unsafe "ext/types.h destroyCredential" c_destroyCredential
  :: CredentialPtr
  -> IO ()

instance CPPValue Credential where

  marshal c = do
    (pp, pl) <- cstring $ credentialPrincipal c
    let call p l = liftIO $ c_toCredential pp (fromIntegral pl) p l
    case credentialSecret c of
      Nothing -> call nullPtr 0
      Just s -> do
        (sp, sl) <- cstring s
        call sp (fromIntegral sl)

  unmarshal cp = do
    p@(pp, plp) <- arrayPair
    s@(sp, slp) <- arrayPair
    liftIO $ c_fromCredential cp pp plp sp slp
    ps <- peekCString p
    ss <- peekMaybeCString s
    return $ Credential ps ss

  destroy = c_destroyCredential
