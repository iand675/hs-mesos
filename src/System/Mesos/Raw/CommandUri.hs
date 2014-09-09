module System.Mesos.Raw.CommandUri where
import           System.Mesos.Internal

type CommandURIPtr = Ptr CommandURI

foreign import ccall unsafe "ext/types.h toCommandURI" c_toCommandURI
  :: Ptr CChar
  -> CInt
  -> Ptr CBool
  -> Ptr CBool
  -> IO CommandURIPtr

foreign import ccall unsafe "ext/types.h fromCommandURI" c_fromCommandURI
  :: CommandURIPtr
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> Ptr CBool
  -> Ptr CBool
  -> Ptr CBool
  -> Ptr CBool
  -> IO ()

foreign import ccall unsafe "ext/types.h destroyCommandURI" c_destroyCommandURI
  :: CommandURIPtr
  -> IO ()

instance CPPValue CommandURI where

  marshal cu = do
    (vp, vl) <- cstring $ commandURIValue cu
    exec <- allocMaybe $ fmap toCBool $ commandURIExecutable cu
    extract <- allocMaybe $ fmap toCBool $ commandURIExtract cu
    liftIO $ c_toCommandURI vp (fromIntegral vl) exec extract

  unmarshal cup = do
    (uriPP, uriLenP) <- arrayPair

    exeSetP <- alloc
    poke exeSetP 0
    exeP <- alloc

    extractSetP <- alloc
    poke extractSetP 0
    extractP <- alloc

    liftIO $ c_fromCommandURI cup uriPP uriLenP exeSetP exeP extractSetP extractP
    uri <- peekCString (uriPP, uriLenP)

    mset <- peekMaybePrim exeP exeSetP
    mextract <- peekMaybePrim extractP extractSetP

    return $ CommandURI uri (fmap fromCBool mset) (fmap fromCBool mextract)

  destroy = c_destroyCommandURI

  equalExceptDefaults (CommandURI uri ms mx) (CommandURI uri' ms' mx') = uri == uri' && ms == ms' && defEq True mx mx'
