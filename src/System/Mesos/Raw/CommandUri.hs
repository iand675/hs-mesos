module System.Mesos.Raw.CommandUri where
import           System.Mesos.Internal

type CommandURIPtr = Ptr CommandURI

foreign import ccall unsafe "ext/types.h toCommandURI" c_toCommandURI
  :: Ptr CChar -- cmd
  -> CInt      -- cmdLen
  -> Ptr CBool -- executable
  -> Ptr CBool -- extract
  -> Ptr CBool -- cache
  -> IO CommandURIPtr

foreign import ccall unsafe "ext/types.h fromCommandURI" c_fromCommandURI
  :: CommandURIPtr -- commandURI
  -> Ptr (Ptr CChar) -- cmd
  -> Ptr CInt -- cmdLen
  -> Ptr CBool -- executableSet
  -> Ptr CBool -- executable
  -> Ptr CBool -- extractSet
  -> Ptr CBool -- extract
  -> Ptr CBool -- cacheSet
  -> Ptr CBool -- cache
  -> IO ()

foreign import ccall unsafe "ext/types.h destroyCommandURI" c_destroyCommandURI
  :: CommandURIPtr
  -> IO ()

instance CPPValue CommandURI where

  marshal cu = do
    (vp, vl) <- cstring $ commandURIValue cu
    exec <- allocMaybe $ fmap toCBool $ commandURIExecutable cu
    extract <- allocMaybe $ fmap toCBool $ commandURIExtract cu
    cache <- allocMaybe $ fmap toCBool $ commandURICache cu
    liftIO $ c_toCommandURI vp (fromIntegral vl) exec extract cache

  unmarshal cup = do
    (uriPP, uriLenP) <- arrayPair

    exeSetP <- alloc
    poke exeSetP 0
    exeP <- alloc

    extractSetP <- alloc
    poke extractSetP 0
    extractP <- alloc

    cacheSetP <- alloc
    poke cacheSetP 0
    cacheP <- alloc

    liftIO $ c_fromCommandURI cup uriPP uriLenP exeSetP exeP extractSetP extractP cacheSetP cacheP
    uri <- peekCString (uriPP, uriLenP)

    mset <- peekMaybePrim exeP exeSetP
    mextract <- peekMaybePrim extractP extractSetP
    mcache <- peekMaybePrim cacheP cacheSetP

    return $ CommandURI uri (fmap fromCBool mset) (fmap fromCBool mextract)
               (fmap fromCBool mcache)

  destroy = c_destroyCommandURI

  equalExceptDefaults (CommandURI uri ms mx mc) (CommandURI uri' ms' mx' mc') = uri == uri' && ms == ms' && defEq True mx mx' && mc == mc'
