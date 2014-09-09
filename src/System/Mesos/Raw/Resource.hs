{-# LANGUAGE OverloadedStrings #-}
module System.Mesos.Raw.Resource where
import           System.Mesos.Internal
import           System.Mesos.Raw.Value

type ResourcePtr = Ptr Resource

foreign import ccall unsafe "ext/types.h toResource" c_toResource
  :: Ptr CChar
  -> CInt
  -> ValuePtr
  -> Ptr CChar
  -> CInt
  -> IO ResourcePtr
foreign import ccall unsafe "ext/types.h fromResource" c_fromResource
  :: ResourcePtr
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> Ptr ValuePtr
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> IO ()
foreign import ccall unsafe "ext/types.h destroyResource" c_destroyResource
  :: ResourcePtr
  -> IO ()

instance CPPValue Resource where
  marshal r = do
    (np, nl) <- cstring $ resourceName r
    (rp, rl) <- maybeCString $ resourceRole r
    vp <- cppValue $ resourceValue r
    liftIO $ c_toResource np (fromIntegral nl) vp rp (fromIntegral rl)

  unmarshal r = do
    np@(npp, nlp) <- arrayPair
    vpp <- alloc
    rp@(rpp, rlp) <- arrayPair
    liftIO $ c_fromResource r npp nlp vpp rpp rlp
    n <- peekCString np
    vp <- peek vpp
    v <- unmarshal vp
    r <- peekMaybeCString rp
    return $ Resource n v r

  destroy = c_destroyResource

  equalExceptDefaults (Resource n v r) (Resource n' v' r') = (n == n') &&
    textToSet v v' &&
    defEq "*" r r'
    where
      textToSet (Text t) (Set s) = [t] == s
      textToSet (Set s) (Text t) = [t] == s
      textToSet v v' = equalExceptDefaults v v'
