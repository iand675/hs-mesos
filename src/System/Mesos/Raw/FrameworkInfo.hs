{-# LANGUAGE OverloadedStrings #-}
module System.Mesos.Raw.FrameworkInfo where
import           System.Mesos.Internal
import           System.Mesos.Raw.FrameworkId

type FrameworkInfoPtr = Ptr FrameworkInfo

foreign import ccall "ext/types.h toFrameworkInfo" c_toFrameworkInfo
  :: Ptr CChar
  -> CInt
  -> Ptr CChar
  -> CInt
  -> Ptr FrameworkIDPtr
  -> Ptr CDouble
  -> Ptr CBool
  -> Ptr CChar
  -> CInt
  -> Ptr CChar
  -> CInt
  -> Ptr CChar
  -> CInt
  -> IO FrameworkInfoPtr

foreign import ccall "ext/types.h fromFrameworkInfo" c_fromFrameworkInfo
  :: FrameworkInfoPtr
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> Ptr FrameworkIDPtr
  -> Ptr CBool
  -> Ptr CDouble
  -> Ptr CBool
  -> Ptr CBool
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> IO ()

foreign import ccall "ext/types.h destroyFrameworkInfo" c_destroyFrameworkInfo
  :: FrameworkInfoPtr
  -> IO ()

instance CPPValue FrameworkInfo where
  marshal fi = do
    (up, ul) <- cstring $ frameworkUser fi
    (np, nl) <- cstring $ frameworkName fi
    (rp, rl) <- maybeCString $ frameworkRole fi
    (hp, hl) <- maybeCString $ frameworkHostname fi
    (pp, pl) <- maybeCString $ frameworkPrincipal fi
    fp' <- allocMaybe $ fmap CDouble $ frameworkFailoverTimeout fi
    cp' <- allocMaybe $ fmap toCBool $ frameworkCheckpoint fi
    let fidFun f = case frameworkID fi of
          Nothing -> f nullPtr
          Just r -> do
            p <- alloc
            fidp <- cppValue r
            poke p fidp
            f p
    fidFun $ \fidp -> liftIO $ c_toFrameworkInfo up
      (fromIntegral ul)
      np
      (fromIntegral nl)
      fidp
      fp'
      cp'
      rp
      (fromIntegral rl)
      hp
      (fromIntegral hl)
      pp
      (fromIntegral pl)

  unmarshal fp = do
    (up, ul) <- arrayPair
    (np, nl) <- arrayPair
    idp <- alloc
    tps <- alloc
    tp <- alloc
    cps <- alloc
    cp <- alloc
    (rp, rl) <- arrayPair
    (hp, hl) <- arrayPair
    (pp, pl) <- arrayPair

    poke up nullPtr
    poke ul 0
    poke np nullPtr
    poke nl 0
    poke idp nullPtr
    poke rp nullPtr
    poke rl 0
    poke hp nullPtr
    poke hl 0
    poke pp nullPtr
    poke pl 0
    liftIO $ c_fromFrameworkInfo fp up ul np nl idp tps tp cps cp rp rl hp hl pp pl
    ubs <- peekCString (up, ul)
    nbs <- peekCString (np, nl)
    mid <- do
      midp <- peek idp
      if midp == nullPtr
        then return Nothing
        else fmap Just $ unmarshal midp
    mt <- fmap (fmap (\(CDouble d) -> d)) $ peekMaybePrim tp tps
    mc <- fmap (fmap (== 1)) $ peekMaybePrim cp cps
    mr <- peekMaybeBS rp rl
    mh <- peekMaybeBS hp hl
    mp <- peekMaybeBS pp pl
    return $ FrameworkInfo ubs nbs mid mt mc mr mh mp

  destroy = c_destroyFrameworkInfo

  equalExceptDefaults (FrameworkInfo u n i ft cp r hn p) (FrameworkInfo u' n' i' ft' cp' r' hn' p') =
    u == u' && n == n' && i == i' && defEq 0 ft ft' && defEq False cp cp' && defEq "*" r r' && hn == hn' && p == p'
