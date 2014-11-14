module System.Mesos.Raw.Filters where
import           System.Mesos.Internal

type FiltersPtr = Ptr Filters

foreign import ccall unsafe "ext/types.h toFilters" c_toFilters
  :: Ptr CDouble
  -> IO FiltersPtr

foreign import ccall unsafe "ext/types.h fromFilters" c_fromFilters
  :: FiltersPtr
  -> Ptr CBool
  -> Ptr CDouble
  -> IO ()

foreign import ccall unsafe "ext/types.h destroyFilters" c_destroyFilters
  :: FiltersPtr
  -> IO ()

instance CPPValue Filters where
  marshal f = case filtersRefuseSeconds f of
    Nothing -> liftIO $ c_toFilters nullPtr
    Just s -> do
      sp <- alloc
      poke sp (CDouble s)
      liftIO $ c_toFilters sp

  unmarshal fp = do
    rsc <- alloc
    rsp <- alloc
    liftIO $ c_fromFilters fp rsc rsp
    ms <- peekMaybePrim rsp rsc
    return $ Filters $ fmap (\(CDouble d) -> d) ms

  destroy = c_destroyFilters

  equalExceptDefaults (Filters f) (Filters f') = defEq 5.0 f f'
