module System.Mesos.Raw.Value where
import           System.Mesos.Internal
import           System.Mesos.Raw.StdString

type ValuePtr = Ptr Value

data ValueType
  = SCALAR
  | RANGES
  | SET
  | TEXT

instance Enum ValueType where
  fromEnum SCALAR = 0
  fromEnum RANGES = 1
  fromEnum SET = 2
  fromEnum TEXT = 3
  toEnum 0 = SCALAR
  toEnum 1 = RANGES
  toEnum 2 = SET
  toEnum 3 = TEXT

data ValueRange = ValueRange Word64 Word64
type ValueRangePtr = Ptr ValueRange

foreign import ccall unsafe "ext/types.h toRange" c_toRange
  :: CULong
  -> CULong
  -> IO ValueRangePtr

foreign import ccall unsafe "ext/types.h fromRange" c_fromRange
  :: ValueRangePtr
  -> Ptr CULong
  -> Ptr CULong
  -> IO ()

foreign import ccall unsafe "ext/types.h destroyRange" c_destroyRange
  :: ValueRangePtr
  -> IO ()

instance CPPValue ValueRange where
  marshal (ValueRange l h) = liftIO $ c_toRange (CULong l) (CULong h)

  unmarshal p = do
    l <- alloc
    h <- alloc
    liftIO $ c_fromRange p l h
    (CULong l') <- peek l
    (CULong r') <- peek h
    return $ ValueRange l' r'

  destroy = c_destroyRange

foreign import ccall unsafe "ext/types.h toValue" c_toValue
  :: CInt
  -> CDouble
  -> Ptr ValueRangePtr
  -> CInt
  -> Ptr StdStringPtr
  -> CInt
  -> Ptr CChar
  -> CInt
  -> IO ValuePtr

foreign import ccall unsafe "ext/types.h fromValue" c_fromValue
  :: ValuePtr
  -> Ptr CInt
  -> Ptr CDouble
  -> Ptr (Ptr ValueRangePtr)
  -> Ptr CInt
  -> Ptr (Ptr StdStringPtr)
  -> Ptr CInt
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> IO ()

foreign import ccall unsafe "ext/types.h destroyValue" c_destroyValue
  :: ValuePtr
  -> IO ()

instance CPPValue Value where
  marshal (Scalar x) = liftIO $ c_toValue (fromIntegral $ fromEnum SCALAR) (CDouble x) nullPtr 0 nullPtr 0 nullPtr 0

  marshal (Ranges rs) = do
    ranges <- mapM (cppValue . uncurry ValueRange) rs
    (rp, rLen) <- arrayLen ranges
    liftIO $ c_toValue (fromIntegral $ fromEnum RANGES) 0 rp (fromIntegral rLen) nullPtr 0 nullPtr 0

  marshal (Set ts) = do
    sps <- mapM (cppValue . StdString) ts
    (sp, sl) <- arrayLen sps
    liftIO $ c_toValue (fromIntegral $ fromEnum SET) 0 nullPtr 0 sp (fromIntegral sl) nullPtr 0

  marshal (Text t) = do
    (tp, tl) <- cstring t
    liftIO $ c_toValue (fromIntegral $ fromEnum TEXT) 0 nullPtr 0 nullPtr 0 tp (fromIntegral tl)

  unmarshal vp = do
    typeP <- alloc
    scalarP <- alloc
    rangePP <- alloc
    rangeLenP <- alloc
    setStrPP <- alloc
    setSizeP <- alloc
    t@(textP, textLenP) <- arrayPair
    liftIO $ c_fromValue vp typeP scalarP rangePP rangeLenP setStrPP setSizeP textP textLenP
    ty <- fmap (toEnum . fromIntegral) $ peek typeP
    case ty of
      SCALAR -> peek scalarP >>= \(CDouble d) -> return $ Scalar d
      RANGES -> do
        rangeP <- peek rangePP
        rangePs <- peekArray (rangeP, rangeLenP)
        rs <- mapM unmarshal rangePs
        return $ Ranges $ map (\(ValueRange l h) -> (l, h)) rs
      SET -> do
        setP <- peek setStrPP
        setPs <- peekArray (setP, setSizeP)
        setStrs <- mapM unmarshal setPs
        return $! Set $ map (\(StdString x) -> x) setStrs
      TEXT -> fmap Text $ peekCString t

  destroy = c_destroyValue
