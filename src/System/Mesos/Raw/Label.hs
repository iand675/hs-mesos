module System.Mesos.Raw.Label where
import           System.Mesos.Internal

type LabelPtr = Ptr Label

newtype Label = Label
  { unLabel :: (ByteString, Maybe ByteString)
  }
  deriving (Show, Eq)

fromLabel :: Label -> (ByteString, Maybe ByteString)
fromLabel = unLabel

toLabel :: (ByteString, Maybe ByteString) -> Label
toLabel = Label

foreign import ccall unsafe "ext/types.h toLabel" c_toLabel
  :: Ptr CChar -- ^ key
  -> CInt -- ^ key length
  -> Ptr CChar -- ^ value
  -> CInt -- ^ value length
  -> IO LabelPtr

foreign import ccall unsafe "ext/types.h fromLabel" c_fromLabel
  :: LabelPtr
  -> Ptr (Ptr CChar) -- ^ key
  -> Ptr CInt -- ^ key length
  -> Ptr (Ptr CChar) -- ^ value
  -> Ptr CInt -- ^ value length
  -> IO ()

foreign import ccall unsafe "ext/types.h destroyLabel" c_destroyLabel
  :: LabelPtr
  -> IO ()

instance CPPValue Label where
  marshal l = do
    let (labelKey, labelVal) = unLabel l
    (keyP, keyl) <- cstring labelKey
    (valueP, valuel) <- maybeCString labelVal
    liftIO $ c_toLabel keyP (fromIntegral keyl)
                       valueP (fromIntegral valuel)

  unmarshal l = do

    (keyPP, keylP) <- arrayPair
    (valuePP, valuelP) <- arrayPair
    poke keyPP nullPtr
    poke valuePP nullPtr

    liftIO $ c_fromLabel l keyPP keylP valuePP valuelP

    key <- peekCString (keyPP, keylP)
    value <- peekMaybeBS valuePP valuelP
    return $ Label (key, value)

  destroy = c_destroyLabel
