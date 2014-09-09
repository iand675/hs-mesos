{-# LANGUAGE OverloadedStrings #-}
module System.Mesos.Raw.Attribute where
import           System.Mesos.Internal
import           System.Mesos.Raw.Value

type AttributePtr = Ptr Attribute

data Attribute = Attribute
  { attributeName  :: !ByteString
  , attributeValue :: !Value
  } deriving (Show, Eq)

toAttribute :: (ByteString, Value) -> Attribute
toAttribute (k, v) = Attribute k v

fromAttribute :: Attribute -> (ByteString, Value)
fromAttribute (Attribute k v) = (k, v)

foreign import ccall unsafe "ext/types.h toAttribute" c_toAttribute
  :: Ptr CChar
  -> CInt
  -> ValuePtr
  -> IO AttributePtr

foreign import ccall unsafe "ext/types.h fromAttribute" c_fromAttribute
  :: AttributePtr
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> Ptr ValuePtr
  -> IO ()

foreign import ccall unsafe "ext/types.h destroyAttribute" c_destroyAttribute
  :: AttributePtr
  -> IO ()

instance CPPValue Attribute where

  marshal a = do
    (np, nl) <- cstring $ attributeName a
    vp <- cppValue $ attributeValue a
    p <- liftIO $ c_toAttribute np (fromIntegral nl) vp
    return p

  unmarshal ap = do
    nps@(npp, nlp) <- arrayPair
    vpp <- alloc
    liftIO $ c_fromAttribute ap npp nlp vpp
    vp <- peek vpp
    n <- peekCString nps
    v <- peekCPP vp
    return $ Attribute n v

  destroy = c_destroyAttribute
