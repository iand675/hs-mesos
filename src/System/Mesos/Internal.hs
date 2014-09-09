{-# LANGUAGE OverloadedStrings #-}
module System.Mesos.Internal (
  module Control.Applicative,
  module Data.Word,
  module Foreign.C,
  module System.Mesos.Types,
  runManaged,
  Managed,
  CPPValue (..),
  ByteString,
  Ptr.Ptr,
  Ptr.FunPtr,
  Ptr.nullPtr,
  alloc,
  allocMaybe,
  arrayPair,
  peek,
  poke,
  pokeMaybe,
  arrayLen,
  cstring,
  maybeCString,
  peekArray,
  peekArray',
  peekCString,
  peekCString',
  peekMaybeCString,
  cppValue,
  peekCPP,
  peekMaybeCPP,
  CBool,
  toCBool,
  fromCBool,
  toStatus,
  peekMaybe,
  peekMaybeBS,
  peekMaybePrim,
  Ptr.Storable,
  liftIO,
  ToID,
  FromID,
  defEq
) where
import           Control.Monad.Managed


import           Control.Applicative
import           Data.ByteString        (ByteString, packCStringLen)
import           Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import           Data.Word
import           Foreign.C              hiding (peekCString)
import qualified Foreign.Marshal        as Ptr
import qualified Foreign.Ptr            as Ptr
import qualified Foreign.Storable       as Ptr
import           System.Mesos.Types

class CPPValue a where
  marshal :: a -> Managed (Ptr.Ptr a)
  unmarshal :: Ptr.Ptr a -> Managed a
  destroy :: Ptr.Ptr a -> IO ()
  equalExceptDefaults :: Eq a => a -> a -> Bool
  equalExceptDefaults = (==)

alloc :: Ptr.Storable a => Managed (Ptr.Ptr a)
alloc = managed Ptr.alloca

allocMaybe :: Ptr.Storable a => Maybe a -> Managed (Ptr.Ptr a)
allocMaybe = maybe (return Ptr.nullPtr) (\x -> alloc >>= \p -> poke p x >> return p)

arrayPair :: Ptr.Storable a => Managed (Ptr.Ptr a, Ptr.Ptr CInt)
arrayPair = (,) <$> alloc <*> alloc

peek :: Ptr.Storable a => Ptr.Ptr a -> Managed a
peek = liftIO . Ptr.peek

poke :: Ptr.Storable a => Ptr.Ptr a -> a -> Managed ()
poke p x = liftIO $ Ptr.poke p x

pokeMaybe :: Ptr.Storable a => Ptr.Ptr a -> (Maybe a) -> Managed ()
pokeMaybe p m = maybe (return ()) (System.Mesos.Internal.poke p) m
-- pokeMaybe' :: Storable a => Ptr.Ptr (Ptr.Ptr a) -> (Maybe a)

arrayLen :: Ptr.Storable a => [a] -> Managed (Ptr.Ptr a, Int)
arrayLen xs = managed $ Ptr.withArrayLen xs . flip . curry

cstring :: ByteString -> Managed (Ptr.Ptr CChar, Int)
cstring bs = managed $ unsafeUseAsCStringLen bs

peekCString :: (Ptr.Ptr (Ptr.Ptr CChar), Ptr.Ptr CInt) -> Managed ByteString
peekCString (pp, lp) = do
  p <- peek pp
  l <- peek lp
  liftIO $ packCStringLen (p, fromIntegral l)

peekArray :: (Ptr.Ptr (Ptr.Ptr a), Ptr.Ptr CInt) -> Managed [Ptr.Ptr a]
peekArray (pp, lp) = do
  l <- peek lp
  liftIO $ Ptr.peekArray (fromIntegral l) pp

peekArray' :: (Ptr.Ptr (Ptr.Ptr a), Int) -> Managed [Ptr.Ptr a]
peekArray' (pp, l) = liftIO $ Ptr.peekArray l pp

peekCString' :: (Ptr.Ptr (Ptr.Ptr CChar), CInt) -> Managed ByteString
peekCString' (pp, l) = do
  p <- System.Mesos.Internal.peek pp
  liftIO $ packCStringLen (p, fromIntegral l)

peekMaybeCString :: (Ptr.Ptr (Ptr.Ptr CChar), Ptr.Ptr CInt) -> Managed (Maybe ByteString)
peekMaybeCString (pp, lp) = do
  p <- System.Mesos.Internal.peek pp
  if p == Ptr.nullPtr
     then return Nothing
     else do
       l <- System.Mesos.Internal.peek lp
       fmap Just $ liftIO $ packCStringLen (p, fromIntegral l)

cppValue :: CPPValue a => a -> Managed (Ptr.Ptr a)
cppValue y = managed $ (\x f -> with (marshal x) $ \p -> f p >>= \r -> destroy p >> return r) y

peekCPP :: CPPValue a => Ptr.Ptr a -> Managed a
peekCPP = unmarshal

peekMaybeCPP pp = do
  p <- peek pp
  if p == Ptr.nullPtr
    then return Nothing
    else fmap Just $ peekCPP p

type CBool = CUChar

toCBool :: Bool -> CBool
toCBool b = if b then 1 else 0

fromCBool :: CBool -> Bool
fromCBool b = b /= 0

toStatus :: CInt -> Status
toStatus = toEnum . fromIntegral

peekMaybe :: (Ptr.Storable a) => Ptr.Ptr (Ptr.Ptr a) -> Managed (Maybe a)
peekMaybe p = do
  pInner <- System.Mesos.Internal.peek p
  if pInner == Ptr.nullPtr
    then return Nothing
    else System.Mesos.Internal.peek pInner >>= return . Just

peekMaybeBS :: Ptr.Ptr (Ptr.Ptr CChar) -> Ptr.Ptr CInt -> Managed (Maybe ByteString)
peekMaybeBS sp slp = do
  sl <- System.Mesos.Internal.peek slp
  spInner <- System.Mesos.Internal.peek sp
  if spInner == Ptr.nullPtr
    then return Nothing
    else liftIO (packCStringLen (spInner, fromIntegral sl)) >>= return . Just

peekMaybePrim :: Ptr.Storable a => Ptr.Ptr a -> Ptr.Ptr CBool -> Managed (Maybe a)
peekMaybePrim p vsp = do
  set <- System.Mesos.Internal.peek vsp
  if set /= 0
    then fmap Just $ System.Mesos.Internal.peek p
    else return Nothing

maybeCString (Just bs) = cstring bs
maybeCString Nothing = return (Ptr.nullPtr, 0)

defEq :: Eq a => a -> Maybe a -> Maybe a -> Bool
defEq d x x' = x == x' || ((x == Nothing || x == Just d) && (x' == Nothing || x' == Just d))

type ToID a = Ptr.Ptr CChar -> CInt -> IO a
type FromID a = a -> Ptr.Ptr (Ptr.Ptr CChar) -> IO CInt
