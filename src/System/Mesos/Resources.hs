{-# LANGUAGE OverloadedStrings #-}
module System.Mesos.Resources where
import Control.Lens
import Data.Word
import Data.List (find)
import System.Mesos.Types

class HasResources a where
  resources :: Lens' a [Resource]

instance HasResources SlaveInfo
instance HasResources ExecutorInfo
instance HasResources Request
instance HasResources Offer
instance HasResources TaskInfo

cpus :: Prism' [Resource] Double
cpus = prism' (\x -> [Resource "cpus" (Scalar x) (Just "*")]) $ \rs -> do
  (Resource _ (Scalar x) _) <- find (\r -> resourceName r == "cpus") rs
  return x

mem :: Prism' [Resource] Double
mem = prism' (\x -> [Resource "mem" (Scalar x) (Just "*")]) $ \rs -> do
  (Resource _ (Scalar x) _) <- find (\r -> resourceName r == "mem") rs
  return x


disk :: Prism' [Resource] Double
disk = prism' (\x -> [Resource "disk" (Scalar x) (Just "*")]) $ \rs -> do
  (Resource _ (Scalar x) _) <- find (\r -> resourceName r == "disk") rs
  return x

ports :: Prism' [Resource] [(Word64, Word64)]
ports = prism' (\x -> [Resource "ports" (Ranges x) (Just "*")]) $ \rs -> do
  (Resource _ (Ranges r) _) <- find (\r -> resourceName r == "ports") rs
  return r


flattened :: Getter [Resource] [Resource]
flattened = undefined

{-
extract
find
get
getAll
parse
parse'
isValid
isAllocatable
isZero
-}

