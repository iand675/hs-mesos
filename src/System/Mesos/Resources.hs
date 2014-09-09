{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module System.Mesos.Resources where
import           Control.Lens
import           Data.ByteString    (ByteString)
import           Data.List          (find, foldl', groupBy)
import           Data.Word
import           System.Mesos.Types

newtype Resources = Resources { fromResources :: [Resource] }
        deriving (Eq, Show)

instance Wrapped Resources where
  type Unwrapped Resources = [Resource]
  _Wrapped' = iso fromResources Resources

instance (t ~ Resources) => Rewrapped Resources t

instance Ord Resources where
  sub <= super = foldl' go True lgs
    where
      lgs = groupBy (\x y -> resourceName x == resourceName y) $ fromResources sub
      rgs = groupBy (\x y -> resourceName x == resourceName y) $ fromResources super
      go b res = case find (\x -> (res ^? traverse . to resourceName) == (x ^? traverse . to resourceName)) rgs of
                   Nothing -> False
                   Just gs -> b && (sumOf (traverse . value . scalar) res <= sumOf (traverse . value . scalar) gs)


class HasResources a where
  resources :: Lens' a [Resource]

instance HasResources SlaveInfo where
  resources = lens slaveInfoResources (\s rs -> s { slaveInfoResources = rs })

instance HasResources ExecutorInfo where
  resources = lens executorInfoResources (\e rs -> e { executorInfoResources = rs })

instance HasResources Request where
  resources = lens reqResources (\r rs -> r { reqResources = rs })

instance HasResources Offer where
  resources = lens offerResources (\o rs -> o { offerResources = rs })

instance HasResources TaskInfo where
  resources = lens taskResources (\t rs -> t { taskResources = rs })

value :: Lens' Resource Value
value = lens resourceValue $ \r v -> r { resourceValue = v }

scalar :: Prism' Value Double
scalar = prism' Scalar $ \x -> case x of
                                 Scalar d -> Just d
                                 _ -> Nothing

ranges :: Prism' Value [(Word64, Word64)]
ranges = prism' Ranges $ \x -> case x of
                                 Ranges rs -> Just rs
                                 _ -> Nothing

set :: Prism' Value [ByteString]
set = prism' Set $ \x -> case x of
                           Set bs -> Just bs
                           _ -> Nothing

text :: Prism' Value ByteString
text = prism' Text $ \x -> case x of
                             Text t -> Just t
                             _ -> Nothing

cpus :: Prism' Resource Double
cpus = prism' (\x -> Resource "cpus" (Scalar x) (Just "*")) $ \r ->
  if resourceName r == "cpus"
     then r ^? value . scalar
     else Nothing

mem :: Prism' Resource Double
mem = prism' (\x -> Resource "mem" (Scalar x) (Just "*")) $ \r ->
  if resourceName r == "mem"
    then r ^? value . scalar
    else Nothing

disk :: Prism' Resource Double
disk = prism' (\x -> Resource "disk" (Scalar x) (Just "*")) $ \r ->
  if resourceName r == "disk"
     then r ^? value . scalar
     else Nothing

ports :: Prism' Resource [(Word64, Word64)]
ports = prism' (\x -> Resource "ports" (Ranges x) (Just "*")) $ \r ->
  if resourceName r == "ports"
     then r ^? value . ranges
     else Nothing

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

