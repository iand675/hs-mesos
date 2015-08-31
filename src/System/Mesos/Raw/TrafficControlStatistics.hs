module System.Mesos.Raw.TrafficControlStatistics where
import           System.Mesos.Internal

type TrafficControlStatisticsPtr = Ptr TrafficControlStatistics

foreign import ccall unsafe "ext/types.h toTrafficControlStatistics" c_toTrafficControlStatistics
  :: Ptr CChar -- ^ id
  -> CInt -- ^ idlen
  -> Ptr CULong -- ^ backlog
  -> Ptr CULong -- ^ bytes
  -> Ptr CULong -- ^ drops
  -> Ptr CULong -- ^ overlimits
  -> Ptr CULong -- ^ packets
  -> Ptr CULong -- ^ qlen
  -> Ptr CULong -- ^ ratebps
  -> Ptr CULong -- ^ ratepps
  -> Ptr CULong -- ^ requeues
  -> IO TrafficControlStatisticsPtr

foreign import ccall unsafe "ext/types.h fromTrafficControlStatistics" c_fromTrafficControlStatistics
  :: TrafficControlStatisticsPtr
  -> Ptr (Ptr CChar) -- ^ id
  -> Ptr CInt -- ^ idlen
  -> Ptr CULong -- ^ backlog
  -> Ptr CBool  -- ^ backlogSet
  -> Ptr CULong -- ^ bytes
  -> Ptr CBool -- ^ bytesSet
  -> Ptr CULong -- ^ drops
  -> Ptr CBool -- ^ bytesSet
  -> Ptr CULong -- ^ overlimits
  -> Ptr CBool -- ^ overlimitsSet
  -> Ptr CULong -- ^ packets
  -> Ptr CBool -- ^ packetsSet
  -> Ptr CULong -- ^ qlen
  -> Ptr CBool -- ^ qlenSet
  -> Ptr CULong -- ^ ratebps
  -> Ptr CBool -- ^ ratebpsSet
  -> Ptr CULong -- ^ ratepps
  -> Ptr CBool -- ^ rateppsSet
  -> Ptr CULong -- ^ requeues
  -> Ptr CBool -- ^ requeuesSet
  -> IO ()

foreign import ccall unsafe "ext/types.h destroyTrafficControlStatistics" c_destroyTrafficControlStatistics
  :: TrafficControlStatisticsPtr
  -> IO ()

instance CPPValue TrafficControlStatistics where
  marshal tcs = do
    (idp, idl) <- cstring $ trafficControlStatisticsIDId' tcs
    blgp <- maybeCULong trafficControlStatisticsBacklog
    bytesp <- maybeCULong trafficControlStatisticsBytes
    dropsp <- maybeCULong trafficControlStatisticsDrops
    overlimitsp <- maybeCULong trafficControlStatisticsOverlimits
    packetsp <- maybeCULong trafficControlStatisticsPackets
    qlenp <- maybeCULong trafficControlStatisticsQlen
    ratebpsp <- maybeCULong trafficControlStatisticsRatebps
    rateppsp <- maybeCULong trafficControlStatisticsRatepps
    requeuesp <- maybeCULong trafficControlStatisticsRequeues

    liftIO $ c_toTrafficControlStatistics idp (fromIntegral idl)
               blgp
               bytesp
               dropsp
               overlimitsp
               packetsp
               qlenp
               ratebpsp
               rateppsp
               requeuesp
   where
     maybeCULong f = allocMaybe . fmap CULong $ f tcs

  unmarshal tcs = do
    (idpp, idlp) <- arrayPair
    backlogp <- alloc
    backlogSetp <- alloc
    bytesp <- alloc
    bytesSetp <- alloc
    dropsp <- alloc
    dropsSetp <- alloc
    overlimitsp <- alloc
    overlimitsSetp <- alloc
    packetsp <- alloc
    packetsSetp <- alloc
    qlenp <- alloc
    qlenSetp <- alloc
    ratebpsp <- alloc
    ratebpsSetp <- alloc
    rateppsp <- alloc
    rateppsSetp <- alloc
    requeuesp <- alloc
    requeuesSetp <- alloc
    liftIO $ c_fromTrafficControlStatistics tcs
               idpp idlp
               backlogp backlogSetp
               bytesp bytesSetp
               dropsp dropsSetp
               overlimitsp overlimitsSetp
               packetsp packetsSetp
               qlenp qlenSetp
               ratebpsp ratebpsSetp
               rateppsp rateppsSetp
               requeuesp requeuesSetp

    tid <- peekCString (idpp, idlp)
    backlog <- toWord64 <$> peekMaybePrim backlogp backlogSetp
    bytes <- toWord64 <$> peekMaybePrim bytesp bytesSetp
    drops <- toWord64 <$> peekMaybePrim dropsp dropsSetp
    overlimits <- toWord64 <$> peekMaybePrim overlimitsp overlimitsSetp
    packets <- toWord64 <$> peekMaybePrim packetsp packetsSetp
    qlen <- toWord64 <$> peekMaybePrim qlenp qlenSetp
    ratebps <- toWord64 <$> peekMaybePrim ratebpsp ratebpsSetp
    ratepps <- toWord64 <$> peekMaybePrim rateppsp rateppsSetp
    requeues <- toWord64 <$> peekMaybePrim requeuesp requeuesSetp
    return $ TrafficControlStatistics tid backlog bytes drops overlimits packets
               qlen ratebps ratepps requeues
   where
    toWord64 mx = case mx of
      Nothing -> Nothing
      Just (CULong x) -> Just x

  destroy = c_destroyTrafficControlStatistics
