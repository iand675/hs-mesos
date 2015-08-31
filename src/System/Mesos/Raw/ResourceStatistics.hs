module System.Mesos.Raw.ResourceStatistics where
import           System.Mesos.Internal
import           System.Mesos.Raw.PerformanceStatistics
import           System.Mesos.Raw.TrafficControlStatistics

type ResourceStatisticsPtr = Ptr ResourceStatistics

foreign import ccall unsafe "ext/types.h toResourceStatistics" c_toResourceStatistics
  :: CDouble -- ^ timestamp
  -> Ptr CUInt -- ^ processes
  -> Ptr CUInt -- ^ threads
  -> Ptr CDouble -- ^ cpusUserTimeSecs
  -> Ptr CDouble -- ^ cpusSystemTimeSecs
  -> CDouble -- ^ cpusLimit
  -> Ptr CUInt -- ^ cpusPeriods
  -> Ptr CUInt -- ^ cpusThrottled
  -> Ptr CDouble -- cpusThrottledTimeSecs
  -> Ptr CULong -- ^ memTotalBytes
  -> Ptr CULong -- ^ memTotalMemSwBytes
  -> Ptr CULong -- ^ memLimitBytes
  -> Ptr CULong -- ^ memSoftLimitBytes
  -> Ptr CULong -- ^ memFileBytes
  -> Ptr CULong -- ^ memAnonBytes
  -> Ptr CULong -- ^ memCacheBytes
  -> Ptr CULong -- ^ memRssBytes
  -> Ptr CULong -- ^ memMappedFileBytes
  -> Ptr CULong -- ^ memSwapBytes
  -> Ptr CULong -- ^ memLowPressureCounter
  -> Ptr CULong -- ^ memMediumPressureCounter
  -> Ptr CULong -- ^ memCriticalPressureCounter
  -> Ptr CULong -- ^ diskLimitBytes
  -> Ptr CULong -- ^ diskUsedBytes
  -> PerformanceStatisticsPtr -- ^ perfstatistics
  -> Ptr CULong -- ^ netRxPackets
  -> Ptr CULong -- ^ netRxBytes
  -> Ptr CULong -- ^ netRxErrors
  -> Ptr CULong -- ^ netRxDropped
  -> Ptr CULong -- ^ netTxPackets
  -> Ptr CULong -- ^ netTxBytes
  -> Ptr CULong -- ^ netTxErrors
  -> Ptr CULong -- ^ netTxDropped
  -> Ptr CDouble -- ^ netTcpRttMicroSecsP50
  -> Ptr CDouble -- ^ netTcpRttMicroSecsP90
  -> Ptr CDouble -- ^ netTcpRttMicroSecsP95
  -> Ptr CDouble -- ^ netTcpRttMicroSecsP99
  -> Ptr CDouble -- ^ netTcpActiveConn
  -> Ptr CDouble -- ^ netTcpTimeWaitConn
  -> Ptr TrafficControlStatisticsPtr -- ^ netTrafficStat
  -> CInt -- netTrafficStat count
  -> IO ResourceStatisticsPtr

foreign import ccall unsafe "ext/types.h fromResourceStatistics" c_fromResourceStatistics
  :: ResourceStatisticsPtr
  -> Ptr CDouble -- ^ timestamp
  -> Ptr CUInt -- ^ processes
  -> Ptr CBool -- ^ processes set?
  -> Ptr CUInt -- ^ threads
  -> Ptr CBool -- ^ threads set?
  -> Ptr CDouble -- ^ cpusUserTimeSecs
  -> Ptr CBool  -- ^ cpusUserTimeSecs set?
  -> Ptr CDouble -- ^ cpusSystemTimeSecs
  -> Ptr CBool -- ^ cpusSystemTimeSecs set?
  -> Ptr CDouble -- ^ cpusLimit
  -> Ptr CUInt -- ^ cpusPeriods
  -> Ptr CBool -- ^ cpusPeriods set?
  -> Ptr CUInt -- ^ cpusThrottled
  -> Ptr CBool -- ^ cpusThrottled set?
  -> Ptr CDouble -- ^ cpusThrottledTimeSecs
  -> Ptr CBool -- ^  cpusThrottledTimeSecs set?
  -> Ptr CULong -- ^ memTotalBytes
  -> Ptr CBool -- ^ memTotalBytes set?
  -> Ptr CULong -- ^ memTotalMemSwBytes
  -> Ptr CBool -- ^ memTotalMemSwBytes set?
  -> Ptr CULong -- ^ memLimitBytes
  -> Ptr CBool -- ^ memLimitBytes set?
  -> Ptr CULong -- ^ memSoftLimitBytes
  -> Ptr CBool -- ^ memSoftLimitBytes set?
  -> Ptr CULong -- ^ memFileBytes
  -> Ptr CBool -- ^ memFileBytes set?
  -> Ptr CULong -- ^ memAnonBytes
  -> Ptr CBool -- ^ memAnonBytes set?
  -> Ptr CULong -- ^ memCacheBytes
  -> Ptr CBool -- ^ memCacheBytes set?
  -> Ptr CULong -- ^ memRssBytes
  -> Ptr CBool -- ^ memRSSBytes set?
  -> Ptr CULong -- ^ memMappedFileBytes
  -> Ptr CBool -- ^ memMappedFileBytes set?
  -> Ptr CULong -- ^ memSwapBytes
  -> Ptr CBool -- ^ memSwapBytes set?
  -> Ptr CULong -- ^ memLowPressureCounter
  -> Ptr CBool -- ^ memLowPressureCounter set?
  -> Ptr CULong -- ^ memMediumPressureCounter
  -> Ptr CBool -- ^ memMediumPressureCounter set?
  -> Ptr CULong -- ^ memCriticalPressureCounter
  -> Ptr CBool -- ^ memCriticalPressureCounter set?
  -> Ptr CULong -- ^ diskLimitBytes
  -> Ptr CBool -- ^ diskLimitBytes set?
  -> Ptr CULong -- ^ diskUsedBytes
  -> Ptr CBool -- ^ diskUsedBytes set?
  -> Ptr PerformanceStatisticsPtr -- ^ perfstatistics
  -> Ptr CULong -- ^ netRxPackets
  -> Ptr CBool -- ^ netRxPackets set?
  -> Ptr CULong -- ^ netRxBytes
  -> Ptr CBool -- ^ netRxBytes set?
  -> Ptr CULong -- ^ netRxErrors
  -> Ptr CBool -- ^ netRxErrors set?
  -> Ptr CULong -- ^ netRxDropped
  -> Ptr CBool -- ^ netRxDropped set?
  -> Ptr CULong -- ^ netTxPackets
  -> Ptr CBool -- ^ netTxPackets set?
  -> Ptr CULong -- ^ netTxBytes
  -> Ptr CBool -- ^ netTxBytes set?
  -> Ptr CULong -- ^ netTxErrors
  -> Ptr CBool -- ^ netTxErrors set?
  -> Ptr CULong -- ^ netTxDropped
  -> Ptr CBool -- ^ netTxDropped set?
  -> Ptr CDouble -- ^ netTcpRttMicroSecsP50
  -> Ptr CBool -- ^ netTcpRttMicroSecsP50 set?
  -> Ptr CDouble -- ^ netTcpRttMicroSecsP90
  -> Ptr CBool -- ^ netTcpRttMicroSecsP90 set?
  -> Ptr CDouble -- ^ netTcpRttMicroSecsP95
  -> Ptr CBool -- ^ netTcpRttMicroSecsP95 set?
  -> Ptr CDouble -- ^ netTcpRttMicroSecsP99
  -> Ptr CBool -- ^ netTcpRttMicroSecsP99 set?
  -> Ptr CDouble -- ^ netTcpActiveConn
  -> Ptr CBool -- ^ netTcpActiveConn set?
  -> Ptr CDouble -- ^ netTcpTimeWaitConn
  -> Ptr CBool -- ^ netTcpTimeWaitConn set?
  -> Ptr (Ptr TrafficControlStatisticsPtr) -- ^ netTrafficStat
  -> Ptr CInt -- ^ netTrafficStatCount
  -> IO ()

foreign import ccall unsafe "ext/types.h destroyResourceStatistics" c_destroyResourceStatistics
  :: ResourceStatisticsPtr
  -> IO ()

instance CPPValue ResourceStatistics where
  marshal s = do
    let allocMULong = allocMaybe . fmap CULong
        allocMDouble = allocMaybe . fmap CDouble
        allocMUInt = allocMaybe . fmap CUInt
    procs <- allocMUInt $ resourceStatisticsProcesses s
    threads <- allocMUInt $ resourceStatisticsThreads s
    cpuUTS <- allocMDouble $ resourceStatisticsCpusUserTimeSecs s
    cpuSTS <- allocMDouble $ resourceStatisticsCpusSystemTimeSecs s
    cpuPs <- allocMUInt $ resourceStatisticsCpusPeriods s
    cpuT <- allocMUInt $ resourceStatisticsCpusThrottled s
    cpuTTS <- allocMDouble $ resourceStatisticsCpusThrottledTimeSecs s

    memTB <- allocMULong $ resourceStatisticsMemoryTotalBytes s
    memTSB <- allocMULong $ resourceStatisticsMemoryTotalMemSwBytes s
    memLB <- allocMULong $ resourceStatisticsMemoryLimitBytes s
    memSLB <- allocMULong $ resourceStatisticsMemorySoftLimitBytes s
    memFB <- allocMULong $ resourceStatisticsMemoryFileBytes s
    memAB <- allocMULong $ resourceStatisticsMemoryAnonymousBytes s
    memCB <- allocMULong $ resourceStatisticsMemoryCacheBytes s
    memRSSB <- allocMULong $ resourceStatisticsMemoryRssBytes s
    memMB <- allocMULong $ resourceStatisticsMemoryMappedFileBytes s
    memSB <- allocMULong $ resourceStatisticsMemorySwapBytes s
    memLPC <- allocMULong $ resourceStatisticsMemoryLowPressureCounter s
    memMPC <- allocMULong $ resourceStatisticsMemoryMediumPressureCounter s
    memCPC <- allocMULong $ resourceStatisticsMemoryCriticalPressureCounter s
    diskLB <- allocMULong $ resourceStatisticsDiskLimitBytes s
    diskUB <- allocMULong $ resourceStatisticsDiskUsedBytes s
    perf <- case resourceStatisticsPerformanceStatistics s of
              Nothing -> return nullPtr
              Just p -> cppValue p
    netRXP <- allocMULong $ resourceStatisticsNetRxPackets s
    netRXB <- allocMULong $ resourceStatisticsNetRxBytes s
    netRXE <- allocMULong $ resourceStatisticsNetRxErrors s
    netRXD <- allocMULong $ resourceStatisticsNetRxDropped s
    netTXP <- allocMULong $ resourceStatisticsNetTxPackets s
    netTXB <- allocMULong $ resourceStatisticsNetTxBytes s
    netTXE <- allocMULong $ resourceStatisticsNetTxErrors s
    netTXD <- allocMULong $ resourceStatisticsNetTxDropped s

    tcpRtt50 <- allocMDouble $ resourceStatisticsNetTcpRttMicroSecsP50 s
    tcpRtt90 <- allocMDouble $ resourceStatisticsNetTcpRttMicroSecsP90 s
    tcpRtt95 <- allocMDouble $ resourceStatisticsNetTcpRttMicroSecsP95 s
    tcpRtt99 <- allocMDouble $ resourceStatisticsNetTcpRttMicroSecsP99 s

    tcpActiveConn <- allocMDouble $ resourceStatisticsNetTcpActiveConnections s
    tcpTimeWaitConn <- allocMDouble $ resourceStatisticsNetTcpTimeWaitConnections s

    trafficControlStats <- mapM cppValue $ resourceStatisticsNetTrafficControlStats s
    (trafficControlStatsP, trafficControlStatsCount) <- arrayLen trafficControlStats

    liftIO $ c_toResourceStatistics
               (CDouble $ resourceStatisticsTimestamp s)
               procs
               threads
               cpuUTS
               cpuSTS
               (CDouble $ resourceStatisticsCpusLimit s)
               cpuPs
               cpuT
               cpuTTS
               memTB
               memTSB
               memLB
               memSLB
               memFB
               memAB
               memCB
               memRSSB
               memMB
               memSB
               memLPC
               memMPC
               memCPC
               diskLB
               diskUB
               perf
               netRXP
               netRXB
               netRXE
               netRXD
               netTXP
               netTXB
               netTXE
               netTXD
               tcpRtt50
               tcpRtt90
               tcpRtt95
               tcpRtt99
               tcpActiveConn
               tcpTimeWaitConn
               trafficControlStatsP
               (fromIntegral trafficControlStatsCount)
               

  unmarshal s = do
    tsP <- alloc
    procsP <- alloc
    procsSP <- alloc
    threadsP <- alloc
    threadsSP <- alloc
    cpuUTSP <- alloc
    cpuUTSSP <- alloc
    cpuSTSP <- alloc
    cpuSTSSP <- alloc
    cpuLP <- alloc
    cpuPsP <- alloc
    cpuPsSP <- alloc
    cpuTP <- alloc
    cpuTSP <- alloc
    cpuTTSP <- alloc
    cpuTTSSP <- alloc
    memTBP <- alloc
    memTBSP <- alloc
    memTSBP <- alloc
    memTSBSP <- alloc
    memLBP <- alloc
    memLBSP <- alloc
    memSLBP <- alloc
    memSLBSP <- alloc
    memFBP <- alloc
    memFBSP <- alloc
    memABP <- alloc
    memABSP <- alloc
    memCBP <- alloc
    memCBSP <- alloc
    memRBP <- alloc
    memRBSP <- alloc
    memMBP <- alloc
    memMBSP <- alloc
    memSBP <- alloc
    memSBSP <- alloc
    memLPCP <- alloc
    memLPCSP <- alloc
    memMPCP <- alloc
    memMPCSP <- alloc
    memCPCP <- alloc
    memCPCSP <- alloc
    diskLBP <- alloc
    diskLBSP <- alloc
    diskUBP <- alloc
    diskUBSP <- alloc
    perfP <- alloc
    poke perfP nullPtr
    netRXPP <- alloc
    netRXPSP <- alloc
    netRXBP <- alloc
    netRXBSP <- alloc
    netRXEP <- alloc
    netRXESP <- alloc
    netRXDP <- alloc
    netRXDSP <- alloc
    netTXPP <- alloc
    netTXPSP <- alloc
    netTXBP <- alloc
    netTXBSP <- alloc
    netTXEP <- alloc
    netTXESP <- alloc
    netTXDP <- alloc
    netTXDSP <- alloc
    netRttP50P <- alloc
    netRttP50SP <- alloc
    netRttP90P <- alloc
    netRttP90SP <- alloc
    netRttP95P <- alloc
    netRttP95SP <- alloc
    netRttP99P <- alloc
    netRttP99SP <- alloc
    netTcpActiveConnP <- alloc
    netTcpActiveConnSP <- alloc
    netTcpTimeWaitConnP <- alloc
    netTcpTimeWaitConnSP <- alloc
    netTrafStatsPPP <- alloc
    netTrafStatsLenP <- alloc
    liftIO $ c_fromResourceStatistics s
      tsP
      procsP
      procsSP
      threadsP
      threadsSP
      cpuUTSP
      cpuUTSSP
      cpuSTSP
      cpuSTSSP
      cpuLP
      cpuPsP
      cpuPsSP
      cpuTP
      cpuTSP
      cpuTTSP
      cpuTTSSP
      memTBP
      memTBSP
      memTSBP
      memTSBSP
      memLBP
      memLBSP
      memSLBP
      memSLBSP
      memFBP
      memFBSP
      memABP
      memABSP
      memCBP
      memCBSP
      memRBP
      memRBSP
      memMBP
      memMBSP
      memSBP
      memSBSP
      memLPCP
      memLPCSP
      memMPCP
      memMPCSP
      memCPCP
      memCPCSP
      diskLBP
      diskLBSP
      diskUBP
      diskUBSP
      perfP
      netRXPP
      netRXPSP
      netRXBP
      netRXBSP
      netRXEP
      netRXESP
      netRXDP
      netRXDSP
      netTXPP
      netTXPSP
      netTXBP
      netTXBSP
      netTXEP
      netTXESP
      netTXDP
      netTXDSP
      netRttP50P
      netRttP50SP
      netRttP90P
      netRttP90SP
      netRttP95P
      netRttP95SP
      netRttP99P
      netRttP99SP
      netTcpActiveConnP
      netTcpActiveConnSP
      netTcpTimeWaitConnP
      netTcpTimeWaitConnSP
      netTrafStatsPPP
      netTrafStatsLenP
    (CDouble ts) <- peek tsP
    procs <- toWord32 <$> peekMaybePrim procsP procsSP
    threads <- toWord32 <$> peekMaybePrim threadsP threadsSP 
    cpuUTS <- toDouble <$> peekMaybePrim cpuUTSP cpuUTSSP
    cpuSTS <- toDouble <$> peekMaybePrim cpuSTSP cpuSTSSP
    (CDouble cpuL) <- peek cpuLP
    cpuPs <- toWord32 <$> peekMaybePrim cpuPsP cpuPsSP
    cpuT <- toWord32 <$> peekMaybePrim cpuTP cpuTSP
    cpuTTS <- toDouble <$> peekMaybePrim cpuTTSP cpuTTSSP
    memTB <- toWord64 <$> peekMaybePrim memTBP memTBSP
    memTSB <- toWord64 <$> peekMaybePrim memTSBP memTSBSP
    memLB <- toWord64 <$> peekMaybePrim memLBP memLBSP
    memSLB <- toWord64 <$> peekMaybePrim memSLBP memSLBSP
    memFB <- toWord64 <$> peekMaybePrim memFBP memFBSP
    memAB <- toWord64 <$> peekMaybePrim memABP memABSP
    memCB <- toWord64 <$> peekMaybePrim memCBP memCBSP
    memRB <- toWord64 <$> peekMaybePrim memRBP memRBSP
    memMB <- toWord64 <$> peekMaybePrim memMBP memMBSP
    memSB <- toWord64 <$> peekMaybePrim memSBP memSBSP
    memLPC <- toWord64 <$> peekMaybePrim memLPCP memLPCSP
    memMPC <- toWord64 <$> peekMaybePrim memMPCP memMPCSP
    memCPC <- toWord64 <$> peekMaybePrim memCPCP memCPCSP
    diskLB <- toWord64 <$> peekMaybePrim diskLBP diskLBSP
    diskUB <- toWord64 <$> peekMaybePrim diskUBP diskUBSP
    perfStats <- peekMaybeCPP perfP
    netRXP <- toWord64 <$> peekMaybePrim netRXPP netRXPSP
    netRXB <- toWord64 <$> peekMaybePrim netRXBP netRXBSP
    netRXE <- toWord64 <$> peekMaybePrim netRXEP netRXESP
    netRXD <- toWord64 <$> peekMaybePrim netRXDP netRXDSP
    netTXP <- toWord64 <$> peekMaybePrim netTXPP netTXPSP
    netTXB <- toWord64 <$> peekMaybePrim netTXBP netTXBSP
    netTXE <- toWord64 <$> peekMaybePrim netTXEP netTXESP
    netTXD <- toWord64 <$> peekMaybePrim netTXDP netTXDSP
    netRttP50 <- toDouble <$> peekMaybePrim netRttP50P netRttP50SP
    netRttP90 <- toDouble <$> peekMaybePrim netRttP90P netRttP90SP
    netRttP95 <- toDouble <$> peekMaybePrim netRttP95P netRttP95SP
    netRttP99 <- toDouble <$> peekMaybePrim netRttP99P netRttP99SP
    netTcpActiveConn <- toDouble <$> peekMaybePrim netTcpActiveConnP netTcpActiveConnSP
    netTcpTimeWaitConn <- toDouble <$> peekMaybePrim netTcpTimeWaitConnP netTcpTimeWaitConnSP
    netTrafStatsLen <- peek netTrafStatsLenP
    netTrafStatsPP <- peek netTrafStatsPPP
    netTrafStatsP <- peekArray' (netTrafStatsPP, fromIntegral netTrafStatsLen)
    netTrafStats <- mapM unmarshal netTrafStatsP
    return $ ResourceStatistics
      ts
      procs
      threads
      cpuUTS
      cpuSTS
      cpuL
      cpuPs
      cpuT
      cpuTTS
      memTB
      memTSB
      memLB
      memSLB
      memFB
      memAB
      memCB
      memRB
      memMB
      memSB
      memLPC
      memMPC
      memCPC
      diskLB
      diskUB
      perfStats
      netRXP
      netRXB
      netRXE
      netRXD
      netTXP
      netTXB
      netTXE
      netTXD
      netRttP50
      netRttP90
      netRttP95
      netRttP99
      netTcpActiveConn
      netTcpTimeWaitConn
      netTrafStats
    where
      toDouble mx = case mx of
                      Nothing -> Nothing
                      Just (CDouble x) -> Just x
      toWord32 mx = case mx of
                      Nothing -> Nothing
                      Just (CUInt x) -> Just x
      toWord64 mx = case mx of
                      Nothing -> Nothing
                      Just (CULong x) -> Just x

  destroy = c_destroyResourceStatistics
