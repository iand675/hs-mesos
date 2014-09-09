module System.Mesos.Raw.ResourceStatistics where
import           System.Mesos.Internal
import           System.Mesos.Raw.PerformanceStatistics

type ResourceStatisticsPtr = Ptr ResourceStatistics

foreign import ccall unsafe "ext/types.h toResourceStatistics" c_toResourceStatistics
  :: CDouble
  -> Ptr CDouble
  -> Ptr CDouble
  -> CDouble
  -> Ptr CUInt
  -> Ptr CUInt
  -> Ptr CDouble
  -> Ptr CULong
  -> Ptr CULong
  -> Ptr CULong
  -> Ptr CULong
  -> Ptr CULong
  -> PerformanceStatisticsPtr
  -> Ptr CULong
  -> Ptr CULong
  -> Ptr CULong
  -> Ptr CULong
  -> Ptr CULong
  -> Ptr CULong
  -> Ptr CULong
  -> Ptr CULong
  -> IO ResourceStatisticsPtr

foreign import ccall unsafe "ext/types.h fromResourceStatistics" c_fromResourceStatistics
  :: ResourceStatisticsPtr
  -> Ptr CDouble
  -> Ptr CDouble
  -> Ptr CBool
  -> Ptr CDouble
  -> Ptr CBool
  -> Ptr CDouble
  -> Ptr CUInt
  -> Ptr CBool
  -> Ptr CUInt
  -> Ptr CBool
  -> Ptr CDouble
  -> Ptr CBool
  -> Ptr CULong
  -> Ptr CBool
  -> Ptr CULong
  -> Ptr CBool
  -> Ptr CULong
  -> Ptr CBool
  -> Ptr CULong
  -> Ptr CBool
  -> Ptr CULong
  -> Ptr CBool
  -> Ptr PerformanceStatisticsPtr
  -> Ptr CULong
  -> Ptr CBool
  -> Ptr CULong
  -> Ptr CBool
  -> Ptr CULong
  -> Ptr CBool
  -> Ptr CULong
  -> Ptr CBool
  -> Ptr CULong
  -> Ptr CBool
  -> Ptr CULong
  -> Ptr CBool
  -> Ptr CULong
  -> Ptr CBool
  -> Ptr CULong
  -> Ptr CBool
  -> IO ()

foreign import ccall unsafe "ext/types.h destroyResourceStatistics" c_destroyResourceStatistics
  :: ResourceStatisticsPtr
  -> IO ()

instance CPPValue ResourceStatistics where

  marshal s = do
    cpuUTS <- alloc
    cpuSTS <- alloc
    cpuPs <- alloc
    cpuT <- alloc
    cpuTTS <- alloc
    memRSS <- alloc
    memLB <- alloc
    memFB <- alloc
    memAB <- alloc
    memMB <- alloc
    cpuUTS' <- maybe (return nullPtr) (\x -> poke cpuUTS (CDouble x) >> return cpuUTS) $ resourceStatisticsCPUsUserTimeSecs s
    cpuSTS' <- maybe (return nullPtr) (\x -> poke cpuSTS (CDouble x) >> return cpuSTS) $ resourceStatisticsCPUsSystemTimeSecs s
    cpuPs' <- maybe (return nullPtr) (\x -> poke cpuPs (CUInt x) >> return cpuPs) $ resourceCPUsPeriods s
    cpuT' <- maybe (return nullPtr) (\x -> poke cpuT (CUInt x) >> return cpuT) $ resourceCPUsThrottled s
    cpuTTS' <- maybe (return nullPtr) (\x -> poke cpuTTS (CDouble x) >> return cpuTTS) $ resourceCPUsThrottledTimeSecs s
    memRSS' <- maybe (return nullPtr) (\x -> poke memRSS (CULong x) >> return memRSS) $ resourceMemoryResidentSetSize s
    memLB' <- maybe (return nullPtr) (\x -> poke memLB (CULong x) >> return memLB) $ resourceMemoryLimitBytes s
    memFB' <- maybe (return nullPtr) (\x -> poke memFB (CULong x) >> return memFB) $ resourceMemoryFileBytes s
    memAB' <- maybe (return nullPtr) (\x -> poke memAB (CULong x) >> return memAB) $ resourceMemoryAnonymousBytes s
    memMB' <- maybe (return nullPtr) (\x -> poke memMB (CULong x) >> return memMB) $ resourceMemoryMappedFileBytes s
    perf <- case resourcePerformanceStatistics s of
              Nothing -> return nullPtr
              Just p -> cppValue p
    let allocMStat = allocMaybe . fmap CULong
    netRXP <- allocMStat $ resourceNetRxPackets s
    netRXB <- allocMStat $ resourceNetRxBytes s
    netRXE <- allocMStat $ resourceNetRxErrors s
    netRXD <- allocMStat $ resourceNetRxDropped s
    netTXP <- allocMStat $ resourceNetTxPackets s
    netTXB <- allocMStat $ resourceNetTxBytes s
    netTXE <- allocMStat $ resourceNetTxErrors s
    netTXD <- allocMStat $ resourceNetTxDropped s
    liftIO $ c_toResourceStatistics (CDouble $ resourceStatisticsTimestamp s)

               cpuUTS'
               cpuSTS'
               (CDouble $ resourceCPUsLimit s)
               cpuPs'
               cpuT'
               cpuTTS'
               memRSS'
               memLB'
               memFB'
               memAB'
               memMB'
               perf
               netRXP
               netRXB
               netRXE
               netRXD
               netTXP
               netTXB
               netTXE
               netTXD

  unmarshal s = do
    tsP <- alloc
    cpuLP <- alloc
    cpuUTSP <- alloc
    cpuUTSSP <- alloc
    cpuSTSP <- alloc
    cpuSTSSP <- alloc
    cpuPsP <- alloc
    cpuPsSP <- alloc
    cpuTP <- alloc
    cpuTSP <- alloc
    cpuTTSP <- alloc
    cpuTTSSP <- alloc
    memRSSP <- alloc
    memRSSSP <- alloc
    memLBP <- alloc
    memLBSP <- alloc
    memFBP <- alloc
    memFBSP <- alloc
    memABP <- alloc
    memABSP <- alloc
    memMBP <- alloc
    memMBSP <- alloc
    perfP <- alloc
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
    liftIO $ c_fromResourceStatistics s
      tsP
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
      memRSSP
      memRSSSP
      memLBP
      memLBSP
      memFBP
      memFBSP
      memABP
      memABSP
      memMBP
      memMBSP
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
    (CDouble ts) <- peek tsP
    cpuUTS <- toDouble <$> peekMaybePrim cpuUTSP cpuUTSSP
    cpuSTS <- toDouble <$> peekMaybePrim cpuSTSP cpuSTSSP
    (CDouble l) <- peek cpuLP
    cpuPs <- toWord32 <$> peekMaybePrim cpuPsP cpuPsSP
    cpuT <- toWord32 <$> peekMaybePrim cpuTP cpuTSP
    cpuTTS <- toDouble <$> peekMaybePrim cpuTTSP cpuTTSSP
    memRSS <- toWord64 <$> peekMaybePrim memRSSP memRSSSP
    memLB <- toWord64 <$> peekMaybePrim memLBP memLBSP
    memFB <- toWord64 <$> peekMaybePrim memFBP memFBSP
    memAB <- toWord64 <$> peekMaybePrim memABP memABSP
    memMB <- toWord64 <$> peekMaybePrim memMBP memMBSP
    perfStats <- peekMaybeCPP perfP
    netRXP <- toWord64 <$> peekMaybePrim netRXPP netRXPSP
    netRXB <- toWord64 <$> peekMaybePrim netRXBP netRXBSP
    netRXE <- toWord64 <$> peekMaybePrim netRXEP netRXESP
    netRXD <- toWord64 <$> peekMaybePrim netRXDP netRXDSP
    netTXP <- toWord64 <$> peekMaybePrim netTXPP netTXPSP
    netTXB <- toWord64 <$> peekMaybePrim netTXBP netTXBSP
    netTXE <- toWord64 <$> peekMaybePrim netTXEP netTXESP
    netTXD <- toWord64 <$> peekMaybePrim netTXDP netTXDSP
    return $ ResourceStatistics ts cpuUTS cpuSTS l cpuPs cpuT cpuTTS memRSS memLB memFB memAB memMB perfStats netRXP netRXB netRXE netRXD netTXP netTXB netTXE netTXD
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
