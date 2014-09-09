module System.Mesos.Raw.PerformanceStatistics where
import           System.Mesos.Internal

type PerformanceStatisticsPtr = Ptr PerformanceStatistics

foreign import ccall unsafe "ext/types.h toPerfStatistics" c_toPerfStatistics
  :: CDouble -- ^ timestamp
  -> CDouble -- ^ duration
  -> (Ptr CULong) -- ^ cycles
  -> (Ptr CULong) -- ^ stalledCyclesFrontend
  -> (Ptr CULong) -- ^ stalledCyclesBackend
  -> (Ptr CULong) -- ^ instructions
  -> (Ptr CULong) -- ^ cacheReferences
  -> (Ptr CULong) -- ^ cacheMisses
  -> (Ptr CULong) -- ^ branches
  -> (Ptr CULong) -- ^ branchMisses
  -> (Ptr CULong) -- ^ busCycles
  -> (Ptr CULong) -- ^ refCycles
  -> (Ptr CDouble) -- ^ cpuClock
  -> (Ptr CDouble) -- ^ taskClock
  -> (Ptr CULong) -- ^ pageFaults
  -> (Ptr CULong) -- ^ minorFaults
  -> (Ptr CULong) -- ^ majorFaults
  -> (Ptr CULong) -- ^ contextSwitches
  -> (Ptr CULong) -- ^ cpuMigrations
  -> (Ptr CULong) -- ^ alignmentFaults
  -> (Ptr CULong) -- ^ emulationFaults
  -> (Ptr CULong) -- ^ l1DcacheLoads
  -> (Ptr CULong) -- ^ l1DcacheLoadMisses
  -> (Ptr CULong) -- ^ l1DcacheStores
  -> (Ptr CULong) -- ^ l1DcacheStoreMisses
  -> (Ptr CULong) -- ^ l1DcachePrefetches
  -> (Ptr CULong) -- ^ l1DcachePrefetchMisses
  -> (Ptr CULong) -- ^ l1IcacheLoads
  -> (Ptr CULong) -- ^ l1IcacheLoadMisses
  -> (Ptr CULong) -- ^ l1IcachePrefetches
  -> (Ptr CULong) -- ^ l1IcachePrefetchMisses
  -> (Ptr CULong) -- ^ llcLoads
  -> (Ptr CULong) -- ^ llcLoadMisses
  -> (Ptr CULong) -- ^ llcStores
  -> (Ptr CULong) -- ^ llcStoreMisses
  -> (Ptr CULong) -- ^ llcPrefetches
  -> (Ptr CULong) -- ^ llcPrefetchMisses
  -> (Ptr CULong) -- ^ dtlbLoads
  -> (Ptr CULong) -- ^ dtlbLoadMisses
  -> (Ptr CULong) -- ^ dtlbStores
  -> (Ptr CULong) -- ^ dtlbStoreMisses
  -> (Ptr CULong) -- ^ dtlbPrefetches
  -> (Ptr CULong) -- ^ dtlbPrefetchMisses
  -> (Ptr CULong) -- ^ itlbLoads
  -> (Ptr CULong) -- ^ itlbLoadMisses
  -> (Ptr CULong) -- ^ branchLoads
  -> (Ptr CULong) -- ^ branchLoadMisses
  -> (Ptr CULong) -- ^ nodeLoads
  -> (Ptr CULong) -- ^ nodeLoadMisses
  -> (Ptr CULong) -- ^ nodeStores
  -> (Ptr CULong) -- ^ nodeStoreMisses
  -> (Ptr CULong) -- ^ nodePrefetches
  -> (Ptr CULong) -- ^ nodePrefetchMisses
  -> IO PerformanceStatisticsPtr

foreign import ccall unsafe "ext/types.h fromPerfStatistics" c_fromPerfStatistics
  :: PerformanceStatisticsPtr
  -> (Ptr CDouble) -- ^ timestamp
  -> (Ptr CDouble) -- ^ duration
  -> (Ptr CULong) -- ^ cycles
  -> (Ptr CBool) -- ^ cyclesSet
  -> (Ptr CULong) -- ^ stalledCyclesFrontend
  -> (Ptr CBool) -- ^ stalledCyclesFrontendSet
  -> (Ptr CULong) -- ^ stalledCyclesBackend
  -> (Ptr CBool) -- ^ stalledCyclesBackendSet
  -> (Ptr CULong) -- ^ instructions
  -> (Ptr CBool) -- ^ instructionsSet
  -> (Ptr CULong) -- ^ cacheReferences
  -> (Ptr CBool) -- ^ cacheReferencesSet
  -> (Ptr CULong) -- ^ cacheMisses
  -> (Ptr CBool) -- ^ cacheMissesSet
  -> (Ptr CULong) -- ^ branches
  -> (Ptr CBool) -- ^ branchesSet
  -> (Ptr CULong) -- ^ branchMisses
  -> (Ptr CBool) -- ^ branchMissesSet
  -> (Ptr CULong) -- ^ busCycles
  -> (Ptr CBool) -- ^ busCyclesSet
  -> (Ptr CULong) -- ^ refCycles
  -> (Ptr CBool) -- ^ refCyclesSet
  -> (Ptr CDouble) -- ^ cpuClock
  -> (Ptr CBool) -- ^ cpuClockSet
  -> (Ptr CDouble) -- ^ taskClock
  -> (Ptr CBool) -- ^ taskClockSet
  -> (Ptr CULong) -- ^ pageFaults
  -> (Ptr CBool) -- ^ pageFaultsSet
  -> (Ptr CULong) -- ^ minorFaults
  -> (Ptr CBool) -- ^ minorFaultsSet
  -> (Ptr CULong) -- ^ majorFaults
  -> (Ptr CBool) -- ^ majorFaultsSet
  -> (Ptr CULong) -- ^ contextSwitches
  -> (Ptr CBool) -- ^ contextSwitchesSet
  -> (Ptr CULong) -- ^ cpuMigrations
  -> (Ptr CBool) -- ^ cpuMigrationsSet
  -> (Ptr CULong) -- ^ alignmentFaults
  -> (Ptr CBool) -- ^ alignmentFaultsSet
  -> (Ptr CULong) -- ^ emulationFaults
  -> (Ptr CBool) -- ^ emulationFaultsSet
  -> (Ptr CULong) -- ^ l1DcacheLoads
  -> (Ptr CBool) -- ^ l1DcacheLoadsSet
  -> (Ptr CULong) -- ^ l1DcacheLoadMisses
  -> (Ptr CBool) -- ^ l1DcacheLoadMissesSet
  -> (Ptr CULong) -- ^ l1DcacheStores
  -> (Ptr CBool) -- ^ l1DcacheStoresSet
  -> (Ptr CULong) -- ^ l1DcacheStoreMisses
  -> (Ptr CBool) -- ^ l1DcacheStoreMissesSet
  -> (Ptr CULong) -- ^ l1DcachePrefetches
  -> (Ptr CBool) -- ^ l1DcachePrefetchesSet
  -> (Ptr CULong) -- ^ l1DcachePrefetchMisses
  -> (Ptr CBool) -- ^ l1DcachePrefetchMissesSet
  -> (Ptr CULong) -- ^ l1IcacheLoads
  -> (Ptr CBool) -- ^ l1IcacheLoadsSet
  -> (Ptr CULong) -- ^ l1IcacheLoadMisses
  -> (Ptr CBool) -- ^ l1IcacheLoadMissesSet
  -> (Ptr CULong) -- ^ l1IcachePrefetches
  -> (Ptr CBool) -- ^ l1IcachePrefetchesSet
  -> (Ptr CULong) -- ^ l1IcachePrefetchMisses
  -> (Ptr CBool) -- ^ l1IcachePrefetchMissesSet
  -> (Ptr CULong) -- ^ llcLoads
  -> (Ptr CBool) -- ^ llcLoadsSet
  -> (Ptr CULong) -- ^ llcLoadMisses
  -> (Ptr CBool) -- ^ llcLoadMissesSet
  -> (Ptr CULong) -- ^ llcStores
  -> (Ptr CBool) -- ^ llcStoresSet
  -> (Ptr CULong) -- ^ llcStoreMisses
  -> (Ptr CBool) -- ^ llcStoreMissesSet
  -> (Ptr CULong) -- ^ llcPrefetches
  -> (Ptr CBool) -- ^ llcPrefetchesSet
  -> (Ptr CULong) -- ^ llcPrefetchMisses
  -> (Ptr CBool) -- ^ llcPrefetchMissesSet
  -> (Ptr CULong) -- ^ dtlbLoads
  -> (Ptr CBool) -- ^ dtlbLoadsSet
  -> (Ptr CULong) -- ^ dtlbLoadMisses
  -> (Ptr CBool) -- ^ dtlbLoadMissesSet
  -> (Ptr CULong) -- ^ dtlbStores
  -> (Ptr CBool) -- ^ dtlbStoresSet
  -> (Ptr CULong) -- ^ dtlbStoreMisses
  -> (Ptr CBool) -- ^ dtlbStoreMissesSet
  -> (Ptr CULong) -- ^ dtlbPrefetches
  -> (Ptr CBool) -- ^ dtlbPrefetchesSet
  -> (Ptr CULong) -- ^ dtlbPrefetchMisses
  -> (Ptr CBool) -- ^ dtlbPrefetchMissesSet
  -> (Ptr CULong) -- ^ itlbLoads
  -> (Ptr CBool) -- ^ itlbLoadsSet
  -> (Ptr CULong) -- ^ itlbLoadMisses
  -> (Ptr CBool) -- ^ itlbLoadMissesSet
  -> (Ptr CULong) -- ^ branchLoads
  -> (Ptr CBool) -- ^ branchLoadsSet
  -> (Ptr CULong) -- ^ branchLoadMisses
  -> (Ptr CBool) -- ^ branchLoadMissesSet
  -> (Ptr CULong) -- ^ nodeLoads
  -> (Ptr CBool) -- ^ nodeLoadsSet
  -> (Ptr CULong) -- ^ nodeLoadMisses
  -> (Ptr CBool) -- ^ nodeLoadMissesSet
  -> (Ptr CULong) -- ^ nodeStores
  -> (Ptr CBool) -- ^ nodeStoresSet
  -> (Ptr CULong) -- ^ nodeStoreMisses
  -> (Ptr CBool) -- ^ nodeStoreMissesSet
  -> (Ptr CULong) -- ^ nodePrefetches
  -> (Ptr CBool) -- ^ nodePrefetchesSet
  -> (Ptr CULong) -- ^ nodePrefetchMisses
  -> (Ptr CBool) -- ^ nodePrefetchMissesSet
  -> IO ()

foreign import ccall unsafe "ext/types.h destroyPerfStatistics" c_destroyPerfStatistics
  :: PerformanceStatisticsPtr -> IO ()

instance CPPValue PerformanceStatistics where
  marshal x = do
    cyclesP <- allocMaybe $ fmap CULong $ performanceStatisticsCycles x
    stalledCyclesFrontendP <- allocMaybe $ fmap CULong $ performanceStatisticsStalledCyclesFrontend x
    stalledCyclesBackendP <- allocMaybe $ fmap CULong $ performanceStatisticsStalledCyclesBackend x
    instructionsP <- allocMaybe $ fmap CULong $ performanceStatisticsInstructions x
    cacheReferencesP <- allocMaybe $ fmap CULong $ performanceStatisticsCacheReferences x
    cacheMissesP <- allocMaybe $ fmap CULong $ performanceStatisticsCacheMisses x
    branchesP <- allocMaybe $ fmap CULong $ performanceStatisticsBranches x
    branchMissesP <- allocMaybe $ fmap CULong $ performanceStatisticsBranchMisses x
    busCyclesP <- allocMaybe $ fmap CULong $ performanceStatisticsBusCycles x
    refCyclesP <- allocMaybe $ fmap CULong $ performanceStatisticsRefCycles x
    cpuClockP <- allocMaybe $ fmap CDouble $ performanceStatisticsCpuClock x
    taskClockP <- allocMaybe $ fmap CDouble $ performanceStatisticsTaskClock x
    pageFaultsP <- allocMaybe $ fmap CULong $ performanceStatisticsPageFaults x
    minorFaultsP <- allocMaybe $ fmap CULong $ performanceStatisticsMinorFaults x
    majorFaultsP <- allocMaybe $ fmap CULong $ performanceStatisticsMajorFaults x
    contextSwitchesP <- allocMaybe $ fmap CULong $ performanceStatisticsContextSwitches x
    cpuMigrationsP <- allocMaybe $ fmap CULong $ performanceStatisticsCpuMigrations x
    alignmentFaultsP <- allocMaybe $ fmap CULong $ performanceStatisticsAlignmentFaults x
    emulationFaultsP <- allocMaybe $ fmap CULong $ performanceStatisticsEmulationFaults x
    l1DcacheLoadsP <- allocMaybe $ fmap CULong $ performanceStatisticsL1DcacheLoads x
    l1DcacheLoadMissesP <- allocMaybe $ fmap CULong $ performanceStatisticsL1DcacheLoadMisses x
    l1DcacheStoresP <- allocMaybe $ fmap CULong $ performanceStatisticsL1DcacheStores x
    l1DcacheStoreMissesP <- allocMaybe $ fmap CULong $ performanceStatisticsL1DcacheStoreMisses x
    l1DcachePrefetchesP <- allocMaybe $ fmap CULong $ performanceStatisticsL1DcachePrefetches x
    l1DcachePrefetchMissesP <- allocMaybe $ fmap CULong $ performanceStatisticsL1DcachePrefetchMisses x
    l1IcacheLoadsP <- allocMaybe $ fmap CULong $ performanceStatisticsL1IcacheLoads x
    l1IcacheLoadMissesP <- allocMaybe $ fmap CULong $ performanceStatisticsL1IcacheLoadMisses x
    l1IcachePrefetchesP <- allocMaybe $ fmap CULong $ performanceStatisticsL1IcachePrefetches x
    l1IcachePrefetchMissesP <- allocMaybe $ fmap CULong $ performanceStatisticsL1IcachePrefetchMisses x
    llcLoadsP <- allocMaybe $ fmap CULong $ performanceStatisticsLlcLoads x
    llcLoadMissesP <- allocMaybe $ fmap CULong $ performanceStatisticsLlcLoadMisses x
    llcStoresP <- allocMaybe $ fmap CULong $ performanceStatisticsLlcStores x
    llcStoreMissesP <- allocMaybe $ fmap CULong $ performanceStatisticsLlcStoreMisses x
    llcPrefetchesP <- allocMaybe $ fmap CULong $ performanceStatisticsLlcPrefetches x
    llcPrefetchMissesP <- allocMaybe $ fmap CULong $ performanceStatisticsLlcPrefetchMisses x
    dtlbLoadsP <- allocMaybe $ fmap CULong $ performanceStatisticsDtlbLoads x
    dtlbLoadMissesP <- allocMaybe $ fmap CULong $ performanceStatisticsDtlbLoadMisses x
    dtlbStoresP <- allocMaybe $ fmap CULong $ performanceStatisticsDtlbStores x
    dtlbStoreMissesP <- allocMaybe $ fmap CULong $ performanceStatisticsDtlbStoreMisses x
    dtlbPrefetchesP <- allocMaybe $ fmap CULong $ performanceStatisticsDtlbPrefetches x
    dtlbPrefetchMissesP <- allocMaybe $ fmap CULong $ performanceStatisticsDtlbPrefetchMisses x
    itlbLoadsP <- allocMaybe $ fmap CULong $ performanceStatisticsItlbLoads x
    itlbLoadMissesP <- allocMaybe $ fmap CULong $ performanceStatisticsItlbLoadMisses x
    branchLoadsP <- allocMaybe $ fmap CULong $ performanceStatisticsBranchLoads x
    branchLoadMissesP <- allocMaybe $ fmap CULong $ performanceStatisticsBranchLoadMisses x
    nodeLoadsP <- allocMaybe $ fmap CULong $ performanceStatisticsNodeLoads x
    nodeLoadMissesP <- allocMaybe $ fmap CULong $ performanceStatisticsNodeLoadMisses x
    nodeStoresP <- allocMaybe $ fmap CULong $ performanceStatisticsNodeStores x
    nodeStoreMissesP <- allocMaybe $ fmap CULong $ performanceStatisticsNodeStoreMisses x
    nodePrefetchesP <- allocMaybe $ fmap CULong $ performanceStatisticsNodePrefetches x
    nodePrefetchMissesP <- allocMaybe $ fmap CULong $ performanceStatisticsNodePrefetchMisses x
    liftIO $ c_toPerfStatistics
      (CDouble $ performanceStatisticsTimestamp x)
      (CDouble $ performanceStatisticsDuration x)
      cyclesP
      stalledCyclesFrontendP
      stalledCyclesBackendP
      instructionsP
      cacheReferencesP
      cacheMissesP
      branchesP
      branchMissesP
      busCyclesP
      refCyclesP
      cpuClockP
      taskClockP
      pageFaultsP
      minorFaultsP
      majorFaultsP
      contextSwitchesP
      cpuMigrationsP
      alignmentFaultsP
      emulationFaultsP
      l1DcacheLoadsP
      l1DcacheLoadMissesP
      l1DcacheStoresP
      l1DcacheStoreMissesP
      l1DcachePrefetchesP
      l1DcachePrefetchMissesP
      l1IcacheLoadsP
      l1IcacheLoadMissesP
      l1IcachePrefetchesP
      l1IcachePrefetchMissesP
      llcLoadsP
      llcLoadMissesP
      llcStoresP
      llcStoreMissesP
      llcPrefetchesP
      llcPrefetchMissesP
      dtlbLoadsP
      dtlbLoadMissesP
      dtlbStoresP
      dtlbStoreMissesP
      dtlbPrefetchesP
      dtlbPrefetchMissesP
      itlbLoadsP
      itlbLoadMissesP
      branchLoadsP
      branchLoadMissesP
      nodeLoadsP
      nodeLoadMissesP
      nodeStoresP
      nodeStoreMissesP
      nodePrefetchesP
      nodePrefetchMissesP

  unmarshal p = do
    timestampP <- alloc
    durationP <- alloc
    cyclesP <- alloc
    cyclesSP <- alloc
    poke cyclesSP 0
    stalledCyclesFrontendP <- alloc
    stalledCyclesFrontendSP <- alloc
    poke stalledCyclesFrontendSP 0
    stalledCyclesBackendP <- alloc
    stalledCyclesBackendSP <- alloc
    poke stalledCyclesBackendSP 0
    instructionsP <- alloc
    instructionsSP <- alloc
    poke instructionsSP 0
    cacheReferencesP <- alloc
    cacheReferencesSP <- alloc
    poke cacheReferencesSP 0
    cacheMissesP <- alloc
    cacheMissesSP <- alloc
    poke cacheMissesSP 0
    branchesP <- alloc
    branchesSP <- alloc
    poke branchesSP 0
    branchMissesP <- alloc
    branchMissesSP <- alloc
    poke branchMissesSP 0
    busCyclesP <- alloc
    busCyclesSP <- alloc
    poke busCyclesSP 0
    refCyclesP <- alloc
    refCyclesSP <- alloc
    poke refCyclesSP 0
    cpuClockP <- alloc
    cpuClockSP <- alloc
    poke cpuClockSP 0
    taskClockP <- alloc
    taskClockSP <- alloc
    poke taskClockSP 0
    pageFaultsP <- alloc
    pageFaultsSP <- alloc
    poke pageFaultsSP 0
    minorFaultsP <- alloc
    minorFaultsSP <- alloc
    poke minorFaultsSP 0
    majorFaultsP <- alloc
    majorFaultsSP <- alloc
    poke majorFaultsSP 0
    contextSwitchesP <- alloc
    contextSwitchesSP <- alloc
    poke contextSwitchesSP 0
    cpuMigrationsP <- alloc
    cpuMigrationsSP <- alloc
    poke cpuMigrationsSP 0
    alignmentFaultsP <- alloc
    alignmentFaultsSP <- alloc
    poke alignmentFaultsSP 0
    emulationFaultsP <- alloc
    emulationFaultsSP <- alloc
    poke emulationFaultsSP 0
    l1DcacheLoadsP <- alloc
    l1DcacheLoadsSP <- alloc
    poke l1DcacheLoadsSP 0
    l1DcacheLoadMissesP <- alloc
    l1DcacheLoadMissesSP <- alloc
    poke l1DcacheLoadMissesSP 0
    l1DcacheStoresP <- alloc
    l1DcacheStoresSP <- alloc
    poke l1DcacheStoresSP 0
    l1DcacheStoreMissesP <- alloc
    l1DcacheStoreMissesSP <- alloc
    poke l1DcacheStoreMissesSP 0
    l1DcachePrefetchesP <- alloc
    l1DcachePrefetchesSP <- alloc
    poke l1DcachePrefetchesSP 0
    l1DcachePrefetchMissesP <- alloc
    l1DcachePrefetchMissesSP <- alloc
    poke l1DcachePrefetchMissesSP 0
    l1IcacheLoadsP <- alloc
    l1IcacheLoadsSP <- alloc
    poke l1IcacheLoadsSP 0
    l1IcacheLoadMissesP <- alloc
    l1IcacheLoadMissesSP <- alloc
    poke l1IcacheLoadMissesSP 0
    l1IcachePrefetchesP <- alloc
    l1IcachePrefetchesSP <- alloc
    poke l1IcachePrefetchesSP 0
    l1IcachePrefetchMissesP <- alloc
    l1IcachePrefetchMissesSP <- alloc
    poke l1IcachePrefetchMissesSP 0
    llcLoadsP <- alloc
    llcLoadsSP <- alloc
    poke llcLoadsSP 0
    llcLoadMissesP <- alloc
    llcLoadMissesSP <- alloc
    poke llcLoadMissesSP 0
    llcStoresP <- alloc
    llcStoresSP <- alloc
    poke llcStoresSP 0
    llcStoreMissesP <- alloc
    llcStoreMissesSP <- alloc
    poke llcStoreMissesSP 0
    llcPrefetchesP <- alloc
    llcPrefetchesSP <- alloc
    poke llcPrefetchesSP 0
    llcPrefetchMissesP <- alloc
    llcPrefetchMissesSP <- alloc
    poke llcPrefetchMissesSP 0
    dtlbLoadsP <- alloc
    dtlbLoadsSP <- alloc
    poke dtlbLoadsSP 0
    dtlbLoadMissesP <- alloc
    dtlbLoadMissesSP <- alloc
    poke dtlbLoadMissesSP 0
    dtlbStoresP <- alloc
    dtlbStoresSP <- alloc
    poke dtlbStoresSP 0
    dtlbStoreMissesP <- alloc
    dtlbStoreMissesSP <- alloc
    poke dtlbStoreMissesSP 0
    dtlbPrefetchesP <- alloc
    dtlbPrefetchesSP <- alloc
    poke dtlbPrefetchesSP 0
    dtlbPrefetchMissesP <- alloc
    dtlbPrefetchMissesSP <- alloc
    poke dtlbPrefetchMissesSP 0
    itlbLoadsP <- alloc
    itlbLoadsSP <- alloc
    poke itlbLoadsSP 0
    itlbLoadMissesP <- alloc
    itlbLoadMissesSP <- alloc
    poke itlbLoadMissesSP 0
    branchLoadsP <- alloc
    branchLoadsSP <- alloc
    poke branchLoadsSP 0
    branchLoadMissesP <- alloc
    branchLoadMissesSP <- alloc
    poke branchLoadMissesSP 0
    nodeLoadsP <- alloc
    nodeLoadsSP <- alloc
    poke nodeLoadsSP 0
    nodeLoadMissesP <- alloc
    nodeLoadMissesSP <- alloc
    poke nodeLoadMissesSP 0
    nodeStoresP <- alloc
    nodeStoresSP <- alloc
    poke nodeStoresSP 0
    nodeStoreMissesP <- alloc
    nodeStoreMissesSP <- alloc
    poke nodeStoreMissesSP 0
    nodePrefetchesP <- alloc
    nodePrefetchesSP <- alloc
    poke nodePrefetchesSP 0
    nodePrefetchMissesP <- alloc
    nodePrefetchMissesSP <- alloc
    poke nodePrefetchMissesSP 0
    liftIO $ c_fromPerfStatistics p
      timestampP
      durationP
      cyclesP
      cyclesSP
      stalledCyclesFrontendP
      stalledCyclesFrontendSP
      stalledCyclesBackendP
      stalledCyclesBackendSP
      instructionsP
      instructionsSP
      cacheReferencesP
      cacheReferencesSP
      cacheMissesP
      cacheMissesSP
      branchesP
      branchesSP
      branchMissesP
      branchMissesSP
      busCyclesP
      busCyclesSP
      refCyclesP
      refCyclesSP
      cpuClockP
      cpuClockSP
      taskClockP
      taskClockSP
      pageFaultsP
      pageFaultsSP
      minorFaultsP
      minorFaultsSP
      majorFaultsP
      majorFaultsSP
      contextSwitchesP
      contextSwitchesSP
      cpuMigrationsP
      cpuMigrationsSP
      alignmentFaultsP
      alignmentFaultsSP
      emulationFaultsP
      emulationFaultsSP
      l1DcacheLoadsP
      l1DcacheLoadsSP
      l1DcacheLoadMissesP
      l1DcacheLoadMissesSP
      l1DcacheStoresP
      l1DcacheStoresSP
      l1DcacheStoreMissesP
      l1DcacheStoreMissesSP
      l1DcachePrefetchesP
      l1DcachePrefetchesSP
      l1DcachePrefetchMissesP
      l1DcachePrefetchMissesSP
      l1IcacheLoadsP
      l1IcacheLoadsSP
      l1IcacheLoadMissesP
      l1IcacheLoadMissesSP
      l1IcachePrefetchesP
      l1IcachePrefetchesSP
      l1IcachePrefetchMissesP
      l1IcachePrefetchMissesSP
      llcLoadsP
      llcLoadsSP
      llcLoadMissesP
      llcLoadMissesSP
      llcStoresP
      llcStoresSP
      llcStoreMissesP
      llcStoreMissesSP
      llcPrefetchesP
      llcPrefetchesSP
      llcPrefetchMissesP
      llcPrefetchMissesSP
      dtlbLoadsP
      dtlbLoadsSP
      dtlbLoadMissesP
      dtlbLoadMissesSP
      dtlbStoresP
      dtlbStoresSP
      dtlbStoreMissesP
      dtlbStoreMissesSP
      dtlbPrefetchesP
      dtlbPrefetchesSP
      dtlbPrefetchMissesP
      dtlbPrefetchMissesSP
      itlbLoadsP
      itlbLoadsSP
      itlbLoadMissesP
      itlbLoadMissesSP
      branchLoadsP
      branchLoadsSP
      branchLoadMissesP
      branchLoadMissesSP
      nodeLoadsP
      nodeLoadsSP
      nodeLoadMissesP
      nodeLoadMissesSP
      nodeStoresP
      nodeStoresSP
      nodeStoreMissesP
      nodeStoreMissesSP
      nodePrefetchesP
      nodePrefetchesSP
      nodePrefetchMissesP
      nodePrefetchMissesSP
    (CDouble timestamp) <- peek timestampP
    (CDouble duration) <- peek durationP
    cycles <- toWord64 <$> peekMaybePrim cyclesP cyclesSP
    stalledCyclesFrontend <- toWord64 <$> peekMaybePrim stalledCyclesFrontendP stalledCyclesFrontendSP
    stalledCyclesBackend <- toWord64 <$> peekMaybePrim stalledCyclesBackendP stalledCyclesBackendSP
    instructions <- toWord64 <$> peekMaybePrim instructionsP instructionsSP
    cacheReferences <- toWord64 <$> peekMaybePrim cacheReferencesP cacheReferencesSP
    cacheMisses <- toWord64 <$> peekMaybePrim cacheMissesP cacheMissesSP
    branches <- toWord64 <$> peekMaybePrim branchesP branchesSP
    branchMisses <- toWord64 <$> peekMaybePrim branchMissesP branchMissesSP
    busCycles <- toWord64 <$> peekMaybePrim busCyclesP busCyclesSP
    refCycles <- toWord64 <$> peekMaybePrim refCyclesP refCyclesSP
    cpuClock <- toDouble <$> peekMaybePrim cpuClockP cpuClockSP
    taskClock <- toDouble <$> peekMaybePrim taskClockP taskClockSP
    pageFaults <- toWord64 <$> peekMaybePrim pageFaultsP pageFaultsSP
    minorFaults <- toWord64 <$> peekMaybePrim minorFaultsP minorFaultsSP
    majorFaults <- toWord64 <$> peekMaybePrim majorFaultsP majorFaultsSP
    contextSwitches <- toWord64 <$> peekMaybePrim contextSwitchesP contextSwitchesSP
    cpuMigrations <- toWord64 <$> peekMaybePrim cpuMigrationsP cpuMigrationsSP
    alignmentFaults <- toWord64 <$> peekMaybePrim alignmentFaultsP alignmentFaultsSP
    emulationFaults <- toWord64 <$> peekMaybePrim emulationFaultsP emulationFaultsSP
    l1DcacheLoads <- toWord64 <$> peekMaybePrim l1DcacheLoadsP l1DcacheLoadsSP
    l1DcacheLoadMisses <- toWord64 <$> peekMaybePrim l1DcacheLoadMissesP l1DcacheLoadMissesSP
    l1DcacheStores <- toWord64 <$> peekMaybePrim l1DcacheStoresP l1DcacheStoresSP
    l1DcacheStoreMisses <- toWord64 <$> peekMaybePrim l1DcacheStoreMissesP l1DcacheStoreMissesSP
    l1DcachePrefetches <- toWord64 <$> peekMaybePrim l1DcachePrefetchesP l1DcachePrefetchesSP
    l1DcachePrefetchMisses <- toWord64 <$> peekMaybePrim l1DcachePrefetchMissesP l1DcachePrefetchMissesSP
    l1IcacheLoads <- toWord64 <$> peekMaybePrim l1IcacheLoadsP l1IcacheLoadsSP
    l1IcacheLoadMisses <- toWord64 <$> peekMaybePrim l1IcacheLoadMissesP l1IcacheLoadMissesSP
    l1IcachePrefetches <- toWord64 <$> peekMaybePrim l1IcachePrefetchesP l1IcachePrefetchesSP
    l1IcachePrefetchMisses <- toWord64 <$> peekMaybePrim l1IcachePrefetchMissesP l1IcachePrefetchMissesSP
    llcLoads <- toWord64 <$> peekMaybePrim llcLoadsP llcLoadsSP
    llcLoadMisses <- toWord64 <$> peekMaybePrim llcLoadMissesP llcLoadMissesSP
    llcStores <- toWord64 <$> peekMaybePrim llcStoresP llcStoresSP
    llcStoreMisses <- toWord64 <$> peekMaybePrim llcStoreMissesP llcStoreMissesSP
    llcPrefetches <- toWord64 <$> peekMaybePrim llcPrefetchesP llcPrefetchesSP
    llcPrefetchMisses <- toWord64 <$> peekMaybePrim llcPrefetchMissesP llcPrefetchMissesSP
    dtlbLoads <- toWord64 <$> peekMaybePrim dtlbLoadsP dtlbLoadsSP
    dtlbLoadMisses <- toWord64 <$> peekMaybePrim dtlbLoadMissesP dtlbLoadMissesSP
    dtlbStores <- toWord64 <$> peekMaybePrim dtlbStoresP dtlbStoresSP
    dtlbStoreMisses <- toWord64 <$> peekMaybePrim dtlbStoreMissesP dtlbStoreMissesSP
    dtlbPrefetches <- toWord64 <$> peekMaybePrim dtlbPrefetchesP dtlbPrefetchesSP
    dtlbPrefetchMisses <- toWord64 <$> peekMaybePrim dtlbPrefetchMissesP dtlbPrefetchMissesSP
    itlbLoads <- toWord64 <$> peekMaybePrim itlbLoadsP itlbLoadsSP
    itlbLoadMisses <- toWord64 <$> peekMaybePrim itlbLoadMissesP itlbLoadMissesSP
    branchLoads <- toWord64 <$> peekMaybePrim branchLoadsP branchLoadsSP
    branchLoadMisses <- toWord64 <$> peekMaybePrim branchLoadMissesP branchLoadMissesSP
    nodeLoads <- toWord64 <$> peekMaybePrim nodeLoadsP nodeLoadsSP
    nodeLoadMisses <- toWord64 <$> peekMaybePrim nodeLoadMissesP nodeLoadMissesSP
    nodeStores <- toWord64 <$> peekMaybePrim nodeStoresP nodeStoresSP
    nodeStoreMisses <- toWord64 <$> peekMaybePrim nodeStoreMissesP nodeStoreMissesSP
    nodePrefetches <- toWord64 <$> peekMaybePrim nodePrefetchesP nodePrefetchesSP
    nodePrefetchMisses <- toWord64 <$> peekMaybePrim nodePrefetchMissesP nodePrefetchMissesSP

    return $ PerformanceStatistics
      timestamp
      duration
      cycles
      stalledCyclesFrontend
      stalledCyclesBackend
      instructions
      cacheReferences
      cacheMisses
      branches
      branchMisses
      busCycles
      refCycles
      cpuClock
      taskClock
      pageFaults
      minorFaults
      majorFaults
      contextSwitches
      cpuMigrations
      alignmentFaults
      emulationFaults
      l1DcacheLoads
      l1DcacheLoadMisses
      l1DcacheStores
      l1DcacheStoreMisses
      l1DcachePrefetches
      l1DcachePrefetchMisses
      l1IcacheLoads
      l1IcacheLoadMisses
      l1IcachePrefetches
      l1IcachePrefetchMisses
      llcLoads
      llcLoadMisses
      llcStores
      llcStoreMisses
      llcPrefetches
      llcPrefetchMisses
      dtlbLoads
      dtlbLoadMisses
      dtlbStores
      dtlbStoreMisses
      dtlbPrefetches
      dtlbPrefetchMisses
      itlbLoads
      itlbLoadMisses
      branchLoads
      branchLoadMisses
      nodeLoads
      nodeLoadMisses
      nodeStores
      nodeStoreMisses
      nodePrefetches
      nodePrefetchMisses

    where
      toDouble mx = case mx of
                      Nothing -> Nothing
                      Just (CDouble x) -> Just x
      toWord64 mx = case mx of
                      Nothing -> Nothing
                      Just (CULong x) -> Just x

  destroy = c_destroyPerfStatistics

