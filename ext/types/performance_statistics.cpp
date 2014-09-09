#include <iostream>
#include "types.h"

using namespace mesos;

PerfStatisticsPtr toPerfStatistics(
	double timestamp,
	double duration,
	unsigned long* cycles,
	unsigned long* stalledCyclesFrontend,
	unsigned long* stalledCyclesBackend,
	unsigned long* instructions,
	unsigned long* cacheReferences,
	unsigned long* cacheMisses,
	unsigned long* branches,
	unsigned long* branchMisses,
	unsigned long* busCycles,
	unsigned long* refCycles,
	double* cpuClock,
	double* taskClock,
	unsigned long* pageFaults,
	unsigned long* minorFaults,
	unsigned long* majorFaults,
	unsigned long* contextSwitches,
	unsigned long* cpuMigrations,
	unsigned long* alignmentFaults,
	unsigned long* emulationFaults,
	unsigned long* l1DcacheLoads,
	unsigned long* l1DcacheLoadMisses,
	unsigned long* l1DcacheStores,
	unsigned long* l1DcacheStoreMisses,
	unsigned long* l1DcachePrefetches,
	unsigned long* l1DcachePrefetchMisses,
	unsigned long* l1IcacheLoads,
	unsigned long* l1IcacheLoadMisses,
	unsigned long* l1IcachePrefetches,
	unsigned long* l1IcachePrefetchMisses,
	unsigned long* llcLoads,
	unsigned long* llcLoadMisses,
	unsigned long* llcStores,
	unsigned long* llcStoreMisses,
	unsigned long* llcPrefetches,
	unsigned long* llcPrefetchMisses,
	unsigned long* dtlbLoads,
	unsigned long* dtlbLoadMisses,
	unsigned long* dtlbStores,
	unsigned long* dtlbStoreMisses,
	unsigned long* dtlbPrefetches,
	unsigned long* dtlbPrefetchMisses,
	unsigned long* itlbLoads,
	unsigned long* itlbLoadMisses,
	unsigned long* branchLoads,
	unsigned long* branchLoadMisses,
	unsigned long* nodeLoads,
	unsigned long* nodeLoadMisses,
	unsigned long* nodeStores,
	unsigned long* nodeStoreMisses,
	unsigned long* nodePrefetches,
	unsigned long* nodePrefetchMisses
	)
{
	PerfStatisticsPtr perf = new PerfStatistics();
    
	perf->set_timestamp(timestamp);
	perf->set_duration(duration);

	if (cycles != NULL)
	{
		perf->set_cycles(*cycles);
	}

	if (stalledCyclesFrontend != NULL)
	{
		perf->set_stalled_cycles_frontend(*stalledCyclesFrontend);
	}

	if (stalledCyclesBackend != NULL)
	{
		perf->set_stalled_cycles_backend(*stalledCyclesBackend);
	}

	if (instructions != NULL)
	{
		perf->set_instructions(*instructions);
	}

	if (cacheReferences != NULL)
	{
		perf->set_cache_references(*cacheReferences);
	}

	if (cacheMisses != NULL)
	{
		perf->set_cache_misses(*cacheMisses);
	}

	if (branches != NULL)
	{
		perf->set_branches(*branches);
	}

	if (branchMisses != NULL)
	{
		perf->set_branch_misses(*branchMisses);
	}

	if (busCycles != NULL)
	{
		perf->set_bus_cycles(*busCycles);
	}

	if (refCycles != NULL)
	{
		perf->set_ref_cycles(*refCycles);
	}

	if (cpuClock != NULL)
	{
		perf->set_cpu_clock(*cpuClock);
	}

	if (taskClock != NULL)
	{
		perf->set_task_clock(*taskClock);
	}

	if (pageFaults != NULL)
	{
		perf->set_page_faults(*pageFaults);
	}

	if (minorFaults != NULL)
	{
		perf->set_minor_faults(*minorFaults);
	}

	if (majorFaults != NULL)
	{
		perf->set_major_faults(*majorFaults);
	}

	if (contextSwitches != NULL)
	{
		perf->set_context_switches(*contextSwitches);
	}

	if (cpuMigrations != NULL)
	{
		perf->set_cpu_migrations(*cpuMigrations);
	}

	if (alignmentFaults != NULL)
	{
		perf->set_alignment_faults(*alignmentFaults);
	}

	if (emulationFaults != NULL)
	{
		perf->set_emulation_faults(*emulationFaults);
	}

	if (l1DcacheLoads != NULL)
	{
		perf->set_l1_dcache_loads(*l1DcacheLoads);
	}

	if (l1DcacheLoadMisses != NULL)
	{
		perf->set_l1_dcache_load_misses(*l1DcacheLoadMisses);
	}

	if (l1DcacheStores != NULL)
	{
		perf->set_l1_dcache_stores(*l1DcacheStores);
	}

	if (l1DcacheStoreMisses != NULL)
	{
		perf->set_l1_dcache_store_misses(*l1DcacheStoreMisses);
	}

	if (l1DcachePrefetches != NULL)
	{
		perf->set_l1_dcache_prefetches(*l1DcachePrefetches);
	}

	if (l1DcachePrefetchMisses != NULL)
	{
		perf->set_l1_dcache_prefetch_misses(*l1DcachePrefetchMisses);
	}

	if (l1IcacheLoads != NULL)
	{
		perf->set_l1_icache_loads(*l1IcacheLoads);
	}

	if (l1IcacheLoadMisses != NULL)
	{
		perf->set_l1_icache_load_misses(*l1IcacheLoadMisses);
	}

	if (l1IcachePrefetches != NULL)
	{
		perf->set_l1_icache_prefetches(*l1IcachePrefetches);
	}

	if (l1IcachePrefetchMisses != NULL)
	{
		perf->set_l1_icache_prefetch_misses(*l1IcachePrefetchMisses);
	}

	if (llcLoads != NULL)
	{
		perf->set_llc_loads(*llcLoads);
	}

	if (llcLoadMisses != NULL)
	{
		perf->set_llc_load_misses(*llcLoadMisses);
	}

	if (llcStores != NULL)
	{
		perf->set_llc_stores(*llcStores);
	}

	if (llcStoreMisses != NULL)
	{
		perf->set_llc_store_misses(*llcStoreMisses);
	}

	if (llcPrefetches != NULL)
	{
		perf->set_llc_prefetches(*llcPrefetches);
	}

	if (llcPrefetchMisses != NULL)
	{
		perf->set_llc_prefetch_misses(*llcPrefetchMisses);
	}

	if (dtlbLoads != NULL)
	{
		perf->set_dtlb_loads(*dtlbLoads);
	}

	if (dtlbLoadMisses != NULL)
	{
		perf->set_dtlb_load_misses(*dtlbLoadMisses);
	}

	if (dtlbStores != NULL)
	{
		perf->set_dtlb_stores(*dtlbStores);
	}

	if (dtlbStoreMisses != NULL)
	{
		perf->set_dtlb_store_misses(*dtlbStoreMisses);
	}

	if (dtlbPrefetches != NULL)
	{
		perf->set_dtlb_prefetches(*dtlbPrefetches);
	}

	if (dtlbPrefetchMisses != NULL)
	{
		perf->set_dtlb_prefetch_misses(*dtlbPrefetchMisses);
	}

	if (itlbLoads != NULL)
	{
		perf->set_itlb_loads(*itlbLoads);
	}

	if (itlbLoadMisses != NULL)
	{
		perf->set_itlb_load_misses(*itlbLoadMisses);
	}

	if (branchLoads != NULL)
	{
		perf->set_branch_loads(*branchLoads);
	}

	if (branchLoadMisses != NULL)
	{
		perf->set_branch_load_misses(*branchLoadMisses);
	}

	if (nodeLoads != NULL)
	{
		perf->set_node_loads(*nodeLoads);
	}

	if (nodeLoadMisses != NULL)
	{
		perf->set_node_load_misses(*nodeLoadMisses);
	}

	if (nodeStores != NULL)
	{
		perf->set_node_stores(*nodeStores);
	}

	if (nodeStoreMisses != NULL)
	{
		perf->set_node_store_misses(*nodeStoreMisses);
	}

	if (nodePrefetches != NULL)
	{
		perf->set_node_prefetches(*nodePrefetches);
	}

	if (nodePrefetchMisses != NULL)
	{
		perf->set_node_prefetch_misses(*nodePrefetchMisses);
	}

	return perf;
}

void fromPerfStatistics(PerfStatisticsPtr perf,
	double* timestamp,
	double* duration,
	unsigned long* cycles,
	bool* cyclesSet,
	unsigned long* stalledCyclesFrontend,
	bool* stalledCyclesFrontendSet,
	unsigned long* stalledCyclesBackend,
	bool* stalledCyclesBackendSet,
	unsigned long* instructions,
	bool* instructionsSet,
	unsigned long* cacheReferences,
	bool* cacheReferencesSet,
	unsigned long* cacheMisses,
	bool* cacheMissesSet,
	unsigned long* branches,
	bool* branchesSet,
	unsigned long* branchMisses,
	bool* branchMissesSet,
	unsigned long* busCycles,
	bool* busCyclesSet,
	unsigned long* refCycles,
	bool* refCyclesSet,
	double* cpuClock,
	bool* cpuClockSet,
	double* taskClock,
	bool* taskClockSet,
	unsigned long* pageFaults,
	bool* pageFaultsSet,
	unsigned long* minorFaults,
	bool* minorFaultsSet,
	unsigned long* majorFaults,
	bool* majorFaultsSet,
	unsigned long* contextSwitches,
	bool* contextSwitchesSet,
	unsigned long* cpuMigrations,
	bool* cpuMigrationsSet,
	unsigned long* alignmentFaults,
	bool* alignmentFaultsSet,
	unsigned long* emulationFaults,
	bool* emulationFaultsSet,
	unsigned long* l1DcacheLoads,
	bool* l1DcacheLoadsSet,
	unsigned long* l1DcacheLoadMisses,
	bool* l1DcacheLoadMissesSet,
	unsigned long* l1DcacheStores,
	bool* l1DcacheStoresSet,
	unsigned long* l1DcacheStoreMisses,
	bool* l1DcacheStoreMissesSet,
	unsigned long* l1DcachePrefetches,
	bool* l1DcachePrefetchesSet,
	unsigned long* l1DcachePrefetchMisses,
	bool* l1DcachePrefetchMissesSet,
	unsigned long* l1IcacheLoads,
	bool* l1IcacheLoadsSet,
	unsigned long* l1IcacheLoadMisses,
	bool* l1IcacheLoadMissesSet,
	unsigned long* l1IcachePrefetches,
	bool* l1IcachePrefetchesSet,
	unsigned long* l1IcachePrefetchMisses,
	bool* l1IcachePrefetchMissesSet,
	unsigned long* llcLoads,
	bool* llcLoadsSet,
	unsigned long* llcLoadMisses,
	bool* llcLoadMissesSet,
	unsigned long* llcStores,
	bool* llcStoresSet,
	unsigned long* llcStoreMisses,
	bool* llcStoreMissesSet,
	unsigned long* llcPrefetches,
	bool* llcPrefetchesSet,
	unsigned long* llcPrefetchMisses,
	bool* llcPrefetchMissesSet,
	unsigned long* dtlbLoads,
	bool* dtlbLoadsSet,
	unsigned long* dtlbLoadMisses,
	bool* dtlbLoadMissesSet,
	unsigned long* dtlbStores,
	bool* dtlbStoresSet,
	unsigned long* dtlbStoreMisses,
	bool* dtlbStoreMissesSet,
	unsigned long* dtlbPrefetches,
	bool* dtlbPrefetchesSet,
	unsigned long* dtlbPrefetchMisses,
	bool* dtlbPrefetchMissesSet,
	unsigned long* itlbLoads,
	bool* itlbLoadsSet,
	unsigned long* itlbLoadMisses,
	bool* itlbLoadMissesSet,
			unsigned long* branchLoads,
			bool* branchLoadsSet,
			unsigned long* branchLoadMisses,
			bool* branchLoadMissesSet,
			unsigned long* nodeLoads,
			bool* nodeLoadsSet,
			unsigned long* nodeLoadMisses,
			bool* nodeLoadMissesSet,
			unsigned long* nodeStores,
			bool* nodeStoresSet,
			unsigned long* nodeStoreMisses,
			bool* nodeStoreMissesSet,
			unsigned long* nodePrefetches,
			bool* nodePrefetchesSet,
			unsigned long* nodePrefetchMisses,
			bool* nodePrefetchMissesSet)
{
	*timestamp = perf->timestamp();
	*duration = perf->duration();

	if (perf->has_cycles())
	{
		*cyclesSet = true;
		*cycles = perf->cycles();
	}

	if (perf->has_stalled_cycles_frontend())
	{
		*stalledCyclesFrontendSet = true;
		*stalledCyclesFrontend = perf->stalled_cycles_frontend();
	}

	if (perf->has_stalled_cycles_backend())
	{
		*stalledCyclesBackendSet = true;
		*stalledCyclesBackend = perf->stalled_cycles_backend();
	}

	if (perf->has_instructions())
	{
		*instructionsSet = true;
		*instructions = perf->instructions();
	}

	if (perf->has_cache_references())
	{
		*cacheReferencesSet = true;
		*cacheReferences = perf->cache_references();
	}

	if (perf->has_cache_misses())
	{
		*cacheMissesSet = true;
		*cacheMisses = perf->cache_misses();
	}

	if (perf->has_branches())
	{
		*branchesSet = true;
		*branches = perf->branches();
	}

	if (perf->has_branch_misses())
	{
		*branchMissesSet = true;
		*branchMisses = perf->branch_misses();
	}

	if (perf->has_bus_cycles())
	{
		*busCyclesSet = true;
		*busCycles = perf->bus_cycles();
	}

	if (perf->has_ref_cycles())
	{
		*refCyclesSet = true;
		*refCycles = perf->ref_cycles();
	}

	if (perf->has_cpu_clock())
	{
		*cpuClockSet = true;
		*cpuClock = perf->cpu_clock();
	}

	if (perf->has_task_clock())
	{
		*taskClockSet = true;
		*taskClock = perf->task_clock();
	}

	if (perf->has_page_faults())
	{
		*pageFaultsSet = true;
		*pageFaults = perf->page_faults();
	}

	if (perf->has_minor_faults())
	{
		*minorFaultsSet = true;
		*minorFaults = perf->minor_faults();
	}

	if (perf->has_major_faults())
	{
		*majorFaultsSet = true;
		*majorFaults = perf->major_faults();
	}

	if (perf->has_context_switches())
	{
		*contextSwitchesSet = true;
		*contextSwitches = perf->context_switches();
	}

	if (perf->has_cpu_migrations())
	{
		*cpuMigrationsSet = true;
		*cpuMigrations = perf->cpu_migrations();
	}

	if (perf->has_alignment_faults())
	{
		*alignmentFaultsSet = true;
		*alignmentFaults = perf->alignment_faults();
	}

	if (perf->has_emulation_faults())
	{
		*emulationFaultsSet = true;
		*emulationFaults = perf->emulation_faults();
	}

	if (perf->has_l1_dcache_loads())
	{
		*l1DcacheLoadsSet = true;
		*l1DcacheLoads = perf->l1_dcache_loads();
	}

	if (perf->has_l1_dcache_load_misses())
	{
		*l1DcacheLoadMissesSet = true;
		*l1DcacheLoadMisses = perf->l1_dcache_load_misses();
	}

	if (perf->has_l1_dcache_stores())
	{
		*l1DcacheStoresSet = true;
		*l1DcacheStores = perf->l1_dcache_stores();
	}

	if (perf->has_l1_dcache_store_misses())
	{
		*l1DcacheStoreMissesSet = true;
		*l1DcacheStoreMisses = perf->l1_dcache_store_misses();
	}

	if (perf->has_l1_dcache_prefetches())
	{
		*l1DcachePrefetchesSet = true;
		*l1DcachePrefetches = perf->l1_dcache_prefetches();
	}

	if (perf->has_l1_dcache_prefetch_misses())
	{
		*l1DcachePrefetchMissesSet = true;
		*l1DcachePrefetchMisses = perf->l1_dcache_prefetch_misses();
	}

	if (perf->has_l1_icache_loads())
	{
		*l1IcacheLoadsSet = true;
		*l1IcacheLoads = perf->l1_icache_loads();
	}

	if (perf->has_l1_icache_load_misses())
	{
		*l1IcacheLoadMissesSet = true;
		*l1IcacheLoadMisses = perf->l1_icache_load_misses();
	}

	if (perf->has_l1_icache_prefetches())
	{
		*l1IcachePrefetchesSet = true;
		*l1IcachePrefetches = perf->l1_icache_prefetches();
	}

	if (perf->has_l1_icache_prefetch_misses())
	{
		*l1IcachePrefetchMissesSet = true;
		*l1IcachePrefetchMisses = perf->l1_icache_prefetch_misses();
	}

	if (perf->has_llc_loads())
	{
		*llcLoadsSet = true;
		*llcLoads = perf->llc_loads();
	}

	if (perf->has_llc_load_misses())
	{
		*llcLoadMissesSet = true;
		*llcLoadMisses = perf->llc_load_misses();
	}

	if (perf->has_llc_stores())
	{
		*llcStoresSet = true;
		*llcStores = perf->llc_stores();
	}

	if (perf->has_llc_store_misses())
	{
		*llcStoreMissesSet = true;
		*llcStoreMisses = perf->llc_store_misses();
	}

	if (perf->has_llc_prefetches())
	{
		*llcPrefetchesSet = true;
		*llcPrefetches = perf->llc_prefetches();
	}

	if (perf->has_llc_prefetch_misses())
	{
		*llcPrefetchMissesSet = true;
		*llcPrefetchMisses = perf->llc_prefetch_misses();
	}

	if (perf->has_dtlb_loads())
	{
		*dtlbLoadsSet = true;
		*dtlbLoads = perf->dtlb_loads();
	}

	if (perf->has_dtlb_load_misses())
	{
		*dtlbLoadMissesSet = true;
		*dtlbLoadMisses = perf->dtlb_load_misses();
	}

	if (perf->has_dtlb_stores())
	{
		*dtlbStoresSet = true;
		*dtlbStores = perf->dtlb_stores();
	}

	if (perf->has_dtlb_store_misses())
	{
		*dtlbStoreMissesSet = true;
		*dtlbStoreMisses = perf->dtlb_store_misses();
	}

	if (perf->has_dtlb_prefetches())
	{
		*dtlbPrefetchesSet = true;
		*dtlbPrefetches = perf->dtlb_prefetches();
	}

	if (perf->has_dtlb_prefetch_misses())
	{
		*dtlbPrefetchMissesSet = true;
		*dtlbPrefetchMisses = perf->dtlb_prefetch_misses();
	}

	if (perf->has_itlb_loads())
	{
		*itlbLoadsSet = true;
		*itlbLoads = perf->itlb_loads();
	}

	if (perf->has_itlb_load_misses())
	{
		*itlbLoadMissesSet = true;
		*itlbLoadMisses = perf->itlb_load_misses();
	}

	if (perf->has_branch_loads())
	{
		*branchLoadsSet = true;
		*branchLoads = perf->branch_loads();
	}

	if (perf->has_branch_load_misses())
	{
		*branchLoadMissesSet = true;
		*branchLoadMisses = perf->branch_load_misses();
	}

	if (perf->has_node_loads())
	{
		*nodeLoadsSet = true;
		*nodeLoads = perf->node_loads();
	}

	if (perf->has_node_load_misses())
	{
		*nodeLoadMissesSet = true;
		*nodeLoadMisses = perf->node_load_misses();
	}

	if (perf->has_node_stores())
	{
		*nodeStoresSet = true;
		*nodeStores = perf->node_stores();
	}

	if (perf->has_node_store_misses())
	{
		*nodeStoreMissesSet = true;
		*nodeStoreMisses = perf->node_store_misses();
	}

	if (perf->has_node_prefetches())
	{
		*nodePrefetchesSet = true;
		*nodePrefetches = perf->node_prefetches();
	}

	if (perf->has_node_prefetch_misses())
	{
		*nodePrefetchMissesSet = true;
		*nodePrefetchMisses = perf->node_prefetch_misses();
	}
}

void destroyPerfStatistics(PerfStatisticsPtr perf)
{
  delete perf;
}
