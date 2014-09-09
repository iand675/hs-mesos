#include <iostream>
#include "types.h"

using namespace mesos;

ResourceStatisticsPtr toResourceStatistics(double timestamp,
					   double* cpusUserTimeSecs,
					   double* cpusSystemTimeSecs,
					   double cpusLimit,
					   unsigned int* cpusPeriods,
					   unsigned int* cpusThrottled,
					   double* cpusThrottledTimeSecs,
					   unsigned long* memoryResidentSetSize,
					   unsigned long* memoryLimitBytes,
					   unsigned long* memoryFileBytes,
					   unsigned long* memoryAnonymousBytes,
					   unsigned long* memoryMappedFileBytes,
					   PerfStatistics* perfStatistics,
					   unsigned long* netRxPackets,
					   unsigned long* netRxBytes,
					   unsigned long* netRxErrors,
					   unsigned long* netRxDropped,
					   unsigned long* netTxPackets,
					   unsigned long* netTxBytes,
					   unsigned long* netTxErrors,
					   unsigned long* netTxDropped
					   )
{
  ResourceStatisticsPtr stats = new ResourceStatistics();
  stats->set_timestamp(timestamp);
  if (cpusUserTimeSecs != NULL)
    stats->set_cpus_user_time_secs(*cpusUserTimeSecs);
  if (cpusSystemTimeSecs != NULL)
    stats->set_cpus_system_time_secs(*cpusSystemTimeSecs);
  stats->set_cpus_limit(cpusLimit);
  if (cpusPeriods != NULL)
    stats->set_cpus_nr_periods(*cpusPeriods);
  if (cpusThrottled != NULL)
    stats->set_cpus_nr_throttled(*cpusThrottled);
  if (cpusThrottledTimeSecs != NULL)
    stats->set_cpus_throttled_time_secs(*cpusThrottledTimeSecs);
  if (memoryResidentSetSize != NULL)
    stats->set_mem_rss_bytes(*memoryResidentSetSize);
  if (memoryLimitBytes != NULL)
    stats->set_mem_limit_bytes(*memoryLimitBytes);
  if (memoryFileBytes != NULL)
    stats->set_mem_file_bytes(*memoryFileBytes);
  if (memoryAnonymousBytes != NULL)
    stats->set_mem_anon_bytes(*memoryAnonymousBytes);
  if (memoryMappedFileBytes != NULL)
    stats->set_mem_mapped_file_bytes(*memoryMappedFileBytes);
  if (perfStatistics != NULL)
    *stats->mutable_perf() = *perfStatistics;

  if (netRxPackets != NULL)
    stats->set_net_rx_packets(*netRxPackets);

  if (netRxBytes != NULL)
    stats->set_net_rx_bytes(*netRxBytes);

  if (netRxErrors != NULL)
    stats->set_net_rx_errors(*netRxErrors);

  if (netRxDropped != NULL)
    stats->set_net_rx_dropped(*netRxDropped);

  if (netTxPackets != NULL)
    stats->set_net_tx_packets(*netTxPackets);

  if (netTxBytes != NULL)
    stats->set_net_tx_bytes(*netTxBytes);

  if (netTxErrors != NULL)
    stats->set_net_tx_errors(*netTxErrors);

  if (netTxDropped != NULL)
    stats->set_net_tx_dropped(*netTxDropped);

  return stats;
}

void fromResourceStatistics(ResourceStatisticsPtr stats,
			    double* timestamp,
			    double* cpusUserTimeSecs,
			    bool* cpusUserTimeSecsSet,
			    double* cpusSystemTimeSecs,
			    bool* cpusSystemTimeSecsSet,
			    double* cpusLimit,
			    unsigned int* cpusPeriods,
			    bool* cpusPeriodsSet,
			    unsigned int* cpusThrottled,
			    bool* cpusThrottledSet,
			    double* cpusThrottledTimeSecs,
			    bool* cpusThrottledTimeSecsSet,
			    unsigned long* memoryResidentSetSize,
			    bool* memoryResidentSetSizeSet,
			    unsigned long* memoryLimitBytes,
			    bool* memoryLimitBytesSet,
			    unsigned long* memoryFileBytes,
			    bool* memoryFileBytesSet,
			    unsigned long* memoryAnonymousBytes,
			    bool* memoryAnonymousBytesSet,
			    unsigned long* memoryMappedFileBytes,
			    bool* memoryMappedFileBytesSet,
			    PerfStatisticsPtr* perfStatistics,
			    unsigned long* netRxPackets,
			    bool* netRxPacketsSet, 
			    unsigned long* netRxBytes,
			    bool* netRxBytesSet, 
			    unsigned long* netRxErrors,
			    bool* netRxErrorsSet, 
			    unsigned long* netRxDropped,
			    bool* netRxDroppedSet, 
			    unsigned long* netTxPackets,
			    bool* netTxPacketsSet, 
			    unsigned long* netTxBytes,
			    bool* netTxBytesSet, 
			    unsigned long* netTxErrors,
			    bool* netTxErrorsSet, 
			    unsigned long* netTxDropped,
			    bool* netTxDroppedSet
			    )
{
  *cpusUserTimeSecsSet = false;
  *cpusSystemTimeSecsSet = false;
  *cpusPeriodsSet = false;
  *cpusThrottledSet = false;
  *cpusThrottledTimeSecsSet = false;
  *memoryResidentSetSizeSet = false;
  *memoryLimitBytesSet = false;
  *memoryFileBytesSet = false;
  *memoryAnonymousBytesSet = false;
  *memoryMappedFileBytesSet = false;
  *netRxPacketsSet = false;
  *netRxBytesSet = false;
  *netRxErrorsSet = false;
  *netRxDroppedSet = false;
  *netTxPacketsSet = false;
  *netTxBytesSet = false;
  *netTxErrorsSet = false;
  *netTxDroppedSet = false;
  *perfStatistics = NULL;

  *timestamp = stats->timestamp();
  *cpusLimit = stats->cpus_limit();

  if (stats->has_cpus_user_time_secs())
    {
      *cpusUserTimeSecs = stats->cpus_user_time_secs();
      *cpusUserTimeSecsSet = true;
    }

  if (stats->has_cpus_system_time_secs())
    {
      *cpusSystemTimeSecs = stats->cpus_system_time_secs();
      *cpusSystemTimeSecsSet = true;
    }

  if (stats->has_cpus_nr_periods())
    {
      *cpusPeriods = stats->cpus_nr_periods();
      *cpusPeriodsSet = true;
    }

  if (stats->has_cpus_nr_throttled())
    {
      *cpusThrottled = stats->cpus_nr_throttled();
      *cpusThrottledSet = true;
    }

  if (stats->has_cpus_throttled_time_secs())
    {
      *cpusThrottledTimeSecs = stats->cpus_throttled_time_secs();
      *cpusThrottledTimeSecsSet = true;
    }

  if (stats->has_mem_rss_bytes())
    {
      *memoryResidentSetSize = stats->mem_rss_bytes();
      *memoryResidentSetSizeSet = true;
    }

  if (stats->has_mem_limit_bytes())
    {
      *memoryLimitBytes = stats->mem_limit_bytes();
      *memoryLimitBytesSet = true;
    }

  if (stats->has_mem_file_bytes())
    {
      *memoryFileBytes = stats->mem_file_bytes();
      *memoryFileBytesSet = true;
    }

  if (stats->has_mem_anon_bytes())
    {
      *memoryAnonymousBytes = stats->mem_anon_bytes();
      *memoryAnonymousBytesSet = true;
    }

  if (stats->has_mem_mapped_file_bytes())
    {
      *memoryMappedFileBytes = stats->mem_mapped_file_bytes();
      *memoryMappedFileBytesSet = true;
    }

  if (stats->has_perf())
    {
      *perfStatistics = stats->mutable_perf();
    }

  if (stats->has_net_rx_packets())
    {
      *netRxPackets = stats->net_rx_packets();
      *netRxPacketsSet = true;
    }
  
  if (stats->has_net_rx_bytes())
    {
      *netRxBytes = stats->net_rx_bytes();
      *netRxBytesSet = true;
    }
  if (stats->has_net_rx_errors())
    {
      *netRxErrors = stats->net_rx_errors();
      *netRxErrorsSet = true;
    }

  if (stats->has_net_rx_dropped())
    {
      *netRxDropped = stats->net_rx_dropped();
      *netRxDroppedSet = true;
    }

  if (stats->has_net_tx_packets())
    {
      *netTxPackets = stats->net_tx_packets();
      *netTxPacketsSet = true;
    }

  if (stats->has_net_tx_bytes())
    {
      *netTxBytes = stats->net_tx_bytes();
      *netTxBytesSet = true;
    }

  if (stats->has_net_tx_errors())
    {
      *netTxErrors = stats->net_tx_errors();
      *netTxErrorsSet = true;
    }

  if (stats->has_net_tx_dropped())
    {
      *netTxDropped = stats->net_tx_dropped();
      *netTxDroppedSet = true;
    }
}

void destroyResourceStatistics(ResourceStatisticsPtr statistics)
{
  delete statistics;
}
