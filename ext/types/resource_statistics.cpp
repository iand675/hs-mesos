#include <iostream>
#include "types.h"

using namespace mesos;

ResourceStatisticsPtr toResourceStatistics(double timestamp,
                                           unsigned int* processes,
                                           unsigned int* threads,
                                           double* cpusUserTimeSecs,
                                           double* cpusSystemTimeSecs,
                                           double cpusLimit,
                                           unsigned int* cpusPeriods,
                                           unsigned int* cpusThrottled,
                                           double* cpusThrottledTimeSecs,
                                           unsigned long* memoryTotalBytes,
                                           unsigned long* memoryTotalMemSwBytes,
                                           unsigned long* memoryLimitBytes,
                                           unsigned long* memorySoftLimitBytes,
                                           unsigned long* memoryFileBytes,
                                           unsigned long* memoryAnonymousBytes,
                                           unsigned long* memoryCacheBytes,
                                           unsigned long* memoryRssBytes,
                                           unsigned long* memoryMappedFileBytes,
                                           unsigned long* memorySwapBytes,
                                           unsigned long* memoryLowPressureCounter,
                                           unsigned long* memoryMediumPressureCounter,
                                           unsigned long* memoryCriticalPressureCounter,
                                           unsigned long* diskLimitBytes,
                                           unsigned long* diskUsedBytes,
                                           PerfStatisticsPtr perfStatistics,
                                           unsigned long* netRxPackets,
                                           unsigned long* netRxBytes,
                                           unsigned long* netRxErrors,
                                           unsigned long* netRxDropped,
                                           unsigned long* netTxPackets,
                                           unsigned long* netTxBytes,
                                           unsigned long* netTxErrors,
                                           unsigned long* netTxDropped,
                                           double* netTcpRttMicroSecsP50,
                                           double* netTcpRttMicroSecsP90,
                                           double* netTcpRttMicroSecsP95,
                                           double* netTcpRttMicroSecsP99,
                                           double* netTcpActiveConn,
                                           double* netTcpTimeWaitConn,
                                           TrafficControlStatisticsPtr* netTrafficControlStats,
                                           int trafficStatsCount
                                           )
{
  ResourceStatisticsPtr stats = new ResourceStatistics();
  stats->set_timestamp(timestamp);
  if (processes != NULL)
    stats->set_processes(*processes);
  if (threads != NULL)
    stats->set_threads(*threads);
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
  if (memoryTotalBytes != NULL)
    stats->set_mem_total_bytes(*memoryTotalBytes);
  if (memoryTotalMemSwBytes)
    stats->set_mem_total_memsw_bytes(*memoryTotalMemSwBytes);
  if (memoryLimitBytes != NULL)
    stats->set_mem_limit_bytes(*memoryLimitBytes);
  if (memorySoftLimitBytes != NULL)
    stats->set_mem_soft_limit_bytes(*memorySoftLimitBytes);
  if (memoryFileBytes != NULL)
    stats->set_mem_file_bytes(*memoryFileBytes);
  if (memoryAnonymousBytes != NULL)
    stats->set_mem_anon_bytes(*memoryAnonymousBytes);
  if (memoryCacheBytes != NULL)
    stats->set_mem_cache_bytes(*memoryCacheBytes);
  if (memoryRssBytes != NULL)
    stats->set_mem_rss_bytes(*memoryRssBytes);
  if (memoryMappedFileBytes != NULL)
    stats->set_mem_mapped_file_bytes(*memoryMappedFileBytes);
  if (memorySwapBytes != NULL)
    stats->set_mem_swap_bytes(*memorySwapBytes);
  if (memoryLowPressureCounter != NULL)
    stats->set_mem_low_pressure_counter(*memoryLowPressureCounter);
  if (memoryMediumPressureCounter != NULL)
    stats->set_mem_medium_pressure_counter(*memoryMediumPressureCounter);
  if (memoryCriticalPressureCounter != NULL)
    stats->set_mem_critical_pressure_counter(*memoryCriticalPressureCounter);
  if (diskLimitBytes != NULL)
    stats->set_disk_limit_bytes(*diskLimitBytes);
  if (diskUsedBytes != NULL)
    stats->set_disk_used_bytes(*diskUsedBytes);
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

  if (netTcpRttMicroSecsP50 != NULL)
    stats->set_net_tcp_rtt_microsecs_p50(*netTcpRttMicroSecsP50);
  if (netTcpRttMicroSecsP90 != NULL)
    stats->set_net_tcp_rtt_microsecs_p90(*netTcpRttMicroSecsP90);
  if (netTcpRttMicroSecsP95 != NULL)
    stats->set_net_tcp_rtt_microsecs_p95(*netTcpRttMicroSecsP95);
  if (netTcpRttMicroSecsP99 != NULL)
    stats->set_net_tcp_rtt_microsecs_p99(*netTcpRttMicroSecsP99);

  if (netTcpActiveConn != NULL)
    stats->set_net_tcp_active_connections(*netTcpActiveConn);

  if (netTcpTimeWaitConn != NULL)
    stats->set_net_tcp_time_wait_connections(*netTcpTimeWaitConn);

  for (int i = 0; i < trafficStatsCount; ++i)
    *stats->add_net_traffic_control_statistics() = *netTrafficControlStats[i];

  return stats;
}

void fromResourceStatistics(ResourceStatisticsPtr stats,
                            double* timestamp,
                            unsigned int* processes,
                            bool* processesSet,
                            unsigned int* threads,
                            bool* threadsSet,
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
                            unsigned long* memoryTotalBytes,
                            bool* memoryTotalBytesSet,
                            unsigned long* memoryTotalMemSwBytes,
                            bool* memoryTotalMemSwBytesSet,
                            unsigned long* memoryLimitBytes,
                            bool* memoryLimitBytesSet,
                            unsigned long* memorySoftLimitBytes,
                            bool* memorySoftLimitBytesSet,
                            unsigned long* memoryFileBytes,
                            bool* memoryFileBytesSet,
                            unsigned long* memoryAnonymousBytes,
                            bool* memoryAnonymousBytesSet,
                            unsigned long* memoryCacheBytes,
                            bool* memoryCacheBytesSet,
                            unsigned long* memoryRssBytes,
                            bool* memoryRssBytesSet,
                            unsigned long* memoryMappedFileBytes,
                            bool* memoryMappedFileBytesSet,
                            unsigned long* memorySwapBytes,
                            bool* memorySwapBytesSet,
                            unsigned long* memoryLowPressureCounter,
                            bool* memoryLowPressureCounterSet,
                            unsigned long* memoryMediumPressureCounter,
                            bool* memoryMediumPressureCounterSet,
                            unsigned long* memoryCriticalPressureCounter,
                            bool* memoryCriticalPressureCounterSet,
                            unsigned long* diskLimitBytes,
                            bool* diskLimitBytesSet,
                            unsigned long* diskUsedBytes,
                            bool* diskUsedBytesSet,
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
                            bool* netTxDroppedSet,
                            double* netTcpRttMicroSecsP50,
                            bool* netTcpRttMicroSecsP50Set,
                            double* netTcpRttMicroSecsP90,
                            bool* netTcpRttMicroSecsP90Set,
                            double* netTcpRttMicroSecsP95,
                            bool* netTcpRttMicroSecsP95Set,
                            double* netTcpRttMicroSecsP99,
                            bool* netTcpRttMicroSecsP99Set,
                            double* netTcpActiveConn,
                            bool* netTcpActiveConnSet,
                            double* netTcpTimeWaitConn,
                            bool* netTcpTimeWaitConnSet,
                            TrafficControlStatisticsPtr** netTrafficControlStats,
                            int* trafficStatsCount
                            )
{
  *timestamp = stats->timestamp();
  *cpusLimit = stats->cpus_limit();

  *processesSet = false;
  *threadsSet = false;
  *cpusUserTimeSecsSet = false;
  *cpusSystemTimeSecsSet = false;
  *cpusPeriodsSet = false;
  *cpusThrottledSet = false;
  *cpusThrottledTimeSecsSet = false;
  *memoryTotalBytesSet = false;
  *memoryTotalMemSwBytesSet = false;
  *memoryLimitBytesSet = false;
  *memorySoftLimitBytesSet = false;
  *memoryFileBytesSet = false;
  *memoryAnonymousBytesSet = false;
  *memoryCacheBytesSet = false;
  *memoryRssBytesSet = false;
  *memoryMappedFileBytesSet = false;
  *memorySwapBytesSet = false;
  *memoryLowPressureCounterSet = false;
  *memoryMediumPressureCounterSet = false;
  *memoryCriticalPressureCounterSet = false;
  *diskLimitBytesSet = false;
  *diskUsedBytesSet = false;
  *netRxPacketsSet = false;
  *netRxBytesSet = false;
  *netRxErrorsSet = false;
  *netRxDroppedSet = false;
  *netTxPacketsSet = false;
  *netTxBytesSet = false;
  *netTxErrorsSet = false;
  *netTxDroppedSet = false;
  *netTcpRttMicroSecsP50Set = false;
  *netTcpRttMicroSecsP90Set = false;
  *netTcpRttMicroSecsP95Set = false;
  *netTcpRttMicroSecsP99Set = false;
  *netTcpActiveConnSet = false;
  *netTcpTimeWaitConnSet = false;

  if (stats->has_processes())
    {
      *processes = stats->processes();
      *processesSet = true;
    }

  if (stats->has_threads())
    {
      *threads = stats->threads();
      *threadsSet = true;
    }

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

  if (stats->has_mem_total_bytes())
    {
      *memoryTotalBytes = stats->mem_total_bytes();
      *memoryTotalBytesSet = true;
    }

  if (stats->has_mem_total_memsw_bytes())
    {
      *memoryTotalMemSwBytes = stats->mem_total_memsw_bytes();
      *memoryTotalMemSwBytesSet = true;
    }

  if (stats->has_mem_limit_bytes())
    {
      *memoryLimitBytes = stats->mem_limit_bytes();
      *memoryLimitBytesSet = true;
    }

  if (stats->has_mem_soft_limit_bytes())
    {
      *memorySoftLimitBytes = stats->mem_soft_limit_bytes();
      *memorySoftLimitBytesSet = true;
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

  if (stats->has_mem_cache_bytes())
    {
      *memoryCacheBytes = stats->mem_cache_bytes();
      *memoryCacheBytesSet = true;
    }

  if (stats->has_mem_rss_bytes())
    {
      *memoryRssBytes = stats->mem_rss_bytes();
      *memoryRssBytesSet = true;
    }

  if (stats->has_mem_mapped_file_bytes())
    {
      *memoryMappedFileBytes = stats->mem_mapped_file_bytes();
      *memoryMappedFileBytesSet = true;
    }

  if (stats->has_mem_swap_bytes())
    {
      *memorySwapBytes = stats->mem_swap_bytes();
      *memorySwapBytesSet = true;
    }

  if (stats->has_mem_low_pressure_counter())
    {
      *memoryLowPressureCounter = stats->mem_low_pressure_counter();
      *memoryLowPressureCounterSet = true;
    }

  if (stats->has_mem_medium_pressure_counter())
    {
      *memoryMediumPressureCounter = stats->mem_medium_pressure_counter();
      *memoryMediumPressureCounterSet = true;
    }

  if (stats->has_mem_critical_pressure_counter())
    {
      *memoryCriticalPressureCounter = stats->mem_critical_pressure_counter();
      *memoryCriticalPressureCounterSet = true;
    }

  if (stats->has_disk_limit_bytes())
    {
      *diskLimitBytes = stats->disk_limit_bytes();
      *diskLimitBytesSet = true;
    }

  if (stats->has_disk_used_bytes())
    {
      *diskUsedBytes = stats->disk_used_bytes();
      *diskUsedBytesSet = true;
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

  if (stats->has_net_tcp_rtt_microsecs_p50())
    {
      *netTcpRttMicroSecsP50 = stats->net_tcp_rtt_microsecs_p50();
      *netTcpRttMicroSecsP50Set = true;
    }

  if (stats->has_net_tcp_rtt_microsecs_p90())
    {
      *netTcpRttMicroSecsP90 = stats->net_tcp_rtt_microsecs_p90();
      *netTcpRttMicroSecsP90Set = true;
    }


  if (stats->has_net_tcp_rtt_microsecs_p95())
    {
      *netTcpRttMicroSecsP95 = stats->net_tcp_rtt_microsecs_p95();
      *netTcpRttMicroSecsP95Set = true;
    }

  if (stats->has_net_tcp_rtt_microsecs_p99())
    {
      *netTcpRttMicroSecsP99 = stats->net_tcp_rtt_microsecs_p99();
      *netTcpRttMicroSecsP99Set = true;
    }

  if (stats->has_net_tcp_active_connections())
    {
      *netTcpActiveConn = stats->net_tcp_active_connections();
      *netTcpActiveConnSet = true;
    }

  if (stats->has_net_tcp_time_wait_connections())
    {
      *netTcpTimeWaitConn = stats->net_tcp_time_wait_connections();
      *netTcpTimeWaitConnSet = true;
    }

  *netTrafficControlStats = stats->mutable_net_traffic_control_statistics()->mutable_data();
  *trafficStatsCount = stats->net_traffic_control_statistics_size();
}

void destroyResourceStatistics(ResourceStatisticsPtr statistics)
{
  delete statistics;
}
