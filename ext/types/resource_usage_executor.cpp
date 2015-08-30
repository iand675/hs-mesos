#include <iostream>
#include "types.h"

using namespace mesos;

ResourceUsage_ExecutorPtr toResourceUsage_Executor(ExecutorInfoPtr executorInfo,
                                                   ResourcePtr* resources,
                                                   int resourcesCount,
                                                   ResourceStatisticsPtr statistics)
{
  ResourceUsage_ExecutorPtr executor = new ResourceUsage_Executor();

  *executor->mutable_executor_info() = *executorInfo;

  if (resources != NULL)
    {
      for (int i = 0; i < resourcesCount; ++i)
        {
          *executor->add_allocated() = *resources[i];
        }
    }

  if (statistics != NULL)
    *executor->mutable_statistics() = *statistics;


  return executor;
}

void fromResourceUsage_Executor(ResourceUsage_ExecutorPtr executor,
                                ExecutorInfoPtr* executorInfo,
                                ResourcePtr** resources,
                                int* resourcesCount,
                                ResourceStatisticsPtr* statistics)
{

  *executorInfo = executor->mutable_executor_info();

  *resources = executor->mutable_allocated()->mutable_data();
  *resourcesCount = executor->allocated_size();

  if (executor->has_statistics())
    *statistics = executor->mutable_statistics();
}

void destroyResourceUsage_Executor(ResourceUsage_ExecutorPtr exe)
{
  delete exe;
}
