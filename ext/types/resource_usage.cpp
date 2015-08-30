#include <iostream>
#include "types.h"

using namespace mesos;

ResourceUsagePtr toResourceUsage(ResourceUsage_ExecutorPtr* executors,
                                 int executorsCount)
{
  ResourceUsagePtr usage = new ResourceUsage();

  for (int i = 0; i < executorsCount; ++i)
    *usage->add_executors() = *executors[i];

  return usage;
}

void fromResourceUsage(ResourceUsagePtr usage,
                       ResourceUsage_ExecutorPtr** executors,
                       int* executorsCount)
{
    *executors = usage->mutable_executors()->mutable_data();
    *executorsCount = usage->executors_size();
}

void destroyResourceUsage(ResourceUsagePtr usage)
{
  delete usage;
}
