#include <iostream>
#include "types.h"

using namespace mesos;

ResourceUsagePtr toResourceUsage(SlaveIDPtr slaveID,
				 FrameworkIDPtr frameworkID,
				 ExecutorIDPtr executorID,
				 char* executorName,
				 int nameLen,
				 TaskIDPtr taskID,
				 ResourceStatisticsPtr statistics)
{
  ResourceUsagePtr usage = new ResourceUsage();
  *usage->mutable_slave_id() = *slaveID;
  *usage->mutable_framework_id() = *frameworkID;
  if (executorID != NULL)
    *usage->mutable_executor_id() = *executorID;
  if (executorName != NULL)
    usage->set_executor_name(executorName, nameLen);
  if (taskID != NULL)
    *usage->mutable_task_id() = *taskID;
  if (statistics != NULL)
    *usage->mutable_statistics() = *statistics;

  return usage;
}

void fromResourceUsage(ResourceUsagePtr usage,
		       SlaveIDPtr* slaveID,
		       FrameworkIDPtr* frameworkID,
		       ExecutorIDPtr* executorID,
		       char** executorName,
		       int* nameLen,
		       TaskIDPtr* taskID,
		       ResourceStatisticsPtr* statistics)
{
  *slaveID = usage->mutable_slave_id();
  *frameworkID = usage->mutable_framework_id();
  if (usage->has_executor_id())
    *executorID = usage->mutable_executor_id();
  if (usage->has_executor_name())
    {
      std::string* n = usage->mutable_executor_name();
      *executorName = (char*) n->data();
      *nameLen = n->size();
    }
  if (usage->has_task_id())
    *taskID = usage->mutable_task_id();
  if (usage->has_statistics())
    *statistics = usage->mutable_statistics();
}

void destroyResourceUsage(ResourceUsagePtr usage)
{
  delete usage;
}
