#include <iostream>
#include "types.h"

using namespace mesos;

TaskInfoPtr toTaskInfo(char* infoName,
		       int infoNameLen,
		       TaskIDPtr taskID,
		       SlaveIDPtr slaveID,
		       ResourcePtr* resources,
		       int resourcesLen,
		       ExecutorInfoPtr executorInfo,
		       CommandInfoPtr commandInfo,
		       char* data,
		       int dataLen,
		       ContainerInfoPtr containerInfo,
		       HealthCheckPtr healthCheck)
{
  TaskInfoPtr info = new TaskInfo();

  info->set_name(infoName, infoNameLen);

  *info->mutable_task_id() = *taskID;

  *info->mutable_slave_id() = *slaveID;

  for (int i = 0; i < resourcesLen; ++i)
    *info->add_resources() = *resources[i];

  if (executorInfo != NULL)
    info->mutable_executor()->MergeFrom(*executorInfo);

  if (commandInfo != NULL)
    info->mutable_command()->MergeFrom(*commandInfo);

  if (data != NULL)
    info->set_data(data, dataLen);

  if (containerInfo != NULL)
    info->mutable_container()->MergeFrom(*containerInfo);

  if (healthCheck != NULL)
    info->mutable_health_check()->MergeFrom(*healthCheck);

  return info;
}

void fromTaskInfo(TaskInfoPtr taskInfo,
		  char** infoName,
		  int* infoNameLen,
		  TaskIDPtr* taskID,
		  SlaveIDPtr* slaveID,
		  ResourcePtr** resources,
		  int* resourcesLen,
		  ExecutorInfoPtr* executorInfo,
		  CommandInfoPtr* commandInfo,
		  char** data,
		  int* dataLen,
		  ContainerInfoPtr* containerInfo,
		  HealthCheckPtr* healthCheck)
{
  std::string* in = taskInfo->mutable_name();
  *infoName = (char*) in->data();
  *infoNameLen = in->size();
  *taskID = taskInfo->mutable_task_id();
  *slaveID = taskInfo->mutable_slave_id();
  *resources = taskInfo->mutable_resources()->mutable_data();
  *resourcesLen = taskInfo->resources_size();
  if (taskInfo->has_executor())
    *executorInfo = taskInfo->mutable_executor();

  if (taskInfo->has_command())
    *commandInfo = taskInfo->mutable_command();	

  if (taskInfo->has_data())
    {
      *data = (char*) taskInfo->mutable_data()->data();
      *dataLen = taskInfo->mutable_data()->size();
    }

  if (taskInfo->has_container())
    *containerInfo = taskInfo->mutable_container();

  if (taskInfo->has_health_check())
    *healthCheck = taskInfo->mutable_health_check();
}

void destroyTaskInfo(TaskInfoPtr taskInfo)
{
  delete taskInfo;
}
