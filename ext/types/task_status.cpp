#include <iostream>
#include "types.h"

using namespace mesos;

TaskStatusPtr toTaskStatus(TaskIDPtr taskID,
			   int state,
			   char* message,
			   int messageLen,
			   char* data,
			   int dataLen,
			   SlaveIDPtr slaveID,
			   ExecutorIDPtr executorID,
			   double* timestamp,
			   bool* healthCheck)
{
  TaskStatusPtr status = new TaskStatus();
  status->mutable_task_id()->MergeFrom(*taskID);
  status->set_state((TaskState) state);

  if (message != NULL)
    status->set_message(message, messageLen);

  if (data != NULL)
    status->set_data(data, dataLen);

  if (slaveID != NULL)
    *status->mutable_slave_id() = *slaveID;

  if (executorID != NULL)
    *status->mutable_executor_id() = *executorID;

  if (timestamp != NULL)
    status->set_timestamp(*timestamp);

  if (healthCheck != NULL)
    status->set_healthy(*healthCheck);

  return status;
}

void fromTaskStatus(TaskStatusPtr status,
		    TaskIDPtr* taskID,
		    int* state,
		    char** message,
		    int* messageLen,
		    char** data,
		    int* dataLen,
		    SlaveIDPtr* slaveID,
		    ExecutorIDPtr* executorID,
		    bool* timestampSet,
		    double* timestamp,
		    bool* healthCheckSet,
		    bool* healthCheck)
{
  *timestampSet = false;
  *healthCheckSet = false;
  *slaveID = NULL;
  *executorID = NULL;

  *taskID = status->mutable_task_id();
  *state = status->state();
  if (status->has_message())
    {
      std::string* m = status->mutable_message();
      *message = (char*) m->data();
      *messageLen = m->size();
    }

  if (status->has_data())
    {
      std::string* d = status->mutable_data();
      *data = (char*) d->data();
      *dataLen = d->size();
    }

  if (status->has_slave_id())
    *slaveID = status->mutable_slave_id();

  if (status->has_executor_id())
    *executorID = status->mutable_executor_id();

  if (status->has_timestamp())
    {
      *timestampSet = true;
      *timestamp = status->timestamp();
    }

  if (status->has_healthy())
    {
      *healthCheckSet = true;
      *healthCheck = status->healthy();
    }
}

void destroyTaskStatus(TaskStatusPtr taskStatus)
{
  delete taskStatus;
}
