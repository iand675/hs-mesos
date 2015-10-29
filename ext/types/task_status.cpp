#include <iostream>
#include "types.h"

using namespace mesos;

TaskStatusPtr toTaskStatus(TaskIDPtr taskID,
			   int state,
			   char* message,
			   int messageLen,
         int* source,
         int* reason,
			   char* data,
			   int dataLen,
			   SlaveIDPtr slaveID,
			   ExecutorIDPtr executorID,
			   double* timestamp,
         char* uuid,
         int uuidLen,
			   bool* healthCheck)
{
  TaskStatusPtr status = new TaskStatus();
  status->mutable_task_id()->MergeFrom(*taskID);
  status->set_state((TaskState) state);

  if (message != NULL)
    status->set_message(message, messageLen);

  if (source != NULL)
    status->set_source((TaskStatus_Source) *source);

  if (reason != NULL)
    status->set_reason((TaskStatus_Reason) *reason);

  if (data != NULL)
    status->set_data(data, dataLen);

  if (slaveID != NULL)
    *status->mutable_slave_id() = *slaveID;

  if (executorID != NULL)
    *status->mutable_executor_id() = *executorID;

  if (timestamp != NULL)
    status->set_timestamp(*timestamp);

  if (uuid != NULL)
    status->set_uuid(uuid, uuidLen);

  if (healthCheck != NULL)
    status->set_healthy(*healthCheck);

  return status;
}

void fromTaskStatus(TaskStatusPtr status,
		    TaskIDPtr* taskID,
		    int* state,
		    char** message,
		    int* messageLen,
        bool* sourceSet,
        int* source,
        bool* reasonSet,
        int* reason,
		    char** data,
		    int* dataLen,
		    SlaveIDPtr* slaveID,
		    ExecutorIDPtr* executorID,
		    bool* timestampSet,
		    double* timestamp,
        char** uuid,
        int* uuidLen,
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

  if (status->has_source())
    {
      *sourceSet = true;
      *source = status->source();
    }

  if (status->has_reason())
    {
      *reasonSet = true;
      *reason = status->reason();
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

  if (status->has_uuid())
    {
      std::string* u = status->mutable_uuid();
      *uuid = (char*) u->data();
      *uuidLen = u->size();
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
