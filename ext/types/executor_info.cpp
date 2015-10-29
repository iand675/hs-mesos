#include <iostream>
#include "types.h"

using namespace mesos;

ExecutorInfoPtr toExecutorInfo(ExecutorIDPtr executorID,
			       FrameworkIDPtr frameworkID,
			       CommandInfoPtr commandInfo,
			       ContainerInfoPtr containerInfo,
			       ResourcePtr* resources,
			       int resourceLen,
			       char* name,
			       int nameLen,
			       char* source,
			       int sourceLen,
             DiscoveryInfoPtr discovery)
{
  ExecutorInfoPtr info = new ExecutorInfo();
  *info->mutable_executor_id() = *executorID;
  *info->mutable_framework_id() = *frameworkID;
  *info->mutable_command() = *commandInfo;
  if (containerInfo != NULL)
    *info->mutable_container() = *containerInfo;

  for (int i = 0; i < resourceLen; ++i)
    *info->add_resources() = *resources[i];

  if (name != NULL)
    info->set_name(name, nameLen);

  if (source != NULL)
    info->set_source(source, sourceLen);

  if (discovery != NULL)
    *info->mutable_discovery() = *discovery;

  return info;
}

void fromExecutorInfo(ExecutorInfoPtr info,
		      ExecutorIDPtr* executorID,
		      FrameworkIDPtr* frameworkID,
		      CommandInfoPtr* commandInfo,
		      ContainerInfoPtr* containerInfo,
		      ResourcePtr** resources,
		      int* resourcesLen,
		      char** name,
		      int* nameLen,
		      char** source,
		      int* sourceLen,
          DiscoveryInfoPtr* discovery)
{
  *executorID = info->mutable_executor_id();

  *frameworkID = info->mutable_framework_id();

  *commandInfo = info->mutable_command();

  *resources = info->mutable_resources()->mutable_data();
  *resourcesLen = info->resources_size();

  if (info->has_container())
    *containerInfo = info->mutable_container();

  if (info->has_name())
    {
      std::string* n = info->mutable_name();
      *name = (char*) n->data();
      *nameLen = n->size();
    }

  if (info->has_source())
    {
      std::string* s = info->mutable_source();
      *source = (char*) s->data();
      *sourceLen = s->size();
    }

  if (info->has_discovery())
    {
      *discovery = info->mutable_discovery();
    }
}

void destroyExecutorInfo(ExecutorInfoPtr info)
{
  delete info;
}
