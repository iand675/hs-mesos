#include <iostream>
#include "types.h"

using namespace mesos;

SlaveInfoPtr toSlaveInfo(char* hostname,
			 int hostnameLen,
			 unsigned int* port,
			 ResourcePtr* resources,
			 int resourcesLen,
			 AttributePtr* attributes,
			 int attributesLen,
			 SlaveIDPtr slaveID,
			 bool* checkpoint)
{
  SlaveInfoPtr info = new SlaveInfo();
  info->set_hostname(hostname, hostnameLen);
  if (port != NULL)
    info->set_port(*port);
  if (resourcesLen > 0)
    {
      ::google::protobuf::RepeatedPtrField<Resource>* rs = info->mutable_resources();
      for (int i = 0; i < resourcesLen; ++i)
	*rs->Add() = *resources[i];
    }
  if (attributesLen > 0)
    {
      ::google::protobuf::RepeatedPtrField<Attribute>* as = info->mutable_attributes();
      for (int i = 0; i < attributesLen; ++i)
	*as->Add() = *attributes[i];
    }
  if (slaveID != NULL)
    *info->mutable_id() = *slaveID;

  if (checkpoint != NULL)
    info->set_checkpoint(*checkpoint);

  return info;
}

void fromSlaveInfo(SlaveInfoPtr slaveInfo,
		   char** hostname,
		   int* hostnameLen,
		   bool* portSet,
		   unsigned int* port,
		   ResourcePtr** resources,
		   int* resourcesLen,
		   AttributePtr** attributes,
		   int* attributeLen,
		   SlaveIDPtr* slaveID,
		   bool* checkpointSet,
		   bool* checkpoint)
{
  *portSet = false;
  *checkpointSet = false;

  std::string h = slaveInfo->hostname();
  *hostname = (char*) h.data();
  *hostnameLen = h.size();

  if (slaveInfo->has_port())
    {
      *port = slaveInfo->port();
      *portSet = true;
    }

  *resourcesLen = slaveInfo->resources_size();
  *resources = slaveInfo->mutable_resources()->mutable_data();

  *attributeLen = slaveInfo->attributes_size();
  *attributes = slaveInfo->mutable_attributes()->mutable_data();

  if (slaveInfo->has_id())
    *slaveID = slaveInfo->mutable_id();

  if (slaveInfo->has_checkpoint())
    {
      *checkpoint = slaveInfo->checkpoint();
      *checkpointSet = true;
    }	
}

void destroySlaveInfo(SlaveInfoPtr slaveInfo)
{
  delete slaveInfo;
}
