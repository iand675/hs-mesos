#include <iostream>
#include "types.h"

using namespace mesos;

RequestPtr toRequest(SlaveIDPtr slaveID,
		     ResourcePtr* resources,
		     int resourceLen)
{
  RequestPtr request = new Request();
  if (slaveID != NULL)
    *request->mutable_slave_id() = *slaveID;

  ::google::protobuf::RepeatedPtrField<Resource>* rs = request->mutable_resources();
  for (int i = 0; i < resourceLen; ++i)
    *rs->Add() = *resources[i];
  return request;
}

void fromRequest(RequestPtr request,
		 SlaveIDPtr* slaveID,
		 ResourcePtr** resources,
		 int* resourceLen)
{
  if (request->has_slave_id())
    *slaveID = request->mutable_slave_id();

  *resources = request->mutable_resources()->mutable_data();
  *resourceLen = request->resources_size();
}

void destroyRequest(RequestPtr request)
{
  delete request;
}
