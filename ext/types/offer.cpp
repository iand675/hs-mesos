#include <iostream>
#include "types.h"

using namespace mesos;

OfferPtr toOffer(OfferIDPtr offerID,
		 FrameworkIDPtr frameworkID,
		 SlaveIDPtr slaveID,
		 char* hostname,
		 int hostnameLen,
		 ResourcePtr* resources,
		 int resourcesLen,
		 AttributePtr* attributes,
		 int attributesLen,
		 ExecutorIDPtr* executorIDs,
		 int idsLen)
{
  OfferPtr offer = new Offer();

  *offer->mutable_id() = *offerID;

  *offer->mutable_framework_id() = *frameworkID;

  *offer->mutable_slave_id() = *slaveID;

  offer->set_hostname(hostname, hostnameLen);

  for (int i = 0; i < resourcesLen; ++i)
    *offer->add_resources() = *resources[i];

  for (int i = 0; i < attributesLen; ++i)
    *offer->add_attributes() = *attributes[i];

  for (int i = 0; i < idsLen; ++i)
    *offer->add_executor_ids() = *executorIDs[i];

  return offer;
}

void fromOffer(OfferPtr offer,
	       OfferIDPtr* offerID,
	       FrameworkIDPtr* frameworkID,
	       SlaveIDPtr* slaveID,
	       char** hostname,
	       int* hostnameLen,
	       ResourcePtr** resources,
	       int* resourcesLen,
	       AttributePtr** attributes,
	       int* attributesLen,
	       ExecutorIDPtr** executorIDs,
	       int* idsLen)
{
  *offerID = offer->mutable_id();

  *frameworkID = offer->mutable_framework_id();

  *slaveID = offer->mutable_slave_id();

  *hostname = (char*) offer->mutable_hostname()->data();

  *hostnameLen = offer->mutable_hostname()->size();

  *resources = offer->mutable_resources()->mutable_data();

  *resourcesLen = offer->resources_size();

  *attributes = offer->mutable_attributes()->mutable_data();

  *attributesLen = offer->attributes_size();

  *executorIDs = offer->mutable_executor_ids()->mutable_data();

  *idsLen = offer->executor_ids_size();
}

void destroyOffer(OfferPtr offer)
{
  delete offer;
}
