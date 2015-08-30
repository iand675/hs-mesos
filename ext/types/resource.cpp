#include <iostream>
#include "types.h"

using namespace mesos;

ResourcePtr toResource(char* name,
		       int nameLen,
		       ValuePtr value,
		       char* role,
           int roleLen,
           char* reservationPrincipal,
           int reservationPrincipalLen,
           char* diskInfoPersistence,
           int diskInfoPersistenceLen,
           VolumePtr diskInfoVolume)
{
  ResourcePtr resource = new Resource();
  resource->set_name(name, nameLen);
  resource->set_type(value->type());

  if (value->has_scalar())
    *(resource->mutable_scalar()) = *value->mutable_scalar();

  if (value->has_ranges())
    *(resource->mutable_ranges()) = *value->mutable_ranges();

  if (value->has_set())
    *(resource->mutable_set()) = *value->mutable_set();

  if (value->has_text())
    *resource->mutable_set()->add_item() = value->mutable_text()->value();

  if (role != NULL)
    resource->set_role(role, roleLen);

  if(reservationPrincipal != NULL)
    {
      Resource_ReservationInfo* reservation = new Resource_ReservationInfo();
      reservation->set_principal(reservationPrincipal, reservationPrincipalLen);
      *resource->mutable_reservation() = *reservation;
    }

  Resource_DiskInfo* diskInfo = NULL;
  if(diskInfoPersistence != NULL)
    {
      if(diskInfo == NULL)
        diskInfo = new Resource_DiskInfo();
      Resource_DiskInfo_Persistence* pers = new Resource_DiskInfo_Persistence();
      pers->set_id(diskInfoPersistence, diskInfoPersistenceLen);
      *diskInfo->mutable_persistence() = *pers;
    }

  if(diskInfoVolume != NULL)
    {
      if(diskInfo == NULL)
        diskInfo = new Resource_DiskInfo();
      *diskInfo->mutable_volume() = *diskInfoVolume;
    }

  if (diskInfo != NULL)
    *resource->mutable_disk() = *diskInfo;

  return resource;
}

void fromResource(ResourcePtr resource,
		  char** name,
		  int* nameLen,
		  ValuePtr* value,
		  char** role,
      int* roleLen,
      char** reservationPrincipal,
      int* reservationPrincipalLen,
      char** diskInfoPersistence,
      int* diskInfoPersistenceLen,
      VolumePtr* diskInfoVolume)
{
  std::string* n = resource->mutable_name();
  *name = (char*) n->data();
  *nameLen = n->size();

  Value* v = new Value();
  if (resource->has_scalar())
    {
      v->set_type(Value_Type_SCALAR);
      *v->mutable_scalar() = resource->scalar();
    }
  if (resource->has_ranges())
    {
      v->set_type(Value_Type_RANGES);
      *v->mutable_ranges() = resource->ranges();
    }
  if (resource->has_set())
    {
      v->set_type(Value_Type_SET);
      *v->mutable_set() = resource->set();
    }

  *value = v;
  if (resource->has_role())
    {
      std::string* r = resource->mutable_role();
      *role = (char*) r->data();
      *roleLen = r->size();
    }

  if (resource->has_reservation())
    {
      std::string* p = resource->mutable_reservation()->mutable_principal();
      *reservationPrincipal = (char*) p->data();
      *reservationPrincipalLen = p->size();
    }

  if (resource->has_disk())
    {
      Resource_DiskInfo* disk = resource->mutable_disk();
      if (disk->has_persistence())
        {
          std::string* d = disk->mutable_persistence()->mutable_id();
          *diskInfoPersistence = (char*) d->data();
          *diskInfoPersistenceLen = d->size();
        }
      if (disk->has_volume())
        {
          *diskInfoVolume = disk->mutable_volume();
        }
    }

}

void destroyResource(ResourcePtr resource)
{
  delete resource;
}
