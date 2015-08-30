#include <iostream>
#include "types.h"

using namespace mesos;

DiscoveryInfoPtr toDiscoveryInfo(int visibility,
                                 char* name,
                                 int nameLen,
                                 char* env,
                                 int envLen,
                                 char* location,
                                 int locationLen,
                                 char* version,
                                 int versionLen,
                                 PortPtr* ports,
                                 int portsCount,
                                 LabelPtr* labels,
                                 int labelsCount)
{
  DiscoveryInfoPtr disc = new DiscoveryInfo();

  disc->set_visibility((DiscoveryInfo_Visibility) visibility);

  if (name != NULL)
    disc->set_name(name, nameLen);

  if (env != NULL)
    disc->set_environment(env, envLen);

  if (location != NULL)
    disc->set_location(location, locationLen);

  if (version != NULL)
    disc->set_version(version, versionLen);

  Ports* ps = new Ports();
  for(int i = 0; i < portsCount; ++i)
    {
      *ps->add_ports() = *ports[i];
    }
  *disc->mutable_ports() = *ps;

  Labels* lbs = new Labels();
  for (int i = 0; i < labelsCount; ++i)
    {
      *lbs->add_labels() = *labels[i];
    }
  *disc->mutable_labels() = *lbs;

  return disc;
}

void fromDiscoveryInfo(DiscoveryInfoPtr disc,
                       int* visibility,
                       char** name,
                       int* nameLen,
                       char** env,
                       int* envLen,
                       char** location,
                       int* locationLen,
                       char** version,
                       int* versionLen,
                       PortPtr** ports,
                       int* portsCount,
                       LabelPtr** labels,
                       int* labelsCount)
{
  if (disc->has_visibility())
    *visibility = (int) disc->visibility();

  if (disc->has_name())
    {
      std::string* n = disc->mutable_name();
      *name = (char*) n->data();
      *nameLen = n->size();
    }

  if (disc->has_environment())
    {
      std::string* e = disc->mutable_environment();
      *env = (char*) e->data();
      *envLen = e->size();
    }

  if (disc->has_location())
    {
      std::string* l = disc->mutable_location();
      *location = (char*) l->data();
      *locationLen = l->size();
    }

  if (disc->has_version())
    {
      std::string* v = disc->mutable_version();
      *version = (char*) v->data();
      *versionLen = v->size();
    }

  if (disc->has_ports())
    {
      *ports = disc->mutable_ports()->mutable_ports()->mutable_data();
      *portsCount = disc->mutable_ports()->ports_size();
    }

  if (disc->has_labels())
    {
      *labels = disc->mutable_labels()->mutable_labels()->mutable_data();
      *labelsCount = disc->mutable_labels()->labels_size();
    }

}

void destroyDiscoveryInfo(DiscoveryInfoPtr disc)
{
  delete disc;
}
