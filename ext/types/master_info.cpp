#include <iostream>
#include "types.h"

using namespace mesos;

MasterInfoPtr toMasterInfo(char* infoID,
			   int infoIDLen,
			   unsigned int infoIP,
			   unsigned int* infoPort,
			   char* pid,
			   int pidLen,
			   char* hostname,
         int hostnameLen,
         char* version,
         int versionLen)
{
  MasterInfoPtr masterInfo = new MasterInfo();
  masterInfo->set_id(infoID, infoIDLen);
  masterInfo->set_ip(infoIP);
  if (infoPort != NULL)
    masterInfo->set_port(*infoPort);

  if (pid != NULL)
    masterInfo->set_pid(pid, pidLen);

  if (hostname != NULL)
    masterInfo->set_hostname(hostname, hostnameLen);

  if (version != NULL)
    masterInfo->set_version(version, versionLen);

  return masterInfo;
}

void fromMasterInfo(MasterInfoPtr info,
		    char** infoID,
		    int* infoIDLen,
		    unsigned int* infoIP,
		    unsigned int* infoPort,
		    char** pid,
		    int* pidLen,
		    char** hostname,
        int* hostnameLen,
        char** version,
        int* versionLen)
{
  std::string* i = info->mutable_id();
  *infoID = (char*) i->data();
  *infoIDLen = i->size();
  *infoIP = info->ip();
  *infoPort = info->port();

  if (info->has_pid())
    {
      std::string* p = info->mutable_pid();
      *pid = (char*) p->data();
      *pidLen = p->size();
    }

  if (info->has_hostname())
    {
      std::string* h = info->mutable_hostname();
      *hostname = (char*) h->data();
      *hostnameLen = h->size();
    }

  if (info->has_version())
    {
      std::string* v = info->mutable_version();
      *version = (char*) v->data();
      *versionLen = v->size();
    }
}

void destroyMasterInfo(MasterInfoPtr info)
{
  delete info;
}
