#include <iostream>
#include "types.h"

using namespace mesos;

FrameworkInfoPtr toFrameworkInfo(char* user,
				 int userLen,
				 char* name,
				 int nameLen,
				 FrameworkIDPtr* frameworkID,
				 double* failoverTimeout,
				 bool* checkpoint,
				 char* role,
				 int roleLen,
				 char* hostname,
				 int hostLen,
				 char* principal,
				 int principalLen)
//	char* hostname,
// int hostLen)
{
  FrameworkInfoPtr info = new FrameworkInfo();
  info->set_user(user, userLen);
  info->set_name(name, nameLen);
  if (frameworkID != NULL)
    info->mutable_id()->MergeFrom(**frameworkID);
  if (failoverTimeout != NULL)
    info->set_failover_timeout(*failoverTimeout);
  if (checkpoint != NULL)
    {
      info->set_checkpoint(*checkpoint);
    }
  if (role != NULL)
    info->set_role(role, roleLen);

  if (hostname != NULL)
    info->set_hostname(hostname, hostLen);

  if (principal != NULL)
    info->set_principal(principal, principalLen);

  return info;
}

void fromFrameworkInfo(FrameworkInfoPtr info,
		       char** user,
		       int* userLen,
		       char** name,
		       int* nameLen,
		       FrameworkIDPtr* frameworkID,
		       bool* failoverSet,
		       double* failoverTimeout,
		       bool* checkpointSet,
		       bool* checkpoint,
		       char** role,
		       int* roleLen,
		       char** hostname,
		       int* hostLen,
		       char** principal,
		       int* principalLen)
{
  *failoverSet = false;
  *checkpointSet = false;

  std::string* u = info->mutable_user();
  *user = (char*) u->data();
  *userLen = u->size();
  std::string* n = info->mutable_name();
  *name = (char*) n->data();
  *nameLen = n->size();
	
  if (info->has_failover_timeout())
    {
      double timeout = info->failover_timeout();
      *failoverTimeout = timeout;
      *failoverSet = true;
    }
  if (info->has_checkpoint())
    {
      bool cp = info->checkpoint();
      *checkpoint = cp;
      *checkpointSet = true;
    }
  if (info->has_id())
    {
      *frameworkID = info->mutable_id();
    }
  if (info->has_role())
    {
      std::string* r = info->mutable_role();
      *role = (char*) r->data();
      *roleLen = r->size();
    }
  
  if (info->has_hostname())
    {
      std::string* h = info->mutable_hostname();
      *hostname = (char*) h->data();
      *hostLen = h->size();
    }

  if (info->has_principal())
    {
      std::string* p = info->mutable_principal();
      *principal = (char*) p->data();
      *principalLen = p->size();
    }
}

void destroyFrameworkInfo(FrameworkInfoPtr info)
{
  delete info;
}
