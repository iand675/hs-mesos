#include <iostream>
#include "types.h"

using namespace mesos;

VolumePtr toVolume (char* containerPath,
		    int containerPathLen,
		    char* hostPath,
		    int hostPathLen,
		    int mode)
{
  VolumePtr p = new Volume;
  p->set_mode((Volume_Mode) mode);
  p->set_container_path(containerPath, containerPathLen);
  if (hostPath != NULL)
    {
      p->set_host_path(hostPath, hostPathLen);
    }

  return p;
}

void fromVolume (VolumePtr p,
		 char** containerPath,
		 int* containerPathLen,
		 char** hostPath,
		 int* hostPathLen,
		 int* mode)
{
  *containerPath = (char*) p->mutable_container_path()->data();
  *containerPathLen = p->mutable_container_path()->size();

  *hostPath = NULL;

  if (p->has_host_path())
    {
      *hostPath = (char*) p->mutable_host_path()->data();
      *hostPathLen = p->mutable_host_path()->size();
    }

  *mode = p->mode();
}

void destroyVolume(VolumePtr volume)
{
  delete volume;
}
