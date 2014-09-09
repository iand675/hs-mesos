#include <iostream>
#include "types.h"

using namespace mesos;

ContainerInfoPtr toContainerInfo (int type,
				  char* image,
				  int imageLen,
				  VolumePtr* volumes,
				  int volumesLen)
{
  ContainerInfoPtr p = new ContainerInfo;
  p->set_type((ContainerInfo_Type) type);
  if (type == ContainerInfo_Type_DOCKER)
    {
      ContainerInfo_DockerInfo d;
      d.set_image(image, imageLen);
      *p->mutable_docker() = d;
    }
  
  for (int i = 0; i < volumesLen; ++i)
    *p->add_volumes() = *volumes[i];

  return p;
}

void fromContainerInfo (ContainerInfoPtr info,
			int* type,
			char** image,
			int* imageLen,
			VolumePtr** volumes,
			int* volumesLen)
{
  *type = info->type();

  if (info->type() == 1) // DOCKER
    {
      *image = (char*) info->mutable_docker()->mutable_image()->data();
      *imageLen = info->mutable_docker()->mutable_image()->size();
    }
  
  *volumes = (VolumePtr*) info->mutable_volumes()->data();
  *volumesLen = info->mutable_volumes()->size();
}

void destroyContainerInfo (ContainerInfoPtr containerInfo)
{
  delete containerInfo;
}
