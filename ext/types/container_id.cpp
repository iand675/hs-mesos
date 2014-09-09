#include <iostream>
#include "types.h"

using namespace mesos;

ContainerIDPtr toContainerID(char* bs, int len)
{
  ContainerIDPtr val = new ContainerID();
  val->set_value(bs, len);
  return val;
}

int fromContainerID(ContainerIDPtr p, char** poke)
{
  *poke = (char*) p->mutable_value()->data();
  return p->mutable_value()->size();
}

void destroyContainerID(ContainerIDPtr p)
{
  delete p;
}

