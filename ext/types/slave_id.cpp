#include <iostream>
#include "types.h"

using namespace mesos;

SlaveIDPtr toSlaveID(char* bs, int len)
{
  SlaveIDPtr val = new SlaveID();
  val->set_value(bs, len);
  return val;
}

int fromSlaveID(SlaveIDPtr p, char** poke)
{
  *poke = (char*) p->mutable_value()->data();
  return p->mutable_value()->size();
}

void destroySlaveID(SlaveIDPtr p)
{
  delete p;
}
