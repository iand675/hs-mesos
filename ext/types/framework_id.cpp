#include <iostream>
#include "types.h"

using namespace mesos;

FrameworkIDPtr toFrameworkID(char* bs, int len)
{
  FrameworkIDPtr val = new FrameworkID();
  val->set_value(bs, len);
  return val;
}

int fromFrameworkID(FrameworkIDPtr p, char** poke)
{
  *poke = (char*) p->mutable_value()->data();
  return p->mutable_value()->size();
}

void destroyFrameworkID(FrameworkIDPtr p)
{
  delete p;
}
