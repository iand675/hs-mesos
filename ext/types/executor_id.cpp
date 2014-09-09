#include <iostream>
#include "types.h"

using namespace mesos;

ExecutorIDPtr toExecutorID(char* bs, int len)
{
  ExecutorIDPtr val = new ExecutorID();
  val->set_value(bs, len);
  return val;
}

int fromExecutorID(ExecutorIDPtr p, char** poke)
{
  *poke = (char*) p->mutable_value()->data();
  return p->mutable_value()->size();
}

void destroyExecutorID(ExecutorIDPtr p)
{
  delete p;
}
