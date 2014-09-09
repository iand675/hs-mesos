#include <iostream>
#include "types.h"

using namespace mesos;

TaskIDPtr toTaskID(char* bs, int len)
{
  TaskIDPtr val = new TaskID();
  val->set_value(bs, len);
  return val;
}

int fromTaskID(TaskIDPtr p, char** poke)
{
  *poke = (char*) p->mutable_value()->data();
  return p->mutable_value()->size();
}

void destroyTaskID(TaskIDPtr p)
{
  delete p;
}
