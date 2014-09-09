#include <iostream>
#include "types.h"

using namespace mesos;

StdStringPtr toStdString(char* str,
			 int strLen)
{
  return new std::string(str, strLen);
}

void fromStdString(StdStringPtr sp, char** str, int* strLen)
{
  *str = (char*) sp->data();
  *strLen = sp->size();
}

void destroyStdString(StdStringPtr sp)
{
  delete sp;
}
