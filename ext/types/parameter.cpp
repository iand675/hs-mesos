#include <iostream>
#include "types.h"

using namespace mesos;

ParameterPtr toParameter(char* keyP,
			 int keyLen,
			 char* valP,
			 int valLen)
{
  ParameterPtr parameter = new Parameter();
  parameter->set_key(keyP, keyLen);
  parameter->set_value(valP, valLen);
  return parameter;
}

void fromParameter(ParameterPtr parameter,
		   char** keyP,
		   int* keyLenP,
		   char** valueP,
		   int* valueLenP)
{
  std::string* k = parameter->mutable_key();
  std::string* v = parameter->mutable_value();
  *keyP = (char*) k->data();
  *keyLenP = k->size();
  *valueP = (char*) v->data();
  *valueLenP = v->size();
}

void destroyParameter(ParameterPtr parameter)
{
  delete parameter;
}
