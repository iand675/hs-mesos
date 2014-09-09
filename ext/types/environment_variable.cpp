#include <iostream>
#include "types.h"

using namespace mesos;

EnvironmentVariablePtr toEnvironmentVariable(char* key,
					     int keyLen,
					     char* value,
					     int valueLen)
{
  EnvironmentVariablePtr var = new Environment_Variable();
  var->set_name(key, keyLen);
  var->set_value(value, valueLen);
  return var;
}

void fromEnvironmentVariable(EnvironmentVariablePtr env,
			     char** key,
			     int* keyLen,
			     char** value,
			     int* valueLen)
{
  std::string* k = env->mutable_name();
  std::string* v = env->mutable_value();
  *key = (char*) k->data();
  *keyLen = k->size();
  *value = (char*) v->data();
  *valueLen = v->size();
};

void destroyEnvironmentVariable(EnvironmentVariablePtr env)
{
  delete env;
}
