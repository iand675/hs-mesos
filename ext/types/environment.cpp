#include <iostream>
#include "types.h"

using namespace mesos;

EnvironmentPtr toEnvironment(EnvironmentVariablePtr* envVars,
			     int envLen)
{
  EnvironmentPtr env = new Environment();
  if (envVars != NULL)
    {
      ::google::protobuf::RepeatedPtrField<Environment_Variable>* vs = env->mutable_variables();
      for (int i = 0; i < envLen; ++i)
	*vs->Add() = *envVars[i];
    }
  
  return env;
}

void fromEnvironment(EnvironmentPtr env,
		     EnvironmentVariablePtr** vars,
		     int* varLen)
{
  *vars = env->mutable_variables()->mutable_data();
  *varLen = env->variables_size();
}

void destroyEnvironment(EnvironmentPtr env)
{
  delete env;
}
