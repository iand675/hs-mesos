#include <iostream>
#include "types.h"

using namespace mesos;

ParametersPtr toParameters(ParameterPtr* parameters,
			   int pLen)
{
  ParametersPtr params = new Parameters();
  for (int i = 0; i < pLen; ++i)
    *params->add_parameter() = *parameters[i];
  return params;
}

void fromParameters(ParametersPtr params,
		    ParameterPtr** parameters,
		    int* pLen)
{
  *parameters = params->mutable_parameter()->mutable_data();
  *pLen = params->parameter_size();
}

void destroyParameters(ParametersPtr params)
{
  delete params;
}
