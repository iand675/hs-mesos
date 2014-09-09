#include <iostream>
#include "types.h"

using namespace mesos;

FiltersPtr toFilters(double* refuseSeconds)
{
  FiltersPtr filters = new Filters();
  if (refuseSeconds != NULL)
    filters->set_refuse_seconds(*refuseSeconds);
  return filters;
}

void fromFilters(FiltersPtr filters,
		 bool* refusalSet,
		 double* refuseSeconds)
{
  *refusalSet = false;
  if (filters->has_refuse_seconds())
    {
      *refusalSet = true;
      *refuseSeconds = filters->refuse_seconds();
    }
}

void destroyFilters(FiltersPtr filters)
{
  delete filters;
}
