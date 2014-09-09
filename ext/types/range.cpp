#include <iostream>
#include "types.h"

using namespace mesos;

ValueRangePtr toRange(unsigned long low,
		      unsigned long high)
{
  ValueRangePtr range = new Value_Range();
  range->set_begin(low);
  range->set_end(high);
  return range;
}

void fromRange(ValueRangePtr range,
	       unsigned long* lowP,
	       unsigned long* highP)
{
  *lowP = range->begin();
  *highP = range->end();
}

void destroyRange(ValueRangePtr range)
{
  delete range;
}
