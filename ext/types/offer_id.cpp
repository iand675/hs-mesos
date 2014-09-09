#include <iostream>
#include "types.h"

using namespace mesos;

OfferIDPtr toOfferID(char* bs, int len)
{
  OfferIDPtr val = new OfferID();
  val->set_value(bs, len);
  return val;
}

int fromOfferID(OfferIDPtr p, char** poke)
{
  *poke = (char*) p->mutable_value()->data();
  return p->value().size();
}

void destroyOfferID(OfferIDPtr p)
{
  delete p;
}
