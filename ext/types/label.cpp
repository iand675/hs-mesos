#include <iostream>
#include "types.h"

using namespace mesos;

LabelPtr toLabel(char* key,
                 int keyLen,
                 char* value,
                 int valueLen)
{
  LabelPtr label = new Label();

  if (key != NULL)
    label->set_key(key, keyLen);

  if (value != NULL)
    label->set_value(value, valueLen);

  return label;
}

void fromLabel(LabelPtr l,
               char** key,
               int* keyLen,
               char** value,
               int* valueLen)
{
  if (l->has_key())
    {
      std::string* k = l->mutable_key();
      *key = (char*) k->data();
      *keyLen = k->size();
    }
  if (l->has_value())
    {
      std::string* v = l->mutable_value();
      *value = (char*) v->data();
      *valueLen = v->size();
    }

}

void destroyLabel(LabelPtr l)
{
  delete l;
}
