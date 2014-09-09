#include <iostream>
#include "types.h"

using namespace mesos;

AttributePtr toAttribute(char* name,
			 int nameLen,
			 ValuePtr value)
{
  AttributePtr attribute = new Attribute();
  attribute->set_name(name, nameLen);
  attribute->set_type(value->type());
  if (value->has_scalar())
    *attribute->mutable_scalar() = *value->mutable_scalar();
  if (value->has_ranges())
    *attribute->mutable_ranges() = *value->mutable_ranges();
  if (value->has_set())
    *attribute->mutable_set() = *value->mutable_set();
  if (value->has_text())
    *attribute->mutable_text() = *value->mutable_text();
  return attribute;
}

void fromAttribute(AttributePtr attribute,
		   char** name,
		   int* nameLen,
		   ValuePtr* vp)
{
  std::string* n = attribute->mutable_name();
  *name = (char*) n->data();
  *nameLen = n->size();

  ValuePtr value = new Value();
  value->set_type(attribute->type());
  if (attribute->has_scalar())
    *value->mutable_scalar() = *attribute->mutable_scalar();
  if (attribute->has_ranges())
    *value->mutable_ranges() = *attribute->mutable_ranges();
  if (attribute->has_set())
    *value->mutable_set() = *attribute->mutable_set();
  if (attribute->has_text())
    *value->mutable_text() = *attribute->mutable_text();

  *vp = value;
}

void destroyAttribute(AttributePtr attribute)
{
  delete attribute;
}
