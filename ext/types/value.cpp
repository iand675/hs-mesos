#include <iostream>
#include "types.h"

using namespace mesos;

ValuePtr toValue(int type,
		 double scalar,
		 ValueRangePtr* ranges,
		 int rangeLen,
		 StdStringPtr* strings,
		 int stringsLen,
		 char* text,
		 int textLen)
{
  ValuePtr value = new Value();
  value->set_type((Value_Type) type);
  if (type == Value_Type_SCALAR)
    {
      Value_Scalar s;
      s.set_value(scalar);
      *value->mutable_scalar() = s;
    }
  else if (type == Value_Type_RANGES)
    {
      Value_RangesPtr rs = value->mutable_ranges();
      for (int i = 0; i < rangeLen; ++i)
	*rs->add_range() = *ranges[i];
    }
  else if (type == Value_Type_SET)
    {
      Value_SetPtr set = value->mutable_set();
      for (int i = 0; i < stringsLen; ++i)
	set->add_item(*strings[i]);
    }
  else if (type == Value_Type_TEXT)
    {
      Value_Text t;
      t.set_value(text, textLen);
      *value->mutable_text() = t;
    }

  return value;
}

void fromValue(ValuePtr value,
	       int* type,
	       double* scalar,
	       ValueRangePtr** ranges,	
	       int* rangeLen,
	       StdStringPtr** strings,
	       int* stringsLen,
	       char** text,
	       int* textLen)
{
  Value_Type t = value->type();
  *type = (int) t;
  if (t == Value_Type_SCALAR)
    {
      *scalar = value->mutable_scalar()->value();
    }
  else if (t == Value_Type_RANGES)
    {
      *rangeLen = value->mutable_ranges()->range_size();
      *ranges = value->mutable_ranges()->mutable_range()->mutable_data();
    }
  else if (t == Value_Type_SET)
    {
      *stringsLen = value->set().item_size();
      *strings = value->mutable_set()->mutable_item()->mutable_data();
    }
  else if (t == Value_Type_TEXT)
    {
      std::string* txt = value->mutable_text()->mutable_value();
      *text = (char*) txt->data();
      *textLen = txt->size();
    }
}

void destroyValue(ValuePtr value)
{
  delete value;
}
