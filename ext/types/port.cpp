#include <iostream>
#include "types.h"

using namespace mesos;

PortPtr toPort(int number,
               char* name,
               int nameLen,
               char* protocol,
               int protocolLen)
{
  PortPtr port = new Port();

  port->set_number(number);

  if (name != NULL)
    port->set_name(name, nameLen);

  if (protocol != NULL)
    port->set_protocol(protocol, protocolLen);

  return port;
}

void fromPort(PortPtr port,
              int* number,
              char** name,
              int* nameLen,
              char** protocol,
              int* protocolLen)
{
  *number = port->number();

  if (port->has_name())
    {
      std::string* n = port->mutable_name();
      *name = (char*) n->data();
      *nameLen = n->size();
    }

  if (port->has_protocol())
    {
      std::string* p = port->mutable_protocol();
      *protocol = (char*) p->data();
      *protocolLen = p->size();
    }

}

void destroyPort(PortPtr l)
{
  delete l;
}
