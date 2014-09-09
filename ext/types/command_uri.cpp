#include <iostream>
#include "types.h"

using namespace mesos;

CommandURIPtr toCommandURI(char* cmd,
			   int cmdLen,
			   bool* executable,
			   bool* extract)
{
  CommandURIPtr uri = new mesos::CommandInfo_URI();
  uri->set_value(cmd, cmdLen);

  if (executable != NULL)
    uri->set_executable(*executable);

  if (extract != NULL)
    uri->set_extract(*extract);

  return uri;
}

void fromCommandURI(CommandURIPtr commandURI,
		    char** cmd,
		    int* cmdLen,
		    bool* executableSet,
		    bool* executable,
		    bool* extractSet,
		    bool* extract)
{
  std::string* cmdStr = commandURI->mutable_value();

  *cmd = (char*) cmdStr->data();
  *cmdLen = cmdStr->size();

  if (commandURI->has_executable())
    {
      *executableSet = true;
      *executable = commandURI->executable();
    }

  if (commandURI->has_extract())
    {
      *extractSet = true;
      *extract = commandURI->extract();
    }
}

void destroyCommandURI(CommandURIPtr commandURI)
{
  delete commandURI;
}
