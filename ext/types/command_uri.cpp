#include <iostream>
#include "types.h"

using namespace mesos;

CommandURIPtr toCommandURI(char* cmd,
			   int cmdLen,
			   bool* executable,
         bool* extract,
         bool* cache)
{
  CommandURIPtr uri = new mesos::CommandInfo_URI();
  uri->set_value(cmd, cmdLen);

  if (executable != NULL)
    uri->set_executable(*executable);

  if (extract != NULL)
    uri->set_extract(*extract);

  if (cache != NULL)
    uri->set_cache(*cache);

  return uri;
}

void fromCommandURI(CommandURIPtr commandURI,
		    char** cmd,
		    int* cmdLen,
		    bool* executableSet,
		    bool* executable,
		    bool* extractSet,
        bool* extract,
        bool* cacheSet,
        bool* cache)
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

  if (commandURI->has_cache())
    {
      *cacheSet = true;
      *cache = commandURI->cache();
    }
}

void destroyCommandURI(CommandURIPtr commandURI)
{
  delete commandURI;
}
