#include <iostream>
#include "types.h"

using namespace mesos;

CommandInfoPtr toCommandInfo(CommandInfo_URIPtr* uris,
			     int urisLen,
			     EnvironmentPtr environment,
			     bool shell,
			     char* value,
			     int valueLen,
                             StdStringPtr* args,
			     int argsLen,
			     char* username,
			     int usernameLen
			     )
{
  CommandInfoPtr info = new CommandInfo();

  for (int i = 0; i < urisLen; ++i)
    *info->add_uris() = *uris[i];

  if (environment != NULL)
    *info->mutable_environment() = *environment;

  info->set_shell(shell);

  info->set_value(value, valueLen);

  for (int i = 0; i < argsLen; ++i)
    *info->add_arguments() = *args[i];

  if (username != NULL)
    info->set_user(username, usernameLen);

  return info;
}

void fromCommandInfo(CommandInfoPtr info,
		     CommandInfo_URIPtr** uris,
		     int* urisLen,
		     EnvironmentPtr* environment,
		     bool* shell,
		     char** value,
		     int* valueLen,
		     StdStringPtr** args,
		     int* argsLen,
		     char** username,
		     int* usernameLen)
{

  *uris = info->mutable_uris()->mutable_data();
  *urisLen = info->uris_size();

  if (info->has_environment())
    *environment = info->mutable_environment();

  *value = (char*) info->mutable_value()->data();
  *valueLen = info->mutable_value()->size();

  *shell = info->shell();
  if (!*shell)
    {
      *args = info->mutable_arguments()->mutable_data();
      *argsLen = info->arguments_size();
    }

  if (info->has_user())
    {
      *username = (char*) info->mutable_user()->data();
      *usernameLen = info->mutable_user()->size();
    }
}

void destroyCommandInfo(CommandInfoPtr info)
{
  delete info;
}
