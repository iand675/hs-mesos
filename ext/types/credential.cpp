#include <iostream>
#include "types.h"

using namespace mesos;

CredentialPtr toCredential(char* principal,
			   int principalLen,
			   char* secret,
			   int secretLen)
{
  CredentialPtr credential = new Credential();
  credential->set_principal(principal, principalLen);
  if (secret != NULL)
    credential->set_secret(secret, secretLen);
  return credential;
}

void fromCredential(
		    CredentialPtr credential,
		    char** principal,
		    int* principalLen,
		    char** secret,
		    int* secretLen)
{
  std::string* p = credential->mutable_principal();
  *principal = (char*) p->data();
  *principalLen = p->size();
  if (credential->has_secret())
    {
      std::string* s = credential->mutable_secret();
      *secret = (char*) s->data();
      *secretLen = s->size();
    }
  else
    {
      *secret = NULL;
      *secretLen = 0;
    }
}

void destroyCredential(CredentialPtr credential)
{
  delete credential;
}
