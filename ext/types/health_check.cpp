#include <iostream>
#include "types.h"

using namespace mesos;

HealthCheckPtr toHealthCheck (bool hasHTTP,
			      int port,
			      char* httpPath,
			      int httpPathSize,
			      unsigned int* statuses,
			      int statusesSize,
			      double* delaySeconds,
			      double* intervalSeconds,
			      double* timeoutSeconds,
			      unsigned int* consecutiveFailures,
			      double* gracePeriodSeconds,
			      CommandInfoPtr command)
{
  HealthCheckPtr hc = new HealthCheck;
  if (hasHTTP)
    {
      HealthCheck_HTTP http;

      http.set_port(port);

      for (int i = 0; i < statusesSize; ++i)
	{
	  http.add_statuses(statuses[i]);
	}

      if (httpPath != NULL)
	http.set_path(httpPath, httpPathSize);

      *hc->mutable_http() = http;
    }
  
  if (delaySeconds != NULL)
    hc->set_delay_seconds(*delaySeconds);

  if (intervalSeconds != NULL)
    hc->set_interval_seconds(*intervalSeconds);
 
  if (timeoutSeconds != NULL)
    hc->set_timeout_seconds(*timeoutSeconds);

  if (consecutiveFailures != NULL)
    hc->set_consecutive_failures(*consecutiveFailures);

  if (gracePeriodSeconds != NULL)
    hc->set_grace_period_seconds(*gracePeriodSeconds);

  if (command != NULL)
    *hc->mutable_command() = *command;

  return hc;
}

void fromHealthCheck (HealthCheckPtr p,
		    bool* hasHTTP,
		    int* port,
		    char** httpPath,
		    int* httpPathSize,
		    unsigned int** statuses,
		    int* statusesSize,
		    double* delaySeconds,
		    bool* delaySecondsSet,
		    double* intervalSeconds,
		    bool* intervalSecondsSet,
		    double* timeoutSeconds,
		    bool* timeoutSecondsSet,
		    unsigned int* consecutiveFailures,
		    bool* consecutiveFailuresSet,
		    double* gracePeriodSeconds,
		    bool* gracePeriodSecondsSet,
		    CommandInfoPtr* command)
{
  *hasHTTP = false;
  *httpPath = NULL;
  *delaySecondsSet = false;
  *intervalSecondsSet = false;
  *timeoutSecondsSet = false;
  *consecutiveFailuresSet = false;
  *gracePeriodSecondsSet = false;

  if (p->has_http())
    {
      *hasHTTP = true;
      *port = p->mutable_http()->port();
      
      if (p->mutable_http()->has_path())
	{
	  *httpPath = (char*) p->mutable_http()->mutable_path()->data();
	  *httpPathSize = p->mutable_http()->mutable_path()->size();
	}

      *statuses = p->mutable_http()->mutable_statuses()->mutable_data();
      *statusesSize = p->mutable_http()->mutable_statuses()->size();
    }

  if (p->has_delay_seconds())
    {
      *delaySecondsSet = true;
      *delaySeconds = p->delay_seconds();
    }

  if (p->has_interval_seconds())
    {
      *intervalSecondsSet = true;
      *intervalSeconds = p->interval_seconds();
    }

  if (p->has_timeout_seconds())
    {
      *timeoutSecondsSet = true;
      *timeoutSeconds = p->timeout_seconds();
    }

  if (p->has_consecutive_failures())
    {
      *consecutiveFailuresSet = true;
      *consecutiveFailures = p->consecutive_failures();
    }

  if (p->has_grace_period_seconds())
    {
      *gracePeriodSecondsSet = true;
      *gracePeriodSeconds = p->grace_period_seconds();
    }

  if (p->has_command())
    *command = p->mutable_command();
}

void destroyHealthCheck (HealthCheckPtr p)
{
  delete p;
}
