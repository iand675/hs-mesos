#include <iostream>
#include "types.h"

using namespace mesos;

TrafficControlStatisticsPtr toTrafficControlStatistics(char* id,
                                                       int idLen,
                                                       unsigned long* backlog,
                                                       unsigned long* bytes,
                                                       unsigned long* drops,
                                                       unsigned long* overlimits,
                                                       unsigned long* packets,
                                                       unsigned long* qlen,
                                                       unsigned long* ratebps,
                                                       unsigned long* ratepps,
                                                       unsigned long* requeues)
{
  TrafficControlStatisticsPtr tcs = new TrafficControlStatistics();

  tcs->set_id(id, idLen);

  if (backlog != NULL)
    tcs->set_backlog(*backlog);

  if (bytes != NULL)
    tcs->set_bytes(*bytes);

  if (drops != NULL)
    tcs->set_drops(*drops);

  if (overlimits != NULL)
    tcs->set_overlimits(*overlimits);

  if (packets != NULL)
    tcs->set_packets(*packets);

  if (qlen != NULL)
    tcs->set_qlen(*qlen);

  if (ratebps != NULL)
    tcs->set_ratebps(*ratebps);

  if (ratepps != NULL)
    tcs->set_ratepps(*ratepps);

  if (requeues != NULL)
    tcs->set_requeues(*requeues);

  return tcs;
}

void fromTrafficControlStatistics(TrafficControlStatisticsPtr tcs,
                                  char** id,
                                  int* idLen,
                                  unsigned long* backlog,
                                  bool* backlogSet,
                                  unsigned long* bytes,
                                  bool* bytesSet,
                                  unsigned long* drops,
                                  bool* dropsSet,
                                  unsigned long* overlimits,
                                  bool* overlimitsSet,
                                  unsigned long* packets,
                                  bool* packetsSet,
                                  unsigned long* qlen,
                                  bool* qlenSet,
                                  unsigned long* ratebps,
                                  bool* ratebpsSet,
                                  unsigned long* ratepps,
                                  bool* rateppsSet,
                                  unsigned long* requeues,
                                  bool* requeuesSet)
{
  std::string *i = tcs->mutable_id();
  *id = (char*) i->data();
  *idLen = i->size();

  *backlogSet = false;
  *bytesSet = false;
  *dropsSet = false;
  *overlimitsSet = false;
  *packetsSet = false;
  *qlenSet = false;
  *ratebpsSet = false;
  *rateppsSet = false;
  *requeuesSet = false;

  if (tcs->has_backlog())
    {
      *backlog = tcs->backlog();
      *backlogSet = true;
    }
  if (tcs->has_bytes())
    {
      *bytes = tcs->bytes();
      *bytesSet = true;
    }

  if (tcs->has_drops())
    {
      *drops = tcs->drops();
      *dropsSet = true;
    }

  if (tcs->has_overlimits())
    {
      *overlimits = tcs->overlimits();
      *overlimitsSet = true;
    }

  if (tcs->has_packets())
    {
      *packets = tcs->packets();
      *packetsSet = true;
    }

  if (tcs->has_qlen())
    {
      *qlen = tcs->qlen();
      *qlenSet = true;
    }

  if (tcs->has_ratebps())
    {
      *ratebps = tcs->ratebps();
      *ratebpsSet = true;
    }

  if (tcs->has_ratepps())
    {
      *ratepps = tcs->ratepps();
      *rateppsSet = true;
    }

  if (tcs->has_requeues())
    {
      *requeues = tcs->requeues();
      *requeuesSet = true;
    }
}

void destroyTrafficControlStatistics(TrafficControlStatisticsPtr tcs)
{
  delete tcs;
}
