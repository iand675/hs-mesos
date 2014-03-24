#include <mesos/mesos.hpp>
#include <mesos/executor.hpp>
#include <mesos/resources.hpp>
#include <mesos/scheduler.hpp>
#include <mesos/values.hpp>


// Resource

// Scheduler
CreateScheduler
DeleteScheduler
OnSchedulerRegistered
OnSchedulerReRegistered
OnSchedulerDisconnected
OnResourceOffers
OnOfferRescinded
OnStatusUpdate
OnFrameworkMessage
OnSlaveLost
OnExecutorLost
OnError

// SchedulerDriver
CreateSchedulerDriver
DestroySchedulerDriver
StartSchedulerDriver
StopSchedulerDriver
AbortSchedulerDriver
JoinSchedulerDriver
RunSchedulerDriver
RequestResources
LaunchTasks
KillTask
DeclineOffer
ReviveOffer
SendFrameworkMessage
ReconcileTasks
