#include <mesos/scheduler.hpp>

typedef mesos::Scheduler *SchedulerPtr;
typedef mesos::SchedulerDriver *SchedulerDriverPtr;
typedef const mesos::FrameworkID *FrameworkIDPtr;
typedef const mesos::MasterInfo *MasterInfoPtr;
typedef const mesos::Offer *OfferPtr;
typedef const mesos::OfferID *OfferIDPtr;
typedef const mesos::TaskStatus *TaskStatusPtr;
typedef const mesos::SlaveID *SlaveIDPtr;
typedef const mesos::ExecutorID *ExecutorIDPtr;
typedef mesos::FrameworkInfo *FrameworkInfoPtr;
typedef mesos::Credential *CredentialPtr;
typedef mesos::Filters *FiltersPtr;
typedef mesos::Request *RequestPtr;
typedef mesos::TaskInfo *TaskInfoPtr;
typedef mesos::TaskID *TaskIDPtr;

typedef void OnSchedulerRegisteredCallback(SchedulerDriverPtr,
		const FrameworkIDPtr,
		const MasterInfoPtr);

typedef void OnSchedulerReRegisteredCallback(SchedulerDriverPtr,
		const MasterInfoPtr);

typedef void OnSchedulerDisconnectedCallback(SchedulerDriverPtr);

typedef void OnSchedulerResourceOffers(SchedulerDriverPtr,
		const OfferPtr,
		int);

typedef void OnOfferRescinded(SchedulerDriverPtr,
		const OfferIDPtr);

typedef void OnStatusUpdate(SchedulerDriverPtr,
		const TaskStatusPtr);

typedef void OnFrameworkMessage(SchedulerDriverPtr,
		const ExecutorIDPtr,
		const SlaveIDPtr,
		const char*,
		int);

typedef void OnSlaveLost(SchedulerDriverPtr,
		const SlaveIDPtr);

typedef void OnExecutorLost(SchedulerDriverPtr,
		const ExecutorIDPtr,
		const SlaveIDPtr,
		int status);

typedef void OnSchedulerError(SchedulerDriverPtr,
		const char*,
		int);

extern "C" {
	extern SchedulerPtr createScheduler(OnSchedulerRegisteredCallback*,
			OnSchedulerReRegisteredCallback*,
			OnSchedulerDisconnectedCallback*,
			OnSchedulerResourceOffers*,
			OnOfferRescinded*,
			OnStatusUpdate*,
			OnFrameworkMessage*,
			OnSlaveLost*,
			OnExecutorLost*,
			OnSchedulerError*);

	extern void destroyScheduler(SchedulerPtr);
	extern void exerciseMethods(SchedulerPtr);

	extern SchedulerDriverPtr createSchedulerDriver(
			SchedulerPtr,
			FrameworkInfoPtr,
			char*,
			int);

	extern SchedulerDriverPtr createSchedulerDriverWithCredentials(
			SchedulerPtr,
			FrameworkInfoPtr,
			char*,
			int,
			CredentialPtr);

	extern void destroySchedulerDriver(SchedulerDriverPtr);	
	extern int startSchedulerDriver(SchedulerDriverPtr);
	extern int stopSchedulerDriver(SchedulerDriverPtr, bool failover);
	extern int abortSchedulerDriver(SchedulerDriverPtr);
	extern int joinSchedulerDriver(SchedulerDriverPtr);
	extern int runSchedulerDriver(SchedulerDriverPtr);
	extern int requestResources(SchedulerDriverPtr, RequestPtr*, int);
	extern int launchTasks(SchedulerDriverPtr,
			OfferIDPtr*,
			int,
			TaskInfoPtr*,
			int,
			FiltersPtr);
	extern int killTask(SchedulerDriverPtr, TaskIDPtr);
	extern int declineOffer(SchedulerDriverPtr,
			OfferIDPtr,
			FiltersPtr);
	extern int reviveOffers(SchedulerDriverPtr);
	extern int schedulerDriverSendFrameworkMessage(SchedulerDriverPtr,
			ExecutorIDPtr,
			SlaveIDPtr,
			char*,
			int);
	extern int reconcileTasks(SchedulerDriverPtr,
			TaskStatusPtr*,
			int);
}
