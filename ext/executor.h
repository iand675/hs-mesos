#include <mesos/executor.hpp>

typedef void OnExecutorRegisteredCallback(mesos::ExecutorDriver*,
		const mesos::ExecutorInfo*,
		const mesos::FrameworkInfo*,
		const mesos::SlaveInfo*);

typedef void OnExecutorReRegisteredCallback(mesos::ExecutorDriver*,
		const mesos::SlaveInfo*);
typedef void OnExecutorDisconnectedCallback(mesos::ExecutorDriver*);
typedef void OnExecutorLaunchTaskCallback(mesos::ExecutorDriver*,
		const mesos::TaskInfo*);
typedef void OnExecutorTaskKilledCallback(mesos::ExecutorDriver*,
		const mesos::TaskID*);
typedef void OnExecutorFrameworkMessageCallback(mesos::ExecutorDriver*, int, const char*);
typedef void OnExecutorShutdownCallback(mesos::ExecutorDriver*);
typedef void OnExecutorErrorCallback(mesos::ExecutorDriver*, int, const char*);

typedef mesos::Executor *ExecutorPtr;
typedef mesos::ExecutorDriver *ExecutorDriverPtr;
extern "C" {

extern ExecutorPtr createExecutor(
	OnExecutorRegisteredCallback*,
	OnExecutorReRegisteredCallback*,
	OnExecutorDisconnectedCallback*,
	OnExecutorLaunchTaskCallback*,
	OnExecutorTaskKilledCallback*,
	OnExecutorFrameworkMessageCallback*,
	OnExecutorShutdownCallback*,
	OnExecutorErrorCallback*
);

extern void destroyExecutor(ExecutorPtr);

}
