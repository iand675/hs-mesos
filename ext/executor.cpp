#include "executor.h"

using namespace mesos;
namespace HsFFI {
class HsExecutor : public Executor
{
public:
	HsExecutor(
		OnExecutorRegisteredCallback* registeredCb,
		OnExecutorReRegisteredCallback* reRegisteredCb,
		OnExecutorDisconnectedCallback* disconnectedCb,
		OnExecutorLaunchTaskCallback* launchTaskCb,
		OnExecutorTaskKilledCallback* taskKilledCb,
		OnExecutorFrameworkMessageCallback* frameworkMessageCb,
		OnExecutorShutdownCallback* shutdownCb,
		OnExecutorErrorCallback* errorCb
	)
	{
		this->registeredCb = registeredCb;
		this->reRegisteredCb = reRegisteredCb;
		this->disconnectedCb = disconnectedCb;
		this->launchTaskCb = launchTaskCb;
		this->taskKilledCb = taskKilledCb;
		this->frameworkMessageCb = frameworkMessageCb;
		this->shutdownCb = shutdownCb;
		this->errorCb = errorCb;
	}

	void registered(ExecutorDriver* driver,
			const ExecutorInfo& executorInfo,
			const FrameworkInfo& frameworkInfo,
			const SlaveInfo& slaveInfo)
	{
		this->registeredCb(driver, &executorInfo, &frameworkInfo, &slaveInfo);
	}

	void reregistered(ExecutorDriver* driver,
			const SlaveInfo& slaveInfo)
	{
		this->reRegisteredCb(driver, &slaveInfo);
	}

	void disconnected(ExecutorDriver* driver)
	{
		this->disconnectedCb(driver);
	}

	void launchTask(ExecutorDriver* driver,
			const TaskInfo& taskInfo)
	{
		this->launchTaskCb(driver, &taskInfo);
	}

	void killTask(ExecutorDriver* driver,
			const TaskID& taskId)
	{
		this->taskKilledCb(driver, &taskId);
	}

	void frameworkMessage(ExecutorDriver* driver,
			const std::string& str)
	{
		this->frameworkMessageCb(driver, str.size(), str.data());
	}

	void shutdown(ExecutorDriver* driver)
	{
		this->shutdownCb(driver);
	}

	void error(ExecutorDriver* driver,
			const std::string& str)
	{
		this->errorCb(driver, str.size(), str.data());
	}
private:
	OnExecutorRegisteredCallback* registeredCb;
	OnExecutorReRegisteredCallback* reRegisteredCb;
	OnExecutorDisconnectedCallback* disconnectedCb;
	OnExecutorLaunchTaskCallback* launchTaskCb;
	OnExecutorTaskKilledCallback* taskKilledCb;
	OnExecutorFrameworkMessageCallback* frameworkMessageCb;
	OnExecutorShutdownCallback* shutdownCb;
	OnExecutorErrorCallback* errorCb;
};
}

// Executor
Executor* createExecutor(
		OnExecutorRegisteredCallback* registeredCb,
		OnExecutorReRegisteredCallback* reRegisteredCb,
		OnExecutorDisconnectedCallback* disconnectedCb,
		OnExecutorLaunchTaskCallback* launchTaskCb,
		OnExecutorTaskKilledCallback* taskKilledCb,
		OnExecutorFrameworkMessageCallback* frameworkMessageCb,
		OnExecutorShutdownCallback* shutdownCb,
		OnExecutorErrorCallback* errorCb
	)
{
	return new HsFFI::HsExecutor(registeredCb, reRegisteredCb, disconnectedCb, launchTaskCb, taskKilledCb, frameworkMessageCb, shutdownCb, errorCb);
}

void destroyExecutor(mesos::Executor* executor)
{
	delete executor;
}

// ExecutorDriver
ExecutorDriver* createExecutorDriver(Executor* executor)
{
	return new MesosExecutorDriver(executor);
}

void destroyExecutorDriver(ExecutorDriver* driver)
{
	delete driver;
}

int startExecutorDriver(ExecutorDriver* driver)
{
	return driver->start();
}

int stopExecutorDriver(ExecutorDriver* driver)
{
	return driver->stop();
}

int abortExecutorDriver(ExecutorDriver* driver)
{
	return driver->abort();
}

int joinExecutorDriver(ExecutorDriver* driver)
{
	return driver->join();
}

int runExecutorDriver(ExecutorDriver* driver)
{
	return driver->run();
}

int sendExecutorDriverStatusUpdate(ExecutorDriver* driver, TaskStatus* status)
{
	return driver->sendStatusUpdate(*status);
}

int sendExecutorDriverFrameworkMessage(ExecutorDriver* driver, char* stringData, int stringLength)
{
	std::string message(stringData, stringLength);
	return driver->sendFrameworkMessage(message);
}

