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
		this->errorC = errorCb;
	}

	void registered(ExecutorDriver* driver,
			const ExecutorInfo& executorInfo,
			const FrameworkInfo& frameworkInfo,
			const SlaveInfo& slaveInfo)
	{
		config->onExecutorRegistered(driver, &executorInfo, &frameworkInfo, &slaveInfo);
	}

	void reregistered(ExecutorDriver* driver,
			const SlaveInfo& slaveInfo)
	{
		config->onExecutorReRegistered(driver, &slaveInfo);
	}

	void disconnected(ExecutorDriver* driver)
	{
		config->onExecutorDisconnected(driver);
	}

	void launchTask(ExecutorDriver* driver,
			const TaskInfo& taskInfo)
	{
		config->onLaunchTask(driver, &taskInfo);
	}

	void killTask(ExecutorDriver* driver,
			const TaskID& taskId)
	{
		config->onTaskKilled(driver, &taskId);
	}

	void frameworkMessage(ExecutorDriver* driver,
			const std::string& str)
	{
		config->onFrameworkMessage(driver, str.size(), str.data());
	}

	void shutdown(ExecutorDriver* driver)
	{
		config->onShutdown(driver);
	}

	void error(ExecutorDriver* driver,
			const std::string& str)
	{
		config->onError(driver, str.size(), str.data());
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

/*
// ExecutorDriver
ExecutorDriver* createExecutorDriver(Executor* executor);
void deleteExecutorDriver(ExecutorDriver* driver)
{
	delete driver;
}

Status* startExecutorDriver(ExecutorDriver* driver)
{
	driver->start();
}

Status* stopExecutorDriver(ExecutorDriver* driver)
{
	driver->stop();
}

Status* abortExecutorDriver(ExecutorDriver* driver)
{
	driver->abort();
}

Status* joinExecutorDriver(ExecutorDriver* driver)
{
	driver->join();
}

Status* runExecutorDriver(ExecutorDriver* driver)
{
	driver->run();
}

Status* sendStatusUpdate(ExecutorDriver* driver, TaskStatus* status)
{
	driver->sendStatusUpdate(status);
}

Status* sendFrameworkMessage(ExecutorDriver* driver, int stringLength, char* stringData)
{
	std::string message(stringData, stringLength);
	driver->sendFrameworkMessage(message);
}
*/
