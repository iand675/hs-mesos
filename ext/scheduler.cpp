#include <iostream>
#include "scheduler.h"

using namespace mesos;
namespace HsFFI {

class HsScheduler : public Scheduler
{
public:
	HsScheduler(
		OnSchedulerRegisteredCallback* registeredCb,
		OnSchedulerReRegisteredCallback* reRegisteredCb,
		OnSchedulerDisconnectedCallback* disconnectedCb,
		OnSchedulerResourceOffers* resourceOffersCb,
		OnOfferRescinded* offerRescindedCb,
		OnStatusUpdate* statusUpdateCb,
		OnFrameworkMessage* frameworkMessageCb,
		OnSlaveLost* slaveLostCb,
		OnExecutorLost* executorLostCb,
		OnSchedulerError* schedulerErrorCb)
	{
		this->registeredCb = registeredCb;
		this->reRegisteredCb = reRegisteredCb;
		this->disconnectedCb = disconnectedCb;
		this->resourceOffersCb = resourceOffersCb;
		this->offerRescindedCb = offerRescindedCb;
		this->statusUpdateCb = statusUpdateCb;
		this->frameworkMessageCb = frameworkMessageCb;
		this->slaveLostCb = slaveLostCb;
		this->executorLostCb = executorLostCb;
		this->schedulerErrorCb = schedulerErrorCb;
	}

	~HsScheduler(){}

	virtual void registered (SchedulerDriver* driver,
			const FrameworkID& frameworkId,
			const MasterInfo& masterInfo)
	{
		registeredCb(driver, &frameworkId, &masterInfo);
	}

	virtual void reregistered (SchedulerDriver* driver,
			const MasterInfo& masterInfo)
	{
		reRegisteredCb(driver, &masterInfo);
	}

	virtual void disconnected (SchedulerDriver* driver)
	{
		disconnectedCb(driver);
	}

	virtual void resourceOffers (SchedulerDriver* driver,
			const std::vector<Offer>& offers)
	{
		std::vector<Offer*> pointerized = std::vector<Offer*>(offers.size());
		for (int i = 0; i < offers.size(); ++i)
			pointerized[i] = (Offer*) &offers[i];

		resourceOffersCb(driver, pointerized.data(), pointerized.size());
	}

	virtual void offerRescinded (SchedulerDriver* driver,
			const OfferID& offerId)
	{
		offerRescindedCb(driver, &offerId);
	}

	virtual void statusUpdate (SchedulerDriver* driver,
			const TaskStatus& status)
	{
		statusUpdateCb(driver, &status);
	}

	virtual void frameworkMessage (SchedulerDriver* driver,
			const ExecutorID& executorId,
			const SlaveID& slaveId,
			const std::string& data)
	{
		frameworkMessageCb(driver, &executorId, &slaveId, data.data(), data.size());
	}

	virtual void slaveLost (SchedulerDriver* driver,
			const SlaveID& slaveId)
	{
		slaveLostCb(driver, &slaveId);
	}

	virtual void executorLost (SchedulerDriver* driver,
			const ExecutorID& executorId,
			const SlaveID& slaveId,
			int status)
	{
		executorLostCb(driver, &executorId, &slaveId, status);
	}

	virtual void error (SchedulerDriver* driver, const std::string& message)
	{
		schedulerErrorCb(driver, message.data(), message.size());
	}

private:
	OnSchedulerRegisteredCallback* registeredCb;
	OnSchedulerReRegisteredCallback* reRegisteredCb;
	OnSchedulerDisconnectedCallback* disconnectedCb;
	OnSchedulerResourceOffers* resourceOffersCb;
	OnOfferRescinded* offerRescindedCb;
	OnStatusUpdate* statusUpdateCb;
	OnFrameworkMessage* frameworkMessageCb;
	OnSlaveLost* slaveLostCb;
	OnExecutorLost* executorLostCb;
	OnSchedulerError* schedulerErrorCb;
};
}

SchedulerPtr createScheduler(OnSchedulerRegisteredCallback* registeredCb,
	OnSchedulerReRegisteredCallback* reRegisteredCb,
	OnSchedulerDisconnectedCallback* disconnectedCb,
	OnSchedulerResourceOffers* resourceOffersCb,
	OnOfferRescinded* offerRescindedCb,
	OnStatusUpdate* statusUpdateCb,
	OnFrameworkMessage* frameworkMessageCb,
	OnSlaveLost* slaveLostCb,
	OnExecutorLost* executorLostCb,
	OnSchedulerError* schedulerErrorCb)
{
	return new HsFFI::HsScheduler(registeredCb,
			reRegisteredCb,
			disconnectedCb,
			resourceOffersCb,
			offerRescindedCb,
			statusUpdateCb,
			frameworkMessageCb,
			slaveLostCb,
			executorLostCb,
			schedulerErrorCb
	);
}

void destroyScheduler(SchedulerPtr scheduler)
{
	delete scheduler;
}

void exerciseMethods(SchedulerPtr scheduler)
{	
	scheduler->registered(NULL, FrameworkID(), MasterInfo());
	scheduler->reregistered(NULL, MasterInfo());
	scheduler->disconnected(NULL);
	scheduler->resourceOffers(NULL, std::vector<Offer>());
	scheduler->offerRescinded(NULL, OfferID());
	scheduler->statusUpdate(NULL, TaskStatus());
	scheduler->frameworkMessage(NULL, ExecutorID(), SlaveID(), std::string());
	scheduler->slaveLost(NULL, SlaveID());
	scheduler->executorLost(NULL, ExecutorID(), SlaveID(), 0);
	scheduler->error(NULL, std::string());
}

SchedulerDriverPtr createSchedulerDriver(SchedulerPtr scheduler, FrameworkInfoPtr framework, char* master, int masterLength)
{
	return new MesosSchedulerDriver(scheduler, *framework, std::string(master, masterLength));
}

SchedulerDriverPtr createSchedulerDriverWithCredentials(SchedulerPtr scheduler, FrameworkInfoPtr framework, char* master, int masterLength, CredentialPtr credential)
{
	return new MesosSchedulerDriver(scheduler, *framework, std::string(master, masterLength), *credential);
}

void destroySchedulerDriver(SchedulerDriverPtr schedulerDriver)
{
	delete schedulerDriver;
}

int startSchedulerDriver(SchedulerDriverPtr schedulerDriver)
{
	return schedulerDriver->start();
}

int stopSchedulerDriver(SchedulerDriverPtr schedulerDriver, bool failover)
{
	return schedulerDriver->stop(failover);	
}

int abortSchedulerDriver(SchedulerDriverPtr schedulerDriver)
{
	return schedulerDriver->abort();
}

int joinSchedulerDriver(SchedulerDriverPtr schedulerDriver)
{
	return schedulerDriver->join();
}

int runSchedulerDriver(SchedulerDriverPtr schedulerDriver)
{
	return schedulerDriver->run();
}

int requestResources(SchedulerDriverPtr schedulerDriver, RequestPtr* request, int requestCount)
{
	std::vector<Request> requests = std::vector<Request>(requestCount);
	for(int i = 0; i < requestCount; ++i)
		requests[i] = *request[i];

	return schedulerDriver->requestResources(requests);
}

int launchTasks(SchedulerDriverPtr schedulerDriver, OfferIDPtr* offer, int offerCount, TaskInfoPtr* taskInfo, int infoCount, FiltersPtr filters)
{
	std::vector<OfferID> offers = std::vector<OfferID>(offerCount);
	for(int i = 0; i < offerCount; ++i)
		offers[i] = *offer[i];

	std::vector<TaskInfo> tasks = std::vector<TaskInfo>(infoCount);
	for(int i = 0; i < infoCount; ++i)
		tasks[i] = *taskInfo[i];

	return schedulerDriver->launchTasks(offers, tasks, *filters);
}

int killTask(SchedulerDriverPtr schedulerDriver, TaskIDPtr task)
{
	return schedulerDriver->killTask(*task);
}

int declineOffer(SchedulerDriverPtr schedulerDriver, OfferIDPtr offerId, FiltersPtr filters)
{
	return schedulerDriver->declineOffer(*offerId, *filters);
}

int reviveOffers(SchedulerDriverPtr schedulerDriver)
{
	return schedulerDriver->reviveOffers();
}

int schedulerDriverSendFrameworkMessage(SchedulerDriverPtr schedulerDriver, ExecutorIDPtr executor, SlaveIDPtr slave, char* msg, int msgLength)
{
	return schedulerDriver->sendFrameworkMessage(*executor, *slave, std::string(msg, msgLength));
}

int reconcileTasks(SchedulerDriverPtr schedulerDriver, TaskStatusPtr* statuses, int statusCount)
{
	std::vector<TaskStatus> tasks = std::vector<TaskStatus>(statusCount);
	for(int i = 0; i < statusCount; ++i)
		tasks[i] = *statuses[i];

	return schedulerDriver->reconcileTasks(tasks);
}
