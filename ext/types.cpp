#include <iostream>
#include "types.h"

using namespace mesos;
FrameworkIDPtr toFrameworkID(char* bs, int len)
{
	FrameworkIDPtr val = new FrameworkID();
	val->set_value(bs, len);
	return val;
}

int fromFrameworkID(FrameworkIDPtr p, char** poke)
{
	*poke = (char*) p->value().data();
	return p->value().size();
}

void destroyFrameworkID(FrameworkIDPtr p)
{
	delete p;
}

OfferIDPtr toOfferID(char* bs, int len)
{
	OfferIDPtr val = new OfferID();
	val->set_value(bs, len);
	return val;
}

int fromOfferID(OfferIDPtr p, char** poke)
{
	*poke = (char*) p->value().data();
	return p->value().size();
}

void destroyOfferID(OfferIDPtr p)
{
	delete p;
}

SlaveIDPtr toSlaveID(char* bs, int len)
{
	SlaveIDPtr val = new SlaveID();
	val->set_value(bs, len);
	return val;
}

int fromSlaveID(SlaveIDPtr p, char** poke)
{
	*poke = (char*) p->value().data();
	return p->value().size();
}

void destroySlaveID(SlaveIDPtr p)
{
	delete p;
}

TaskIDPtr toTaskID(char* bs, int len)
{
	TaskIDPtr val = new TaskID();
	val->set_value(bs, len);
	return val;
}

int fromTaskID(TaskIDPtr p, char** poke)
{
	*poke = (char*) p->value().data();
	return p->value().size();
}

void destroyTaskID(TaskIDPtr p)
{
	delete p;
}

ExecutorIDPtr toExecutorID(char* bs, int len)
{
	ExecutorIDPtr val = new ExecutorID();
	val->set_value(bs, len);
	return val;
}

int fromExecutorID(ExecutorIDPtr p, char** poke)
{
	*poke = (char*) p->value().data();
	return p->value().size();
}

void destroyExecutorID(ExecutorIDPtr p)
{
	delete p;
}

ContainerIDPtr toContainerID(char* bs, int len)
{
	ContainerIDPtr val = new ContainerID();
	val->set_value(bs, len);
	return val;
}

int fromContainerID(ContainerIDPtr p, char** poke)
{
	*poke = (char*) p->value().data();
	return p->value().size();
}

void destroyContainerID(ContainerIDPtr p)
{
	delete p;
}

FrameworkInfoPtr toFrameworkInfo(char* user,
	int userLen,
	char* name,
	int nameLen,
	FrameworkIDPtr* frameworkID,
	double* failoverTimeout,
	bool* checkpoint,
	char* role,
	int roleLen,
	char* hostname,
	int hostLen)
{
	FrameworkInfoPtr info = new FrameworkInfo();
	info->set_user(user, userLen);
	info->set_name(name, nameLen);
	if (frameworkID != NULL)
		info->mutable_id()->MergeFrom(**frameworkID);
	if (failoverTimeout != NULL)
		info->set_failover_timeout(*failoverTimeout);
	if (checkpoint != NULL)
	{
		info->set_checkpoint(*checkpoint);
	}
	if (role != NULL)
		info->set_role(role, roleLen);
	if (hostname != NULL)
		info->set_hostname(hostname, hostLen);
	return info;
}

void fromFrameworkInfo(FrameworkInfoPtr info,
	char** user,
	int* userLen,
	char** name,
	int* nameLen,
	FrameworkIDPtr* frameworkID,
	bool* failoverSet,
	double* failoverTimeout,
	bool* checkpointSet,
	bool* checkpoint,
	char** role,
	int* roleLen,
	char** hostname,
	int* hostLen)
{
	*failoverSet = false;
	*checkpointSet = false;

	const std::string u = info->user();
	*user = (char*) u.data();
	*userLen = u.size();
	const std::string n = info->name();
	*name = (char*) n.data();
	*nameLen = n.size();
	
	if (info->has_failover_timeout())
	{
		double timeout = info->failover_timeout();
		*failoverTimeout = timeout;
		*failoverSet = true;
	}
	if (info->has_checkpoint())
	{
		bool cp = info->checkpoint();
		*checkpoint = cp;
		*checkpointSet = true;
	}
	if (info->has_id())
	{
		*frameworkID = info->mutable_id();
	}
	if (info->has_role())
	{
		const std::string r = info->role();
		*role = (char*) r.data();
		*roleLen = r.size();
	}
	if (info->has_hostname())
	{
		const std::string h = info->hostname();
		*hostname = (char*) h.data();
		*hostLen = h.size();
	}
}

void destroyFrameworkInfo(FrameworkInfoPtr info)
{
	delete info;
}

// **********************************************************************

CommandURIPtr toCommandURI(char* cmd,
	int cmdLen,
	bool* executable)
{
	CommandURIPtr uri = new mesos::CommandInfo_URI();
	uri->set_value(cmd, cmdLen);
	if (executable != NULL)
		uri->set_executable(*executable);
	return uri;
}

void fromCommandURI(CommandURIPtr commandURI,
	char** cmd,
	int* cmdLen,
	bool* executableSet,
	bool* executable)
{
	*executableSet = false;
	std::string cmdStr = commandURI->value();
	*cmd = (char*) cmdStr.data();
	*cmdLen = cmdStr.size();
	if (commandURI->has_executable())
	{
		*executableSet = true;
		*executable = commandURI->executable();
	}
}

void destroyCommandURI(CommandURIPtr commandURI)
{
	delete commandURI;
}

// **********************************************************************
MasterInfoPtr toMasterInfo(char* infoID,
	int infoIDLen,
	unsigned int infoIP,
	unsigned int infoPort,
	char* pid,
	int pidLen,
	char* hostname,
	int hostnameLen)
{
	MasterInfoPtr masterInfo = new MasterInfo();
	masterInfo->set_id(infoID, infoIDLen);
	masterInfo->set_ip(infoIP);
	masterInfo->set_port(infoPort);
	if (pid != NULL)
		masterInfo->set_pid(pid, pidLen);
	if (hostname != NULL)
		masterInfo->set_hostname(hostname, hostnameLen);
	return masterInfo;
}

void fromMasterInfo(MasterInfoPtr info,
	char** infoID,
	int* infoIDLen,
	unsigned int* infoIP,
	unsigned int* infoPort,
	char** pid,
	int* pidLen,
	char** hostname,
	int* hostnameLen)
{
	std::string i = info->id();
	*infoID = (char*) i.data();
	*infoIDLen = i.size();
	*infoIP = info->ip();
	*infoPort = info->port();
	if (info->has_pid())
	{
		std::string p = info->pid();
		*pid = (char*) p.data();
		*pidLen = p.size();
	}
	if (info->has_hostname())
	{
		std::string h = info->hostname();
		*hostname = (char*) h.data();
		*hostnameLen = h.size();
	}
}

void destroyMasterInfo(MasterInfoPtr info)
{
	delete info;
}

SlaveInfoPtr toSlaveInfo(char* hostname,
	int hostnameLen,
	unsigned int* port,
	ResourcePtr* resources,
	int resourcesLen,
	AttributePtr* attributes,
	int attributesLen,
	SlaveIDPtr slaveID,
	bool* checkpoint)
{
	SlaveInfoPtr info = new SlaveInfo();
	info->set_hostname(hostname, hostnameLen);
	if (port != NULL)
		info->set_port(*port);
	if (resourcesLen > 0)
	{
		::google::protobuf::RepeatedPtrField<Resource>* rs = info->mutable_resources();
		for (int i = 0; i < resourcesLen; ++i)
			*rs->Add() = *resources[i];
	}
	if (attributesLen > 0)
	{
		::google::protobuf::RepeatedPtrField<Attribute>* as = info->mutable_attributes();
		for (int i = 0; i < attributesLen; ++i)
			*as->Add() = *attributes[i];
	}
	if (slaveID != NULL)
		*info->mutable_id() = *slaveID;
	if (checkpoint != NULL)
		info->set_checkpoint(*checkpoint);

	return info;
}

void fromSlaveInfo(SlaveInfoPtr slaveInfo,
	char** hostname,
	int* hostnameLen,
	unsigned int* port,
	ResourcePtr** resources,
	int* resourcesLen,
	AttributePtr** attributes,
	int* attributeLen,
	SlaveIDPtr* slaveID,
	bool** checkpoint)
{
	std::string h = slaveInfo->hostname();
	*hostname = (char*) h.data();
	*hostnameLen = h.size();

	if (slaveInfo->has_port())
		*port = slaveInfo->port();

	*resourcesLen = slaveInfo->resources_size();
	int len = *resourcesLen;
	for (int i = 0; i < len; ++i)
	{
		/* code */
	}
}

void destroySlaveInfo(SlaveInfoPtr slaveInfo)
{
	delete slaveInfo;
}


RequestPtr toRequest(SlaveIDPtr slaveID,
	ResourcePtr* resources,
	int resourcesLen)
{
	RequestPtr request = new Request();
	*request->mutable_slave_id() = *slaveID;
	if (resourcesLen > 0)
	{
		::google::protobuf::RepeatedPtrField<Resource>* rs = request->mutable_resources();
		for (int i = 0; i < resourcesLen; ++i)
			*rs->Add() = *resources[i];
	}

	return request;
}

void fromRequest(RequestPtr request,
	ResourcePtr** resources,
	int* resourceLen)
{

}

void destroyRequest(RequestPtr request)
{
	delete request;
}


TaskInfoPtr toTaskInfo(char* infoName,
	int infoNameLen,
	TaskIDPtr taskID,
	SlaveIDPtr slaveID,
	ResourcePtr* resources,
	int resourcesLen,
	ExecutorInfoPtr executorInfo,
	CommandInfoPtr commandInfo,
	char* data,
	int dataLen)
{
	TaskInfoPtr info = new TaskInfo();
	info->set_name(infoName, infoNameLen);
	*info->mutable_task_id() = *taskID;
	*info->mutable_slave_id() = *slaveID;
	for (int i = 0; i < resourcesLen; ++i)
		info->add_resources()->MergeFrom(*resources[i]);
	if (executorInfo != NULL)
		info->mutable_executor()->MergeFrom(*executorInfo);
	if (commandInfo != NULL)
		info->mutable_command()->MergeFrom(*commandInfo);
	if (data != NULL)
		info->set_data(data, dataLen);
}

void fromTaskInfo(TaskInfoPtr taskInfo,
	char** infoName,
	int* infoNameLen,
	TaskIDPtr* taskID,
	SlaveIDPtr* slaveID,
	ResourcePtr** resources,
	int* resourcesLen,
	ExecutorInfoPtr** executorInfo,
	CommandInfoPtr** commandInfo,
	char* data,
	int* dataLen)
{

}

void destroyTaskInfo(TaskInfoPtr taskInfo)
{
	delete taskInfo;
}


TaskStatusPtr toTaskStatus(TaskIDPtr taskID,
	int state,
	char* message,
	int messageLen,
	char* data,
	int dataLen,
	SlaveIDPtr slaveID,
	double* timestamp)
{
	TaskStatusPtr status = new TaskStatus();
	status->mutable_task_id()->MergeFrom(*taskID);
	status->set_state((TaskState) state);
	if (message != NULL)
		status->set_message(message, messageLen);
	if (data != NULL)
		status->set_data(data, dataLen);
	if (slaveID != NULL)
		status->mutable_slave_id()->MergeFrom(*slaveID);
	if (timestamp != NULL)
		status->set_timestamp(*timestamp);

	return status;
}

void fromTaskStatus(TaskStatusPtr,
	TaskIDPtr* taskID,
	int* state,
	char** message,
	int* messageLen,
	char** data,
	int* dataLen,
	SlaveIDPtr** slaveID,
	double* timestamp)
{

}

void destroyTaskStatus(TaskStatusPtr taskStatus)
{
	delete taskStatus;
}

// **********************************************************************
FiltersPtr toFilters(double* refuseSeconds)
{
	FiltersPtr filters = new Filters();
	if (refuseSeconds != NULL)
		filters->set_refuse_seconds(*refuseSeconds);
	return filters;
}

void fromFilters(FiltersPtr filters,
	bool* refusalSet,
	double* refuseSeconds)
{
	*refusalSet = false;
	if (filters->has_refuse_seconds())
	{
		*refusalSet = true;
		*refuseSeconds = filters->refuse_seconds();
	}
}

void destroyFilters(FiltersPtr filters)
{
	delete filters;
}

// **********************************************************************

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
	std::string p = credential->principal();
	*principal = (char*) p.data();
	*principalLen = p.size();
	if (credential->has_secret())
	{
		std::string s = credential->secret();
		*secret = (char*) s.data();
		*secretLen = s.size();
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
