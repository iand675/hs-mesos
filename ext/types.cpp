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
				 int roleLen)
//	char* hostname,
// int hostLen)
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
  // if (hostname != NULL)
  //  info->set_hostname(hostname, hostLen);
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
		       int* roleLen)
		       // char** hostname,
		       // int* hostLen)
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
	/*
       	if (info->has_hostname())
	{
		const std::string h = info->hostname();
		*hostname = (char*) h.data();
		*hostLen = h.size();
	}
	*/
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
	bool* portSet,
	unsigned int* port,
	ResourcePtr** resources,
	int* resourcesLen,
	AttributePtr** attributes,
	int* attributeLen,
	SlaveIDPtr* slaveID,
	bool* checkpointSet,
	bool* checkpoint)
{
	*portSet = false;
	*checkpointSet = false;

	std::string h = slaveInfo->hostname();
	*hostname = (char*) h.data();
	*hostnameLen = h.size();

	if (slaveInfo->has_port())
	{
		*port = slaveInfo->port();
		*portSet = true;
	}

	*resourcesLen = slaveInfo->resources_size();
	*resources = slaveInfo->mutable_resources()->mutable_data();

	*attributeLen = slaveInfo->attributes_size();
	*attributes = slaveInfo->mutable_attributes()->mutable_data();

	if (slaveInfo->has_id())
		*slaveID = slaveInfo->mutable_id();

	if (slaveInfo->has_checkpoint())
	{
		*checkpoint = slaveInfo->checkpoint();
		*checkpointSet = true;
	}	
}

void destroySlaveInfo(SlaveInfoPtr slaveInfo)
{
	delete slaveInfo;
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
	std::string* n = info->mutable_name();
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

	return info;
}

void fromTaskInfo(TaskInfoPtr taskInfo,
	char** infoName,
	int* infoNameLen,
	TaskIDPtr* taskID,
	SlaveIDPtr* slaveID,
	ResourcePtr** resources,
	int* resourcesLen,
	ExecutorInfoPtr* executorInfo,
	CommandInfoPtr* commandInfo,
	char** data,
	int* dataLen)
{
	std::string* in = taskInfo->mutable_name();
	*infoName = (char*) in->data();
	*infoNameLen = in->size();
	*taskID = taskInfo->mutable_task_id();
	*slaveID = taskInfo->mutable_slave_id();
	*resources = taskInfo->mutable_resources()->mutable_data();
	*resourcesLen = taskInfo->resources_size();
	if (taskInfo->has_executor())
		*executorInfo = taskInfo->mutable_executor();

	if (taskInfo->has_command())
		*commandInfo = taskInfo->mutable_command();	

	if (taskInfo->has_data())
	{
		std::string d = taskInfo->data();
		*data = (char*) d.data();
		*dataLen = d.size();
	}
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

void fromTaskStatus(TaskStatusPtr status,
	TaskIDPtr* taskID,
	int* state,
	char** message,
	int* messageLen,
	char** data,
	int* dataLen,
	SlaveIDPtr* slaveID,
	bool* timestampSet,
	double* timestamp)
{
	*timestampSet = false;

	*taskID	= status->mutable_task_id();
	*state = status->state();
	if (status->has_message())
	{
		std::string m = status->message();
		*message = (char*) m.data();
		*messageLen = m.size();
	}

	if (status->has_data())
	{
		std::string d = status->data();
		*data = (char*) d.data();
		*dataLen = d.size();
	}

	if (status->has_slave_id())
		*slaveID = status->mutable_slave_id();

	if (status->has_timestamp())
	{
		*timestampSet = true;
		*timestamp = status->timestamp();
	}
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

// **********************************************************************
ResourcePtr toResource(char* name,
	int nameLen,
	ValuePtr value,
	char* role,
	int roleLen)
{
	ResourcePtr resource = new Resource();
	resource->set_name(name, nameLen);
	resource->set_type(value->type());

	if (value->has_scalar())
		*(resource->mutable_scalar()) = *value->mutable_scalar();

	if (value->has_ranges())
		*(resource->mutable_ranges()) = *value->mutable_ranges();

	if (value->has_set())
		*(resource->mutable_set()) = *value->mutable_set();

	if (value->has_text())
		*resource->mutable_set()->add_item() = value->mutable_text()->value();

	if (role != NULL)
		resource->set_role(role, roleLen);

	return resource;
}

void fromResource(ResourcePtr resource,
	char** name,
	int* nameLen,
	ValuePtr* value,
	char** role,
	int* roleLen)
{
	std::string n = resource->name();
	*name = (char*) n.data();
	*nameLen = n.size();

	Value* v = new Value();
	if (resource->has_scalar())
	{
		v->set_type(Value_Type_SCALAR);
		*v->mutable_scalar() = resource->scalar();
	}
	if (resource->has_ranges())
	{
		v->set_type(Value_Type_RANGES);
		*v->mutable_ranges() = resource->ranges();
	}
	if (resource->has_set())
	{
		v->set_type(Value_Type_SET);
		*v->mutable_set() = resource->set();
	}

	*value = v;

	std::string r = resource->role();
	*role = (char*) r.data();
	*roleLen = r.size();
}

void destroyResource(ResourcePtr resource)
{
	delete resource;
}

// **********************************************************************
ExecutorInfoPtr toExecutorInfo(ExecutorIDPtr executorID,
	FrameworkIDPtr frameworkID,
	CommandInfoPtr commandInfo,
	ResourcePtr* resources,
	int resourceLen,
	char* name,
	int nameLen,
	char* source,
	int sourceLen,
	char* data,
	int dataLen)
{
	ExecutorInfoPtr info = new ExecutorInfo();
	*info->mutable_executor_id() = *executorID;
	*info->mutable_framework_id() = *frameworkID;
	*info->mutable_command() = *commandInfo;
	::google::protobuf::RepeatedPtrField<Resource>* rs = info->mutable_resources();
	for (int i = 0; i < resourceLen; ++i)
		*rs->Add() = *resources[i];
	if (name != NULL)
		info->set_name(name, nameLen);
	if (source != NULL)
		info->set_source(source, sourceLen);
	if (data != NULL)
		info->set_data(data, dataLen);

	return info;
}

void fromExecutorInfo(ExecutorInfoPtr info,
	ExecutorIDPtr* executorID,
	FrameworkIDPtr* frameworkID,
	CommandInfoPtr* commandInfo,
	ResourcePtr** resources,
	int* resourcesLen,
	char** name,
	int* nameLen,
	char** source,
	int* sourceLen,
	char** data,
	int* dataLen)
{
	*executorID = info->mutable_executor_id();
	*frameworkID = info->mutable_framework_id();
	*commandInfo = info->mutable_command();
	*resources = info->mutable_resources()->mutable_data();
	*resourcesLen = info->resources_size();
	if (info->has_name())
	{
		std::string n = info->name();
		*name = (char*) n.data();
		*nameLen = n.size();
	}
	if (info->has_source())
	{
		std::string s = info->source();
		*source = (char*) s.data();
		*sourceLen = s.size();
	}
	if (info->has_data())
	{
		std::string d = info->data();
		*data = (char*) d.data();
		*dataLen = d.size();
	}
}

void destroyExecutorInfo(ExecutorInfoPtr info)
{
	delete info;
}
// **********************************************************************
EnvironmentVariablePtr toEnvironmentVariable(char* key,
	int keyLen,
	char* value,
	int valueLen)
{
	EnvironmentVariablePtr var = new Environment_Variable();
	var->set_name(key, keyLen);
	var->set_value(value, valueLen);
	return var;
}

void fromEnvironmentVariable(EnvironmentVariablePtr env,
	char** key,
	int* keyLen,
	char** value,
	int* valueLen)
{
	std::string k = env->name();
	std::string v = env->value();
	*key = (char*) k.data();
	*keyLen = k.size();
	*value = (char*) v.data();
	*valueLen = v.size();
};

void destroyEnvironmentVariable(EnvironmentVariablePtr env)
{
	delete env;
}
// **********************************************************************
EnvironmentPtr toEnvironment(EnvironmentVariablePtr* envVars,
	int envLen)
{
	EnvironmentPtr env = new Environment();
	if (envVars != NULL)
	{
		::google::protobuf::RepeatedPtrField<Environment_Variable>* vs = env->mutable_variables();
		for (int i = 0; i < envLen; ++i)
			*vs->Add() = *envVars[i];
	}

	return env;
}

void fromEnvironment(EnvironmentPtr env,
	EnvironmentVariablePtr** vars,
	int* varLen)
{
	*vars = env->mutable_variables()->mutable_data();
	*varLen = env->variables_size();
}

void destroyEnvironment(EnvironmentPtr env)
{
	delete env;
}
// **********************************************************************
AttributePtr toAttribute(char* name,
	int nameLen,
	ValuePtr value)
{
	AttributePtr attribute = new Attribute();
	attribute->set_name(name, nameLen);
	attribute->set_type(value->type());
	if (value->has_scalar())
		*attribute->mutable_scalar() = *value->mutable_scalar();
	if (value->has_ranges())
		*attribute->mutable_ranges() = *value->mutable_ranges();
	if (value->has_set())
		*attribute->mutable_set() = *value->mutable_set();
	if (value->has_text())
		*attribute->mutable_text() = *value->mutable_text();
	return attribute;
}

void fromAttribute(AttributePtr attribute,
	char** name,
	int* nameLen,
	ValuePtr* vp)
{
	std::string n = attribute->name();
	*name = (char*) n.data();
	*nameLen = n.size();

	ValuePtr value = new Value();
	value->set_type(attribute->type());
	if (attribute->has_scalar())
		*value->mutable_scalar() = *attribute->mutable_scalar();
	if (attribute->has_ranges())
		*value->mutable_ranges() = *attribute->mutable_ranges();
	if (attribute->has_set())
		*value->mutable_set() = *attribute->mutable_set();
	if (attribute->has_text())
		*value->mutable_text() = *attribute->mutable_text();

	*vp = value;
}

void destroyAttribute(AttributePtr attribute)
{
	delete attribute;
}

// **********************************************************************

RequestPtr toRequest(SlaveIDPtr slaveID,
	ResourcePtr* resources,
	int resourceLen)
{
	RequestPtr request = new Request();
	if (slaveID != NULL)
		*request->mutable_slave_id() = *slaveID;

	::google::protobuf::RepeatedPtrField<Resource>* rs = request->mutable_resources();
	for (int i = 0; i < resourceLen; ++i)
		*rs->Add() = *resources[i];
	return request;
}

void fromRequest(RequestPtr request,
	SlaveIDPtr* slaveID,
	ResourcePtr** resources,
	int* resourceLen)
{
	if (request->has_slave_id())
		*slaveID = request->mutable_slave_id();

	*resources = request->mutable_resources()->mutable_data();
	*resourceLen = request->resources_size();
}

void destroyRequest(RequestPtr request)
{
	delete request;
}
// **********************************************************************

ValuePtr toValue(int type,
	double scalar,
	ValueRangePtr* ranges,
	int rangeLen,
	StdStringPtr* strings,
	int stringsLen,
	char* text,
	int textLen)
{
	ValuePtr value = new Value();
	value->set_type((Value_Type) type);
	if (type == Value_Type_SCALAR)
	{
		Value_Scalar s;
		s.set_value(scalar);
		*value->mutable_scalar() = s;
	}
	else if (type == Value_Type_RANGES)
	{
		Value_RangesPtr rs = value->mutable_ranges();
		for (int i = 0; i < rangeLen; ++i)
			*rs->add_range() = *ranges[i];
	}
	else if (type == Value_Type_SET)
	{
		Value_SetPtr set = value->mutable_set();
		for (int i = 0; i < stringsLen; ++i)
			set->add_item(*strings[i]);
	}
	else if (type == Value_Type_TEXT)
	{
		Value_Text t;
		t.set_value(text, textLen);
		*value->mutable_text() = t;
	}

	return value;
}

void fromValue(ValuePtr value,
	int* type,
	double* scalar,
	ValueRangePtr** ranges,	
	int* rangeLen,
	StdStringPtr** strings,
	int* stringsLen,
	char** text,
	int* textLen)
{
	Value_Type t = value->type();
	*type = (int) t;
	if (t == Value_Type_SCALAR)
	{
		*scalar = value->mutable_scalar()->value();
	}
	else if (t == Value_Type_RANGES)
	{
		*rangeLen = value->mutable_ranges()->range_size();
		*ranges = value->mutable_ranges()->mutable_range()->mutable_data();
	}
	else if (t == Value_Type_SET)
	{
		*stringsLen = value->set().item_size();
		*strings = value->mutable_set()->mutable_item()->mutable_data();
	}
	else if (t == Value_Type_TEXT)
	{
		std::string txt = value->mutable_text()->value();
		*text = (char*) txt.data();
		*textLen = txt.size();
	}
}

void destroyValue(ValuePtr value)
{
	delete value;
}

// **********************************************************************

CommandInfoPtr toCommandInfo(CommandInfo_URIPtr* uris,
	int urisLen,
	EnvironmentPtr environment,
	char* value,
	int valueLen)
{
	CommandInfoPtr info = new CommandInfo();
	for (int i = 0; i < urisLen; ++i)
		*info->add_uris() = *uris[i];
	if (environment != NULL)
		*info->mutable_environment() = *environment;
	info->set_value(value, valueLen);
	return info;
}

void fromCommandInfo(CommandInfoPtr info,
	CommandInfo_URIPtr** uris,
	int* urisLen,
	EnvironmentPtr* environment,
	char** value,
	int* valueLen)
{
	*uris = info->mutable_uris()->mutable_data();
	*urisLen = info->uris_size();

	if (info->has_environment())
		*environment = info->mutable_environment();

	*value = (char*) info->mutable_value()->data();
	*valueLen = info->mutable_value()->size();
}

void destroyCommandInfo(CommandInfoPtr info)
{
	delete info;
}

// **********************************************************************
ResourceUsagePtr toResourceUsage(SlaveIDPtr slaveID,
	FrameworkIDPtr frameworkID,
	ExecutorIDPtr executorID,
	char* executorName,
	int nameLen,
	TaskIDPtr taskID,
	ResourceStatisticsPtr statistics)
{
	ResourceUsagePtr usage = new ResourceUsage();
	*usage->mutable_slave_id() = *slaveID;
	*usage->mutable_framework_id() = *frameworkID;
	if (executorID != NULL)
		*usage->mutable_executor_id() = *executorID;
	if (executorName != NULL)
		usage->set_executor_name(executorName, nameLen);
	if (taskID != NULL)
		*usage->mutable_task_id() = *taskID;
	if (statistics != NULL)
		*usage->mutable_statistics() = *statistics;

	return usage;
}

void fromResourceUsage(ResourceUsagePtr usage,
	SlaveIDPtr* slaveID,
	FrameworkIDPtr* frameworkID,
	ExecutorIDPtr* executorID,
	char** executorName,
	int* nameLen,
	TaskIDPtr* taskID,
	ResourceStatisticsPtr* statistics)
{
	*slaveID = usage->mutable_slave_id();
	*frameworkID = usage->mutable_framework_id();
	if (usage->has_executor_id())
		*executorID = usage->mutable_executor_id();
	if (usage->has_executor_name())
	{
		std::string n = usage->executor_name();
		*executorName = (char*) n.data();
		*nameLen = n.size();
	}
	if (usage->has_task_id())
		*taskID = usage->mutable_task_id();
	if (usage->has_statistics())
		*statistics = usage->mutable_statistics();
}

void destroyResourceUsage(ResourceUsagePtr usage)
{
	delete usage;
}
// **********************************************************************
OfferPtr toOffer(OfferIDPtr offerID,
	FrameworkIDPtr frameworkID,
	SlaveIDPtr slaveID,
	char* hostname,
	int hostnameLen,
	ResourcePtr* resources,
	int resourcesLen,
	AttributePtr* attributes,
	int attributesLen,
	ExecutorIDPtr* executorIDs,
	int idsLen)
{
	OfferPtr offer = new Offer();
	*offer->mutable_id() = *offerID;
	*offer->mutable_framework_id() = *frameworkID;
	*offer->mutable_slave_id() = *slaveID;
	offer->set_hostname(hostname, hostnameLen);
	for (int i = 0; i < resourcesLen; ++i)
		*offer->add_resources() = *resources[i];
	for (int i = 0; i < attributesLen; ++i)
		*offer->add_attributes() = *attributes[i];
	for (int i = 0; i < idsLen; ++i)
		*offer->add_executor_ids() = *executorIDs[i];

	return offer;
}

void fromOffer(OfferPtr offer,
	OfferIDPtr* offerID,
	FrameworkIDPtr* frameworkID,
	SlaveIDPtr* slaveID,
	char** hostname,
	int* hostnameLen,
	ResourcePtr** resources,
	int* resourcesLen,
	AttributePtr** attributes,
	int* attributesLen,
	ExecutorIDPtr** executorIDs,
	int* idsLen)
{
	*offerID = offer->mutable_id();
	*frameworkID = offer->mutable_framework_id();
	*slaveID = offer->mutable_slave_id();
	*hostname = (char*) offer->mutable_hostname()->data();
	*hostnameLen = offer->mutable_hostname()->size();
	*resources = offer->mutable_resources()->mutable_data();
	*resourcesLen = offer->resources_size();
	*attributes = offer->mutable_attributes()->mutable_data();
	*attributesLen = offer->attributes_size();
	*executorIDs = offer->mutable_executor_ids()->mutable_data();
	*idsLen = offer->executor_ids_size();
}

void destroyOffer(OfferPtr offer)
{
	delete offer;
}

// **********************************************************************

ResourceStatisticsPtr toResourceStatistics(double timestamp,
	double* cpusUserTimeSecs,
	double* cpusSystemTimeSecs,
	double cpusLimit,
	unsigned int* cpusPeriods,
	unsigned int* cpusThrottled,
	double* cpusThrottledTimeSecs,
	unsigned long* memoryResidentSetSize,
	unsigned long* memoryLimitBytes,
	unsigned long* memoryFileBytes,
	unsigned long* memoryAnonymousBytes,
	unsigned long* memoryMappedFileBytes)
{
	ResourceStatisticsPtr stats = new ResourceStatistics();
	stats->set_timestamp(timestamp);
	if (cpusUserTimeSecs != NULL)
		stats->set_cpus_user_time_secs(*cpusUserTimeSecs);
	if (cpusSystemTimeSecs != NULL)
		stats->set_cpus_system_time_secs(*cpusSystemTimeSecs);
	stats->set_cpus_limit(cpusLimit);
	if (cpusPeriods != NULL)
		stats->set_cpus_nr_periods(*cpusPeriods);
	if (cpusThrottled != NULL)
		stats->set_cpus_nr_throttled(*cpusThrottled);
	if (cpusThrottledTimeSecs != NULL)
		stats->set_cpus_throttled_time_secs(*cpusThrottledTimeSecs);
	if (memoryResidentSetSize != NULL)
		stats->set_mem_rss_bytes(*memoryResidentSetSize);
	if (memoryLimitBytes != NULL)
		stats->set_mem_limit_bytes(*memoryLimitBytes);
	if (memoryFileBytes != NULL)
		stats->set_mem_file_bytes(*memoryFileBytes);
	if (memoryAnonymousBytes != NULL)
		stats->set_mem_anon_bytes(*memoryAnonymousBytes);
	if (memoryMappedFileBytes != NULL)
		stats->set_mem_mapped_file_bytes(*memoryMappedFileBytes);
	return stats;
}

void fromResourceStatistics(ResourceStatisticsPtr stats,
double* timestamp,
double* cpusUserTimeSecs,
bool* cpusUserTimeSecsSet,
double* cpusSystemTimeSecs,
bool* cpusSystemTimeSecsSet,
double* cpusLimit,
unsigned int* cpusPeriods,
bool* cpusPeriodsSet,
unsigned int* cpusThrottled,
bool* cpusThrottledSet,
double* cpusThrottledTimeSecs,
bool* cpusThrottledTimeSecsSet,
unsigned long* memoryResidentSetSize,
bool* memoryResidentSetSizeSet,
unsigned long* memoryLimitBytes,
bool* memoryLimitBytesSet,
unsigned long* memoryFileBytes,
bool* memoryFileBytesSet,
unsigned long* memoryAnonymousBytes,
bool* memoryAnonymousBytesSet,
unsigned long* memoryMappedFileBytes,
bool* memoryMappedFileBytesSet)
{
	*cpusUserTimeSecsSet = false;
	*cpusSystemTimeSecsSet = false;
	*cpusPeriodsSet = false;
	*cpusThrottledSet = false;
	*cpusThrottledTimeSecsSet = false;
	*memoryResidentSetSizeSet = false;
	*memoryLimitBytesSet = false;
	*memoryFileBytesSet = false;
	*memoryAnonymousBytesSet = false;
	*memoryMappedFileBytesSet = false;

	*timestamp = stats->timestamp();
	*cpusLimit = stats->cpus_limit();

	if (stats->has_cpus_user_time_secs())
	{
		*cpusUserTimeSecs = stats->cpus_user_time_secs();
		*cpusUserTimeSecsSet = true;
	}

	if (stats->has_cpus_system_time_secs())
	{
		*cpusSystemTimeSecs = stats->cpus_system_time_secs();
		*cpusSystemTimeSecsSet = true;
	}

	if (stats->has_cpus_nr_periods())
	{
		*cpusPeriods = stats->cpus_nr_periods();
		*cpusPeriodsSet = true;
	}

	if (stats->has_cpus_nr_throttled())
	{
		*cpusThrottled = stats->cpus_nr_throttled();
		*cpusThrottledSet = true;
	}

	if (stats->has_cpus_throttled_time_secs())
	{
		*cpusThrottledTimeSecs = stats->cpus_throttled_time_secs();
		*cpusThrottledTimeSecsSet = true;
	}

	if (stats->has_mem_rss_bytes())
	{
		*memoryResidentSetSize = stats->mem_rss_bytes();
		*memoryResidentSetSizeSet = true;
	}

	if (stats->has_mem_limit_bytes())
	{
		*memoryLimitBytes = stats->mem_limit_bytes();
		*memoryLimitBytesSet = true;
	}

	if (stats->has_mem_file_bytes())
	{
		*memoryFileBytes = stats->mem_file_bytes();
		*memoryFileBytesSet = true;
	}

	if (stats->has_mem_anon_bytes())
	{
		*memoryAnonymousBytes = stats->mem_anon_bytes();
		*memoryAnonymousBytesSet = true;
	}

	if (stats->has_mem_mapped_file_bytes())
	{
		*memoryMappedFileBytes = stats->mem_mapped_file_bytes();
		*memoryMappedFileBytesSet = true;
	}
}

void destroyResourceStatistics(ResourceStatisticsPtr statistics)
{
	delete statistics;
}

// **********************************************************************

ParametersPtr toParameters(ParameterPtr* parameters,
	int pLen)
{
	ParametersPtr params = new Parameters();
	for (int i = 0; i < pLen; ++i)
		*params->add_parameter() = *parameters[i];
	return params;
}

void fromParameters(ParametersPtr params,
	ParameterPtr** parameters,
	int* pLen)
{
	*parameters = params->mutable_parameter()->mutable_data();
	*pLen = params->parameter_size();
}

void destroyParameters(ParametersPtr params)
{
	delete params;
}

// **********************************************************************

ParameterPtr toParameter(char* keyP,
	int keyLen,
	char* valP,
	int valLen)
{
	ParameterPtr parameter = new Parameter();
	parameter->set_key(keyP, keyLen);
	parameter->set_value(valP, valLen);
	return parameter;
}

void fromParameter(ParameterPtr parameter,
	char** keyP,
	int* keyLenP,
	char** valueP,
	int* valueLenP)
{
	std::string k = parameter->key();
	std::string v = parameter->value();
	*keyP = (char*) k.data();
	*keyLenP = k.size();
	*valueP = (char*) v.data();
	*valueLenP = v.size();
}

void destroyParameter(ParameterPtr parameter)
{
	delete parameter;
}

// **********************************************************************

ValueRangePtr toRange(unsigned long low,
	unsigned long high)
{
	ValueRangePtr range = new Value_Range();
	range->set_begin(low);
	range->set_end(high);
	return range;
}

void fromRange(ValueRangePtr range,
	unsigned long* lowP,
	unsigned long* highP)
{
	*lowP = range->begin();
	*highP = range->end();
}

void destroyRange(ValueRangePtr range)
{
	delete range;
}
// **********************************************************************

StdStringPtr toStdString(char* str,
	int strLen)
{
	return new std::string(str, strLen);
}

void fromStdString(StdStringPtr sp, char** str, int* strLen)
{
	*str = (char*) sp->data();
	*strLen = sp->size();
}

void destroyStdString(StdStringPtr sp)
{
	delete sp;
}


