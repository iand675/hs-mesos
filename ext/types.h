#include <mesos/mesos.hpp>

typedef mesos::FrameworkID *FrameworkIDPtr;
typedef mesos::OfferID *OfferIDPtr;
typedef mesos::SlaveID *SlaveIDPtr;
typedef mesos::TaskID *TaskIDPtr;
typedef mesos::ExecutorID *ExecutorIDPtr;
typedef mesos::ContainerID *ContainerIDPtr;
typedef mesos::FrameworkInfo *FrameworkInfoPtr;
typedef mesos::CommandInfo *CommandInfoPtr;
typedef mesos::CommandInfo_URI *CommandInfo_URIPtr;
typedef mesos::ExecutorInfo *ExecutorInfoPtr;
typedef mesos::MasterInfo *MasterInfoPtr;
typedef mesos::SlaveInfo *SlaveInfoPtr;
typedef mesos::Value *ValuePtr;
typedef mesos::Value_Scalar *Value_ScalarPtr;
typedef mesos::Value_Range *Value_RangePtr;
typedef mesos::Value_Ranges *Value_RangesPtr;
typedef mesos::Value_Set *Value_SetPtr;
typedef mesos::Value_Text *Value_TextPtr;
typedef mesos::Attribute *AttributePtr;
typedef mesos::Resource *ResourcePtr;
typedef mesos::ResourceStatistics *ResourceStatisticsPtr;
typedef mesos::ResourceUsage *ResourceUsagePtr;
typedef mesos::Request *RequestPtr;
typedef mesos::Offer *OfferPtr;
typedef mesos::TaskInfo *TaskInfoPtr;
typedef mesos::TaskStatus *TaskStatusPtr;
typedef mesos::Filters *FiltersPtr;
typedef mesos::Environment *EnvironmentPtr;
typedef mesos::Environment_Variable *EnvironmentVariablePtr;
typedef mesos::Parameter *ParameterPtr;
typedef mesos::Parameters *ParametersPtr;
typedef mesos::Credential *CredentialPtr;
typedef mesos::CommandInfo_URI *CommandURIPtr;

extern "C" {
	// **********************************************************************
	extern FrameworkIDPtr toFrameworkID(char* bs, int len);
	extern int fromFrameworkID(FrameworkIDPtr p, char** poke);
	extern void destroyFrameworkID(FrameworkIDPtr p);

	// **********************************************************************
	extern OfferIDPtr toOfferID(char* bs, int len);
	extern int fromOfferID(OfferIDPtr p, char** poke);
	extern void destroyOfferID(OfferIDPtr p);

	// **********************************************************************
	extern SlaveIDPtr toSlaveID(char* bs, int len);
	extern int fromSlaveID(SlaveIDPtr p, char** poke);
	extern void destroySlaveID(SlaveIDPtr p);

	// **********************************************************************
	extern TaskIDPtr toTaskID(char* bs, int len);
	extern int fromTaskID(TaskIDPtr p, char** poke);
	extern void destroyTaskID(TaskIDPtr p);

	// **********************************************************************
	extern ExecutorIDPtr toExecutorID(char* bs, int len);
	extern int fromExecutorID(ExecutorIDPtr p, char** poke);
	extern void destroyExecutorID(ExecutorIDPtr p);

	// **********************************************************************
	extern ContainerIDPtr toContainerID(char* bs, int len);
	extern int fromContainerID(ContainerIDPtr p, char** poke);
	extern void destroyContainerID(ContainerIDPtr p);

	// **********************************************************************
	extern FrameworkInfoPtr toFrameworkInfo(char* user,
		int userLen,
		char* name,
		int nameLen,
		FrameworkIDPtr* frameworkID,
		double* failoverTimeout,
		bool* checkpoint,
		char* role,
		int roleLen,
		char* hostname,
		int hostLen);
	extern void fromFrameworkInfo(FrameworkInfoPtr info,
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
		int* hostLen);
	extern void destroyFrameworkInfo(FrameworkInfoPtr info);

	// **********************************************************************
	extern CommandURIPtr toCommandURI(char* cmd,
		int cmdLen,
		bool* executable);

	extern void fromCommandURI(CommandURIPtr commandURI,
		char** cmd,
		int* cmdLen,
		bool* executableSet,
		bool* executable);

	extern void destroyCommandURI(CommandURIPtr commandURI);
	// **********************************************************************
	extern CommandInfoPtr toCommandInfo(CommandURIPtr* uris,
		int urisLen,
		EnvironmentPtr env,
		char* value,
		int valueLen);

	extern void fromCommandInfo(CommandInfoPtr info,
		CommandURIPtr** uris,
		int* urisLen,
		bool* envSet,
		EnvironmentPtr* env,
		char** value,
		int* valueLen);

	extern void destroyCommandInfo(CommandInfoPtr info);
	// **********************************************************************
	
	extern ExecutorInfoPtr toExecutorInfo(ExecutorIDPtr eid,
		FrameworkIDPtr fid,
		CommandInfoPtr ci,
		ResourcePtr* resources,
		int resourcesLen,
		char* name,
		int nameLen,
		char* source,
		int sourceLen,
		char* data,
		int dataLen);

	extern void fromExecutorInfo(ExecutorInfoPtr info,
		ExecutorIDPtr* eid,
		FrameworkIDPtr* fid,
		CommandInfoPtr* ci,
		ResourcePtr** resources,
		int* resourcesLen,
		bool* nameSet,
		char* name,
		int* nameLen,
		bool* sourceSet,
		char* source,
		int* sourceLen,
		bool* dataSet,
		char* data,
		int* dataLen);

	extern void destroyExecutorInfo(ExecutorInfoPtr info);
	// **********************************************************************
	extern MasterInfoPtr toMasterInfo(char* infoID,
		int infoIDLen,
		unsigned int infoIP,
		unsigned int infoPort,
		char* pid,
		int pidLen,
		char* hostname,
		int hostnameLen);
	extern void fromMasterInfo(MasterInfoPtr info,
		char** infoID,
		int* infoIDLen,
		unsigned int* infoIP,
		unsigned int* infoPort,
		char** pid,
		int* pidLen,
		char** hostname,
		int* hostnameLen);
	extern void destroyMasterInfo(MasterInfoPtr info);
	
	// **********************************************************************
	extern SlaveInfoPtr toSlaveInfo(char* hostname,
		int hostnameLen,
		unsigned int* port,
		ResourcePtr* resources,
		int resourcesLen,
		AttributePtr* attributes,
		int attributeLen,
		SlaveIDPtr slaveID,
		bool* checkpoint);
	extern void fromSlaveInfo(SlaveInfoPtr slaveInfo,
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
		bool* checkpoint);
	extern void destroySlaveInfo(SlaveInfoPtr slaveInfo);
	// **********************************************************************

	extern RequestPtr toRequest(SlaveIDPtr slaveID,
		ResourcePtr* resources,
		int resourceLen);
	extern void fromRequest(RequestPtr request,
		bool* slaveIDSet,
		SlaveIDPtr* slaveID,
		ResourcePtr** resources,
		int* resourceLen);
	extern void destroyRequest(RequestPtr request);
	
	// **********************************************************************
	extern OfferPtr toOffer(OfferIDPtr offerID,
		FrameworkIDPtr frameworkID,
		SlaveIDPtr slaveID,
		char* hostname,
		int hostnameLen,
		ResourcePtr* resources,
		int resourceLen,
		AttributePtr* attributes,
		int attributeLen,
		ExecutorIDPtr* executors,
		int executorLen);
	extern void fromOffer(OfferPtr offer,
		OfferIDPtr* offerID,
		FrameworkIDPtr* frameworkID,
		SlaveIDPtr* slaveID,
		char** hostname,
		int* hostnameLen,
		ResourcePtr** resources,
		int* resourceLen,
		AttributePtr** attributes,
		int* attributeLen,
		ExecutorIDPtr** executors,
		int* executorLen);
	extern void destroyOffer(OfferPtr offer);
	// **********************************************************************
	extern TaskInfoPtr toTaskInfo(char* infoName,
		int infoNameLen,
		TaskIDPtr taskID,
		SlaveIDPtr slaveID,
		ResourcePtr* resources,
		int resourcesLen,
		ExecutorInfoPtr* executorInfo,
		CommandInfoPtr* commandInfo,
		char* data,
		int dataLen);
	extern void fromTaskInfo(TaskInfoPtr taskInfo,
		char** infoName,
		int* infoNameLen,
		TaskIDPtr* taskID,
		SlaveIDPtr* slaveID,
		ResourcePtr** resources,
		int* resourcesLen,
		ExecutorInfoPtr** executorInfo,
		CommandInfoPtr** commandInfo,
		char* data,
		int dataLen);
	extern void deleteTaskInfo(TaskInfoPtr taskInfo);

	// **********************************************************************
	extern TaskStatusPtr toTaskStatus(TaskIDPtr taskID,
		int state,
		char** message,
		int* messageLen,
		char** data,
		int* dataLen,
		SlaveIDPtr* slaveID,
		double* timestamp);
	extern void fromTaskStatus(TaskStatusPtr,
		TaskIDPtr* taskID,
		int* state,
		char*** message,
		int** messageLen,
		char*** data,
		int** dataLen,
		SlaveIDPtr** slaveID,
		double** timestamp);
	extern void destroyTaskStatus(TaskStatusPtr taskStatus);
	
	// **********************************************************************
	extern FiltersPtr toFilters(double* refuseSeconds);
	extern void fromFilters(FiltersPtr filters,
		bool* refusalSet,
		double* refuseSeconds);
	extern void destroyFilters(FiltersPtr filters);
	// **********************************************************************
	extern EnvironmentVariablePtr toEnvironmentVariable(char* key,
		int keyLen,
		char* value,
		int valueLen);
	extern void fromEnvironmentVariable(EnvironmentVariablePtr var,
		char** key,
		int* keyLen,
		char** value,
		int* valueLen);
	extern void destroyEnvironmentVariable(EnvironmentVariablePtr env);
	// **********************************************************************
	extern EnvironmentPtr toEnvironment(EnvironmentVariablePtr* env,
		int envLen);
	extern void fromEnvironment(EnvironmentPtr env,
		EnvironmentVariablePtr** variables,
		int* envLen);
	extern void destroyEnvironment(EnvironmentPtr env);
	// **********************************************************************
	extern CredentialPtr toCredential(char* principal,
		int principalLen,
		char* secret,
		int secretLen);
	extern void fromCredential(
		CredentialPtr credential,
		char** principal,
		int* principalLen,
		char** secret,
		int* secretLen);
	extern void destroyCredential(CredentialPtr credential);
};

