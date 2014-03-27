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
		ExecutorInfoPtr executorInfo,
		CommandInfoPtr commandInfo,
		char* data,
		int dataLen);
	extern void fromTaskInfo(TaskInfoPtr taskInfo,
		char** infoName,
		int* infoNameLen,
		TaskIDPtr* taskID,
		SlaveIDPtr* slaveID,
		ResourcePtr** resources,
		int* resourcesLen,
		ExecutorInfoPtr* executorInfo,
		CommandInfoPtr* commandInfo,
		char** data,
		int* dataLen);
	extern void destroyTaskInfo(TaskInfoPtr taskInfo);

	// **********************************************************************
	extern TaskStatusPtr toTaskStatus(TaskIDPtr taskID,
		int state,
		char* message,
		int messageLen,
		char* data,
		int dataLen,
		SlaveIDPtr slaveID,
		double* timestamp);
	extern void fromTaskStatus(TaskStatusPtr status,
		TaskIDPtr* taskID,
		int* state,
		char** message,
		int* messageLen,
		char** data,
		int* dataLen,
		SlaveIDPtr* slaveID,
		double* timestamp);
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
	// **********************************************************************
	extern ResourcePtr toResource(char* name,
		int nameLen,
		ValuePtr value,
		char* role,
		int roleLen);
	extern void fromResource(ResourcePtr resource,
		char** name,
		int* nameLen,
		ValuePtr* value,
		char** role,
		int* roleLen);
	extern void destroyResource(ResourcePtr resource);
	// **********************************************************************
	extern ExecutorInfoPtr toExecutorInfo(ExecutorIDPtr executorID,
		FrameworkIDPtr frameworkID,
		CommandInfoPtr commandInfo,
		ResourcePtr* resources,
		int resourceLen,
		char* name,
		int nameLen,
		char* source,
		int sourceLen,
		char* data,
		int dataLen);
	extern void fromExecutorInfo(ExecutorInfoPtr executorInfo,
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
		int* dataLen);
	extern void destroyExecutorInfo(ExecutorInfoPtr executorInfo);
	// **********************************************************************
	extern AttributePtr toAttribute(char* name,
		int nameLen,
		ValuePtr value);
	extern void fromAttribute(AttributePtr attribute,
		char** name,
		int* nameLen,
		ValuePtr* value);
	extern void destroyAttribute(AttributePtr attribute);
	// **********************************************************************
	extern RequestPtr toRequest(SlaveIDPtr slaveID,
		ResourcePtr* resources,
		int resourceLen);
	extern void fromRequest(RequestPtr request,
		SlaveIDPtr* slaveID,
		ResourcePtr** resources,
		int* resourceLen);
	extern void destroyRequest(RequestPtr request);
	// **********************************************************************
	extern ValuePtr toValue(int type,
		double scalar,
		unsigned long* lows,
		unsigned long* highs,
		int rangeLen,
		char** strings,
		int* stringLens,
		int stringsLen,
		char* text,
		int textLen);

	extern void fromValue(ValuePtr value,
		int* type,
		double* scalar,
		unsigned long** lows,
		unsigned long** highs,
		int* rangeLen,
		char*** strings,
		int** stringLens,
		int* stringsLen,
		char** text,
		int* textLen);

	extern void destroyValue(ValuePtr value);
	// **********************************************************************
	extern CommandInfoPtr toCommandInfo(CommandInfo_URIPtr* uris,
		int urisLen,
		EnvironmentPtr environment,
		char* value,
		int valueLen);

	extern void fromCommandInfo(CommandInfoPtr info,
		CommandInfo_URIPtr** uris,
		int* urisLen,
		EnvironmentPtr* environment,
		char** value,
		int* valueLen);

	extern void destroyCommandInfo(CommandInfoPtr info);
	// **********************************************************************
	extern ResourceUsagePtr toResourceUsage(SlaveIDPtr slaveID,
		FrameworkIDPtr frameworkID,
		ExecutorIDPtr executorID,
		char* executorName,
		int nameLen,
		TaskIDPtr taskID,
		ResourceStatisticsPtr statistics);
	
	extern void fromResourceUsage(ResourceUsagePtr usage,
		SlaveIDPtr* slaveID,
		FrameworkIDPtr* frameworkID,
		ExecutorIDPtr* executorID,
		char** executorName,
		int* nameLen,
		TaskIDPtr* taskID,
		ResourceStatisticsPtr* statistics);

	extern void destroyResourceUsage(ResourceUsagePtr usage);
	// **********************************************************************
	extern OfferPtr toOffer(OfferIDPtr offerID,
		FrameworkIDPtr frameworkID,
		SlaveIDPtr slaveID,
		char* hostname,
		int hostnameLen,
		ResourcePtr* resources,
		int resourcesLen,
		AttributePtr* attributes,
		int attributeLen,
		ExecutorIDPtr* executorIDs,
		int idsLen);

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
		int* idsLen);

	extern void destroyOffer(OfferPtr offer);
	// **********************************************************************
	extern void destroyResourceStatistics(ResourceStatisticsPtr statistics);
	extern ResourceStatisticsPtr toResourceStatistics(double timestamp,
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
		unsigned long* memoryMappedFileBytes);
	extern void fromResourceStatistics(ResourceStatisticsPtr stats,
		double timestamp,
		double* cpusUserTimeSecs,
		bool* cpusUserTimeSecsSet,
		double* cpusSystemTimeSecs,
		bool* cpusSystemTimeSecsSet,
		double cpusLimit,
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
		bool* memoryMappedFileBytesSet);

	// **********************************************************************
	extern ParameterPtr toParameter(char* key,
		int keyLen,
		char* value,
		int valueLen);
	extern void fromParameter(ParameterPtr parameter,
		char** keyP,
		int* keyLenP,
		char** valueP,
		int* valueLenP);
	extern void destroyParameter(ParameterPtr parameter);
	// **********************************************************************
	extern ParametersPtr toParameters(ParameterPtr* parameters,
		int pLen);
	extern void fromParameters(ParametersPtr params,
		ParameterPtr** parameters,
		int* pLen);
	extern void destroyParameters(ParametersPtr params);
};

