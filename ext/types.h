#include <mesos/mesos.hpp>

// some_header_file.h
#ifndef HS_MESOS_WRAPPERS
#define HS_MESOS_WRAPPERS

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
typedef mesos::Value_Range *ValueRangePtr;
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
typedef mesos::HealthCheck *HealthCheckPtr;
typedef mesos::ContainerInfo *ContainerInfoPtr;
typedef mesos::PerfStatistics *PerfStatisticsPtr;
typedef mesos::Volume *VolumePtr;
typedef mesos::ContainerInfo *ContainerInfoPtr;
typedef std::string *StdStringPtr;

typedef mesos::Label *LabelPtr;
typedef mesos::Port *PortPtr;
typedef mesos::DiscoveryInfo *DiscoveryInfoPtr;

extern "C" {

	extern FrameworkIDPtr toFrameworkID(char* bs, int len);

	extern int fromFrameworkID(FrameworkIDPtr p, char** poke);

	extern void destroyFrameworkID(FrameworkIDPtr p);

	extern OfferIDPtr toOfferID(char* bs, int len);

	extern int fromOfferID(OfferIDPtr p, char** poke);

	extern void destroyOfferID(OfferIDPtr p);

	extern SlaveIDPtr toSlaveID(char* bs, int len);

	extern int fromSlaveID(SlaveIDPtr p, char** poke);

	extern void destroySlaveID(SlaveIDPtr p);

	extern TaskIDPtr toTaskID(char* bs, int len);

	extern int fromTaskID(TaskIDPtr p, char** poke);

	extern void destroyTaskID(TaskIDPtr p);

	extern ExecutorIDPtr toExecutorID(char* bs, int len);

	extern int fromExecutorID(ExecutorIDPtr p, char** poke);

	extern void destroyExecutorID(ExecutorIDPtr p);

	extern ContainerIDPtr toContainerID(char* bs, int len);

	extern int fromContainerID(ContainerIDPtr p, char** poke);

	extern void destroyContainerID(ContainerIDPtr p);

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
						int hostLen,
						char* principal,
						int principalLen);

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
				      int* hostLen,
				      char** principal,
				      int* principalLen);

	extern void destroyFrameworkInfo(FrameworkInfoPtr info);

	extern CommandURIPtr toCommandURI(char* cmd,
					  int cmdLen,
					  bool* executable,
					  bool* extract);

	extern void fromCommandURI(CommandURIPtr commandURI,
				   char** cmd,
				   int* cmdLen,
				   bool* executableSet,
				   bool* executable,
				   bool* extractSet,
				   bool* extract);

	extern void destroyCommandURI(CommandURIPtr commandURI);

  extern MasterInfoPtr toMasterInfo(char* infoID,
				    int infoIDLen,
				    unsigned int infoIP,
				    unsigned int* infoPort,
				    char* pid,
				    int pidLen,
				    char* hostname,
            int hostnameLen,
            char* version,
            int versionLen);

  extern void fromMasterInfo(MasterInfoPtr info,
			     char** infoID,
			     int* infoIDLen,
			     unsigned int* infoIP,
			     unsigned int* infoPort,
			     char** pid,
			     int* pidLen,
			     char** hostname,
                             int* hostnameLen,
  char** version,
                             int* versionLen);

  extern void destroyMasterInfo(MasterInfoPtr info);

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

  extern TaskInfoPtr toTaskInfo(char* infoName,
				int infoNameLen,
				TaskIDPtr taskID,
				SlaveIDPtr slaveID,
				ResourcePtr* resources,
				int resourcesLen,
				ExecutorInfoPtr executorInfo,
				CommandInfoPtr commandInfo,
				char* data,
				int dataLen,
				ContainerInfoPtr containerInfo,
        HealthCheckPtr healthCheck,
        LabelPtr* labels,
        int labelsCount,
        DiscoveryInfoPtr discovery);

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
			   int* dataLen,
			   ContainerInfoPtr* containerInfo,
			   HealthCheckPtr* healthCheck
			   );

  extern void destroyTaskInfo(TaskInfoPtr taskInfo);
  
  extern TaskStatusPtr toTaskStatus(TaskIDPtr taskID,
				    int state,
				    char* message,
				    int messageLen,
				    char* data,
				    int dataLen,
				    SlaveIDPtr slaveID,
				    ExecutorIDPtr executorID,
				    double* timestamp,
				    bool* healthCheck);

	extern void fromTaskStatus(TaskStatusPtr status,
				   TaskIDPtr* taskID,
				   int* state,
				   char** message,
				   int* messageLen,
				   char** data,
				   int* dataLen,
				   SlaveIDPtr* slaveID,
				   ExecutorIDPtr* executorID,
				   bool* timestampSet,
				   double* timestamp,
				   bool* healthCheckSet,
				   bool* healthCheck);

	extern void destroyTaskStatus(TaskStatusPtr taskStatus);	

	extern FiltersPtr toFilters(double* refuseSeconds);

	extern void fromFilters(FiltersPtr filters,
		bool* refusalSet,
		double* refuseSeconds);

	extern void destroyFilters(FiltersPtr filters);

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


	extern EnvironmentPtr toEnvironment(EnvironmentVariablePtr* env,
		int envLen);

	extern void fromEnvironment(EnvironmentPtr env,
		EnvironmentVariablePtr** variables,
		int* envLen);

	extern void destroyEnvironment(EnvironmentPtr env);

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

  extern ResourcePtr toResource(char* name,
                         int nameLen,
                         ValuePtr value,
                         char* role,
                         int roleLen,
                         char* reservationPrincipal,
                         int reservationPrincipalLen,
                         char* diskInfoPersistence,
                         int diskInfoPersistenceLen,
                                VolumePtr diskInfoVolume);

  extern void fromResource(ResourcePtr resource,
                    char** name,
                    int* nameLen,
                    ValuePtr* value,
                    char** role,
                    int* roleLen,
                    char** reservationPrincipal,
                    int* reservationPrincipalLen,
                    char** diskInfoPersistence,
                    int* diskInfoPersistenceLen,
                           VolumePtr* diskInfoVolume);

	extern void destroyResource(ResourcePtr resource);

  extern ExecutorInfoPtr toExecutorInfo(ExecutorIDPtr executorID,
					FrameworkIDPtr frameworkID,
					CommandInfoPtr commandInfo,
					ContainerInfoPtr containerInfo,
					ResourcePtr* resources,
					int resourceLen,
					char* name,
					int nameLen,
					char* source,
					int sourceLen,
          DiscoveryInfoPtr discovery);

  extern void fromExecutorInfo(ExecutorInfoPtr executorInfo,
			       ExecutorIDPtr* executorID,
			       FrameworkIDPtr* frameworkID,
			       CommandInfoPtr* commandInfo,
			       ContainerInfoPtr* containerInfo,
			       ResourcePtr** resources,
			       int* resourcesLen,
			       char** name,
			       int* nameLen,
			       char** source,
			       int* sourceLen,
             DiscoveryInfoPtr* discovery);

	extern void destroyExecutorInfo(ExecutorInfoPtr executorInfo);

	extern AttributePtr toAttribute(char* name,
		int nameLen,
		ValuePtr value);
	extern void fromAttribute(AttributePtr attribute,
		char** name,
		int* nameLen,
		ValuePtr* value);
	extern void destroyAttribute(AttributePtr attribute);

	extern RequestPtr toRequest(SlaveIDPtr slaveID,
		ResourcePtr* resources,
		int resourceLen);

	extern void fromRequest(RequestPtr request,
		SlaveIDPtr* slaveID,
		ResourcePtr** resources,
		int* resourceLen);

	extern void destroyRequest(RequestPtr request);

	extern ValuePtr toValue(int type,
		double scalar,
		ValueRangePtr* lows,
		int rangeLen,
		StdStringPtr* strings,
		int stringsLen,
		char* text,
		int textLen);

	extern void fromValue(ValuePtr value,
		int* type,
		double* scalar,
		ValueRangePtr** ranges,
		int* rangeLen,
		StdStringPtr** strings,
		int* stringsLen,
		char** text,
		int* textLen);

	extern void destroyValue(ValuePtr value);

	extern CommandInfoPtr toCommandInfo(CommandInfo_URIPtr* uris,
					    int urisLen,
					    EnvironmentPtr environment,
					    bool shell,
					    char* value,
					    int valueLen,
					    StdStringPtr* args,
					    int argsLen,
					    char* username,
					    int usernameLen);

	extern void fromCommandInfo(CommandInfoPtr info,
				    CommandInfo_URIPtr** uris,
				    int* urisLen,
				    EnvironmentPtr* environment,
				    bool* shell,
				    char** value,
				    int* valueLen,
				    StdStringPtr** args,
				    int* argsLen,
				    char** username,
				    int* usernameLen);

	extern void destroyCommandInfo(CommandInfoPtr info);

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
						    unsigned long* memoryMappedFileBytes,
						    PerfStatisticsPtr perfStatistics,
						    unsigned long* netRxPackets,
						    unsigned long* netRxBytes,
						    unsigned long* netRxErrors,
						    unsigned long* netRxDropped,
						    unsigned long* netTxPackets,
						    unsigned long* netTxBytes,
						    unsigned long* netTxErrors,
						    unsigned long* netTxDropped
						    );

  extern void fromResourceStatistics(ResourceStatisticsPtr stats,
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
				     bool* memoryMappedFileBytesSet,
				     PerfStatisticsPtr* perfStatistics,
				     unsigned long* netRxPackets,
				     bool* netRxPacketsSet, 
				     unsigned long* netRxBytes,
				     bool* netRxBytesSet, 
				     unsigned long* netRxErrors,
				     bool* netRxErrorsSet, 
				     unsigned long* netRxDropped,
				     bool* netRxDroppedSet, 
				     unsigned long* netTxPackets,
				     bool* netTxPacketsSet, 
				     unsigned long* netTxBytes,
				     bool* netTxBytesSet, 
				     unsigned long* netTxErrors,
				     bool* netTxErrorsSet, 
				     unsigned long* netTxDropped,
				     bool* netTxDroppedSet
				     );
  

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

	extern ParametersPtr toParameters(ParameterPtr* parameters,
		int pLen);
	extern void fromParameters(ParametersPtr params,
		ParameterPtr** parameters,
		int* pLen);
	extern void destroyParameters(ParametersPtr params);

	extern StdStringPtr toStdString(char* str,
		int strLen);

	extern void fromStdString(StdStringPtr sp,
		char** str,
		int* strLen);

	extern void destroyStdString(StdStringPtr sp);

	extern ValueRangePtr toRange(unsigned long low,
		unsigned long high);

	extern void fromRange(ValueRangePtr range,
		unsigned long* lowP,
		unsigned long* highP);

	extern void destroyRange(ValueRangePtr range);

  extern PerfStatisticsPtr toPerfStatistics(
					    double timestamp,
					    double duration,
					    unsigned long* cycles,
					    unsigned long* stalledCyclesFrontend,
					    unsigned long* stalledCyclesBackend,
					    unsigned long* instructions,
					    unsigned long* cacheReferences,
					    unsigned long* cacheMisses,
					    unsigned long* branches,
					    unsigned long* branchMisses,
					    unsigned long* busCycles,
					    unsigned long* refCycles,
					    double* cpuClock,
					    double* taskClock,
					    unsigned long* pageFaults,
					    unsigned long* minorFaults,
					    unsigned long* majorFaults,
					    unsigned long* contextSwitches,
					    unsigned long* cpuMigrations,
					    unsigned long* alignmentFaults,
					    unsigned long* emulationFaults,
					    unsigned long* l1DcacheLoads,
					    unsigned long* l1DcacheLoadMisses,
					    unsigned long* l1DcacheStores,
					    unsigned long* l1DcacheStoreMisses,
					    unsigned long* l1DcachePrefetches,
					    unsigned long* l1DcachePrefetchMisses,
					    unsigned long* l1IcacheLoads,
					    unsigned long* l1IcacheLoadMisses,
					    unsigned long* l1IcachePrefetches,
					    unsigned long* l1IcachePrefetchMisses,
					    unsigned long* llcLoads,
					    unsigned long* llcLoadMisses,
					    unsigned long* llcStores,
					    unsigned long* llcStoreMisses,
					    unsigned long* llcPrefetches,
					    unsigned long* llcPrefetchMisses,
					    unsigned long* dtlbLoads,
					    unsigned long* dtlbLoadMisses,
					    unsigned long* dtlbStores,
					    unsigned long* dtlbStoreMisses,
					    unsigned long* dtlbPrefetches,
					    unsigned long* dtlbPrefetchMisses,
					    unsigned long* itlbLoads,
					    unsigned long* itlbLoadMisses,
					    unsigned long* branchLoads,
					    unsigned long* branchLoadMisses,
					    unsigned long* nodeLoads,
					    unsigned long* nodeLoadMisses,
					    unsigned long* nodeStores,
					    unsigned long* nodeStoreMisses,
					    unsigned long* nodePrefetches,
					    unsigned long* nodePrefetchMisses
					    );

extern void fromPerfStatistics(PerfStatisticsPtr perf,
			       double* timestamp,
			       double* duration,
			       unsigned long* cycles,
			       bool* cyclesSet,
			       unsigned long* stalledCyclesFrontend,
			       bool* stalledCyclesFrontendSet,
			       unsigned long* stalledCyclesBackend,
			       bool* stalledCyclesBackendSet,
			       unsigned long* instructions,
			       bool* instructionsSet,
			       unsigned long* cacheReferences,
			       bool* cacheReferencesSet,
			       unsigned long* cacheMisses,
			       bool* cacheMissesSet,
			       unsigned long* branches,
			       bool* branchesSet,
			       unsigned long* branchMisses,
			       bool* branchMissesSet,
			       unsigned long* busCycles,
			       bool* busCyclesSet,
			       unsigned long* refCycles,
			       bool* refCyclesSet,
			       double* cpuClock,
			       bool* cpuClockSet,
			       double* taskClock,
			       bool* taskClockSet,
			       unsigned long* pageFaults,
			       bool* pageFaultsSet,
			       unsigned long* minorFaults,
			       bool* minorFaultsSet,
			       unsigned long* majorFaults,
			       bool* majorFaultsSet,
			       unsigned long* contextSwitches,
			       bool* contextSwitchesSet,
			       unsigned long* cpuMigrations,
			       bool* cpuMigrationsSet,
			       unsigned long* alignmentFaults,
			       bool* alignmentFaultsSet,
			       unsigned long* emulationFaults,
			       bool* emulationFaultsSet,
			       unsigned long* l1DcacheLoads,
			       bool* l1DcacheLoadsSet,
			       unsigned long* l1DcacheLoadMisses,
			       bool* l1DcacheLoadMissesSet,
			       unsigned long* l1DcacheStores,
			       bool* l1DcacheStoresSet,
			       unsigned long* l1DcacheStoreMisses,
			       bool* l1DcacheStoreMissesSet,
			       unsigned long* l1DcachePrefetches,
			       bool* l1DcachePrefetchesSet,
			       unsigned long* l1DcachePrefetchMisses,
			       bool* l1DcachePrefetchMissesSet,
			       unsigned long* l1IcacheLoads,
			       bool* l1IcacheLoadsSet,
			       unsigned long* l1IcacheLoadMisses,
			       bool* l1IcacheLoadMissesSet,
			       unsigned long* l1IcachePrefetches,
			       bool* l1IcachePrefetchesSet,
			       unsigned long* l1IcachePrefetchMisses,
			       bool* l1IcachePrefetchMissesSet,
			       unsigned long* llcLoads,
			       bool* llcLoadsSet,
			       unsigned long* llcLoadMisses,
			       bool* llcLoadMissesSet,
			       unsigned long* llcStores,
			       bool* llcStoresSet,
			       unsigned long* llcStoreMisses,
			       bool* llcStoreMissesSet,
			       unsigned long* llcPrefetches,
			       bool* llcPrefetchesSet,
			       unsigned long* llcPrefetchMisses,
			       bool* llcPrefetchMissesSet,
			       unsigned long* dtlbLoads,
			       bool* dtlbLoadsSet,
			       unsigned long* dtlbLoadMisses,
			       bool* dtlbLoadMissesSet,
			       unsigned long* dtlbStores,
			       bool* dtlbStoresSet,
			       unsigned long* dtlbStoreMisses,
			       bool* dtlbStoreMissesSet,
			       unsigned long* dtlbPrefetches,
			       bool* dtlbPrefetchesSet,
			       unsigned long* dtlbPrefetchMisses,
			       bool* dtlbPrefetchMissesSet,
			       unsigned long* itlbLoads,
			       bool* itlbLoadsSet,
			       unsigned long* itlbLoadMisses,
			       bool* itlbLoadMissesSet,
			       unsigned long* branchLoads,
			       bool* branchLoadsSet,
			       unsigned long* branchLoadMisses,
			       bool* branchLoadMissesSet,
			       unsigned long* nodeLoads,
			       bool* nodeLoadsSet,
			       unsigned long* nodeLoadMisses,
			       bool* nodeLoadMissesSet,
			       unsigned long* nodeStores,
			       bool* nodeStoresSet,
			       unsigned long* nodeStoreMisses,
			       bool* nodeStoreMissesSet,
			       unsigned long* nodePrefetches,
			       bool* nodePrefetchesSet,
			       unsigned long* nodePrefetchMisses,
			       bool* nodePrefetchMissesSet);

  extern void destroyPerfStatistics(PerfStatisticsPtr perf);

  extern ContainerInfoPtr toContainerInfo (int type,
					   char* image,
					   int imageLen,
					   VolumePtr* volumes,
					   int volumesLen);

  extern void fromContainerInfo (ContainerInfoPtr info,
				 int* type,
				 char** image,
				 int* imageLen,
				 VolumePtr** volumes,
				 int* volumesLen);

  extern void destroyContainerInfo (ContainerInfoPtr containerInfo);

  extern VolumePtr toVolume (char* containerPath,
			     int containerPathLen,
			     char* hostPath,
			     int hostPathLen,
			     int mode);

  extern void fromVolume (VolumePtr p,
			  char** containerPath,
			  int* containerPathLen,
			  char** hostPath,
			  int* hostPathLen,
			  int* mode);

  extern void destroyVolume(VolumePtr volume);

  extern HealthCheckPtr toHealthCheck (bool hasHTTP,
			      int port,
			      char* httpPath,
			      int httpPathSize,
			      unsigned int* statuses,
			      int statusesSize,
			      double* delaySeconds,
			      double* intervalSeconds,
			      double* timeoutSeconds,
			      unsigned int* consecutiveFailures,
			      double* gracePeriodSeconds,
				       CommandInfoPtr command);
  
  extern void fromHealthCheck (HealthCheckPtr p,
		    bool* hasHTTP,
		    int* port,
		    char** httpPath,
		    int* httpPathSize,
		    unsigned int** statuses,
		    int* statusesSize,
		    double* delaySeconds,
		    bool* delaySecondsSet,
		    double* intervalSeconds,
		    bool* intervalSecondsSet,
		    double* timeoutSeconds,
		    bool* timeoutSecondsSet,
		    unsigned int* consecutiveFailures,
		    bool* consecutiveFailuresSet,
		    double* gracePeriodSeconds,
		    bool* gracePeriodSecondsSet,
			       CommandInfoPtr* command);

  extern void destroyHealthCheck (HealthCheckPtr p);

  extern LabelPtr toLabel(char* key,
                      int keyLen,
                      char* value,
                      int valueLen);

  extern void fromLabel(LabelPtr l,
                        char** key,
                        int* keyLen,
                        char** value,
                        int* valueLen);

  extern void destroyLabel(LabelPtr l);

  extern PortPtr toPort(int number,
                        char* name,
                        int nameLen,
                        char* protocol,
                        int protocolLen);
  extern void fromPort(PortPtr port,
                       int* number,
                       char** name,
                       int* nameLen,
                       char** protocol,
                       int* protocolLen);
  extern void destroyPort(PortPtr p);

  extern DiscoveryInfoPtr toDiscoveryInfo(int visibility,
                                          char* name,
                                          int nameLen,
                                          char* env,
                                          int envLen,
                                          char* location,
                                          int locationLen,
                                          char* version,
                                          int versionLen,
                                          PortPtr* ports,
                                          int portsCount,
                                          LabelPtr* labels,
                                          int labelsCount);
 extern void fromDiscoveryInfo(DiscoveryInfoPtr disc,
                               int* visibility,
                               char** name,
                               int* nameLen,
                               char** env,
                               int* envLen,
                               char** location,
                               int* locationLen,
                               char** version,
                               int* versionLen,
                               PortPtr** ports,
                               int* portsCount,
                               LabelPtr** labels,
                               int* labelsCount);

  extern void destroyDiscoveryInfo(DiscoveryInfoPtr disc);
};


#endif
