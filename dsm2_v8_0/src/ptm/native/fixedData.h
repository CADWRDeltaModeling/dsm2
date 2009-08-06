#ifdef __sun // Sun version
#define STDCALL
#define STRLEN_TYPE long int
#define initFixedData init_fixed_data_
#define updateNodeInfo updatenodeinfo_
#define updateWBInfo updatewbinfo_
#define getMaximumNumberOfWaterbodies get_maximum_number_of_waterbodies_
#define getMaximumNumberOfChannels get_maximum_number_of_channels_
#define getMaximumNumberOfReservoirs get_maximum_number_of_reservoirs_
#define getMaximumNumberOfDiversions get_maximum_number_of_diversions_
#define getMaximumNumberOfPumps get_maximum_number_of_pumps_
#define getMaximumNumberOfBoundaryWaterbodies get_maximum_number_of_boundary_waterbodies_
#define getMaximumNumberOfStageBoundaries get_maximum_number_of_stage_boundaries_
#define getMaximumNumberOfConveyors get_maximum_number_of_conveyors_
#define getMaximumNumberOfNodes get_maximum_number_of_nodes_
#define getMaximumNumberOfXsections get_maximum_number_of_xsections_
#define getMaximumNumberOfReservoirNodes get_maximum_number_of_reservoir_nodes_
#define getNumberOfWaterbodies get_number_of_waterbodies_
#define getNumberOfChannels get_number_of_channels_
#define getNumberOfChannelGroups get_number_of_channel_groups_
#define getNumberOfReservoirs get_number_of_reservoirs_
#define getNumberOfDiversions get_number_of_diversions_
#define getNumberOfPumps get_number_of_pumps_
#define getNumberOfBoundaryWaterbodies get_number_of_boundary_waterbodies_
#define getNumberOfStageBoundaries get_number_of_stage_boundaries_
#define getNumberOfConveyors get_number_of_conveyors_
#define getNumberOfNodes get_number_of_nodes_
#define getNumberOfXsections get_number_of_xsections_
#define getUniqueIdForChannel get_unique_id_for_channel_
#define getUniqueIdForReservoir get_unique_id_for_reservoir_
#define getUniqueIdForBoundary get_unique_id_for_boundary_
#define getUniqueIdForStageBoundary get_unique_id_for_stage_boundary_
#define getUniqueIdForConveyor get_unique_id_for_conveyor_
#define getWaterbodyType get_waterbody_type_
#define getWaterbodyGroup get_waterbody_group_
#define getLocalIdForWaterbody get_local_id_for_waterbody_
#define getNumberOfNodesForWaterbody get_number_of_nodes_for_waterbody_
#define getNodeArrayForWaterbody get_node_array_for_waterbody_
#define getChannelLength get_channel_length_
#define getChannelNumberOfNodes get_channel_number_of_nodes_
#define getChannelNodeArray get_channel_node_array_
#define getChannelNumberOfXsections get_channel_number_of_xsections_
#define getChannelXsectionIds get_channel_xsection_ids_
#define getChannelXsectionDistances get_channel_xsection_distances_
#define getReservoirArea get_reservoir_area_
#define getReservoirBottomElevation get_reservoir_bottom_elevation_
#define getReservoirName get_reservoir_name_
#define getReservoirNumberOfNodes get_reservoir_number_of_nodes_
#define getReservoirNodeArray get_reservoir_node_array_
#define getDiversionNumberOfNodes get_diversion_number_of_nodes_
#define getPumpNumberOfNodes get_pump_number_of_nodes_
#define getBoundaryWaterbodyNumberOfNodes get_boundary_waterbody_number_of_nodes_
#define getConveyorNumberOfNodes get_conveyor_number_of_nodes_
#define getDiversionNodeArray get_diversion_node_array_
#define getPumpNodeArray get_pump_node_array_
#define getBoundaryWaterbodyNodeArray get_boundary_waterbody_node_array_
#define getConveyorNodeArray get_conveyor_node_array_
#define getNumberOfWaterbodiesForNode get_number_of_waterbodies_for_node_
#define getBoundaryTypeForNode get_boundary_type_for_node_
#define getWaterbodyIdArrayForNode get_waterbody_id_array_for_node_
#define getXsectionNumberOfElevations get_xsection_number_of_elevations_
#define getXsectionWidths get_xsection_widths_
#define getXsectionElevations get_xsection_elevations_
#define getXsectionAreas get_xsection_areas_
#define getXsectionMinimumElevation get_xsection_minimum_elevation_
#define getParticleBooleanInputs get_particle_boolean_inputs_
#define getParticleFloatInputs get_particle_float_inputs_
#define getParticleNumberOfInjections get_particle_number_of_injections_
#define getParticleInjectionNodes get_particle_injection_nodes_
#define getParticleNumberOfParticlesInjected get_particle_number_of_particles_injected_
#define getParticleInjectionStartJulmin get_particle_injection_start_julmin_
#define getParticleInjectionLengthJulmin get_particle_injection_length_julmin_
#define getNumberOfFluxes get_number_of_fluxes_
#define getNumberIncoming get_number_incoming_
#define getNumberOutgoing get_number_outgoing_
#define getFluxIncoming get_flux_incoming_
#define getFluxOutgoing get_flux_outgoing_
#define getFluxIncomingType get_flux_incoming_type_
#define getFluxOutgoingType get_flux_outgoing_type_
#define getNumberOfGroupOutputs get_number_of_group_outputs_
#define getNumberOfGroupMembers get_number_of_group_members_
#define getGroupMemberType get_group_member_type_
#define getGroupMemberIndex get_group_member_index_
#define getModelStartTime get_model_start_time_
#define getModelEndTime get_model_end_time_
#define getModelPtmTimeStep get_model_ptm_time_step_
#define getDisplayInterval get_display_interval_
#define getAnimationFilename get_animation_filename_
#define getModelAnimationOutputInterval get_model_animation_output_interval_
#define getBehaviorFilename get_behavior_filename_
#define getTraceFilename get_trace_filename_
#define getModelTraceOutputInterval get_model_trace_output_interval_
#define getRestartOutputFilename get_restart_output_filename_
#define getRestartOutputInterval get_restart_output_interval_
#define getRestartInputFilename get_restart_input_filename_
#define getModelDate get_model_date_
#define getModelTime get_model_time_
#define cdt2jmin cdt2jmin_
#define initFluxOutput init_flux_output_
#define writeFluxOutput write_flux_output_
#define closeFluxOutput close_flux_output_
#define setFlux set_flux_
#define setGroup set_group_
#define getWaterbodyAccountingType get_waterbody_accounting_type_
#define getWaterbodyObjectType get_waterbody_object_type_
#define doesQualBinaryExist does_qual_binary_exist_
#define getQualConstituentNames get_qual_constituent_names_
#define getNumberConstituents get_number_constituents_
#define STDCALL
#else // Microsoft C++ version
//#define STDCALL __stdcall
#define STDCALL
#define STRLEN_TYPE int
#define initFixedData INIT_FIXED_DATA
#define updateNodeInfo UPDATENODEINFO
#define updateWBInfo UPDATEWBINFO
#define getMaximumNumberOfWaterbodies GET_MAXIMUM_NUMBER_OF_WATERBODIES
#define getMaximumNumberOfChannels GET_MAXIMUM_NUMBER_OF_CHANNELS
#define getMaximumNumberOfReservoirs GET_MAXIMUM_NUMBER_OF_RESERVOIRS
#define getMaximumNumberOfDiversions GET_MAXIMUM_NUMBER_OF_DIVERSIONS
#define getMaximumNumberOfPumps GET_MAXIMUM_NUMBER_OF_PUMPS
#define getMaximumNumberOfBoundaryWaterbodies GET_MAXIMUM_NUMBER_OF_BOUNDARY_WATERBODIES
#define getMaximumNumberOfStageBoundaries GET_MAXIMUM_NUMBER_OF_STAGE_BOUNDARIES
#define getMaximumNumberOfConveyors GET_MAXIMUM_NUMBER_OF_CONVEYORS
#define getMaximumNumberOfNodes GET_MAXIMUM_NUMBER_OF_NODES
#define getMaximumNumberOfXsections GET_MAXIMUM_NUMBER_OF_XSECTIONS
#define getMaximumNumberOfReservoirNodes GET_MAXIMUM_NUMBER_OF_RESERVOIR_NODES
#define getNumberOfWaterbodies GET_NUMBER_OF_WATERBODIES
#define getNumberOfChannels GET_NUMBER_OF_CHANNELS
#define getNumberOfChannelGroups GET_NUMBER_OF_CHANNEL_GROUPS
#define getNumberOfReservoirs GET_NUMBER_OF_RESERVOIRS
#define getNumberOfDiversions GET_NUMBER_OF_DIVERSIONS
#define getNumberOfPumps GET_NUMBER_OF_PUMPS
#define getNumberOfBoundaryWaterbodies GET_NUMBER_OF_BOUNDARY_WATERBODIES
#define getNumberOfStageBoundaries GET_NUMBER_OF_STAGE_BOUNDARIES
#define getNumberOfConveyors GET_NUMBER_OF_CONVEYORS
#define getNumberOfNodes GET_NUMBER_OF_NODES
#define getNumberOfXsections GET_NUMBER_OF_XSECTIONS
#define getUniqueIdForChannel GET_UNIQUE_ID_FOR_CHANNEL
#define getUniqueIdForReservoir GET_UNIQUE_ID_FOR_RESERVOIR
#define getUniqueIdForBoundary GET_UNIQUE_ID_FOR_BOUNDARY
#define getUniqueIdForStageBoundary GET_UNIQUE_ID_FOR_STAGE_BOUNDARY
#define getUniqueIdForConveyor GET_UNIQUE_ID_FOR_CONVEYOR
#define getWaterbodyType GET_WATERBODY_TYPE
#define getWaterbodyGroup GET_WATERBODY_GROUP
#define getLocalIdForWaterbody GET_LOCAL_ID_FOR_WATERBODY
#define getNumberOfNodesForWaterbody GET_NUMBER_OF_NODES_FOR_WATERBODY
#define getNodeArrayForWaterbody GET_NODE_ARRAY_FOR_WATERBODY
#define getChannelLength GET_CHANNEL_LENGTH
#define getChannelNumberOfNodes GET_CHANNEL_NUMBER_OF_NODES
#define getChannelNodeArray GET_CHANNEL_NODE_ARRAY
#define getChannelNumberOfXsections GET_CHANNEL_NUMBER_OF_XSECTIONS
#define getChannelXsectionIds GET_CHANNEL_XSECTION_IDS
#define getChannelXsectionDistances GET_CHANNEL_XSECTION_DISTANCES
#define getReservoirArea GET_RESERVOIR_AREA
#define getReservoirBottomElevation GET_RESERVOIR_BOTTOM_ELEVATION
#define getReservoirName GET_RESERVOIR_NAME
#define getReservoirNumberOfNodes GET_RESERVOIR_NUMBER_OF_NODES
#define getReservoirNodeArray GET_RESERVOIR_NODE_ARRAY
#define getDiversionNumberOfNodes GET_DIVERSION_NUMBER_OF_NODES
#define getPumpNumberOfNodes GET_PUMP_NUMBER_OF_NODES
#define getBoundaryWaterbodyNumberOfNodes GET_BOUNDARY_WATERBODY_NUMBER_OF_NODES
#define getConveyorNumberOfNodes GET_CONVEYOR_NUMBER_OF_NODES
#define getDiversionNodeArray GET_DIVERSION_NODE_ARRAY
#define getPumpNodeArray GET_PUMP_NODE_ARRAY
#define getBoundaryWaterbodyNodeArray GET_BOUNDARY_WATERBODY_NODE_ARRAY
#define getConveyorNodeArray GET_CONVEYOR_NODE_ARRAY
#define getNumberOfWaterbodiesForNode GET_NUMBER_OF_WATERBODIES_FOR_NODE
#define getBoundaryTypeForNode GET_BOUNDARY_TYPE_FOR_NODE
#define getWaterbodyIdArrayForNode GET_WATERBODY_ID_ARRAY_FOR_NODE
#define getXsectionNumberOfElevations GET_XSECTION_NUMBER_OF_ELEVATIONS
#define getXsectionWidths GET_XSECTION_WIDTHS
#define getXsectionElevations GET_XSECTION_ELEVATIONS
#define getXsectionAreas GET_XSECTION_AREAS
#define getXsectionMinimumElevation GET_XSECTION_MINIMUM_ELEVATION
#define getParticleBooleanInputs GET_PARTICLE_BOOLEAN_INPUTS
#define getParticleFloatInputs GET_PARTICLE_FLOAT_INPUTS
#define getParticleNumberOfInjections GET_PARTICLE_NUMBER_OF_INJECTIONS
#define getParticleInjectionNodes GET_PARTICLE_INJECTION_NODES
#define getParticleNumberOfParticlesInjected GET_PARTICLE_NUMBER_OF_PARTICLES_INJECTED
#define getParticleInjectionStartJulmin GET_PARTICLE_INJECTION_START_JULMIN
#define getParticleInjectionLengthJulmin GET_PARTICLE_INJECTION_LENGTH_JULMIN
#define getNumberOfFluxes GET_NUMBER_OF_FLUXES
#define getNumberIncoming GET_NUMBER_INCOMING
#define getNumberOutgoing GET_NUMBER_OUTGOING
#define getFluxIncoming GET_FLUX_INCOMING
#define getFluxOutgoing GET_FLUX_OUTGOING
#define getFluxIncomingType GET_FLUX_INCOMING_TYPE
#define getFluxOutgoingType GET_FLUX_OUTGOING_TYPE
#define getFluxIncomingAccountType GET_FLUX_INCOMING_ACCOUNT_TYPE
#define getFluxOutgoingAccountType GET_FLUX_OUTGOING_ACCOUNT_TYPE
#define getNumberOfGroupOutputs GET_NUMBER_OF_GROUP_OUTPUTS
#define getNumberOfGroupMembers GET_NUMBER_OF_GROUP_MEMBERS
#define getGroupMemberType GET_GROUP_MEMBER_TYPE
#define getGroupMemberIndex GET_GROUP_MEMBER_INDEX

#define getModelStartTime GET_MODEL_START_TIME
#define getModelEndTime GET_MODEL_END_TIME
#define getModelPtmTimeStep GET_MODEL_PTM_TIME_STEP
#define getDisplayInterval GET_DISPLAY_INTERVAL
#define getAnimationFilename GET_ANIMATION_FILENAME
#define getModelAnimationOutputInterval GET_MODEL_ANIMATION_OUTPUT_INTERVAL
#define getBehaviorFilename GET_BEHAVIOR_FILENAME
#define getTraceFilename GET_TRACE_FILENAME
#define getModelTraceOutputInterval GET_MODEL_TRACE_OUTPUT_INTERVAL
#define getRestartOutputFilename GET_RESTART_OUTPUT_FILENAME
#define getRestartOutputInterval GET_RESTART_OUTPUT_INTERVAL
#define getRestartInputFilename GET_RESTART_INPUT_FILENAME
#define getModelDate GET_MODEL_DATE
#define getModelTime GET_MODEL_TIME
#define cdt2jmin CDT2JMIN
#define initFluxOutput INIT_FLUX_OUTPUT
#define writeFluxOutput WRITE_FLUX_OUTPUT
#define closeFluxOutput CLOSE_FLUX_OUTPUT
#define setFlux SET_FLUX
#define setGroup SET_GROUP
#define getWaterbodyAccountingType GET_WATERBODY_ACCOUNTING_TYPE
#define getWaterbodyObjectType GET_WATERBODY_OBJECT_TYPE
#define doesQualBinaryExist DOES_QUAL_BINARY_EXIST
#define getQualConstituentNames GET_QUAL_CONSTITUENT_NAMES
#define getNumberConstituents GET_NUMBER_CONSTITUENTS
#endif


//const int LEN1 = 50;
//const int LEN2 = 80;
//const int LEN3 = 180;
//const int LEN4 = 20;
#define LEN1 50
#define LEN2 80
#define LEN3 180
#define LEN4 20




#ifdef __cplusplus
extern "C" {
#endif
  /*
    Fortran subroutine to read fixed input from file
    specified in environment variable DSM2INPUT
    This subroutine updates the common block which is
    later read by PTMFixedInput class
  */
  void STDCALL initFixedData(char * filename, STRLEN_TYPE filenameLength);
  void STDCALL updateNodeInfo();
  void STDCALL updateWBInfo();
  int STDCALL getMaximumNumberOfWaterbodies();
  int STDCALL getMaximumNumberOfChannels();
  int STDCALL getMaximumNumberOfReservoirs();
  int STDCALL getMaximumNumberOfDiversions();
  int STDCALL getMaximumNumberOfPumps();
  int STDCALL getMaximumNumberOfBoundaryWaterbodies();
  int STDCALL getMaximumNumberOfStageBoundaries();
  int STDCALL getMaximumNumberOfConveyors();
  int STDCALL getMaximumNumberOfNodes();
  int STDCALL getMaximumNumberOfXsections();
  int STDCALL getMaximumNumberOfReservoirNodes();
  //
  int STDCALL getNumberOfWaterbodies();
  int STDCALL getNumberOfChannels();
  int STDCALL getNumberOfChannelGroups();
  int STDCALL getNumberOfReservoirs();
  int STDCALL getNumberOfDiversions();
  int STDCALL getNumberOfPumps();
  int STDCALL getNumberOfBoundaryWaterbodies();
  int STDCALL getNumberOfStageBoundaries();
  int STDCALL getNumberOfConveyors();
  int STDCALL getNumberOfNodes();
  int STDCALL getNumberOfXsections();
  //
  int STDCALL getUniqueIdForChannel(int * localIndex);
  int STDCALL getUniqueIdForReservoir(int * localIndex);
  int STDCALL getUniqueIdForBoundary(int * localIndex);
  int STDCALL getUniqueIdForStageBoundary(int * localIndex);
  int STDCALL getUniqueIdForConveyor(int * localIndex);
  //
  int STDCALL getWaterbodyType(int * uniqId);
  int STDCALL getWaterbodyGroup(int * uniqId);
  int STDCALL getLocalIdForWaterbody(int * uniqId);
  int STDCALL getNumberOfNodesForWaterbody(int * uniqId);
  void STDCALL getNodeArrayForWaterbody(int * uniqId, int * nodeArray);
  //
  int STDCALL getChannelLength(int* channelNumber);
  int STDCALL getChannelNumberOfNodes(int* channelNumber);
  void STDCALL getChannelNodeArray(int* channelNumber, int * nodeArray);
  int STDCALL getChannelNumberOfXsections(int* channelNumber); 
  void STDCALL getChannelXsectionIds(int* channelNumber, int *xSectionIds);
  void STDCALL getChannelXsectionDistances(int* channelNumber, float *xSectionDistances);
  
  float STDCALL getReservoirArea(int* reservoirNumber);
  float STDCALL getReservoirBottomElevation(int* reservoirNumber);
  void STDCALL getReservoirName(int* reservoirNumber, char * name, STRLEN_TYPE nameLength);
  int STDCALL getReservoirNumberOfNodes(int* reservoirNumber);
  void STDCALL getReservoirNodeArray(int* reservoirNumber, int *nodeArray);
  
  int STDCALL getDiversionNumberOfNodes(int* number);
  int STDCALL getPumpNumberOfNodes(int* number);
  int STDCALL getBoundaryWaterbodyNumberOfNodes(int* number);
  int STDCALL getConveyorNumberOfNodes(int* number);
  
  void STDCALL getDiversionNodeArray(int* diversionNumber, int *nodeArray);
  void STDCALL getPumpNodeArray(int* pumpNumber, int * nodeArray);
  void STDCALL getBoundaryWaterbodyNodeArray(int* boundaryNumber, int * nodeArray);
  void STDCALL getConveyorNodeArray(int* number, int * nodeArray);
  
  int STDCALL getNumberOfWaterbodiesForNode(int* nodeNumber);
  void STDCALL getBoundaryTypeForNode(int* nodeNumber, char * name, STRLEN_TYPE nameLength);
  void STDCALL getWaterbodyIdArrayForNode(int* nodeNumber, int *waterbodyIdArray);
  
  int STDCALL getXsectionNumberOfElevations();
  void STDCALL getXsectionWidths(int* number, float *array);
  void STDCALL getXsectionElevations(int* number, float *array);
  void STDCALL getXsectionAreas(int* number, float *array);
  float STDCALL getXsectionMinimumElevation(int* number);
  
  void STDCALL getParticleBooleanInputs(int *array);
  void STDCALL getParticleFloatInputs(float *array);
  
  int STDCALL getParticleNumberOfInjections();
  void STDCALL getParticleInjectionNodes(int *array);
  void STDCALL getParticleNumberOfParticlesInjected(int *array);
  void STDCALL getParticleInjectionStartJulmin(int *array);
  void STDCALL getParticleInjectionLengthJulmin(int *array);
  
  int STDCALL getNumberOfFluxes();
  int STDCALL getNumberIncoming(int* index);
  int STDCALL getNumberOutgoing(int* index);
  void STDCALL getFluxIncoming(int* index,int *array, int* nmember);
  void STDCALL getFluxOutgoing(int* index,int *array, int* nmember);
  void STDCALL getFluxIncomingType(int* index,int *array, int* nmember);
  void STDCALL getFluxOutgoingType(int* index,int *array, int* nmember);
  
  int STDCALL getNumberOfGroupOutputs();
  int STDCALL getNumberOfGroupMembers(int* index);
  void STDCALL getGroupMemberType(int* index,int *array, int* nmember);
  void STDCALL getGroupMemberIndex(int* index,int *array, int* nmember);


  int STDCALL getModelStartTime();
  int STDCALL getModelEndTime();
  int STDCALL getModelPtmTimeStep();
  int STDCALL getDisplayInterval();
  
  void STDCALL getAnimationFilename(char *array, STRLEN_TYPE arrayLength);
  int STDCALL getModelAnimationOutputInterval();
  void STDCALL getBehaviorFilename(char *array, STRLEN_TYPE arrayLength);
  void STDCALL getTraceFilename(char *array, STRLEN_TYPE arrayLength);
  int STDCALL getModelTraceOutputInterval();
  void STDCALL getRestartOutputFilename(char *array, STRLEN_TYPE arrayLength);
  int STDCALL getRestartOutputInterval();
  void STDCALL getRestartInputFilename(char *array, STRLEN_TYPE arrayLength);
  
  void STDCALL getModelDate(int * jmin, char *date, STRLEN_TYPE dateLength);
  void STDCALL getModelTime(int * jmin, char *time, STRLEN_TYPE timeLength);
  int STDCALL cdt2jmin(char *dateTime, STRLEN_TYPE dateTimeLength);
  
  void STDCALL initFluxOutput();
  void STDCALL writeFluxOutput();
  void STDCALL closeFluxOutput();
  void STDCALL setFlux(int * fluxId, float * fluxValue);
  void STDCALL setGroup(int * groupId, float * groupValue);
  
  int STDCALL getWaterbodyAccountingType( int *  id);
  int STDCALL getWaterbodyObjectType( int *  id);
  int STDCALL doesQualBinaryExist();
  void STDCALL getQualConstituentNames(int* conNum, char* array, STRLEN_TYPE arrayLength);
  int STDCALL getNumberConstituents();
#ifdef __cplusplus
	   }
#endif
