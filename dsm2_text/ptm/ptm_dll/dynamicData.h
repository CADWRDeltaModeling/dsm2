#ifdef __sun
#define STDCALL
#define STRLEN_TYPE long int
#define readMultTide read_mult_tide_
#define setTideFileTime set_tide_file_time_
#define getExtFromInt get_ext_from_int_
#define getUpNodeDepth get_up_node_depth_
#define getDownNodeDepth get_down_node_depth_
#define getUpNodeStage get_up_node_stage_
#define getDownNodeStage get_down_node_stage_
#define getUpNodeFlow get_up_node_flow_
#define getDownNodeFlow get_down_node_flow_
#define getUpNodeArea get_up_node_area_
#define getDownNodeArea get_down_node_area_
#define getReservoirVolume get_reservoir_volume_
#define getNodeNumberForConnection get_node_number_for_connection_
#define getResevoirDepth get_resevoir_depth_
#define getReservoirFlowForConnection get_reservoir_flow_for_connection_
#define getDiversionAtNode get_diversion_at_node_
#define getReservoirPumping get_reservoir_pumping_
#define getBoundaryFlow get_boundary_flow_
#define getStageBoundaryFlow get_stage_boundary_flow_
#define getConveyorFlow get_conveyor_flow_
#else // Microsoft C++ version
#define STDCALL 
#define STRLEN_TYPE int
#define readMultTide READ_MULT_TIDE
#define setTideFileTime SET_TIDE_FILE_TIME
#define getExtFromInt GET_EXT_FROM_INT
#define getUpNodeDepth GET_UP_NODE_DEPTH
#define getDownNodeDepth GET_DOWN_NODE_DEPTH
#define getUpNodeStage GET_UP_NODE_STAGE
#define getDownNodeStage GET_DOWN_NODE_STAGE
#define getUpNodeFlow GET_UP_NODE_FLOW
#define getDownNodeFlow GET_DOWN_NODE_FLOW
#define getUpNodeArea GET_UP_NODE_AREA
#define getDownNodeArea GET_DOWN_NODE_AREA
#define getReservoirVolume GET_RESERVOIR_VOLUME
#define getNodeNumberForConnection GET_NODE_NUMBER_FOR_CONNECTION
#define getResevoirDepth GET_RESEVOIR_DEPTH
#define getReservoirFlowForConnection GET_RESERVOIR_FLOW_FOR_CONNECTION
#define getDiversionAtNode GET_DIVERSION_AT_NODE
#define getReservoirPumping GET_RESERVOIR_PUMPING
#define getBoundaryFlow GET_BOUNDARY_FLOW
#define getStageBoundaryFlow GET_STAGE_BOUNDARY_FLOW
#define getConveyorFlow GET_CONVEYOR_FLOW
#endif

//
#ifdef __cplusplus
extern "C" {
#endif
////////////
//External fortran function to read header file and update
//fortran common blocks dsm2TideCom
void STDCALL readMultTide();
/////////////
//External fortran function to set model time in fortran common
//blocks before reading tide file information
void STDCALL setTideFileTime(int* modelTime);

int STDCALL getExtFromInt(int * internal);
float STDCALL getUpNodeDepth( int * number);
float STDCALL getDownNodeDepth( int * number);
float STDCALL getUpNodeStage( int * number);
float STDCALL getDownNodeStage( int * number);
float STDCALL getUpNodeFlow( int * number);
float STDCALL getDownNodeFlow( int * number);
float STDCALL getUpNodeArea( int * number);
float STDCALL getDownNodeArea( int * number);
float STDCALL getReservoirVolume( int * number);
int STDCALL getNodeNumberForConnection( int * reservoirNumber, int * connection);
float STDCALL getResevoirDepth( int * number);
float STDCALL getReservoirFlowForConnection( int * reservoirNumber, int * connection);
int STDCALL getDiversionAtNode( int * number);
int STDCALL getReservoirPumping( int * number);
float STDCALL getBoundaryFlow( int * number);
float STDCALL getStageBoundaryFlow( int * number);
float STDCALL getConveyorFlow(  int * number);
#ifdef __cplusplus
}
#endif
