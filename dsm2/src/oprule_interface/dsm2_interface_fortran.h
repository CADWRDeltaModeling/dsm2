// FORTRAN-C interface for operating rules to discover/manipulate
// FORTRAN model variables using FORTRAN functions/subroutines.
// This file takes care of naming conventions, the actual FORTRAN functions
// are either in model_interface or elsewhere in the model source.
// Note that this file is vendor-specific. If we add/change compilers it
// will almost certainly need to be changed.

#ifdef _WIN32
#define get_external_flow GET_EXTERNAL_FLOW
#define set_external_flow SET_EXTERNAL_FLOW
#define set_external_flow_datasource SET_EXTERNAL_FLOW_DATASOURCE
#define get_transfer_flow GET_TRANSFER_FLOW
#define set_transfer_flow SET_TRANSFER_FLOW
#define set_transfer_flow_datasource SET_TRANSFER_FLOW_DATASOURCE



#define set_gate_install SET_GATE_INSTALL
#define set_gate_install_datasource SET_GATE_INSTALL_DATASOURCE
#define is_gate_install IS_GATE_INSTALL
#define set_device_op_coef SET_DEVICE_OP_COEF
#define get_device_op_coef GET_DEVICE_OP_COEF
#define set_device_op_datasource SET_DEVICE_OP_DATASOURCE

#define set_device_height SET_DEVICE_HEIGHT
#define get_device_height GET_DEVICE_HEIGHT
#define set_device_height_datasource SET_DEVICE_HEIGHT_DATASOURCE
#define set_device_width SET_DEVICE_WIDTH
#define get_device_width GET_DEVICE_WIDTH
#define set_device_width_datasource SET_DEVICE_WIDTH_DATASOURCE
#define set_device_elev SET_DEVICE_ELEV
#define get_device_elev GET_DEVICE_ELEV
#define set_device_elev_datasource SET_DEVICE_ELEV_DATASOURCE
#define set_device_nduplicate SET_DEVICE_NDUPLICATE
#define get_device_nduplicate GET_DEVICE_NDUPLICATE
#define set_device_flow_coef SET_DEVICE_FLOW_COEF
#define get_device_flow_coef GET_DEVICE_FLOW_COEF

#define get_surf_elev CHSTATUS_mp_GLOBALSTREAMSURFACEELEVATION
#define get_flow CHSTATUS_mp_GLOBALSTREAMFLOW
#define get_res_flow RESERVOIRS_mp_GET_RES_FLOW
#define get_res_surf_elev RESERVOIRS_mp_GET_RES_SURF_ELEV
#define ext2int EXT2INT
#define reservoir_index RESNDX
#define reservoir_connect_index RESCONNECTNDX
#define ext2intnode EXT2INTNODE
#define qext_index QEXT_INDEX
#define transfer_index TRANSFER_INDEX
#define chan_comp_point CHAN_COMP_POINT
#define gate_index GATENDX
#define device_index DEVICENDX
#define ts_index TS_INDEX
#define get_chan_velocity TIDEFILE_mp_CHANNELVELOCITY
#define direct_to_node DIRECT_TO_NODE
#define direct_from_node DIRECT_FROM_NODE
#define direct_to_from_node DIRECT_TO_FROM_NODE
#define channel_length CHANNEL_LENGTH
#else

#define get_external_flow get_external_flow_
#define set_external_flow set_external_flow_
#define set_external_flow_datasource set_external_flow_datasource_
#define get_transfer_flow get_transfer_flow_
#define set_transfer_flow set_transfer_flow_
#define set_transfer_flow_datasource set_transfer_flow_datasource_
#define set_gate_install set_gate_install_
#define set_gate_install_datasource set_gate_install_datasource_
#define is_gate_install is_gate_install_
#define set_device_op_coef set_device_op_coef_
#define get_device_op_coef get_device_op_coef_
#define set_device_op_datasource set_device_op_datasource_
#define set_device_height set_device_height_
#define get_device_height get_device_height_
#define set_device_height_datasource set_device_height_datasource_
#define set_device_width set_device_width_
#define get_device_width get_device_width_
#define set_device_width_datasource set_device_width_datasource_
#define set_device_elev set_device_elev_
#define get_device_elev get_device_elev_
#define set_device_elev_datasource set_device_elev_datasource_
#define set_device_nduplicate set_device_nduplicate_
#define get_device_nduplicate get_device_nduplicate_
#define set_device_flow_coef set_device_flow_coef_
#define get_device_flow_coef get_device_flow_coef_
#define get_surf_elev chstatus_mp_globalstreamsurfaceelevation_
#define get_flow chstatus_mp_globalstreamflow_
#define get_res_flow reservoirs_mp_get_res_flow_
#define get_res_surf_elev reservoirs_mp_get_res_surf_elev_
#define ext2int ext2int_
#define reservoir_index resndx_
#define reservoir_connect_index resconnectndx_
#define ext2intnode ext2intnode_
#define qext_index qext_index_
#define transfer_index transfer_index_
#define chan_comp_point chan_comp_point_
#define gate_index gatendx_
#define device_index devicendx_
#define ts_index ts_index_
#define get_chan_velocity tidefile_mp_channelvelocity_
#define direct_to_node direct_to_node_
#define direct_from_node direct_from_node_
#define direct_to_from_node direct_to_from_node_
#define channel_length channel_length_
#endif

// routines for retrieving indexes and converting external
// numbers to internal ones

#define STDCALL
extern "C" int STDCALL gate_index(const char* name,
                                    unsigned int len);
extern "C" int STDCALL device_index(const int& gateno,
                                    const char* name,
                                    unsigned int len);
extern "C" int STDCALL ext2int(const int& extchan);
extern "C" int STDCALL ext2intnode(const int& extres);
extern "C" int STDCALL reservoir_index(const char* name,
                                     unsigned int len);
extern "C" int STDCALL reservoir_connect_index(const int& resndx, const int& internal_node);

extern "C" void STDCALL chan_comp_point(const int& intchan,
                                          const double& distance,
                                          int points[],
                                          double weights[]);
extern "C" double STDCALL channel_length(const int & intchan);

extern "C" int STDCALL ts_index(const char* name, unsigned int len);
extern "C" int STDCALL qext_index(const char* name, unsigned int len);
extern "C" int STDCALL transfer_index(const char* name, unsigned int len);

extern "C" int STDCALL direct_to_node();
extern "C" int STDCALL direct_from_node();
extern "C" int STDCALL direct_to_from_node();


///////////////////////////

// Model variable interfaces

extern "C" double STDCALL get_external_flow(const int& ndx);
extern "C" void STDCALL set_external_flow(const int& ndx,
                                              const double& val);
extern "C" void STDCALL set_external_flow_datasource(const int& ndx,
                                              const int& expr,
                                              const double& val,
                                              const bool& timedep);
extern "C" double STDCALL get_transfer_flow(const int& ndx);
extern "C" void STDCALL set_transfer_flow(const int& ndx,
                                              const double& val);
extern "C" void STDCALL set_transfer_flow_datasource(const int& ndx,
                                              const int& expr,
                                              const double& val,
                                              const bool& timedep);




extern "C" double STDCALL is_gate_install(const int& ndx);

extern "C" void STDCALL set_gate_install(const int& ndx,
                                             const double& install);
extern "C" void STDCALL set_gate_install_datasource(const int& ndx,
                                                      const int& expr,
                                                      const int& val,
                                                      const bool& timedep);

extern "C" double get_surf_elev(const int& comp_pt);
extern "C" double get_flow(const int& comp_pt);
extern "C" double get_res_flow(const int& resndx,
                                         const int& conn);
extern "C" double get_res_surf_elev(const int& resndx);
extern "C" double STDCALL get_device_op_coef(const int& ndx,
                                             const int& devndx,
											 const int& direct);

extern "C" void STDCALL set_device_op_coef(const int& ndx,
                                           const int& devndx,
										   const int& direct,
                                           const double& val);
extern "C" void STDCALL set_device_op_datasource(const int& ndx,
                                           const int& devndx,
										   const int& direct,
                                           const int& expr,
                                           const double& val,
                                           const bool& timedep);


extern "C" double STDCALL get_device_height(const int& ndx,
                                             const int& devndx);

extern "C" void STDCALL set_device_height(const int& ndx,
                                           const int& devndx,
                                           const double& val);
extern "C" void STDCALL set_device_height_datasource(const int& ndx,
                                           const int& devndx,
                                           const int& expr,
                                           const double& val,
                                           const bool& timedep);


extern "C" double STDCALL get_device_elev(const int& ndx,
                                             const int& devndx);

extern "C" void STDCALL set_device_elev(const int& ndx,
                                           const int& devndx,
                                           const double& val);
extern "C" void STDCALL set_device_elev_datasource(const int& ndx,
                                           const int& devndx,
                                           const int& expr,
                                           const double& val,
                                           const bool& timedep);

extern "C" double STDCALL get_device_width(const int& ndx,
                                             const int& devndx);

extern "C" void STDCALL set_device_width(const int& ndx,
                                           const int& devndx,
                                           const double& val);
extern "C" void STDCALL set_device_width_datasource(const int& ndx,
                                           const int& devndx,
                                           const int& expr,
                                           const double& val,
                                           const bool& timedep);


extern "C" double STDCALL get_device_nduplicate(const int& ndx,
                                             const int& devndx);

extern "C" void STDCALL set_device_nduplicate(const int& ndx,
                                           const int& devndx,
                                           const double& val);

extern "C" double STDCALL get_device_flow_coef(const int& ndx,
                                             const int& devndx,
											 const int& direction);

extern "C" void STDCALL set_device_flow_coef(const int& ndx,
                                           const int& devndx,
										   const int& direction,
                                           const double& val
										   );


extern "C" double STDCALL get_chan_velocity(const int&, const double&);
