// FORTRAN-C interface for operating rules to discover/manipulate
// FORTRAN model variables using FORTRAN functions/subroutines.
// This file takes care of naming conventions, the actual FORTRAN functions
// are either in model_interface or elsewhere in the model source.
// Note that this file is vendor-specific. If we add/change compilers it
// will almost certainly need to be changed.

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
extern "C" int STDCALL get_oprule_log_level();

// options of the oprule log (SCALAR table) and the tide file gate state; model_interface.f90
extern "C" int STDCALL get_oprule_log_file(char* buf, const int& buflen);
extern "C" int STDCALL get_oprule_log_text();
extern "C" int STDCALL get_oprule_log_devices();
extern "C" int STDCALL get_oprule_log_context();
extern "C" double STDCALL get_oprule_log_tol_op();
extern "C" double STDCALL get_oprule_log_tol_dim();
extern "C" int STDCALL get_oprule_log_trace_interval();
extern "C" double STDCALL get_oprule_log_flush_hours();
extern "C" int STDCALL get_tidefile_gate_state();
extern "C" int STDCALL get_hydro_tidefile_name(char* buf, const int& buflen);

// gate tables and state for the oprule log; model_interface.f90 (names are copied NUL terminated into buf,
// the return value is the length)
extern "C" int STDCALL get_gate_count();
extern "C" int STDCALL get_gate_device_count(const int& gndx);
extern "C" int STDCALL get_gate_name(const int& gndx, char* buf, const int& buflen);
extern "C" int STDCALL get_device_name(const int& gndx, const int& devndx, char* buf, const int& buflen);
extern "C" int STDCALL get_device_structure_type(const int& gndx, const int& devndx);
extern "C" int STDCALL get_gate_object_name(const int& gndx, char* buf, const int& buflen);
extern "C" int STDCALL get_gate_node_id(const int& gndx);
extern "C" void STDCALL get_gate_connection(const int& gndx, int& objtype, int& objid, int& compoint,
                                            int& nodecompoint);
extern "C" double STDCALL get_gate_flow(const int& gndx);
extern "C" double STDCALL get_device_property(const int& gndx, const int& devndx, const int& prop);
extern "C" int STDCALL get_device_source(const int& gndx, const int& devndx, const int& prop, char* buf,
                                         const int& buflen);


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
extern "C" void STDCALL set_device_nduplicate_datasource(const int& ndx,
                                           const int& devndx,
                                           const int& expr,
                                           const double& val,
                                           const bool& timedep);

extern "C" double STDCALL get_device_flow_coef(const int& ndx,
                                             const int& devndx,
											 const int& direction);

extern "C" void STDCALL set_device_flow_coef(const int& ndx,
                                           const int& devndx,
										   const int& direction,
                                           const double& val
										   );


extern "C" double STDCALL get_chan_velocity(const int&, const double&);
