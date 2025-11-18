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
