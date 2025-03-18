#ifndef PYDSM2HYDRO_H
#define PYDSM2HYDRO_H

#ifdef __cplusplus
extern "C" {
#endif

/* Declare Fortran functions with appropriate name mangling */
void prepare_hydro();
void fourpt_init();
void fourpt_step();
void fourpt_winddown();
// void dsm2_init(char* name, int* status);
// void dsm2_setup_hydro(char* name, int* status);
// void dsm2_start_hydro(int* status);
// void dsm2_resume_hydro(int* status);
// void dsm2_transfer_flow_hydro(int* status);
// void dsm2_advance_hydro(int* status);
// void dsm2_close_hydro(int* status);
// void get_channel_flow(int* chan_no, double* flow, int* status);
// void get_node_flow(int* node_no, double* flow, int* status);
// void set_channel_flow(int* chan_no, double* flow, int* status);
// void set_gate_height(int* gate_no, double* height, int* status);
// void set_gate_position(int* gate_no, double* op_to_node, int* status);
// void get_reservoir_flow(int* res_no, double* flow, int* status);

extern char init_input_file[130];
extern int julmin;
extern int end_julmin;

#ifdef __cplusplus
}
#endif

#endif /* PYDSM2HYDRO_H */
