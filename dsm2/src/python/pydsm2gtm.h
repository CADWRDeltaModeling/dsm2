#ifndef PYDSM2GTM_H
#define PYDSM2GTM_H

#ifdef __cplusplus
extern "C" {
#endif

/* Declare Fortran functions with appropriate name mangling */
void gtm_prepare1();
void gtm_prepare2();
void gtm_prepareloop();
void gtm_loop();
void gtm_wrapup();

extern char gtm_init_input_file[130];
extern double gtm_start_jmin;
extern double gtm_end_jmin;
extern double gtm_time_interval;


#ifdef __cplusplus
}
#endif

#endif /* PYDSM2GTM_H */