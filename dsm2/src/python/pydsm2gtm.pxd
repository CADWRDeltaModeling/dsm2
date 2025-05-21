cdef extern from "pydsm2gtm.h":
    void gtm_prepare1()
    void gtm_prepare2()
    void gtm_prepareloop()
    void gtm_loop()
    void gtm_wrapup()
    char gtm_init_input_file[130]
    double gtm_start_jmin;
    double gtm_end_jmin;
    double gtm_time_interval;