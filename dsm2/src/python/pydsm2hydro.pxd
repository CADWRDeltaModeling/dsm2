cdef extern from "pydsm2hydro.h":
    void prepare_hydro()
    void fourpt_init()
    void fourpt_step()
    void fourpt_winddown()
    char init_input_file[130]
    int julmin;
    int end_julmin;
