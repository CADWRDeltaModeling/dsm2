# cython: language_level=3

import numpy as np
cimport numpy as np
from libc.string cimport strncpy
cimport pydsm2gtm

def py_gtm_init(input_file: str):
    pydsm2gtm.gtm_prepare1()
    strncpy(gtm_init_input_file, input_file.encode("utf-8"), 129)
    pydsm2gtm.gtm_prepare2()
    pydsm2gtm.gtm_prepareloop()


def py_gtm_loop():
    pydsm2gtm.gtm_loop()


def py_gtm_wrapup():
    pydsm2gtm.gtm_wrapup()

def get_gtm_start_jmin():
    return gtm_start_jmin

def get_gtm_end_jmin():
    return gtm_end_jmin

def get_gtm_time_interval():
    return gtm_time_interval


