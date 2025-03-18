# cython: language_level=3

import numpy as np
cimport numpy as np
from libc.string cimport strncpy
cimport pydsm2hydro


def py_fourpt_init(input_file: str):
    prepare_hydro()
    strncpy(init_input_file, input_file.encode("utf-8"), 129)
    fourpt_init()


def py_fourpt_step():
    fourpt_step()


def py_fourpt_winddown():
    fourpt_winddown()


def get_julmin():
    return julmin

def get_end_julmin():
    return end_julmin
