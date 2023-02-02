# This is an example to drive DSM2 GTM using Python.
# None of the Python APIs are finalized and subject to frequent changes.

# Assuming that pydsm2gtm module is available to import, for example, at the
# current directory.
from pydsm2gtm import pydsm2gtm
import sys
import numpy as np

def main():
    # path_gtm_inp = 'gtm.inp'
    path_gtm_inp = sys.argv[1:]
    pydsm2gtm.py_gtm_init(path_gtm_inp)
    for pydsm2gtm.current_time in np.arange(pydsm2gtm.gtm_start_jmin,
                                            pydsm2gtm.gtm_end_jmin+pydsm2gtm.gtm_time_interval,
                                            pydsm2gtm.gtm_time_interval):
        pydsm2gtm.py_gtm_loop()
    pydsm2gtm.py_gtm_loop()
    pydsm2gtm.py_gtm_wrapup()


if __name__ == '__main__':
    main()
