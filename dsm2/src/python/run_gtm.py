# This is an example to drive DSM2 GTM using Python.
# None of the Python APIs are finalized and subject to frequent changes.

# Assuming that pydsm2gtm module is available to import, for example, at the
# current directory.
from pydsm2gtm import pydsm2gtm

def main():
    path_gtm_inp = 'gtm.inp'
    pydsm2gtm.py_gtm_init(path_gtm_inp)
    # print(pydsm2gtm.julmin)
    # print(pydsm2gtm.end_julmin)
    print('pygtm_start: ',pydsm2gtm.gtm_start_jmin)
    print('pygtm_end: ',pydsm2gtm.gtm_end_jmin)
    # while (pydsm2gtm.julmin <= pydsm2gtm.end_julmin):
    #     pydsm2gtm.py_gtm_loop()
    pydsm2gtm.py_gtm_loop()
    pydsm2gtm.py_gtm_wrapup()


if __name__ == '__main__':
    main()
