# This is an example to drive DSM2 Hydro using Python.
# None of the Python APIs are finalized and subject to frequent changes.

# Assuming that pydsm2 module is available to import, for example, at the
# current directory.
from pydsm2 import pydsm2

def main():
    path_hydro_inp = 'hydro.inp'
    pydsm2.py_fourpt_init(path_hydro_inp)
    while (pydsm2.julmin < pydsm2.end_julmin):
        pydsm2.py_fourpt_step_before_updatenetwork()
        pydsm2.py_updatenetwork_prepare()
        # If something needs to be manipulated, this would be the best place,
        # right before solving a matrix.
        # For example, the elevation of a weir can be adjusted as shown
        # below.
        # pydsm2.py_set_gate_device_elev(1, 1, 0.)
        pydsm2.py_updatenetwork_loop()
        pydsm2.py_updatenetwork_wrapup()
        pydsm2.py_fourpt_step_after_updatenetwork()
    pydsm2.py_fourpt_winddown()


if __name__ == '__main__':
    main()
