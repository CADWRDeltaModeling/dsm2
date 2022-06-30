# This is an example to drive DSM2 Hydro using Python.
# None of the Python APIs are finalized and subject to frequent changes.

# Assuming that pydsm2hydro module is available to import, for example, at the
# current directory.
from pydsm2hydro import pydsm2hydro

def main():
    path_hydro_inp = 'hydro.inp'
    pydsm2hydro.py_fourpt_init(path_hydro_inp)
    while (pydsm2hydro.julmin <= pydsm2hydro.end_julmin):
        pydsm2hydro.py_fourpt_step_before_updatenetwork()
        pydsm2hydro.py_updatenetwork_prepare()
        # If something needs to be manipulated, this would be the best place,
        # right before solving a matrix.
        # For example, the elevation of a weir can be adjusted as shown
        # below.
        # pydsm2hydro.py_set_gate_device_elev(1, 1, 0.)
        pydsm2hydro.py_updatenetwork_loop()
        pydsm2hydro.py_updatenetwork_wrapup()
        pydsm2hydro.py_fourpt_step_after_updatenetwork()
    pydsm2hydro.py_fourpt_winddown()


if __name__ == '__main__':
    main()
