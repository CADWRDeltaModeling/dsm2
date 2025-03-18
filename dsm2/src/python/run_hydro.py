# This is an example to drive DSM2 Hydro using Python.
# None of the Python APIs are finalized and subject to frequent changes.

# Assuming that pydsm2hydro module is available to import, for example, at the
# current directory.
def main():
    import pydsm2hydro

    input_file = "hydro_.inp"
    pydsm2hydro.py_fourpt_init(input_file)
    print(pydsm2hydro.get_julmin())
    end_julmin = pydsm2hydro.get_end_julmin()
    print(end_julmin)
    while pydsm2hydro.get_julmin() < end_julmin:
        pydsm2hydro.py_fourpt_step()
    pydsm2hydro.py_fourpt_winddown()


if __name__ == '__main__':
    main()

