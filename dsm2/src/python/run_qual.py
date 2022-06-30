# This is an example to drive DSM2 Qual using Python.
# None of the Python APIs are finalized and subject to frequent changes.

# Assuming that pydsm2qual module is available to import, for example, at the
# current directory.
from pydsm2qual import pydsm2qual

def main():
    path_qual_inp = 'qual.inp'
    pydsm2qual.py_qual_init(path_qual_inp)
    print(pydsm2qual.julmin)
    print(pydsm2qual.end_julmin)
    while (pydsm2qual.julmin <= pydsm2qual.end_julmin):
        pydsm2qual.py_qual_loop()
    pydsm2qual.py_qual_wrapup()


if __name__ == '__main__':
    main()

