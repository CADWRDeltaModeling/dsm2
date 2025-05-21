# This is an test to drive DSM2 GTM using Python.
# None of the Python APIs are finalized and subject to frequent changes.

# Assuming that pydsm2gtm module is available to import, for example, at the
# current directory.

def main():
    import pydsm2gtm

    input_file = "gtm.inp"
    pydsm2gtm.py_gtm_init(input_file)
    print(pydsm2gtm.get_julmin())
    gtm_end_julmin = pydsm2gtm.gtm_end_jmin()
    print(gtm_end_julmin)
    gtm_start_julmin = pydsm2gtm.gtm_start_jmin()
    gtm_time_interval = pydsm2gtm.gtm_time_interval()
    gtm_current_time = gtm_start_julmin
    while gtm_current_time <= gtm_end_julmin:
        pydsm2gtm.py_gtm_loop()
        gtm_current_time += gtm_time_interval
    pydsm2gtm.py_gtm_wrapup()


if __name__ == '__main__':
    main()