"""pytest-regressions fixture to use and diff pandas dataframes extracted from the DSS file output"""
__author__ = "Doug Jackson, QEDA Consulting, LLC"
__email__ = "doug@QEDAconsulting.com"

import numpy as np
import pandas as pd
import pytest
from pytest_regressions.common import perform_regression_check

@pytest.fixture
def dss_regression(datadir, original_datadir, request):
    """ Fixture used to test data against known versions previously recorded by this same fixture."""
    return DSSregressionFixture(datadir, original_datadir, request)

class DSSregressionFixture:
    """ Implementation of `dss_regression` fixture."""
    def __init__(self, datadir, original_datadir, request):
        self.request = request
        self.datadir = datadir
        self.original_datadir = original_datadir
        self.force_regen = False

    def check(self, dssDict, dssPath, basename=None, fullpath=None, obtained_filename=None, check_fn=None):
        """ Checks a column of a pandas dataframe containing a time series."""

        if check_fn is None:
            def check_fn(obtained_filename, expected_filename):
                """ Compare time series from two different pandas dataframes using numpy.allclose()."""
                is_same_shape = False
                is_allclose = False
                try:
                    obtainedData = pd.read_csv(str(obtained_filename))
                    expectedData = pd.read_csv(str(expected_filename))

                    # The dataframe also uses dssPath as a column name
                    is_same_shape = obtainedData[dssPath].shape==expectedData[dssPath].shape

                    if is_same_shape:
                        is_allclose = np.allclose(obtainedData[dssPath].to_numpy(), expectedData[dssPath].to_numpy())

                    if not is_same_shape:
                        raise AssertionError(f"The shape of the time series, {dssPath}, is not the same in {obtained_filename} and {expected_filename}.")
                    if not is_allclose:
                        raise AssertionError(f"{dssPath} differs in {obtained_filename} and {expected_filename}.")
                except KeyError as e:
                    raise AssertionError(f"{dssPath} not in DSS output file.")

        def dump_fn(filename):
            """ Dump the dataframe to a CSV file """
            dssDict[dssPath].to_csv(filename)

        perform_regression_check(
            datadir=self.datadir,
            original_datadir=self.original_datadir,
            request=self.request,
            check_fn=check_fn,
            dump_fn=dump_fn,
            extension=".csv",
            basename=basename,
            fullpath=fullpath,
            force_regen=self.force_regen,
            obtained_filename=obtained_filename,
        )
