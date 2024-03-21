""" pytest-regressions fixture to use and diff pandas dataframes containing survival fractions"""
__author__ = "Doug Jackson, QEDA Consulting, LLC"
__email__ = "doug@QEDAconsulting.com"

import numpy as np
import pandas as pd
import pytest
from pytest_regressions.common import perform_regression_check

@pytest.fixture
def surv_regression(datadir, original_datadir, request):
    """ Fixture used to test data against known versions previously recorded by this same fixture."""
    return SurvRegressionFixture(datadir, original_datadir, request)

class SurvRegressionFixture:
    """ Implementation of `surv_regression` fixture."""
    def __init__(self, datadir, original_datadir, request):
        self.request = request
        self.datadir = datadir
        self.original_datadir = original_datadir
        self.force_regen = False

    def check(self, survDF, metric, basename=None, fullpath=None, obtained_filename=None, check_fn=None):
        """ Checks a column of a pandas dataframe containing survival fractions."""

        if check_fn is None:
            def check_fn(obtained_filename, expected_filename):
                """ Compare survival fractions from two different pandas dataframes using numpy.allclose()."""
                is_same_shape = False
                is_allclose = False
                try:
                    obtainedData = pd.read_csv(str(obtained_filename))
                    expectedData = pd.read_csv(str(expected_filename))

                    is_same_shape = obtainedData[metric].shape==expectedData[metric].shape

                    if is_same_shape:
                        is_allclose = np.allclose(obtainedData[metric].to_numpy(), expectedData[metric].to_numpy())

                    if not is_same_shape:
                        raise AssertionError(f"The shape of the survival output metric, {metric}, is not the same in {obtained_filename} and {expected_filename}.")
                    if not is_allclose:
                        raise AssertionError(f"{metric} differs in {obtained_filename} and {expected_filename}.")
                except KeyError as e:
                    raise AssertionError(f"{metric} not in survival output dataframe.")

        def dump_fn(filename):
            """ Dump the dataframe to a CSV file """
            survDF.to_csv(filename)

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
