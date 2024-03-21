"""Regression tests for ECO-PTM"""
__author__ = "Doug Jackson, QEDA Consulting, LLC"
__email__ = "doug@QEDAconsulting.com"

from pathlib import Path
import os
import re
import subprocess
import pytest
import pandas as pd
import platform
import pyhecdss

# Number of paths in the DSS files
# Ideally this would be specifiable at runtime
NUM_DSS_PATHS = 15

@pytest.fixture(scope="module")
def run_ecoptm(pytestconfig):
    """Fixture to run the ECO-PTM and provide a path to the project directory.

    Args:
        pytestconfig: pytest fixture to manage command line options

    Returns:
        projectDir: path to the ECO-PTM project directory
    """
    no_run = pytestconfig.getoption("no_run")

    if not no_run:
        javaCodeRootDir = Path(pytestconfig.getoption("javaCodeRootDir"))
        javaHome = Path(pytestconfig.getoption("javaHome"))
        configFile = Path(pytestconfig.getoption("configFile"))

        # Assemble the Java classpath and set the shell
        if platform.system()=="Linux":
            classpath = f"{os.path.join(javaCodeRootDir, 'lib', '*')}:{os.path.join(javaCodeRootDir, 'PTM.jar')}"
            shell = True
        else:
            classpath = f"{os.path.join(javaCodeRootDir, 'lib', '*')};{os.path.join(javaCodeRootDir, 'PTM.jar')}"
            shell = False

        javaPath = os.path.join(javaHome, "bin", "java")
        projectDir = os.path.dirname(configFile)

        javaCommand = f'{javaPath} -ss5M -ms512M -mx1G -cp "{classpath}" DWR.DMS.PTM.MainPTM {configFile}'

        print(f"javaCodeRootDir: {javaCodeRootDir}, javaHome: {javaHome}")
        print(f"classpath: {classpath}, shell: {shell}")
        print(f"javaCommand: {javaCommand}")
        print(f"projectDir: {projectDir}")

        # Run the ECO-PTM
        pwd = Path.cwd()
        os.chdir(projectDir)
        print("Launching ECO-PTM")
        completedProc = subprocess.run(javaCommand)
        exitCode = completedProc.returncode
        if exitCode!=0:
            raise RuntimeError()
        os.chdir(pwd)

    yield projectDir

@pytest.fixture(scope="module")
def ecoptm_survDF(run_ecoptm):
    """Fixture to process the ECO-PTM survival outputs and provide a pandas dataframe

    Args:
        run_ecoptm: pytest fixture to run the ECO-PTM

    Returns:
        survDF: pandas dataframe containing survival fractions
    """
    survivalFilePath = os.path.join(run_ecoptm, "survival_writes_all.csv")
    
    # Determine which line the particle-specific survival fractions start on
    with open(survivalFilePath, "r") as fH:
        for i, line in enumerate(fH):
            if re.search("Particle ID", line) is not None:
                particleIDline = i
                break
            
    # Read the overall survival fractions
    survDF = pd.read_csv(survivalFilePath, skiprows=2, nrows=particleIDline-2, header=None,
                                names=["groupID", "station", "numArrived", "numLost", "numSurvived", "survivalFrac"])
    survDF = survDF[["numArrived", "numLost", "numSurvived", "survivalFrac"]]
    
    yield survDF
    # Add any teardown code here

@pytest.fixture(scope="module")
def ecoptm_dss(run_ecoptm):
    """Fixture to process the ECO-PTM DSS output and place it in a dictionary of pandas dataframes

    Args:
        run_ecoptm: pytest fixture to run the ECO-PTM

    Returns:
        dssDict: dictionary of pandas dataframes
    """
    dssFilePath = os.path.join(run_ecoptm, "output", "ptmout_ecoptm_regression_tests.dss")

    dssDict = {}
    with pyhecdss.DSSFile(dssFilePath) as ds:
        pathnames = ds.get_pathnames()

        for p in pathnames:
            thisData, thisUnits, thisPtype = ds.read_rts(p)

            dssDict[p] = thisData
    
    yield dssDict

@pytest.mark.parametrize("test_metric", ["numArrived", "numLost", "numSurvived", "survivalFrac"])
def test_compare_ecoptm_surv(ecoptm_survDF, surv_regression, test_metric):
    """ Compare ECO-PTM survival outputs
    
    Args:
        ecoptm_survDF: pytest fixture yielding a pandas dataframe containing survival fractions
        surv_regression: custom pytest-regression fixture
        test_metric: survival metrics to compare
    """
    surv_regression.check(ecoptm_survDF, test_metric)

@pytest.mark.parametrize("dssPathIndex", list(range(NUM_DSS_PATHS)))
def test_compare_ecoptm_dss(ecoptm_dss, dss_regression, dssPathIndex):
    """ Compare ECO-PTM DSS outputs
    
    Args:
        ecoptm_dss: pytest fixture yielding a dictionary of pandas dataframes
        dss_regression: custom pytest-regression fixture
        dssPathIndex: index into list of DSS paths
    """
    dssPaths = list(ecoptm_dss.keys())
    dss_regression.check(ecoptm_dss, dssPaths[dssPathIndex])