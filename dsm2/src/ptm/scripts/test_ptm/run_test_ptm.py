"""Wrapper script to run test_ptm during development.

Used to run test_ptm within an IDE for development and debugging purposes.
"""
__author__ = "Doug Jackson, QEDA Consulting, LLC"
__email__ = "doug@QEDAconsulting.com"

import os
import json

###########################################################################
# Constants
###########################################################################
workingDir ="D:/QEDA/DWR/programs/test_ptm/"
configFile = "D:/QEDA/DWR/programs/test_ptm/test_ptm_config_31oct23.json"

###########################################################################
# Run
###########################################################################
os.chdir(workingDir)
from test_ptm import TestPTM

# Read configuration file
try:
    with open(configFile) as fH:
        config = json.load(fH)
except IOError as e:
    print(f"Could not load configuration file {configFile}. Does it exist? {e}")
except json.JSONDecodeError as e:
    print(f"Error while parsing PTM configuration file: {e}")          

# Read configuration parameter values
try:
    workingDir = config["workingDir"]
    javaHome = config["javaHome"]
    javaRootDir = config["javaRootDir"]
    runTestSuite = config["runTestSuite"]
    runProfilingTest = config["runProfilingTest"]
    runCodeCoverage = config["runCodeCoverage"]
    useNewRandomSeed = config["useNewRandomSeed"]
    runSpecificTests = config["runSpecificTests"]
    runSpecificScenarios = config["runSpecificScenarios"]
except ValueError as e:
    print("Unable to read all parameter values from configuration file.")
    print(e)
except KeyError as e:
    print(f"Required parameter {e} missing from configuration file.")

t = TestPTM(javaHome, javaRootDir, workingDir, runCodeCoverage, useNewRandomSeed)

# Run a subset of tests, if specified 
if len(runSpecificTests)>0:
    t.tests = [str(t) for t in runSpecificTests]
print(f"Tests to run: {t.tests}")
   
# Run a subset of scenarios, if specified
if len(runSpecificScenarios)>0:
    origFlowScenarios = t.flowScenarios
    newFlowScenarios = [s for s in runSpecificScenarios]
    t.flowScenarios = {}
    for flowScenario in newFlowScenarios:
        try:
            t.flowScenarios[flowScenario] = origFlowScenarios[flowScenario]
        except:
            print("Flow scenario", flowScenario, "not recognized")
print(f"Scenarios to run: {t.flowScenarios}")
print("="*75)
            
if runTestSuite:
    t.runTestSuite()
    t.processTestSuite()

if runProfilingTest:
    t.runProfilingTest()

