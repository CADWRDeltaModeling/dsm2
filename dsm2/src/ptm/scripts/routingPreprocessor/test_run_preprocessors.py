"""Wrapper script to run run_preprocessors during development.

Used to run run_preprocessors within an IDE for development and debugging purposes.
"""
__author__ = "Doug Jackson, QEDA Consulting, LLC"
__email__ = "doug@QEDAconsulting.com"

import os
import sys
import yaml

###########################################################################
# Constants
###########################################################################
workingDir = "C:/Users/admin/Documents/QEDA/DWR/programs/ECO_PTM_SouthDelta/dsm2/src/ptm/scripts/routingPreprocessor"
###########################################################################
# Run
###########################################################################
os.chdir(workingDir)

# Add the working directory to the system path
sys.path.append(workingDir)

configFile = os.path.join(workingDir, "config_preprocessors.yaml")

# Import run_preprocessors
from run_preprocessors import RunPreprocessors

# Read YAML configuration file
try:
    with open(configFile) as fH:
        config = yaml.safe_load(fH)
except IOError as e:
    print(f"Could not load configuration file {configFile}. Does it exist? {e}")
    sys.exit()
except yaml.YAMLError as e:
    print(f"Error while parsing configuration file: {e}")
    sys.exit()

# Read configuration parameter values
try:
    runRoutingPreprocessor = config["runRoutingPreprocessor"]
    runTidefilePreprocessor = config["runTidefilePreprocessor"]
except ValueError as e:
    print("Unable to read all parameter values from configuration file.")
    print(e)
    sys.exit()
except KeyError as e:
    print(f"Required parameter {e} missing from configuration file.")
    sys.exit()

r = RunPreprocessors(workingDir, configFile)
if runRoutingPreprocessor:
    print("Running routing preprocessor")
    r.runRoutingPreprocessor()
if runTidefilePreprocessor:
    print("Running tide file preprocessor")
    r.runTidefilePreprocessor()