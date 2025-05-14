"""Python interface to run tidefile and routing preprocessors

Used to calculate transition probabilities for the continuous time multistate Markov (CTMM) routing model
and modify the DSM2 HYDRO tidefile to work with the CTMM.
"""
__author__ = "Doug Jackson, QEDA Consulting, LLC"
__email__ = "doug@QEDAconsulting.com"
__version__ = "0.1"

import os
import glob
import sys
import platform
import shutil
import argparse
import subprocess
import zipfile
import datetime as dt
import numpy as np

class RunPreprocessors:

    def __init__(self, workingDir, configFile):
        """Initialize a RunPreprocessors object.
        
        Keyword arguments:
        workingDir (str) -- path to the location where the tests should be run
        configFile (str) -- path to the location of the YAML configuration file
        """ 
        self.workingDir = workingDir
        self.configFile = configFile

        if platform.system() == "Windows":
            self.shell = False
        else:
            self.shell = True

        # Read YAML configuration file
        try:
            with open(configFile) as fH:
                self.config = yaml.safe_load(fH)
        except IOError as e:
            print(f"Could not load configuration file {configFile}. Does it exist? {e}")
            sys.exit()
        except yaml.YAMLError as e:
            print(f"Error while parsing configuration file: {e}")
            sys.exit()
                    
    def runRoutingPreprocessor(self):
        """Run the routing preprocessor."""
        print("="*80)
        print("Launching routing preprocessor")
        Rcommand = f"Rscript {self.workingDir}/routingPreprocessor.R {self.configFile}"

        # Specify all routingPreprocessor.R command line arguments
        Rcommand = f"{Rcommand} {self.routingPreprocessorArgs}"

        print(f"Rcommand: {Rcommand}")
        
        with subprocess.Popen(Rcommand, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, shell=self.shell,
                            bufsize=1, universal_newlines=True) as p:

            # Print stdout line-by-line
            for line in p.stdout:
                print(line, end="")

    def runTidefilePreprocessor(self):
        """Run the tide file preprocessor."""
        print("="*80)
        print("Launching tide file preprocessor")  
        Rcommand = f"Rscript {self.workingDir}/tidefilePreprocessor.R {self.configFile}"

        # Specify all tidefilePreprocessor.R command line arguments
        Rcommand = f"{Rcommand} {self.tidefilePreprocessorArgs}"
        
        print(f"Rcommand: {Rcommand}")

        with subprocess.Popen(Rcommand, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, shell=self.shell,
                            bufsize=1, universal_newlines=True) as p:

            # Print stdout line-by-line
            for line in p.stdout:
                print(line, end="")

    def runTidefilePreprocessorQA(self):
        """Run the tide file preprocessor QA script."""
        print("="*80)
        print("Launching tide file preprocessor QA")  
        Rcommand = f"Rscript {self.workingDir}/tidefilePreprocessorQA.R {self.configFile}"
        
        print(f"Rcommand: {Rcommand}")

        with subprocess.Popen(Rcommand, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, shell=self.shell,
                            bufsize=1, universal_newlines=True) as p:

            # Print stdout line-by-line
            for line in p.stdout:
                print(line, end="")                
        
    def setRoutingPreprocessorArgs(self, routingPreprocessorArgs):
        """Override specific routing preprocessor arguments"""
        # Assemble the command line arguments using either values passed in via the run_preprocessors.py
        # command line arguments are from configFile
        self.routingPreprocessorArgs = ""

        # Sequence of command line arguments expected by routingPreprocessor.R
        routingPreprocessorArgList = ["tideFile", "transProbsStartDate", "transProbsEndDate", "numCores"]
        for arg in routingPreprocessorArgList:
            if arg in routingPreprocessorArgs:
                thisArg = routingPreprocessorArgs[arg]    
            elif arg in self.config["routingPreprocessor"]:
                thisArg = self.config["routingPreprocessor"][arg]
            else:
                print(f"Routing preprocessor argument {arg} not found in configuration file or command line arguments. Aborting.")
                sys.exit()

            self.routingPreprocessorArgs = f"{self.routingPreprocessorArgs} {thisArg}"
    
    def setTidefilePreprocessorArgs(self, tidefilePreprocessorArgs):
        """Override specific tidefile preprocessor arguments"""
        self.tidefilePreprocessorArgs = ""

        tidefilePreprocessorArgList = ["tideFile"]
        for arg in tidefilePreprocessorArgList:
            if arg in tidefilePreprocessorArgs:
                thisArg = tidefilePreprocessorArgs[arg]
            elif arg in self.config["tidefilePreprocessor"]:
                thisArg = self.config["tidefilePreprocessor"][arg]
            else:
                print(f"Tide file preprocessor argument {arg} not found in configuration file or command line arguments. Aborting.") 
                sys.exit()
            
            self.tidefilePreprocessorArgs = f"{self.tidefilePreprocessorArgs} {thisArg}"

    def findTideFile(self):
        """Look for one, and only one, tide file in the working directory"""
        print(f"Searching for a tide file in {self.workingDir}")
        tideFiles = glob.glob(os.path.join(workingDir, "*.h5"))
        if len(tideFiles)==0:
            print("No tide file found. Aborting.")
            sys.exit()
        elif len(tideFiles)>1:
            print("Multiple tide files found. Aborting.")
            sys.exit()
        else:
            tideFile = tideFiles[0]
            print(f"Found tide file: {tideFile}")
            return tideFile

if __name__=="__main__":
    import argparse
    import yaml 

    # Read in command line arguments
    parser = argparse.ArgumentParser(description="Launcher to run tidefile and routing preprocessors")
    parser.add_argument("--configFile", action="store", dest="configFile", required=True)
    parser.add_argument("--tideFile", action="store", dest="tideFile", required=False)
    parser.add_argument("--transProbsStartDate", action="store", dest="transProbsStartDate", required=False)
    parser.add_argument("--transProbsEndDate", action="store", dest="transProbsEndDate", required=False)
    parser.add_argument("--numCores", action="store", dest="numCores", required=False)
    parser.add_argument("--runBoth", action="store_true", dest="runBoth", required=False)
    parser.add_argument("--runRoutingPreprocessor", action="store_true", dest="runRoutingPreprocessor", required=False)
    parser.add_argument("--runTidefilePreprocessor", action="store_true", dest="runTidefilePreprocessor", required=False)
    args = parser.parse_args()

    configFile = args.configFile
    
    # Read optional routingPreprocessor arguments, if present
    routingPreprocessorArgs = {}
    if args.transProbsStartDate is not None:
        routingPreprocessorArgs["transProbsStartDate"] = args.transProbsStartDate
    if args.transProbsEndDate is not None:
        routingPreprocessorArgs["transProbsEndDate"] = args.transProbsEndDate
    if args.numCores is not None:
        routingPreprocessorArgs["numCores"] = args.numCores
    
    # Read optional tidefilePreprocessor arguments, if present
    tidefilePreprocessorArgs = {}

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

    workingDir = os.path.dirname(os.path.realpath(__file__))
    workingDir = workingDir.replace("\\", "/")
    print(f"Working directory: {workingDir}")
    
    # Read configuration parameter values
    try:
        runRoutingPreprocessor = config["runRoutingPreprocessor"]
        runTidefilePreprocessor = config["runTidefilePreprocessor"]
        runTidefilePreprocessorQA = config["runTidefilePreprocessorQA"]
    except ValueError as e:
        print("Unable to read all parameter values from configuration file.")
        print(e)
        sys.exit()
    except KeyError as e:
        print(f"Required parameter {e} missing from configuration file.")
        sys.exit()
    
    # Use optional command line arguments for runRoutingPreprocessor and runTidefilePreprocessor
    if args.runBoth:
        runRoutingPreprocessor = True
        runTidefilePreprocessor = True
    elif args.runRoutingPreprocessor:
        runRoutingPreprocessor = True
        runTidefilePreprocessor = False
    elif args.runTidefilePreprocessor:
        runRoutingPreprocessor = False
        runTidefilePreprocessor = True
    
    r = RunPreprocessors(workingDir, configFile)

    # If tideFile is not specified on the command line, look for it in the config file.
    if args.tideFile is not None:
        tideFile = args.tideFile
    else:
        try:
            tideFile = config["tideFile"]
            print(f"Read tide file from configuration file: {tideFile}")
        except:
            tideFile = None

    # If tideFile is not specified on the command line or in the config file, look for it in workingDir
    if tideFile is None:
        tideFile = r.findTideFile()
        print(f"Found tide file in working directory: {tideFile}")

    routingPreprocessorArgs["tideFile"] = tideFile
    tidefilePreprocessorArgs["tideFile"] = tideFile

    if runRoutingPreprocessor:
        r.setRoutingPreprocessorArgs(routingPreprocessorArgs)
        r.runRoutingPreprocessor()

    if runTidefilePreprocessor:
        r.setTidefilePreprocessorArgs(tidefilePreprocessorArgs)
        r.runTidefilePreprocessor()
    
    if runTidefilePreprocessorQA:
        r.runTidefilePreprocessorQA()