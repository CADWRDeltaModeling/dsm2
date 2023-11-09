"""Reads outputs from an ECO-PTM model run."""
import os
import re
import glob
import pandas as pd
from io import StringIO

__author__ = "Doug Jackson, QEDA Consulting, LLC"
__email__ = "doug@QEDAconsulting.com"

class PTMoutputReader:

    def __init__(self, outputDir):
        """Initialize a ReadPTMoutputs object and read in ECO-PTM outputs.
        
        Keyword arguments:
        outputDir (str) -- the path to the ECO-PTM outputs directory
        """
        self.outputDir = outputDir
        
        # Initialize output data frames
        self.trace = pd.DataFrame(columns=["tmStamp", "particleNum", "nodeNum", "wbNum"])
        self.travelTime = pd.DataFrame(columns=["particleID", "releaseStation", "releaseDatetime", "detectStation", "travelTime_min"])
        self.survival = pd.DataFrame(columns=["groupID", "station", "numArrived", "numLost", "numSurvived", "survivalFrac"])
        self.survivalProb = pd.DataFrame(columns=["nodeID", "particleID", "groupID", "survivalProb"])
        self.entrainment = pd.DataFrame(columns=["nodeID", "particleID", "entrainmentProb"])
        
        # Read the outputs
        self.readTraceFile()
        self.readTravelTimeFile()
        self.readSurvival()
        self.readEntrainment()
                
    def readTraceFile(self):
        """Read the trace file into a Pandas dataframe."""
        
        # Read the path to the trace file
        traceFilePath = glob.glob(os.path.join(self.outputDir, "trace*.out"))
    
        if len(traceFilePath)>1:
            print("More than one trace file found in the output directory.")
            return
    
        try:
            self.trace = pd.read_csv(traceFilePath[0], sep=" ", skiprows=1, header=None, 
                        names=["tmStamp", "particleNum", "nodeNum", "wbNum"])
        except:
            print("Could not read the trace file.")
    
    def readTravelTimeFile(self):
        """Read the travel time file into a Pandas dataframe."""
        
        # Read the path to the travel time file
        travelTimeFilePath = glob.glob(os.path.join(self.outputDir, "travel_time*.csv"))
        
        if len(travelTimeFilePath)>1:
            print("More than one travel time file found in the output directory.")
            return
    
        try:
            self.travelTime = pd.read_csv(travelTimeFilePath[0], skiprows=1, header=None,
                                          names=["particleID", "releaseStation", "releaseDatetime", "detectStation", "travelTime_min"])
        except:
            print("Could not read travel time file.")
        
    def readSurvival(self):
        """Read the survival files into a Pandas dataframe."""
        
        # Read the path to the file containing the overall survival fractions
        survivalFilePath = os.path.join(self.outputDir, "..", "survival_writes_all")
        
        if not os.path.exists(survivalFilePath):
            print(f"No survival output file found. Expected location: {survivalFilePath}.")
            return
        
        try:
            # Determine which line the particle-specific survival probabilities start on
            with open(survivalFilePath, "r") as fH:
                for i, line in enumerate(fH):
                    if re.search("Particle ID", line) is not None:
                        particleIDline = i
                        break
            
            # Read the overall survival fractions
            self.survival = pd.read_csv(survivalFilePath, skiprows=2, nrows=particleIDline-2, header=None,
                                        names=["groupID", "station", "numArrived", "numLost", "numSurvived", "survivalFrac"])
            
            # Read the survival probabilities
            self.survivalProb = pd.read_csv(survivalFilePath, skiprows=particleIDline+1, header=None,
                                            names=["particleID", "groupID", "survivalProb"])
        except Exception as e:
            print("Could not read the file containing the overall survival fractions and survival probabilities.")
            print(f"Exception: {e}")

        self.survivalGroups = self.survival[["groupID", "station"]]
        self.survivalGroups.set_index("groupID", inplace=True)
        
    def readEntrainment(self):
        """Read the entrainment file into a Pandas dataframe."""
        
        # Read the path to the entrainment file
        entrainmentFilePath = glob.glob(os.path.join(self.outputDir, "entrainment*.csv"))
        
        if len(entrainmentFilePath)>1:
            print("More than one entrainment file found in the output directory.")
            return
        
        try:
            # Read in each chunk from the file
            entrainmentData = pd.DataFrame()
            
            with open(entrainmentFilePath[0], "r") as fH:
                
                # A string to temporarily store each chunk of data
                tempDataString = ""
                
                entrainmentDataList = []
                for line in fH:
                    # Detect a header line
                    if re.search("^[A-Za-z]", line) is not None:
                        
                        # Read the previous chunk, if any, into a dataframe and add it to the master dataframe
                        if tempDataString.count("\n")>1:
                            thisData = pd.read_csv(StringIO(tempDataString))
                            thisData = thisData[["Node ID", "pId", "Entrainment Probability"]]
                            thisData = thisData.rename(columns={"Node ID":"nodeID", "pId":"particleID", 
                                                                "Entrainment Probability":"entrainmentProb"})
                            entrainmentDataList.append(thisData)
                            
                        # Create a new tempDataString
                        tempDataString = line
     
                    elif re.search("^\d", line) is not None:
                        tempDataString+=line
                            
            # Read the final chunk
            if tempDataString.count("\n")>1:
                thisData = pd.read_csv(StringIO(tempDataString))
                thisData = thisData[["Node ID", "pId", "Entrainment Probability"]]
                thisData = thisData.rename(columns={"Node ID":"nodeID", "pId":"particleID", 
                                                    "Entrainment Probability":"entrainmentProb"})
                entrainmentDataList.append(thisData)
            
            entrainmentData = pd.concat(entrainmentDataList, ignore_index=True)
            
            # Convert nodeID to an integer
            entrainmentData["nodeID"] = entrainmentData["nodeID"].astype("int")
            
        except:
            print("Could not read entrainment file.")
            return
        
        self.entrainment = entrainmentData