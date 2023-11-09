"""Automatic test framework for ECO-PTM model. 

Used to help modelers verify that survival, travel time, routing, and other model behaviors are still
reasonable following any modification of the base ECO-PTM model.
"""
__author__ = "Doug Jackson, QEDA Consulting, LLC"
__email__ = "doug@QEDAconsulting.com"
__version__ = "0.3"

import os
import sys
import platform
import shutil
import argparse
import subprocess
import zipfile
import datetime as dt
import numpy as np

from ptm_output_processor import PTMoutputProcessor
from ptm_output_reader import PTMoutputReader

class TestPTM:

    def __init__(self, javaHome, javaRootDir, workingDir, runCodeCoverage, useNewRandomSeed):
        """Initialize a TestPTM object.
        
        Keyword arguments:
        javaHome (str) -- path to the JRE, e.g., "C:\Program Files (x86)\Java\jdk1.8.0_201"
        javaRootDir (str) -- the path to the Java source code, e.g., dsm2\dsm2\src\ptm
        workingDir (str) -- the path to the location where the tests should be run
        runCodeCoverage (bool) -- flag to indicate whether code coverage testing should be enabled
        useNewRandomSeed (bool) -- flag to indicate whether to use a new random seed for each run
        """ 
        self.javaHome = javaHome
        self.javaRootDir = javaRootDir
        self.workingDir = workingDir
        self.runCodeCoverage = runCodeCoverage
        self.useNewRandomSeed = {True:"Yes", False:"No"}[useNewRandomSeed]
          
        numTests = 14
        self.tests = np.arange(0, numTests)+1
        self.tests = [str(t) for t in self.tests]
        
        # Short descriptions of tests
        self.testDescriptions = {"1":"Routing probabilities at Sutter and Steamboat Sloughs",
                                 "2":"Routing probabilities at Georgiana Slough and DCC; proportion of particles entering the interior Delta",
                                 "3":"Large spatial scale routing",
                                 "4":"Reach-specific survival and travel time: reach 1, release from Sacramento upstream of Freeport --> Freeport",
                                 "5":"Reach-specific survival and travel time: reach 2, release from Freeport --> Sutter Slough and Steamboat Slough",
                                 "6":"Reach-specific survival and travel time: reach 3, release from mainstem Sacramento River upstream of Georgiana Slough --> Georgiana Slough and DCC",
                                 "7":"Reach-specific survival and travel time: reach 4, release from Sutter Slough --> Rio Vista",
                                 "8":"Reach-specific survival and travel time: reach 5, release from mainstem Sacramento River upstream of Georgiana Slough --> Rio Vista",
                                 "9":"Reach-specific survival and travel time: reach 6, release from Georgiana Slough --> Rio Vista",
                                 "10":"Reach-specific survival and travel time: reach 7, release from Georgiana Slough --> Mokelumne River",
                                 "11":"Reach-specific survival and travel time: reach 8, release from DCC --> Mokelumne River",
                                 "12":"Reach-specific survival and travel time: reach 9, release from Rio Vista --> Chipps Island",
                                 "13":"Reach-specific survival and travel time: reach 10, release from Mokelumne River --> Chipps Island",
                                 "14":"Route-specific survival and travel time"}

        # Descriptions of routes
        self.routeDescriptions = {1:"Freeport to Sutter Slough to Rio Vista to Chipps",
                                    2:"Freeport to Steamboat Slough to Rio Vista to Chipps",
                                    3:"Freeport to Rio Vista to Chipps Island via the mainstem Sacramento River",
                                    4:"Freeport to DCC to Chipps Island via the interior Delta",
                                    5:"Freeport to Georgiana Slough to Chipps Island via the interior Delta."}
        # Descriptions of reaches
        self.reachDescriptions = {"1":"Sacramento to Freeport", "2":"Freeport to Steamboat and Sutter Sloughs", 
                                    "3":"mainstem Sacramento River from Steamboat and Sutter Sloughs to the Delta Cross Channel (DCC) and Georgiana Slough",
                                    "4":"Sutter Slough to Rio Vista", "5":"Steamboat Slough to Rio Vista", 
                                    "6":"mainstem Sacramento River from Georgiana Slough to Rio Vista",
                                    "7":"Georgiana Slough to the Mokelumne River", 
                                    "8":"DCC to the Mokelumne River", "9":"Rio Vista to Chipps Island",
                                    "10":"Mokelumne River to Chipps Island"}

        # Descriptions of nodes
        self.nodeDescriptions = {339:"Sutter Slough junction with mainstem Sacramento River", 
                                340:"Steamboat Slough junction with mainstem Sacramento River",
                                342:"DCC junction with mainstem Sacramento River",
                                343:"Georgiana Slough junction with mainstem Sacramento River"}                                                                                                         
        
        self.flowScenarios = {"lowFlow":{"startDate":"21APR2015", "endDate":"28SEP2015", "releaseDate":"05/01/2015"},
                    "medFlow":{"startDate":"18MAR2012", "endDate":"25AUG2012", "releaseDate":"03/28/2012"},
                    "highFlow":{"startDate":"15MAR2011", "endDate":"22AUG2011", "releaseDate":"03/25/2011"}}
                    
        self.profilingScenarios = {"profiling":{"startDate":"01JAN2011", "endDate":"31JAN2014"}}
                                                    
        # Define the nodeIDs and groupIDs used for processing each test
        self.nodeIDs = {"1":[339, 340], "2":[342, 343]}
        self.groupIDs = {"1":[3, 4, 5], "2":[6, 7, 8], 
                        "4":1, "5":2, "6":3, "7":4, "8":5, "9":6, "10":7, "11":8, "12":9, "13":10}
        
        # Define the survival groups that make up each route
        self.routeGroupIDs = {1:[2, 4, 9], 2:[2, 5, 9], 3:[2, 3, 6, 9],
                                4:[2, 3, 8, 10], 5:[2, 3, 7, 10]}
                                
        # Assemble the Java classpath and set the shell
        if platform.system()=="Linux":
            self.classpath = os.path.join(javaRootDir, "javabin") + ":" + os.path.join(javaRootDir, "lib", "*")
            self.shell = True
        else:
            self.classpath = os.path.join(javaRootDir, "javabin") + ";" + os.path.join(javaRootDir, "lib", "*")
            self.shell = False
        
        # Read the location of this script
        self.rootDir = os.path.dirname(os.path.realpath(__file__))
        self.templatesDir = os.path.join(self.rootDir, "test_templates")
        
        self.tidefilePath = os.path.join(self.rootDir, "data", "hist_v822.h5")
        self.tidefilePath = self.tidefilePath.replace("\\", "/")
        
        # Create the working directory, if necessary
        if not os.path.exists(self.workingDir):
            os.makedirs(self.workingDir)

        os.chdir(self.workingDir)
        
        self.javaPath = os.path.join(self.javaHome, "bin", "java")
        print("="*75)
        print(f"workingDir: {self.workingDir}")
        print(f"javaHome: {self.javaHome}")
        print(f"javaPath: {self.javaPath}")
        print(f"javaRootDir: {self.javaRootDir}")
        print(f"classpath: {self.classpath}")
        print(f"rootDir: {self.rootDir}")
        print(f"useNewRandomSeed: {useNewRandomSeed}")
                    
    def runTestSuite(self):
        """Run the suite of tests."""
        # Create a directory with the datetime stamp for this run of the tests
        self.testsDir = os.path.join(self.workingDir, "tests_" + dt.datetime.now().strftime("%d%b%y_%H.%M").lower())

        # Create a subdirectory for this run of the tests
        if os.path.exists(self.testsDir):
            shutil.rmtree(self.testsDir)
        os.makedirs(self.testsDir)
        
        # Run the tests
        for test in self.tests:
      
            for flowScenario in self.flowScenarios:
    
                print("Running test ", test, ", flow scenario ", flowScenario, sep="")
        
                # Create a subdirectory for this test
                thisTestDir = os.path.join(self.testsDir, "test_" + test + "_" + flowScenario)
                os.makedirs(thisTestDir)
                os.makedirs(os.path.join(thisTestDir, "output"))
                
                # Temporary kluge: Copy an existing DSS file into the output directory to avoid DSS ERROR
                shutil.copy(os.path.join(self.rootDir, "data", "ptmout-10-22-1997.dss"),
                            os.path.join(thisTestDir, "output", "ptmout_test_" + test + "_" + flowScenario + ".dss"))
        
                # Read the configuration file templates
                with open(os.path.join(self.templatesDir, "template_dsm2_config.inp"), "r") as fH:
                    dsm2Config = fH.read()
                with open(os.path.join(self.templatesDir, "template_ptm_config.inp"), "r") as fH:
                    ptmConfig = fH.read()
                with open(os.path.join(self.templatesDir, "template_ptm_behavior_inputs.inp"), "r") as fH:
                    behaviorConfigTemplate = fH.read()
                with open(os.path.join(self.templatesDir, "template_ptm_behavior_inputs_test_" + test + ".inp"), "r") as fH:
                    behaviorConfigTestSpecific = fH.read()
                
                # Modify the configuration files
                dsm2Config = dsm2Config.replace("DSM2MODIFIER_PLACEHOLDER", "test_" + test)        
                dsm2Config = dsm2Config.replace("START_DATE_PLACEHOLDER", self.flowScenarios[flowScenario]["startDate"])        
                dsm2Config = dsm2Config.replace("END_DATE_PLACEHOLDER", self.flowScenarios[flowScenario]["endDate"])        
                dsm2Config = dsm2Config.replace("HYDROTIDEFILE_PLACEHOLDER", self.tidefilePath)
                dsm2Config = dsm2Config.replace("PTMOUTFILE_PLACEHOLDER", "test_" + test + "_" + flowScenario + ".pof")
                dsm2Config = dsm2Config.replace("PTMOUTPUTFILE_PLACEHOLDER", "ptmout_test_" + test + "_" + flowScenario)
        
                ptmConfig = ptmConfig.replace("DSM2_CONFIG_PLACEHOLDER",
                                                "dsm2_config_test_" + test + "_" + flowScenario + ".inp")
                ptmConfig = ptmConfig.replace("BEHAVIOR_CONFIG_PLACEHOLDER",
                                                "ptm_behavior_inputs_test" + test + "_" + flowScenario + ".inp")
                ptmConfig = ptmConfig.replace("TEST_PLACEHOLDER", "test_" + test + "_" + flowScenario)
                                                
                behaviorConfig = behaviorConfigTemplate.replace("TEST-SPECIFIC_PLACEHOLDER", behaviorConfigTestSpecific)
                behaviorConfig = behaviorConfig.replace("TEST_PLACEHOLDER", "test_" + test + "_" + flowScenario)
                behaviorConfig = behaviorConfig.replace("RELEASE_DATE_PLACEHOLDER", self.flowScenarios[flowScenario]["releaseDate"])
                behaviorConfig = behaviorConfig.replace("USE_NEW_RANDOM_SEED_PLACEHOLDER", self.useNewRandomSeed)
                     
                # Fix path separators for Linux
                if platform.system()=="Linux":
                    behaviorConfig = behaviorConfig.replace("\\output\\", "/output/")
                    
                # Write the modified configuration files
                with open(os.path.join(thisTestDir, "dsm2_config_test_" + test + "_" + flowScenario + ".inp"), "w") as fH:
                    print(dsm2Config, file=fH)
                with open(os.path.join(thisTestDir, "ptm_config_test_" + test + "_" + flowScenario + ".inp"), "w") as fH:
                    print(ptmConfig, file=fH)
                with open(os.path.join(thisTestDir, "ptm_behavior_inputs_test" + test + "_" + flowScenario + ".inp"), "w") as fH:
                    print(behaviorConfig, file=fH)
                
                # Run the test
                os.chdir(thisTestDir)
    
                if self.runCodeCoverage:
                    javaCommand = (self.javaPath + " -javaagent:" + self.rootDir + "/data/jacoco-0.8.5-20190510.230805-3/lib/jacocoagent.jar=output=file " +
                            "-Xss5M -Xms512M -Xmx1G -classpath " + '"' + self.classpath + '" ' +
                            "DWR.DMS.PTM.MainPTM " + "ptm_config_test_" + test + "_" + flowScenario + ".inp")
                else:
                    javaCommand = (self.javaPath + " -Xss5M -Xms512M -Xmx1G -classpath " + '"' + self.classpath + '" ' +
                            "DWR.DMS.PTM.MainPTM " + "ptm_config_test_" + test + "_" + flowScenario + ".inp")
                    
                print(javaCommand)                
                
                with subprocess.Popen(javaCommand, stdout=subprocess.PIPE, shell=self.shell,
                                    bufsize=1, universal_newlines=True) as p:
        
                    # Print stdout line-by-line
                    for line in p.stdout:
                        print(line, end="")

    def processTestSuite(self):
        """Process the suite of tests."""
        print("\n===============Processing test outputs===============")
        
        # Create a directory to save all of the plots and other outputs
        mainOutputDir = os.path.join(self.testsDir, "output")
        if not os.path.exists(mainOutputDir):
            os.makedirs(mainOutputDir)
        
        # Create a temporary copy of the base ECO-PTM outputs
        baseOutputZipFile = os.path.join(self.testsDir, "baseECO_PTM_outputs.zip")
        baseOutputDir = os.path.join(self.testsDir, "baseECO_PTM_outputs")
        shutil.copy(os.path.join(self.rootDir, "data", "baseECO_PTM_outputs.zip"), baseOutputZipFile)
        with zipfile.ZipFile(baseOutputZipFile, "r") as zipH:
            zipH.extractall(self.testsDir)
                      
        with open(os.path.join(mainOutputDir, "testResults.txt"), "w") as fH:
        
            for flowScenario in self.flowScenarios:
            
                for test in self.tests:
                    testName = "test_" + test + "_" + flowScenario
                    thisBaseOutputDir = os.path.join(baseOutputDir, testName, "output")
                    
                    print("="*160, file=fH)
                    print("Test ", test, ": ", self.testDescriptions[test], ", ", flowScenario, sep="", file=fH)
                    print("Processing test ", test, ", flow scenario ", flowScenario, "...", sep="")
                    thisOutputDir = os.path.join(self.testsDir, "test_" + test + "_" + flowScenario, "output")
                    
                    if self.runCodeCoverage:
                        # Specify a directory to save the JaCoCo output into
                        jaCoCoOutputDir = os.path.join(mainOutputDir, "jacoco-report_test_" + test + "_" + flowScenario)
                    
                        # Generate the coverage report
                        javaCommand = (self.javaPath + ' -jar "' + self.rootDir + '/data/jacoco-0.8.5-20190510.230805-3/lib/jacococli.jar" ' +  
                            'report "' + os.path.join(self.testsDir, "test_" + test + "_" + flowScenario, "jacoco.exec") + 
                            '" --classfiles "' + os.path.join(self.javaRootDir, "javabin", "DWR", "DMS", "PTM") + 
                            '" --html "' + jaCoCoOutputDir + 
                            '" --name ' + "test_" + test + "_" + flowScenario + ' --sourcefiles "' + os.path.join(self.javaRootDir, "DWR", "DMS", "PTM"))
                        print(javaCommand)                

                        with subprocess.Popen(javaCommand, stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=self.shell,
                                            bufsize=1, universal_newlines=True) as p:

                            # Print stdout line-by-line
                            for line in p.stdout:
                                print(line, end="")

                            # Print stderr
                            for i, line in enumerate(p.stderr):
                                if i==0:
                                    print("\n===============JaCoCo coverage report errors and warnings===============")
                                print(line, end="")
         
                        # Copy the JaCoCo session into the report output folder
                        if not os.path.exists(jaCoCoOutputDir):
                            os.makedirs(jaCoCoOutputDir)
                        shutil.copy(os.path.join(self.testsDir, "test_" + test + "_" + flowScenario, "jacoco.exec"), 
                                    os.path.join(jaCoCoOutputDir, "jacoco.exec"))
                    
                    # Read the survival file and obtain the groupID to station mapping
                    pOR = PTMoutputReader(thisOutputDir)
                    survivalGroups = pOR.survivalGroups
                    
                    # Create a PTM output processor object     
                    pPO = PTMoutputProcessor(self.rootDir, mainOutputDir, testName)
               
                    # Entrainment Fraction
                    if test in ["1", "2"]:
                        # Calculate the entrainment fractions
                        entrainmentFracs = pPO.entrainmentFrac(thisOutputDir, groupIDs=self.groupIDs[test])
                        print("-"*75, file=fH)
                        print("entrainment fractions:", file=fH)
                        for description in entrainmentFracs:
                            # For clarity, replace "groupID" with "reach"
                            thisDescription = description.replace("groupID", "reach")
                            print(thisDescription, ": ", entrainmentFracs[description], sep="", file=fH)
            
                        # Compare the entrainment fractions for this run to the results from the base ECO-PTM
                        entrainmentFracDiffs = pPO.compareEntrainmentFracs(thisBaseOutputDir, thisOutputDir, groupIDs=self.groupIDs[test])
                        print("-"*75, file=fH)
                        print("difference in entrainment fractions (new-base ECO-PTM):", file=fH)
                        for description in entrainmentFracDiffs:
                            # For clarity, replace "groupID" with "reach"
                            thisDescription = description.replace("groupID", "reach")
                            print(thisDescription, ": ", entrainmentFracDiffs[description], sep="", file=fH)
            
                    # Entrainment probability
                    if test in ["1", "2"]:
                        # Plot the entrainment probability distribution for this model
                        for nodeID in self.nodeIDs[test]:
                            pPO.entrainmentProbDist(thisOutputDir, nodeID)
        
                            # Compare the entrainment probability distributions of this model and the base ECO-PTM
                            statistic, pValue, overlapCoeff = pPO.compareEntrainmentProbDist(thisBaseOutputDir, thisOutputDir, nodeID)
                            print("-"*75, file=fH)
                            print("entrainment probability distribution statistics, node", nodeID, file=fH)
                            print("entrainment Anderson-Darling statistic: ", statistic, ", pValue: ", pValue, sep="", file=fH)
                            print("entrainment overlap coefficient:", overlapCoeff, file=fH)
                    
                    # Spatial heat map showing interior Delta as a different color
                    if test in ["2"]:
                        pPO.spatialHeatMap(thisOutputDir, region="interiorDelta")
                        pPO.spatialHeatMap(thisBaseOutputDir, region="interiorDelta", baseECO_PTM=True)
                    
                    # Spatial heat map showing large spatial scale routing
                    if test in ["1", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"]:
                        pPO.spatialHeatMap(thisOutputDir, region="all")
                    
                    # Spatial heat map comparing routing of this model versus the base ECO-PTM
                    if test in ["1", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"]:
                        pPO.compareHeatMaps(thisBaseOutputDir, thisOutputDir)
                    
                    # Spatial heat map comparing routing of this model versus the base ECO-PTM,
                    # showing interior Delta as a different color
                    if test in ["2"]:
                        pPO.compareHeatMaps(thisBaseOutputDir, thisOutputDir, region="interiorDelta")
                        pPO.compareHeatMaps(thisBaseOutputDir, thisOutputDir, region="northDelta")
                    
                    # Survival fraction
                    if test in ["4", "5", "6", "7", "8", "9", "10", "11", "12", "13"]:
                        description, survivalFrac = pPO.survivalFrac(thisOutputDir, groupID=self.groupIDs[test])
                        print("-"*75, file=fH)
                        description = description.replace("groupID", "reach")
                        print("survival fraction, " + description + ": " + str(survivalFrac), file=fH)
                        
                        description, survivalFracDiff = pPO.compareSurvivalFrac(thisBaseOutputDir, thisOutputDir, self.groupIDs[test])
                        description = description.replace("groupID", "reach")
                        print("survivalFracDiff (new-base ECO-PTM), " + description + ": " + str(survivalFracDiff), file=fH)
                    
                    # Survival probability
                    if test in ["4", "5", "6", "7", "8", "9", "10", "11", "12", "13"]:
                        pPO.survivalProbDist(thisOutputDir, self.groupIDs[test])
                        statistic, pValue, overlapCoeff = pPO.compareSurvivalProbDist(thisBaseOutputDir, thisOutputDir, self.groupIDs[test])
                        print("-"*75, file=fH)
                        print("survival probability distribution statistics, reach ", self.groupIDs[test], 
                                " (", survivalGroups.loc[self.groupIDs[test], "station"], ")", sep="", file=fH)
                        print("survival probability Anderson-Darling statistic:", statistic, ", pValue:", pValue, file=fH)
                        print("survival probability overlap coefficient:", overlapCoeff, file=fH)
                    
                    # Travel time
                    if test in ["4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14"]:
                        pPO.travelTimeDist(thisOutputDir)
                        
                        statistic, pValue, overlapCoeff = pPO.compareTravelTimeDist(thisBaseOutputDir, thisOutputDir)
                        print("-"*75, file=fH)
                        print("travel time distribution statistics", file=fH)
                        print("travel time Anderson-Darling statistic:", statistic, ", pValue:", pValue, file=fH)
                        print("travel time overlap coefficient:", overlapCoeff, file=fH)
                    
                    # Route-specific survival probabilities
                    if test in ["14"]:
                        routeSurvivalProbs = pPO.routeSurvivalProb(thisOutputDir, self.routeGroupIDs)
                        print("route-specific survival probabilities:", file=fH)
                        print("(-999 indicates survival probability calculation for route is missing one or more reaches, e.g., because the DCC is closed.)",
                            file=fH)
                        print(routeSurvivalProbs.to_string(index=False), file=fH)
                        
                        routeSurvivalProbDiffs = pPO.compareRouteSurvivalProb(thisBaseOutputDir, thisOutputDir, self.routeGroupIDs)
                        print("difference in route-specific survival probabilities (new-base ECO-PTM):", file=fH)
                        print("(-999 indicates survival probability calculation for route is missing one or more reaches, e.g., because the DCC is closed.)",
                            file=fH)
                        print(routeSurvivalProbDiffs.to_string(index=False), file=fH)
                        
                        # Create a spatial heat map of the route-specific survival probabilities
                        pPO.spatialHeatMapRouteSurvivalProb(thisOutputDir, self.routeGroupIDs)
                        pPO.spatialHeatMapRouteSurvivalProb(thisBaseOutputDir, self.routeGroupIDs, baseECO_PTM=True)
                    
                    print("done")
        
            # Print route descriptions
            print("="*160, file=fH)
            print("route descriptions", file=fH)
            for route in self.routeDescriptions:
                print("route ", route, ": ", self.routeDescriptions[route], sep="", file=fH)
            
            # Print reach descriptions
            print("="*160, file=fH)
            print("reach descriptions", file=fH)
            for reach in self.reachDescriptions:
                print("reach ", reach, ": ", self.reachDescriptions[reach], sep="", file=fH)
            
            # Print node descriptions
            print("="*160, file=fH)
            print("node descriptions", file=fH)
            for nodeID in self.nodeDescriptions:
                print("nodeID ", nodeID, ": ", self.nodeDescriptions[nodeID], sep="", file=fH)
                
        # Remove the temporary copy of the base ECO-PTM outputs
        os.remove(baseOutputZipFile)
        shutil.rmtree(baseOutputDir)

    def runProfilingTest(self):
        """Run a long-running test so the user can profile the code using VisualVM"""
        # Create a directory with the datetime stamp for this profiling run
        profilingDir = os.path.join(self.workingDir, "profile_" + dt.datetime.now().strftime("%d%b%y_%H.%M").lower())

        # Create a subdirectory for this profile run
        if os.path.exists(profilingDir):
            shutil.rmtree(profilingDir)
        os.makedirs(profilingDir)
        os.makedirs(os.path.join(profilingDir, "output"))
                
        print("Launching profiling run.")
        
        # Temporary kluge: Copy an existing DSS file into the output directory to avoid DSS ERROR
        shutil.copy(os.path.join(self.rootDir, "data", "ptmout-10-22-1997.dss"),
                    os.path.join(profilingDir, "output", "ptmout_profiling.dss"))        
            
        # Read the configuration file templates
        with open(os.path.join(self.templatesDir, "template_dsm2_config.inp"), "r") as fH:
            dsm2Config = fH.read()
        with open(os.path.join(self.templatesDir, "template_ptm_config.inp"), "r") as fH:
            ptmConfig = fH.read()
        with open(os.path.join(self.templatesDir, "template_ptm_behavior_inputs.inp"), "r") as fH:
            behaviorConfigTemplate = fH.read()
        with open(os.path.join(self.templatesDir, "template_ptm_behavior_inputs_profiling.inp"), "r") as fH:
            behaviorConfigTestSpecific = fH.read()
                
        # Modify the configuration files
        dsm2Config = dsm2Config.replace("DSM2MODIFIER_PLACEHOLDER", "profiling")        
        dsm2Config = dsm2Config.replace("START_DATE_PLACEHOLDER", self.profilingScenarios["profiling"]["startDate"])        
        dsm2Config = dsm2Config.replace("END_DATE_PLACEHOLDER", self.profilingScenarios["profiling"]["endDate"])        
        dsm2Config = dsm2Config.replace("HYDROTIDEFILE_PLACEHOLDER", self.tidefilePath)
        dsm2Config = dsm2Config.replace("PTMOUTFILE_PLACEHOLDER", "profiling.pof")
        dsm2Config = dsm2Config.replace("PTMOUTPUTFILE_PLACEHOLDER", "ptmout_profiling")
        
        ptmConfig = ptmConfig.replace("DSM2_CONFIG_PLACEHOLDER",
                                        "dsm2_config_profiling.inp")
        ptmConfig = ptmConfig.replace("BEHAVIOR_CONFIG_PLACEHOLDER",
                                        "ptm_behavior_inputs_profiling.inp")
        ptmConfig = ptmConfig.replace("TEST_PLACEHOLDER", "profiling")

        behaviorConfig = behaviorConfigTemplate.replace("TEST-SPECIFIC_PLACEHOLDER", behaviorConfigTestSpecific)                                        
        behaviorConfig = behaviorConfig.replace("TEST_PLACEHOLDER", "profiling")
        behaviorConfig = behaviorConfig.replace("USE_NEW_RANDOM_SEED_PLACEHOLDER", self.useNewRandomSeed)

        # Fix path separators for Linux
        if platform.system()=="Linux":
            behaviorConfig = behaviorConfig.replace("\\output\\", "/output/")
                                                        
        # Write the modified configuration files
        with open(os.path.join(profilingDir, "dsm2_config_profiling.inp"), "w") as fH:
            print(dsm2Config, file=fH)
        with open(os.path.join(profilingDir, "ptm_config_profiling.inp"), "w") as fH:
            print(ptmConfig, file=fH)
        with open(os.path.join(profilingDir, "ptm_behavior_inputs_profiling.inp"), "w") as fH:
            print(behaviorConfig, file=fH)
                
        # Run the test
        os.chdir(profilingDir)

        javaCommand = (self.javaPath + " -Xss5M -Xms512M -Xmx512M -classpath " + '"' + self.classpath + '" ' +
                "DWR.DMS.PTM.MainPTM " + "ptm_config_profiling.inp")
        print(javaCommand)     

        with subprocess.Popen(javaCommand, stdout=subprocess.PIPE, shell=self.shell,
                            bufsize=1, universal_newlines=True) as p:

            # Print stdout line-by-line
            for line in p.stdout:
                print(line, end="")

if __name__=="__main__":
    import argparse
    import json
     
     # Read in command line arguments
    parser = argparse.ArgumentParser(description="Automatic test framework for ECO-PTM model.")
    parser.add_argument("--configFile", action="store", dest="configFile", required=True)
    args = parser.parse_args()

    configFile = args.configFile

    # Read configuration file
    try:
        with open(configFile) as fH:
            config = json.load(fH)
    except IOError as e:
        print(f"Could not load configuration file {configFile}. Does it exist? {e}")
        sys.exit()
    except json.JSONDecodeError as e:
        print(f"Error while parsing PTM configuration file: {e}")
        sys.exit()            

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
        sys.exit()
    except KeyError as e:
        print(f"Required parameter {e} missing from configuration file.")
        sys.exit()
    
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
                sys.exit()
    print(f"Scenarios to run: {t.flowScenarios}")
    print("="*75)
                
    if runTestSuite:
        t.runTestSuite()
        t.processTestSuite()
    
    if runProfilingTest:
        t.runProfilingTest()