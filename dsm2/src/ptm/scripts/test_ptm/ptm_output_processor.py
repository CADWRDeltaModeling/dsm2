"""Creates plots and generates summary statistics from ECO-PTM model outputs.

Intended for use by the test_ptm automatic test framework or as a set of tools 
available for use by unit tests or ad hoc analyses.
"""
import os
import warnings
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patheffects as pe
from matplotlib import cm
import seaborn as sns
from scipy.stats import ks_2samp, anderson_ksamp, gaussian_kde

from ptm_output_reader import PTMoutputReader

__author__ = "Doug Jackson, QEDA Consulting, LLC"
__email__ = "doug@QEDAconsulting.com"

class PTMoutputProcessor:

    def __init__(self, rootDir, mainOutputDir, testName):
        """Initialize ProcessPTMoutputs object.
        
        Keyword arguments:
        rootDir (str) -- the location where test_ptm is being run
        mainOutputDir (str) -- the location where the plots and other summary outputs will be saved
        testName (str) -- the name of the test; used to label plots and other outputs
        """
        self.rootDir = rootDir
        self.mainOutputDir = mainOutputDir
        self.testName = testName
        
        self.minLineWidth = 1
        self.maxLineWidth = 5
        self.minColormapVal = 0.1
        
        self.tickLabelSize = 14
        self.axisLabelSize = 18
        self.legendFontSize = 14
        self.titleFontSize = 20
        
        # Read the channel and node mapping
        try:
            self.channels = pd.read_csv(os.path.join(self.rootDir, "data", "channels_v8_1_2.csv"))
            self.channelsIntExt = pd.read_csv(os.path.join(self.rootDir, "data", "channelsIntExt_v8_1_2.csv"))
            self.nodes = pd.read_csv(os.path.join(self.rootDir, "data", "nodes_v8_1_2.csv"))
            self.nodesIntExt = pd.read_csv(os.path.join(self.rootDir, "data", "nodesIntExt_v8_1_2.csv"))
        except:
            print("Could not read the channel and node mapping.")
    
    def calcOverlapCoeff(self, data1, data2):
        """Calculate the overlap coefficient for two distributions.
        
        Keyword arguments:
        data1 (vector) -- vector of values in distribution 1 
        data2 (vector) -- vector of values in distribution 2
        """
        # Convert data to numpy arrays
        data1 = np.array(data1)
        data2 = np.array(data2)

        dataMin = np.min([data1.min(), data2.min()])
        dataMax = np.max([data1.max(), data2.max()])

        # Cannot calculate stepSize if dataMin==dataMax
        if dataMin==dataMax:
            return -999
        
        try:
            # Calculate an appropriate step size
            stepSize = (dataMax-dataMin)/1000
            xVals = np.arange(dataMin, dataMax, stepSize)
            
            # Generate empirical PDFs using Gaussian kernel density estimation
            kde1 = gaussian_kde(data1)
            kde2 = gaussian_kde(data2)

            # Calculate the actual areas under both curves
            sumKde1 = np.sum(kde1(xVals))*stepSize
            sumKde2 = np.sum(kde2(xVals))*stepSize

            # Calculate the overlap
            overlap = 0
            for x in xVals:
                overlap+=np.min([kde1(x).min(), kde2(x).min()])*stepSize

            # Calculate the overlap coefficient
            overlapCoeff = np.round(overlap/np.min([sumKde1, sumKde2]), 3)
        
        except:
            return -999
        
        return overlapCoeff
    
    def spatialHeatMap(self, outputDir, region="all", baseECO_PTM=False):
        """Use the trace file to create a spatial heat map showing where particles were seen.
        
        Keyword arguments:
        outputDir (str) -- path to the ECO-PTM outputs
        region (str) -- name of region definition used to mark regions with different colors
        """
        o = PTMoutputReader(outputDir)
        
        # Make local copies of the outputs to avoid side effects
        trace = o.trace.copy()
        
        # Count the unique particles seen in each channel
        trace = trace[["particleNum", "wbNum"]]
        trace = trace.loc[(trace["wbNum"]>0) & (trace["wbNum"]<=self.channelsIntExt["internal"].max())]
        trace = trace.drop_duplicates()
        traceGrp = trace.groupby(["wbNum"], as_index=False)
        particleCount = traceGrp["particleNum"].count()
        particleCount = particleCount.rename(columns={"wbNum":"channel", "particleNum":"particleCount"})
    
        # Look up the upNode and downNode coordinates for each channel
        particleCount = pd.merge(particleCount, self.channels, on="channel", how="outer")
        particleCount = pd.merge(particleCount, self.nodes, left_on="upNode", right_on="Node")
        particleCount = particleCount.rename(columns={"x":"upX", "y":"upY"})
        particleCount = pd.merge(particleCount, self.nodes, left_on="downNode", right_on="Node")
        particleCount = particleCount.rename(columns={"x":"downX", "y":"downY"})
        particleCount = particleCount.drop(["Node_x", "Node_y"], axis=1)
        particleCount.fillna(0, inplace=True)
    
        # Calculate a line width based on particleCount
        particleCount["logParticleCount"] = np.log(particleCount["particleCount"]+1)
        minLogParticleCount = particleCount["logParticleCount"].min()
        maxLogParticleCount = particleCount["logParticleCount"].max()
        particleCount["interpFac"] = (particleCount["logParticleCount"]-minLogParticleCount)/(maxLogParticleCount-minLogParticleCount)
        particleCount["lineWidth"] = self.minLineWidth + particleCount["interpFac"]*(self.maxLineWidth-self.minLineWidth)
        particleCount["colormapVal"] = self.minColormapVal + particleCount["interpFac"]*(1-self.minColormapVal)
    
        # Exclude marked channels
        particleCount = particleCount.loc[particleCount["region_" + region]!="EXCLUDE"]
        
        # Create and save the spatial heat map
        fig, ax = plt.subplots(figsize=[6, 8])
        for index, row in particleCount.iterrows():
            if row["particleCount"]>0 and row["region_" + region]=="Y":
                ax.plot([row["upX"], row["downX"]], [row["upY"], row["downY"]], linewidth=row["lineWidth"], 
                        color=cm.Reds(self.scaleLogistic(row["colormapVal"])))
                ax.text((row["upX"]+row["downX"])/2, (row["upY"]+row["downY"])/2, int(row["particleCount"]),
                        color=cm.Reds(self.scaleLogistic(1-row["colormapVal"])), fontsize=1)                
            elif row["particleCount"]>0 and row["region_" + region]=="N":
                ax.plot([row["upX"], row["downX"]], [row["upY"], row["downY"]], linewidth=row["lineWidth"], 
                        color=cm.Blues(self.scaleLogistic(row["colormapVal"])))
                ax.text((row["upX"]+row["downX"])/2, (row["upY"]+row["downY"])/2, int(row["particleCount"]),
                        color=cm.Blues(self.scaleLogistic(1-row["colormapVal"])), fontsize=1)           
            else:
                ax.plot([row["upX"], row["downX"]], [row["upY"], row["downY"]], linewidth=0.5, color="black")     
        
        # Calculate the minimum and maximum coordinates
        xMin = np.min([particleCount["upX"].min(), particleCount["downX"].min()])
        xMax = np.max([particleCount["upX"].max(), particleCount["downX"].max()])
        yMin = np.min([particleCount["upY"].min(), particleCount["downY"].min()])
        yMax = np.min([particleCount["upY"].max(), particleCount["downY"].max()])

        ax.set_xlim([xMin-10, xMax+10])
        ax.set_ylim([yMin-10, yMax+10])
        plt.gca().set_aspect('equal', adjustable='box')
        plt.gca().axes.get_xaxis().set_visible(False)
        plt.gca().axes.get_yaxis().set_visible(False)
    
        if baseECO_PTM:
            ax.set_title(self.testName + ": Particle counts, base ECO-PTM")
            plt.savefig(os.path.join(self.mainOutputDir, self.testName + "_spatialHeatMapBaseECO_PTM.pdf"))
        else:
            ax.set_title(self.testName + ": Particle counts, new ECO-PTM")
            plt.savefig(os.path.join(self.mainOutputDir, self.testName + "_spatialHeatMap.pdf"))
        
        plt.close("all")
    
    def compareHeatMaps(self, outputDir1, outputDir2, region="all"):
        """Compare the count of particles seen in each channel in two different runs.
        
        Keyword arguments:
        outputDir1 (str) -- path to the ECO-PTM outputs for the first run
        outputDir2 (str) -- path to the ECO-PTM outputs for the second run
        region (str) -- name of the region definition used to mark regions with different colors
        """
        o1 = PTMoutputReader(outputDir1)
        o2 = PTMoutputReader(outputDir2)
        
        # Make local copies of the outputs to avoid side effects
        trace1 = o1.trace.copy()
        trace2 = o2.trace.copy()
        
        # Count the unique particles seen in each channel
        trace1 = trace1[["particleNum", "wbNum"]]
        trace1 = trace1.loc[(trace1["wbNum"]>0) & (trace1["wbNum"]<=self.channelsIntExt["internal"].max())]
        trace1 = trace1.drop_duplicates()
        trace1grp = trace1.groupby(["wbNum"], as_index=False)
        particleCount1 = trace1grp["particleNum"].count()
        particleCount1 = particleCount1.rename(columns={"wbNum":"channel", "particleNum":"particleCount"})
        
        trace2 = trace2[["particleNum", "wbNum"]]
        trace2 = trace2.loc[(trace2["wbNum"]>0) & (trace2["wbNum"]<=self.channelsIntExt["internal"].max())]
        trace2 = trace2.drop_duplicates()
        trace2grp = trace2.groupby(["wbNum"], as_index=False)
        particleCount2 = trace2grp["particleNum"].count()
        particleCount2 = particleCount2.rename(columns={"wbNum":"channel", "particleNum":"particleCount"})
        
        # Merge the particle counts for the two runs
        particleCount = pd.merge(particleCount1, particleCount2, on="channel", how="outer", suffixes=["_1", "_2"])
        particleCount.fillna(0, inplace=True)
        particleCount["particleCount"] = particleCount["particleCount_2"] - particleCount["particleCount_1"]
        # Look up the upNode and downNode coordinates for each channel
        particleCount = pd.merge(particleCount, self.channels, on="channel", how="outer")
        particleCount = pd.merge(particleCount, self.nodes, left_on="upNode", right_on="Node")
        particleCount = particleCount.rename(columns={"x":"upX", "y":"upY"})
        particleCount = pd.merge(particleCount, self.nodes, left_on="downNode", right_on="Node")
        particleCount = particleCount.rename(columns={"x":"downX", "y":"downY"})
        particleCount = particleCount.drop(["Node_x", "Node_y"], axis=1)
        particleCount.fillna(0, inplace=True)
    
        # Determine the number of particles inserted
        numParticles = np.max([particleCount["particleCount_1"].max(), particleCount["particleCount_2"].max()])
        
        # Scale colors and line widths to 10% of the number of particles inserted
        maxParticleCount = np.ceil(numParticles/10)
        
        # Calculate a line width based on particleCount
        particleCount["interpFac"] = (particleCount["particleCount"])/(maxParticleCount)
        particleCount.loc[particleCount["interpFac"]>1, "interpFac"] = 1
        particleCount["lineWidth"] = self.minLineWidth + np.abs(particleCount["interpFac"])*(self.maxLineWidth-self.minLineWidth)
        particleCount["colormapVal"] = np.abs(particleCount["interpFac"])
        
        # Exclude marked channels
        particleCount = particleCount.loc[particleCount["region_" + region]!="EXCLUDE"]
           
        # Create and save the spatial heat map
        fig, ax = plt.subplots(figsize=[6, 8])
        for index, row in particleCount.iterrows():               
            if row["particleCount"]>0  and row["region_" + region]=="Y":
                ax.plot([row["upX"], row["downX"]], [row["upY"], row["downY"]], linewidth=row["lineWidth"], 
                        color=cm.Blues(self.scaleLogistic(row["colormapVal"])))
                ax.text((row["upX"]+row["downX"])/2, (row["upY"]+row["downY"])/2, int(row["particleCount"]),
                        color=cm.Blues(self.scaleLogistic(1-row["colormapVal"])), fontsize=1)
            elif row["particleCount"]<0 and row["region_" + region]=="Y":
                ax.plot([row["upX"], row["downX"]], [row["upY"], row["downY"]], linewidth=row["lineWidth"], 
                        color=cm.Reds(self.scaleLogistic(row["colormapVal"])))
                ax.text((row["upX"]+row["downX"])/2, (row["upY"]+row["downY"])/2, int(row["particleCount"]),
                        color=cm.Reds(self.scaleLogistic(1-row["colormapVal"])), fontsize=1)
            elif row["region_" + region]=="Y":
                ax.plot([row["upX"], row["downX"]], [row["upY"], row["downY"]], linewidth=0.5, color="black")                
            
        # Calculate the minimum and maximum coordinates
        xMin = np.min([particleCount["upX"].min(), particleCount["downX"].min()])
        xMax = np.max([particleCount["upX"].max(), particleCount["downX"].max()])
        yMin = np.min([particleCount["upY"].min(), particleCount["downY"].min()])
        yMax = np.min([particleCount["upY"].max(), particleCount["downY"].max()])
        
        ax.set_xlim([xMin-10, xMax+10])
        ax.set_ylim([yMin-10, yMax+10])
        plt.gca().set_aspect('equal', adjustable='box')
        plt.gca().axes.get_xaxis().set_visible(False)
        plt.gca().axes.get_yaxis().set_visible(False)
    
        if region=="all":
            ax.set_title(self.testName + ":\nParticle counts, new - base ECO-PTM")
            plt.savefig(os.path.join(self.mainOutputDir, self.testName + "_compareHeatMaps.pdf"))
        else:
            ax.set_title(self.testName + ": " + region + "\nParticle counts, new - base ECO-PTM")
            plt.savefig(os.path.join(self.mainOutputDir, self.testName + "_compareHeatMaps_" + region + ".pdf"))
            
        plt.close("all")
    
    def travelTimeDist(self, outputDir):
        """Use the travel time file to create a histogram of travel times.
        
        Keyword arguments:
        outputDir (str) -- path to the ECO-PTM outputs
        """
        o = PTMoutputReader(outputDir)
        
        # Make local copies of the outputs to avoid side effects
        travelTime = o.travelTime.copy()

        if travelTime.shape[0]==0:
            print("Could not plot travel time distributions: no data to plot.")
            return       

        maxTravelTime = travelTime["travelTime_min"].max()
        bins = np.linspace(0, maxTravelTime, 100)
        
        # Create and save the histogram
        fig, ax = plt.subplots(figsize=[10, 8])
        sns.histplot(travelTime["travelTime_min"], bins=bins, stat="density",
                     label="new ECO-PTM", color="blue", alpha=0.4, edgecolor=None, ax=ax)
        ax.set_title(self.testName, fontsize=self.titleFontSize)
        ax.set_xlim([0, travelTime["travelTime_min"].max()])
        ax.set_xlabel("travel time (minutes)", fontsize=self.axisLabelSize)
        ax.set_ylabel("density", fontsize=self.axisLabelSize)
        ax.tick_params(axis="both", which="major", labelsize=self.tickLabelSize)
    
        plt.savefig(os.path.join(self.mainOutputDir, self.testName + "_travelTimeDist.pdf"))
        plt.close("all")
    
    def compareTravelTimeDist(self, outputDir1, outputDir2):
        """Compare the travel time distributions from two different runs.
        
        Keyword arguments:
        outputDir1 (str) -- path to the ECO-PTM outputs for the base ECO-PTM
        outputDir2 (str) -- path to the ECO-PTM outputs for the new ECO-PTM
        """        
        o1 = PTMoutputReader(outputDir1)
        o2 = PTMoutputReader(outputDir2)
        
        # Make local copies of the outputs to avoid side effects
        travelTime1 = o1.travelTime.copy()
        travelTime2 = o2.travelTime.copy()

        if travelTime1.shape[0]==0 or travelTime2.shape[0]==0:
            print("Could not plot travel time distributions: no data to plot.")
            return -999, -999, -999
        
        maxTravelTime = np.max([travelTime1["travelTime_min"].max(), travelTime2["travelTime_min"].max()])
        bins = np.linspace(0, maxTravelTime, 100)

        # Calculate the overlap coefficient
        overlapCoeff = self.calcOverlapCoeff(travelTime1["travelTime_min"], travelTime2["travelTime_min"])
        
        # Create and save the density plot
        fig, ax = plt.subplots(figsize=[10, 8])
        sns.histplot(travelTime1["travelTime_min"], bins=bins, stat="density",
                     label="base ECO-PTM", color="red", alpha=0.4, edgecolor=None, ax=ax)
        sns.histplot(travelTime2["travelTime_min"], bins=bins, stat="density", 
                     label="new ECO-PTM", color="blue", alpha=0.4, edgecolor=None, ax=ax)
        ax.set_title((self.testName + ", overlap coefficient:" + str(overlapCoeff)), fontsize=self.titleFontSize)
        ax.set_xlim([0, maxTravelTime])
        ax.set_xlabel("travel time (minutes)", fontsize=self.axisLabelSize)
        ax.set_ylabel("density", fontsize=self.axisLabelSize)
        ax.tick_params(axis="both", which="major", labelsize=self.tickLabelSize)
        ax.legend(fontsize=self.legendFontSize)
    
        plt.savefig(os.path.join(self.mainOutputDir, self.testName + "_compareTravelTimeDist.pdf"))
        plt.close("all") 
               
        # Calculate the k-sample Anderson-Darling test statistics
        try:
            with warnings.catch_warnings():
                warnings.simplefilter("ignore")
                aD = anderson_ksamp([travelTime1["travelTime_min"], travelTime2["travelTime_min"]])            
            statistic = np.round(aD.statistic, decimals=4)
            pValue = np.round(aD.significance_level, decimals=4)
                   
            return statistic, pValue, overlapCoeff
        except:
            return -999, -999, -999
    
    def entrainmentProbDist(self, outputDir, nodeID):
        """Use the entrainment file to create a histogram of entrainment probabilities.
        
        Keyword arguments:
        outputDir (str) -- path to the ECO-PTM outputs
        nodeID (int) -- the node at which to monitor entrainment
        """
        o = PTMoutputReader(outputDir)
        
        # Make local copies of the outputs to avoid side effects
        entrainment = o.entrainment.copy()
                              
        # Extract the entrainment probabilities for just the node of interest
        entrainment = entrainment.loc[entrainment["nodeID"]==nodeID]
        
        if entrainment.shape[0]==0:
            print("Could not plot entrainment distributions: no data to plot.")
            return     
                
        # Create and save the histogram
        fig, ax = plt.subplots(figsize=[10, 8])
        sns.histplot(entrainment["entrainmentProb"], bins=np.linspace(0, 1, 100), stat="density",
                     label="new ECO-PTM", color="blue", alpha=0.4, edgecolor=None, ax=ax)
        ax.set_title(self.testName + ", node " + str(nodeID), fontsize=self.titleFontSize)
        ax.set_xlim([0, 1])
        ax.set_xlabel("entrainment probability", fontsize=self.axisLabelSize)
        ax.set_ylabel("density", fontsize=self.axisLabelSize)
        ax.tick_params(axis="both", which="major", labelsize=self.tickLabelSize)
    
        plt.savefig((os.path.join(self.mainOutputDir, self.testName + 
                                  "_entrainmentProbDist_nodeID_" + str(nodeID) + ".pdf")))
        plt.close("all") 

    def compareEntrainmentProbDist(self, outputDir1, outputDir2, nodeID):
        """Compare the entrainment probability distributions from two different runs.
        
        Keyword arguments:
        outputDir1 (str) -- path to the ECO-PTM outputs for the base ECO-PTM
        outputDir2 (str) -- path to the ECO-PTM outputs for the new ECO-PTM
        nodeID (int) -- the node at which to monitor entrainment        
        """        
        o1 = PTMoutputReader(outputDir1)
        o2 = PTMoutputReader(outputDir2)
        
        # Make local copies of the outputs to avoid side effects
        entrainment1 = o1.entrainment.copy()
        entrainment2 = o2.entrainment.copy()
        
        # Extract the entrainment probabilities for just the node of interest
        entrainment1 = entrainment1.loc[entrainment1["nodeID"]==nodeID]
        entrainment2 = entrainment2.loc[entrainment2["nodeID"]==nodeID]

        if entrainment1.shape[0]==0 or entrainment2.shape[0]==0:
            print("Could not plot entrainment distributions: no data to plot.")
            return -999, -999, -999        
        
        bins=np.linspace(0, 1, 100)

        # Calculate the overlap coefficient
        overlapCoeff = self.calcOverlapCoeff(entrainment1["entrainmentProb"], entrainment2["entrainmentProb"])
                
        # Create and save the density plot
        fig, ax = plt.subplots(figsize=[10, 8])
        sns.histplot(entrainment1["entrainmentProb"], bins=bins, stat="density",
                     label="base ECO-PTM", color="red", alpha=0.4, edgecolor=None, ax=ax)
        sns.histplot(entrainment2["entrainmentProb"], bins=bins, stat="density",
                     label="new ECO-PTM", color="blue", alpha=0.4, edgecolor=None, ax=ax)
        ax.set_title((self.testName + ", node " + str(nodeID) + ", overlap coefficient:" + str(overlapCoeff)), 
                        fontsize=self.titleFontSize)
        ax.set_xlim([0, 1])
        ax.set_xlabel("entrainment probability", fontsize=self.axisLabelSize)
        ax.set_ylabel("density", fontsize=self.axisLabelSize)
        ax.tick_params(axis="both", which="major", labelsize=self.tickLabelSize)
        ax.legend(fontsize=self.legendFontSize)
    
        plt.savefig((os.path.join(self.mainOutputDir, self.testName + 
                                  "_compareEntrainmentProbDist_nodeID_" + str(nodeID) + ".pdf")))
        plt.close("all") 

        # Calculate the k-sample Anderson-Darling test statistics
        try:
            with warnings.catch_warnings():
                warnings.simplefilter("ignore")
                aD = anderson_ksamp([entrainment1["entrainmentProb"], entrainment2["entrainmentProb"]])        
            statistic = np.round(aD.statistic, decimals=4)
            pValue = np.round(aD.significance_level, decimals=4)
                
            return statistic, pValue, overlapCoeff
        except:
            return -999, -999, -999

    def entrainmentFrac(self, outputDir, groupIDs):
        """Calculate fraction of fish entrained in all routes exiting a junction.
        
        Keyword arguments:
        outputDir (str) -- path to the ECO-PTM outputs
        groupIDs (list of ints) -- all survival groups exiting the junction
        """
        o = PTMoutputReader(outputDir)
        
        # Make local copies of the outputs to avoid side effects
        survival = o.survival.copy()
        
        # Subset to just the relevant groups
        survival = survival.loc[survival["groupID"].isin(groupIDs)]
        
        # Concatenate groupID and station
        actualGroupIDs = survival["groupID"].unique()
        descriptions = ["groupID " + str(g) + " (" + str(survival.loc[survival["groupID"]==g, "station"].values[0]) + ")" for g in actualGroupIDs]
        
        # Calculate the entrainment fractions
        # Entrainment fractions are based only on particles that have a complete time history, not on all of the particles
        # that passed the start station. That is, only particles whose fate (died or survived) is known are considered.
        survival["numKnownFate"] = survival["numLost"] + survival["numSurvived"]
        
        return dict(zip(descriptions, list(np.round(survival["numKnownFate"]/survival["numKnownFate"].sum(), decimals=4))))
    
    def compareEntrainmentFracs(self, outputDir1, outputDir2, groupIDs):
        """Compare fraction of fish entrained in all routes exiting a junction from two different runs.
        
        Keyword arguments:
        outputDir1 (str) -- path to the ECO-PTM outputs for the base ECO-PTM
        outputDir2 (str) -- path to the ECO-PTM outputs for the new ECO-PTM
        groupIDs (list of ints) -- all survival groups exiting the junction
        """  
        entrainmentFrac1 = self.entrainmentFrac(outputDir1, groupIDs)
        entrainmentFrac2 = self.entrainmentFrac(outputDir2, groupIDs)
        
        entrainmentFracDiff = {}
        for description in entrainmentFrac1.keys():
            try:
                entrainmentFracDiff[description] = np.round(entrainmentFrac2[description] - entrainmentFrac1[description],
                                                           decimals=4)
            except:
                entrainmentFracDiff[description] = -999
        
        return entrainmentFracDiff

    def survivalFrac(self, outputDir, groupID):
        """Read the survival fraction for the specified survival group.
        
        Keyword arguments:
        outputDir (str) -- path to the ECO-PTM outputs
        groupID (str) -- the survival group for which to read survival fraction
        """
        o = PTMoutputReader(outputDir)
        
        # Make local copies of the outputs to avoid side effects
        survival = o.survival.copy()
               
        # Return the survival fraction for the group of interest
        try:
            # Concatenate groupID and station
            description = ("groupID " + str(groupID) + " (" + o.survivalGroups.loc[groupID, "station"] + ")")
            thisSurvivalFrac = float(survival.loc[survival["groupID"]==groupID, "survivalFrac"].values[0].round(decimals=4))
            return description, thisSurvivalFrac
        except:
            print("Could not calculate survival fraction. Do all output files exist? Did any particles reach the end station?")
            return "No group description available", -999
        
    def compareSurvivalFrac(self, outputDir1, outputDir2, groupID):
        """Compare survival fractions from two different runs.
        
        Keyword arguments:
        outputDir1 (str) -- path to the ECO-PTM outputs for the base ECO-PTM
        outputDir2 (str) -- path to the ECO-PTM outputs for the new ECO-PTM
        groupID (int) -- the survival group for which to compare survival fractions  
        """
        o1 = PTMoutputReader(outputDir1)
        o2 = PTMoutputReader(outputDir2)
        
        # Make local copies of the outputs to avoid side effects
        survival1 = o1.survival.copy()
        survival2 = o2.survival.copy()
        
        # Extract the survival fractions for just the group of interest
        try:
            # Concatenate groupID and station
            description = ("groupID " + str(groupID) + " (" + o1.survivalGroups.loc[groupID, "station"] + ")")
            survivalFrac1 = float(survival1.loc[survival1["groupID"]==groupID, "survivalFrac"].values[0])
            survivalFrac2 = float(survival2.loc[survival2["groupID"]==groupID, "survivalFrac"].values[0])
            return description, np.round(survivalFrac2 - survivalFrac1, decimals=4)
        except:
            print("Could not calculate survival fraction. Do all output files exist? Did any particles reach the end station?")
            return "No group description available", -999
        
    def survivalProbDist(self, outputDir, groupID):
        """Create a histogram of survival probabilities.
        
        Keyword arguments:
        outputDir (str) -- path to the ECO-PTM outputs
        groupID (int) -- the survival group for which to monitor survival probability
        """
        o = PTMoutputReader(outputDir)
        
        # Make local copies of the outputs to avoid side effects
        survivalProb = o.survivalProb.copy()

        # Extract the survival probabilities for just the group of interest
        survivalProb = survivalProb.loc[survivalProb["groupID"]==groupID]

        if survivalProb.shape[0]==0:
            print("Could not plot survival probability distributions: no data to plot.")
            return 
                
        bins = np.linspace(0, 1, 100)
        
        # Create and save the histogram
        fig, ax = plt.subplots(figsize=[10, 8])
        sns.histplot(survivalProb["survivalProb"], bins=bins, stat="density", 
                     label="new ECO-PTM", color="blue", alpha=0.4, edgecolor=None, ax=ax)
        ax.set_title(self.testName + ", reach " + str(groupID) + " (" + o.survivalGroups.loc[groupID, "station"] + ")",
                        fontsize=self.titleFontSize)
        ax.set_xlim([0, 1])
        ax.set_xlabel("survival probability", fontsize=self.axisLabelSize)
        ax.set_ylabel("density", fontsize=self.axisLabelSize)
        ax.tick_params(axis="both", which="major", labelsize=self.tickLabelSize)
    
        plt.savefig((os.path.join(self.mainOutputDir, self.testName + 
                                  "_survivalProbDist_reach_" + str(groupID) + ".pdf")))
        plt.close("all") 

    def compareSurvivalProbDist(self, outputDir1, outputDir2, groupID):
        """Compare the survival probability distributions from two different runs.
        
        Keyword arguments:
        outputDir1 (str) -- path to the ECO-PTM outputs for the base ECO-PTM
        outputDir2 (str) -- path to the ECO-PTM outputs for the new ECO-PTM
        groupID (int) -- the survival group for which to monitor survival probability
        """        
        o1 = PTMoutputReader(outputDir1)
        o2 = PTMoutputReader(outputDir2)
        
        # Make local copies of the outputs to avoid side effects
        survivalProb1 = o1.survivalProb.copy()
        survivalProb2 = o2.survivalProb.copy()
        
        # Extract the survival probabilities for just the group of interest
        survivalProb1 = survivalProb1.loc[survivalProb1["groupID"]==groupID]
        survivalProb2 = survivalProb2.loc[survivalProb2["groupID"]==groupID]

        if survivalProb1.shape[0]==0 or survivalProb2.shape[0]==0:
            print("Could not plot survival probability distributions: no data to plot.")
            return -999, -999, -999
        
        bins = np.linspace(0, 1, 100)

        # Calculate the overlap coefficient
        overlapCoeff = self.calcOverlapCoeff(survivalProb1["survivalProb"], survivalProb2["survivalProb"])
                
        # Create and save the histogram
        fig, ax = plt.subplots(figsize=[10, 8])
        sns.histplot(survivalProb1["survivalProb"], bins=bins, stat="density",
                     label="base ECO-PTM", color="red", alpha=0.4, edgecolor=None, ax=ax)
        sns.histplot(survivalProb2["survivalProb"], bins=bins, stat="density",
                     label="new ECO-PTM", color="blue", alpha=0.4, edgecolor=None, ax=ax)
        ax.set_title(self.testName + ", reach " + 
                     str(groupID) + " (" + o1.survivalGroups.loc[groupID, "station"] + ")" +
                     ", overlap coefficient: " + str(overlapCoeff), fontsize=self.titleFontSize)
        ax.set_xlim([0, 1])
        ax.set_xlabel("survival probability", fontsize=self.axisLabelSize)
        ax.set_ylabel("density", fontsize=self.axisLabelSize)
        ax.tick_params(axis="both", which="major", labelsize=self.tickLabelSize)
        ax.legend(fontsize=self.legendFontSize)
    
        plt.savefig((os.path.join(self.mainOutputDir, self.testName + 
                                  "_compareSurvivalProbDist_reach_" + str(groupID) + ".pdf")))
        plt.close("all") 
        
        # Calculate the k-sample Anderson-Darling test statistics
        try:
            with warnings.catch_warnings():
                warnings.simplefilter("ignore")
                aD = anderson_ksamp([survivalProb1["survivalProb"], survivalProb2["survivalProb"]])        
            statistic = np.round(aD.statistic, decimals=4)
            pValue = np.round(aD.significance_level, decimals=4)
                   
            return statistic, pValue, overlapCoeff
        except:
            return -999, -999, -999

    def routeSurvivalProb(self, outputDir, routeGroupIDs):
        """Calculate route-specific survival probabilities.
        
        Keyword arguments:
        outputDir (str) -- path to the ECO-PTM outputs
        routeGroupIDs (dict of lists of integers) -- the survival groups that make up each route
        """
        o = PTMoutputReader(outputDir)
        
        # Make local copies of the outputs to avoid side effects
        survival = o.survival.copy()
        
        # Calculate survival probability for each route as the product of the reach-specific survivalFracs
        survivalProbsList = []
        for routeID in routeGroupIDs:
            thisSurvival = survival.loc[survival["groupID"].isin(routeGroupIDs[routeID]), "survivalFrac"]
            
            # Detect missing groups
            if thisSurvival.shape[0]!=len(routeGroupIDs[routeID]):
                thisSurvivalProb = -999
            else:
                thisSurvivalProb = thisSurvival.product()
            survivalProbsList.append(pd.DataFrame([{"routeID":routeID, "survivalProb":thisSurvivalProb}]))
        
        survivalProbs = pd.concat(survivalProbsList, ignore_index=True)
        
        # Round for easier viewing
        survivalProbs["survivalProb"] = survivalProbs["survivalProb"].round(4)
        
        return survivalProbs

    def compareRouteSurvivalProb(self, outputDir1, outputDir2, routeGroupIDs):
        """Compare the route-specific survival probabilities from two different runs.
        
        Keyword arguments:
        outputDir1 (str) -- path to the ECO-PTM outputs for the base ECO-PTM
        outputDir2 (str) -- path to the ECO-PTM outputs for the new ECO-PTM
        routeGroupIDs (dict of lists of integers) -- the survival groups that make up each route
        """
        o1 = PTMoutputReader(outputDir1)
        o2 = PTMoutputReader(outputDir2)
        
        # Make local copies of the outputs to avoid side effects
        survival1 = o1.survival.copy()
        survival2 = o2.survival.copy()
        
        # Calculate survival probability for each route as the product of the reach-specific survivalFracs
        survivalProbDiffsList = []
        for routeID in routeGroupIDs:
            thisSurvival1 = survival1.loc[survival1["groupID"].isin(routeGroupIDs[routeID]), "survivalFrac"]
            thisSurvival2 = survival2.loc[survival2["groupID"].isin(routeGroupIDs[routeID]), "survivalFrac"]
            
            # Detect missing groups
            if (thisSurvival1.shape[0]!=len(routeGroupIDs[routeID])) or (thisSurvival2.shape[0]!=len(routeGroupIDs[routeID])):
                thisSurvivalProbDiff = -999
            else:
                thisSurvivalProb1 = thisSurvival1.product()
                thisSurvivalProb2 = thisSurvival2.product()
                thisSurvivalProbDiff = thisSurvivalProb2 - thisSurvivalProb1
            
            survivalProbDiffsList.append(pd.DataFrame([{"routeID":routeID, 
                                                                        "survivalProbDiff":thisSurvivalProbDiff}]))
    
        survivalProbDiffs = pd.concat(survivalProbDiffsList, ignore_index=True)
        
        # Round for easier viewing
        survivalProbDiffs["survivalProbDiff"] = survivalProbDiffs["survivalProbDiff"].round(4)
        
        return survivalProbDiffs
    
    def spatialHeatMapRouteSurvivalProb(self, outputDir, routeGroupIDs, baseECO_PTM=False):
        """Plot the route-specific survival probabilities using a spatial heat map
        
        Keyword arguments:
        outputDir (str) -- path to the ECO-PTM outputs
        routeGroupIDs (dict of lists of integers) -- the survival groups that make up each route
        """
        o = PTMoutputReader(outputDir)
        
        # Make local copies of the outputs to avoid side effects
        survival = o.survival.copy()

        # Calculate survival probability for each route as the product of the reach-specific survivalFracs
        survivalProbsList = []
        for routeID in routeGroupIDs:
            thisSurvival = survival.loc[survival["groupID"].isin(routeGroupIDs[routeID]), "survivalFrac"]
            
            # Detect missing groups
            if thisSurvival.shape[0]!=len(routeGroupIDs[routeID]):
                thisSurvivalProb = -999
            else:
                thisSurvivalProb = thisSurvival.product()
            survivalProbsList.append(pd.DataFrame([{"routeID":routeID, "survivalProb":thisSurvivalProb}]))
        
        survivalProbs = pd.concat(survivalProbsList, ignore_index=True)
        
        # Merge the upNode and downNode coordinates with the channels                          
        channels = pd.merge(self.channels, self.nodes, left_on="upNode", right_on="Node")
        channels = channels.rename(columns={"x":"upX", "y":"upY"})
        channels = pd.merge(channels, self.nodes, left_on="downNode", right_on="Node")
        channels = channels.rename(columns={"x":"downX", "y":"downY"})                           

        # Exclude marked channels
        channels = channels.loc[channels["region_all"]!="EXCLUDE"]
                              
        # Plot the Delta
        fig, ax = plt.subplots(figsize=[6, 8])
        for index, row in channels.iterrows():
            ax.plot([row["upX"], row["downX"]], [row["upY"], row["downY"]], linewidth=self.minLineWidth, 
                    color="black")
        # Plot the survival probabilities for each route
        for i, routeID in enumerate(routeGroupIDs):
            route = "route" + str(routeID)
            thisSurvivalProb = float(survivalProbs.loc[survivalProbs["routeID"]==routeID, "survivalProb"].values[0])
            
            lineWidth = self.maxLineWidth - (0.75*self.maxLineWidth)*((i+1)/len(routeGroupIDs))
            
            thisChannels = channels[["channel", route, "upX", "upY", "downX", "downY"]]
                       
            thisChannels = thisChannels.loc[thisChannels[route].isin(["Y", "YT"])]
            for index, row in thisChannels.iterrows():
                ax.plot([row["upX"], row["downX"]], [row["upY"], row["downY"]], linewidth=lineWidth, 
                        color=cm.Reds(thisSurvivalProb))
                if row[route]=="YT":
                    ax.text((row["upX"]+row["downX"])/2, (row["upY"]+row["downY"])/2, np.round(thisSurvivalProb, 3), 
                        color="black", fontsize=1)
                        
        # Calculate the minimum and maximum coordinates
        xMin = np.min([channels["upX"].min(), channels["downX"].min()])
        xMax = np.max([channels["upX"].max(), channels["downX"].max()])
        yMin = np.min([channels["upY"].min(), channels["downY"].min()])
        yMax = np.min([channels["upY"].max(), channels["downY"].max()])
        ax.set_xlim([xMin-10, xMax+10])
        ax.set_ylim([yMin-10, yMax+10])
        plt.gca().set_aspect('equal', adjustable='box')
        plt.gca().axes.get_xaxis().set_visible(False)
        plt.gca().axes.get_yaxis().set_visible(False)
    
        if baseECO_PTM:
            ax.set_title(self.testName + ": Route-specific P(survival), base ECO-PTM", fontsize=10)
            plt.savefig(os.path.join(self.mainOutputDir, self.testName + "_routeSpecificSurvivalBaseECO_PTM.pdf"))
        else:
            ax.set_title(self.testName + ": Route-specific P(survival), new ECO-PTM", fontsize=10)
            plt.savefig(os.path.join(self.mainOutputDir, self.testName + "_routeSpecificSurvival.pdf"))
        plt.close("all")  

    def scaleLogistic(self, x, k=10, x0=0.75):
        """Scale input according to a logistic function
    
        Keyword arguments:
        x (float) -- input value to be scaled
        k (float) -- logistic growth rate, or steepness of the curve
        x0 (float) -- the x value at the sigmoid's midpoint
        """    
        return x*(1/(1+np.exp(-k*(x-x0))))
    