# -*- coding: utf-8 -*-
"""
Verification of ECO-PTM with Java I/O code

Doug Jackson
doug@QEDAconsulting.com
"""
import os
import xarray as xr
import pandas as pd
import matplotlib.pyplot as plt
from datetime import datetime as dt
####################################################################################################
# Constants
####################################################################################################
workingDir = "/Users/djackson/Documents/QEDA/DWR/programs/dsm2_master/dsm2/dsm2/src/ptm/development/verification"


# Map from new to old flux indices
iMap = {0:11, 1:2, 2:14, 3:13, 4:4, 5:7, 6:6, 7:3, 8:9, 9:5, 10:1, 11:11, 12:6, 13:12, 14:15, 15:19, 16:18, 17:20, 18:17, 19:8}

figWidth = 8
figHeight = 6

legendFontsize = 12
tickFontsize = 10
labelFontsize = 12
####################################################################################################
# Run
####################################################################################################
os.chdir(workingDir)

for runType in ["filtered", "not_filtered"]:
    outputDir = os.path.join(workingDir, "output", runType)
    os.makedirs(outputDir, exist_ok=True)
    
    # Load new model outputs 
    newOutputFile = os.path.join(workingDir, "newModel", runType, "ptm_out.ncd")

    ds = xr.open_dataset(newOutputFile)
    
    nodeFlux = ds["nodeFlux"].to_pandas()
    nodeFlux.columns = [c.decode("utf8") for c in nodeFlux.columns]
    nodeFlux.index = [i.decode("utf8") for i in nodeFlux.index]
    
    nodes = nodeFlux.columns
    nodeFlux["datetime"] = [dt.strptime(d, "%m/%d/%Y %H:%M:%S") for d in nodeFlux.index]
    
    groupFlux = ds["groupFlux"].to_pandas()
    groupFlux.columns = [c.decode("utf8") for c in groupFlux.columns]
    groupFlux.index = [i.decode("utf8") for i in groupFlux.index]
    
    groups = groupFlux.columns
    groupFlux["datetime"] = [dt.strptime(d, "%m/%d/%Y %H:%M:%S") for d in groupFlux.index]
    
    ds.close()
    
    nodeFlux.to_csv(os.path.join(outputDir, "newNodeFlux.csv"), index=False)
    groupFlux.to_csv(os.path.join(outputDir, "newGroupFlux.csv"), index=False)
    
    # Load old model outputs
    oldNodeFluxFile = os.path.join(workingDir, "oldModel", runType, "ptm_out_FLUX.csv")
    oldNodeFlux = pd.read_csv(oldNodeFluxFile)
    oldNodeFlux["datetime"] = [dt.strptime(d, "%d%b%Y %H%M") for d in oldNodeFlux["datetime"]]
    
    oldGroupFluxFile = os.path.join(workingDir, "oldModel", runType, "ptm_out_PTM_GROUP.csv")
    oldGroupFlux = pd.read_csv(oldGroupFluxFile)
    oldGroupFlux["datetime"] = [dt.strptime(d, "%d%b%Y %H%M") for d in oldGroupFlux["datetime"]]
    
    # Generate comparison plots
    for node in nodes:
        fig, ax = plt.subplots(figsize=[figWidth, figHeight])
        ax.plot(nodeFlux["datetime"], nodeFlux[node], label="Java I/O (new model)")
        ax.plot(oldNodeFlux["datetime"], oldNodeFlux[node], label="Fortran I/O (old model)")
        ax.legend(fontsize=legendFontsize)
        ax.tick_params(axis="both", which="both", labelsize=tickFontsize)
        ax.set_ylabel("particle flux (%)", fontsize=labelFontsize)
        ax.set_title(f"Flux: {node}")
        ax.tick_params(axis="x", labelrotation=45)
        plt.tight_layout()
        plt.savefig(os.path.join(outputDir, f"nodeFlux_{node}.png"))
        plt.close("all")
        
    for group in groups:
        fig, ax = plt.subplots(figsize=[figWidth, figHeight])
        ax.plot(groupFlux["datetime"], groupFlux[group], label="Java I/O (new model)")
        ax.plot(oldGroupFlux["datetime"], oldGroupFlux[group], label="Fortran I/O (old model)")
        ax.legend(fontsize=legendFontsize)
        ax.tick_params(axis="both", which="both", labelsize=tickFontsize)
        ax.set_ylabel("particle flux (%)", fontsize=labelFontsize)
        ax.set_title(f"Flux: {group}")
        ax.tick_params(axis="x", labelrotation=45)
        plt.tight_layout()
        plt.savefig(os.path.join(outputDir, f"groupFlux_{group}.png"))
        plt.close("all")

        
                