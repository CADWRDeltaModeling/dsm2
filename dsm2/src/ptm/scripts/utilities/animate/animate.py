# -*- coding: utf-8 -*-
"""Script to plot particle and vFish positions
"""
__author__ = "Doug Jackson, QEDA Consulting, LLC"
__email__ = "doug@qedaconsulting.com"
import os
import numpy as np
import pandas as pd
import hvplot.pandas
import holoviews as hv
hv.extension("bokeh")
import geopandas
import panel as pn
import xarray as xr
from datetime import datetime as dt

workingDir = os.path.dirname(os.path.realpath(__file__))

if __name__=="__main__":
    import argparse

    # Read in command line arguments
    parser = argparse.ArgumentParser(description="Script to plot particle and vFish positions.")
    parser.add_argument("--animFile1", action="store", dest="animFile1", required=True)
    parser.add_argument("--animFile2", action="store", dest="animFile2", required=False)
    parser.add_argument("--fluxFile1", action="store", dest="fluxFile1", required=False)
    parser.add_argument("--fluxFile2", action="store", dest="fluxFile2", required=False)
    parser.add_argument("--shapeFile", action="store", dest="shapeFile", required=False)
    parser.add_argument("--titleFont", action="store", dest="titleFont", required=False)
    parser.add_argument("--figWidth", action="store", dest="figWidth", required=False)
    parser.add_argument("--figHeight", action="store", dest="figHeight", required=False)
    args = parser.parse_args()

    animFile1 = args.animFile1
    animFile2 = args.animFile2
    fluxFile1 = args.fluxFile1
    fluxFile2 = args.fluxFile2

    if args.shapeFile is not None:
        DSM2flowlineShapefile = args.shapeFile
    else:
        DSM2flowlineShapefile = os.path.join(workingDir, "shapefile", "DSM2_Flowline_Segments.shp")
    print(f"DSM2 flowline shapefile: {DSM2flowlineShapefile}")
    
    if args.titleFont is not None:
        titleFont = int(args.titleFont)
    else:
        titleFont = 10

    if args.figWidth is not None:
        figWidth = int(args.figWidth)
    else:
        figWidth = 800
    
    if args.figHeight is not None:
        figHeight = int(args.figHeight)
    else:
        figHeight = 600
####################################################################################################
# Functions
####################################################################################################
def createAnimDF(animFile):
    ds = xr.open_dataset(animFile)
    anim = ds["anim"].to_pandas().reset_index()
    anim.columns = ["particleNum", "extChannel", "normXdist"]

    # Obtain geocoordinates of channels
    DSM2chans = geopandas.read_file(DSM2flowlineShapefile).to_crs(epsg=3857)
    chanList = []
    upNodeXlist = []
    upNodeYlist = []
    downNodeXlist = []
    downNodeYlist = []
    for r in DSM2chans.iterrows():
        chan = r[1]
        upNode = chan.geometry.interpolate(1, normalized=True)
        downNode = chan.geometry.interpolate(0, normalized=True)
        
        chanList.append(chan.id)
        upNodeXlist.append(upNode.coords.xy[0][0])
        upNodeYlist.append(upNode.coords.xy[1][0])
        downNodeXlist.append(downNode.coords.xy[0][0])
        downNodeYlist.append(downNode.coords.xy[1][0])
    chans = pd.DataFrame({"extChannel":chanList, "upNodeX":upNodeXlist, "upNodeY":upNodeYlist,
                          "downNodeX":downNodeXlist, "downNodeY":downNodeYlist})

    b = DSM2chans.to_crs(epsg=3857).geometry.bounds
    extents = (b.minx.min(), b.miny.min(), b.maxx.max(), b.maxy.max())

    # Specify coordinates for channel number placeholder
    chans = pd.concat([pd.DataFrame([{"extChannel":-1, "upNodeX":extents[0], "upNodeY":extents[1],
                                             "downNodeX":extents[0], "downNodeY":extents[1]}]), 
                       chans])

    # Merge coordinates with records
    anim = pd.merge(anim, chans, how="left", on="extChannel")

    # Load modelDate and modelTime
    modelDate = ds["modelDate"].to_pandas().reset_index()
    modelTime = ds["modelTime"].to_pandas().reset_index()
    
    modelDate.columns = ["timestep", "modelDate"]
    modelTime.columns = ["timestep", "modelTime"]

    modelDate = [m.decode("utf8") for m in modelDate["modelDate"]]
    modelTime = np.array([m.decode("utf8") for m in modelTime["modelTime"]])
    modelTime[np.where(modelTime=="2400")] = "2359"

    modelDatetime = [dt.strptime(f"{d} {t}", "%d%b%Y %H%M") for (d, t) in zip(modelDate, modelTime)]
    datetimeIndex = list(np.arange(0, len(modelDatetime)))
    numRecords = len(modelDatetime)

    # Repeat modelDatetime
    numParticles = len(anim["particleNum"].unique())
    anim["modelDatetime"] = np.repeat(modelDatetime, numParticles)
    anim["datetimeIndex"] = np.repeat(datetimeIndex, numParticles)

    # Calculate interpolated location
    anim["easting"] = anim["upNodeX"] + (anim["normXdist"]/100)*(anim["downNodeX"] - anim["upNodeX"])
    anim["northing"] = anim["upNodeY"] + (anim["normXdist"]/100)*(anim["downNodeY"] - anim["upNodeY"])
    
    print(f"Processing {animFile}")
    print(f"PTM animation file has {numRecords} records")
    print(f"Animation start at {modelDatetime[0]}")
    print(f"Animation ends at {modelDatetime[-1]}")
    print(f"There are {numParticles} particles in this animation")
    
    return anim, extents, numRecords

def createFluxDF(fluxFile):
    """Create data frame of flux outputs

    Keyword arguments:
    fluxFile (str) -- full path to the flux output file
    """
    ds = xr.open_dataset(fluxFile)

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

    flux = pd.merge(nodeFlux, groupFlux, on="datetime", how="outer")
    flux["datetime"] = pd.to_datetime(flux["datetime"])

    return flux
####################################################################################################
# Run
####################################################################################################
print("="*100)
anim1, extents1, numRecords1 = createAnimDF(animFile1)
if animFile2 is not None:
    print("-"*100)
    anim2, extents2, numRecords2 = createAnimDF(animFile2)
else:
    numRecords2 = 0
print("="*100)

fluxLocs = set()
if fluxFile1 is not None:
    flux1 = createFluxDF(fluxFile1)
    thisLocs = list(flux1.columns)
    thisLocs.remove("datetime")
    fluxLocs = fluxLocs | set(thisLocs)
    print(f"fluxLocs: {fluxLocs}")
if fluxFile2 is not None:
    flux2 = createFluxDF(fluxFile2)
    thisLocs = list(flux2.columns)
    thisLocs.remove("datetime")
    fluxLocs = fluxLocs | set(thisLocs)
fluxLocs = list(fluxLocs)
fluxLocs.sort()

# Tiles always in pseudo mercator epsg=3857
plotMap = hv.element.tiles.CartoLight().opts(width=figWidth, height=figHeight)
plotMap.extents = extents1
 
pn.config.throttled = True

# Spatial plots
datetimeIndexSlider = pn.widgets.IntSlider(value=0, start=0, end=(np.max([numRecords1, numRecords2])-1), name="datetime index")

rdf1 = pn.rx(anim1)
p1 = plotMap*rdf1[rdf1["datetimeIndex"]==datetimeIndexSlider].hvplot(x="easting", 
                                                                   y="northing", 
                                                                   kind="scatter").opts(title=animFile1,
                                                                                        fontsize={"title": titleFont}) 

col1 = pn.Column(pn.panel(p1, widget_location="top"))
col2 = pn.Column()
if animFile2 is not None:
    rdf2 = pn.rx(anim2)

    p2 = plotMap*rdf2[rdf2["datetimeIndex"]==datetimeIndexSlider].hvplot(x="easting", 
                                                                        y="northing", 
                                                                        kind="scatter").opts(title=animFile2,
                                                                                            fontsize={"title": titleFont})
    col2.append(pn.panel(p2, widget_location="top"))

# Flux plots
selectFluxLoc = pn.widgets.Select(name="loc", options=fluxLocs)

fluxPane = pn.Row()
if fluxFile1 is not None:
    flux1long = pd.melt(flux1, id_vars="datetime", var_name="loc", value_name="flux")
    flux1rx = pn.rx(flux1long)
    flux1plot = flux1rx[flux1rx["loc"]==selectFluxLoc].hvplot.line(x="datetime", y="flux")
    col1.append(pn.panel(flux1plot, widget_location="top"))
if fluxFile2 is not None:
    flux2long = pd.melt(flux2, id_vars="datetime", var_name="loc", value_name="flux")
    flux2rx = pn.rx(flux2long)
    flux2plot = flux2rx[flux2rx["loc"]==selectFluxLoc].hvplot.line(x="datetime", y="flux")
    col2.append(pn.panel(flux2plot, widget_location="top"))

animPane = pn.Row(col1, col2)

server = animPane.show()

