# DSM2 Geo referenced grid

## Background

DSM2 input specifies geographically based information such as channel
lengths and cross section distances from the upstream node. However the
geographically referenced node locations or channel outlines are not
directly needed for hydrodynamic calculations. 

In addition to this the cross sections in DSM2 are based on bathymetry
data that is used to generate elevation to cross-sectional property
information. 

Even though this information is not needed directly it is very important
to keep the geographically referenced information in sync with the input
parameters such as channel length and cross section locations in DSM2
input

There have been different areas for which the grid was developed over
time, the Sacramento-San Joaquin Delta, the San Joaquin River Extension
and the Aqueduct grid. The one that is most commonly referred to as DSM2
grid is the Sacramento-San Joaquin Delta

## Sacramento - San Joaquin Delta grid

The original DSM2 grid was based on hand calculated distances based on
USGS Quad maps of the Delta (circa 1990). These were done on paper maps
and the original information has been lost though.

A <a href="attachments/87228652/87228654.pdf"
data-linked-resource-id="87228654" data-linked-resource-version="1"
data-linked-resource-type="attachment"
data-linked-resource-default-alias="DSM2_Grid2.0.pdf"
data-nice-type="PDF Document"
data-linked-resource-content-type="application/pdf"
data-linked-resource-container-id="87228652"
data-linked-resource-container-version="1">pdf version of this grid</a>
based on presumably this information is often found in circulation. The
grid map contained in this pdf was originally created using AutoCAD.
However the node locations in this pdf version are clearly not in the
stream at many times and certainly not geo-referenced to any projection
system. Nodes and channels were not always placed very carefully,
presumably because the map was primarily used to identify approximate
locations of nodes, channels, and stations, and channel connectivity.

In the late 1990s or early 2000s, a paper copy of the grid was placed on
a digitizing tablet, and nodes were digitized by clicking on each one
(Amy Little might have done this). The result was a file containing
approximate UTM coordinates of each node. This file was used by the DSM2
Cross-Section Development Program (CSDP) to create representations of
DSM2 channels and cross-sections. 

<a href="http://msb-confluence/pages/viewpage.action?pageId=15368194"
rel="nofollow">CSDP</a> was developed by Brad Tom and Nicky Sandhu based
upon specifications written by Ralph Finch in 1998 to derive the cross
sections from bathymetry data, which at the time were mostly single beam
soundings of depth that were available over many decades in the Delta.
This tool is the basis of the current grid in 2000 and the recalibrated
grid in 2009. As a by product of this effort the node locations and
channel outlines were stored in UTM coordinates. Originally, these were
not used directly to derive channel lengths, but they were used
indirectly in determining the distance of a cross-section from the
upstream node of a channel. The DSM2 GIS Reference project, which began
in 2018, will used CSDP centerlines to determine channel lengths.

<a href="http://msb-confluence/pages/viewpage.action?pageId=15368194"
rel="nofollow">CSDP</a> was developed pre ArcGIS and with ArcGIS now
being fairly standards in DWR there is a need to provide this
geographical information in ArcGIS format. In recent years, Jane
Schafer-Kramer created an <a
href="http://msb-confluence/download/attachments/12648674/Delta_Stations_with_DSM2_Grid.mpk?version=1&amp;modificationDate=1506539043973&amp;api=v2"
style="letter-spacing: 0.0px;" rel="nofollow">ArcGIS version of this
grid</a>. Jane developed, under Ralph Finch's guidance, an ArcGIS
referenced grid by manually putting nodes at the closest location based
on the pdf version of the grid. Again the channel lengths from these
would not match either CSDP or the original grid as it is an independent
manual effort. Furthermore there would be a mismatch to the location of
the cross-section positions. 

In 2012, Tom Heinzer was contracted to develop an ArcGIS based extension
to allow a user to develop cross-sections from DEM which in turn is
based on interpolations of depth sounding data. This again is a work in
progress and cannot import the current cross-sectional profiles
available in CSDP.

In 2017, [CSDP grid data for the 2009 calibration was imported into
ArcGIS](Cross-Section_Development_Program_CSDP_) and along with it the
channel outlines and node locations. The channel outlines in ArcGIS were
used to calculate lengths for the channels and these were then compared
to the current grid.  There were many mismatches discovered and these
should be addressed in future efforts

## 2009 Grid

The 2009 Grid is used for DSM2 v8.2. It is similar to the pdf gridmap,
but it includes some upper sacramento river changes. 

The node locations and the associated channel network lengths do have a
match with the 2000 calibration files (spot checked). However, the 2009
CH2MHill [mini calibration](Mini_Calibration_2009_) adjusted node
positions, channel lengths, and cross-sections for channels 412-418. 
The changes made in these channels were incorporated into DSM2, and are
included in DSM2 versions as recent as v8.2.0, which is the current
release as of 10/2019. However, we did not get any CSDP or GIS data from
CH2MHill.  Node locations were reverse engineered using the mini
calibration lengths, starting with the common node position from channel
412. The overall sum of the length (reach 412-418) was unchanged and
this assumption allows for a reasonable reverse engineering effort. 

This reverse engineered effort is available on the shared drive as
shapefiles \\cnrastore-bdo\Delta_Mod\Share\maps\csdp_2009_calib_converted\CSDP_Channels_Adjusted_MiniCalib.shp
(channels)
and \\cnrastore-bdo\Delta_Mod\Share\maps\csdp_2009_calib_converted\CSDP_Nodes_Adjusted_MiniCalib.shp. The
Nodes shapefile is missing some files, and cannot be loaded into ArcGIS.
It was loaded into OpenJUMP, and exported
to \\cnrastore-bdo\Delta_Mod\Share\maps\csdp_2009_calib_converted\CSDP_Nodes_Adjusted_MiniCalib_Recovered.shp.
These files are also available
in \\nasbdo\Modeling_Data\DSM2_GIS_CSDP_gridmaps\GISGridmapV8.2.

These layers are the closest approximation to the grid used for DSM2
v8.2. 

There is a large discrepancy in the channel length for channel 422
between cross channel and northern head of Georgiana slough. CSDP and
ArcGIS calculations put it at 3300 feet while in DSM2 input files it is
5300 feet. This is not an isolated incidence, there are many others as
documented in this <a href="attachments/87228641/87228640.xlsx"
data-linked-resource-id="87228640" data-linked-resource-version="1"
data-linked-resource-type="attachment"
data-linked-resource-default-alias="CSDP_vs_Channels_inp_Lengths.xlsx"
data-nice-type="Excel Spreadsheet"
data-linked-resource-content-type="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
data-linked-resource-container-id="87228641"
data-linked-resource-container-version="1">CSDP_vs_Channels_inp_Lengths.xlsx</a>

## DSM2 v8.1 and v8.2 grid

For version 8.1 and 8.2, use this grid for referencing DSM2 elements
approximately. The channels and nodes layers are incomplete, not very
accurate, and contain errors.

Shapefiles are available in
\\nasbdo\Modeling_Data\DSM2_GIS_CSDP_gridmaps\GISGridmapV8.2\\

## DSM2 v8.3 grid

The 2019 grid is used for DSM2 v8.3, which is under development, and
will be the result of the DSM2 GIS Reference Project.

Three shapefiles (located here:
\\nasbdo\Modeling_Data\DSM2_GIS_CSDP_gridmaps\GISGridmapV8.3) each have
been created from the CSDP network (channel centerlines) and landmark
(nodes) data for both the 2009 calibration (DSM2 V8.2) and the 2019
calibration (DSM2 V8.3). The shapefiles were created by exporting
network and landmark data from the CSDP to WKT files and importing the
results into QGIS, then saving to shapefiles. This is intended to be a
first step toward creating a
<a href="http://msb-confluence/display/DM/GIS+Reference"
rel="nofollow">georeferenced grid map</a>. Shapefiles are available in
\\nasbdo\Modeling_Data\DSM2_GIS_CSDP_gridmaps\GISGridmapV8.3\\

1.  **dsm2_channels_centerlines** contains the channel centerlines as
    created in the CSDP. Many channels have endpoints that are  
    not located at the node; this was done to improve the accuracy of
    the DSM2 channel volume. Also, many centerlines do not follow the
    actual channel centerline perfectly.
2.  **dsm2_channels_straightlines** contains straight lines connecting
    the two endpoints of each CSDP centerline.
3.  **dsm2_nodes** contains the CSDP landmark data. The node locations
    were previously not very accurate; they have now been corrected.
4.  **dsm2_boundary_flow_nodes** contains points placed at the locations
    of nodes where boundary flows are applied.
5.  **dsm2_boundary_stage_node** contains a point placed at the location
    of the node where the boundary stage is applied
6.  **dsm2_gates** contains points placed at the approximate location of
    the channel centerline near each gate. In DSM2, gates are located at
    the ends of channels. The points in this layer is intended to
    represent the approximate location in DSM2, and not necessarily the
    physical location of the gate.

## Future Directions

We need a georeferenced gridmap. It should have the following features:

1.  Display nodes as circles with numbers inside.
2.  Display straightline channels with numbers, and an arrow indicating
    positive flow direction.
3.  Display channels derived from CSDP centerlines, with numbers, and an
    arrow indicating positive flow direction. 
4.  straightline channels and CSDP centerline channels should be
    different colors.
5.  Useful for printing on a plotter.
6.  Easy to modify when CSDP node locations or channels change. 
7.  Good contrast with background, so we can easily determine
    connectivity and read all the numbers. 

-   Michael Mehrdadi is working on an ArcGIS gridmap using the
    shapefiles for the 2019 grid. 
-   Hans Kim is working on a Google Earth gridmap. This will likely be a
    useful training tool, and may have other uses. 
    -    The current version (as of 10/31/2019) of the gridmap is found
        here: <a href="attachments/87228639/87228638.kml"
        data-linked-resource-id="87228638" data-linked-resource-version="1"
        data-linked-resource-type="attachment"
        data-linked-resource-default-alias="DSM2_Grid_191029.kml"
        data-linked-resource-content-type="application/octet-stream"
        data-linked-resource-container-id="87228639"
        data-linked-resource-container-version="1">DSM2_Grid_191029.kml</a>.
        It can be opened with Google Earth or imported into Google Map.
    -    Updates will be made as new shapefiles become available.

  

## Attachments:

<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[DSM2_Grid_191029.kml](attachments/87228639/87228638.kml)
(application/octet-stream)  
