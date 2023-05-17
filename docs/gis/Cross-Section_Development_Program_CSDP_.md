# Cross-Section Development Program (CSDP)

## Introduction

Bathymetry data is used by CSDP to draw cross-sections which are then
converted to DSM2-Hydro cross sectional input. Furthermore CSDP provides
the channel and cross section locations in GIS projection of NAD 27,
NGVD 29

CSDP was developed by Brad Tom in the 1990s, and has recently been
updated for use in the DSM2 GIS Reference Project.

### How to get started in using CSDP?

The CSDP Manual is available here Cross-Section Development Program
(CSDP)

A [hands on tutorial and presentation](CSDP_Tutorial) given by Brad Tom
in 2009 is a good reference resource.

In version 8.x the irregular xsection file format has changed. To change
this information to the new format run the script under
vista/scripts/dsm2/csdp_geom_converter.py with the location of the
directory as input

    vscript scripts/dsm2/csdp_geom_converter.py <dir_containing_csdp_calculated_xsections>

This will create a irregular_xsections_dsm2.inp which will contain all
the cross sections in that directory in the new 8.x format

CSDP will now create DSM2 geometry input in both the original multi-file
format used by older versions of DSM2, and the newer single file format,
so the above script is no longer needed.

An ArcGIS extension was developed as a modern replacement for CSDP by
Tom Heinzer. However this has not been available publicly yet and the
grid and cross sections are still being developed in this tool.

## CSDP conversion to ArcGIS

Using WKT (Well Known Text) format and QGIS (add delimited text layer)
capabilities, the information from CSDP files was converted to
shapefiles

node.cdl contained the information about the nodes in CSDP corresponding
to DSM2 node locations. 

mjtstrm_vec.cdo contained the outlines of levees and other features
which are now redundant given the availability of maps (raster based
tile layers) from google, open street etc.

05jul2000.cdn is the channel network which included the centerline of
channels and the cross-section created by looking at bathymetry data
(those are available separately as large files)

delta_2009Calib.cdn is the channel network for persumably the 2009
calibration.

The files are available on the shared drive
(<a href="file://cnrastore-bdo/Delta_Mod/Share/maps/"
rel="nofollow">\\cnrastore-bdo\Delta_Mod\Share\maps</a>) from both 2000
(CSDP_Converted_2000Calib.qgs) and 2009 (CSDP_Converted_2009Calib.qgs)
calibrations

For more information on DSM2 gridmaps and how they relate to CSDP files,
see [DSM2 Geo referenced grid](DSM2_Geo_referenced_grid).

-   Write up needed on CSDP and its successor the ArcGIS extension 
-   Write up needed using approach by Ines using ArcGIS and python
    scripts 

  

## Attachments:

<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[CSDP_vs_Channels_inp_Lengths.xlsx](attachments/87228641/87228640.xlsx)
(application/vnd.openxmlformats-officedocument.spreadsheetml.sheet)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[Clifton_court_2011.png](attachments/87228641/87228642.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[Clifton_court_1990.png](attachments/87228641/87228643.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[RSAC092_2011.png](attachments/87228641/87228644.png) (image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[RSAC092_1990.png](attachments/87228641/87228645.png) (image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[RSAN018_2011.png](attachments/87228641/87228646.png) (image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[RSAN018_1990.png](attachments/87228641/87228647.png) (image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[RSAN007_2011.png](attachments/87228641/87228648.png) (image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[RSAN007_1990.png](attachments/87228641/87228649.png) (image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[csdpmanual.pdf](attachments/87228641/87228650.pdf) (application/pdf)  
