# Cross-Section Development Program (CSDP)

## Introduction

Bathymetry data is used by CSDP to draw cross-sections, which are then converted to DSM2-Hydro cross-sectional input. Furthermore, CSDP provides the channel and cross-section locations in GIS projection of NAD 27, NGVD 29.

CSDP was developed by Brad Tom in the 1990s and has recently been updated for use in the DSM2 GIS Reference Project.

### Getting Started

The [CSDP Manual](attachments/87228641/87228650.pdf) is available for reference. A [hands-on tutorial and presentation](CSDP_Tutorial) given by Brad Tom in 2009 is also a good resource.

In version 8.x, the irregular cross-section file format has changed. To update this information to the new format, run the following script:

```bash
vscript scripts/dsm2/csdp_geom_converter.py <dir_containing_csdp_calculated_xsections>
```

This will create an `irregular_xsections_dsm2.inp` file containing all the cross-sections in the new 8.x format.

CSDP now creates DSM2 geometry input in both the original multi-file format and the newer single-file format, so the above script is no longer needed.

An ArcGIS extension was developed as a modern replacement for CSDP by Tom Heinzer. However, this has not been publicly released yet, and the grid and cross-sections are still being developed in this tool.

## Conversion to ArcGIS

Using WKT (Well-Known Text) format and QGIS capabilities, information from CSDP files was converted to shapefiles. For more details, see [DSM2 Geo-referenced Grid](DSM2_Geo_referenced_grid).

## Attachments

- [CSDP_vs_Channels_inp_Lengths.xlsx](attachments/87228641/87228640.xlsx)
- [Clifton_court_2011.png](attachments/87228641/87228642.png)
- [Clifton_court_1990.png](attachments/87228641/87228643.png)
- [RSAC092_2011.png](attachments/87228641/87228644.png)
- [RSAC092_1990.png](attachments/87228641/87228645.png)
- [RSAN018_2011.png](attachments/87228641/87228646.png)
- [RSAN018_1990.png](attachments/87228641/87228647.png)
- [RSAN007_2011.png](attachments/87228641/87228648.png)
- [RSAN007_1990.png](attachments/87228641/87228649.png)
