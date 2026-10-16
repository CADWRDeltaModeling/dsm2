# CSDP Tutorial

Brad Tom (developer of CSDP) gave a
[presentation](/attachments/87228824/87228825.ppt) on CSDP in February 2009. A recording of this presentation is available as below.

[Download PDF](/attachments/87228824/87228823.pdf)

## Updates

In version 8.x the irregular xsection file format has changed. To change
this information to the new format run the script under
`vista/scripts/dsm2/csdp_geom_converter.py` with the location of the
directory as input:

```bash
vscript scripts/dsm2/csdp_geom_converter.py <dir_containing_csdp_calculated_xsections>
```

This will create a `irregular_xsections_dsm2.inp` which will contain all the cross sections in that directory in the new 8.x format.

- [CSDP Network File Format](/csdp/CSDP_Network_File_Format)
- [CSDP Network Summary Report](/csdp/CSDP_Network_Summary_Report)
- [Exporting Channel Lengths from CSDP Network file](/csdp/Exporting_Channel_Lengths_from_CSDP_Network_file)
- [Exporting CSDP Information into GIS](/csdp/Exporting_CSDP_Information_into_GIS)
- [Extracting Bathymetry Data From An Irregularly Shaped Region](/csdp/Extracting_Bathymetry_Data_From_An_Irregularly_Shaped_Region)
- [Importing Digital Elevation Maps (DEMs) into CSDP](/csdp/Importing_Digital_Elevation_Maps_DEMs_into_CSDP)
- [Merging multiple versions of network files](/csdp/Merging_multiple_versions_of_network_files)

## Attachments:

- [csdpWebexClass.pdf](/attachments/87228824/87228823.pdf) (application/pdf)
- [csdpWebexClass.ppt](/attachments/87228824/87228825.ppt) (application/vnd.ms-powerpoint)
