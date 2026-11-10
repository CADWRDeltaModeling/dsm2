# CSDP Tutorial

Brad Tom (developer of CSDP) gave a
[presentation](../attachments/87228824/87228825.ppt) on CSDP in February 2009. A recording of this presentation is available as below.

[Download PDF](../attachments/87228824/87228823.pdf)

## Updates

In version 8.x the irregular xsection file format has changed. To change
this information to the new format run the script under
`vista/scripts/dsm2/csdp_geom_converter.py` with the location of the
directory as input:

```bash
vscript scripts/dsm2/csdp_geom_converter.py <dir_containing_csdp_calculated_xsections>
```

This will create a `irregular_xsections_dsm2.inp` which will contain all the cross sections in that directory in the new 8.x format.

- [CSDP Network File Format](CSDP_Network_File_Format.md)
- [CSDP Network Summary Report](CSDP_Network_Summary_Report.md)
- [Exporting Channel Lengths from CSDP Network file](Exporting_Channel_Lengths_from_CSDP_Network_file.md)
- [Exporting CSDP Information into GIS](Exporting_CSDP_Information_into_GIS.md)
- [Extracting Bathymetry Data From An Irregularly Shaped Region](Extracting_Bathymetry_Data_From_An_Irregularly_Shaped_Region.md)
- [Importing Digital Elevation Maps (DEMs) into CSDP](Importing_Digital_Elevation_Maps_DEMs_into_CSDP.md)
- [Merging multiple versions of network files](Merging_multiple_versions_of_network_files.md)

## Attachments:

- [csdpWebexClass.pdf](../attachments/87228824/87228823.pdf) (application/pdf)
- [csdpWebexClass.ppt](../attachments/87228824/87228825.ppt) (application/vnd.ms-powerpoint)
