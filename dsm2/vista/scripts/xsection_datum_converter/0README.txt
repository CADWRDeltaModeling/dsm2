The script here is used in converting the DSM2 cross-sections input from NGVD 29 to NAVD88.

There are a few manual steps to the conversion

First get the DSM2 hydro echo file that contains the xsections (generated used hydro -e <<main.inp>> on your setup). 
Next get the appropriate gis.inp file. 
We provide the gis_2009_calibration.inp for the 2009 calibration grid (others can be obtained from dsm2 grid map application)

1.) Run xsection_to_vertcon script on the hydro echo and gis input file. 
2.) This will create a xsections_for_conv.in file and vertcon.ctrl in the same directory as where this script is run
3.) Run vertcon < vertcon.ctrl which will create vertcon.out file
4.) Run xsection_from_vertcon script that will take the same hydro echo, gis input along with the vertcon.out.
This will produce a hydro echo file with the modified xsections