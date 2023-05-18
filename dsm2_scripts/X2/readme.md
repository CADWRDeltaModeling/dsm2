# Background
X2 compliance rules within CALSIM use ANNs for calculating the X2. The ANN is trained using DSM2 output.

The script that calculates X2 for DSM2 DSS output is in this github repository: https://github.com/CADWRDeltaModeling/dsm2/tree/master/dsm2_scripts/X2/

The X2 location is calculated from the EC along channels:
  In the Sacramento River, from Martinez to Rio Vista
  In the San Joaquin River, from Martinez to the North Fork of the Mokelumne River, just upstream of San Andreas Landing.

# Installation
The script x2_daily.py is run using vscript, which is an extension of python created by Nicky Sandhu for the Vista application, which was included with earlier versions of DSM2.
Here is a link to one of the earlier versions of DSM2 which includes Vista: https://data.cnra.ca.gov/dataset/dsm2/resource/8828c95e-7ea0-406d-bf01-6e7d800d94f8

# DSM2 study requirements
You will need a Qual EC output DSS file, created by running Qual with the output specification file included here: output_channel_ec_for_x2.inp

# Usage
(path to vscript)\vscript x2_daily_v3.py (path to DSS EC output file) (path to output DSS file) (output DSS F part) (time window) ("sac" or "sjr")

# Example
d:\delta\dsm2_V8.2\vista\bin\vscript x2_daily.py ..\ex_2020\output\DCP_EX_2020_QUAL.dss x2_daily.dss Base_Study "01APR2001 0000 - 01OCT2015 0000" sac
output:

# Results
X2 is calculated as a time series, and the results are written to a DSS file, using the information provided to the script. The pathname for the time series will indicate which River was used (Sacramento or San Joaquin) in the b part, and the name of the study in the f part.
