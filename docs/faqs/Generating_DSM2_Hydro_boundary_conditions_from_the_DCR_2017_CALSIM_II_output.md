# Generating DSM2 Hydro boundary conditions from the DCR 2017 CALSIM II output

run DCR 2017 with CWF script.zip

I tried to generate the DSM2 Hydro boundary conditions from the DCR 2017
CALSIM II output (with a 2020 development level) .  But for this CALSIM
II output, we don’t have a corresponding script that can be used to
generate the boundary conditions. The closest script available is from
CALWATERFix for the previous version of CALSIM II output (with a 2005
development level).  So I used this script.  After some basic edits
(e.g., change directory, file name, etc.),  the script ran and generated
the boundary conditions.  However, because the script and the CALSIM II
output are not paired, using the old script for the new output could
introduce errors.  I compared the boundary conditions generated from DCR
2017  with those for CALWATERFix (generated with the same script but
from an older version of CALSIM output).  The patterns are match but
there are noticeable differences at some spots (e.g., Aug 24 1994 or
July 19 2001).  I haven't figured out what exactly caused the
differences, but could be the different assumptions used in the CALSIM
II studies.  

I have a read me file inside of the zip file to provide the instruction
about how to run the script.  The zip file is too big to upload so I
left two files out.  You can download the left out files here:

1.  this file should be in .\run DCR 2017 with CWF
    script\timeseries Planning_Tide_82years.zip
2.  this file should be in .\run DCR 2017 with CWF
    script\studies\planning\timeseries\CALSIM2020D09EDV\_\_2017DCR_OldANN_NewWSIDI-SWPDemand_x64_20171115.zip

  

## Attachments:

<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[2020D09EDV\_\_2017DCR_OldANN_NewWSIDI-SWPDemand_x64_20171115.zip](attachments/87228602/87228601.zip)
(application/zip)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[Planning_Tide_82years.zip](attachments/87228602/87228603.zip)
(application/zip)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" /> [run DCR
2017 with CWF script.zip](attachments/87228602/87228604.zip)
(application/zip)  
