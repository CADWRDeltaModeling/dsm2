"""This script changes references to the informix database to 
    references to MS Access
"""

import sys


filename = sys.argv[1]
if not filename.endswith("move_to_access"):
    filename = filename + ".move_to_access"

print filename

outfilename=filename.replace(".move_to_access", "")
print outfilename

f=open(filename,'r')
outfile=open(outfilename,'w')

for line in f.readlines():
    linereplace=line.replace("dsm2input","dsm2input_access")
    linereplace=linereplace.replace("dsm2input_access_access","dsm2input_access")
    linereplace=linereplace.replace("../../timeseries","${DSM2_HOME}/timeseries")
    if line != linereplace: print "Changing %s to %s" % (line,linereplace)
    outfile.write(linereplace)
    
    
f.close()
outfile.close()
sys.exit()
