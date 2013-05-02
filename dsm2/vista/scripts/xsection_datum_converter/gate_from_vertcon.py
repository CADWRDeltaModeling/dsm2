import sys
import os
import os.path
from gov.ca.dsm2.input.datum import ConvertXsectionsFromNGVD29ToNAVD88 as converter
if __name__=='__main__':
    echo_file=sys.argv[1]
    gis_file=sys.argv[2]
    print 'Convert gates from %s'%'vertcon.out'
    converter.writeOutGatesAfterVertconConversion(echo_file, gis_file, 'vertcon.out')