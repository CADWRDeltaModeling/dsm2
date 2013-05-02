import sys
import os
import os.path
from gov.ca.dsm2.input.datum import ConvertXsectionsFromNGVD29ToNAVD88 as converter
if __name__=='__main__':
    echo_file=sys.argv[1]
    gis_file=sys.argv[2]
    print 'Convert gates from %s'%echo_file
    
    vertcon_in=os.path.join(os.path.dirname(echo_file),'vertcon.in')
    vertcon_out=os.path.join(os.path.dirname(echo_file),'vertcon.out')
    converter.writeOutGateDataForVerconConversion(echo_file, gis_file, vertcon_in)
    converter.writeVertconControlFile(vertcon_in,vertcon_out)
    
