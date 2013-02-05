import sys
print sys.argv
from vista.app import PTMDualAnimator
#
if len(sys.argv) < 2:
    print 'No config file specified!'
    sys.exit(-1)
PTMDualAnimator(sys.argv[1])
