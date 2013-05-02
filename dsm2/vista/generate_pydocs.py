import os, sys
sys.executable='jython'
import pydoc
import vdss, vmath,irreg,cdec,annutils, interpolate,vchecker,vdisplay,vexpr,vmath,vtidefile,vtimeseries,vutils
if __name__=='__main__':
    print 'Current working directory: ', os.getcwd()
    os.chdir('d:\\dev\\wk-vista\\vista\\doc\\pydocs')
    print 'Now working in directory: ', os.getcwd()
    modules = [vdss,vmath,irreg,cdec,annutils,interpolate, vchecker, vdisplay, vexpr, vmath, vtidefile, vtimeseries, vutils]
    for m in modules:
        pydoc.writedoc(m)
#
