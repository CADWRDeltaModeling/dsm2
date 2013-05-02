from vdss import *
from vdisplay import *
from vista.set import Pathname
#calculations on the dicu pathnames
def do_sum(cpart,dicufile):
    g=opendss(dicufile)
    g=findparts(g,c=cpart)
    ts=None
    for ref in g:
        if ts==None:
            ts=ref.data
        else:
            ts+=ref.data
    path=Pathname.createPathname(ts.name)
    path=set_part(path,'ALL',Pathname.B_PART)
    ts.name=str(path)
#
def do_scale(cpart,scale,outfile):    
    g=opendss(dicufile)
    g=findparts(g,c=cpart)
    for ref in g:
        ds=ref.data*scale
        writedss(outfile,ds.name,ds)
#
if __name__=='__main__':
    dicufile=r'D:\models\DSM2v8.1.x\Historical_MiniCalibration_811_MTZ_ts_corrected\timeseries\dicu_201004.dss'
    outfile=r'D:\models\DSM2v8.1.x\Historical_MiniCalibration_811_MTZ_ts_corrected\timeseries\dicu_201004_minus20.dss'
    cparts=['DIV-FLOW','DRAIN-FLOW','SEEP-FLOW']
    for cpart in cparts:
        ts=do_scale(cpart,0.8,outfile)
