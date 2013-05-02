from vtidefile import opentidefile
from vdss import writedss, opendss,findparts,set_part
from vutils import timewindow,timeinterval
import sys
import string
from vista.set import DataReference, Pathname,TimeSeriesMath
def get_matching(dss,pattern):
    matches = dss.getCatalogedPathnames(pattern)
    if matches==None:
        print 'No matches in %s of %s'%(dss,pattern)
    else:
        paths = jarray.zeros(matches.size(),String)
        matches.toArray(paths)
        Arrays.sort(paths)
        condensed = dss.getDataManager().dataManager().getCondensedCatalog(paths,True)
        data=[]
        for match in matches:
            data.append(dss.get(match))
        return data
    return None
def net_dicu(dicufile,nodes):
    dicu = opendss(dicufile)
    sum=None
    for n in nodes:
        matches=findparts(dicu,b=str(n))
        if matches==None:
            print 'No matching B part of %s'%n
        else:
            for d in matches:
                if sum==None:
                    sum=d
                else:
                    cpart = d.getPathname().getPart(Pathname.C_PART)
                    if cpart == 'DRAIN-FLOW':
                        sum=sum - d
                    else:
                        sum=sum + d
    return sum
def doall(dicufile,bndryfile):
    sum=net_dicu(dicufile,[334,333,332,331,330])
    pathname=sum.getPathname().toString()
    pathname=set_part(pathname,'NET_DICU_NORTH_OF_FREEPORT',Pathname.B_PART)
    pathname=set_part(pathname,'FLOW',Pathname.C_PART)
    writedss(dicufile,pathname,sum.getData())
    #sum_daily=TimeSeriesMath.sample(sum.data,timeinterval("1DAY"),TimeSeriesMath.LAST_VAL)
    #g=opendss(bndryfile)
    #sac=findparts(g,b='RSAC155',c='FLOW',f='DWR-DMS-201004')
    #sac_adj=sac+sum_daily
    #pathname=sac.getPathname()
    #pathname=set_part(pathname,'RSAC155_DICU_ADJ',Pathname.B_PART)
    #writedss(bndryfile,pathname,sac_adj.data)
if __name__=='__main__':
    doall('z:/calibration/run_template_sac_bndry/dicu_201004.dss','z:/calibration/run_template_sac_bndry/hist19902010.dss')
        