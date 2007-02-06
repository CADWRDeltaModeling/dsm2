""" Transfer and transform data from one source to another
    This module provides functionality for opening one data source, grabbing data that matches a pattern, transforming it or clipping it in time and then exporting it to another data source.
"""
import sys
from vdss import *
from vmath import *
from vista.set import DataReference,Pathname

def transfer(infile,outfile,select,window,transform,interval):
    f=opendss(infile)
    g=findpath(f,select)
    select=select.upper()
    
    if not outfile:
        raise "Output file name is None"
    
    if not g or len(g) == 0:
        raise "No matches for selection [%s] found in file %s" % (select,infile)

    
    for ref in g:
        if window:
            r = DataReference.create(ref,window)
            if not r:
                print "No data in window for reference: %s" % ref
                continue
        else:
            r=ref
        
        d = r.getData()

        if not d or len(d)==0:
            raise "Data retrieval failed for %s " % ref2
        if transform:
            transform=transform.lower()
            if transform == "period_max":
                #if d.getTimeInterval() >= interval: break
                d=per_max(d,interval.toString())
            elif transform == "period_min":
                d=per_min(d,interval.toString())
            elif transform == "period_ave":
                #if d.getTimeInterval() >= interval: break
                d=per_avg(d,interval.toString())
            elif transform == "tidal_ave":
                #if not d.getTimeInterval().toS in ["1D
                d=tidal_avg(d)
            elif transform == "godin":
                #if d.getTimeInterval() >= timeinterval("1DAY"): break
                d=godin(d)
            else:
                raise "Transform %s not recognized" %transform
        
        p=Pathname.createPathname(d.getName())
        if transform:
            old_c=p.getPart(Pathname.C_PART)
            new_c=old_c+"_"+transform.upper()
            p.setPart(Pathname.C_PART,new_c)
        path=p.toString()
        writedss(outfile,path,d)

    #
    return

def usage():
    print """
    transfer.py [options] infile

    Args:
    infile:   name of file to use as input

    Options:
    --out        name of outputfile
                 default out.dss
    --selection  regular expression identifying pattern to match
                 default /////// matches all
    --transform  name of transform:
                 [period_max, period_min, period_ave, tidal_ave, godin]
                 default None copies directly
    --interval   time interval to use in transform if applicable (e.g. 1DAY for daily ave)
    --window     sub time window to copy, default is whole series

    Example:
    A batch script might have a line:
    
    >call vscript transfer.py --out=out.dss --transform=period_min --interval=1DAY
     --window='01JAN2000 0000 - 31JAN2000 0000' --selection=///STAGE//1HOUR// in.dss

    """

             
    

def main():
    import getopt,sys
    from vtimeseries import timeinterval,timewindow
    try:
        opts,args = getopt.getopt(sys.argv[1:], "h", ["help","out=", "selection=","window=","transform=","interval="])
    except getopt.GetoptError,e:
        # print help information and exit:
        print "Argument error %s " %e
        sys.exit(2)
    if args and len(args) > 0:
        infile=args[0]
    else:
        infile = None
    outfile="out.dss"
    select="///////"
    window=None
    transform=None
    interval=None
    for o, a in opts:
        if o in ("-h", "--help"):
            usage()
            return
        if o in ("-o", "--out"):
            outfile=a
        if o in ("-s", "--selection"):
            select=a
        if o in ("-w", "--window"):
            window=timewindow(a)
        if o in ("-t", "--transform"):
            transform=a
        if o in ("-i", "--interval"):
            interval = timeinterval(a)
    if not infile:
        raise "No input file specified"
    transfer(infile,outfile,select,  \
             window,transform,interval)
    #
#

if __name__=='__main__':
    main()
    sys.exit()

