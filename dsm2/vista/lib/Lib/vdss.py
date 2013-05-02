import re
import string
from vista.set import Constants, DefaultReference,\
     DataReference, DataSetAttr, DataType, \
     FlagUtils, FlaggedDataSetElement, \
     Pathname, \
     RegularTimeSeries, IrregularTimeSeries, \
     SetUtils, PathnamePredicate, \
     PartNamePredicate, PathPartPredicate, \
     SortMechanism
from vista.db.dss import DSSUtil
from vista.time import TimeFactory
from vista.app import MainProperties
DSSUtil.setAccessProperties(MainProperties.getProperties())

def wrap_data(ds, filename='', server='',pathname=''):
    """
    wrap_data(dataset, filename='', server='', pathname=''
    wraps data set in a filename, servername and pathname
    """
    return gen_ref(ds,filename,server,pathname)
#
def gen_ref(ds, filename='', server='', pathname=''):
    "generates a DataReference given a dataset"
    if isinstance(ds, DataReference): return ds
    if (filename == '' and server == '' and pathname == ''):
        return DefaultReference(ds)
    else :
        return DefaultReference(server, filename, pathname, ds)

#
def get_part_id(part):
    part = string.upper(part)
    if part == "A": pId = Pathname.A_PART
    elif part == "B": pId = Pathname.B_PART
    elif part == "C": pId = Pathname.C_PART
    elif part == "E": pId = Pathname.E_PART
    elif part == "F": pId = Pathname.F_PART
    else:
        raise "Part Name should be one of A,B,C,E or F"
    return pId
#
def set_part(pathname, new_part, new_part_id=Pathname.B_PART):
    p=Pathname.createPathname(pathname)
    p.setPart(new_part_id, new_part)
    return p.toString()
def regexp_path(path):
    """
    regexp_path(path):
    given a dss pathname string, returns a string suitable for regexp handling
    """
    path=re.sub('\+','\+',path)
    path=re.sub('\/\/','/.*/',path)
    return path
#
def findparts(g,a='',b='',c='',d='',e='',f='',exact=True):
    """
    findparts(g,a='',b='',c='',d='',e='',f='',exact=True):
    this returns an array of matching data references
    g is the group returned from opendss function
    a,b,c,d,e & f are the part names to be searched for
    exact means that the exact string is matched as opposed to the reg. exp.
    e.g. findparts(g,b='C61',c='FLOW') returns all data references that
    match C61 in the b part and FLOW in the c part exactly.
    """
    pa = [a,b,c,d,e,f]
    map(string.strip,pa)
    if exact:
        for i in range(6):
            if len(pa[i]) > 0:
                pa[i] = '^'+pa[i]+'$'
    return g.find(pa)
#
def findpath(g,path,exact=1):
    """
    findpath(g,path,exact=1):
    this returns an array of matching data references
    g is the group returned from opendss function
    path is the dsspathname e.g. '//C6/FLOW-CHANNEL////'
    exact means that the exact string is matched as opposed to the reg. exp.
    """
    pa = string.split(string.strip(path),'/')[1:]
    while len(pa) < 6: pa.append('')
    if exact:
        for i in range(6):
            if len(pa[i]) > 0:
                pa[i] = '^'+pa[i]+'$'
    return g.find(pa)
#
def find(group,filter,part="",selecting=1):
    """
    find(group, filter, part="", selecting=1):
    returns a copy of the group filtered by the filter...
    part can be one of A,B,C,E or F.
    selecting=0 for removing matched and 1 for keeping only matched
    """
    g=group.clone()
    pId = -1
    try :
        pId = get_part_id(part)
    except :
        pId = -1
    if pId == -1:
        g.filterBy(PathnamePredicate(filter), selecting)
    else:
        g.filterBy(PathPartPredicate(filter,pId), selecting)
    return g
#
def sort(group,part_name="B", increasing=True):
    """
    sort(group , part_name = 'B',increasing=True):
    Sorts given group by pathname part which is one of A,B,C,E or F
    with increasing=True for increasing and increasing=False for decreasing.
    Sort done in place; no value returned.
    """
    if increasing:
        dir = SortMechanism.INCREASING
    else:
        dir = SortMechanism.DECREASING
    pId = get_part_id(part_name)
    group.sortBy(PartNamePredicate(pId,dir))
#
def opendss(filename, server='local'):
    """
    opendss( filename, server='local') :
    opens a dss file with the given filename and returns it in a group object
    The group object behaves like an array of data references. Each such data
    reference contains the data set.
    """
    return DSSUtil.createGroup(server,filename)
#
def writedss(filename, pathname, ds) :
    """
    writedss(filename,pathname,ds):
    writes the given data set ds to the filename and the pathname.
    """
    DSSUtil.writeData(filename,pathname,ds)
#
def writeascii(filename,ds,outputFlags=False) :
    """
    writeascii(filename,ds,outputFlags=False)
    writes the given data set to the given filename.
    """
    SetUtils.write(ds,filename,outputFlags)
#
_dummy_dse = FlaggedDataSetElement()
def make_flag_value(flag_val):
    flag_vals = string.split(flag_val,"|")
    if len(flag_vals) !=2 : raise "Invalid flag: %s"%flag_val
    flag_type = FlagUtils.getQualityFlagId(flag_vals[0])
    user_id = DSSUtil.getUserId(string.lower(flag_vals[1]))
    _dummy_dse.setFlag(0)
    if flag_type == 0:
        FlagUtils.clearAllFlags(_dummy_dse,user_id)
    else:
        FlagUtils.setQualityFlag(_dummy_dse,flag_type,user_id)
    return _dummy_dse.getFlag()
#
def read_dss_txt(file,dssts=True,flag=False):
    """
    read_dss_txt(file, dssts=True,flag=False): reads from a ascii file in dssts or dssits format
    and writes out the data to the appropriate dss file and pathnames.
    If dssts == True then dssts format is assumed in the file else dssits format is assumed
    For more info  look up doc on read_dssts(file) and read_dssits(file)
    If flag == True then it expects a flag value in the ascii file as well
    The flag values in the ascii file are represented as
    flag_type|user_name
    where flag_type is one of UNSCREENED,QUESTIONABLE,MISSING,REJECT,OK
    & user_name is one of the authorized users/agency.
    e.g. MISSING|nsandhu or REJECT|kate
    """
    import string
    tf = TimeFactory.getInstance()
    f = open(file)
    line = f.readline()[:-1]
    dssfile = line
    while line :
        try :
            line = string.upper(f.readline()[:-1])
            if line == "FINISH": break;
            path = Pathname.createPathname(string.upper(line))
        except :
            print 'Incorrect format for path: ', line
            break
        try :
            line = f.readline()[:-1]
            units = string.upper(line)
        except :
            print 'Incorrect format for units: ', line
            break
        try :
            line = f.readline()[:-1]
            type = string.upper(line)
        except :
            print 'Incorrect format for type: ', line
            break
        if dssts:
            try :
                line = f.readline()[:-1]
                stime = tf.createTime(line)
            except :
                print 'Incorrect format for time: ', line
                break
        #
        line = string.upper(f.readline()[:-1])
        xvals = []
        yvals = []
    if flag:
        flags = []
    else:
        flags = None
        while line != "END" :
            if dssts:
                try:
                    if flag:
                        vals = string.split(line)
                        if len(vals) != 2: raise "No flags in file %s @ line: %s"%(file,line)
                        yvals.append(float(vals[0]))
                        flags.append(make_flag_value(vals[1]))
                    else:   # no flags
                        yvals.append(float(line))
                except:
                    yvals.append(Constants.MISSING_VALUE);
                    if flag: flags.append(make_flag_value('MISSING|null'))
            else :
                try :
                    line = string.strip(line)
                    tmstr = line[0:14]
                    tm = tf.createTime(tmstr)
                    try :
                        if flag:
                            vals = string.split(line[14:])
                            if len(vals) != 2: raise "No flags in file %s @ line: %s"%(file,line)
                            val = float(vals[0])
                            flag_val = vals[1]
                        else:   # no flag
                            val = float(line[14:])
                    except :
                        val = Constants.MISSING_VALUE
                    xvals.append(tm)
                    yvals.append(val)
                    flags.append(make_flag_value(flag_val))
                except Exception, exc:
                    print exc
                    print "Error reading line: ", line
            line = string.upper(f.readline()[:-1])
            # create appropriate time series object
            if len(yvals) == 0 : continue
            #print yvals, flags
        if ( dssts ):
            attr = DataSetAttr(DataType.REGULAR_TIME_SERIES,"TIME",units,"",type)
            ts = RegularTimeSeries("",repr(stime),path.getPart(Pathname.E_PART),
                                   yvals, flags, attr)
        else :
            attr = DataSetAttr(DataType.IRREGULAR_TIME_SERIES,"TIME",units,"",type)
            ts = IrregularTimeSeries("",xvals, yvals, flags, attr)
    #
    #for x in ts: print x
    # write to dss
    DSSUtil.writeData(dssfile,repr(path),ts)
    #
# function for regular time series format
def read_dssts(file,flag=0):
    """ read_dssts(file): reads dss from dssts format file and writes it out to a dss file
    DSSTS FORMAT : The ()* brackets are not actually entered and are just to denote the
    parts that repeats. The format is :-

    dss_filename
    ( # not in actual file
    pathname ( of this format /A/B/C//E/F/ where A-F are the various path part names )
    units ( a string for units such as cfs
    type ( one of inst-val, per-val, inst-cum, per-cum, per-avg)
    date (in ddMMMyyyy HHmm format)
    values
    end
    )* # repeate between brackets as many times as necessary
    finish
    """
    read_dss_txt(file,1,flag)
# function for irregular time series format
def read_dssits(file,flag=0):
    """
    read_dssits(file): reads dss from dssits format file and writes it out to a dss file
    DSSITS FORMAT : The ()* brackets are not actually entered and are just to denote the
    parts that repeats. The format is :-

    dss_filename
    ( # not in actual file
    pathname ( of this format /A/B/C//E/F/ where A-F are the various path part names )
    units ( a string for units such as cfs
    type ( one of inst-val, per-val, inst-cum, per-cum, per-avg)
    date ( in ddMMMyyyy HHmm format ) value
    end
    )* # repeate between brackets as many times as necessary
    finish
    """
    read_dss_txt(file,0,flag)
#
pp_map = { "a": Pathname.A_PART,
       "b": Pathname.B_PART,
       "c": Pathname.C_PART,
       "f": Pathname.F_PART}
def rename(oldfile,oldpart,newfile,newpart,part_name="f"):
    """
    rename(oldfile,oldpart,newfile,newpart,part_name="f")
    Reads all matching oldpart's in oldfile and replaces
    oldparts with newparts and writes out to newfile. The
    part_name is the partid to be replaced and is one of
    a,b,c or f.
    """
    g=opendss(oldfile)
    g.filterBy(PathPartPredicate(oldpart,pp_map[part_name]),1)
    if len(g) == 0:
        raise SystemExit,'No FPART: %s in DSS FILE: %s'%(oldpart,oldfile)
    refs = g.getAllDataReferences()
    nrefs = len(refs)
    g.removeDataReference(0,nrefs-1)
    index = 0
    while index < nrefs:
        ref = refs[index]
    path = ref.getPathname()
    ds = ref.getData()
    path.setPart(Pathname.F_PART,newpart)
    print 'Doing %d of %d\nOLD: %s -> NEW: %s'\
          %(index,nrefs-1,str(ref.getPathname()),str(path))
    writedss(newfile,str(path),ds)
    refs[index] = None
    index=index+1
#
def make_dss_path(pathname, a_part='', b_part='', f_part=''):
    """
    make_dss_path(pathname, a_part='', b_part='', f_part='')
    Adds additional A, B and F part text to an input pathname, and
    checks for max length of each part and the pathname. Returns
    a either a pathname object or string which is the new pathname,
    depending on the type of the input pathname.
    """
    pnp = string.split(str(pathname), '/')
    pnp[1] += a_part
    pnp[2] += b_part
    pnp[6] += f_part
    pnp[4] = '' # remove D part for now
    # strictly trim to 32 chars each part
    for ndx in range(1, 7):
        if len(pnp[ndx]) > 32: pnp[ndx] = pnp[ndx][:32]
    pn = string.join(pnp, '/')
    if len(pn) > 80: pn = pn[:80]
    if isinstance(pathname, Pathname): return Pathname.createPathname(pn)
    else: return pn
#
def dsAddFlags(dataset):
    """
    dsAddFlags(dataset)
    Add UNSCREENED_FLAG to dataset that does not have any flags
    """
    if dataset.isFlagged(): return dataset
    # create copy of incoming dataset but with flags
    import jarray
    fa = jarray.zeros(len(dataset), 'i')
    if dataset.getAttributes().getType() == DataType.REGULAR_TIME_SERIES:    #RTS
        datasetFlagged = RegularTimeSeries(dataset.getName(), str(dataset.getStartTime()), \
                             str(dataset.getTimeInterval()), dataset.getYArray(), \
                             fa, dataset.getAttributes())
    else:   # ITS
        xa = jarray.zeros(len(dataset), 'd')
        ya = jarray.zeros(len(dataset), 'd')
        for i in range(len(dataset)):
            xa[i] = dataset[i].getX()
            ya[i] = dataset[i].getY()
        datasetFlagged = IrregularTimeSeries(
            dataset.getName(), xa, ya, fa, dataset.getAttributes())
    return datasetFlagged
#
