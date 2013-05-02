# reads from a dss ts or its file and writes out to the appropriate
# dss file and pathname
def read_dss_txt(file,dssts=1):
    import string
    from vista.set import DataSetAttr, DataType
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
        if ( dssts ) :
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
        while line != "END" :
            if ( dssts ) :
                try :
                    yvals.append(string.atof(line))
                except :
                    yvals.append(Constants.MISSING_VALUE);
            else :
                try :
                    line = string.strip(line)
                    tmstr = line[0:14]
                    tm = tf.createTime(tmstr)
                    try :
                        val = string.atof(line[14:])
                    except :
                        val = Constants.MISSING_VALUE
                    xvals.append(tm)
                    yvals.append(val)
                except:
                    print "Error reading line: ", line
            line = string.upper(f.readline()[:-1])
        # create appropriate time series object
        if len(yvals) == 0 : continue
        if ( dssts ):
            attr = DataSetAttr(DataType.REGULAR_TIME_SERIES,"TIME",units,"",type)
            ts = RegularTimeSeries("",repr(stime),path.getPart(Pathname.E_PART),
                                   yvals, None, attr)
        else :
            attr = DataSetAttr(DataType.IRREGULAR_TIME_SERIES,"TIME",units,"",type)
            ts = IrregularTimeSeries("",xvals, yvals,None, attr)
        # write to dss
        DSSUtil.writeData(dssfile,repr(path),ts)
    #
# function for regular time series format
def read_dssts(file):
    " reads dss from dssts format"
    read_dss_txt(file,1)
# function for irregular time series format
def read_dssits(file):
    read_dss_txt(file,0)
#
