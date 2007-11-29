#! /bin/ksh
# set up default values
PRG=`whence $0` >/dev/null 2>&1
V_HOME=`dirname $PRG`/..
progname=`basename $0`
# if PTM_HOME not set then use default
if [ -z "$PTM_HOME" ] ; then
    export PTM_HOME
    PTM_HOME=$V_HOME
fi
# set library path for local client
if [ -z "${LD_LIBRARY_PATH}" ] ; then
    LD_LIBRARY_PATH=$PTM_HOME/lib
else
    LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$PTM_HOME/lib
fi
export LD_LIBRARY_PATH
#
exec jre -ss1m -mx32m -classpath $JAVA_HOME/lib/classes.zip:$PTM_HOME/lib/ptm.jar:$PTM_HOME/lib/edu.jar:$PTM_HOME/lib/COM.jar DWR.DMS.PTM.mainPTM $*
