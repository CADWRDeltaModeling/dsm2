#! /bin/ksh
# set up default values
PRG=`whence $0` >/dev/null 2>&1
V_HOME=`dirname $PRG`/..
progname=`basename $0`
# if VISTA_HOME not set then use default
if [ -z "$VISTA_HOME" ] ; then
    export VISTA_HOME
    VISTA_HOME=$V_HOME
fi
# set library path for local client
if [ -z "${LD_LIBRARY_PATH}" ] ; then
    LD_LIBRARY_PATH=$VISTA_HOME/lib
else
    LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$VISTA_HOME/lib
fi
export LD_LIBRARY_PATH
#
exec /site/java/jdk1.2beta4/bin/java -ss1m -mx32m -classpath $VISTA_HOME/lib/vista.jar:.:$VISTA_HOME/lib/Acme.jar:$VISTA_HOME/lib/oro.jar:$VISTA_HOME/lib/COM.jar:$VISTA_HOME/lib/swingall.jar:$VISTA_HOME/lib/mail.jar:$VISTA_HOME/lib/activation.jar DWR.Data.GUI.MainGUI 
