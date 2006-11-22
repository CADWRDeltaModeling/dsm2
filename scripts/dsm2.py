##!d:/vista/bin/vscript.bat 

from jnios import os
import sys,string,time

from config import setConfigVars,getAttr

#
if __name__ == '__main__':
    if len(sys.argv) <> 3:
        print len(sys.argv),sys.argv[1],sys.argv[2],sys.argv[3]
        raise SystemExit("""
        Usage: vscript dsm2.py [both|hydro|qual] [configurationfile]
        where  configurationfile  is the input file for env variables
        (give full path if not in current running shell)
        and  both|hydro|qual indicates which DSM2 module
        to run, default is both
        """)
    else:
        dsm2module = sys.argv[1]
        infile = sys.argv[2]
    if dsm2module != "both" and dsm2module != "hydro" and dsm2module != "qual" and dsm2module != "ptm" and dsm2module != "qual_do" and dsm2module != "qual_ec":
        print 'Inputs: ' + sys.argv[1] + ' ' + sys.argv[2]
        raise SystemExit("""Must specify both, hydro, qual or ptm""")
    print 'Loading configuration file'
    setConfigVars(infile)

if dsm2module == "both" or dsm2module == "hydro":
    print 'Running Hydro'
################some change  made by Jon#####################    
    hydroexe=getAttr('hydroexe')
    hydroinp=getAttr('hydroinp')

    if hydroexe:
        command=getAttr('hydroexe')
        from os.path import exists
        if not exists(command):
            raise command+' is not a valid path'

    else:
        command='hydro.exe'
        
    if hydroinp:
        command=command+" "+hydroinp
    else:
        raise "no path to hydro.inp is specified"
   #command=getAttr('hydroexe') + ' hydro.inp '
 ##################################################       
   
    
    
    #command='"C:\Program Files\Microsoft Visual Studio\Common\MSDev98\Bin\DFDEV.EXE"' + ' d:\delta\models\dsm2-dbase\dsm2.dsw'

    print "Command: ", command
    status=os.system(command)
    #print time.asctime()
    if status == 0:
        print "Hydro run completed."
    else:
        print "Hydro run failed; status: %d" %(status)
        sys.exit()
#
if dsm2module == "both" or dsm2module == "qual" or dsm2module == "qual_ec":
    print 'Running Qual'
################some change  made by Jon#####################    
    qualexe=getAttr('qualexe')
    qualinp=getAttr('qualinp')

    if qualexe:
        command=getAttr('qualexe')
        from os.path import exists
        if not exists(command):
            raise command+' is not a valid path'

    else:
        command='qual.exe'
        
    if qualinp:
        command=command+" "+qualinp
    else:
        raise "no path to qual.inp is specified"
     #command=getAttr('qualexe') + ' qual_ec.inp ' 
 ##################################################  
   
    #command='"C:\Program Files\Microsoft Visual Studio\Common\MSDev98\Bin\DFDEV.EXE"' + ' d:\delta\models\dsm2-dbase\dsm2.dsw'
    print "Command: ", command
    #print time.asctime()
    status=os.system(command)
    if status == 0:
        print "Qual run completed."
        #print time.asctime()
    else:
        print "Qual run failed."
        #print time.asctime()
        sys.exit()

if dsm2module == "both" or dsm2module == "qual_do":
    print 'Running Qual DO Run'
    command=getAttr('qualexe') + ' qual_do.inp ' 
    #command='"C:\Program Files\Microsoft Visual Studio\Common\MSDev98\Bin\DFDEV.EXE"' + ' d:\delta\models\dsm2-dbase\dsm2.dsw'
    print "Command: ", command
    #print time.asctime()
    status=os.system(command)
    if status == 0:
        print "Qual run completed."
        #print time.asctime()
    else:
        print "Qual run failed."
        #print time.asctime()
        sys.exit()

if dsm2module=="ptm":
    print'Running PTM'
    command=" .\\ptm.bat .\\ptm.inp"
    print "Command: ", command    
    status=os.system(command)
    if status == 0:
        print "PTM run completed."
        #print time.asctime()
    else:
        print "PTM run failed."
        #print time.asctime()
        sys.exit()    
#
sys.exit()





