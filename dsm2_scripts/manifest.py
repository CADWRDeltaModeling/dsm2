""" The manifest script takes a dsm2 echo file and produces a list of all the .inp and .dss files in the simulation
    It can also optionally (by providing and empty or nonexistent directory name) copy all the files to the specified location
"""


from optparse import OptionParser
import shutil
import os
import re
import sys
import string
import os.path



def manifest(infile,outfile,copydir):
    f=open(infile,'r')
    txt=f.readlines()
    f.close()
    out=open(outfile,'w')
    inp_file_re=re.compile(r"(#\s?[0-9]+)\s+(.*?\.inp)")
    entry_file_re=re.compile(r"(# Entry file \(top priority\):)\s+(.*?\.inp)")
    dss_file_re=re.compile(r"\s+([\S\/]*?\.dss)\b")
    past_inp=False
    past_scalar=False
    filelist = []
    dssfilelist = []
    for line in txt:
        if "SCALAR" in line:
            past_scalar=True
        if "OUTPUT_" in line:
            break
        if "Simulation input data" in line:
            past_inp=True
        if past_scalar:
            for m in dss_file_re.findall(line):
                dssfilelist.append(m)
        elif not past_inp:
            m=entry_file_re.match(line)
            if m:
                filelist.append(m.group(2))
                continue
            m=inp_file_re.match(line)
            if m: filelist.append(m.group(2))
    
    dssfiles = set(dssfilelist)
    files = filelist + list(dssfiles)
    # not portable!!!
    files = [x.replace("/","\\") for x in files]
    out.write(string.join(files,"\n"))
    out.close()
    if copydir:
        if os.path.exists(copydir):
            if len(os.listdir(copydir)) != 0:
                print "Copy directory %s already exists and is not empty. Manifest created, but no copy made" % copydir
                return
        else:
            os.mkdir(copydir)
        for f in files:
            shutil.copy(f,copydir)
        
       
if (__name__ == "__main__"):
    if len(sys.argv) < 3 or sys.argv[1] == "--help" or sys.argv[1] == "--usage":
        print "\nUsage: manifest.py echofile manifestfile [copydir]\n"\
              "\nCopydir is an optional directory. If provided, all .inp and .dss files in\nmanifest will be copied here"
    else:
        # parser = OptionParser()
        #   parser.add_option("-i", "--infile", dest="infile",
                  # help="echo file used as input for manifest", metavar="INFILE")
        # parser.add_option("-o", "--outfile", dest="outfile",
                  # help="manifest", metavar="MANIFEST")
        # parser.add_option("-c", "--copydir", dest="copydir",
                  # help="directory for copies", metavar="COPYDIR")
        # (options, args) = parser.parse_args()
        infile = sys.argv[1]
        if not infile.endswith(".inp"):
            print "Source echo file does not have extension .inp; for safety, aborting"
        else:
            outfile = sys.argv[2]
            copydir = None
            if len(sys.argv) > 3: 
                copydir = sys.argv[3]
            manifest(infile,outfile,copydir)
            sys.exit()