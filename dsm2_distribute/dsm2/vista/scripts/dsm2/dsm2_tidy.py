""" Cleanup script for dsm2 input files.
Usage:
> dsm2_tidy.py inputfile outputfile
where 
inputfile   is the file to be cleaned
outputfile  is the destination to write
"""

import shlex   # for quote-aware split()
import re
import string
import sys
import zipfile
import os.path

# Some regular expressions we want to compile once
block_txt =\
r"""^([A-Z_]+[ \t]*\n)(.*?)(END[ \t]*$)(\n|(^#.*?\n)*)
"""

top_comment_re=re.compile(r"(\n|(^#.*\n))*",re.M)
block_re=re.compile(block_txt,re.M | re.DOTALL)
number_re = re.compile(r"([0-9.-]+|length|none)")

def is_numberlike(inp):
    """ Returns whether the input a number 
        (or a special case in a field of numbers) 
    """
    isnum=number_re.match(inp.strip()) != None
    return isnum

def tidy(infile,outfile):
    """ tidies up the input file and writes to outfile """
    f=open(infile,"r")
    txt=f.read()
    f.close()
    zipname = r"dsm2_tidy_backup.zip"
    if os.path.exists( zipname ):
        bak = zipfile.ZipFile(zipname,"a")
    else:
        bak = zipfile.ZipFile(zipname,"w")
    bak.write(infile)
    bak.close()
  
    if not txt.endswith("\n"):
        txt += "\n"
    top_comment = top_comment_re.match(txt).group(0)
    if top_comment:
        top_comment=top_comment.strip()
    else:
        top_comment=""
    matches = block_re.findall(txt)
    out=[top_comment]
    if len(matches) == 0:
        raise ValueError("No input blocks found in file %s. Make sure the last block has an END indicated" % infile)
            
    for m in matches:
        key=m[0].strip()
        all_block=m[1].split("\n")
        header=all_block[0].strip().split()
        lines = [line.strip() for line in all_block[1:] if len(line.strip())>0 ]
        max_width = [len(x) for x in header] + [0]
        numberlike = [True for x in header] + [False]
        justify = ["-" for x in header]+["-"]
        numberlike[0] = False
        format=["%s" for x in header]+["%s"]
        inputs = []
        
        for line in lines:
            splitline = line.strip().split("#")
            if (len(splitline) > 1):
                comment ="#%s" % splitline[1].strip()
            else:
                comment = ""
            input = shlex.split(splitline[0])+[comment]
            if (len(numberlike) < len(input)):
                print "Problem with assesing number formate in line: " + line
            for i in range(len(input)):
                if len(input[i]) > max_width[i]: 
                    max_width[i] = len(input[i])
                numberlike[i] &= (is_numberlike(input[i]))
            inputs.append(input )
        out.append(key)
        for i in range(len(numberlike)):
            if numberlike[i]: 
                justify[i]="" 
            format[i]="%"+justify[i]+str(max_width[i])+"s"

        header = [format[i] % header[i] for i in range(len(header))]
        out.append(string.join(header," "))
        for input in inputs:
            fields = []
            for i in range(len(input)):
                if input[i].find(" ") >=0:
                    if not input[i].find("#") == 0:
                        input[i]="\"%s\"" % input[i]
                fieldstr=format[i] % input[i]
                fields.append(fieldstr)
            out.append(string.join(fields," "))
        out.append("END\n")
        trail_comment=m[3]
        out.append(trail_comment)
    outfile=open(outfile,"w")          
    outfile.write(string.join(out,"\n"))

if __name__=="__main__":
    if len(sys.argv) >= 2 and sys.argv[1] != "usage":
        inp = sys.argv[1]
        if len(sys.argv) == 3:
            out = sys.argv[2]
        else:
            out = inp
        try:
            tidy(inp,out)
        except ValueError,e:
            print e
            print "Error in dsm2_tidy"
    elif (len(sys.argv) == 2) and sys.argv[1] == "usage":
        print __doc__
    else:
        print __doc__
    sys.exit()

