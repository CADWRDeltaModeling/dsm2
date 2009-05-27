import sys
import string

@COMPONENTSCRIPT



def fixup(infile,outfile,component,reorder):
    f = open(infile,'r')
    fout = open(outfile,'w')
    lines = f.readlines()
    mlist = component_members()[component]
    fout.write("%s\n" % component.upper())
    fout.write(string.join([x.upper() for x in mlist],"    ")+"\n")
    for line in lines:
        if line.strip(" \n") == "": continue
        parts = line.strip().split("\t")
        if len(parts) == 0: continue
        parts=[parts[i] for i in reorder]
        fout.write(string.join(parts, "    ")+"\n")
    fout.write("END\n\n")

def ordered_print(items):
    for i,item in zip(range(len(items)),items):
         print "%s: %s" % (i,item)

def generateNotepad():
    tablelist=component_order()
    includes = set(include_block().values())
    folds = string.join([x.upper() for x in tablelist+list(includes)]," ")
    member_dict=component_members()
    keys=[]
    for key in member_dict:
        keys+=member_dict[key]
    keywords = string.join([key.upper() for key in keys]," ")
    userfile = open("userDefineLangTemplate.xml",'r')
    usertxt = userfile.read()
    userfile.close()
    usertxt=usertxt.replace("@FOLDS",folds)
    usertxt=usertxt.replace("@KEYS",keywords)

    userfile = open("userDefineLang.xml","w")
    userfile.write(usertxt)
    userfile.close()
                  
         
         
if (__name__=="__main__"):
    if len(sys.argv) == 1:
        print "Usage: component order"
        print "   or: component members [table]"
    else:
        arg=sys.argv[1]
        if arg == "order":
            corder = component_order()
            ordered_print(corder)
        if arg == "members": 
            members = component_members()
            if len(sys.argv)==2:
                for key in component_order():
                    print "%s" % key
                    morder = members[key]
                    ordered_print(morder)
                    print "\n"
            else:
                ordered_print(component_members()[sys.argv[2]])
        if arg == "notepad":
            generateNotepad()


