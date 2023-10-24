import sys
import string

@COMPONENTSCRIPT

def ordered_print(items):
    """Given a sequence of items, prints them in order with number then item"""
    for i,item in zip(range(len(items)),items):
         print "%s: %s" % (i,item)

def generateNotepad():
    """ Generates a Notepad++ user defined language file with syntax highlights for the keywords """
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


