import sys
import string

def component_order():
    return["qext",\
      "hydro_comp_point",\
      "reservoir_node_connect",\
      "reservoir_flow_connections",\
      "node_flow_connections",\
      "stage_boundaries",\
      "virtual_xsect"]


def component_members():
    return {"qext":["name","attach_obj_name","attached_obj_type","attached_obj_no"],\
      "hydro_comp_point":["comp_index","channel","distance"],\
      "reservoir_node_connect":["res_node_index","res_name","res_index","connect_index","node_no","ext_node_no","connection_type"],\
      "reservoir_flow_connections":["connection_index","res_name","res_index","res_flow_index","flow_index","flow_name","flow_type"],\
      "node_flow_connections":["connection_index","int_node_no","ext_node_no","node_flow_index","flow_index","flow_name","flow_type"],\
      "stage_boundaries":["name","int_node_no","ext_node_no"],\
      "virtual_xsect":["chan_no","num_virt_sec","vsecno","num_elev","min_elev","elevation","area","wet_p","width"]}


def include_block():
    return {\
    "node__flow_connections":"processed_data",
    "reservoir_flow_connections":"processed_data",
    "virtual_xsect":"processed_data",
    "reservoir_node_connect":"processed_data",
    "hydro_comp_point":"processed_data",
    "qext":"processed_data"}

def include_block_order():
      return[\
      "processed_data"]



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


