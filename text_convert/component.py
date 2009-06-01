import sys
import string

def component_order():
    return["envvar",\
      "scalar",\
      "channel",\
      "xsect",\
      "xsect_layer",\
      "reservoir",\
      "reservoir_connection",\
      "gate",\
      "gate_device",\
      "transfer",\
      "io_file",\
      "tidefile",\
      "group",\
      "group_member",\
      "channel_ic",\
      "reservoir_ic",\
      "operating_rule",\
      "oprule_expression",\
      "oprule_time_series",\
      "rate_coefficient",\
      "particle_insertion",\
      "particle_flux_output",\
      "particle_group_output",\
      "input_climate",\
      "input_transfer_flow",\
      "input_gate",\
      "boundary_stage",\
      "boundary_flow",\
      "source_flow",\
      "source_flow_reservoir",\
      "node_concentration",\
      "reservoir_concentration",\
      "output_channel",\
      "output_reservoir",\
      "output_channel_source_group",\
      "output_reservoir_source_group",\
      "output_gate"]


def component_members():
    return {"envvar":["name","value"],\
      "scalar":["name","value"],\
      "channel":["chan_no","length","manning","dispersion","upnode","downnode"],\
      "xsect":["chan_no","dist","file"],\
      "xsect_layer":["chan_no","dist","elev","area","width","wet_perim"],\
      "reservoir":["name","area","bot_elev"],\
      "reservoir_connection":["res_name","node","coef_in","coef_out"],\
      "gate":["name","from_obj","from_identifier","to_node"],\
      "gate_device":["gate_name","device","structure","nduplicate","width","elev","height","cf_to_node","cf_from_node","default_op","position_control"],\
      "transfer":["name","from_obj","from_identifier","to_obj","to_identifier"],\
      "io_file":["model","type","io","interval","file"],\
      "tidefile":["start_date","end_date","file"],\
      "group":["name"],\
      "group_member":["group_name","member_type","pattern"],\
      "channel_ic":["chan_no","distance","stage","flow"],\
      "reservoir_ic":["res_name","stage"],\
      "operating_rule":["name","action","trigger"],\
      "oprule_expression":["name","definition"],\
      "oprule_time_series":["name","fillin","file","path"],\
      "rate_coefficient":["group_name","constituent","variable","value"],\
      "particle_insertion":["node","nparts","delay","duration"],\
      "particle_flux_output":["name","from_wb","to_wb","interval","file"],\
      "particle_group_output":["name","group_name","interval","file"],\
      "input_climate":["name","variable","fillin","file","path"],\
      "input_transfer_flow":["transfer_name","fillin","file","path"],\
      "input_gate":["gate_name","device","variable","fillin","file","path"],\
      "boundary_stage":["name","node","fillin","file","path"],\
      "boundary_flow":["name","node","fillin","file","path"],\
      "source_flow":["name","node","sign","fillin","file","path"],\
      "source_flow_reservoir":["name","res_name","sign","fillin","file","path"],\
      "node_concentration":["name","node_no","variable","fillin","file","path"],\
      "reservoir_concentration":["name","res_name","variable","fillin","file","path"],\
      "output_channel":["name","chan_no","distance","variable","interval","period_op","file"],\
      "output_reservoir":["name","res_name","node","variable","interval","period_op","file"],\
      "output_channel_source_group":["name","chan_no","distance","variable","source_group","interval","period_op","file"],\
      "output_reservoir_source_group":["name","res_name","variable","source_group","interval","period_op","file"],\
      "output_gate":["name","gate_name","device","variable","interval","period_op","file"]}


def include_block():
    return {\
    "output_reservoir_concentration":"output_time_series",
    "reservoir":"grid",
    "particle_flux_output":"particle",
    "xsect":"grid",
    "output_gate":"output_time_series",
    "scalar":"configuration",
    "boundary_stage":"hydro_time_series",
    "node_concentration":"qual_time_series",
    "xsect_layer":"grid",
    "group_member":"groups",
    "oprule_expression":"operation",
    "input_transfer_flow":"hydro_time_series",
    "group":"groups",
    "output_channel":"output_time_series",
    "particle_group_output":"particle",
    "transfer":"grid",
    "input_climate":"qual_time_series",
    "oprule_time_series":"operation",
    "particle_insertion":"particle",
    "gate_device":"grid",
    "channel":"grid",
    "boundary_flow":"hydro_time_series",
    "output_channel_concentration":"output_time_series",
    "input_gate":"hydro_time_series",
    "operating_rule":"operation",
    "reservoir_concentration":"qual_time_series",
    "reservoir_ic":"initial_condition",
    "gate":"grid",
    "rate_coefficient":"qual_spatial",
    "reservoir_connection":"grid",
    "source_flow":"hydro_time_series",
    "source_flow_reservoir":"hydro_time_series",
    "channel_ic":"initial_condition",
    "envvar":"configuration",
    "output_reservoir":"output_time_series"}









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
    folds = string.join([x.upper() for x in tablelist]," ")
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


