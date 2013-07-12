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
      "gate_pipe_device",\
      "gate_weir_device",\
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
      "particle_filter",\
      "particle_res_filter",\
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
      "output_channel_source_track",\
      "output_reservoir_source_track",\
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
      "gate_pipe_device":["gate_name","device","nduplicate","radius","elev","cf_from_node","cf_to_node","default_op"],\
      "gate_weir_device":["gate_name","device","nduplicate","width","elev","height","cf_from_node","cf_to_node","default_op"],\
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
      "particle_filter":["name","node","at_wb","fillin","file","path"],\
      "particle_res_filter":["name","res_name","at_wb","fillin","file","path"],\
      "particle_flux_output":["name","from_wb","to_wb","interval","file"],\
      "particle_group_output":["name","group_name","interval","file"],\
      "input_climate":["name","variable","fillin","file","path"],\
      "input_transfer_flow":["transfer_name","fillin","file","path"],\
      "input_gate":["gate_name","device","variable","fillin","file","path"],\
      "boundary_stage":["name","node","fillin","file","path"],\
      "boundary_flow":["name","node","sign","fillin","file","path"],\
      "source_flow":["name","node","sign","fillin","file","path"],\
      "source_flow_reservoir":["name","res_name","sign","fillin","file","path"],\
      "node_concentration":["name","node_no","variable","fillin","file","path"],\
      "reservoir_concentration":["name","res_name","variable","fillin","file","path"],\
      "output_channel":["name","chan_no","distance","variable","interval","period_op","file"],\
      "output_reservoir":["name","res_name","node","variable","interval","period_op","file"],\
      "output_channel_source_track":["name","chan_no","distance","variable","source_group","interval","period_op","file"],\
      "output_reservoir_source_track":["name","res_name","variable","source_group","interval","period_op","file"],\
      "output_gate":["name","gate_name","device","variable","interval","period_op","file"]}


def include_block():
    return {\
    "reservoir":"hydro_time_series",
    "particle_flux_output":"particle",
    "xsect":"grid",
    "output_gate":"output_time_series",
    "scalar":"parameter",
    "boundary_stage":"hydro_time_series",
    "node_concentration":"gtm_time_series",
    "gate_pipe_device":"hydro_time_series",
    "xsect_layer":"grid",
    "output_channel_source_track":"output_time_series",
    "group_member":"groups",
    "oprule_expression":"operation",
    "input_transfer_flow":"hydro_time_series",
    "group":"groups",
    "output_channel":"output_time_series",
    "particle_group_output":"particle",
    "transfer":"hydro_time_series",
    "input_climate":"gtm_time_series",
    "oprule_time_series":"operation",
    "particle_insertion":"particle",
    "channel":"grid",
    "boundary_flow":"hydro_time_series",
    "input_gate":"hydro_time_series",
    "operating_rule":"operation",
    "reservoir_concentration":"gtm_time_series",
    "reservoir_ic":"initial_condition",
    "gate":"hydro_time_series",
    "output_reservoir_source_track":"output_time_series",
    "particle_res_filter":"particle",
    "rate_coefficient":"gtm_spatial",
    "reservoir_connection":"hydro_time_series",
    "source_flow":"hydro_time_series",
    "gate_weir_device":"hydro_time_series",
    "particle_filter":"particle",
    "source_flow_reservoir":"hydro_time_series",
    "channel_ic":"initial_condition",
    "envvar":"configuration",
    "output_reservoir":"output_time_series"}

def include_block_order():
      return[\
      "configuration",\
      "parameter",\
      "grid",\
      "initial_condition",\
      "hydro_time_series",\
      "operation",\
      "groups",\
      "qual_time_series",\
      "qual_spatial",\
      "gtm_time_series",\
      "gtm_spatial",\
      "output_time_series",\
      "particle"]



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


