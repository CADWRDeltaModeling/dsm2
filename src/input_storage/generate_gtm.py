""" DSM2 input specifications. 
This script defines the input blocks, fields, groupings and profiles for the DSM2 models.

"""
DSM2_NAME_LEN=32
DSM2_OBJECT_TYPE_LEN=16
DSS_FILE_LEN=128
INTERVAL_LEN=16
PERIOD_OP_LEN=16

import sys
import os

#sys.path.append(os.getenv("input_storage_home")+"\\src")
sys.path.append("../../../input_storage/src")

from generate_input_storage import *
LAST_FIELD=1

def generate_dsm2():
    outdir = os.path.dirname(__file__)
    # Create an object called an envvar, that will be used for text substitutions
    component = TableComponent("envvar",
                           [CharField("name",DSM2_NAME_LEN,32),\
                            CharField("value",128,LAST_FIELD)],
                           ["name"])
    component.layered=True
    prep_component(component,outdir)

    # Create an object called a scalar, that will be used for text substitutions
    component = TableComponent("scalar",
                           [CharField("name",DSM2_NAME_LEN,32),\
                            CharField("value",64,LAST_FIELD)],
                           ["name"])
    component.layered=True
    prep_component(component,outdir)

    # Create a parent object called a channel.
    component = TableComponent("channel",         # name of the table
                           [IntField("chan_no"),  # integer field
                           IntField("length"),
                           DoubleField("manning",10,4),  # double field with width=10 precision=4 
                           DoubleField("dispersion",12,4),  # double field with width=10 precision=4                            
                           IntField("upnode"),
                           IntField("downnode")],
                           ["chan_no"])            # chan_no is the identifier        
    component.layered=True                         # Component is part of the layering system
    prep_component(component,outdir)               # Group reads/writes/clears are based on the
                                                   # the order in which they are "prepped" 

    # Create the child object called a xsect. It is linked to a parent channel by the field
    # chan_no
    component = TableComponent("xsect",
                           [IntField("chan_no"), 
                           DoubleField("dist",8,3),
                           CharField("file",128,24)],  # character width is allocated for 128 characters, but the printing "hint" is width=24   
                           ["chan_no","dist"],         # identifiers
                           parent="channel",           # parent table. overrides will be based on the channel table
                           parent_id=["chan_no"])      # field in the xsect that links to the parent identifier (in this case also chan_no)              
    component.layered = False
    prep_component(component,outdir)

    component = TableComponent("xsect_layer",
                           [IntField("chan_no"), 
                           DoubleField("dist",8,3),
                           DoubleField("elev",10,3),
                           DoubleField("area",12,3),
                           DoubleField("width",12,3),
                           DoubleField("wet_perim",12,3)],
                           ["chan_no","dist","elev"],
                           parent="channel",                  # parent table. overrides will be based on the channel table
                           parent_id=["chan_no"])             # field in the xsect that links to the parent identifier (in this case also chan_no)              
    component.layered = False
    prep_component(component,outdir)    
    

    component = TableComponent("reservoir",
                           [CharField("name",DSM2_NAME_LEN,16),
                           DoubleField("area",12,6),
                           DoubleField("bot_elev",12,2)],
                           ["name"])
    component.layered=True                         # Component is part of the layering system
    prep_component(component,outdir)               # Group reads/writes/clears are based on the
    
    component = TableComponent("reservoir_connection",
                           [CharField("res_name",DSM2_NAME_LEN,16), 
                           IntField("node"),
                           DoubleField("coef_in",10,3),
                           DoubleField("coef_out",12,3)],
                           ["res_name","node"],
                           parent="reservoir",                  # parent table. overrides will be based on the channel table
                           parent_id=["res_name"])             # field in the xsect that links to the parent id (in this case also chan_no)              
    component.layered = False
    prep_component(component,outdir)    
 
    component = TableComponent("gate",
                           [CharField("name",DSM2_NAME_LEN,16),
                           CharField("from_obj",DSM2_OBJECT_TYPE_LEN,16),
                           CharField("from_identifier",DSM2_NAME_LEN,16),
                           IntField("to_node")],
                           ["name"])
    component.layered=True                         # Component is part of the layering system
    prep_component(component,outdir)               # Group reads/writes/clears are based on the
 
    component = TableComponent("gate_pipe_device",
                           [CharField("gate_name",DSM2_NAME_LEN,16),
                            CharField("device",DSM2_NAME_LEN,16),                           
                            IntField("nduplicate"),
                            DoubleField("radius",10,3),
                            DoubleField("elev",10,3),
                            DoubleField("cf_from_node",14,4),                            
                            DoubleField("cf_to_node",14,4),
                            CharField("default_op",16,18)],
                            ["gate_name","device"],
                            parent="gate",                  # parent table. overrides will be based on the channel table
                            parent_id=["gate_name"])             # field in the xsect that links to the parent identifier (in this case also chan_no)              
    component.layered = False
    prep_component(component,outdir)   
 
    component = TableComponent("gate_weir_device",
                           [CharField("gate_name",DSM2_NAME_LEN,16),
                            CharField("device",DSM2_NAME_LEN,16),                           
                            IntField("nduplicate"),
                            DoubleField("width",10,3),
                            DoubleField("elev",10,3),
                            DoubleField("height",10,3),
                            DoubleField("cf_from_node",14,4),                            
                            DoubleField("cf_to_node",14,4),
                            CharField("default_op",16,18)],
                            ["gate_name","device"],
                            parent="gate",                  # parent table. overrides will be based on the channel table
                            parent_id=["gate_name"])             # field in the xsect that links to the parent identifier (in this case also chan_no)              
    component.layered = False
    prep_component(component,outdir)    
   
    component = TableComponent("transfer",
                           [CharField("name",DSM2_NAME_LEN,16),
                           CharField("from_obj",DSM2_OBJECT_TYPE_LEN,16),
                           CharField("from_identifier",DSM2_NAME_LEN,16),
                           CharField("to_obj",DSM2_OBJECT_TYPE_LEN,16),
                           CharField("to_identifier",DSM2_NAME_LEN,16)],
                           ["name"])
    component.layered=True                         # Component is part of the layering system
    prep_component(component,outdir)               # Group reads/writes/clears are based on the

   
    component = TableComponent("io_file",
                            [CharField("model",8,8),\
                             CharField("type",8,8),\
                             CharField("io",8,8),\
                             CharField("interval",16,10),\
                             CharField("file",128,LAST_FIELD)],
                             ["model","type","io"])
    component.layered=True
    prep_component(component,outdir)

    component = TableComponent("tidefile",
                              [CharField("start_date",16,16),\
                              CharField("end_date",16,16),\
                              CharField("file",128,LAST_FIELD)],
                              ["start_date"])
    component.layered=True
    prep_component(component,outdir)

    
    component = TableComponent("group",
                              [CharField("name",DSM2_NAME_LEN,LAST_FIELD)],
                              ["name"])
    component.layered=True
    prep_component(component,outdir)
    
    component = TableComponent("group_member",
                              [CharField("group_name",DSM2_NAME_LEN,16),
                               CharField("member_type",DSM2_OBJECT_TYPE_LEN,16),
                               CharField("pattern",256,LAST_FIELD)],                              
                              ["group_name","pattern"],\
                              parent="group",\
                              parent_id=["group_name"])
    component.layered=False
    prep_component(component,outdir)
    
    
    component = TableComponent("channel_ic",
                              [IntField("chan_no",),
                               CharField("distance",8,8),
                               DoubleField("stage",10,4),
                               DoubleField("flow",12,4)],          
                              ["chan_no","distance"])
    component.layered=True
    prep_component(component,outdir)

    component = TableComponent("reservoir_ic",
                              [CharField("res_name",DSM2_NAME_LEN,16),
                               DoubleField("stage",10,3)],          
                              ["res_name"])
    component.layered=True
    prep_component(component,outdir)

    
    component = TableComponent("operating_rule",
                            [CharField("name",DSM2_NAME_LEN,32),\
                             CharField("action",512,48),\
                             CharField("trigger",512,LAST_FIELD)],\
                             ["name"])
    component.layered=True
    prep_component(component,outdir)    

    component = TableComponent("oprule_expression",
                            [CharField("name",DSM2_NAME_LEN,32),\
                             CharField("definition",512,LAST_FIELD)],\
                             ["name"])
    component.layered=True
    prep_component(component,outdir)    
    
    component = TableComponent("oprule_time_series",
                             [CharField("name",DSM2_NAME_LEN,16),\
                              CharField("fillin", 8,12),\
                              CharField("file",DSS_FILE_LEN,32),\
                              CharField("path",80,LAST_FIELD)],\
                             ["name"])   # identifier
    component.layered=True
    prep_component(component,outdir)  
    
    component = TableComponent("rate_coefficient",
                              [CharField("group_name",DSM2_NAME_LEN,16),
                              CharField("constituent",16,16),
                              CharField("variable",16,16),
                              DoubleField("value",16,4)],          
                              ["group_name","constituent","variable"])
    component.layered=True
    prep_component(component,outdir)

    component = TableComponent("group_variable",
                              [CharField("group_name",DSM2_NAME_LEN,16),
                              CharField("constituent",16,16),
                              CharField("variable",16,16),
                              DoubleField("value",16,4)],          
                              ["group_name","constituent","variable"])
    component.layered=True
    prep_component(component,outdir)

    component = TableComponent("group_variable_sed",
                              [CharField("group_name",DSM2_NAME_LEN,16),
                              IntField("sed_zone"),
                              IntField("sed_layer"),
                              CharField("variable",16,16),
                              DoubleField("value",16,4)],          
                              ["group_name","sed_zone","sed_layer","variable"])
    component.layered=True
    prep_component(component,outdir)
    
    component = TableComponent("particle_insertion",
                             [IntField("node"),\
                              IntField("nparts"),\
                              CharField("delay", 8,12),\
                              CharField("duration",16,12)],
                             ["node", "delay"])   # identifier
    component.layered=True
    prep_component(component,outdir)

    component = TableComponent("particle_filter",
                             [CharField("name", DSM2_NAME_LEN,16),\
                              IntField("node"),\
                              CharField("at_wb",DSM2_NAME_LEN,24),\
                              CharField("fillin", 8,12),\
                              CharField("file",DSS_FILE_LEN,32),\
                              CharField("path",80,LAST_FIELD)
                             ],
                             ["name"])   # identifier
    component.layered=True
    prep_component(component,outdir)

    component = TableComponent("particle_res_filter",
                             [CharField("name", DSM2_NAME_LEN,16),\
                              CharField("res_name",32,16),\
                              CharField("at_wb",DSM2_NAME_LEN,24),\
                              CharField("fillin", 8,12),\
                              CharField("file",DSS_FILE_LEN,32),\
                              CharField("path",80,LAST_FIELD)
                             ],
                             ["name"])   # identifier
    component.layered=True
    prep_component(component,outdir)

    component = TableComponent("particle_flux_output",
                             [CharField("name",DSM2_NAME_LEN,16),
                              CharField("from_wb",DSM2_NAME_LEN+8,24),\
                              CharField("to_wb",DSM2_NAME_LEN+8,24),\
                              CharField("interval",INTERVAL_LEN,10),\
                              CharField("file",DSS_FILE_LEN,LAST_FIELD)             
                             ],
                             ["name"])   # identifier
    component.layered=True
    prep_component(component,outdir)

    
    component = TableComponent("particle_group_output",
                             [CharField("name",DSM2_NAME_LEN,16),
                              CharField("group_name",DSM2_NAME_LEN+8,24),\
                              CharField("interval",INTERVAL_LEN,10),\
                              CharField("file",DSS_FILE_LEN,LAST_FIELD)             
                             ],
                             ["name"])   # identifier
    component.layered=True
    prep_component(component,outdir)    
    

#    Input and output ###########################################    


    component = TableComponent("input_climate",
                             [CharField("name",DSM2_NAME_LEN,16),\
                              CharField("variable",16,12),\
                              CharField("fillin", 8,12),\
                              CharField("file",DSS_FILE_LEN,32),\
                              CharField("path",80,LAST_FIELD)
                             ],
                             ["name","variable"])   # identifier
    component.layered=True
    prep_component(component,outdir)  


    component = TableComponent("input_transfer_flow",
                             [CharField("transfer_name",DSM2_NAME_LEN,16),\
                              CharField("fillin", 8,12),\
                              CharField("file",DSS_FILE_LEN,32),\
                              CharField("path",80,LAST_FIELD)
                             ],
                             ["transfer_name"])   # identifier
    component.layered=True
    prep_component(component,outdir)  


    component = TableComponent("input_gate",
                             [CharField("gate_name",DSM2_NAME_LEN,16),\
                              CharField("device",32,16),\
                              CharField("variable",16,12),\
                              CharField("fillin", 8,12),\
                              CharField("file",DSS_FILE_LEN,32),\
                              CharField("path",80,LAST_FIELD)
                             ],
                             ["gate_name","device","variable"])   # identifier
    component.layered=True
    prep_component(component,outdir)  
 

    component = TableComponent("boundary_stage",
                             [CharField("name",DSM2_NAME_LEN,16),\
                              IntField("node"),
                              CharField("fillin", 8,12),\
                              CharField("file",DSS_FILE_LEN,32),\
                              CharField("path",80,LAST_FIELD)
                             ],
                             ["name"])   # identifier
    component.layered=True
    prep_component(component,outdir)  
    
    component = TableComponent("boundary_flow",
                             [CharField("name",DSM2_NAME_LEN,16),\
                              IntField("node"),\
							  IntField("sign"),\
                              CharField("fillin", 8,12),\
                              CharField("file",DSS_FILE_LEN,32),\
                              CharField("path",80,LAST_FIELD)
                             ],
                             ["name"])   # identifier
    component.layered=True
    prep_component(component,outdir)      
    

    component = TableComponent("source_flow",
                             [CharField("name",DSM2_NAME_LEN,16),\
                              IntField("node"),\
                              IntField("sign"),\
                              CharField("fillin", 8,12),\
                              CharField("file",DSS_FILE_LEN,32),\
                              CharField("path",80,LAST_FIELD)
                             ],
                             ["name"])   # identifier
    component.layered=True
    prep_component(component,outdir)      

   
    component = TableComponent("source_flow_reservoir",
                             [CharField("name",DSM2_NAME_LEN,16),\
                              CharField("res_name",32,16),\
                              IntField("sign"),\
                              CharField("fillin", 8,12),\
                              CharField("file",DSS_FILE_LEN,32),\
                              CharField("path",80,50)
                              ],
                             ["name"])   # identifier
    component.layered=True
    prep_component(component,outdir)    

    component = TableComponent("node_concentration",
                             [CharField("name",DSM2_NAME_LEN,16),\
                              IntField("node_no"),\
                              CharField("variable",16,16),\
                              CharField("fillin", 8,12),\
                              CharField("file",DSS_FILE_LEN,LAST_FIELD),\
                              CharField("path",80,50)
                             ],
                             ["name","variable"])   # identifier
    component.layered=True
    prep_component(component,outdir)    
    

    component = TableComponent("reservoir_concentration",
                             [CharField("name",DSM2_NAME_LEN,16),\
                              CharField("res_name",32,16),\
                              CharField("variable",16,16),\
                              CharField("fillin", 8,12),\
                              CharField("file",DSS_FILE_LEN,LAST_FIELD),\
                              CharField("path",80,50)
                              ],
                             ["name","variable"])   # identifier
    component.layered=True
    prep_component(component,outdir)        
    

    component = TableComponent("input_time_series",
                             [CharField("group_name",DSM2_NAME_LEN,16),\
                              CharField("variable",16,12),\
                              CharField("fillin", 8,12),\
                              CharField("file",DSS_FILE_LEN,32),\
                              CharField("path",80,LAST_FIELD)
                             ],
                             ["group_name","variable"])   # identifier
    component.layered=True
    prep_component(component,outdir)  
    
    
    component = TableComponent("output_channel",
                             [CharField("name",DSM2_NAME_LEN,16),
                              IntField("chan_no"),\
                              CharField("distance", 8,12),\
                              CharField("variable",16,12),\
                              CharField("interval",INTERVAL_LEN,10),\
                              CharField("period_op",PERIOD_OP_LEN,10),\
                              CharField("file",DSS_FILE_LEN,LAST_FIELD)             
                             ],
                             ["name","variable"])   # identifier
    component.layered=True
    prep_component(component,outdir)
    
    
    component = TableComponent("output_reservoir",
                             [CharField("name",DSM2_NAME_LEN,16),
                              CharField("res_name",32,16),\
                              CharField("node",8,8),\
                              CharField("variable",16,12),\
                              CharField("interval",INTERVAL_LEN,10),\
                              CharField("period_op",PERIOD_OP_LEN,10),\
                              CharField("file",DSS_FILE_LEN,LAST_FIELD)             
                             ],
                             ["name","variable"])   # identifier
    component.layered=True
    prep_component(component,outdir)    

    component = TableComponent("output_channel_source_track",
                             [CharField("name",DSM2_NAME_LEN,16),
                              IntField("chan_no"),\
                              CharField("distance", 8,12),\
                              CharField("variable",16,12),\
                              CharField("source_group",DSM2_NAME_LEN,12),\
                              CharField("interval",INTERVAL_LEN,10),\
                              CharField("period_op",PERIOD_OP_LEN,10),\
                              CharField("file",DSS_FILE_LEN,LAST_FIELD)             
                             ],
                             ["name","variable","source_group"])   # identifier
    component.layered=True
    prep_component(component,outdir)
    
    
    component = TableComponent("output_reservoir_source_track",
                             [CharField("name",DSM2_NAME_LEN,16),
                              CharField("res_name",32,16),\
                              CharField("variable",16,12),\
                              CharField("source_group",DSM2_NAME_LEN,12),\
                              CharField("interval",INTERVAL_LEN,10),\
                              CharField("period_op",PERIOD_OP_LEN,10),\
                              CharField("file",DSS_FILE_LEN,LAST_FIELD)             
                             ],
                             ["name","variable","source_group"])   # identifier
    component.layered=True
    prep_component(component,outdir)    

     
    component = TableComponent("output_gate",
                             [CharField("name",DSM2_NAME_LEN,16),
                              CharField("gate_name",32,16),\
                              CharField("device",32,16),\
                              CharField("variable",16,12),\
                              CharField("interval",INTERVAL_LEN,10),\
                              CharField("period_op",PERIOD_OP_LEN,10),\
                              CharField("file",DSS_FILE_LEN,LAST_FIELD)             
                             ],
                             ["name","variable"])   # identifier
    component.layered=True
    prep_component(component,outdir)


    component = TableComponent("suspended_sediment_type",
                             [CharField("composition",16,16)            
                             ],
                             ["composition"])   # identifier
    component.layered=True
    prep_component(component,outdir)	
	
	
    component = TableComponent("suspended_sediment_boundary",
                             [CharField("name",DSM2_NAME_LEN,16),\
                              CharField("composition",16,16),\
                              DoubleField("percent",16,6)           
                             ],
                             ["name","composition"])   # identifier
    component.layered=True
    prep_component(component,outdir)	    	
	
	
    envvar_keywords=["envvar"]
    scalar_keywords = ["scalar"]
    grid_keywords=["channel","xsect","xsect_layer","reservoir","reservoir_connection","gate",\
                   "gate_weir_device","gate_pipe_device","transfer"]
    #todo: the following additions are not really desired, and should be eliminated in 8.1
    grid_keywords_additions = ["source_flow_reservoir","input_gate","input_transfer_flow"]
    
    hydro_ic_keywords=["channel_ic","reservoir_ic"]
    oprule_keywords=["operating_rule","oprule_expression","oprule_time_series"]
    hydro_time_series_keywords = ["boundary_stage","boundary_flow","source_flow",\
                         "source_flow_reservoir","input_gate","input_transfer_flow"]  # ,"oprule_time_series"
    
    hydro_time_series_keywords_additions = ["reservoir","reservoir_connection",\
                                            "gate","gate_weir_device","gate_pipe_device","transfer"]
    
    qual_time_series_keywords = ["node_concentration",\
                         "reservoir_concentration",\
                         "input_time_series",\
                         "input_climate"]
    qual_spatial_keywords = ["rate_coefficient"]
    gtm_time_series_keywords = ["node_concentration",\
                         "reservoir_concentration",\
                         "input_time_series",\
                         "input_climate"]
    gtm_spatial_keywords = ["group_variable","group_variable_sed"]
    sediment_keywords = ["suspended_sediment_type","suspended_sediment_boundary","group_variable_sed"]
    water_body_output_keywords   =   ["output_channel","output_reservoir"]
    source_group_output_keywords = ["output_channel_source_track","output_reservoir_source_track"]
    gate_output_keywords         = ["output_gate"]
    groups_keywords   = ["group","group_member"]
    io_file_keywords  = ["io_file"]
    tidefile_keywords = ["tidefile"]
    particle_keywords = ["particle_insertion","particle_filter","particle_res_filter","particle_group_output","particle_flux_output"]
    
    
    define_text_sub("envvar",outdir)
    define_include_block("configuration", envvar_keywords)
    define_include_block("parameter",  scalar_keywords)    
    define_include_block("grid", grid_keywords+grid_keywords_additions)
    define_include_block("initial_condition", hydro_ic_keywords)
    define_include_block("hydro_time_series",hydro_time_series_keywords+hydro_time_series_keywords_additions )
    define_include_block("operation",oprule_keywords)
    define_include_block("groups",groups_keywords)
    define_include_block("qual_time_series", qual_time_series_keywords)
    define_include_block("qual_spatial",qual_spatial_keywords)
    define_include_block("gtm_time_series", gtm_time_series_keywords)
    define_include_block("gtm_spatial",gtm_spatial_keywords)	
    define_include_block("output_time_series",water_body_output_keywords\
                                             +gate_output_keywords\
                                             +source_group_output_keywords)
    define_include_block("particle",particle_keywords)
    
    # Each of these is a list of include sections that are used by the profiles below
    envvar_includes=["configuration"]
    hydro_includes=["configuration","parameter","grid",\
                    "initial_condition",\
                    "operation",\
                    "hydro_time_series",\
                    "output_time_series"]
    qual_includes=["configuration","parameter","qual_time_series",\
                   "groups","qual_spatial","output_time_series"]
    ptm_includes=["configuration","parameter","groups","particle"]
    gtm_includes=["configuration","parameter","gtm_time_series",\
                  "groups","gtm_spatial","output_time_series"]
    grid_includes=["grid"]
    
    # These are profiles. They are lists of keywords and include sections that can
    # be set active/legal within the code. For instance, if you are only processing
    # "ENVVAR" you would set the envvar profile active.
    define_profile("envvar",envvar_keywords+envvar_includes) # envvar profile: envvar + corresponding include file
    define_profile("Hydro",envvar_keywords+scalar_keywords+io_file_keywords+grid_keywords \
                          +hydro_ic_keywords+hydro_time_series_keywords+oprule_keywords+water_body_output_keywords\
                          +gate_output_keywords+hydro_includes)
    define_profile("Grid",grid_keywords+grid_keywords_additions+grid_includes) 
    define_profile("Qual",envvar_keywords+scalar_keywords+io_file_keywords+tidefile_keywords+qual_time_series_keywords\
                         +groups_keywords+qual_spatial_keywords+water_body_output_keywords\
                         +source_group_output_keywords+qual_includes)
    define_profile("PTM",envvar_keywords+scalar_keywords+io_file_keywords+tidefile_keywords\
                         +groups_keywords+particle_keywords+ptm_includes)
    define_profile("GTM",envvar_keywords+scalar_keywords+io_file_keywords+tidefile_keywords+gtm_time_series_keywords\
                         +groups_keywords+gtm_spatial_keywords+water_body_output_keywords\
                         +source_group_output_keywords+sediment_keywords+sediment_bed_keywords+gtm_includes)
    finalize(outdir)


if (__name__ == "__main__"):
    generate_dsm2()
print("Done")
os.system("pause")
