""" HDF5 IO for the tidefile
This script is not meant to be regularly run. It has nothing
to do with the text input system. It was created to 
make hdf5 storage of complex attributes a little easier. If you need to
reproduce this, what you can do is move this script to a parallel directory,
then run the script, then selectively move only the item specific .h and .cpp and .fi
files and prepend dsm2_tidefile_ to the names of the cpp and header files
(admittedly the names are ridiculously long that way
"""

import sys
sys.path.append("../../../input_storage/src")
from generate_input_storage import *
LAST_FIELD=1
DSM2_NAME_LEN=32
def generate_dsm2_processed_data():
    outdir = os.path.dirname(__file__)
    component = TableComponent("qext",         # name of the table
                           [CharField("name",DSM2_NAME_LEN,32),
                           IntField("source_type"),
                           IntField("source_index"),
                           DoubleField("source_value",16,8),
                           CharField("attach_obj_name",DSM2_NAME_LEN,32),
                           IntField("attached_obj_type"),
                           IntField("attached_obj_no")],["name"])
    component.layered=True                         # Component is part of the layering system
    prep_component(component,outdir)               # Group reads/writes/clears are based on the
                                                   # the order in which they are "prepped" 

    component = TableComponent("hydro_comp_point",         # name of the table
                           [IntField("comp_index"),
                           IntField("channel"),
                           DoubleField("distance",16,8)],
                           ["comp_index"])
    component.layered=True                         # Component is part of the layering system
    prep_component(component,outdir)               # Group reads/writes/clears are based on the
                                                   # the order in which they are "prepped" 

    component = TableComponent("reservoir_node_connect",         # name of the table
                               [IntField("res_node_index"),
                               CharField("res_name",DSM2_NAME_LEN,32),
                               IntField("res_index"),                               
                               IntField("connect_index"),
                               IntField("node_no"),
                               IntField("ext_node_no"),
                               CharField("connection_type",8,8)],
                               ["res_node_index"])
    component.layered=True                         # Component is part of the layering system
    prep_component(component,outdir)               # Group reads/writes/clears are based on the
                                                   # the order in which they are "prepped" 
                                                   

    component = TableComponent("reservoir_flow_connections",         # name of the table
                               [IntField("connection_index"),
                               CharField("res_name",DSM2_NAME_LEN,32),
                               IntField("res_index"),
                               IntField("res_flow_index"),                               
                               IntField("flow_index"),
                               CharField("flow_name",DSM2_NAME_LEN,32),                               
                               CharField("flow_type",8,8)],
                               ["res_flow_index"])
    component.layered=True                         # Component is part of the layering system
    prep_component(component,outdir)               # Group reads/writes/clears are based on the
                                                   # the order in which they are "prepped" 

    component = TableComponent("node_flow_connections",         # name of the table
                               [IntField("connection_index"),
                               IntField("int_node_no"),
                               IntField("ext_node_no"),
                               IntField("node_flow_index"),
                               IntField("flow_index"),
                               CharField("flow_name",DSM2_NAME_LEN,32),                                                              
                               CharField("flow_type",8,8)],
                               ["node_flow_index"])
    component.layered=True                         # Component is part of the layering system
    prep_component(component,outdir)               # Group reads/writes/clears are based on the
                                                   # the order in which they are "prepped"
                                                   
    component = TableComponent("stage_boundaries",         # name of the table
                               [CharField("name",DSM2_NAME_LEN,32),
                               IntField("int_node_no"),
                               IntField("ext_node_no")],
                               ["name"])
    component.layered=True                         # Component is part of the layering system
    prep_component(component,outdir)               # Group reads/writes/clears are based on the
                                                   # the order in which they are "prepped"

    component = TableComponent("virtual_xsect",         # name of the table
                           [IntField("chan_no"),
						   IntField("num_virt_sec"),
                           IntField("vsecno"),
						   IntField("num_elev"),
						   DoubleField("min_elev",16,8),
						   DoubleField("elevation",16,8),
						   DoubleField("area",16,8),
						   DoubleField("wet_p",16,8),
						   DoubleField("width",16,8)],
                           ["chan_no"])
    component.layered=True                         # Component is part of the layering system
    prep_component(component,outdir)               # Group reads/writes/clears are based on the
                                                   # the order in which they are "prepped" 
												                                                   
    processed_data_keywords=["qext","hydro_comp_point","reservoir_node_connect","node__flow_connections","reservoir_flow_connections","virtual_xsect"]
 
    define_text_sub("envvar",outdir)
    define_include_block("processed_data", processed_data_keywords)
        
    # These are profiles. They are lists of keywords and include sections that can
    # be set active/legal within the code. For instance, if you are only processing
    # "ENVVAR" you would set the envvar profile active.
    define_profile("processed_data",processed_data_keywords)    
    finalize(outdir)


if (__name__ == "__main__"):
    generate_dsm2_processed_data()
