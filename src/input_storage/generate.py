""" Example script that shows how to specify some user objects

"""
DSM2_NAME_LEN=32
DSS_FILE_LEN=128
INTERVAL_LEN=16
PERIOD_OP_LEN=16

import sys
sys.path.append("d:/delta/models/input_storage/src")
from generate_input_storage import *
LAST_FIELD=1

def generate_dsm2():
    outdir = os.path.dirname(__file__)
    # Create an object called an envvar, that will be used for text substitutions
    component = TableComponent("envvar",
                           [CharField("name",32,32),\
                            CharField("value",128,LAST_FIELD)],
                           ["name"])
    component.layered=True
    prep_component(component,outdir)

    # Create an object called an envvar, that will be used for text substitutions
    component = TableComponent("scalar",
                           [CharField("name",32,32),\
                            CharField("value",32,LAST_FIELD)],
                           ["name"])
    component.layered=True
    prep_component(component,outdir)
    component = TableComponent("io_file",
	                        [CharField("model",8,8),\
							 CharField("type",8,8),\
							 CharField("io",8,8),\
							 CharField("interval",16,10),\
							 CharField("filename",128,LAST_FIELD)],
							 ["model","type","io"])
    component.layered=True
    prep_component(component,outdir)

    component = TableComponent("tidefile",
	                          [CharField("start_date",16,16),\
							  CharField("end_date",16,16),\
							  CharField("filename",128,LAST_FIELD)],
							  ["start_date"])
    component.layered=True
    prep_component(component,outdir)


    component = TableComponent("output_channel",
	                         [CharField("name",DSM2_NAME_LEN,16),
							  IntField("channel"),\
						      IntField("distance"),\
							  CharField("variable",16,12),\
						      CharField("interval",INTERVAL_LEN,10),\
                       		  CharField("period_op",PERIOD_OP_LEN,10),\
							  CharField("filename",DSS_FILE_LEN,LAST_FIELD)             
                             ],
							 ["name","variable"])   # identifier
    component.layered=True
    prep_component(component,outdir)
	
	
    component = TableComponent("output_reservoir",
	                         [CharField("name",DSM2_NAME_LEN,16),
							  CharField("reservoir",32,16),\
						      IntField("node"),\
							  CharField("variable",16,12),\
						      CharField("interval",INTERVAL_LEN,10),\
                       		  CharField("period_op",PERIOD_OP_LEN,10),\
							  CharField("filename",DSS_FILE_LEN,LAST_FIELD)             
                             ],
							 ["name","variable"])   # identifier
    component.layered=True
    prep_component(component,outdir)	


    component = TableComponent("output_gate",
	                         [CharField("name",DSM2_NAME_LEN,16),
							  CharField("gate",32,16),\
						      CharField("device",32,16),\
							  CharField("variable",16,12),\
						      CharField("interval",INTERVAL_LEN,10),\
                       		  CharField("period_op",PERIOD_OP_LEN,10),\
							  CharField("filename",DSS_FILE_LEN,LAST_FIELD)             
                             ],
							 ["name","variable"])   # identifier
    component.layered=True
    prep_component(component,outdir)


	
    define_text_sub("envvar",outdir)
    define_include_block("PARAMETER", ["scalar","envvar"])
    define_include_block("output", ["output_channel","output_reservoir","output_gate"])

    finalize(outdir)


if (__name__ == "__main__"):
    generate_dsm2()
