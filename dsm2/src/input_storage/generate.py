""" Example script that shows how to specify some user objects

"""

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
	
	
	
	
    define_text_sub("envvar",outdir)
    define_include_block("include", ["scalar"])

    finalize(outdir)


if (__name__ == "__main__"):
    generate_dsm2()
