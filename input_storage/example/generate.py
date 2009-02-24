""" Example script that shows how to specify some user objects

"""


import sys
sys.path.append("../src")
sys.path.append("../../src")
from generate_input_storage import *


## Example that creates the "channel" and "xsect" objects given in the documentation
#
def do_example():
    # Create a parent object called a channel.
    outdir = os.path.dirname(__file__)
    component = TableComponent("channel",         # name of the table
                           [IntField("chan_no"),  # integer field
                           DoubleField("manning",10,4),  # double field with width=10 precision=4  
                           IntField("upnode"),
                           IntField("downnode")],
                           ["chan_no"])            # chan_no is the identifier        
    component.layered=True                         # Component is part of the layering system
    prep_component(component,outdir)               # Group reads/writes/clears are based on the
                                                   # the order in which they are "prepped" 

    # Create the child object called a channel. It is linked to a parent channel by the field
    # chan_no
    component = TableComponent("xsect",
                           [IntField("chan_no"), 
                           DoubleField("dist",8,3),
                           CharField("file",128,24)],  # character width is allocated for 128 characters, but the printing "hint" is width=24   
                           ["chan_no","dist"],         # identifiers
                           parent="channel",         # parent table. overrides will be based on the channel table
                           parent_id=["chan_no"])    # field in the xsect that links to the parent identifier (in this case also chan_no)              
    component.layered = False
    prep_component(component,outdir)

    # Create an object called an envvar, that will be used for text substitutions
    component = TableComponent("envvar",
                           [CharField("name",32,16),\
                            CharField("value",128,16)],
                           ["name"])
    component.layered=True
    prep_component(component,outdir)


    define_text_sub("envvar",outdir)
    define_include_block("include", ["channel","xsect"])

    finalize(outdir)




if (__name__ == "__main__"):
    do_example()
