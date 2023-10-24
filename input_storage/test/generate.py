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
                           [IntField("chan_no"),
                           DoubleField("manning",10,4),
                           IntField("upnode"),
                            IntField("downnode")],
                           ["chan_no"])            # chan_no is the identifier
    component.layered=True                         # Component is part of the layering system
    prep_component(component,outdir)

    # Create the child object called a channel. It is linked to a parent channel by the field
    # chan_no
    component = TableComponent("xsect",
                           [IntField("chan_no"),
                           DoubleField("dist",8,3),
                           CharField("file",120,24)], \
                           ["chan_no","dist"],
                           parent="channel",
                           parent_id=["chan_no"])
    component.layered = False
    prep_component(component,outdir)

    # Create an object called an envvar, that will be used for text substitutions
    component = TableComponent("envvar",
                           [CharField("name",32,16),\
                            CharField("value",128,16)],
                           ["name"])
    component.layered=True
    prep_component(component,outdir)

    define_include_block("include", ["channel","xsect"])
    define_text_sub("envvar",outdir)
    define_profile("envvar",["envvar"])
    define_profile("all",["envvar","channel","xsect","include"])
    finalize(outdir)




if (__name__ == "__main__"):
    do_example()
