""" Script that converts DB models to equivalent text input files
    Usage >convertdb2txt.py model_name output_dir
"""
import dsm2_tidy
import shutil
from dbutil import *
from sqlquery import *
from component import *
import os.path
import sys

blocks=include_block()

# list of all files  grouped by block,
# which will be used to create the model.inp file
files={}

# Map of DB input types to the text tables that might contain data of this type
INPUT_TYPE_TXT_PARENT_TABLES={"grid":["channel","gate","reservoir","transfer","channel_ic","reservoir_ic"],\
                              "input":["input_climate","input_transfer_flow",\
                              "input_gate","boundary_stage",\
                              "boundary_flow","source_flow","source_flow_reservoir",\
                              "node_concentration","reservoir_concentration"],
                              "output":["output_channel","output_reservoir",\
                                        "output_channel_source_track",\
                                        "output_reservoir_source_track",\
                                        "output_gate"],\
                              "param":["scalar"],\
                              "oprule":["operating_rule","oprule_expression",\
                                        "oprule_time_series"],\
                              "group":["group"],\
                              "qual_spatial":["rate_coefficient"]}

# map of parent to child tables
TXT_CHILD_TABLES={"channel":["xsect_layer"],\
                  "gate":["gate_device"],\
                  "reservoir":["reservoir_connection"],
                  "group":["group_member"]
                }
# sql to poplulate each of the text tables
SQL={"channel":channelSQL,\
               "gate":gateSQL,\
               "reservoir":reservoirSQL,\
               "transfer":transferSQL,\
               "channel_ic":channelicSQL,\
               "reservoir_ic":reservoiricSQL,\
               "input_climate":inputclimateSQL,\
               "input_transfer_flow":inputtransferflowSQL,\
               "input_gate":inputgateSQL,\
               "scalar":paramSQL,\
               "boundary_stage":boundarystageSQL,\
               "boundary_flow":boundaryflowSQL,\
               "source_flow":sourceflowSQL,\
               "source_flow_reservoir":sourceflowreservoirSQL,\
               "node_concentration":nodeconcSQL,\
               "reservoir_concentration":reservoirconcSQL,\
               "output_channel":outputchannelSQL,\
               "output_reservoir":outputresSQL,\
               "output_channel_source_track":outputchannelconcSQL,\
               "output_reservoir_source_track":outputresconcSQL,\
               "output_gate":outputgateSQL,\
               "operating_rule":opruleSQL,\
               "oprule_expression":opruleexSQL,\
               "oprule_time_series":opruletsSQL,\
               "group":groupSQL,\
               "rate_coefficient":ratecoeffSQL,\
               "xsect_layer":xsectlayerSQL,
              "gate_device":gatedeviceSQL,
              "reservoir_connection":reservoirconnectionSQL,
              "group_member":groupmemberSQL}

LAYER_NAME_TRANSLATIONS={}

COMPONENT_MEMBERS=component_members()

def channel_ic_convert(data):
    new_data=[str(field) for field in data]
    if data[1] < -1:
        new_data[1]="length"
    return new_data


####  Converters: These are functions that convert a row from
#     database fields to acceptable text fields. Nearly all of them start
#     by calling trivial_convert, which converts the fields to strings and
#     replaces NULLs with the word "none". The rest of the converters
#     are special cases and particular tables/groups of tables
def trivial_convert(row):
    new_row=[str(field) for field in row]
    new_row=[field.replace("None","none") for field in new_row]
    #new_row=[field.replace("(","{") for field in new_row] #cause problem in oprule which accepts ( not {
    #new_row=[field.replace(")","}") for field in new_row] #cause problem in oprule which accepts ( not {
    #new_row=[field.replace("EC ","ec ") for field in new_row]
    #new_row=[field.replace("DOC ","doc ") for field in new_row]
    return new_row

def quote_string(field):
    if (field.find(" ") >= 0):
        return "\"%s\"" % field
    else:
        return field

def quote_string_convert(row):
    new_row=trivial_convert(row)
    new_row=[quote_string(field) for field in new_row]
    return new_row

def quote_string_drop_interior_quote_converter(row):
    new_row=trivial_convert(row)
    new_row=[field.replace("\"","") for field in new_row]
    new_row=[field.replace("\'","") for field in new_row]
    new_row=[quote_string(field) for field in new_row]
    return new_row

def oprule_converter(row):
    new_row=trivial_convert(row)
    new_row=[field.replace("\"","") for field in new_row]
    new_row=[field.replace("\'","") for field in new_row]
    new_row=[quote_string(field) for field in new_row]
    new_row[0] = str(row[0])
    return new_row

def all_lower_converter(row):
    new_row=trivial_convert(row)
    new_row=[field.lower() for field in new_row]
    return new_row


def group_member_converter(row):
    new_row=trivial_convert(row)
    obj_type_mappings={"Boundary Stage":"stage",\
                       "Boundary Flow" :"flow_boundary",\
                       "Source/Sink Flow": "source_sink"}
    if obj_type_mappings.has_key(new_row[1]):
        new_row[1]=obj_type_mappings[new_row[1]]
    new_row=[field.lower() for field in new_row]
    return new_row

def chan_conc_with_source_converter(row):
    new_row=trivial_convert(row)
    if (new_row[4] == "" or new_row[4] == "none"):
        new_row[4] = "all"
    if new_row[0].endswith("_"+new_row[4]):
        new_row[0]=new_row[0][:-(len(new_row[4])+1)]  # remove source from name
    return new_row

def res_conc_with_source_converter(row):
    new_row=trivial_convert(row)
    if (new_row[3] == "" or new_row[3] == "none"):
        new_row[3] = "all"
    if new_row[0].endswith("_"+new_row[3]):
        new_row[0]=new_row[0][:-(len(new_row[3])+1)]  # remove source from name
    return new_row


def chan_output_no_source_converter(row):
    new_row=trivial_convert(row)
    if new_row[2].startswith("-9999"): new_row[2]="length"
    return new_row[:4] + new_row[5:]

def res_output_no_source_converter(row):
    new_row=trivial_convert(row)
    return new_row[:4] + new_row[5:]

def gate_converter(row):
    new_row=trivial_convert(row)
    new_row[1]=new_row[1].lower()
    return new_row

def input_with_sign_converter(row):
    """Converts a row with a NULL sign in column 2 to a sign of 1"""
    new_row=trivial_convert(row)
    if new_row[2] == "none":
        new_row[2]="1"
    return new_row


# This is a list of the converters used for tables that use special converters
# to convert their rows to text.
# If a table does not appear in the keys of this list,
# only trivial_convert will be used on the table
CONVERTERS={"channel_ic" : channel_ic_convert,
            "gate":gate_converter,
            "operating_rule": oprule_converter,
            "oprule_expression": oprule_converter,
            "group_member":group_member_converter,
            "scalar":all_lower_converter,
            "rate_coefficient":all_lower_converter,
            "boundary_flow":input_with_sign_converter,
            "source_flow":input_with_sign_converter,
            "output_channel_source_track":chan_conc_with_source_converter,
            "output_reservoir_source_track":res_conc_with_source_converter,
            "output_channel":chan_output_no_source_converter,
            "output_reservoir":res_output_no_source_converter
            }

def exclude_table(filename,tablename,data):
    #print "%s (%s)" %(tablename,len(data))
    non_specified_source=["all","none","None","",None]
    if tablename == "output_channel":
        source_rows=[row for row in data if not row[4] in non_specified_source]
        return len(source_rows) != 0
    if tablename == "output_reservoir":
        source_rows=[row for row in data if not row[4] in non_specified_source]
        return len(source_rows) != 0
    if tablename == "output_channel_source_track":
        source_rows=[row for row in data if not row[4] in non_specified_source]
        return len(source_rows) == 0
    if tablename == "output_reservoir_source_track":
        source_rows=[row for row in data if not row[3] in non_specified_source]
        return len(source_rows) == 0
    return False

def convert_table(filename,tablename,layerid,description=None):
        sql=SQL[tablename]
        data=cur.execute(sql,layerid).fetchall()

        if exclude_table(filename,tablename,data):
            return
        if not data or (len(data) ==0):
            return

        file_exists = os.path.exists(filename)   # This means we are appending to a file we already wrote once

        fout=open(filename,"a")  # requires that the directory is initially empty or weird overwriting could result

        if not file_exists and description:
            #write the description of the layer
            fout.write("# Description:\n# %s\n\n" % description)

        has_used_column = (sql.find("used") > 6 and sql.find("used") < sql.find("FROM"))
        header=COMPONENT_MEMBERS[tablename]
        fout.write(tablename.upper())
        if CONVERTERS.has_key(tablename):
            converter=CONVERTERS[tablename]
        else:
            converter=trivial_convert
        headertxt=string.join(header,"        ").upper()
        fout.write("\n%s\n" % headertxt)

        for row in data:
            is_used = True
            if (has_used_column):
                is_used = row[-1]
                row=row[:-1]
            datastr=converter(row)
            rowtxt=string.join(datastr,"        ")
            if not is_used:
                rowtxt="^"+rowtxt
            fout.write("%s\n" % rowtxt)
        fout.write("END\n\n##\n")
        fout.close()
        block=blocks[tablename]
        if files.has_key(block):
            if not filename in files[block]:
                files[block].append(filename)
        else:
            files[block]=[filename]

def convert_layer(db_name,cur,txt_name,dest_dir,suffix=None):
    """
        Takes a parent layer like 'std_delta_grid' and a text name for the layer
        like txt_name is 'delta_090304' and creates corresponding text input
        grouped in files by parent object. So if txt_name is 'delta_090304' the output files
        would be channel_delta090304.inp,  gate_delta090304.inp, etc.
    """
    component_type=get_component_type(db_name,cur)
    txt_parent_list=INPUT_TYPE_TXT_PARENT_TABLES[component_type]
    layeridSQL="SELECT layer_id,description FROM layer_definition WHERE name LIKE ?"
    layerid,description=cur.execute(layeridSQL,db_name).fetchone()

    for txt_parent in txt_parent_list:
        txt_child_list=[]
        if txt_parent in TXT_CHILD_TABLES.keys():
            txt_child_list=TXT_CHILD_TABLES[txt_parent]
        layer_filename=create_dest_filename(txt_parent,txt_name,suffix)
        fname=os.path.join(dest_dir,layer_filename)
        convert_table(fname,txt_parent,layerid,description)
        for txt_child in txt_child_list:
             convert_table(fname,txt_child,layerid)

def file_grouping_for_table(parent_table):
    """Return the first part of the name of the input file, which may
        be the name of a table (channel) or larger group (grid)
    """
    return parent_table

def init_layer_name_translations():
    f=open("layer_name_translations.txt")
    layers = f.readlines()
    f.close()
    for layer in layers:
        layer=layer.strip().split(",")
        if layer and len(layer)==2:
            LAYER_NAME_TRANSLATIONS[layer[0]]=layer[1]

def translate_layer_name(prefix,layer_name):
    if LAYER_NAME_TRANSLATIONS.has_key(layer_name):
        return  LAYER_NAME_TRANSLATIONS[layer_name]
    else:
        if layer_name.lower().endswith("_flow"): return layer_name[0:-5]
        if layer_name.lower().endswith("_stage"): return layer_name[0:-6]
        if prefix.startswith("output") and layer_name.lower().endswith("_output"):
            layer_name=layer_name[:-7]
        if prefix.startswith("op") and layer_name.lower().endswith("_ops"):
            layer_name=layer_name[:-4]
        if prefix.startswith("op") and layer_name.lower().endswith("_op"):
            layer_name=layer_name[:-3]
        layer_name=layer_name.replace("std_output_hydro","std_hydro")
        layer_name=layer_name.replace("std_output_qual","std_qual")
        layer_name=layer_name.replace("concentration","conc")
        layer_name=layer_name.replace("out_res_","")
        layer_name=layer_name.replace("out_chann_","")


        return layer_name

PREFIX = {"operating_rule" : "oprule",
                   "oprule_expression":"oprule",
                   "oprule_time_series" : "oprule",
                   "source_flow_reservoir" : "source_flow"}




def prefix_for_table(table_name):
    if PREFIX.has_key(table_name):
        return  PREFIX[table_name]
    else:
        return table_name


def create_dest_filename(parent_table, layer_name,suffix=None):
    # The prefix may be the name of the parent table or a bigger grouping like "op_rule"
    prefix = prefix_for_table(parent_table)
    layer_base_name = translate_layer_name(prefix,layer_name)
    if suffix:
	    suffix="_"+suffix
    else:
	    suffix = ""
    return prefix+"_"+layer_base_name+suffix+".inp"



def get_layers_in_model(cursor, model_name):
    sql=\
"""
SELECT lay.name,comp.component_type,model.computer_model
FROM layer_definition lay,model_component comp,model_definition model
WHERE lay.layer_id=comp.component_id
AND model.model_id = comp.model_id
AND model.name LIKE ?
ORDER BY comp.component_type,comp.layer
"""

    data=cur.execute(sql,model_name).fetchall()
    data=[ str(mod[0]) for mod in data if not((mod[1]=="grid" and mod[2]=="qual")\
                                           or (mod[1]=="group" and mod[2]=="hydro"))]
    return data


def get_component_type(db_layer_name,cur):
    SQL="SELECT component_type FROM layer_definition WHERE name=?;"
    comptype=cur.execute(SQL,db_layer_name).fetchone()[0]
    return comptype

def create_top_level_model_file(fname):
    """
    Create model.inp
    This is the top file that will probably end up being called
    something like hydro.inp
    """

    blocks = files.keys() # list of include block names
    if len(blocks) == 0:
        print "Not creating top level file because there were no files produced"
        return
    incl_order=include_block_order()  # order of blocks (this is the order they were defined by define_include_block() in generate.py)
    # sort them according to the order
    blocks.sort(lambda x,y: cmp(incl_order.index(x),incl_order.index(y)))
    f=open(fname,"w")
    for key in blocks:
        files_in_block=["${DSM2INPUTDIR}/"+os.path.split(x)[1] for x in files[key]]  #  list of files in include block
        f.write(key.upper() +"\n" + string.join(files_in_block,"\n") + "\nEND\n\n")
    f.close()

def tidy_files():
    blocks = files.keys()
    for key in blocks:
        files_in_block=files[key]
        for f in files_in_block:
            dsm2_tidy.tidy(f,f+".tmp")
            shutil.move(f+".tmp",f)


if __name__ == "__main__":
    init_layer_name_translations()
    if (len(sys.argv) < 2):
        print "Usage:\nconvertdb2txt.py model_name [dest_dir=model_name]"
    model_name = sys.argv[1]
    if len(sys.argv) >=4:
	    suffix = sys.argv[3]
    else:
	    suffix = ""
    if (len(sys.argv) >= 3):
        dest_dir = sys.argv[2]
    else:
        dest_dir = model_name
    dbcnn=DBConnect("dsm2input","dsmtwo","User2Dmin")
    # Uncomment for MS Access
    #dbcnn=DBConnect("dsm2_tutorial","Admin","blay")
    cur=dbcnn.cnn.cursor()
    db_layer_names=get_layers_in_model(cur,model_name)
    for layer in db_layer_names:
        print layer
    # check to make sure the destination directory is empty
    existing_inp_files = [x for x in os.listdir(dest_dir) if x.endswith(".inp")]
    if len(existing_inp_files) != 0:
        raise "The destination directory must be empty of .inp files."
    # convert the layers.
    for layer in db_layer_names:
        convert_layer(layer,cur,layer,dest_dir,suffix)

    # now create the top level input file
    cur.close()
    create_top_level_model_file(os.path.join(dest_dir,"model.inp"))

    inp_files = [x for x in os.listdir(dest_dir) if x.endswith(".inp")]
    if len(inp_files) != 0:
        tidy_files()


