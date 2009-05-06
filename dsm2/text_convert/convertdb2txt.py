""" Script that converts DB models to equivalent text input files
    Usage >convertdb2txt.py model_name output_dir
"""

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
                                        "output_channel_concentration",\
                                        "output_reservoir_concentration",\
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
               "output_channel_concentration":outputchannelconcSQL,\
               "output_reservoir_concentration":outputresconcSQL,\
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


              
COMPONENT_MEMBERS=component_members()

def channel_ic_convert(data):
    new_data=[str(field) for field in data]
    if data[1] < -1:
        new_data[1]="length"
    return new_data

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
        
def conc_with_source_converter(row):
    new_row=trivial_convert(row)
    if (new_row[4] == "" or new_row[4] == "none"): 
        new_row[4] = "all"
        print "found it"
        print new_row
    if (new_row[3] == "" or new_row[3] == "none"): 
        new_row[3] = "all"
        print "found it"
        print new_row
    return new_row
    
    
    
CONVERTERS={"channel_ic" : channel_ic_convert,
                             "operating_rule": oprule_converter,
                             "oprule_expression": oprule_converter,
                             "group_member":group_member_converter,
                             "scalar":all_lower_converter,
                             "rate_coefficient":all_lower_converter,
                             "output_channel_concentration":conc_with_source_converter,
                             "output_reservoir_concentration":conc_with_source_converter,
                             }

def convert_table(filename,tablename,layerid):
        #print "Converting table: %s\n" % tablename
        sql=SQL[tablename]
        data=cur.execute(sql,layerid).fetchall()
        if not data or (len(data) ==0): 
            return
        fout=open(filename,"a")  # requires that the directory is initially empty or weird overwriting could result
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

def convert_layer(db_name,cur,txt_name,dest_dir,group_by="parent_table"):
    """
        Takes a parent layer like 'std_delta_grid' and a text name for the layer
        like txt_name is 'delta_090304' and creates corresponding text input
        grouped in files by parent object. So if txt_name is 'delta_090304' the output files
        would be channel_delta090304.inp,  gate_delta090304.inp, etc.
    """
    component_type=get_component_type(db_name,cur)
    txt_parent_list=INPUT_TYPE_TXT_PARENT_TABLES[component_type]
    layeridSQL="SELECT layer_id FROM layer_definition WHERE name LIKE ?"    
    layerid=cur.execute(layeridSQL,db_name).fetchone()[0]
    
    for txt_parent in txt_parent_list:
        txt_child_list=[]
        if txt_parent in TXT_CHILD_TABLES.keys():
            txt_child_list=TXT_CHILD_TABLES[txt_parent]
        layer_filename=create_dest_filename(txt_parent,txt_name)
        fname=os.path.join(dest_dir,layer_filename)
        convert_table(fname,txt_parent,layerid)
        for txt_child in txt_child_list:
             convert_table(fname,txt_child,layerid)

def file_grouping_for_table(parent_table):
    """Return the first part of the name of the input file, which may
        be the name of a table (channel) or larger group (grid)
    """
    return parent_table

def init_layer_name_translations():
    f=open("layer_translations.txt")
    layers = f.readlines()
    f.close()
    for layer in layers:
        layer=layer.strip().split(",")
        if layer and len(layer==2):
            LAYER_TRANSLATIONS[layer[0]]=layer[1]

def translate_layer_name(layer_name):
    if LAYER_NAME_TRANSLATIONS.has_key(layer_name):
        return  LAYER_NAME_TRANSLATIONS[layer_name]
    else:
        if layer_name.lower().endswith("_flow"): return layer_name[0:-5]
        if layer_name.lower().endswith("_stage"): return layer_name[0:-6]
        layer_name=layer_name.replace("std_output_hydro","std_hydro")
        layer_name=layer_name.replace("std_output_qual","std_qual")
        layer_name=layer_name.replace("concentration","conc")
        return layer_name

PREFIX = {"operating_rule" : "oprule",
                   "oprule_expression":"oprule",
                   "oprule_time_series" : "oprule",
                   "source_flow_reservoir" : "source_flow"}

LAYER_NAME_TRANSLATIONS={}

                   
def prefix_for_table(table_name):
    if PREFIX.has_key(table_name):
        return  PREFIX[table_name]
    else:
        return table_name
        
        
def create_dest_filename(parent_table, layer_name):
    # The prefix may be the name of the parent table or a bigger grouping like "op_rule"
    prefix = prefix_for_table(parent_table)
    layer_base_name = translate_layer_name(layer_name)
    return prefix+"_"+layer_base_name+".inp"
    

def get_layers_in_model(cursor, model_name):
    sql=\
"""
SELECT lay.name
FROM layer_definition lay,model_component comp,model_definition model
WHERE lay.layer_id=comp.component_id
AND model.model_id = comp.model_id
AND model.name LIKE ?
ORDER BY comp.component_type,comp.layer
"""
    data=cur.execute(sql,model_name).fetchall()
    data=[ str(mod[0]) for mod in data ]
    return data
        
def get_component_type(db_layer_name,cur):
    SQL="SELECT component_type FROM layer_definition WHERE name=?;"
    comptype=cur.execute(SQL,db_layer_name).fetchone()[0]
    return comptype

if __name__ == "__main__":
    if (len(sys.argv) < 2):
        print "Usage:\nconvertdb2txt.py model_name [dest_dir=model_name]"
    model_name = sys.argv[1]
    if (len(sys.argv) == 3):
        dest_dir = sys.argv[2]
    else:
        dest_dir = model_name
    dbcnn=DBConnect("dsm2input","dsmtwo","User2Dmin")
    cur=dbcnn.cnn.cursor()
    db_layer_names=get_layers_in_model(cur,model_name)
    # check to make sure the destination directory is empty
    existing_inp_files = [x for x in os.listdir(dest_dir) if x.endswith(".inp")]
    if len(existing_inp_files) != 0:
        raise "The destination directory must be empty of .inp files."
    # convert the layers.
    for layer in db_layer_names:
        convert_layer(layer,cur,layer,dest_dir)
    f=open("model.inp","w")
    for key in files:
        files_in_block=[os.path.split(x)[1] for x in files[key]]
        f.write(key.upper() +"\n" + string.join([os.path.split(x)[1] for x in files[key]],"\n") + "\nEND\n\n")
    f.close()
    cur.close()
    

    

