from dbutil import *
from sqlquery import *
from component import *


INPUT_TYPE_TXT_PARENT_TABLES={"grid":["channel","gate","reservoir","transfer"],\
                              "input":["input_climate","input_transfer_flow",\
                              "input_gate","input_node","boundary_stage",\
                              "boundary_flow","source_flow","source_flow_reservoir",\
                              "node_concentration","reservoir_concentration"],
                              "output":["output_channel","output_reservoir",\
                                        "output_channel_concentration",\
                                        "output_reservoir_concentration",\
                                        "output_gate"],\
                              "param":["envvar","scalar"],\
                              "oprule":["operating_rule","oprule_expression",\
                                        "oprule_time_series"],\
                              "group":["group"],\
                              "qual_spatial":["rate_coefficient"]}

TXT_CHILD_TABLES={"channel":["xsect_layer"],\
                  "gate":["gate_device"],\
                  "reservoir":["reservoir_connection"],\
                  }

PARENT_DB_SQL={"channel":channelSQL,\
               "gate":gateSQL,\
               "reservoir":reservoirSQL,\
               "transfer":transferSQL,\
               "input_climate":inputclimateSQL,\
               "input_transfer_flow":inputtransferflowSQL,\
               "input_gate":inputgateSQL,\
               "input_node":inputnodeSQL,\
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
               "envvar":envvarSQL,\
               "operating_rule":opruleSQL,\
               "oprule_expression":opruleexSQL,\
               "oprule_time_series":opruletsSQL,\
               "group":groupSQL,\
               "rate_coefficient":ratecoeffSQL}

CHILD_DB_SQL={"xsect_layer":xsectlayerSQL,
              "gate_device":gatedeviceSQL,
              "reservoir_connection":reservoirconnectionSQL}

COMPONENT_MEMBERS=component_members()

def convert_layer(db_name,cur,txt_name,group_by="parent_table"):
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
        fname=txt_parent+"_"+txt_name+".inp"
        fout=open(fname,"w")
        parent_SQL=PARENT_DB_SQL[txt_parent]
        data=cur.execute(parent_SQL,layerid).fetchall()
        header=COMPONENT_MEMBERS[txt_parent]
        fout.write(txt_parent.upper())
        fout.write("\n")
        for ele in header:
            fout.write(ele.upper())
            fout.write(" "*8)
        fout.write("\n")
        for row in data:
            for ele in row:
                fout.write(str(ele))
                fout.write(" "*3)
            fout.write("\n")
        fout.write("END\n")    
        for txt_child in txt_child_list:
            child_SQL=CHILD_DB_SQL[txt_child]
           
            data=cur.execute(child_SQL,layerid).fetchall()
            header=COMPONENT_MEMBERS[txt_child]
            fout.write(txt_child.upper()+"\n")
            for ele in header:
                fout.write(ele)
                fout.write(" "*8)
            fout.write("\n")
            for row in data:
                for ele in row:
                    fout.write(str(ele))
                    fout.write(" "*3)
                fout.write("\n")
            fout.write("END\n")
        fout.close()

        
def get_component_type(db_layer_name,cur):
    SQL="SELECT component_type FROM layer_definition WHERE name=?;"
    type=cur.execute(SQL,db_layer_name).fetchone()[0]
    return type

if __name__ == "__main__":
    dbcnn=DBConnect("dsm2input")
    cur=dbcnn.cnn.cursor()
    DB_LAYER_NAME=["std_delta_grid","delta_historical_stage"]
    for layer in DB_LAYER_NAME:
        convert_layer(layer,cur,layer)

cur.close()
        
    

