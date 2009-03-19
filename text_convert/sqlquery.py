channelSQL=\
"""SELECT channel_number,length_ft,manning,dispersion, up_node ,down_node 
FROM channel WHERE layer_id=?;"""
gateSQL=\
"""SELECT gate.name,object_type_description.name,gate.obj_connected_identifier,gate.node_connected 
FROM gate, object_type_description 
WHERE object_type_description.object_type_id=gate.obj_connected_type AND gate.layer_id=?;"""


channelicSQL=\
"""
SELECT ci1.channel_number, ci1.distance, ci1.initial_value, ci2.initial_value
FROM channel_init_condition ci1, channel_init_condition ci2
WHERE ci1.channel_number=ci2.channel_number
AND ci1.distance=ci2.distance 
AND ci1.variable_name LIKE "stage" AND ci2.variable_name LIKE "flow"
AND ci1.layer_id=? AND ci2.layer_id=ci1.layer_id
ORDER BY ci1.channel_number, ci1.distance DESC;
"""

def channel_ic_convert(data):
    new_data=[str(field) for field in data]
    if data[1] < -1:
        new_data[1]="length"
    return new_data

xsectlayerSQL=\
"""
SELECT channel.channel_number, channel_xsect.channel_fract_dist, 
xsect_layer.elev,xsect_layer.area,xsect_layer.width, xsect_layer.wet_perimeter 
FROM channel,channel_xsect,xsect_layer 
WHERE channel.layer_id=? AND channel.channel_id=channel_xsect.channel_id 
AND channel_xsect.xsect_id=xsect_layer.xsect_id ; 
"""

gatedeviceSQL=\
"""
SELECT gate.name, gate_device.name, gate_structure_description.name, 
nduplicate, max_width, base_elev, height, 
flow_coef_to_node, flow_coef_from_node, 
gate_default_op_description.name, gate_control_type_description.name 
FROM gate_device, gate, gate_structure_description, gate_control_type_description, gate_default_op_description 
WHERE gate.gate_id=gate_device.gate_id 
AND gate_device.structure_type= gate_structure_description.structure_type_id 
AND gate_device.control_type = gate_control_type_description.control_type_id  
AND gate.layer_id = ? and gate_device.default_op=gate_default_op_description.op_id 
ORDER BY gate.name, gate_device.name;"""

#######
reservoirSQL=\
"""SELECT name, area, bottom_elev 
FROM reservoir 
WHERE layer_id=?
ORDER BY name"""

reservoirconnectionSQL=\
"""
SELECT reservoir.name, reservoir_connections.connected_node_number, 
reservoir_connections.in_coef, reservoir_connections.out_coef 
FROM reservoir, reservoir_connections 
WHERE reservoir.layer_id=? 
AND reservoir_connections.reservoir_id =reservoir.reservoir_id
ORDER BY reservoir.name,reservoir_connections.connected_node_number;
"""

reservoiricSQL=\
"""
SELECT reservoir_name, initial_value
FROM reservoir_init_condition
WHERE layer_id=?
ORDER BY reservoir_name;
"""

#######
transferSQL=\
"""
SELECT t.name, obj1.name,t.from_object_identifier,obj2.name,t.to_object_identifier
FROM transfer t, object_type_description obj1, object_type_description obj2
WHERE t.from_object_type=obj1.object_type_id AND t.to_object_type=obj2.object_type_id
AND t.layer_id=?
ORDER BY t.name;
"""
#####
inputclimateSQL=""
inputtransferflowSQL=""
inputgateSQL=""
inputnodeSQL=""
boundarystageSQL="""
SELECT  input_time_series_node.name, node,input_file,path,fill_in_type_description.name
FROM input_time_series_node,fill_in_type_description
WHERE role_id=1 AND layer_id=? AND input_time_series_node.fillin = fill_in_type_description.fill_in_type_id
ORDER BY  input_time_series_node.name;
"""

boundaryflowSQL=\
"""
SELECT  input_time_series_node.name,node,sign,fill_in_type_description.name,input_file,path
FROM input_time_series_node,fill_in_type_description
WHERE role_id=2 AND layer_id=? AND input_time_series_node.fillin = fill_in_type_description.fill_in_type_id
ORDER BY input_time_series_node.name;
"""

sourceflowSQL=\
"""
SELECT input_time_series_node.name,node,sign,fill_in_type_description.name,input_file,path
FROM input_time_series_node,fill_in_type_description
WHERE role_id=4 AND layer_id=? 
AND input_time_series_node.fillin = fill_in_type_description.fill_in_type_id
ORDER BY input_time_series_node.name;
"""

#########

sourceflowreservoirSQL=\
"""
SELECT t.name,reservoir,sign,f.name,input_file,path
FROM input_time_series_reservoir t, fill_in_type_description f
WHERE role_id=4 AND layer_id=? 
AND t.fillin = f.fill_in_type_id
ORDER BY t.name,reservoir;
"""

nodeconcSQL=sourceflowSQL
reservoirconcSQL=sourceflowreservoirSQL
  
outputchannelSQL=\
"""
SELECT out.name,out.channel,distance,variable_name,time_interval,period_op,output_file
FROM output_time_series_channel out
WHERE out.layer_id=?
ORDER BY out.name,variable_name,time_interval;
"""

outputresSQL=""
outputresSQL=\
"""
SELECT out.name,out.reservoir,connection_node,variable_name,time_interval,period_op,output_file
FROM output_time_series_reservoir out
WHERE out.layer_id=?
ORDER BY out.name,variable_name,time_interval;
"""


outputchannelconcSQL=\
"""
SELECT out.name,out.channel,distance,variable_name,source_group,time_interval,period_op,output_file
FROM output_time_series_channel out
WHERE out.layer_id=?
ORDER BY out.name,variable_name,source_group,time_interval;
"""


outputresconcSQL=\
"""
SELECT out.name,out.reservoir,variable_name,source_group,time_interval,period_op,output_file
FROM output_time_series_reservoir out
WHERE out.layer_id=?
ORDER BY out.name,variable_name,source_group,time_interval;
"""

outputgateSQL=\
"""
SELECT out.name,gate,device,variable_name,time_interval,period_op,output_file
FROM output_time_series_gate out
WHERE out.layer_id=?
ORDER BY out.name,variable_name,time_interval;
"""


opruleSQL=\
"""
SELECT t.name,oprule_action,oprule_trigger
FROM operating_rule t
WHEREt.layer_id=?
ORDER BY t.name;
"""

opruleexSQL=\
"""
SELECT t.name,t.definition
FROM expression t
WHEREt.layer_id=?
ORDER BY t.name;
"""

opruletsSQL=\
"""
SELECT in.name,fill_in_type_description.name,input_file,path
FROM input_time_series_oprule in,fill_in_type_description fill
WHERE layer_id=? 
AND in.fillin = fill.fill_in_type_id
ORDER BY in.name;
"""
groupSQL=\
"""
SELECT name from groups where layer_id=?
OREDER BY name;
"""

groupmemberSQL=\
"""
SELECT groups.name,obj.name,identifier
FROM groups,group_member,object_type_description obj
WHERE layer_id=?
AND groups.group_id=group_member.group_id
AND group_member.object_type_id=obj.object_type_id
ORDER BY groups.name, obj.name, identifier;
"""

ratecoeffSQL=\
"""
SELECT group_name,const.name,ratvar.name,coefficient_value
FROM rate_coefficient rate,nc_constituent_description const,
rate_variable_description ratvar
WHERE rate.rate_variable_id=ratvar.rate_variable_id
AND rate.constituent_id=const.constituent_id
AND layer_id=?
ORDER BY group_name,const.name,ratvar.name;
"""

