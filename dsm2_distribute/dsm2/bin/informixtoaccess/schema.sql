CREATE SEQUENCE gen_permission_id INCREMENT BY 1 START WITH 0
 MINVALUE 0;

CREATE SEQUENCE global_gen INCREMENT BY 1 START WITH 2018448
 MINVALUE 2018448;

CREATE SEQUENCE temp_seq INCREMENT BY 1 START WITH 2301601
 MINVALUE 2301601;

CREATE SEQUENCE test_gen INCREMENT BY 1 START WITH 910658
 MINVALUE 910658;

create table channel (
    channel_id INT not null,
    layer_id INT not null,
    channel_number INT not null,
    length_ft INT not null,
    manning FLOAT not null,
    dispersion FLOAT not null,
    down_node INT not null,
    up_node INT not null,
    used INT not null
)
extent size 32 next size 32
lock mode page;

create table channel_init_condition (
    icid INT not null,
    channel_number INT not null,
    distance INT not null,
    layer_id INT not null,
    variable_name VARCHAR(16) not null,
    initial_value FLOAT not null,
    used INT not null
)
extent size 32 next size 32
lock mode page;

create table channel_xsect (
    xsect_id INT not null,
    channel_id INT not null,
    channel_fract_dist FLOAT not null,
    copied_from INT,
    description VARCHAR(32)
)
extent size 32 next size 32
lock mode page;

create table climate_variable_description (
    climate_variable_id INT not null,
    name VARCHAR(32) not null,
    description VARCHAR(64)
)
extent size 32 next size 32
lock mode page;

create table component_type_description (
    component_type_id INT not null,
    name VARCHAR(16) not null,
    description VARCHAR(64)
)
extent size 32 next size 32
lock mode page;

create table expression (
    expression_id INT not null,
    layer_id INT not null,
    name VARCHAR(32) not null,
    definition VARCHAR(200) not null
)
extent size 32 next size 32
lock mode page;

create table fill_in_type_description (
    fill_in_type_id INT not null,
    name VARCHAR(16) not null,
    description VARCHAR(255)
)
extent size 32 next size 32
lock mode page;

create table gate (
    gate_id INT not null,
    layer_id INT not null,
    name VARCHAR(32) not null,
    obj_connected_type INT not null,
    obj_connected_identifier VARCHAR(32) not null,
    node_connected INT not null,
    used INT not null
)
extent size 32 next size 32
lock mode page;

create table gate_control_type_description (
    control_type_id INT not null,
    name VARCHAR(32) not null,
    description VARCHAR(255)
)
extent size 32 next size 32
lock mode page;

create table gate_default_op_description (
    op_id INT not null,
    name VARCHAR(32) not null,
    description VARCHAR(255)
)
extent size 32 next size 32
lock mode page;

create table gate_device (
    device_id INT not null,
    gate_id INT not null,
    name VARCHAR(32) not null,
    structure_type INT not null,
    control_type INT not null,
    nduplicate INT not null,
    max_width FLOAT not null,
    base_elev FLOAT not null,
    height FLOAT not null,
    flow_coef_from_node FLOAT not null,
    flow_coef_to_node FLOAT not null,
    default_op INT not null
)
extent size 32 next size 32
lock mode page;

create table gate_structure_description (
    structure_type_id INT not null,
    name VARCHAR(16) not null,
    description VARCHAR(255)
)
extent size 32 next size 32
lock mode page;

create table group_member (
    group_member_id INT not null,
    group_id INT not null,
    object_type_id INT not null,
    identifier VARCHAR(32) not null
)
extent size 32 next size 32
lock mode page;

create table groups (
    group_id INT not null,
    layer_id INT not null,
    name VARCHAR(32) not null,
    description VARCHAR(255),
    used INT not null
)
extent size 32 next size 32
lock mode page;

create table input_role_description (
    role_id INT not null,
    name VARCHAR(32) not null
)
extent size 32 next size 32
lock mode page;

create table input_time_series_climate (
    input_series_id INT not null,
    layer_id INT not null,
    climate_variable_id INT not null,
    input_file VARCHAR(128) not null,
    path VARCHAR(80) not null,
    sign INT,
    fillin INT not null
)
extent size 32 next size 32
lock mode page;

create table input_time_series_gate (
    input_series_id INT not null,
    layer_id INT not null,
    gate VARCHAR(32) not null,
    device VARCHAR(32),
    input_file VARCHAR(128) not null,
    path VARCHAR(80) not null,
    variable_name VARCHAR(16) not null,
    fillin INT not null,
    used INT not null
)
extent size 32 next size 32
lock mode page;

create table input_time_series_node (
    input_series_id INT not null,
    name VARCHAR(32) not null,
    node INT not null,
    role_id INT not null,
    layer_id INT not null,
    variable_name VARCHAR(16) not null,
    input_file VARCHAR(128) not null,
    path VARCHAR(80) not null,
    sign INT,
    fillin INT not null,
    used INT not null
)
extent size 32 next size 32
lock mode page;

create table input_time_series_oprule (
    input_series_id INT not null,
    layer_id INT not null,
    name VARCHAR(32) not null,
    input_file VARCHAR(128) not null,
    path VARCHAR(80) not null,
    sign INT,
    fillin INT not null
)
extent size 32 next size 32
lock mode page;

create table input_time_series_reservoir (
    input_series_id INT not null,
    name VARCHAR(32) not null,
    reservoir VARCHAR(32) not null,
    layer_id INT not null,
    variable_name VARCHAR(16) not null,
    input_file VARCHAR(128) not null,
    role_id INT not null,
    path VARCHAR(80) not null,
    sign INT,
    fillin INT not null,
    used INT not null
)
extent size 32 next size 32
lock mode page;

create table input_time_series_transfer (
    input_series_id INT not null,
    layer_id INT not null,
    transfer VARCHAR(32) not null,
    input_file VARCHAR(128) not null,
    path VARCHAR(80) not null,
    variable_name VARCHAR(16) not null,
    fillin INT not null,
    used INT not null
)
extent size 32 next size 32
lock mode page;

create table layer_definition (
    layer_id INT not null,
    name VARCHAR(32) not null,
    description VARCHAR(255),
    owner VARCHAR(16) not null,
    creation_date DATETIME YEAR TO MINUTE not null,
    component_type VARCHAR(16) not null
)
extent size 32 next size 32
lock mode page;

create table model_component (
    model_component_id INT not null,
    model_id INT not null,
    component_id INT not null,
    component_type VARCHAR(16) not null,
    layer INT not null
)
extent size 32 next size 32
lock mode page;

create table model_definition (
    model_id INT not null,
    name VARCHAR(48) not null,
    computer_model VARCHAR(32) not null,
    description VARCHAR(255),
    simulation_id INT not null,
    owner VARCHAR(32) not null,
    creation_date DATETIME YEAR TO MINUTE not null
)
extent size 32 next size 32
lock mode page;

create table model_parameter_description (
    model_parameter_id INT not null,
    name VARCHAR(32) not null,
    description VARCHAR(255)
)
extent size 32 next size 32
lock mode page;

create table model_parameter_values (
    param_value_id INT not null,
    model_parameter_id INT not null,
    layer_id INT not null,
    parameter_value VARCHAR(32) not null
)
extent size 32 next size 32
lock mode page;

create table nc_constituent_description (
    constituent_id INT not null,
    name VARCHAR(32) not null,
    description VARCHAR(255) not null
)
extent size 32 next size 32
lock mode page;

create table object_type_description (
    object_type_id INT not null,
    name VARCHAR(16) not null
)
extent size 32 next size 32
lock mode page;

create table operating_rule (
    oprule_id INT not null,
    layer_id INT not null,
    name VARCHAR(32) not null,
    oprule_action VARCHAR(255) not null,
    oprule_trigger VARCHAR(255) not null,
    used INT not null
)
extent size 32 next size 32
lock mode page;

create table output_time_series_channel (
    out_id INT not null,
    layer_id INT not null,
    name VARCHAR(32) not null,
    channel INT not null,
    distance INT not null,
    variable_name VARCHAR(16) not null,
    output_file VARCHAR(128) not null,
    time_interval VARCHAR(16) not null,
    period_op VARCHAR(16) not null,
    used INT not null,
    source_group VARCHAR(20)
)
extent size 32 next size 32
lock mode page;

create table output_time_series_gate (
    out_id INT not null,
    name VARCHAR(32) not null,
    gate VARCHAR(32) not null,
    device VARCHAR(32),
    layer_id INT not null,
    output_file VARCHAR(128) not null,
    variable_name VARCHAR(16) not null,
    time_interval VARCHAR(16) not null,
    period_op VARCHAR(16) not null,
    used INT not null
)
extent size 32 next size 32
lock mode page;

create table output_time_series_node (
    out_id INT not null,
    name VARCHAR(32) not null,
    node INT not null,
    layer_id INT not null,
    output_file VARCHAR(128) not null,
    variable_name VARCHAR(16) not null,
    time_interval VARCHAR(16) not null,
    period_op VARCHAR(16) not null,
    used INT not null,
    source_group VARCHAR(32)
)
extent size 32 next size 32
lock mode page;

create table output_time_series_reservoir (
    out_id INT not null,
    name VARCHAR(32) not null,
    reservoir VARCHAR(32) not null,
    connection_node INT,
    layer_id INT not null,
    output_file VARCHAR(128) not null,
    variable_name VARCHAR(16) not null,
    time_interval VARCHAR(16) not null,
    period_op VARCHAR(16) not null,
    used INT not null,
    source_group VARCHAR(32)
)
extent size 32 next size 32
lock mode page;

create table period_op_description (
    period_op_id INT not null,
    name VARCHAR(16) not null,
    description VARCHAR(255)
)
extent size 32 next size 32
lock mode page;

create table permissions (
    permission_id INT not null,
    user_name VARCHAR(32) not null,
    component_set_id INT not null,
    expiration_date DATE
)
extent size 32 next size 32
lock mode page;

create table primary_key_generation (
    id_id INT not null,
    new_id INT
)
extent size 32 next size 32
lock mode page;

create table rate_coefficient (
    rate_coefficient_id INT not null,
    layer_id INT not null,
    group_name VARCHAR(32) not null,
    rate_variable_id INT not null,
    constituent_id INT not null,
    coefficient_value FLOAT not null
)
extent size 32 next size 32
lock mode page;

create table rate_variable_description (
    rate_variable_id INT not null,
    name VARCHAR(32) not null,
    description VARCHAR(255) not null
)
extent size 32 next size 32
lock mode page;

create table reservoir (
    reservoir_id INT not null,
    layer_id INT not null,
    name VARCHAR(32) not null,
    area FLOAT not null,
    bottom_elev FLOAT not null,
    used INT not null
)
extent size 32 next size 32
lock mode page;

create table reservoir_connections (
    connection_id INT not null,
    reservoir_id INT not null,
    connected_node_number INT not null,
    in_coef FLOAT not null,
    out_coef FLOAT not null
)
extent size 32 next size 32
lock mode page;

create table reservoir_init_condition (
    icid INT not null,
    reservoir_name VARCHAR(32) not null,
    layer_id INT not null,
    variable_name VARCHAR(16) not null,
    initial_value FLOAT not null,
    used INT not null
)
extent size 32 next size 32
lock mode page;

create table simulation_definition (
    simulation_id INT not null,
    name VARCHAR(32) not null,
    description VARCHAR(255),
    owner VARCHAR(32) not null,
    creation_date DATETIME YEAR TO MINUTE default CURRENT YEAR TO MINUTE not null
)
extent size 32 next size 32
lock mode page;

create table transfer (
    transfer_id INT not null,
    layer_id INT not null,
    name VARCHAR(32) not null,
    from_object_type INT not null,
    from_object_identifier VARCHAR(32) not null,
    to_object_type INT not null,
    to_object_identifier VARCHAR(32) not null,
    used INT not null
)
extent size 32 next size 32
lock mode page;

create table xsect_layer (
    xsect_layer_id INT not null,
    xsect_id INT not null,
    elev FLOAT not null,
    width FLOAT not null,
    area FLOAT not null,
    wet_perimeter FLOAT not null
)
extent size 32 next size 32
lock mode page;

create view delete_layers (layer_id,name,description,owner,creation_date,
       component_type) as
   select x0.layer_id ,x0.name ,x0.description ,x0.owner ,x0.creation_date ,
       x0.component_type 
   from "informix".layer_definition x0 
   where (x0.layer_id != ALL (
      select x1.component_id 
      from "informix".model_component x1 
    ) ) ;
create view dsm2_channel_in_model (layer,channel_id,used,channel_number,
       length_ft,manning,dispersion,down_node,up_node,model_id) as
   select x1.layer ,x0.channel_id ,x0.used ,x0.channel_number ,
       x0.length_ft ,x0.manning ,x0.dispersion ,x0.down_node ,
       x0.up_node ,x1.model_id 
   from "informix".channel x0 ,"informix".model_component x1 
   where (((x0.layer_id = x1.component_id ) AND (x0.used != 0 ) ) AND (x1.layer >= ALL (
      select x3.layer 
      from "informix".channel x2 ,"informix".model_component x3 
      where (((x2.layer_id = x3.component_id ) AND (x3.model_id = x1.model_id ) ) AND (x2.channel_number = x0.channel_number ) ) 
    ) ) ) ;
create view dsm2_gate_device (gate,device,layer_id,model_id) as
   select x0.name ,x1.name ,x0.layer_id ,x2.model_id 
   from "informix".gate x0 ,"informix".gate_device x1 ,"informix".model_component x2 
   where ((x0.layer_id = x2.component_id ) AND (x0.gate_id = x1.gate_id ) )  union select x3.name ,
          NULL ::varchar(32) ,x3.layer_id ,x4.model_id 
      from "informix".gate x3 ,"informix".model_component x4 
      where (x3.layer_id = x4.component_id ) ;
create view dsm2_input_time_series (input_series_id,used,input_name,
       object_type_id,loc_name,loc_num,sub_loc,path,variable_name,
       sign,fillin,role_id,role_name,input_file,layer,model_id) as
   select x0.input_series_id ,x0.used ,x0.name ,2 ,x0.node ::varchar(32) ,
       x0.node ,'' ::varchar(32) ,x0.path ,x0.variable_name ,
       x0.sign ,x0.fillin ,x0.role_id ,x1.name ,x0.input_file ,
       x2.layer ,x2.model_id 
   from "informix".input_time_series_node x0 ,"informix".input_role_description x1 ,"informix".model_component x2 
   where (((x0.role_id = x1.role_id ) AND (x0.layer_id = x2.component_id ) ) AND (UPPER(x2.component_type ) = 'INPUT' ) )  union select x3.input_series_id ,
          1 ,x3.name ,111 ,x3.name ,0 ,'' ::varchar(32) ,x3.path ,
          'TS_VALUE' ::varchar(16) ,x3.sign ,x3.fillin ,x4.role_id ,
          x4.name ::varchar(32) ,x3.input_file ,x5.layer ,x5.model_id 
      from "informix".input_time_series_oprule x3 ,"informix".input_role_description x4 ,"informix".model_component x5 
      where (((x4.name = 'oprule' ) AND (x3.layer_id = x5.component_id ) ) AND (UPPER(x5.component_type ) = 'OPRULE' ) )  union select x6.input_series_id ,
             x6.used ,x6.name ,3 ,x6.reservoir ,0 ,'' ::varchar(32) ,
             x6.path ,x6.variable_name ,x6.sign ,x6.fillin ,4 ,
             'SOURCE-SINK' ::varchar(32) ,x6.input_file ,x8.layer ,
             x8.model_id 
         from "informix".input_time_series_reservoir x6 ,"informix".input_role_description x7 ,"informix".model_component x8 
         where (((x6.role_id = x7.role_id ) AND (x6.layer_id = x8.component_id ) ) AND (UPPER(x8.component_type ) = 'INPUT' ) )  union select x9.input_series_id ,
                x9.used ,x9.transfer ,6 ,x9.transfer ,0 ,'' ::varchar(32) ,
                x9.path ,x9.variable_name ,NULL ::integer ,x9.fillin ,
                18 ,'TRANSFER' ::varchar(32) ,x9.input_file ,
                x10.layer ,x10.model_id 
            from "informix".input_time_series_transfer x9 ,"informix".model_component x10 ,"informix".input_role_description x11 
            where (((x9.layer_id = x10.component_id ) AND (UPPER(x10.component_type ) = 'INPUT' ) ) AND (x11.role_id = x11.role_id ) )  union select x12.input_series_id ,
                   x12.used ,((SUBSTR (x12.gate ,1 ,23 )|| '-' ) || SUBSTR (x12.device ,1 ,8 )) ::varchar(32) ,
                   4 ,x12.gate ,0 ,x12.device ,x12.path ,x12.variable_name ,
                   NULL ::integer ,x12.fillin ,17 ,'GATE-OP' ::varchar(32) ,
                   x12.input_file ,x13.layer ,x13.model_id 
               from "informix".input_time_series_gate x12 ,"informix".model_component x13 ,"informix".input_role_description x14 
               where (((x12.layer_id = x13.component_id ) AND (UPPER(x13.component_type ) = 'INPUT' ) ) AND (x14.role_id = x14.role_id ) )  union select x15.input_series_id ,
                      1 ,x18.name ,30 ,'global' ::varchar(32) ,
                      0 ,'' ::varchar(32) ,x15.path ,x18.name ::varchar(16) ,
                      x15.sign ,x15.fillin ,x16.role_id ,x16.name ::varchar(32) ,
                      x15.input_file ,x17.layer ,x17.model_id 
                  from "informix".input_time_series_climate x15 ,"informix".input_role_description x16 ,"informix".model_component x17 ,"informix".climate_variable_description x18 
                  where ((((x16.name = 'climate' ) AND (x15.layer_id = x17.component_id ) ) AND (UPPER(x17.component_type ) = 'INPUT' ) ) AND (x18.climate_variable_id = x15.climate_variable_id ) ) ;
create view dsm2_output_time_series (out_id,name,object_type,
       loc_name,loc_number,sub_loc,used,variable_name,time_interval,
       period_op,source_group,output_file,layer,model_id) as
   select x0.out_id ,x0.name ,4 ,x0.gate ,0 ,x0.device ,x0.used ,
       x0.variable_name ,x0.time_interval ,x0.period_op ,NULL ::varchar(32) ,
       x0.output_file ,x1.layer ,x1.model_id 
   from "informix".output_time_series_gate x0 ,"informix".model_component x1 
   where ((x0.layer_id = x1.component_id ) AND (UPPER(x1.component_type ) = 'OUTPUT' ) )  union select x2.out_id ,
          x2.name ,2 ,NULL ::varchar(32) ,x2.node ,NULL ::varchar(32) ,
          x2.used ,x2.variable_name ,x2.time_interval ,x2.period_op ,
          x2.source_group ,x2.output_file ,x3.layer ,x3.model_id 
      from "informix".output_time_series_node x2 ,"informix".model_component x3 
      where ((x2.layer_id = x3.component_id ) AND (UPPER(x3.component_type ) = 'OUTPUT' ) )  union select x4.out_id ,
             x4.name ,1 ,NULL ::varchar(32) ,x4.channel ,x4.distance ::varchar(32) ,
             x4.used ,x4.variable_name ,x4.time_interval ,x4.period_op ,
             x4.source_group ,x4.output_file ,x5.layer ,x5.model_id 
         from "informix".output_time_series_channel x4 ,"informix".model_component x5 
         where ((x4.layer_id = x5.component_id ) AND (UPPER(x5.component_type ) = 'OUTPUT' ) )  union select x6.out_id ,
                x6.name ,3 ,x6.reservoir ,0 ,x6.connection_node ::varchar(32) ,
                x6.used ,x6.variable_name ,x6.time_interval ,
                x6.period_op ,x6.source_group ,x6.output_file ,
                x7.layer ,x7.model_id 
            from "informix".output_time_series_reservoir x6 ,"informix".model_component x7 
            where ((x6.layer_id = x7.component_id ) AND (UPPER(x7.component_type ) = 'OUTPUT' ) ) ;
create view dsm2_reservoir_node_in_model (type_name,type_id,name,
       model_id) as
   select x2.name ,x2.object_type_id ,x0.name ,x1.model_id 
   from "informix".reservoir x0 ,"informix".model_component x1 ,"informix".object_type_description x2 
   where ((x0.layer_id = x1.component_id ) AND (x2.object_type_id = 3 ) )  union select x5.name ,
          x5.object_type_id ,x3.up_node ::varchar(32) ,x4.model_id 
      from "informix".channel x3 ,"informix".model_component x4 ,"informix".object_type_description x5 
      where ((x3.layer_id = x4.component_id ) AND (x5.object_type_id = 2 ) )  union select x8.name ,
             x8.object_type_id ,x6.down_node ::varchar(32) ,x7.model_id 
         from "informix".channel x6 ,"informix".model_component x7 ,"informix".object_type_description x8 
         where ((x6.layer_id = x7.component_id ) AND (x8.object_type_id = 2 ) ) ;
create view dsm2_water_body (type_name,type_id,name,layer_id) as
   select x1.name ,x1.object_type_id ,x0.name ,x0.layer_id 
   from "informix".reservoir x0 ,"informix".object_type_description x1 
   where (x1.object_type_id = 3 )  union select x3.name ,x3.object_type_id ,
          x2.channel_number ::varchar(32) ,x2.layer_id 
      from "informix".channel x2 ,"informix".object_type_description x3 
      where (x3.object_type_id = 1 ) ;
create view edit_gate_device (gate_name,layer_name,device_id,
       gate_id,name,structure_type,control_type,nduplicate,max_width,
       base_elev,height,flow_coef_from_node,flow_coef_to_node,
       default_op) as
   select x0.name ,x1.name ,x2.device_id ,x2.gate_id ,x2.name ,
       x2.structure_type ,x2.control_type ,x2.nduplicate ,x2.max_width ,
       x2.base_elev ,x2.height ,x2.flow_coef_from_node ,x2.flow_coef_to_node ,
       x2.default_op 
   from "informix".gate x0 ,"informix".layer_definition x1 ,"informix".gate_device x2 
   where ((x0.layer_id = x1.layer_id ) AND (x0.gate_id = x2.gate_id ) ) ;
create view generate_global_id (next_id) as
   select "informix".test_gen.nextval 
   from "informix".fill_in_type_description x0 
   where (x0.fill_in_type_id = 1 ) ;
create view hydro_model (model_name,model_id,hydro_model_name,
       hydro_model_id) as
   select x0.name ,x0.model_id ,x1.name ,x1.model_id 
   from "informix".model_definition x0 ,"informix".model_definition x1 
   where ((x1.computer_model LIKE 'hydro' ) AND (x0.simulation_id = x1.simulation_id ) ) ;
create view model_input_ts_node (layer,input_series_id,used,name,
       node,role_id,layer_id,variable_name,input_file,path,sign,
       fillin,model_id) as
   select x1.layer ,x0.input_series_id ,x0.used ,x0.name ,x0.node ,
       x0.role_id ,x0.layer_id ,x0.variable_name ,x0.input_file ,
       x0.path ,x0.sign ,x0.fillin ,x1.model_id 
   from "informix".input_time_series_node x0 ,"informix".model_component x1 
   where (((x0.layer_id = x1.component_id ) AND (x0.used != 0 ) ) AND (x1.layer >= ALL (
      select x3.layer 
      from "informix".input_time_series_node x2 ,"informix".model_component x3 
      where (((x2.layer_id = x3.component_id ) AND (x3.model_id = x1.model_id ) ) AND (x2.name = x0.name ) ) 
    ) ) ) ;
create view v_channel (channel_id,layer_id,channel_number,length_ft,
       manning,dispersion,down_node,up_node,used) as
   select x0.channel_id ,x0.layer_id ,x0.channel_number ,x0.length_ft ,
       x0.manning ,x0.dispersion ,x0.down_node ,x0.up_node ,x0.used 
   from "informix".channel x0 ;
create view v_channel_init_condition (icid,channel_number,distance,
       layer_id,variable_name,initial_value,used) as
   select x0.icid ,x0.channel_number ,x0.distance ,x0.layer_id ,
       x0.variable_name ,x0.initial_value ,x0.used 
   from "informix".channel_init_condition x0 ;
create view v_channel_xsect (xsect_id,channel_id,channel_fract_dist,
       copied_from,description) as
   select x0.xsect_id ,x0.channel_id ,x0.channel_fract_dist ,
       x0.copied_from ,x0.description 
   from "informix".channel_xsect x0 ;
create view v_climate_variable_description (climate_variable_id,
       name,description) as
   select x0.climate_variable_id ,x0.name ,x0.description 
   from "informix".climate_variable_description x0 ;
create view v_component_type_description (component_type_id,name,
       description) as
   select x0.component_type_id ,x0.name ,x0.description 
   from "informix".component_type_description x0 ;
create view v_dsm2_water_body (type_name,type_id,name,layer_id) as
   select x0.type_name ,x0.type_id ,x0.name ,x0.layer_id 
   from "informix".dsm2_water_body x0 ;
create view v_edit_gate_device (gate_name,layer_name,device_id,
       gate_id,name,structure_type,control_type,nduplicate,max_width,
       base_elev,height,flow_coef_from_node,flow_coef_to_node,
       default_op) as
   select x0.gate_name ,x0.layer_name ,x0.device_id ,x0.gate_id ,
       x0.name ,x0.structure_type ,x0.control_type ,x0.nduplicate ,
       x0.max_width ,x0.base_elev ,x0.height ,x0.flow_coef_from_node ,
       x0.flow_coef_to_node ,x0.default_op 
   from "informix".edit_gate_device x0 ;
create view v_expression (expression_id,layer_id,name,definition) as
   select x0.expression_id ,x0.layer_id ,x0.name ,x0.definition 
   from "informix".expression x0 ;
create view v_fill_in_type_description (fill_in_type_id,name,
       description) as
   select x0.fill_in_type_id ,x0.name ,x0.description 
   from "informix".fill_in_type_description x0 ;
create view v_gate (gate_id,layer_id,name,obj_connected_type,
       obj_connected_identifier,node_connected,used) as
   select x0.gate_id ,x0.layer_id ,x0.name ,x0.obj_connected_type ,
       x0.obj_connected_identifier ,x0.node_connected ,x0.used 
   from "informix".gate x0 ;
create view v_gate_control_type_description (control_type_id,
       name,description) as
   select x0.control_type_id ,x0.name ,x0.description 
   from "informix".gate_control_type_description x0 ;
create view v_gate_default_op_description (op_id,name,description) as
   select x0.op_id ,x0.name ,x0.description 
   from "informix".gate_default_op_description x0 ;
create view v_gate_device (device_id,gate_id,name,structure_type,
       control_type,nduplicate,max_width,base_elev,height,flow_coef_from_node,
       flow_coef_to_node,default_op) as
   select x0.device_id ,x0.gate_id ,x0.name ,x0.structure_type ,
       x0.control_type ,x0.nduplicate ,x0.max_width ,x0.base_elev ,
       x0.height ,x0.flow_coef_from_node ,x0.flow_coef_to_node ,
       x0.default_op 
   from "informix".gate_device x0 ;
create view v_gate_structure_description (structure_type_id,name,
       description) as
   select x0.structure_type_id ,x0.name ,x0.description 
   from "informix".gate_structure_description x0 ;
create view v_group (group_id,layer_id,name,description,used) as
   select x0.group_id ,x0.layer_id ,x0.name ,x0.description ,
       x0.used 
   from "informix".groups x0 ;
create view v_group_member (group_member_id,group_id,object_type_id,
       identifier) as
   select x0.group_member_id ,x0.group_id ,x0.object_type_id ,
       x0.identifier 
   from "informix".group_member x0 ;
create view v_input_role_description (role_id,name) as
   select x0.role_id ,x0.name 
   from "informix".input_role_description x0 ;
create view v_input_time_series_climate (input_series_id,layer_id,
       climate_variable_id,input_file,path,sign,fillin) as
   select x0.input_series_id ,x0.layer_id ,x0.climate_variable_id ,
       x0.input_file ,x0.path ,x0.sign ,x0.fillin 
   from "informix".input_time_series_climate x0 ;
create view v_input_time_series_gate (input_series_id,layer_id,
       gate,device,input_file,path,variable_name,fillin,used) as
   select x0.input_series_id ,x0.layer_id ,x0.gate ,x0.device ,
       x0.input_file ,x0.path ,x0.variable_name ,x0.fillin ,x0.used 
   from "informix".input_time_series_gate x0 ;
create view v_input_time_series_node (input_series_id,name,node,
       role_id,layer_id,variable_name,input_file,path,sign,fillin,
       used) as
   select x0.input_series_id ,x0.name ,x0.node ,x0.role_id ,
       x0.layer_id ,x0.variable_name ,x0.input_file ,x0.path ,
       x0.sign ,x0.fillin ,x0.used 
   from "informix".input_time_series_node x0 ;
create view v_input_time_series_oprule (input_series_id,layer_id,
       name,input_file,path,sign,fillin) as
   select x0.input_series_id ,x0.layer_id ,x0.name ,x0.input_file ,
       x0.path ,x0.sign ,x0.fillin 
   from "informix".input_time_series_oprule x0 ;
create view v_input_time_series_reservoir (input_series_id,name,
       reservoir,layer_id,variable_name,input_file,role_id,path,
       sign,fillin,used) as
   select x0.input_series_id ,x0.name ,x0.reservoir ,x0.layer_id ,
       x0.variable_name ,x0.input_file ,x0.role_id ,x0.path ,
       x0.sign ,x0.fillin ,x0.used 
   from "informix".input_time_series_reservoir x0 ;
create view v_input_time_series_transfer (input_series_id,layer_id,
       transfer,input_file,path,variable_name,fillin,used) as
   select x0.input_series_id ,x0.layer_id ,x0.transfer ,x0.input_file ,
       x0.path ,x0.variable_name ,x0.fillin ,x0.used 
   from "informix".input_time_series_transfer x0 ;
create view v_layer_definition (layer_id,name,description,owner,
       creation_date,component_type) as
   select x0.layer_id ,x0.name ,x0.description ,x0.owner ,x0.creation_date ,
       x0.component_type 
   from "informix".layer_definition x0 ;
create view v_model_component (model_component_id,model_id,component_id,
       component_type,layer) as
   select x0.model_component_id ,x0.model_id ,x0.component_id ,
       x0.component_type ,x0.layer 
   from "informix".model_component x0 ;
create view v_model_definition (model_id,name,computer_model,
       description,simulation_id,owner,creation_date) as
   select x0.model_id ,x0.name ,x0.computer_model ,x0.description ,
       x0.simulation_id ,x0.owner ,x0.creation_date 
   from "informix".model_definition x0 ;
create view v_model_parameter_description (model_parameter_id,
       name,description) as
   select x0.model_parameter_id ,x0.name ,x0.description 
   from "informix".model_parameter_description x0 ;
create view v_model_parameter_values (param_value_id,model_parameter_id,
       layer_id,parameter_value) as
   select x0.param_value_id ,x0.model_parameter_id ,x0.layer_id ,
       x0.parameter_value 
   from "informix".model_parameter_values x0 ;
create view v_nc_constituent_description (constituent_id,name,
       description) as
   select x0.constituent_id ,x0.name ,x0.description 
   from "informix".nc_constituent_description x0 ;
create view v_object_type_description (object_type_id,name) as
   select x0.object_type_id ,x0.name 
   from "informix".object_type_description x0 ;
create view v_operating_rule (oprule_id,layer_id,name,oprule_action,
       oprule_trigger,used) as
   select x0.oprule_id ,x0.layer_id ,x0.name ,x0.oprule_action ,
       x0.oprule_trigger ,x0.used 
   from "informix".operating_rule x0 ;
create view v_output_time_series_channel (out_id,layer_id,name,
       channel,distance,variable_name,output_file,time_interval,
       period_op,used,source_group) as
   select x0.out_id ,x0.layer_id ,x0.name ,x0.channel ,x0.distance ,
       x0.variable_name ,x0.output_file ,x0.time_interval ,x0.period_op ,
       x0.used ,x0.source_group 
   from "informix".output_time_series_channel x0 ;
create view v_output_time_series_gate (out_id,name,gate,device,
       layer_id,output_file,variable_name,time_interval,period_op,
       used) as
   select x0.out_id ,x0.name ,x0.gate ,x0.device ,x0.layer_id ,
       x0.output_file ,x0.variable_name ,x0.time_interval ,x0.period_op ,
       x0.used 
   from "informix".output_time_series_gate x0 ;
create view v_output_time_series_node (out_id,name,node,layer_id,
       output_file,variable_name,time_interval,period_op,used,
       source_group) as
   select x0.out_id ,x0.name ,x0.node ,x0.layer_id ,x0.output_file ,
       x0.variable_name ,x0.time_interval ,x0.period_op ,x0.used ,
       x0.source_group 
   from "informix".output_time_series_node x0 ;
create view v_output_time_series_reservoir (out_id,name,reservoir,
       connection_node,layer_id,output_file,variable_name,time_interval,
       period_op,used,source_group) as
   select x0.out_id ,x0.name ,x0.reservoir ,x0.connection_node ,
       x0.layer_id ,x0.output_file ,x0.variable_name ,x0.time_interval ,
       x0.period_op ,x0.used ,x0.source_group 
   from "informix".output_time_series_reservoir x0 ;
create view v_period_op_description (period_op_id,name,description) as
   select x0.period_op_id ,x0.name ,x0.description 
   from "informix".period_op_description x0 ;
create view v_permissions (permission_id,user_name,component_set_id,
       expiration_date) as
   select x0.permission_id ,x0.user_name ,x0.component_set_id ,
       x0.expiration_date 
   from "informix".permissions x0 ;
create view v_primary_key_generation (id_id,new_id) as
   select x0.id_id ,x0.new_id 
   from "informix".primary_key_generation x0 ;
create view v_rate_coefficient (rate_coefficient_id,layer_id,
       group_name,rate_variable_id,constituent_id,coefficient_value) as
   select x0.rate_coefficient_id ,x0.layer_id ,x0.group_name ,
       x0.rate_variable_id ,x0.constituent_id ,x0.coefficient_value 
   from "informix".rate_coefficient x0 ;
create view v_rate_variable_description (rate_variable_id,name,
       description) as
   select x0.rate_variable_id ,x0.name ,x0.description 
   from "informix".rate_variable_description x0 ;
create view v_reservoir (reservoir_id,layer_id,name,area,bottom_elev,
       used) as
   select x0.reservoir_id ,x0.layer_id ,x0.name ,x0.area ,x0.bottom_elev ,
       x0.used 
   from "informix".reservoir x0 ;
create view v_reservoir_connections (connection_id,reservoir_id,
       connected_node_number,in_coef,out_coef) as
   select x0.connection_id ,x0.reservoir_id ,x0.connected_node_number ,
       x0.in_coef ,x0.out_coef 
   from "informix".reservoir_connections x0 ;
create view v_reservoir_init_condition (icid,reservoir_name,layer_id,
       variable_name,initial_value,used) as
   select x0.icid ,x0.reservoir_name ,x0.layer_id ,x0.variable_name ,
       x0.initial_value ,x0.used 
   from "informix".reservoir_init_condition x0 ;
create view v_simulation_definition (simulation_id,name,description,
       owner,creation_date) as
   select x0.simulation_id ,x0.name ,x0.description ,x0.owner ,
       x0.creation_date 
   from "informix".simulation_definition x0 ;
create view v_transfer (transfer_id,layer_id,name,from_object_type,
       from_object_identifier,to_object_type,to_object_identifier,
       used) as
   select x0.transfer_id ,x0.layer_id ,x0.name ,x0.from_object_type ,
       x0.from_object_identifier ,x0.to_object_type ,x0.to_object_identifier ,
       x0.used 
   from "informix".transfer x0 ;
create view v_xsect_layer (xsect_layer_id,xsect_id,elev,width,
       area,wet_perimeter) as
   select x0.xsect_layer_id ,x0.xsect_id ,x0.elev ,x0.width ,
       x0.area ,x0.wet_perimeter 
   from "informix".xsect_layer x0 ;
ALTER SEQUENCE gen_permission_id RESTART WITH 208;
ALTER SEQUENCE global_gen RESTART WITH 2018656;
ALTER SEQUENCE temp_seq RESTART WITH 2301768;
ALTER SEQUENCE test_gen RESTART WITH 910867;
alter table channel add constraint unique 
	(layer_id, channel_number)
	constraint u103_244;

alter table channel add constraint primary key 
	(channel_id)
	constraint u103_284;

alter table channel add constraint check 
	((length_ft > 0 ))
	constraint c103_226;

alter table channel add constraint check 
	(((manning >= 0 ) AND (manning < 1 ) ))
	constraint c103_227;

alter table channel add constraint check 
	((dispersion >= 0 ))
	constraint c103_228;

alter table channel_init_condition add constraint unique 
	(layer_id, channel_number, distance, variable_name)
	constraint u104_245;

alter table channel_init_condition add constraint primary key 
	(icid)
	constraint u104_285;

alter table channel_xsect add constraint unique 
	(channel_id, channel_fract_dist)
	constraint u105_246;

alter table channel_xsect add constraint primary key 
	(xsect_id)
	constraint u105_286;

alter table climate_variable_description add constraint primary key 
	(climate_variable_id)
	constraint pk_climate_vari339;

alter table component_type_description add constraint unique 
	(name)
	constraint u106_247;

alter table component_type_description add constraint primary key 
	(component_type_id)
	constraint u106_287;

alter table expression add constraint unique 
	(layer_id, name)
	constraint u107_248;

alter table expression add constraint primary key 
	(expression_id)
	constraint u107_288;

alter table fill_in_type_description add constraint unique 
	(name)
	constraint u108_249;

alter table fill_in_type_description add constraint primary key 
	(fill_in_type_id)
	constraint u108_289;

alter table gate add constraint unique 
	(name, layer_id)
	constraint u109_250;

alter table gate add constraint primary key 
	(gate_id)
	constraint u109_290;

alter table gate_control_type_description add constraint unique 
	(name)
	constraint u110_251;

alter table gate_control_type_description add constraint primary key 
	(control_type_id)
	constraint u110_291;

alter table gate_default_op_description add constraint unique 
	(name)
	constraint u111_252;

alter table gate_default_op_description add constraint primary key 
	(op_id)
	constraint u111_292;

alter table gate_device add constraint unique 
	(name, gate_id)
	constraint u112_253;

alter table gate_device add constraint primary key 
	(device_id)
	constraint u112_293;

alter table gate_structure_description add constraint unique 
	(name)
	constraint u114_255;

alter table gate_structure_description add constraint primary key 
	(structure_type_id)
	constraint u114_295;

alter table group_member add constraint primary key 
	(group_member_id)
	constraint u202_381;

alter table groups add constraint unique 
	(name, layer_id)
	constraint u200_371;

alter table groups add constraint primary key 
	(group_id)
	constraint u200_372;

alter table input_role_description add constraint unique 
	(name)
	constraint u116_257;

alter table input_role_description add constraint primary key 
	(role_id)
	constraint u116_297;

alter table input_time_series_climate add constraint unique 
	(layer_id, climate_variable_id)
	constraint ct_input_time_s839;

alter table input_time_series_climate add constraint primary key 
	(input_series_id)
	constraint pk_input_time_s937;

alter table input_time_series_gate add constraint unique 
	(gate, device, variable_name, layer_id)
	constraint u117_258;

alter table input_time_series_gate add constraint primary key 
	(input_series_id)
	constraint u117_298;

alter table input_time_series_node add constraint unique 
	(node, layer_id, name, variable_name)
	constraint u118_259;

alter table input_time_series_node add constraint primary key 
	(input_series_id)
	constraint u118_299;

alter table input_time_series_oprule add constraint unique 
	(layer_id, name)
	constraint u119_260;

alter table input_time_series_oprule add constraint primary key 
	(input_series_id)
	constraint u119_300;

alter table input_time_series_reservoir add constraint unique 
	(layer_id, name, reservoir)
	constraint u120_261;

alter table input_time_series_reservoir add constraint primary key 
	(input_series_id)
	constraint u120_301;

alter table input_time_series_transfer add constraint unique 
	(layer_id, transfer, variable_name)
	constraint u121_262;

alter table input_time_series_transfer add constraint primary key 
	(input_series_id)
	constraint u121_302;

alter table layer_definition add constraint unique 
	(name)
	constraint u122_263;

alter table layer_definition add constraint primary key 
	(layer_id)
	constraint u122_303;

alter table model_component add constraint unique 
	(model_id, component_type, layer)
	constraint u123_264;

alter table model_component add constraint primary key 
	(model_component_id)
	constraint u123_304;

alter table model_definition add constraint unique 
	(name)
	constraint u124_265;

alter table model_definition add constraint primary key 
	(model_id)
	constraint u124_305;

alter table model_definition add constraint check 
	((computer_model IN ('hydro' ,'qual' ,'ptm' )))
	constraint c124_229;

alter table model_parameter_description add constraint unique 
	(name)
	constraint u125_266;

alter table model_parameter_description add constraint primary key 
	(model_parameter_id)
	constraint u125_306;

alter table model_parameter_values add constraint unique 
	(model_parameter_id, layer_id)
	constraint u126_267;

alter table model_parameter_values add constraint primary key 
	(param_value_id)
	constraint u126_307;

alter table nc_constituent_description add constraint primary key 
	(constituent_id)
	constraint pk_nonconserve_155;

alter table object_type_description add constraint unique 
	(name)
	constraint u127_268;

alter table object_type_description add constraint primary key 
	(object_type_id)
	constraint u127_308;

alter table operating_rule add constraint unique 
	(layer_id, name)
	constraint u128_269;

alter table operating_rule add constraint primary key 
	(oprule_id)
	constraint u128_309;

alter table output_time_series_channel add constraint unique 
	(name, variable_name, layer_id, time_interval, period_op)
	constraint u220_270;

alter table output_time_series_channel add constraint primary key 
	(out_id)
	constraint u220_310;

alter table output_time_series_gate add constraint unique 
	(name, variable_name, layer_id, time_interval, period_op)
	constraint u130_271;

alter table output_time_series_gate add constraint primary key 
	(out_id)
	constraint u130_311;

alter table output_time_series_node add constraint unique 
	(layer_id, variable_name, time_interval, period_op, name)
	constraint u221_272;

alter table output_time_series_node add constraint primary key 
	(out_id)
	constraint u221_312;

alter table output_time_series_reservoir add constraint unique 
	(name, layer_id, variable_name, time_interval, period_op, source_group)
	constraint u132_273;

alter table output_time_series_reservoir add constraint primary key 
	(out_id)
	constraint u132_313;

alter table period_op_description add constraint unique 
	(name)
	constraint u133_274;

alter table period_op_description add constraint primary key 
	(period_op_id)
	constraint u133_314;

alter table permissions add constraint unique 
	(user_name, component_set_id)
	constraint u134_275;

alter table permissions add constraint primary key 
	(permission_id)
	constraint u134_315;

alter table primary_key_generation add constraint primary key 
	(id_id)
	constraint u136_317;

alter table rate_coefficient add constraint unique 
	(layer_id, group_name, rate_variable_id, constituent_id)
	constraint ct_rate_coeffic667;

alter table rate_coefficient add constraint primary key 
	(rate_coefficient_id)
	constraint pk_rate_coeffic503;

alter table rate_variable_description add constraint primary key 
	(rate_variable_id)
	constraint pk_rate_variabl517;

alter table reservoir add constraint unique 
	(layer_id, name)
	constraint u137_277;

alter table reservoir add constraint primary key 
	(reservoir_id)
	constraint u137_318;

alter table reservoir add constraint check 
	((area > 0. ))
	constraint c137_243;

alter table reservoir_connections add constraint unique 
	(reservoir_id, connected_node_number)
	constraint u138_278;

alter table reservoir_connections add constraint primary key 
	(connection_id)
	constraint u138_319;

alter table reservoir_connections add constraint check 
	((in_coef >= 0 ))
	constraint c138_234;

alter table reservoir_connections add constraint check 
	((out_coef >= 0 ))
	constraint c138_235;

alter table reservoir_init_condition add constraint unique 
	(layer_id, reservoir_name, variable_name)
	constraint u139_279;

alter table reservoir_init_condition add constraint primary key 
	(icid)
	constraint u139_320;

alter table simulation_definition add constraint unique 
	(name)
	constraint u140_280;

alter table simulation_definition add constraint primary key 
	(simulation_id)
	constraint u140_321;

alter table transfer add constraint unique 
	(layer_id, name)
	constraint u141_281;

alter table transfer add constraint primary key 
	(transfer_id)
	constraint u141_322;

alter table xsect_layer add constraint unique 
	(xsect_id, elev)
	constraint u143_283;

alter table xsect_layer add constraint primary key 
	(xsect_layer_id)
	constraint u143_324;

alter table xsect_layer add constraint check 
	((width >= 0 ))
	constraint c143_240;

alter table xsect_layer add constraint check 
	((area >= 0 ))
	constraint c143_241;

alter table xsect_layer add constraint check 
	((wet_perimeter >= 0 ))
	constraint c143_242;

alter table channel add constraint foreign key 
	(layer_id)
	references layer_definition
	(layer_id) 
	on delete cascade 
	constraint r103_325;

alter table channel_xsect add constraint foreign key 
	(channel_id)
	references channel
	(channel_id) 
	on delete cascade 
	constraint r105_326;

alter table expression add constraint foreign key 
	(layer_id)
	references layer_definition
	(layer_id) 
	on delete cascade 
	constraint r107_327;

alter table gate add constraint foreign key 
	(obj_connected_type)
	references object_type_description
	(object_type_id) 
	constraint r109_328;

alter table gate add constraint foreign key 
	(layer_id)
	references layer_definition
	(layer_id) 
	on delete cascade 
	constraint r109_329;

alter table gate_device add constraint foreign key 
	(gate_id)
	references gate
	(gate_id) 
	on delete cascade 
	constraint r112_330;

alter table gate_device add constraint foreign key 
	(default_op)
	references gate_default_op_description
	(op_id) 
	constraint r112_331;

alter table gate_device add constraint foreign key 
	(control_type)
	references gate_control_type_description
	(control_type_id) 
	constraint r112_332;

alter table gate_device add constraint foreign key 
	(structure_type)
	references gate_structure_description
	(structure_type_id) 
	on delete cascade 
	constraint r112_333;

alter table group_member add constraint foreign key 
	(group_id)
	references groups
	(group_id) 
	on delete cascade 
	constraint r202_387;

alter table group_member add constraint foreign key 
	(object_type_id)
	references object_type_description
	(object_type_id) 
	constraint r202_388;

alter table input_time_series_climate add constraint foreign key 
	(climate_variable_id)
	references climate_variable_description
	(climate_variable_id) 
	on delete cascade 
	constraint fk_input_time_s756;

alter table input_time_series_climate add constraint foreign key 
	(fillin)
	references fill_in_type_description
	(fill_in_type_id) 
	on delete cascade 
	constraint fk_input_time_s954;

alter table input_time_series_gate add constraint foreign key 
	(layer_id)
	references layer_definition
	(layer_id) 
	on delete cascade 
	constraint r117_336;

alter table input_time_series_gate add constraint foreign key 
	(fillin)
	references fill_in_type_description
	(fill_in_type_id) 
	constraint r117_337;

alter table input_time_series_node add constraint foreign key 
	(layer_id)
	references layer_definition
	(layer_id) 
	on delete cascade 
	constraint r118_338;

alter table input_time_series_node add constraint foreign key 
	(role_id)
	references input_role_description
	(role_id) 
	constraint r118_339;

alter table input_time_series_node add constraint foreign key 
	(fillin)
	references fill_in_type_description
	(fill_in_type_id) 
	constraint r118_340;

alter table input_time_series_oprule add constraint foreign key 
	(layer_id)
	references layer_definition
	(layer_id) 
	on delete cascade 
	constraint r119_341;

alter table input_time_series_oprule add constraint foreign key 
	(fillin)
	references fill_in_type_description
	(fill_in_type_id) 
	constraint r119_342;

alter table input_time_series_reservoir add constraint foreign key 
	(fillin)
	references fill_in_type_description
	(fill_in_type_id) 
	constraint r120_343;

alter table input_time_series_reservoir add constraint foreign key 
	(layer_id)
	references layer_definition
	(layer_id) 
	on delete cascade 
	constraint r120_344;

alter table input_time_series_reservoir add constraint foreign key 
	(role_id)
	references input_role_description
	(role_id) 
	constraint r120_345;

alter table input_time_series_transfer add constraint foreign key 
	(layer_id)
	references layer_definition
	(layer_id) 
	on delete cascade 
	constraint r121_346;

alter table input_time_series_transfer add constraint foreign key 
	(fillin)
	references fill_in_type_description
	(fill_in_type_id) 
	constraint r121_347;

alter table layer_definition add constraint foreign key 
	(component_type)
	references component_type_description
	(name) 
	constraint r122_348;

alter table model_component add constraint foreign key 
	(model_id)
	references model_definition
	(model_id) 
	on delete cascade 
	constraint r123_349;

alter table model_component add constraint foreign key 
	(component_type)
	references component_type_description
	(name) 
	constraint r123_350;

alter table model_definition add constraint foreign key 
	(simulation_id)
	references simulation_definition
	(simulation_id) 
	on delete cascade 
	constraint r124_351;

alter table model_parameter_values add constraint foreign key 
	(model_parameter_id)
	references model_parameter_description
	(model_parameter_id) 
	constraint r126_352;

alter table model_parameter_values add constraint foreign key 
	(layer_id)
	references layer_definition
	(layer_id) 
	on delete cascade 
	constraint r126_353;

alter table operating_rule add constraint foreign key 
	(layer_id)
	references layer_definition
	(layer_id) 
	on delete cascade 
	constraint r128_354;

alter table output_time_series_channel add constraint foreign key 
	(layer_id)
	references layer_definition
	(layer_id) 
	on delete cascade 
	constraint r220_355;

alter table output_time_series_channel add constraint foreign key 
	(period_op)
	references period_op_description
	(name) 
	constraint r220_356;

alter table output_time_series_gate add constraint foreign key 
	(period_op)
	references period_op_description
	(name) 
	constraint r130_357;

alter table output_time_series_gate add constraint foreign key 
	(layer_id)
	references layer_definition
	(layer_id) 
	on delete cascade 
	constraint r130_358;

alter table output_time_series_node add constraint foreign key 
	(layer_id)
	references layer_definition
	(layer_id) 
	on delete cascade 
	constraint r221_359;

alter table output_time_series_node add constraint foreign key 
	(period_op)
	references period_op_description
	(name) 
	constraint r221_360;

alter table output_time_series_reservoir add constraint foreign key 
	(layer_id)
	references layer_definition
	(layer_id) 
	on delete cascade 
	constraint r132_361;

alter table output_time_series_reservoir add constraint foreign key 
	(period_op)
	references period_op_description
	(name) 
	constraint r132_362;

alter table rate_coefficient add constraint foreign key 
	(rate_variable_id)
	references rate_variable_description
	(rate_variable_id) 
	on delete cascade 
	constraint fk_rate_coeffic917;

alter table rate_coefficient add constraint foreign key 
	(constituent_id)
	references nc_constituent_description
	(constituent_id) 
	on delete cascade 
	constraint fk_rate_coeffic825;

alter table reservoir add constraint foreign key 
	(layer_id)
	references layer_definition
	(layer_id) 
	on delete cascade 
	constraint r137_364;

alter table reservoir_connections add constraint foreign key 
	(reservoir_id)
	references reservoir
	(reservoir_id) 
	on delete cascade 
	constraint r138_365;

alter table transfer add constraint foreign key 
	(layer_id)
	references layer_definition
	(layer_id) 
	on delete cascade 
	constraint r141_366;

alter table transfer add constraint foreign key 
	(from_object_type)
	references object_type_description
	(object_type_id) 
	constraint r141_367;

alter table transfer add constraint foreign key 
	(to_object_type)
	references object_type_description
	(object_type_id) 
	constraint r141_368;

alter table xsect_layer add constraint foreign key 
	(xsect_id)
	references channel_xsect
	(xsect_id) 
	on delete cascade 
	constraint r143_370;

