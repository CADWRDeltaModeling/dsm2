!<license>
!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
!    Department of Water Resources.
!    This file is part of DSM2.

!    The Delta Simulation Model 2 (DSM2) is free software:
!    you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.

!    DSM2 is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.

!    You should have received a copy of the GNU General Public License
!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
!</license>

module constants
    !-----miscellaneous values
    !-----For compilation using 8 or 4 byte precision. HECDSS is always 4
    integer :: REAL_PRECISION
    parameter(REAL_PRECISION=8)

 


    !-----index numbers for channels & nodes
    integer :: &
        chan_no
    integer :: length
    integer :: manning
    integer :: disp
    integer :: boundary_type
    integer :: xsect
    integer :: node_no
    integer :: upnode
    integer :: downnode
    integer :: dist
    parameter ( &
        chan_no=1 &
        ,length=2 &
        ,manning=3 &
        ,disp=4 &
        ,boundary_type=5 &
        ,xsect=6 &
        ,node_no=7 &
        ,upnode=8 &
        ,downnode=9 &
        ,dist=10 &
        )

    !-----index numbers for cross sections
    integer :: &
        x_no          ! initial flow (cfs)
    integer :: x_width          ! initial flow (cfs)
    integer :: x_botelev          ! initial flow (cfs)
    integer :: x_up          ! initial flow (cfs)
    integer :: x_init_stage          ! initial flow (cfs)
    integer :: x_init_flow          ! initial flow (cfs)
    parameter ( &
        x_no=1 &
        ,x_width=2 &
        ,x_botelev=3 &
        ,x_up=4 &
        ,x_init_stage=5 &
        ,x_init_flow=6 &
        )

    !-----irregular geometry numbers
    integer :: &
        irg_chan               ! filename containing geom viewer xsect output
    integer :: irg_dist               ! filename containing geom viewer xsect output
    integer :: irg_fn               ! filename containing geom viewer xsect output
                              ! for this cross section
    parameter ( &
        irg_chan=1 &
        ,irg_dist=2 &
        ,irg_fn=3)

    !-----index numbers for reservoirs
    integer :: &
        res_name
    integer :: res_area
    integer :: res_stage
    integer :: res_botelv
    integer :: res_node
    integer :: res_coeff2res
    integer :: res_coeff2chan
    parameter ( &
        res_name=1 &
        ,res_area=2 &
        ,res_stage=3 &
        ,res_botelv=4 &
        ,res_node=5 &
        ,res_coeff2res=6 &
        ,res_coeff2chan=7 &
        )

    !-----object-to-object connections
    integer :: &
        obj2obj_from_objtype      ! internal flow ID name (e.g. 'IF', 'HOOD_DIV')
    integer :: obj2obj_from_objname      ! internal flow ID name (e.g. 'IF', 'HOOD_DIV')
    integer :: obj2obj_to_objtype      ! internal flow ID name (e.g. 'IF', 'HOOD_DIV')
    integer :: obj2obj_to_objname      ! internal flow ID name (e.g. 'IF', 'HOOD_DIV')
    integer :: obj2obj_pathinput_label      ! internal flow ID name (e.g. 'IF', 'HOOD_DIV')
    integer :: obj2obj_flow      ! internal flow ID name (e.g. 'IF', 'HOOD_DIV')
    integer :: obj2obj_poscoeff      ! internal flow ID name (e.g. 'IF', 'HOOD_DIV')
    integer :: obj2obj_negcoeff      ! internal flow ID name (e.g. 'IF', 'HOOD_DIV')
    integer :: obj2obj_groupname      ! internal flow ID name (e.g. 'IF', 'HOOD_DIV')
    integer :: obj2obj_objname      ! internal flow ID name (e.g. 'IF', 'HOOD_DIV')

    parameter ( &
        obj2obj_from_objtype=1 &
        ,obj2obj_from_objname=2 &
        ,obj2obj_to_objtype=3 &
        ,obj2obj_to_objname=4 &
        ,obj2obj_pathinput_label=5 &
        ,obj2obj_flow=6 &
        ,obj2obj_poscoeff=7 &
        ,obj2obj_negcoeff=8 &
        ,obj2obj_groupname=9 &
        ,obj2obj_objname=10 &
        )

    !-----index numbers for gates

    integer, parameter :: gate_operation=22
    integer, parameter :: gate_op_to_node=221
    integer, parameter :: gate_op_from_node=222
    integer, parameter :: gate_install=128
    integer, parameter :: gate_position=129
    integer, parameter :: gate_height=130
    integer, parameter :: gate_width=131
    integer, parameter :: gate_elev=132


    !-----constants representing different kinds of data sources
    integer :: &
        const_data        ! data from operating rule expression
    integer :: dss_data        ! data from operating rule expression
    integer :: expression_data        ! data from operating rule expression

    parameter ( &
        const_data=128 &
        ,dss_data=256 &
        ,expression_data=512 &
        )






    !-----index numbers for path input
    integer :: &
        inpath_label         ! start time for data
    integer :: inpath_node         ! start time for data
    integer :: inpath_a_part         ! start time for data
    integer :: inpath_b_part         ! start time for data
    integer :: inpath_c_part         ! start time for data
    integer :: inpath_e_part         ! start time for data
    integer :: inpath_f_part         ! start time for data
    integer :: inpath_ID         ! start time for data
    integer :: inpath_meas_type         ! start time for data
    integer :: inpath_interval         ! start time for data
    integer :: inpath_filename         ! start time for data
    integer :: inpath_value         ! start time for data
    integer :: inpath_fillin         ! start time for data
    integer :: inpath_priority         ! start time for data
    integer :: inpath_sdate         ! start time for data
    integer :: inpath_stime         ! start time for data

    parameter ( &
        inpath_label=1 &
        ,inpath_node=2 &
        ,inpath_a_part=3 &
        ,inpath_b_part=4 &
        ,inpath_c_part=5 &
        ,inpath_e_part=6 &
        ,inpath_f_part=7 &
        ,inpath_ID=12 &
        ,inpath_meas_type=13 &
        ,inpath_interval=14 &
        ,inpath_filename=15 &
        ,inpath_value=16 &
        ,inpath_fillin=17 &
        ,inpath_priority=18 &
        ,inpath_sdate=19 &
        ,inpath_stime=20 &
        )

    !-----index numbers for printout
    integer :: &
        outpath_filename
    integer :: outpath_a_part
    integer :: outpath_b_part
    integer :: outpath_c_part
    integer :: outpath_e_part
    integer :: outpath_f_part
    integer :: outpath_name
    integer :: outpath_chan
    integer :: outpath_dist
    integer :: outpath_node
    integer :: outpath_res_name
    integer :: outpath_res_node
    integer :: outpath_type
    integer :: outpath_interval
    integer :: outpath_period
    integer :: outpath_modifier
    integer :: outpath_from_name
    integer :: outpath_source_group
    integer :: outpath_variable
    integer :: outpath_fromwb
    integer :: outpath_towb
    integer :: outpath_gate
    integer :: outpath_gate_device
     

    parameter ( &
        outpath_filename=1 &
        ,outpath_a_part=2 &
        ,outpath_b_part=3 &
        ,outpath_c_part=4 &
        ,outpath_e_part=5 &
        ,outpath_f_part=6 &
        ,outpath_name=7 &
        ,outpath_chan=8 &
        ,outpath_dist=9 &
        ,outpath_node=10 &
        ,outpath_res_name=11 &
        ,outpath_res_node=12 &
        ,outpath_type=13 &
        ,outpath_interval=14 &
        ,outpath_period=15 &
        ,outpath_modifier=16 &
        ,outpath_from_name=17 &
        ,outpath_source_group=18 &
        ,outpath_variable=19 &
        ,outpath_fromwb=20 &
        ,outpath_towb=21 &
        ,outpath_gate=22 &
        ,outpath_gate_device=23 &
        )

    !-----"internal/pseudo" environment variables
    integer :: &
        envvar_name
    integer :: envvar_value

    parameter ( &
        envvar_name=1 &
        ,envvar_value=2 &
        )

    !-----magic numbers for i/o files section
    integer :: &
        io_model
    integer :: io_type
    integer :: io_io
    integer :: io_interval
    integer :: io_filename

    parameter ( &
        io_model=1 &
        ,io_type=2 &
        ,io_io=3 &
        ,io_interval=4 &
        ,io_filename=5 &
        )

    !-----model
    integer :: &
        hydro
    integer :: qual
    integer :: ptm

    parameter ( &
        hydro=1 &
        ,qual=2 &
        ,ptm=3 &
        )

    !-----type
    integer :: &
        io_restart
    integer :: io_echo
    integer :: io_animation
    integer :: io_trace
    integer :: io_behavior
    integer :: io_group
    integer :: io_hdf5

    parameter( &
        io_restart=1 &
        ,io_echo=2 &
        ,io_animation=3 &
        ,io_trace=4 &
        ,io_behavior=5 &
        ,io_group=6 &
        ,io_hdf5=7 &
        )

    !-----io
    integer :: &
        io_read
    integer :: io_write

    parameter( &
        io_read=1 &
        ,io_write=2 &
        )

    !-----quad integration index numbers
    integer :: &
        q_pt
    integer :: q_wt
    parameter ( &
        q_pt=1 &
        ,q_wt=2 &
        )

    !-----location name --> chan/dist pair translation
    integer :: &
        trans_name
    integer :: trans_chan
    integer :: trans_dist
    integer :: trans_node
    integer :: trans_res
    integer :: trans_gate
    integer :: trans_const
    parameter ( &
        trans_name=1 &
        ,trans_chan=2 &
        ,trans_dist=3 &
        ,trans_node=4 &
        ,trans_res=5 &
        ,trans_gate=6 &
        ,trans_const=7 &
        )

    !-----assign types to input paths
    integer :: &
        type_string
    integer :: type_part
    integer :: type_match
    integer :: type_sign
    integer :: type_groupname
    integer :: type_massfrac
    integer :: type_value_in
    integer :: type_value_out
    integer :: type_value_flag

    parameter ( &
        type_string=1 &
        ,type_part=2 &
        ,type_match=3 &
        ,type_sign=4 &
        ,type_groupname=5 &
        ,type_massfrac=6 &
        ,type_value_in=7 &
        ,type_value_out=8 &
        ,type_value_flag=9 &
        )


    !-----magic numbers for tidefile input

    integer :: &
        tide_sdate
    integer :: tide_stime
    integer :: tide_edate
    integer :: tide_etime
    integer :: tide_fname

    parameter ( &
        tide_sdate=1 &
        ,tide_stime=2 &
        ,tide_edate=3 &
        ,tide_etime=4 &
        ,tide_fname=5 &
        )

    !-----magic numbers for qual binary file input

    integer :: &
        binary_fname

    parameter ( &
        binary_fname=1 &
        )

    !-----channel coefficients

    integer :: &
        coeff_chan
    integer :: coeff_res
    integer :: coeff_type
    integer :: coeff_const
    integer :: coeff_value

    parameter ( &
        coeff_chan=1 &
        ,coeff_res=2 &
        ,coeff_type=3 &
        ,coeff_const=4 &
        ,coeff_value=5 &
        )

    integer*4 :: &
        TIDE_START         ! go to last tide block
    integer*4 :: TIDE_LENGTH         ! go to last tide block

    parameter ( &
        TIDE_START=1357 &
        ,TIDE_LENGTH=2468 &
        )

    !-----boundary types
    integer :: &
        stage_boundary
    integer :: flow_boundary
    integer :: gate_boundary
    !     &     ,unknown_type
    !-----fixme: fill in other data types later

    parameter ( &
        flow_boundary=1 &
        ,stage_boundary=2 &
        ,gate_boundary=3 &
        !     &     ,unknown_type=4
        )

    !-----data quality flags, bit positions
    integer :: &
        SCREENED_DATA           ! rejected data bit position
    integer :: GOOD_DATA           ! rejected data bit position
    integer :: MISSING_DATA           ! rejected data bit position
    integer :: QUESTION_DATA           ! rejected data bit position
    integer :: REJECT_DATA           ! rejected data bit position
    integer :: MISS_OR_REJ_DATA           ! rejected data bit position
    integer :: SCREENED_BIT           ! rejected data bit position
    integer :: MISSING_BIT           ! rejected data bit position
    integer :: GOOD_BIT           ! rejected data bit position
    integer :: QUESTION_BIT           ! rejected data bit position
    integer :: REJECT_BIT           ! rejected data bit position

    parameter ( &
        SCREENED_DATA=100 &
        ,GOOD_DATA=101 &
        ,MISSING_DATA=102 &
        ,QUESTION_DATA=103 &
        ,REJECT_DATA=104 &
        ,MISS_OR_REJ_DATA=110 &
        ,SCREENED_BIT=0 &
        ,GOOD_BIT=1 &
        ,MISSING_BIT=2 &
        ,QUESTION_BIT=3 &
        ,REJECT_BIT=4 &
        )

    !-----object type codes
    integer :: &
        obj_channel
    integer :: obj_node
    integer :: obj_reservoir
    integer :: obj_gate
    integer :: obj_qext
    integer :: obj_obj2obj
    integer :: obj_flux
    integer :: obj_stage
    integer :: obj_null
    integer :: obj_group
    integer :: obj_oprule
    integer :: obj_boundary_flow
    integer :: obj_source_sink
    integer :: obj_climate
    integer :: obj_filter          ! particle filter without interrupting flows


     parameter ( &
          obj_channel=1 &
          ,obj_node=2 &
          ,obj_reservoir=3 &
          ,obj_gate=4 &
          ,obj_qext=5 &
          ,obj_obj2obj=6 &
          ,obj_flux=7 &
          ,obj_stage=8 &
          ,obj_null=9 &
          ,obj_boundary_flow=15 &
          ,obj_source_sink=16 &
          ,obj_group=22 &
          ,obj_climate=30 &
          ,obj_oprule=111 &
          ,obj_filter=120 &
          )

    !-----data types
    integer :: &
        per_type_per_aver
    integer :: per_type_per_cum
    integer :: per_type_per_min
    integer :: per_type_per_max
    integer :: per_type_inst_val
    integer :: per_type_inst_cum
    integer :: per_type_null

    parameter ( &
        per_type_per_aver=1 &
        ,per_type_per_cum=2 &
        ,per_type_per_min=3 &
        ,per_type_per_max=4 &
        ,per_type_inst_val=5 &
        ,per_type_inst_cum=6 &
        ,per_type_null=7 &
        )

    !-----coefficient type codes
    integer :: &
        decay
    integer :: settle
    integer :: benthic
    integer :: alg_grow
    integer :: alg_resp
    integer :: alg_die

    parameter ( &
        decay=1 &
        ,settle=2 &
        ,benthic=3 &
        ,alg_grow=4 &
        ,alg_resp=5 &
        ,alg_die=6)

    !-----misc magic characters and numbers

    logical :: &
        EXACT_MATCH        ! search for substring match
    logical :: SUBSTR_MATCH        ! search for substring match

    integer :: &
        TO_BOUNDARY           ! only obj2obj (internal) flows
    integer :: NEAREST_BOUNDARY           ! only obj2obj (internal) flows
    integer :: IGNORE_BOUNDARY           ! only obj2obj (internal) flows
    integer :: TO_OBJ           ! only obj2obj (internal) flows
    integer :: FROM_OBJ           ! only obj2obj (internal) flows
    integer :: ALL_FLOWS           ! only obj2obj (internal) flows
    integer :: NO_CONNECT           ! only obj2obj (internal) flows
    integer :: QEXT_FLOWS           ! only obj2obj (internal) flows
    integer :: QINT_FLOWS           ! only obj2obj (internal) flows


    parameter ( &
        EXACT_MATCH=.true. &
        ,SUBSTR_MATCH=.false. &
        ,TO_BOUNDARY=1 &
        ,NEAREST_BOUNDARY=2 &
        ,IGNORE_BOUNDARY=3 &
        ,TO_OBJ=1 &
        ,FROM_OBJ=2 &
        ,ALL_FLOWS=0 &
        ,NO_CONNECT=-100 &
        ,QEXT_FLOWS=-200 &
        ,QINT_FLOWS=-300 &
        )

    !
    !-----input stuff

    character :: &
        delimiter*1          ! backslash character
    character :: empty_field*1          ! backslash character
    character :: iep_sep*1          ! backslash character
    character :: backslash*1          ! backslash character
    parameter ( &
        delimiter='|' &
        ,empty_field=' ' &
        ,iep_sep='+' &
        !          ,backslash='\\' &      !! <UNIX>
        ,backslash='\' &       !! <NT>
        )

    character*14 :: generic_date   ! generic date/time start
    parameter (generic_date='01JAN3001 0000')

    integer :: &
        chan_length            ! integer for initializing variables
    integer :: chan_mid            ! integer for initializing variables
    integer :: chan_up            ! integer for initializing variables
    integer :: chan_down            ! integer for initializing variables
    integer :: miss_val_i            ! integer for initializing variables
    integer :: prev_julmin_i            ! integer for initializing variables
    integer :: start_file_i            ! integer for initializing variables
    integer :: end_file_i            ! integer for initializing variables
    integer :: fill_interp            ! integer for initializing variables
    integer :: fill_first            ! integer for initializing variables
    integer :: fill_last            ! integer for initializing variables
    integer :: fill_bydata            ! integer for initializing variables
    integer :: init_small_i            ! integer for initializing variables
    integer :: init_big_i            ! integer for initializing variables

    real*8 :: &
        head_diff            ! instead of a fixed flow, calc from head diff

    parameter ( &
        chan_length= -99999999 &
        ,chan_mid=88888 &
        ,chan_up=77771 &
        ,chan_down=77770 &
        ,miss_val_i=-901 &
        ,prev_julmin_i=-902 &
        ,start_file_i=-903 &
        ,end_file_i=-904 &
        ,fill_last=1 &
        ,fill_interp=2 &
        ,fill_first=3 &
        ,fill_bydata=4 &
        ,init_small_i=0 &
        ,init_big_i=9998888 &
        ,head_diff=-9998888. &
        )

    real*8 :: &
        miss_val_r            ! for initializing irreg_geom structures
    real*8 :: init_small_r            ! for initializing irreg_geom structures
    real*8 :: init_big_r            ! for initializing irreg_geom structures
    parameter ( &
        miss_val_r=-901. &
        ,init_small_r=-99999.0 &
        ,init_big_r=99999.0 &
        )

    character*1 :: &
        miss_val_c           ! missing character marker
    parameter ( &
        miss_val_c=char(1) &
        )

    !-----magic numbers for non-conservative constituents
    integer :: &
        ncc_do
    integer :: ncc_organic_n
    integer :: ncc_nh3
    integer :: ncc_no2
    integer :: ncc_no3
    integer :: ncc_organic_p
    integer :: ncc_po4
    integer :: ncc_algae
    integer :: ncc_bod
    integer :: ncc_temp

    parameter ( &
        ncc_do=1 &
        ,ncc_organic_n=2 &
        ,ncc_nh3=3 &
        ,ncc_no2=4 &
        ,ncc_no3=5 &
        ,ncc_organic_p=6 &
        ,ncc_po4=7 &
        ,ncc_algae=8 &
        ,ncc_bod=9 &
        ,ncc_temp=10 &
        )
      


    !-----magic numbers for temperature coefficients
    !     total number of these should be equal to temp_coeff_type,
    !     so temp_coeff_type must be increased if you add more of these
    !     temp_ coefficients
    integer :: &
        temp_bod_decay
    integer :: temp_bod_set
    integer :: temp_reaer
    integer :: temp_do_ben
    integer :: temp_orgn_decay
    integer :: temp_orgn_set
    integer :: temp_nh3_decay
    integer :: temp_nh3_ben
    integer :: temp_no2_decay
    integer :: temp_orgp_decay
    integer :: temp_orgp_set
    integer :: temp_po4_ben
    integer :: temp_alg_grow
    integer :: temp_alg_resp
    integer :: temp_alg_set
    integer :: temp_alg_die


    parameter ( &
        temp_bod_decay=1 &
        ,temp_bod_set=2 &
        ,temp_reaer=3 &
        ,temp_do_ben=4 &

        ,temp_orgn_decay=5 &
        ,temp_orgn_set=6 &
        ,temp_nh3_decay=7 &
        ,temp_nh3_ben=8 &
        ,temp_no2_decay=9 &

        ,temp_orgp_decay=10 &
        ,temp_orgp_set=11 &
        ,temp_po4_ben=12 &

        ,temp_alg_grow=13 &
        ,temp_alg_resp=14 &
        ,temp_alg_set=15 &
        ,temp_alg_die=16)
     
end module



