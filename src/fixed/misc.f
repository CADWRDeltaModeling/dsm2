C!    Copyright (C) 1996, 1997, 1998 State of California,
C!    Department of Water Resources.
C!
C!    Delta Simulation Model 2 (DSM2): A River, Estuary, and Land
C!    numerical model.  No protection claimed in original FOURPT and
C!    Branched Lagrangian Transport Model (BLTM) code written by the
C!    United States Geological Survey.  Protection claimed in the
C!    routines and files listed in the accompanying file "Protect.txt".
C!    If you did not receive a copy of this file contact Dr. Paul
C!    Hutton, below.
C!
C!    This program is licensed to you under the terms of the GNU General
C!    Public License, version 2, as published by the Free Software
C!    Foundation.
C!
C!    You should have received a copy of the GNU General Public License
C!    along with this program; if not, contact Dr. Paul Hutton, below,
C!    or the Free Software Foundation, 675 Mass Ave, Cambridge, MA
C!    02139, USA.
C!
C!    THIS SOFTWARE AND DOCUMENTATION ARE PROVIDED BY THE CALIFORNIA
C!    DEPARTMENT OF WATER RESOURCES AND CONTRIBUTORS "AS IS" AND ANY
C!    EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
C!    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
C!    PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE CALIFORNIA
C!    DEPARTMENT OF WATER RESOURCES OR ITS CONTRIBUTORS BE LIABLE FOR
C!    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
C!    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
C!    OR SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS; OR
C!    BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
C!    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
C!    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
C!    USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
C!    DAMAGE.
C!
C!    For more information about DSM2, contact:
C!
C!    Dr. Paul Hutton
C!    California Dept. of Water Resources
C!    Division of Planning, Delta Modeling Section
C!    1416 Ninth Street
C!    Sacramento, CA  95814
C!    916-653-5601
C!    hutton@water.ca.gov
C!
C!    or see our home page: http://wwwdelmod.water.ca.gov/

c-----miscellaneous values
c-----For compilation using 8 or 4 byte precision. Use 4 for compatibility with Qual
      integer REAL_PRECISION
      parameter(REAL_PRECISION=8)

 


c-----index numbers for channels & nodes
      integer
     &     chan_no
     &     ,length
     &     ,manning
     &     ,disp
     &     ,boundary_type
     &     ,xsect
     &     ,node_no
     &     ,upnode
     &     ,downnode
     &     ,dist
      parameter (
     &     chan_no=1
     &     ,length=2
     &     ,manning=3
     &     ,disp=4
     &     ,boundary_type=5
     &     ,xsect=6
     &     ,node_no=7
     &     ,upnode=8
     &     ,downnode=9
     &     ,dist=10
     &     )

c-----index numbers for cross sections
      integer
     &     x_no                 ! cross section number
     &     ,x_width             ! rectangular channel width (feet)
     &     ,x_botelev           ! bottom stage (feet wrt NGVD)
     &     ,x_up                ! true if upstream cross section
     &     ,x_init_stage        ! initial stage (NGVD feet)
     &     ,x_init_flow         ! initial flow (cfs)
      parameter (
     &     x_no=1
     &     ,x_width=2
     &     ,x_botelev=3
     &     ,x_up=4
     &     ,x_init_stage=5
     &     ,x_init_flow=6
     &     )

c-----irregular geometry numbers
      integer
     &     irg_chan             ! channel number
     &     ,irg_dist            ! distance along channel
     &     ,irg_fn              ! filename containing geom viewer xsect output
                                ! for this cross section
      parameter (
     &     irg_chan=1
     &     ,irg_dist=2
     &     ,irg_fn=3
     &     )

c-----index numbers for reservoirs
      integer
     &     res_name
     &     ,res_area
     &     ,res_stage
     &     ,res_botelv
     &     ,res_node
     &     ,res_coeff2res
     &     ,res_coeff2chan
      parameter (
     &     res_name=1
     &     ,res_area=2
     &     ,res_stage=3
     &     ,res_botelv=4
     &     ,res_node=5
     &     ,res_coeff2res=6
     &     ,res_coeff2chan=7
     &     )

c-----object-to-object connections
      integer
     &     obj2obj_from_objtype
     &     ,obj2obj_from_objname
     &     ,obj2obj_to_objtype
     &     ,obj2obj_to_objname
     &     ,obj2obj_pathinput_label
     &     ,obj2obj_flow        ! for fixed flow value
     &     ,obj2obj_poscoeff    ! for stage-driven flow
     &     ,obj2obj_negcoeff    ! for stage-driven flow
     &     ,obj2obj_groupname    ! accounting name (e.g. 'diversion', 'drainage')
     &     ,obj2obj_objname     ! internal flow ID name (e.g. 'IF', 'HOOD_DIV')

      parameter (
     &     obj2obj_from_objtype=1
     &     ,obj2obj_from_objname=2
     &     ,obj2obj_to_objtype=3
     &     ,obj2obj_to_objname=4
     &     ,obj2obj_pathinput_label=5
     &     ,obj2obj_flow=6
     &     ,obj2obj_poscoeff=7
     &     ,obj2obj_negcoeff=8
     &     ,obj2obj_groupname=9
     &     ,obj2obj_objname=10
     &     )

c-----index numbers for gates

      integer, parameter :: gate_operation=22
      integer, parameter :: gate_op_to_node=221
      integer, parameter :: gate_op_from_node=222
      integer, parameter :: gate_install=128
	integer, parameter :: gate_position=129

c-----constants representing different kinds of data sources
      integer
     &     const_data              ! constant scalar
     &     ,dss_data               ! data from dss file
     &     ,expression_data       ! data from operating rule expression

      parameter (
     &     const_data=128
     &     ,dss_data=256
     &     ,expression_data=512
     &     )






c-----index numbers for path input
      integer
     &     inpath_label         ! translation label, or gate/reservoir name
     &     ,inpath_node
     &     ,inpath_a_part
     &     ,inpath_b_part
     &     ,inpath_c_part
     &     ,inpath_e_part
     &     ,inpath_f_part
     &     ,inpath_ID
     &     ,inpath_meas_type
     &     ,inpath_interval
     &     ,inpath_filename     ! DSS input filename, or...
     &     ,inpath_value        ! ...use constant value
     &     ,inpath_fillin
     &     ,inpath_priority
     &     ,inpath_sdate        ! start date for data
     &     ,inpath_stime        ! start time for data

      parameter (
     &     inpath_label=1
     &     ,inpath_node=2
     &     ,inpath_a_part=3
     &     ,inpath_b_part=4
     &     ,inpath_c_part=5
     &     ,inpath_e_part=6
     &     ,inpath_f_part=7
     &     ,inpath_ID=12
     &     ,inpath_meas_type=13
     &     ,inpath_interval=14
     &     ,inpath_filename=15
     &     ,inpath_value=16
     &     ,inpath_fillin=17
     &     ,inpath_priority=18
     &     ,inpath_sdate=19
     &     ,inpath_stime=20
     &     )

c-----index numbers for printout
      integer
     &     outpath_filename
     &     ,outpath_a_part
     &     ,outpath_b_part
     &     ,outpath_c_part
     &     ,outpath_e_part
     &     ,outpath_f_part
     &     ,outpath_name
     &     ,outpath_chan
     &     ,outpath_dist
     &     ,outpath_node
     &     ,outpath_res_name
     &     ,outpath_res_node
     &     ,outpath_type
     &     ,outpath_interval
     &     ,outpath_period
     &     ,outpath_modifier
     &     ,outpath_from_name
     &     ,outpath_source_group
     &     ,outpath_variable
     &     ,outpath_fromwb      ! from waterbody (PTM)
     &     ,outpath_towb

      parameter (
     &     outpath_filename=1
     &     ,outpath_a_part=2
     &     ,outpath_b_part=3
     &     ,outpath_c_part=4
     &     ,outpath_e_part=5
     &     ,outpath_f_part=6
     &     ,outpath_name=7
     &     ,outpath_chan=8
     &     ,outpath_dist=9
     &     ,outpath_node=10
     &     ,outpath_res_name=11
     &     ,outpath_res_node=12
     &     ,outpath_type=13
     &     ,outpath_interval=14
     &     ,outpath_period=15
     &     ,outpath_modifier=16
     &     ,outpath_from_name=17
     &     ,outpath_source_group=18
     &     ,outpath_variable=19
     &     ,outpath_fromwb=20
     &     ,outpath_towb=21
     &     )

c-----"internal/pseudo" environment variables
      integer
     &     envvar_name
     &     ,envvar_value

      parameter (
     &     envvar_name=1
     &     ,envvar_value=2
     &     )

c-----magic numbers for i/o files section
      integer
     &     io_model
     &     ,io_type
     &     ,io_io
     &     ,io_interval
     &     ,io_filename

      parameter (
     &     io_model=1
     &     ,io_type=2
     &     ,io_io=3
     &     ,io_interval=4
     &     ,io_filename=5
     &     )

c-----model
      integer
     &     hydro
     &     ,qual
     &     ,ptm

      parameter (
     &     hydro=1
     &     ,qual=2
     &     ,ptm=3
     &     )

c-----type
      integer
     &     io_restart
     &     ,io_tide
     &     ,io_animation
     &     ,io_trace
     &     ,io_behavior
     &     ,io_group
     &	 ,io_hdf5

      parameter(
     &     io_restart=1
     &     ,io_tide=2
     &     ,io_animation=3
     &     ,io_trace=4
     &     ,io_behavior=5
     &     ,io_group=6
     &     ,io_hdf5=7
     &     )

c-----io
      integer
     &     io_read
     &     ,io_write

      parameter(
     &     io_read=1
     &     ,io_write=2
     &     )

c-----quad integration index numbers
      integer
     &     q_pt
     &     ,q_wt
      parameter (
     &     q_pt=1
     &     ,q_wt=2
     &     )

c-----location name --> chan/dist pair translation
      integer
     &     trans_name
     &     ,trans_chan
     &     ,trans_dist
     &     ,trans_node
     &     ,trans_res
     &     ,trans_gate
     &     ,trans_const
      parameter (
     &     trans_name=1
     &     ,trans_chan=2
     &     ,trans_dist=3
     &     ,trans_node=4
     &     ,trans_res=5
     &     ,trans_gate=6
     &     ,trans_const=7
     &     )

c-----assign types to input paths
      integer
     &     type_string
     &     ,type_part
     &     ,type_match
     &     ,type_sign
     &     ,type_groupname
     &     ,type_massfrac
     &     ,type_value_in
     &     ,type_value_out
     &     ,type_value_flag

      parameter (
     &     type_string=1
     &     ,type_part=2
     &     ,type_match=3
     &     ,type_sign=4
     &     ,type_groupname=5
     &     ,type_massfrac=6
     &     ,type_value_in=7
     &     ,type_value_out=8
     &     ,type_value_flag=9
     &     )


c-----magic numbers for tidefile input

      integer
     &     tide_sdate
     &     ,tide_stime
     &     ,tide_edate
     &     ,tide_etime
     &     ,tide_fname

      parameter (
     &     tide_sdate=1
     &     ,tide_stime=2
     &     ,tide_edate=3
     &     ,tide_etime=4
     &     ,tide_fname=5
     &     )

c-----magic numbers for qual binary file input

      integer
     &     binary_fname

      parameter (
     &     binary_fname=1
     &     )

c-----channel coefficients

      integer
     &     coeff_chan
     &     ,coeff_res
     &     ,coeff_type
     &     ,coeff_const
     &     ,coeff_value

      parameter (
     &     coeff_chan=1
     &     ,coeff_res=2
     &     ,coeff_type=3
     &     ,coeff_const=4
     &     ,coeff_value=5
     &     )

      integer*4
     &     TIDE_START           ! get just first tide block
     &     ,TIDE_LENGTH         ! go to last tide block

      parameter (
     &     TIDE_START=1357
     &     ,TIDE_LENGTH=2468
     &     )

c-----boundary types
      integer
     &     stage_boundary
     &     ,flow_boundary
     &     ,gate_boundary
c     &     ,unknown_type
c-----fixme: fill in other data types later

      parameter (
     &     flow_boundary=1
     &     ,stage_boundary=2
     &     ,gate_boundary=3
c     &     ,unknown_type=4
     &     )

c-----data quality flags, bit positions
      integer
     &     SCREENED_DATA        ! screened data flag
     &     ,GOOD_DATA           ! good data flag
     &     ,MISSING_DATA        ! missing data flag
     &     ,QUESTION_DATA       ! questionable data flag
     &     ,REJECT_DATA         ! rejected data flag
     &     ,MISS_OR_REJ_DATA    ! missing or rejected data flag
     &     ,SCREENED_BIT        ! screened data bit position
     &     ,MISSING_BIT         ! missing data bit position
     &     ,GOOD_BIT            ! good data bit position
     &     ,QUESTION_BIT        ! questionable data bit position
     &     ,REJECT_BIT          ! rejected data bit position

      parameter (
     &     SCREENED_DATA=100
     &     ,GOOD_DATA=101
     &     ,MISSING_DATA=102
     &     ,QUESTION_DATA=103
     &     ,REJECT_DATA=104
     &     ,MISS_OR_REJ_DATA=110
     &     ,SCREENED_BIT=0
     &     ,GOOD_BIT=1
     &     ,MISSING_BIT=2
     &     ,QUESTION_BIT=3
     &     ,REJECT_BIT=4
     &     )

c-----object type codes
      integer
     &     obj_channel
     &     ,obj_node
     &     ,obj_reservoir
     &     ,obj_gate
     &     ,obj_qext            ! external flow (sourc/sink)
     &     ,obj_obj2obj         ! object-to-object flow
     &     ,obj_flux            ! qual or ptm mass or particle flux
     &     ,obj_stage           ! stage boundary
     &     ,obj_null            ! null object
     &     ,obj_group           ! group of objects
     &     ,obj_oprule
     &     ,obj_boundary_flow
     &     ,obj_source_sink
     &     ,obj_climate


      parameter (
     &     obj_channel=1
     &     ,obj_node=2
     &     ,obj_reservoir=3
     &     ,obj_gate=4
     &     ,obj_qext=5
     &     ,obj_obj2obj=6
     &     ,obj_flux=7
     &     ,obj_stage=8
     &     ,obj_null=9
     &     ,obj_boundary_flow=15
     &     ,obj_source_sink=16
     &     ,obj_group=22
     &     ,obj_climate=30
     &     ,obj_oprule=111
     &     )

c-----data types
      integer
     &     per_type_per_aver   ! period average
     &     ,per_type_per_cum   ! period cumulative
     &     ,per_type_per_min   ! period minimum
     &     ,per_type_per_max   ! period maximum
     &     ,per_type_inst_val  ! instantaneous value
     &     ,per_type_inst_cum  ! instantaneous cumulative
     &     ,per_type_null

      parameter (
     &     per_type_per_aver=1
     &     ,per_type_per_cum=2
     &     ,per_type_per_min=3
     &     ,per_type_per_max=4
     &     ,per_type_inst_val=5
     &     ,per_type_inst_cum=6
     &     ,per_type_null=7
     &     )

c-----coefficient type codes
      integer
     &     decay
     &     ,settle
     &     ,benthic
     &     ,alg_grow
     &     ,alg_resp
     &     ,alg_die

      parameter (
     &     decay=1
     &     ,settle=2
     &     ,benthic=3
     &     ,alg_grow=4
     &     ,alg_resp=5
     &     ,alg_die=6)

c-----misc magic characters and numbers

      logical
     &     EXACT_MATCH          ! search for exactly matching string
     &     ,SUBSTR_MATCH        ! search for substring match

      integer
     &     TO_BOUNDARY          ! increment an interval to boundary
     &     ,NEAREST_BOUNDARY    ! increment an interval to nearest boundary
     &     ,IGNORE_BOUNDARY     ! ignore boundary
     &     ,TO_OBJ              ! return structure with flow to object
     &     ,FROM_OBJ            ! return structure with flow from object
     &     ,ALL_FLOWS           ! all qext, obj2obj, and reservoir connection flows
     &     ,NO_CONNECT          ! all qext, obj2obj, but no reservoir connection flows
     &     ,QEXT_FLOWS          ! only qext flows
     &     ,QINT_FLOWS          ! only obj2obj (internal) flows


      parameter (
     &     EXACT_MATCH=.true.
     &     ,SUBSTR_MATCH=.false.
     &     ,TO_BOUNDARY=1
     &     ,NEAREST_BOUNDARY=2
     &     ,IGNORE_BOUNDARY=3
     &     ,TO_OBJ=1
     &     ,FROM_OBJ=2
     &     ,ALL_FLOWS=0
     &     ,NO_CONNECT=-100
     &     ,QEXT_FLOWS=-200
     &     ,QINT_FLOWS=-300
     &     )

c
c-----input stuff

      character
     &     delimiter*1          ! field delimiter character
     &     ,empty_field*1       ! empty string field
     &     ,iep_sep*1           ! IEP identifier separator
     &     ,backslash*1         ! backslash character
      parameter (
     &     delimiter='|'
     &     ,empty_field=' '
     &     ,iep_sep='+'
c     &     ,backslash='\\'      !! <UNIX>
     &     ,backslash='\'       !! <NT>
     &     )

      character*14 generic_date   ! generic date/time start
      parameter (generic_date='01JAN3001 0000')

      integer
     &     chan_length          ! indicates use full channel length
     &     ,chan_mid            ! indicates use 1/2 channel length
     &     ,chan_up             ! indicates upstream end of channel
     &     ,chan_down           ! indicates downstream end of channel
     &     ,miss_val_i          ! integer missing value marker
     &     ,prev_julmin_i       ! integer marker to set first julmin of current
                                !   tide file to ending julmin of previous file +1
     &     ,start_file_i        ! integer marker to set first julmin of current
                                !   tide file to first julmin of the file
     &     ,end_file_i          ! integer marker to set first julmin of current
                                !   tide file to last julmin of the file
     &     ,fill_interp         ! indicates interpolate between data values
     &     ,fill_first          ! indicates use first value for all times
     &     ,fill_last           ! indicates use last data value (default)
     &     ,fill_bydata         ! indicates use DSS data type:
                                !   interp for INST-VAL
                                !   last for PER-AVER
     &     ,init_small_i        ! integer for initializing structures
     &     ,init_big_i          ! integer for initializing variables

      real*8
     &     head_diff            ! instead of a fixed flow, calc from head diff

      parameter (
     &     chan_length= -99999999
     &     ,chan_mid=88888
     &     ,chan_up=77771
     &     ,chan_down=77770
     &     ,miss_val_i=-901
     &     ,prev_julmin_i=-902
     &     ,start_file_i=-903
     &     ,end_file_i=-904
     &     ,fill_last=1
     &     ,fill_bydata=2
     &     ,fill_first=3
     &     ,fill_interp=4
     &     ,init_small_i=0
     &     ,init_big_i=9998888
     &     ,head_diff=-9998888.
     &     )

      real*8
     &     miss_val_r           ! missing value marker
     &     ,init_small_r        ! for initializing irreg_geom structures
     &     ,init_big_r          ! for initializing irreg_geom structures
      parameter (
     &     miss_val_r=-901.
     &     ,init_small_r=-99999.0
     &     ,init_big_r=99999.0
     &     )

      character*1
     &     miss_val_c           ! missing character marker
      parameter (
     &     miss_val_c=char(1)
     &     )

c-----magic numbers for non-conservative constituents
      integer
     &     ncc_do
     &     ,ncc_organic_n
     &     ,ncc_nh3
     &     ,ncc_no2
     &     ,ncc_no3
     &     ,ncc_organic_p
     &     ,ncc_po4
     &     ,ncc_algae
     &     ,ncc_bod
     &     ,ncc_temp

      parameter (
     &     ncc_do=1
     &     ,ncc_organic_n=2
     &     ,ncc_nh3=3
     &     ,ncc_no2=4
     &     ,ncc_no3=5
     &     ,ncc_organic_p=6
     &     ,ncc_po4=7
     &     ,ncc_algae=8
     &     ,ncc_bod=9
     &     ,ncc_temp=10
     &     )
      


c-----magic numbers for temperature coefficients

      integer
     &     temp_bod_decay
     &     ,temp_bod_set
     &     ,temp_reaer
     &     ,temp_do_ben

     &     ,temp_orgn_decay
     &     ,temp_orgn_set
     &     ,temp_nh3_decay
     &     ,temp_nh3_ben
     &     ,temp_no2_decay

     &     ,temp_orgp_decay
     &     ,temp_orgp_set
     &     ,temp_po4_ben

     &     ,temp_alg_grow
     &     ,temp_alg_resp
     &     ,temp_alg_set
     &     ,temp_alg_die

      parameter (
     &     temp_bod_decay=1
     &     ,temp_bod_set=2
     &     ,temp_reaer=3
     &     ,temp_do_ben=4

     &     ,temp_orgn_decay=5
     &     ,temp_orgn_set=6
     &     ,temp_nh3_decay=7
     &     ,temp_nh3_ben=8
     &     ,temp_no2_decay=9

     &     ,temp_orgp_decay=10
     &     ,temp_orgp_set=11
     &     ,temp_po4_ben=12

     &     ,temp_alg_grow=13
     &     ,temp_alg_resp=14
     &     ,temp_alg_set=15
     &     ,temp_alg_die=16)



