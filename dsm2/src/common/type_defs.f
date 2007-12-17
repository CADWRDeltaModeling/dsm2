C!<license>
C!</license>

c-----type definitions for DSM2
      module type_defs

c----- basic object

      type object_t
      sequence
         integer object         ! object type of WB (e.g. res, chan, ...)
         character*32 obj_name  ! ID of particular WB (e.g. clfct, dxc, ...)
         integer object_no      ! ID number of particular WB
      end type


c-----encapsulate a datasource      
	type datasource_t    ! Encapsulates a source for time varying data
	sequence
	   real*8 value             ! Data if constant value
	   integer source_type      ! Type of data source (constant, DSS, expression)
	   integer indx_ptr         ! Index or pointer to data
	end type 




c-----input section maximums
      integer,parameter :: max_sections=50 
      integer,parameter :: max_fields=30   ! max fields per  section
      integer,parameter :: max_inp_files=1 ! max input files allowed


c-----valid keywords type
      type form_t
      sequence
         character*32 sect      ! section name
         character*16 fld(max_fields) ! field names in section          
         integer      fldnum    ! number of fields in section
         logical repeat         ! true if repeating field keywords allowed
      end type

      integer,parameter :: max_xsects = 10
      integer,parameter :: max_cpn = 4

      type channel_t
         sequence
         integer*4 id           ! RDB ID number
         integer*4 chan_no      ! external channel no. (map identifier)
         logical*4 inUse        ! true to use this channel
         integer xsect(max_xsects) ! channel rectangular cross section numbers
         integer length         ! channel length in feet
         real*4  manning        ! manning's N coefficient
         real*4  disp           ! dispersion coefficient
         integer dist(max_xsects) ! Distance along the channel for X-Section
         real*4  BottomElev(2)  ! Bottom Elevation of each x-section defined
         integer nxsect         ! Number of rectangular X-Sections defined for this channel
         integer upnode         ! upstream node
         integer downnode       ! downstream node
      end type

c-----track internal and external flows at nodes and reservoirs
      integer max_qobj          ! maximum internal and external flows at objects
      parameter (max_qobj=10)

      type node_t
      sequence
         integer node_id        ! external (map) node ID of this node
		                          ! fixme ID is probably a holdover from database id, 
								  ! should be node_no for consistency
         integer nconnect       ! number of channels connected to node
         integer nup            ! number of channels connected on chan upstream end
         integer ndown          ! number of channels connected on chan downstream end
         integer boundary_type  ! type of boundary condition
         logical*4 qual_int	  ! if TRUE, node is a Qual "internal" node (not on grid boundary)
         integer upstream(max_cpn) ! upstream channel numbers
         integer downstream(max_cpn) ! downstream channel numbers
         integer qint(max_qobj) ! index of internal flows at node
         integer qext(max_qobj) ! index of external flows at node
	   integer sumQChan       ! index of sum of attached channel with flow boundary
      end type

c-----Right now this is for rectangular x-sects only.  Later will generalize.
      type xsects_t
      sequence
         real*8    width        ! width of channel in feet
         real*8    botelv       ! bottom stage (NGVD, feet)
         real*8    init_stage   ! initial stage at cross section (NGVD, feet)
         real*8    init_flow    ! initial flow at cross section (cfs)
         logical upstream       ! true if upstream xsect; false if the downstream         
         integer*4 id           ! RDB ID number
      end type

c      MaxResConnectChannel must be consistent with maxresnodes
      integer, parameter ::  maxresnodes=15  ! Maximum reservoir connections to channels/nodes

      type reservoir_t
      sequence
         character*32 name      ! reservoir name
         real*8 area            ! average top area
         real*8 botelv          ! bottom elevation wrt datum
         real*8 stage           ! stage elevation
         real*8 coeff2res(maxresnodes) ! to reservoir flow coefficient to node
         real*8 coeff2chan(maxresnodes) ! to channel flow coefficient to node
         integer*4 id           ! RDB ID number
         logical*4 inUse        ! true to use this reservoir
         logical*4 isNodeGated(maxresnodes) ! flag that a node is gated
         integer*4 nConnect     ! number of nodes connected using reservoir connections
         integer*4 nnodes       ! total nodes connected to this reservoir, whether by
                                ! reservoir connections or gates
         integer*4 node_no(maxresnodes) ! (internal) connecting node number
         integer*4 qint(max_qobj) ! index of internal flows at reservoir
         integer*4 qext(max_qobj) ! index of external flows at reservoir
      end type

c-----path input (time-varying data)

      integer
     &     max_path_const       ! maximum number of constituents associated with path

      parameter (
     &     max_path_const=10
     &     )

      type pathinput_t
      sequence
         character*32 name      ! name of the data stream needed to match
                                ! flow and concentration paths between hydro and qual)
c--------"from" tags: construct the DSS pathname using a-f parts, or give
c--------IEP-style information
c--------if a-f parts are not input, these will be constructed from the
c--------IEP tags
         character*128 filename ! DSS filename
         character*32 a_part    ! DSS A part
         character*32 b_part    ! DSS B part
         character*32 c_part    ! DSS C part
         character*32 e_part    ! DSS E part
         character*32 f_part    ! DSS F part
         character*32 ID        ! ID or study name string
         character*15 interval  ! e.g. MIN, DAY
         character*1 dummy1     ! make up for character*15 in type alignment
         character*32 object_name
         character*80 path      ! DSS pathname (constructed from a-f parts)
         
         real*8 constant_value  ! constant value (instead of reading from DSS filename)
         real*8 value           ! value for this timestep
         real*8 mass_frac       ! fraction of mass this flow takes
         real*8 value_in        ! incoming value to check
         real*8 value_out       ! outgoing value to change to         

         integer*4 value_flag   ! data quality flag for this timestep
c--------other values that may be input by user
         integer fillin         ! how to fill in between data (first, last, interp, data)
         integer priority       ! priority to use for replacing data at a location
         integer locid          ! location id where the input path applies (for checking duplicates)
         integer object         ! object type this data goes to: channel, reservoir, node, gate?
         integer object_no      ! number of object
         integer data_type      ! data type: flow, stage, gate position..
c--------'type' section
         integer group_ndx      ! group index
         integer*4 use_flag     ! data quality flag to use for this path
         integer gate_param     ! time-varying gate parameter
         integer ndx_file       ! pointer to infilename vector
         integer*4 diff_julmin  ! path start time difference from run_start_date in minutes
         integer locnum         ! internal chan or gate device this input assigned to (+ upstream end, - downstream end)
         integer const_ndx(max_path_const) ! constituent number index
         integer n_consts       ! number of constituents
         integer no_intervals   ! e.g. 1, 15
         integer intvl_path     ! path number for this interval
         integer per_type       ! period type: per-average, instantaneous, etc.
         logical replace        ! used with old "priority" system, needed to compile
         logical sync           ! needed for backward compatibility
         logical use            ! true to use this input path         
         character*1 sign       ! sign to change value to (+, -, or ' ' for none)
	   character*7 dummy3     ! make up for character*1 in data alignment
         character*14 start_date ! path start date and time
	   character*2 dummy2     ! make up for character*14 in data alignment

      end type

c-----path output (time-varying data)

      type reserv_t      ! reservoir sub-type
      sequence
         integer node_no        ! DSM2 node number reservoir connected to
         integer hydro_node_no  ! Hydro node number
      end type


c-----source of constituents
      type source_t
      sequence
         integer object         ! object type
         integer object_no      ! object number
         integer group_ndx      ! index to group
      end type

      integer
     &     max_ft_flux          ! maximum number of from-to fluxes
     &    ,max_group_out		  ! maximum number of group residence outputs
      parameter (
     &     max_ft_flux=100
     &    ,max_group_out=100
     &     )

      type pathoutput_t
      sequence
         character*80 path      ! DSS pathname
         character*32 object_name         
         character*32 name      !station name (b part) for path (optional)
         character*32 a_part    ! DSS A part
         character*32 b_part    ! DSS B part
         character*32 c_part    ! DSS C part
         character*32 e_part    ! DSS E part
         character*32 f_part    ! DSS F part
         character*32 device    ! Gate device name
         character*32 modifier  ! used for study name or such         
         character*16 interval  ! e.g. MIN, DAY
         character*16 meas_type ! e.g. STAGE, VELOCITY, TDS         
         character*8 units      ! e.g. cfs, feet, umhos/cm              
         integer object         ! output from which object: channel, reservoir, etc.
         integer ndx_file       ! pointer to outfilename         
         integer object_no      ! object number of output
         integer chan_dist      ! distance downstream from upstream end of chan
         integer gate_device    ! gate device
         integer res_node_no
         integer reservoir_hydro_node_no
c--------type(reserv_t) reservoir ! reservoir sub-type
         integer flux_from_type
	   integer flux_to_type
	   integer flux_from_ndx
         integer flux_to_ndx
	   integer flux_group_ndx
         integer source_group_ndx ! source of constituents type
         integer const_ndx      ! constituent number index
         integer no_intervals   ! e.g. 1, 15
         integer per_type       ! PER-AVER or INST-VALUE
         integer intvl_path     ! path number for this interval
         logical*4 need_tmp_outfile ! true if this path should be written to tmp file
         character*130 filename ! output filename
         logical use            ! true to use this output path         
      end type

c-----pseudo (internal) environment variables
      type envvar_t
      sequence
         character*130 name
         character*130 value
      end type

c-----input/output file names

      type io_file_t
      sequence
         logical use            ! .true. if restart/tide to be read/written
         integer unit           ! restart/tide read/write unit
         character*80 interval  ! interval for restart/tide writing (e.g. 1HOUR)
         character*130 filename ! restart/tide read/write filename
         character*42 dummy
      end type

c-----translations:
c-----location name --> chan/dist pair, node, or reservoir name

      type trans_t
      sequence
         character*32 from_name ! translate from this name
         integer object         ! translate to this type of object
         integer object_no      ! translate to this object number
         character*32 obj_name  ! translate to this object name
         integer chan_dist      ! translate to this channel distance
         character*16 constituent ! translate to this constituent name
      end type

C-----Qual Parameters

      type constituents_t
      sequence
         logical conservative   ! true if conservative, false if nonconservative
         character*16 name      ! constituent name
         integer object         ! object type of injection
         integer object_no      ! object number of injection
         integer group_ndx      ! index to group
      end type

      integer
     &     max_constituent      ! Maximum number of constituents
     &     ,max_conqext         ! Maximum number of constituents for an external flow

      parameter (
     &     max_constituent=24
     &     ,max_conqext=12
     &     )

c-----external flows

      type qext_t
      sequence
         character*32 name      ! name of external flow (for matching flow/concentration)
         character*32 obj_name  ! ID of this flow (e.g. CVP, SWP, ...) needed???
         type(datasource_t) datasource  ! global pathnumber of flow
         type(object_t) attach ! object info this flow is attached to
         real*4 flow            ! external flow value for this timestep
         real*4 prev_flow       ! external flow value for previous timestep
         real*4 avg             ! external flow value averaged over tideblock time interval
         real*4 prev_avg        ! previous avg
         real*4 mass_frac       ! fraction of mass this flow takes
         integer changed_ndx    ! index of nqext_ndx of changed flows
         integer group_ndx      ! index to group
      end type

c-----object-to-object water transfer
c-----from- and to-object sub-type
      type from_to_t
      sequence
         integer object         ! object type code (e.g. channel, node, or reservoir)
         character*32 obj_name  ! object name or...
         integer object_no      ! ...object number
         integer hydrochan      ! hydro channel number for node flow
         integer group_ndx      ! index to group
         real*4 mass_frac       ! fraction of mass this flow takes
         real*4 coeff           ! Flow coefficient for stage-driven transfer
      end type

      type obj2obj_t
      sequence
         character*32 name      ! name of this flow (e.g. IF, HOOD_DIV)
	   type(datasource_t) datasource  ! source of time varying flow
         type(from_to_t) from   ! From object
         type(from_to_t) to     ! To object
         integer*4 ID           ! transfer ID         
         real*4 constant_value  ! fixed flow value (if given), or head diff code
         real*4 flow            ! flow between the two objects at this time step
         real*4 prev_flow       ! flow between the two objects at previous time step
         real*4 flow_avg        ! Average flow between the two objects
         real*4 constituent_conc(max_constituent) ! Concentration of each constituent
         logical use            ! true to use this transfer
      end type

c-----stage boundary object
      type stgbnd_t
      sequence
         character*32 name
	   type(datasource_t) datasource  ! data source for value of stage boundary
	   real*4 value          ! current value
         integer node          ! node at which stage boundary applies
      end type

c-----data value, quality flags, and timestamp object
      type dataqual_t
      sequence
         real*8 data
         integer*4 flag
         integer*4 julmin
      end type
      
      end module