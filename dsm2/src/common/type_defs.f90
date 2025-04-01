!!<license>
!!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
!!    Department of Water Resources.
!!    This file is part of DSM2.

!!    The Delta Simulation Model 2 (DSM2) is free software:
!!    you can redistribute it and/or modify
!!    it under the terms of the GNU General Public License as published by
!!    the Free Software Foundation, either version 3 of the License, or
!!    (at your option) any later version.

!!    DSM2 is distributed in the hope that it will be useful,
!!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!!    GNU General Public License for more details.

!!    You should have received a copy of the GNU General Public License
!!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
!!</license>

!-----type definitions for DSM2
module type_defs
    use iso_c_binding
    use constants, only: miss_val_c, miss_val_i, miss_val_r
    use gtm_precision, only: gtm_real !@# From GTM; for definition of channel_t.

    !-----encapsulate a datasource
    type datasource_t    ! Encapsulates a source for time varying data
        sequence
        real*8 value             ! Data if constant value
        integer source_type      ! Type of data source (constant, DSS, expression)
        integer indx_ptr         ! Index or pointer to data
    end type

    !-----input section maximums
    integer, parameter :: max_sections = 50
    integer, parameter :: max_fields = 30   ! max fields per  section
    integer, parameter :: max_inp_files = 2 ! max input files allowed

    !-----valid keywords type
    type form_t
        sequence
        character*32 sect      ! section name
        character*16 fld(max_fields) ! field names in section
        integer fldnum    ! number of fields in section
        logical repeat         ! true if repeating field keywords allowed
    end type

    integer, parameter :: max_xsects = 10
    integer, parameter :: max_cpn = 4

    type channel_t
        sequence
        integer*4 id           ! RDB ID number
        !@# integer*4 chan_no      ! external channel no. (map identifier)        !@# repeated below
        logical*4 inUse        ! true to use this channel
        integer xsect(max_xsects) ! channel rectangular cross section numbers
        integer length         ! channel length in feet
        !@# real*4  manning        ! manning's N coefficient        !@# repeated below
        real*4 disp           ! dispersion coefficient
        integer dist(max_xsects) ! Distance along the channel for X-Section
        real*4 BottomElev(2)  ! Bottom Elevation of each x-section defined
        integer nxsect         ! Number of rectangular X-Sections defined for this channel
        integer upnode         ! upstream node
        integer downnode       ! downstream node
        real*4 chan_dx_col         ! channel dx (distance between computational points)
        !@# vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
        !@# From GTM (common_variables.f90)
        integer :: channel_num                       !< actual channel number in DSM2 grid
        integer :: chan_no                           !< index channel number
        integer :: channel_length                    !< channel length
        integer :: up_node                           !< upstream DSM2 node
        integer :: down_node                         !< downstream DSM2 node
        integer :: up_comp                           !< upstream computational point
        integer :: down_comp                         !< downstream computational point
        integer :: start_cell                        !< starting cell
        integer :: end_cell                          !< ending cell
        real(gtm_real) :: dispersion                 !< dispersion coefficient
        real(gtm_real) :: manning                    !< Manning's n
        real(gtm_real) :: chan_btm_up                !< upstream channel bottom elevation
        real(gtm_real) :: chan_btm_down              !< downstream channel bottom elevation
        !@# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    end type

    !-----track internal and external flows at nodes and reservoirs
    integer :: max_qobj          ! maximum internal and external flows at objects
    parameter(max_qobj=10)

    type node_t
        sequence
        integer :: node_id = 0   ! external (map) node ID of this node
        ! fixme ID is probably a holdover from database id,
        ! should be node_no for consistency
        integer :: nconnect = 0       ! number of channels connected to node
        integer :: nup = 0            ! number of channels connected on chan upstream end
        integer :: ndown = 0          ! number of channels connected on chan downstream end
        integer :: boundary_type = miss_val_i       ! type of boundary condition
        integer :: upstream(max_cpn) = 0   ! upstream channel numbers
        integer :: downstream(max_cpn) = 0 ! downstream channel numbers
        integer :: qinternal(max_qobj) = 0 ! index of internal flows at node miss_val_i breaks the code
        integer :: qext(max_qobj) = 0 ! index of external flows at node
        integer :: sumQChan = miss_val_i       ! index of sum of attached channel with flow boundary
        logical*4 :: qual_int = .false.     ! if TRUE, node is a Qual "internal" node (not on grid boundary)
    end type

    !-----Right now this is for rectangular x-sects only.  Later will generalize.
    type xsect_t
        sequence
        real*8 :: width = miss_val_r        ! width of channel in feet
        real*8 :: botelv = miss_val_r       ! bottom stage (NGVD, feet)
        real*8 :: init_stage = miss_val_r   ! initial stage at cross section (NGVD, feet)
        real*8 :: init_flow = miss_val_r     ! initial flow at cross section (cfs)
        logical :: upstream = .false.        ! true if upstream xsect; false if the downstream
        integer*4 :: id = miss_val_i          ! RDB ID number
    end type

    !      MaxResConnectChannel must be consistent with maxresnodes
    integer, parameter ::  maxresnodes = 50  ! Maximum reservoir connections to channels/nodes
    integer, parameter ::  maxreselevs = 50  ! Maximum reservoir elevation/area layers

    type reservoir_t
        sequence
        character*32 :: name = ' '      ! reservoir name
        real*8 :: toparea = 0.D0           ! average top area, renamed to toparea
        real*8 :: botelv = 0.D0         ! bottom elevation wrt datum
        real*8 :: area(maxreselevs) = 0.D0           ! reservoir area at elev
        real*8 :: elev(maxreselevs) = 0.D0         !  elevation wrt datum
        real*8 :: vol(maxreselevs) = 0.D0         !  reservoir vol at elev
        real*8 :: stage = 0.D0          ! stage elevation
        real*8 :: coeff2res(maxresnodes) = 0.D0  ! to reservoir flow coefficient to node
        real*8 :: coeff2chan(maxresnodes) = 0.D0 ! to channel flow coefficient to node
        real*4 :: dummy_for_alignment
        integer*4 :: id = miss_val_i             ! RDB ID number !todo: needed?
        logical*4 :: inUse = .false.             ! true to use this reservoir
        logical*4 :: isNodeGated(maxresnodes) = .false. ! flag that a node is gated
        integer*4 :: nConnect = 0                ! number of nodes connected using reservoir connections
        integer*4 :: nnodes = 0      ! total nodes connected to this reservoir, whether by
        ! reservoir connections or gates
        integer*4 :: nelevs = 0      ! total elevation~area values
        integer*4 :: node_no(maxresnodes) = 0    ! (internal) connecting node number
        integer*4 :: first_connect_index  ! index of this reservoir, connection 1
        ! in list of all connections (starting from res 1, connect 1)
        integer*4 :: qinternal(max_qobj) = 0 ! index of internal flows at reservoir, miss_val_i will break code
        integer*4 :: qext(max_qobj) = 0 ! index of external flows at reservoir
    end type

    !-----path input (time-varying data)

    integer :: &
        max_path_const       ! maximum number of constituents associated with path

    parameter( &
        max_path_const=10 &
        )

    type pathinput_t
        sequence
        character*32 :: name = ' '
        ! name of the data stream needed to match
        ! flow and concentration paths between hydro and qual)
        character*128 :: filename = ' ' ! DSS filename
        character*32 :: variable = ' '    ! DSS C part
        character*16 :: interval = ' '  ! e.g. MIN, DAY !eli was 15, changed for alignment
        character*32 :: obj_name = ' ' !todo needed?
        character*392 path               ! DSS pathname

        real*8 :: constant_value = miss_val_r  ! constant value (instead of reading from DSS filename)
        real*8 :: value = miss_val_r            ! value for this timestep
        real*8 :: mass_frac = 1.D0               ! fraction of mass this flow takes. needed?
        real*8 :: value_in = miss_val_r         ! incoming value to check
        real*8 :: value_out = miss_val_r        ! outgoing value to change to

        integer :: value_flag                   ! data quality flag for this timestep
        integer :: fillin = miss_val_i        ! how to fill in between data (first, last, interp, data)
        integer :: locid = miss_val_i          ! location id where the input path applies (for checking duplicates)
        integer :: obj_type = miss_val_i         ! object type this data goes to: channel, reservoir, node, gate?
        integer :: obj_no = miss_val_i      ! number of object
        integer data_type      ! data type: flow, stage, gate position..
        !--------'type' section
        integer group_ndx      ! group index
        integer gate_param     ! time-varying gate parameter
        integer ndx_file       ! pointer to infilename vector
        integer*4 diff_julmin  ! path start time difference from run_start_date in minutes
        integer locnum         ! internal chan or gate device this input assigned to (+ upstream end, - downstream end)
        integer const_ndx(max_path_const) ! constituent number index
        integer n_consts       ! number of constituents
        integer no_intervals   ! e.g. 1, 15
        integer intvl_path     ! path number for this interval
        integer per_type       ! period type: per-average, instantaneous, etc.
        integer :: sign = 0    ! forced sign convention for the series
        logical :: useobj = .false.! true to use this input path
        character*14 :: start_date = ' '! path start date and time
        character*2 dummy3     ! make up for character*14 in data alignment
        logical :: replace = .false.
    end type

    !-----path output (time-varying data)

    type reserv_t      ! reservoir sub-type
        sequence
        integer node_no        ! DSM2 node number reservoir connected to
        integer hydro_node_no  ! Hydro node number
    end type

    !-----source of constituents
    type source_t
        sequence
        integer object         ! object type
        integer object_no      ! object number
        integer group_ndx      ! index to group
    end type

    integer :: &
        max_ft_flux                  ! maximum number of group residence outputs
    integer :: max_group_out                  ! maximum number of group residence outputs
    parameter( &
        max_ft_flux=100, &
         max_group_out=100 &
        )

    type pathoutput_t
        sequence
        character*130 :: filename = ' '     ! output filename
        character*6 :: dummy                ! for alignment
        character*32 :: name = ' '     !station name (b part) for path (optional)
        character*392 :: path = ' '      ! DSS pathname
        character*32 :: obj_name = ' '
        character*32 :: a_part = ' '    ! DSS A part
        character*32 :: b_part = ' '    ! DSS B part
        character*32 :: c_part = ' '    ! DSS C part
        character*32 :: e_part = ' '    ! DSS E part
        character*32 :: f_part = ' '    ! DSS F part
        character*32 :: device = ' '    ! Gate device name
        character*32 :: modifier = ' '  ! used for study name or such
        character*16 :: interval = ' '  ! e.g. MIN, DAY
        character*16 :: meas_type = ' ' ! e.g. STAGE, VELOCITY, TDS
        character*8 :: units = ' '      ! e.g. cfs, feet, umhos/cm
        integer :: obj_type = miss_val_i  ! output from which object: channel, reservoir, etc.
        integer :: ndx_file = miss_val_i      ! pointer to outfilename
        integer :: obj_no = miss_val_i      ! object number of output
        integer :: chan_dist = miss_val_i       ! distance downstream from upstream end of chan
        integer :: gate_device = miss_val_i    ! gate device
        integer :: res_node_no = miss_val_i
        integer :: reservoir_hydro_node_no = miss_val_i
        integer :: flux_from_type = miss_val_i
        integer :: flux_to_type = miss_val_i
        integer :: flux_from_ndx = miss_val_i
        integer :: flux_to_ndx = miss_val_i
        integer :: flux_group_ndx = miss_val_i
        integer :: source_group_ndx = miss_val_i ! source of constituents type
        integer :: const_ndx = miss_val_i        ! constituent number index
        integer :: no_intervals = miss_val_i     ! e.g. 1, 15
        integer :: per_type = miss_val_i         ! PER-AVER or INST-VALUE
        integer :: intvl_path = miss_val_i       ! path number for this interval
        logical*4 :: need_tmp_outfile = miss_val_i             ! true if this path should be written to tmp file
        logical :: use = .false.            ! true to use this output path
        character*20 :: dummy2    ! Alignment
    end type

    !-----input/output file names

    type io_file_t
        sequence
        logical use            ! .true. if restart/tide to be read/written
        integer unit           ! restart/tide read/write unit
        character*16 interval  ! interval for restart/tide writing (e.g. 1HOUR)
        character*130 filename ! restart/tide read/write filename
        character*6 dummy      ! alignment to multiple of 8
    end type

    !-----translations:
    !-----location name --> chan/dist pair, node, or reservoir name

    type trans_t
        sequence
        character*32 from_name ! translate from this name
        integer object         ! translate to this type of object
        integer object_no      ! translate to this object number
        character*32 obj_name  ! translate to this object name
        integer chan_dist      ! translate to this channel distance
        character*16 constituent ! translate to this constituent name
    end type

    !-----Qual Parameters

    type constituent_t
        sequence
        character*16 :: name = ' '     ! constituent name
        integer :: object = miss_val_i        ! object type of injection
        integer :: object_no = miss_val_i      ! object number of injection
        integer :: group_ndx = miss_val_i      ! index to group
        logical :: conservative = .true.   ! true if conservative, false if nonconservative
    end type

    integer :: &
        max_constituent          ! Maximum number of constituents for an external flow
    integer :: max_conqext          ! Maximum number of constituents for an external flow

    parameter( &
        max_constituent=24, &
         max_conqext=12 &
        )

    !-----external flows

    type qext_t
        sequence
        character*32 :: name = ' '     ! name of external flow (for matching flow/concentration)
        character*32 :: obj_name = ' '  ! ID of this flow (e.g. CVP, SWP, ...) needed???
        type(datasource_t) :: datasource  ! global pathnumber of flow
        character*32 :: attach_obj_name ! object info this flow is attached to
        integer :: attach_obj_type
        integer :: attach_obj_no
        !eli changed from real*4 next 5
        real*4 :: flow = 0.D0            ! external flow value for this timestep
        real*4 :: prev_flow = 0.D0       ! external flow value for previous timestep
        real*4 :: avg = 0.D0             ! external flow value averaged over tideblock time interval
        real*4 :: prev_avg = 0.D0        ! previous avg
        real*4 :: mass_frac = 1.D0       ! fraction of mass this flow takes
        integer :: changed_ndx = 0       ! index of nqext_ndx of changed flows
        integer :: group_ndx = 0         ! index to group
        character*4 :: dummy  ! for alignment
    end type

    !-----object-to-object water transfer
    !-----from- and to-object sub-type
    type from_to_t
        sequence
        character*32 :: obj_name = ' '     ! object name or...
        integer :: obj_type = miss_val_i     ! object type code (e.g. channel, node, or reservoir)
        integer :: obj_no = miss_val_i  ! ...obj_type number
        integer hydrochan      ! hydro channel number for node flow
        integer group_ndx      ! index to group
        real*4 :: mass_frac = 1.0  ! fraction of mass this flow takes
        real*4 coeff               ! Flow coefficient for stage-driven transfer
    end type

    type obj2obj_t
        sequence
        character*32 :: name = ' '     ! name of this flow (e.g. IF, HOOD_DIV)
        type(datasource_t) datasource  ! source of time varying flow
        type(from_to_t) :: from_obj   ! From object
        type(from_to_t) :: to_obj     ! To object
        integer*4 id            ! transfer ID
        real*4 constant_value   ! fixed flow value (if given), or head diff code
        real*4 flow            ! flow between the two objects at this time step
        real*4 prev_flow       ! flow between the two objects at previous time step
        real*4 flow_avg        ! Average flow between the two objects
        real*4 constituent_conc(max_constituent) ! Concentration of each constituent
        logical :: use = .false.         ! true to use this transfer
    end type

    !-----stage boundary object
    type stgbnd_t
        sequence
        character*32 name
        type(datasource_t) datasource  ! data source for value of stage boundary
        real*4 value          ! current value
        integer node          ! node at which stage boundary applies
    end type

    !-----data value, quality flags, and timestamp object
    type, bind(c) :: dataqual_t
        real*8 data
        integer*4 flag
        integer*4 julmin
    end type

    type tidefile_t
        character*16 start_date ! when to start using this tidefile (date and time)
        character*16 end_date  ! when to quit using this tidefile (date and time, or time length (e.g. 5day_3hour))
        logical binarytf       ! true for binary tidefile (not HDF5)
        integer*4 start_julmin_file ! file timestamp start
        integer*4 end_julmin_file ! file timestamp end
        integer*4 start_julmin ! when to start using this tidefile (wrt tidefile date)
        integer*4 end_julmin   ! when to quit using this tidefile (wrt tidefile date)
        integer ntideblocks    ! number of tideblocks
        integer interval       ! minutes between tideblocks
        character*128 filename ! tidefile name
    end type

end module
