!<license>
!    Copyright (C) 2013 State of California,
!    Department of Water Resources.
!    This file is part of DSM2-GTM.
!
!    The Delta Simulation Model 2 (DSM2) - General Transport Model (GTM) 
!    is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    DSM2 is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
!</license>

!> Collection of common variables required from DSM2
!> Those are variables from runtime_data.f, constants.f, 
!> io_units.f and iopath_data.f.
!>@ingroup process_io
module common_dsm2_vars

      use common_variables

    !> From DSM2/common/runtime_data.f
       character(len=:), allocatable :: dsm2_name         ! DSM2 name
       character(len=:), allocatable  :: dsm2_modifier    ! DSM2 modifier

       !-----dates, timestep
       !-----Note: julian minutes are minutes from 01jan1900 0000 (31dec1899 2400)
       integer, parameter :: max_print_dates = 10         ! previous time step in minutes
       integer:: nprints = 1                              ! number of start/stop output date/times

       integer :: prev_julmin                      ! (hydro) when to start writing tidefile, jul mins
       integer :: start_julmin                     ! (hydro) when to start writing tidefile, jul mins
       integer :: end_julmin                       ! (hydro) when to start writing tidefile, jul mins
       
       character*14 :: current_date = ' '                  ! current date/time (corresponds to julmin)
       character*14 :: run_start_date = ' '                ! date/time of start of run
       character*14 :: run_end_date = ' '                  ! date/time of end of run
       character*14 :: tf_start_date = ' '                 ! (hydro) date/time of when to start writing tidefile
       character*14 :: print_start_date(max_print_dates) = ' ' ! date/time of print starts
       character*14 :: print_end_date(max_print_dates) = ' '   ! date/time of print ends
 
       character*4 :: npartition                           ! (gtm) number of cells between two computational points

       !-----alternate method: instead of start/end dates, specify run length
       !-----in form, e.g. 5day_3hour.  Model will generate end date/times.
       character*80:: run_length = ' '

       !-----time step
       character*80:: time_step_intvl_hydro = ' '
       character*80:: time_step_intvl_qual = ' '
       character*80:: time_step_intvl_gtm = ' '

       !-----flush output interval
       character*80:: flush_intvl = '5DAY'

       !-----display time interval
       character*80:: display_intvl = ' '

       !-----titles
       integer, parameter ::  max_titles = 30              ! actual number of titles
       integer :: ntitles                                  ! actual number of titles
       character*80 ::  title(max_titles)
 
     !> From DSM2/common/constants.f
       !-----model
       integer :: dsm2_module
       integer, parameter :: hydro = 1
       integer, parameter :: qual = 2
       integer, parameter :: ptm = 3
       integer, parameter :: gtm = 4
       character*16 :: dsm2_version = '8.1.1', svn_build = '2151:2203' 

       !-----type
       integer, parameter :: io_restart = 1
       integer, parameter :: io_echo = 2
       integer, parameter :: io_animation = 3
       integer, parameter :: io_trace = 4
       integer, parameter :: io_behavior = 5
       integer, parameter :: io_group = 6
       integer, parameter :: io_hdf5 = 7

       !-----io
       integer, parameter :: io_read = 1
       integer, parameter :: io_write = 2

       !-----data types
       integer, parameter :: per_type_per_aver = 1
       integer, parameter :: per_type_per_cum = 2
       integer, parameter :: per_type_per_min = 3
       integer, parameter :: per_type_per_max = 4
       integer, parameter :: per_type_inst_val = 5
       integer, parameter :: per_type_inst_cum = 6
       integer, parameter :: per_type_null = 7

       integer, parameter :: prev_julmin_i = -902         ! integer for initializing variables
       integer, parameter :: start_file_i = -903          ! integer for initializing variables
       integer, parameter :: end_file_i = -904            ! integer for initializing variables
       integer, parameter :: fill_last =1                 ! integer for initializing variables       
       integer, parameter :: fill_interp = 2              ! integer for initializing variables
       integer, parameter :: fill_first = 3               ! integer for initializing variables
       integer, parameter :: fill_bydata = 4              ! integer for initializing variables
       integer, parameter :: init_small_i = 0             ! integer for initializing variables
       integer, parameter :: init_big_i = 9998888         ! integer for initializing variables

       !-----object type codes
       integer, parameter :: obj_channel = 1
       integer, parameter :: obj_node = 2
       integer, parameter :: obj_reservoir = 3
       integer, parameter :: obj_gate = 4
       integer, parameter :: obj_qext = 5
       integer, parameter :: obj_obj2obj = 6
       integer, parameter :: obj_flux = 7
       integer, parameter :: obj_stage = 8
       integer, parameter :: obj_null = 9
       integer, parameter :: obj_group = 22
       integer, parameter :: obj_oprule = 111
       integer, parameter :: obj_boundary_flow = 15
       integer, parameter :: obj_source_sink = 16
       integer, parameter :: obj_climate = 30
       integer, parameter :: obj_filter = 120

       
       !-----misc magic characters and numbers
       logical, parameter :: EXACT_MATCH = .true.
       integer, parameter :: TO_BOUNDARY = 1
       integer, parameter :: NEAREST_BOUNDARY = 2
       integer, parameter :: IGNORE_BOUNDARY = 3
           
    !> From DSM2/common/type_def.f
       !-----path input (time-varying data)
       integer, parameter :: max_path_const = 10     ! maximum number of constituents associated with path
    
       !-----data value, quality flags, and timestamp object
       type dataqual_t
           sequence
           real*8 data
           integer*4 flag
           integer*4 julmin
       end type

       type pathinput_t
           sequence
           character*32 :: name = ' '              ! name of the data stream needed to match
                                                   ! flow and concentration paths between hydro and qual)
           character*128 :: filename = ' '         ! DSS filename
           character*32 :: variable = ' '          ! DSS C part
           character*16 :: interval = ' '          ! e.g. MIN, DAY !eli was 15, changed for alignment
           character*32 :: obj_name  = ' '         !todo needed?
           character*80 path                       ! DSS pathname
           real*8 :: constant_value  = miss_val_r  ! constant value (instead of reading from DSS filename)
           real*8 :: value = miss_val_r            ! value for this timestep
           real*8 :: mass_frac =1.D0               ! fraction of mass this flow takes. needed?
           real*8 :: value_in = miss_val_r         ! incoming value to check
           real*8 :: value_out = miss_val_r        ! outgoing value to change to
           integer :: value_flag                   ! data quality flag for this timestep
           integer :: fillin  = miss_val_i         ! how to fill in between data (first, last, interp, data)
           integer :: locid = miss_val_i           ! location id where the input path applies (for checking duplicates)
           integer :: obj_type = miss_val_i        ! object type this data goes to: channel, reservoir, node, gate?
           integer :: obj_no = miss_val_i          ! number of object
           integer data_type                       ! data type: flow, stage, gate position..
           integer :: i_var                        ! constituent no      (to be filled later when GTM 
           integer :: i_node                       ! GTM internal node    decides index for var and node)
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
           character*14 :: start_date  = ' '! path start date and time
           character*2 dummy3     ! make up for character*14 in data alignment
           logical :: replace = .false.
       end type     
                    
    !> From DSM2/common/envvar.f   
       integer, parameter:: ENVVAR_NAME_LEN = 32
       integer, parameter:: ENVVAR_VALUE_LEN = 128
       !----pseudo (internal) environment variables
       type envvar_t
         sequence
         character (len=ENVVAR_NAME_LEN) :: name
         character (len=ENVVAR_VALUE_LEN) :: value
       end type
            
       ! max number of pseudo (internal) env vars
       integer,parameter :: max_envvars = 128
       type(envvar_t)::  envvars(max_envvars)
       integer::  nenvvars                                 ! actual number of envvars used

    !> From DSM2/common/iopath_data.f
       !-----input/output file names
       type io_file_t
           sequence
           logical use                                     ! .true. if restart/tide to be read/written
           integer unit                                    ! restart/tide read/write unit
           character*16 :: interval                        ! interval for restart/tide writing (e.g. 1HOUR)
           character*130 :: filename                       ! restart/tide read/write filename
           character*6 dummy                               ! alignment to multiple of 8
       end type
    
       integer, parameter :: max_file_types = 7            ! number of types of files (restart, tide, animation,...)
       integer, parameter :: max_iogroups = 4              ! one per model (number of types of files (restart, tide, animation,...))
       type(io_file_t):: io_files(max_iogroups,max_file_types,2)

       character(len=130) :: output_filename               ! output filename
       character(len=20) :: temp_dir= ' '                  ! directory for temporary files
       character(len=8) :: per_type_names(per_type_null)   ! data type names (e.g. 'PER-AVER')    

       ! max number of unique dss input files
       integer :: n_dssfiles = 0                           ! total number of unique dss files
       character(len=130), allocatable :: indssfiles(:)    ! unique dss input file names
       integer, allocatable :: ifltab_in(:,:)              ! DSS table for each input file       
       integer, parameter :: max_dssinfiles = 20           ! temporary dss file size, will be replaced by n_dssfiles
       character(len=130), dimension(max_dssinfiles)::infilenames= ' ' ! temporary unique dss input file names

       ! max number of unique dss output files
       integer :: n_outdssfiles = 0                        ! total number of unique dss files
       character(len=130), allocatable :: outdssfiles(:)   ! unique dss output file names
       integer, allocatable :: ifltab_out(:,:)             ! DSS table for each output file       
       integer, parameter :: max_dssoutfiles = 100         ! temporary dss file size, will be replaced by n_outdssfiles
       character(len=130), dimension(max_dssoutfiles)::outfilenames= ' ' ! temporary unique dss input file names
       
       !-----path input (time-varying data)
       integer :: ninpaths = 0
       integer :: n_inputpaths = 0                         ! total number of input paths (constant + DSS)
       type(pathinput_t), allocatable :: pathinput(:)
       
       logical :: dss_direct = .false.
       logical :: binary_output = .false.    
       logical :: output_comp_pt                           ! true to output results at computational points
       
       character(len=14), parameter :: generic_date = '01JAN3001 0000' ! generic date/time start

       !----path output dss files
       integer :: noutpaths = 0
       type pathoutput_t
           character*32 :: a_part = ' '                    ! DSS A part
           character*32 :: b_part = ' '                    ! DSS B part
           character*32 :: c_part = ' '                    ! DSS C part
           character*32 :: e_part = ' '                    ! DSS E part
           character*32 :: f_part = ' '                    ! DSS F part
           integer :: ndx_file                             ! pointer to outfilename vector
           integer :: no_intervals
           character*16 :: interval                        ! e.g. MIN, DAY
           integer :: per_type
           character*8  :: units
           character*16 :: param
           character*8  :: perop
           character*32 :: sourcegroup
           character*128 :: filename
           character*80 :: path
           character*32 :: name                           ! station name (b part) for path (optional)
           character*16 :: meas_type                      ! e.g. STAGE, VELOCITY, TDS
           character*32 :: modifier                       ! used for study name or such
           logical :: need_tmp_outfile = .false.
           integer :: channo
           integer :: distance
           integer :: intvl_path                          ! path number for this interval
       end type
       type(pathoutput_t), allocatable :: pathoutput(:)
       

     !> From DSM2/common/logging.f
       integer, parameter :: LOG_ERROR = 0
       integer, parameter :: LOG_WARNING = 1
       integer, parameter :: LOG_INFO = 2 
       integer, parameter :: LOG_DEBUG = 2
       integer :: print_level                             ! diagnostic printout level
    
     contains
       
     !> Add environmental variables into envvars(:)  
     subroutine add_envvar(name,val)
          implicit none
          integer:: j
          character (len=ENVVAR_NAME_LEN) :: name
          character (len=ENVVAR_VALUE_LEN) :: val
          if (trim(name).eq.'DSM2MODIFIER') then
              dsm2_modifier = trim(val)
          end if          
          do j = 1, nenvvars
              if (name == envvars(j)%name) then
                  envvars(j)%value =val
                  return
              endif
          enddo
          nenvvars=nenvvars+1
          if (nenvvars > max_envvars) then
            write(unit_error,'(a,i)') &
                'Too many envvars specified; max allowed is:',max_envvars
            call exit(-1)
          endif  
          envvars(nenvvars)%name=name
          envvars(nenvvars)%value=val
          return
    end subroutine
        
end module