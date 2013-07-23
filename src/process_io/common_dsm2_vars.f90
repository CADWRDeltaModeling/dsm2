

!> Collection of common variables required from DSM2
!> Those are variables from runtime_data.f, constants.f, 
!> io_units.f and iopath_data.f.
!>@ingroup process_io
module common_dsm2_vars

    !> From DSM2/common/runtime_data.f
       character(len=:), allocatable :: dsm2_name

       !-----dates, timestep
       !-----Note: julian minutes are minutes from 01jan1900 0000 (31dec1899 2400)
       integer, parameter :: max_print_dates = 10 ! previous time step in minutes
       integer :: time_step                       ! previous time step in minutes
       integer :: prev_time_step                  ! previous time step in minutes
       integer:: nprints = 1                      ! number of start/stop output date/times

       integer*4 :: julmin                        ! (hydro) when to start writing tidefile, jul mins
       integer*4 :: prev_julmin                   ! (hydro) when to start writing tidefile, jul mins
       integer*4 :: start_julmin                  ! (hydro) when to start writing tidefile, jul mins
       integer*4 :: end_julmin                    ! (hydro) when to start writing tidefile, jul mins
       integer*4 :: jul_generic_date              ! (hydro) when to start writing tidefile, jul mins
       integer*4 :: tf_start_julmin               ! (hydro) when to start writing tidefile, jul mins     
       
       character*14:: current_date = ' '          ! current date/time (corresponds to julmin)
       character*14:: run_start_date = ' '        ! date/time of start of run
       character*14:: run_end_date = ' '          ! date/time of end of run
       character*14:: tf_start_date = ' '         ! (hydro) date/time of when to start writing tidefile
       character*14:: print_start_date(max_print_dates) = ' ' ! date/time of print starts
       character*14:: print_end_date(max_print_dates) = ' '   ! date/time of print ends
 
       character*4 :: npartition                  ! (gtm) number of cells between two computational points

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
       integer, parameter ::  max_titles = 30   ! actual number of titles
       integer :: ntitles                       ! actual number of titles
       character*80 ::  title(max_titles)
     
    !> From DSM2/common/constants.f
       !-----model
       integer :: dsm2_module
       integer, parameter :: hydro = 1
       integer, parameter :: qual = 2
       integer, parameter :: ptm = 3
       integer, parameter :: gtm = 4

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

       integer, parameter :: miss_val_i = -901            ! integer for initializing variables
       integer, parameter :: prev_julmin_i = -902         ! integer for initializing variables
       integer, parameter :: start_file_i = -903          ! integer for initializing variables
       integer, parameter :: end_file_i = -904            ! integer for initializing variables
       integer, parameter :: fill_last =1                 ! integer for initializing variables       
       integer, parameter :: fill_interp = 2              ! integer for initializing variables
       integer, parameter :: fill_first = 3               ! integer for initializing variables
       integer, parameter :: fill_bydata = 4              ! integer for initializing variables
       integer, parameter :: init_small_i = 0             ! integer for initializing variables
       integer, parameter :: init_big_i = 9998888         ! integer for initializing variables

       real*8, parameter :: miss_val_r = -901.            ! for initializing irreg_geom structures
       real*8, parameter :: init_small_r = -99999.0       ! for initializing irreg_geom structures
       real*8, parameter :: init_big_r = 99999.0          ! for initializing irreg_geom structures   
                
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
       integer::  nenvvars    ! actual number of envvars used
       
       
    !> From DSM2/common/io_units.f
       integer, parameter :: unit_error=0     ! error messages
       integer, parameter :: unit_input=11    ! input unit
       integer, parameter :: unit_screen=6    ! output unit to screen (MUST be 6)
       integer, parameter :: unit_output=14   ! output file
       integer, parameter :: unit_text=13     ! temporary (scratch) text file output
       integer, save :: unit_hydro

    !> From DSM2/common/iopath_data.f
       !-----input/output file names
       type io_file_t
           sequence
           logical use            ! .true. if restart/tide to be read/written
           integer unit           ! restart/tide read/write unit
           character*16 interval  ! interval for restart/tide writing (e.g. 1HOUR)
           character*130 filename ! restart/tide read/write filename
           character*6 dummy      ! alignment to multiple of 8
       end type
    
       integer, parameter :: max_file_types = 7     ! number of types of files (restart, tide, animation,...)
       integer, parameter :: max_iogroups = 4       ! one per model (number of types of files (restart, tide, animation,...))
       type(io_file_t):: io_files(max_iogroups,max_file_types,2)

       character(len=130) :: output_filename              ! output filename
       character(len=20) :: temp_dir= ' '                 ! directory for temporary files
       character(len=8) :: per_type_names(per_type_null)  ! data type names (e.g. 'PER-AVER')    
       
       logical :: dss_direct = .false.
       logical :: binary_output = .false.    
       logical :: check_input_data          ! true to warn about bad data
       logical :: cont_missing              ! true to warn about bad data
       logical :: cont_unchecked            ! true to warn about bad data
       logical :: cont_question             ! true to warn about bad data
       logical :: cont_bad                  ! true to warn about bad data
       logical :: warn_missing              ! true to warn about bad data
       logical :: warn_unchecked            ! true to warn about bad data
       logical :: warn_question             ! true to warn about bad data
       logical :: warn_bad                  ! true to warn about bad data
       logical :: output_comp_pt            ! true to output results at computational points
    
     !> From DSM2/common/logging.f
       integer, parameter :: LOG_ERROR = 0
       integer, parameter :: LOG_WARNING = 1
       integer, parameter :: LOG_INFO = 2
       integer, parameter :: LOG_DEBUG = 2
       integer :: print_level   ! diagnostic printout level
     
     !> From DSM2/common/common_tide.f
       integer, parameter :: max_tide_files = 12
       integer :: nintides
       integer :: current_tidefile

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
       type(tidefile_t):: tide_files(0:max_tide_files)       

       integer, parameter :: TO_BOUNDARY = 1          ! only obj2obj (internal) flows
    
     contains
       
     !> Add environmental variables into envvars(:)  
     subroutine add_envvar(name,val)
          implicit none
          integer:: j
          character (len=ENVVAR_NAME_LEN) :: name
          character (len=ENVVAR_VALUE_LEN) :: val
          do j = 1, nenvvars
            if(name == envvars(j)%name) then
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