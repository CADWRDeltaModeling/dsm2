

!> Collection of common variables required from DSM2
!> Those are variables from runtime_data.f, constants.f, 
!> io_units.f and iopath_data.f.
!>@ingroup gtm_core
module common_dsm2_vars

    !> From DSM2/common/runtime_data.f
       character(len=:), allocatable :: dsm2_name

    !> From DSM2/common/constants.f
       !-----model
       integer :: dsm2_module
       integer :: hydro
       integer :: qual
       integer :: ptm
       integer :: gtm

       parameter ( hydro = 1,  &
                   qual = 2,   &
                   ptm = 3,    &
                   gtm = 4)
       !-----type
       integer :: io_restart
       integer :: io_echo
       integer :: io_animation
       integer :: io_trace
       integer :: io_behavior
       integer :: io_group
       integer :: io_hdf5

       parameter( io_restart=1,   &
                  io_echo=2,      &
                  io_animation=3, &
                  io_trace=4,     &
                  io_behavior=5,  &
                  io_group=6,     &
                  io_hdf5=7)
       !-----io
       integer :: io_read
       integer :: io_write

       parameter( io_read=1,  &
                  io_write=2)

       !-----data types
       integer :: per_type_per_aver
       integer :: per_type_per_cum
       integer :: per_type_per_min
       integer :: per_type_per_max
       integer :: per_type_inst_val
       integer :: per_type_inst_cum
       integer :: per_type_null

       parameter (per_type_per_aver = 1,  &
                  per_type_per_cum = 2,   &
                  per_type_per_min = 3,   &
                  per_type_per_max = 4,   &
                  per_type_inst_val = 5,  &
                  per_type_inst_cum = 6,  &
                  per_type_null=7)  
       
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
    
       integer:: max_file_types               ! number of types of files (restart, tide, animation,...)

       parameter (max_iogroups = 4,     &     ! one per model (number of types of files (restart, tide, animation,...))
                  max_file_types = 7)         ! named in constants with names like io_restart    
       type(io_file_t):: io_files(max_iogroups,max_file_types,2)

       character(len=130)::output_filename    ! output filename

       character(len=8):: per_type_names(per_type_null) ! data type names (e.g. 'PER-AVER')    
    
       contains
    
end module