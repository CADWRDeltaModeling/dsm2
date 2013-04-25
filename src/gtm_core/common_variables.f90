!<license>
!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
!    Department of Water Resources.
!    This file is part of DSM2.
!
!    The Delta Simulation Model 2 (DSM2) is free software: 
!    you can redistribute it and/or modify
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

!> This module contains common variables definition, such as n_time, n_chan, n_comp  
!> and n_segm as well as properties for channels, computational points and segments. 
!>@ingroup gtm_core
module common_variables

    use gtm_precision
    integer :: n_time = LARGEINT                                !< number of time steps
    integer :: n_comp = LARGEINT                                !< number of computational points
    integer :: n_chan = LARGEINT                                !< number of channels
    integer :: n_segm = LARGEINT                                !< number of segments
    integer :: n_xsect = LARGEINT                               !< number of entries in virt xsect table
    integer :: npartition_x = LARGEINT                          !< number of cells within a segment
    integer :: npartition_t = LARGEINT                          !< number of gtm time intervals partition from hydro time interval
    integer :: ncell = LARGEINT                                 !< number of cells in the entire network
    integer :: orig_start_julmin = LARGEINT                     !< original start time in hydro run
    integer :: orig_end_julmin = LARGEINT                       !< original end time in hydro run
    integer :: orig_time_interval = LARGEINT                    !< original time interval in hydro run
    integer :: orig_ntideblocks = LARGEINT                      !< original time blocks in hydro run
    integer :: start_julmin = LARGEINT                          !< gtm start time
    integer :: end_julmin = LARGEINT                            !< gtm end time
    integer :: time_interval = LARGEINT                         !< gtm simulation time interval
    integer :: ntideblocks = LARGEINT                           !< gtm time blocks
    
    real(gtm_real), allocatable :: hydro_flow(:,:)              !< flow from DSM2 hydro
    real(gtm_real), allocatable :: hydro_area(:,:)              !< area from DSM2 hydro
    real(gtm_real), allocatable :: hydro_ws(:,:)                !< water surface from DSM2 hydro
    real(gtm_real), allocatable :: hydro_avga(:,:)              !< average area from DSM2 hydro
    
    !> Define scalar and envvar in input file
    type scalar_t
        character(len=:), allocatable :: gtmoutdssfile
        character(len=:), allocatable :: dsm2modifier
        character(len=:), allocatable :: title
        character(len=:), allocatable :: run_time_step
        character(len=9), allocatable :: run_start_date
        character(len=9), allocatable :: run_end_date
        character(len=4), allocatable :: run_start_time
        character(len=4), allocatable :: run_end_time
        character(len=:), allocatable :: temp_dir
    end type
    type(scalar_t) :: scalar
   
    !> Define channel type to store channel related arrays
    type channel_t                                              !< channel between hydro nodes
         integer :: channel_num                                 !< actual channel number in DSM2 grid
         integer :: chan_no                                     !< index channel number
         integer :: channel_length                              !< channel length
         integer :: up_comp_pt                                  !< upstream computational point
         integer :: down_comp_pt                                !< downstream computational point
    end type
    type(channel_t), allocatable :: chan_geom(:)
     
    !> Define computational point type to store computational point related arrays   
    type comp_pt_t                                              !< computational points
         integer :: comp_index                                  !< computational point index
         integer :: chan_no                                     !< channel number
         real(gtm_real) :: distance                             !< distance from upstream node
    end type
    type(comp_pt_t), allocatable :: comp_pt(:)
    
    !> Define segment type to store segment related arrays
    type segment_t                                             !< segment between computational points
         integer :: segm_no                                    !< segment serial no
         integer :: chan_no                                    !< channel no
         integer :: up_comppt                                  !< upstream computational point (used as index to search time series data)        
         integer :: down_comppt                                !< downstream computational point
         real(gtm_real) :: up_distance                         !< up_comppt distance from upstream node
         real(gtm_real) :: down_distance                       !< down_comppt distance from upstream node
         real(gtm_real) :: length                              !< segment length in feet
    end type
    type(segment_t), allocatable :: segm(:)
    
    contains
    
    !> Allocate channel_t array    
    subroutine allocate_channel_property()
        use error_handling
        implicit none
        integer :: istat = 0
        character(len=128) :: message
        allocate(chan_geom(n_chan), stat = istat)
        if (istat .ne. 0 )then
           call gtm_fatal(message)
        end if
        return
    end subroutine

    !> Allocate comp_pt_t array
    subroutine allocate_comp_pt_property()
        use error_handling
        implicit none
        integer :: istat = 0
        character(len=128) :: message
        allocate(comp_pt(n_comp), stat = istat)
        if (istat .ne. 0 )then
           call gtm_fatal(message)
        end if
        return
    end subroutine
    
    !> Allocate segment_t array
    subroutine allocate_segment_property()
        use error_handling
        implicit none
        integer :: istat = 0
        character(len=128) :: message
        n_segm = n_comp - n_chan
        allocate(segm(n_segm), stat = istat)
        if (istat .ne. 0 )then
           call gtm_fatal(message)
        end if
        return
    end subroutine
    
    !> Allocate hydro time series array
    subroutine allocate_hydro_ts()
        use error_handling
        implicit none
        integer :: istat = 0
        character(len=128) :: message
        allocate(hydro_flow(n_comp,n_time),hydro_area(n_comp,n_time), &
                 hydro_ws(n_comp,n_time),hydro_avga(n_comp,n_time), stat = istat)
        if (istat .ne. 0 )then
           call gtm_fatal(message)
        end if
        hydro_flow = LARGEREAL
        hydro_area = LARGEREAL
        hydro_ws = LARGEREAL
        hydro_avga = LARGEREAL
        return
    end subroutine    

    !> Deallocate channel property
    subroutine deallocate_channel()
        implicit none
        deallocate(chan_geom)
        return
    end subroutine
 
    !> Deallocate computational point property
    subroutine deallocate_comp_pt()
        implicit none
        deallocate(comp_pt)
        return
    end subroutine

    !> Deallocate segment property
    subroutine deallocate_segment()
        implicit none
        deallocate(segm)
        return
    end subroutine
           
    !> Deallocate hydro time series array
    subroutine deallocate_hydro_ts()
        implicit none
        deallocate(hydro_flow, hydro_area, hydro_ws, hydro_avga)
        return
    end subroutine
    
end module