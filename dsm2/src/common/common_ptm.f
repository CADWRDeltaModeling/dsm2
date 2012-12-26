C!<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    The Delta Simulation Model 2 (DSM2) is free software: 
C!    you can redistribute it and/or modify
C!    it under the terms of the GNU General Public License as published by
C!    the Free Software Foundation, either version 3 of the License, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public License for more details.

C!    You should have received a copy of the GNU General Public License
C!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
C!</license>


      module common_ptm
      type part_injection_t
         character*16 type          ! particle type (e.g. 'striped bass', 'salmon')
         character*16 :: slength = ' ' ! Length of time from the start of the simulation when parts are injected e.g. 15min, 1day
         character*16 :: length = ' '  ! Length of injection e.g. 15min, 1day
         character*14 :: start_date = ' ' ! start date and time of injection
         character*2 dummy1
         character*14 :: end_date = ' '   ! end date and time
         character*2 dummy2
         integer   :: node=0     ! Location where particles are injected
         integer   :: nparts=0   ! Number of particles injected at location
         integer*4 :: start_julmin ! start julian minute
         integer*4 :: end_julmin ! end julian minute
         integer*4    length_julmin ! injection length in minutes
      end type

c-----common blocks for DSM2 PTM
c-----Scalars for ptm

      logical
     &     ptm_ivert            ! True to use a vertical velocity profile
     &     ,ptm_itrans          ! True to use transverse velocity profile
     &     ,ptm_iey             ! True to use transverse mixing
     &     ,ptm_iez             ! True to use vertical mixing
     &     ,ptm_iprof           ! True writes to a file locations of parts at specified timesteps and in spec. chnls
     &     ,ptm_igroup          ! True reads from a file groups of chnls & res and writes # of parts in each group
     &     ,ptm_flux_percent    ! True outputs flux as a percentage of particles
     &     ,ptm_group_percent   ! True outputs groups as a percentage of particles
     &     ,ptm_flux_cumulative ! True outputs flux as cumulative values with time


      integer
     &     ptm_random_seed
     &     ,ptm_time_step
     &     ,ptm_no_animated


      real
     &     ptm_trans_constant,
     &     ptm_vert_constant,
     &     ptm_trans_a_coef,
     &     ptm_trans_b_coef,
     &     ptm_trans_c_coef


c-----Values that indicate whether a scalar was specified in the input
      integer :: ptm_random_seed_int = 0
      integer :: ptm_vert_constant_int = 0
      integer :: ptm_trans_constant_int = 0
      integer :: ptm_iprof_int = 0
      integer :: ptm_igroup_int = 0
      integer :: ptm_ivert_int = 0
      integer :: ptm_itrans_int = 0
      integer :: ptm_iey_int = 0
      integer :: ptm_iez_int = 0
      integer :: ptm_time_step_int = 0
      integer :: ptm_trans_a_coef_int = 0
      integer :: ptm_trans_b_coef_int = 0
      integer :: ptm_trans_c_coef_int = 0
      integer :: ptm_flux_percent_int = 0
      integer :: ptm_group_percent_int = 0
      integer :: ptm_flux_cumulative_int = 0
      integer :: ptm_no_animated_int = 0


      integer,parameter :: max_injection = 200 ! Maximum Injection Locations and time of injections combined
      integer :: npartno   = 0           ! Actual No of Injection Loc and times combined   ! todo: eli changed initialization to 0 for text? consequences?
      integer :: ngroup_outputs = 0      ! Actual No of Groups requested


      type(part_injection_t) :: part_injection(max_injection)
      ! todo: this can be inferred, yes?
      integer, parameter :: max_chanres =600 ! Maximum number of channels and Reservoirs
      integer :: nchanres                 ! Actual number of channels and Reservoirs


c-----Particle filters info, with filter input table for DSM2 PTM
      integer :: nfilter = 0      ! Actual No of Filters
      integer,parameter :: max_filter = 1000 ! Maximum Filter No
      type part_filter_t
         character*16  type_f     ! filter type (for future possible expansions)
         character*32  name       ! filter name
         integer       ndx        ! filter index
         integer       node       ! node at which filter resides (internal node id)(general filter)
         character*32  resname    ! reservoir at which filter resides (special filter for source flows connecting to res, e.g. clifton)
         character*32  at_wb      ! waterbody at which filter resides
         integer       at_wb_type ! type of waterbody at which filter resides
         integer       at_wb_ndx  ! index of waterbody at which filter resides (internal id in the specified wb type)
         integer       at_wb_id   ! index of waterbody at which filter resides (internal id of all wbs)
         real*8        op         ! efficiency of particle passing this filter (specified timestamp): 1-open; 0-block
         character*8   fillin     ! how to fill in between data (first, last, interp, data)
         character*128 filename   ! DSS filename
         character*80  path       ! DSS pathname
      end type
      type(part_filter_t) :: part_filter(max_filter)

      end module
