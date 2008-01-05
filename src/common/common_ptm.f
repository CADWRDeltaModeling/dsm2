!<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    DSM2 is free software: you can redistribute it and/or modify
C!    it under the terms of the GNU General Public License as published by
C!    the Free Software Foundation, either version 3 of the License, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public License for more details.

C!    You should have received a copy of the GNU General Public License
C!    along with DSM2.  If not, see <http://www.gnu.org/licenses/>.
!</license>


      module common_ptm
      type part_injection_t
         character*16 type          ! particle type (e.g. 'striped bass', 'salmon')
         character*16 slength = ' ' ! Length of time from the start of the simulation when parts are injected e.g. 15min, 1day
         character*16 length = ' '  ! Length of injection e.g. 15min, 1day
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
      integer :: npartno   = 1       ! Actual No of Injection Loc and times combined
      integer :: ngroup_outputs = 0      ! Actual No of Groups requested


      type(part_injection_t) :: part_injection(max_injection)
      ! todo: this can be inferred, yes?
      integer, parameter :: max_chanres =600 ! Maximum number of channels and Reservoirs
      integer :: nchanres                 ! Actual number of channels and Reservoirs
      end module


