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

!-----Qual binary for PTM
!     NOTE: The contents of this module are deprecated. 
!     The Qual binary file must be written
!     in HDF5 and not based on nodes. 
!     This module is here to keep legacy references happy.
module common_qual_bin
      integer, parameter :: max_num_const = 10    ! maximum number of constituents
      integer, parameter :: max_qual_nodes = 650  ! maximum number of nodes

      type qual_bin_file_t
         integer*4 start_julmin_file ! file timestamp start
         integer*4 end_julmin_file   ! file timestamp end
         integer*4 start_julmin      ! when to start using this tidefile (wrt tidefile date)
         integer*4 end_julmin        ! when to quit using this tidefile (wrt tidefile date)
         integer*4 interval          ! minutes between tideblocks
         character*14 start_date     ! file timestamp start
         character*14 end_date       ! file timestamp end
         character*14,dimension(max_num_const) :: constituent = ' ' ! name of quality constituent
         character*150 :: filename = ' '                  ! tidefile name
         character*7 version         ! binary file version
      end type
      type(qual_bin_file_t) qual_bin_file

      real*4 Qnode(0:max_qual_nodes,max_num_const)
             ! array of quality constituents for nodec

      integer*4 &
          QualTime      ! julian minute timestamp from qual binary file

      integer :: numnode
      integer :: neq
      integer :: qual2node(0:max_qual_nodes)

end module