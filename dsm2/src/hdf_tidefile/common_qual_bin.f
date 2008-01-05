C!<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    DSM2 is free software: you can redistribute it and/or modify
C!    it under the terms of the GNU General Public !<license as published by
C!    the Free Software Foundation, either version 3 of the !<license, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public !<license for more details.

C!    You should have received a copy of the GNU General Public !<license
C!    along with DSM2.  If not, see <http://www.gnu.org/!<licenses/>.
C!</license>

c-----Qual binary for PTM
c     NOTE: The contents of this module are deprecated. 
c     The Qual binary file must be written
c     in HDF5 and not based on nodes. 
c     This module is here to keep legacy references happy.
      module common_qual_bin
      integer, parameter :: max_num_cons = 10    ! maximum number of constituents
      integer, parameter :: max_qual_nodes = 650 ! maximum number of nodes

      type qual_bin_file_t
         integer*4 start_julmin_file ! file timestamp start
         integer*4 end_julmin_file ! file timestamp end
         integer*4 start_julmin ! when to start using this tidefile (wrt tidefile date)
         integer*4 end_julmin   ! when to quit using this tidefile (wrt tidefile date)
         integer*4 interval       ! minutes between tideblocks
         character*14 start_date ! file timestamp start
         character*14 end_date  ! file timestamp end
         character*14 :: constituent(max_num_const) = ' ' ! name of quality constituent
         character*150 :: filename = ' '                  ! tidefile name
         character*7 version    ! binary file version
      end type
      type(qual_bin_file_t) qual_bin_file

      real*4 Qnode(0:max_qual_nodes,max_num_const)
             ! array of quality constituents for node

      integer*4
     &     QualTime      ! julian minute timestamp from qual binary file

      integer :: numnode
      integer :: neq
      integer :: qual2node(0:max_qual_nodes)

      end module