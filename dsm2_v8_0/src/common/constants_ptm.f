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

      module constants_ptm

c-----particle passing between waterbodies (flux output)
      integer
     &     ptm_from_wb
     &     ,ptm_to_wb
     &     ,ptm_interval
     &     ,ptm_filename
     &     ,ptm_modifier
     &     ,b_part
     &     ,ptm_group

      parameter (
     &     ptm_from_wb=1
     &     ,ptm_to_wb=2
     &     ,ptm_interval=3
     &     ,ptm_filename=4
     &     ,ptm_modifier=5
     &     ,b_part=6
     &     ,ptm_group=7
     &     )

c-----magic numbers for particle injection
      integer
     &     partno_node
     &     ,partno_nparts
     &     ,partno_slength
     &     ,partno_length
     &     ,partno_sdate
     &     ,partno_stime
     &     ,partno_edate
     &     ,partno_etime
     &     ,partno_type

      parameter (
     &     partno_node=1
     &     ,partno_nparts=2
     &     ,partno_slength=3
     &     ,partno_length=4
     &     ,partno_sdate=5
     &     ,partno_stime=6
     &     ,partno_edate=7
     &     ,partno_etime=8
     &     ,partno_type=9
     &     )


c-----magic numbers for particle output groups
      integer
     &     group_name
     &     ,group_memtype
     &     ,group_memid

      parameter (
     &     group_name=1
     &     ,group_memtype=2
     &     ,group_memid=3
     &     )

c-----magic number for 'all' keyword in flux output
      integer alltypes
      parameter (alltypes = -100)

c-----magic numbers for profile of particle distribution option

      integer
     &     partdist_chan
     &     ,partdist_slength
     &     ,partdist_sdate
     &     ,partdist_stime

      parameter (
     &     partdist_chan=1
     &     ,partdist_slength=2
     &     ,partdist_sdate=3
     &     ,partdist_stime=4
     &     )

      integer 
     &     unit_binary

      parameter (
     &     unit_binary=3
     &     )
     
      end module
