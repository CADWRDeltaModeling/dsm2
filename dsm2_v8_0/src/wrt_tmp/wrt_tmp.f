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

      program wrt_tmp

c-----Write temporary files from FourPt (perhaps left from model crash)
c-----to permanent files.

c-----Filenames will be on command line.

      use IO_Units
      USE DFLIB                 !! <NT>

      implicit none

      include '../fixed/common.f'
      include 'dss.inc'
      include 'readdss.inc'
      include 'writedss.inc'

c-----local variables

      integer
     &     i                    ! loop index
     &     ,n_args              ! number of command line args
     &     ,iargc               ! number of command line args intrinsic

      integer*2 i2

 610  format('Warning--',i3,' command line arguments specified, 6 used.')

      n_args=iargc()
      if (n_args .eq. 0) then
         write(unit_error,'(a)')
     &        'Must specify temporary files on command line.'
         goto 900
      endif

      if (n_args .gt. 6) then
         write(unit_error, 610) n_args
         n_args=6
      endif

      do i2=1,6
         scratch_file_array(i2)=miss_val_c
      enddo

      do i2=1, n_args
         call getarg(i2,scratch_file_array(i2))
      enddo

      call zset('MLEVEL','',2)
      call wrt_outpaths

      call exit(0)

 900  continue

      call exit(2)

      end
