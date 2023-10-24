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

subroutine process_io_file(model,filetype,io,interval,filename)
!-----process a character line into data arrays for
!-----output file names
      use io_units
      use iopath_data
      use constants_ptm
      use common_ptm
      implicit none

!-----local variables

      integer &
          i,i1,i2,i3           ! indices

      character*8 &
          model &              ! string for model
          ,filetype &          ! string for type
          ,io &                ! string for io
          ,interval            ! string for interval
      character*128 &
          filename            ! string for filename


 610  format(/a)
 620  format(/'Invalid value given in ',a,' field: ',a)

!-----fill in structure
      call locase(model)

      if (model(1:5) .eq. 'hydro') then
         i1=hydro
      else if (model(1:4) .eq. 'qual') then
         i1=qual
      else if (model(1:3) .eq. 'ptm') then
         i1=ptm
      else
         write(unit_error, "('Model not recognized: ',a)")model
         call exit(-1)
      endif

      ! todo: this doesn't make any sense
      if (filetype .eq. 'output') then
         output_filename=filename
         return
      endif

      if (filetype(1:7) .eq. 'restart') then
         i2=io_restart
      else if (filetype(1:4).eq. 'echo') then
         i2=io_echo
      else if (filetype(1:3) .eq. 'hdf') then
         i2=io_hdf5
      else if (filetype(1:3) .eq. 'ani') then
         i2=io_animation
      else if (filetype(1:3) .eq. 'tra') then
         i2=io_trace
      else if (filetype(1:3) .eq. 'beh') then
         i2=io_behavior
      else if (filetype(1:3) .eq. 'gro') then
         i2=io_group
         ptm_igroup_int=1
         ptm_igroup=.true.
      else
         write(unit_error,*) "IO file type not recognized: ", filetype
         call exit(-1)
      endif

      if (io .eq. 'in') then
         i3=io_read
      else if (io .eq. 'out') then
         i3=io_write
      else
         write(unit_error, *) "IO must be 'in' or 'out' "
         call exit(-1)
       endif

      if (interval .ne. ' ' .and. &
          interval .ne. 'none') then
         io_files(i1,i2,i3).interval=interval
      endif

      io_files(i1,i2,i3).use=.true.
      io_files(i1,i2,i3).filename=filename

      return
end subroutine

