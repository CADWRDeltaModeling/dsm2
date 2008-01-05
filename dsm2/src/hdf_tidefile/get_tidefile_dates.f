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

      subroutine get_tidefile_dates(itide)
	use hdfvars
	use io_units
      use common_tide
c-----Get the start julian datetime from tidefile number itide

      implicit none

c-----included common blocks

      integer,external :: getHDF5NumberOfTimeIntervals
      integer,external :: getHDF5StartTime
      integer,external :: getHDF5EndTime
      integer,external :: getHDF5TimeInterval

c-----arguments

      integer itide             ! tidefile number
     &     ,tide_block_no       ! tide block number


      hdf5_hydrofile=trim(tide_files(itide).filename)
	inquire (file=hdf5_hydrofile, exist=h5_file_exists)
	if (.not. h5_file_exists) then
          write (unit_error,*) "HDF5 file does not exist: " //
     &         tide_files(itide).filename
	    call exit(2)
	end if
      ! Opens the file and groups for DSM2
	call OpenHDF5()
c-----local variables
      if (.not. tide_files(itide).binarytf) then
         tide_files(itide).start_julmin_file = getHDF5StartTime()
         tide_files(itide).end_julmin_file = getHDF5EndTime()
         tide_files(itide).ntideblocks = getHDF5NumberOfTimeIntervals()
	   tide_files(itide).interval = getHDF5TimeInterval()
      endif
      call CloseHDF5()

      return
      end
