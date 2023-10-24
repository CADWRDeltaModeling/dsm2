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

subroutine get_tidefile_dates(itide)
      use hdf5
	use hdfvars
	use io_units
      use common_tide
!-----Get the start julian datetime from tidefile number itide

      implicit none

!-----arguments

      integer itide             ! tidefile number

      integer(HID_T) :: tfile_id
      integer        :: ierror
      logical        :: hdf_file_exists
      hdf5_hydrofile=trim(tide_files(itide).filename)
	inquire (file=tide_files(itide).filename, exist=hdf_file_exists)
	if (.not. hdf_file_exists) then
          write (unit_error,*) "HDF5 file does not exist: " // &
              tide_files(itide).filename
	    call exit(2)
	end if

      ! Opens the file and groups for DSM2
      call h5fopen_f(trim(tide_files(itide).filename), &
                     H5F_ACC_RDONLY_F, &
                     tfile_id, &
                     ierror)
      call verify_error(ierror, "Opening tidefile to check dates")
      call getTimeAttributes(tfile_id, &
                             tide_files(itide).start_julmin_file, &
                             tide_files(itide).end_julmin_file, &
                             tide_files(itide).interval, &
                             tide_files(itide).ntideblocks)

      call h5fclose_f(tfile_id, ierror)
      return
end subroutine
