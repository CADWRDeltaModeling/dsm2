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

! --- reads in the grid information from a hydro tidefile
! --- h5open_f must already have been called

submodule (mod_fixed) mod_read_grid_from_tidefile
contains
module subroutine read_grid_from_tidefile()
      use hdf5
      use common_tide
      use input_storage_fortran
      use runtime_data
      use io_units
      implicit none

      character(len=11) :: group_name = "hydro", subgroup_name = "input"
      integer(HID_T) :: file_id
      integer(HID_T) :: group_id, subgroup_id
      integer :: ierror = 0
      logical :: file_exist

      inquire(FILE=trim(tide_files(1).filename),EXIST=file_exist)
      if (.not. file_exist)then
          write (UNIT_ERROR,*) "Could not read grid from tidefile (exists?, access?): ", &
                             tide_files(1).filename
          call exit(-2)
      end if

      call h5fopen_f(trim(tide_files(1).filename), H5F_ACC_RDONLY_F, file_id, ierror)
      call VerifyHDF5(ierror,"HDF5 opened to read grid")
      call h5gopen_f (file_id, "hydro",    group_id,   ierror)  ! open group instead of create
	call h5gopen_f(group_id, "input", subgroup_id, ierror)

      call read_buffer_profile_from_hdf5("Grid",subgroup_id,ierror)  ! Do the actual read

      call verify_error(ierror,"Error reading grid from hdf5")
      call h5gclose_f (subgroup_id, ierror)
      call h5gclose_f (group_id, ierror)
      call h5fclose_f(file_id,ierror)
      return
end subroutine
end submodule mod_read_grid_from_tidefile
