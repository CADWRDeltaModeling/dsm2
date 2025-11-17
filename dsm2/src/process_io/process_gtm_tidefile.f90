!<license>
!    Copyright (C) 2017 State of California,
!    Department of Water Resources.
!    This file is part of DSM2-GTM.
!
!    The Delta Simulation Model 2 (DSM2) - General Transport Model (GTM)
!    is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    DSM2 is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
!</license>

!>@ingroup process_io
module process_gtm_tidefile

    contains

    !> Process a character line into data arrays for tide file info.
    !> This has been significantly modified for single tidefile case.
    !> Multi-tide concept from DSM2-Qual is not necessary.
    subroutine process_tidefile(tide_filename)
        use hdf5
        use h5lt
        use gtm_vars
        use error_handling
        use time_utilities
        implicit none
        character(len=128), intent(in) :: tide_filename   !< DSM2-Hydro tidefile
        integer(HID_T) :: tfile_id
        integer        :: ierror
        logical        :: hdf_file_exists
        integer, dimension(1) :: hdf5_read_buffer
  	    inquire (file = trim(tide_filename), exist = hdf_file_exists)
	    if (.not. hdf_file_exists) then
             write (*,*) "HDF5 file does not exist: " // &
                                  tide_filename
	         call exit(2)
	    end if
	    hydro_hdf5 = tide_filename

        ! Opens the file and groups for DSM2
        call h5fopen_f(trim(tide_filename),  &
                       H5F_ACC_RDONLY_F,     &
                       tfile_id,             &
                       ierror)

        call verify_error(ierror, "Opening tidefile to check dates")
        call h5ltget_attribute_int_f(tfile_id, "hydro", "Start time", hdf5_read_buffer, ierror)
        call verify_error(ierror, "Reading attribute Start time string from hdf5 file")
        hydro_start_jmin = hdf5_read_buffer(1)

        call h5ltget_attribute_int_f(tfile_id, "hydro", "Time interval", hdf5_read_buffer, ierror)
        call verify_error(ierror, "Reading attribute Time interval from hdf5 file")
        hydro_time_interval = hdf5_read_buffer(1)

        call h5ltget_attribute_int_f(tfile_id, "hydro", "Number of intervals", hdf5_read_buffer, ierror)
        call verify_error(ierror, "Reading attribute Number of intervals from hdf5 file")
        hydro_ntideblocks = hdf5_read_buffer(1)

        hydro_end_jmin = hydro_start_jmin + (hydro_ntideblocks-1)*hydro_time_interval
        call h5fclose_f(tfile_id, ierror)
        return
    end subroutine

end module