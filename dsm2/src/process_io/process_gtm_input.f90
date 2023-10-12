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

!> Main program to process input text file
!> This reads text to buffer, assigns buffer to variabls in common_dsm2_*,
!> prints buffer to echo text file, and prints buffer to hdf5 file.
!>@ingroup process_io
module process_gtm_input

    use gtm_precision
    use error_handling
    use common_dsm2_vars
    use input_storage_fortran

    contains

    !> Read the input text file in
    subroutine read_input_text(init_input_file)
        use buffer_gtm_input_common
        use buffer_gtm_input_qual
        implicit none
        character*(*), intent(in) :: init_input_file         !< gtm input text file
        logical :: echo_only, file_exists
        !read all text into buffers and process envvironmental variables
        if (init_input_file .ne. miss_val_c) then
            inquire(file = init_input_file, exist = file_exists)
            if (.not. file_exists)then
                write(*,*)"Input file does not exist: ",init_input_file
                call exit(1)
            end if
            call process_input_text(init_input_file)         ! reads input text
            call buffer_input_common()                       ! locate buffer to the variables in common_dsm2_vars.f90
            call buffer_input_qual()                         ! locate buffer to the variables in common_dsm2_qual.f90
            if (io_files(gtm,io_echo,io_write).use) then
                call write_input_to_echofile(io_files(gtm,io_echo,io_write).filename)
            end if
            !call read_grid_from_tidefile()
            !call buffer_input_grid()    ! processes grid
        end if
        return
    end subroutine


    !> Process input text file into buffer and write them to echo textfile and HDF5 file
    subroutine process_input_text(init_input_file)
        implicit none
        character*(*), intent(in) :: init_input_file                 !< initial input file
        integer :: ierror = 0

        dsm2_module = gtm
        dsm2_name = "GTM"
        call clear_all_buffers(ierror)
        call init_file_reader(ierror)
        call set_initial_context_profile(dsm2_name)

        ! Read, collect and process the "ENVVAR" section used for text substitution
        call set_user_substitution_enabled(.false.,ierror)              ! don't try to substitute now
        call set_substitution_not_found_is_error(.false.,ierror)
        call set_active_profile("envvar",ierror)                        ! read only ENVVAR blocks
        call verify_error(ierror, "Error setting active profile")
        call read_buffer_from_text(init_input_file,ierror)              ! read starting from this file
        call verify_error(ierror, "Error reading from text (envvar pass)")

        ! Process the results
        call process_text_substitution(ierror)
        call set_user_substitution_enabled(.true.,ierror)               ! enable text substitution and substitute now
        call set_substitution_not_found_is_error(.true.,ierror)
        call verify_error(ierror,"Error substitution")

        call clear_all_buffers(ierror)                                  ! clear the envvar buffer

        ! Do a second pass on all the input, making use of the text substitution we just prepped
        call set_active_profile(dsm2_name,ierror)                       ! activate all keywords for the model
        call read_buffer_from_text(init_input_file,ierror)              ! Perform the read into buffers
        call verify_error(ierror,"Error reading from text (full pass)")
        call prioritize_all_buffers(ierror)                             ! prioritzied buffer
        call verify_error(ierror,"Error prioritizing buffers, sorting layers")
        return
    end subroutine


    !> Write buffer to echo file
    subroutine write_input_to_echofile(out_echo_filename)
        implicit none
        character*(*), intent(in) :: out_echo_filename                  ! output echo file name
        logical :: append_text=.false.
        integer :: ierror = 0
        call write_buffer_profile_to_text(dsm2_name,         &
                                          trim(out_echo_filename), &
                                          .false.,           &
                                          ierror)
        call verify_error(ierror,"Error writing echoed text")
        return
    end subroutine


    !> Write buffer in HDF5 file (GTM tidefile)
    subroutine write_input_to_hdf5(file_id)
        use hdf5
        implicit none
        integer(HID_T), intent(in) :: file_id
        integer(HID_T) :: group_id
        integer :: ierror = 0

        call h5gcreate_f(file_id, "input", group_id, ierror)

        ! Write buffer to HDF5
        !call write_buffer_profile_to_hdf5(dsm2_name,file_id,ierror)  !todo: this should be turned on once we finalize the desired input blocks
        call envvar_write_buffer_to_hdf5(group_id, ierror)
        call scalar_write_buffer_to_hdf5(group_id, ierror)
        call io_file_write_buffer_to_hdf5(group_id, ierror)
        call tidefile_write_buffer_to_hdf5(group_id, ierror)
        call node_concentration_write_buffer_to_hdf5(group_id, ierror)
        call reservoir_concentration_write_buffer_to_hdf5(group_id, ierror)
        call group_write_buffer_to_hdf5(group_id, ierror)
        call group_member_write_buffer_to_hdf5(group_id, ierror)
        call rate_coefficient_write_buffer_to_hdf5(group_id, ierror)

        ! Close file and  FORTRAN interface.
        call h5gclose_f(group_id, ierror)
        call verify_error(ierror, "error in closing input group in hdf5 file")
        return
    end subroutine


end module