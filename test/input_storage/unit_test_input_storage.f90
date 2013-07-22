

!> Routines to test input storage API
!>@ingroup test_input_storage
module ut_input_storage
    
    use error_handling
    
    
    contains
    
    subroutine test_input_storage
    use common_dsm2_vars
        use process_gtm_input
        !use hdf5
        !use input_storage_fortran
        implicit none
        !integer :: nnode_conc
        !integer :: error
        !character(len=21), PARAMETER :: filename="test_input_storage.h5" ! File name
        !integer(HID_T) :: file_id                                        ! File identifier
        !logical :: ext
        !logical :: append_text=.false.
        !integer :: ierror = 0
       
        dsm2_module = gtm
        dsm2_name = "GTM"
        call read_input_text("gtm.inp")
        !call clear_all_buffers(ierror)
        !call init_file_reader(ierror)
        !call set_initial_context_profile(dsm2_name)

        ! Read, collect and process the "ENVVAR" section used for 
        ! text substitution
        !call set_user_substitution_enabled(.false.,ierror)          ! don't try to substitute now
        !call set_substitution_not_found_is_error(.false.,ierror)
        !call set_active_profile("envvar",ierror)                    ! read only ENVVAR blocks
        !call verify_error(ierror, "Error setting active profile")
        !call read_buffer_from_text("gtm.inp",ierror)                ! read starting from this file
        !call verify_error(ierror, "Error reading from text (envvar pass)")
        
        ! Process the results
        !call process_text_substitution(ierror)
        !call set_user_substitution_enabled(.true.,ierror)           ! enable text substitution and substitute now
        !call set_substitution_not_found_is_error(.true.,ierror)
        !call verify_error(ierror,"Error substitution")
        
        !call clear_all_buffers(ierror)                              ! clear the envvar buffer

        ! Do a second pass on all the input, making use of the text
        ! substitution we just prepped
        !call set_active_profile(dsm2_name,ierror)                   ! activate all keywords for the model
        !call read_buffer_from_text("gtm.inp",ierror)                ! Perform the read into buffers
        !call verify_error(ierror,"Error reading from text (full pass)")
        !call prioritize_all_buffers(ierror)                         ! prioritzied buffer
        !call verify_error(ierror,"Error prioritizing buffers, sorting layers")

        !call write_buffer_profile_to_text(dsm2_name,"echo_gtm.inp",.false.,ierror)
        !call verify_error(ierror,"Error writing echoed text")
        !========


     !  !
      !  nnode_conc = node_concentration_buffer_size()
      !  print *,"Number of Node Concentrations: ", nnode_conc

      !  ! Now write it out to hdf5
      !  call h5open_f (error)
      !  call h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)
      !  if (error .ne. 0) then
      !      print*,"Could not open file, hdf error: ", error
      !      print*,"Check if it already exists and delete if so -- failure to replace seems to be an HDF5 bug"
      !      call exit(2)
      !  end if
      !  print*,"invert"

      !  ! Write buffer to HDF5
      !  call envvar_write_buffer_to_hdf5(file_id,ierror)
      !  call scalar_write_buffer_to_hdf5(file_id,ierror)
      !  call io_file_write_buffer_to_hdf5(file_id,ierror)
      !  call tidefile_write_buffer_to_hdf5(file_id,ierror)
      !  call node_concentration_write_buffer_to_hdf5(file_id,ierror)
        
      !  ! Write buffer to echo text file
      !  call envvar_write_buffer_to_text("echo_gtm_out.txt",.false.,ierror)
      !  call scalar_write_buffer_to_text("echo_gtm_out.txt",.true.,ierror)
      !  call io_file_write_buffer_to_text("echo_gtm_out.txt",.true.,ierror)
      !  call tidefile_write_buffer_to_text("echo_gtm_out.txt",.true.,ierror)
      !  call node_concentration_write_buffer_to_text("echo_gtm_out.txt",.true.,ierror)

      !  ! Close file and  FORTRAN interface.
      !  call h5fclose_f(file_id, error)
      !  print *, "file close status: ", error
      !  call h5close_f(error)
      !  print*, "hdf5 shutdown status: ", error
      !  return
      
      
      return          
    end subroutine


    !> Writes in all text starting from input filename
    !subroutine write_input_buffers()
    !    use input_storage_fortran
    !    use iopath_data
    !    use runtime_data
    !    use envvar
    !    implicit none
    !    integer :: ierror = 0
    !    logical :: append_text=.false.
    !    ! Write all buffers to text in the order they were defined
    !    if (io_files(dsm2_module,io_echo,io_write).use)then
    !        append_text=.false.
    !        call write_buffer_profile_to_text(trim(dsm2_name),          &
    !                                          io_files(dsm2_module,        &
    !                                          io_echo,             &
    !                                          io_write).filename,  &
    !                                          append_text,ierror)
    !        call verify_error(ierror,"Error writing echoed text")
    !    end if
    !    return
    !end subroutine
   
end module