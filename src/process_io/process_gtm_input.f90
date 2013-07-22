

module process_gtm_input

    use gtm_precision
    use error_handling
    use common_dsm2_vars    
    use input_storage_fortran
    
    contains

    !> 
    subroutine read_input_text(init_input_file)  
        implicit none
        character*(*), intent(in) :: init_input_file     ! initial input file (provided) [optional]   
        LOGICAL   OK, isopen, echo_only, file_exists
        !read all text into buffers and process envvironmental variables
        if (init_input_file .ne. miss_val_c) then
            inquire(file = init_input_file, exist = file_exists)      
            if (.not. file_exists)then
                write(*,*)"Input file does not exist: ",init_input_file
                call exit(1)     
            end if
            call process_input_text(init_input_file)   ! reads and echoes text
            !call process_initial_text()       ! reads scalar and envvars from buffer and processes
            !call initialize_runtimes
            !call buffer_input_tidefile()      ! process tidefile name(s)
            !call read_grid_from_tidefile()
            !call buffer_input_grid()    ! processes grid
        end if
        return
    end subroutine

    !> Process input text file into buffer and write them to echo textfile and HDF5 file
    subroutine process_input_text(init_input_file)

        !use hdf5
        use buffer_gtm_input_common
        implicit none
        character*(*), intent(in) :: init_input_file                 !< initial input file
        character(len=20) :: out_tidefile_name                       ! output tidefile name
        character(len=20) :: out_echo_filename                       ! output echo file name
        !integer(HID_T) :: file_id                                    ! File identifier
        logical :: append_text=.false.
        integer :: ierror = 0
        integer :: len_h5, len_echo

        dsm2_name = "GTM"
        out_tidefile_name = "test_process_io.h5"
        out_echo_filename =   "gtm_echo.inp"     
        len_h5 = scan(out_tidefile_name, ".", back=.true.) + 2      !todo: not sure why I got two echo files. Naming got problem with allocatable length. 
        len_echo = scan(out_echo_filename,".", back=.true.) + 3     !      this needs to be fixed.
       
        call clear_all_buffers(ierror)
        call init_file_reader(ierror)
        call set_initial_context_profile(dsm2_name)

        ! Read, collect and process the "ENVVAR" section used for 
        ! text substitution
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

        ! Do a second pass on all the input, making use of the text
        ! substitution we just prepped
        call set_active_profile(dsm2_name,ierror)                       ! activate all keywords for the model
        call read_buffer_from_text(init_input_file,ierror)              ! Perform the read into buffers
        call verify_error(ierror,"Error reading from text (full pass)")
        call prioritize_all_buffers(ierror)                             ! prioritzied buffer
        call verify_error(ierror,"Error prioritizing buffers, sorting layers")

        call buffer_input_common()

        call write_buffer_profile_to_text(dsm2_name,out_echo_filename,.false.,ierror)
        call verify_error(ierror,"Error writing echoed text")         
        
        return
    end subroutine

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
      
      



end module