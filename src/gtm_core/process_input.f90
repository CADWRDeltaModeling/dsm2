

module process_input

    use gtm_precision
    use input_storage_fortran
    contains

    subroutine read_input_text(init_input_file)
    
        implicit none
        character*(*), intent(in) :: init_input_file ! initial input file (provided) [optional]   
        LOGICAL   OK, isopen, echo_only,file_exists
        !read all text into buffers and process envvironmental variables
        if (init_input_file .ne. miss_val_c) then
            inquire(file=init_input_file, exist=file_exists)      
            if (.not. file_exists)then
                write(*,*)"Input file does not exist: ",init_input_file
                call exit(1)     
            end if
            call input_text(init_input_file)  ! reads and echoes text
            !call process_initial_text()       ! reads scalar and envvars from buffer and processes
            !call initialize_runtimes
            !call buffer_input_tidefile()      ! process tidefile name(s)
            !call read_grid_from_tidefile()
            !call buffer_input_grid()    ! processes grid
        end if
        return
    end subroutine



end module