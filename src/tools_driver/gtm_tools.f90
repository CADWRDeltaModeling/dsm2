



program gtm_tools

    use create_restart
    implicit none
    integer :: tool_no
    character(len=128) :: restart_file
    character(len=128) :: gtm_tidefile
    character(len=14) :: datetime
    
    write(*,'(//5x,a52)') "****************************************************"
    write(*,'(5x,a52)')   "*                                                  *"    
    write(*,'(5x,a52)')   "*                 DSM2-GTM TOOLS                   *"
    write(*,'(5x,a52)')   "*                                                  *"      
    write(*,'(5x,a52/)')  "****************************************************"    
    write(*,*) ""
    write(*,*) "Please select the tools you want to use:"
    write(*,*) "(1) Create restart file from DSM2-GTM tidefile"
    write(*,*) "(2)"
    
    read(*,*) tool_no
    if (tool_no .eq. 1) then
        write(*,*) "You select (1) Create restart file from DSM2-GTM tidefile"
        write(*,*) "Please type in output restart file name:"        
        !read(*,*) restart_file
        write(*,*) "Please type in DSM2-GTM tidefile name:"
        !read(*,*) gtm_tidefile
        write(*,*) "Please type in time to output (format: DDMMMYYYY HHMM):"
        !read(*,'(a14)') datetime
        !call create_restart_file(trim(restart_file), trim(gtm_tidefile), trim(datetime))
        !call create_restart_file("test.txt", "channel_gtm.h5", "01FEB1998 0900")
        !todo:: not working, not sure why. It is working in unit test.
        call test_create_restart
    else
        write(*,*) "Please select a tool!"
    end if       
   
    stop

end program
 
    !> Routine to test creating restart text file
    subroutine test_create_restart()
        use gtm_precision
        use create_restart
        implicit none
        integer :: file_unit
        character(len=11) :: restart_filename
        real(gtm_real) :: init(64,2)
        integer :: ncell, nvar
        integer :: i, j
        
        file_unit = 131
        restart_filename = "restart.txt"
        call create_restart_file(restart_filename, "channel_gtm.h5", "01FEB1998 0900")
        open(file_unit, file = restart_filename)
        read(file_unit,*)
        read(file_unit,*)
        read(file_unit,*) nvar
        read(file_unit,*) ncell
        do i = 1, ncell
            read(file_unit,*) (init(i,j),j=1,nvar)
        end do    
        close(file_unit) 
        return
    end subroutine