

!> This module contains unit test for 
!>@ingroup test_process_io
module ut_create_restart

    use fruit
 
    contains
    
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
        call assertEquals (init(1,1), dble(459.276732218191), weakest_eps, "problem in create_restart")
        close(file_unit) 
        return
    end subroutine
    
end module    

