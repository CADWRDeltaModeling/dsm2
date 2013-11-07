
!> This module contains unit test for read_init_file.
!>@ingroup test_process_io
module ut_read_init_file

    use fruit
 
    contains
    
    subroutine test_read_init_file
        use gtm_precision
        use read_init
        implicit none
        integer, parameter :: ncell = 64
        integer, parameter :: nvar = 1
        character(len=15) :: restart_file_name
        real(gtm_real) :: init(ncell, nvar)
        restart_file_name = "channel_gtm.qrf"
        call read_init_file(init, restart_file_name, ncell, nvar)
        call assertEquals (init(5,1), dble(83.0), weakest_eps, "problem in read_init_file")
        return
    end subroutine

end module        