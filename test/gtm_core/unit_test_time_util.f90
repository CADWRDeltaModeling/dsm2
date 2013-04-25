
!> 
!>@ingroup test_gtm_core
module ut_time_util

    use fruit
    use gtm_precision
    use error_handling
    contains
    
    subroutine test_time_util()
        use time_util
        implicit none
        integer :: jday 
        character(len=14) :: cdt
        call cdt2jmin('01SEP2001 2400',jday)
        call assertEquals (jday, 53474400, "problem in cdt2jmin function")
        call jmin2cdt(53474400, cdt)
        call assertEquals (cdt, '01SEP2001 2400', "problem in jmin2cdt function")
        return
    end subroutine
end module    