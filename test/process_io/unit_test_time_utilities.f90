
!> Test routines of time functionalities
!>@ingroup test_process_io
module ut_time_utilities

    use fruit
    use gtm_precision
    use error_handling
    contains
    
    subroutine test_time_util()
        use time_utilities
        implicit none
        integer :: jday
        character(len=14) :: cdt
        integer :: offset, num_buffers, remainder
        integer, allocatable :: memlen(:)
        jday = cdt2jmin('01SEP2001 2400')
        call assertEquals (jday, 53474400, "problem in cdt2jmin function")
        cdt = jmin2cdt(53474400)
        call assertEquals (cdt, '01SEP2001 2400', "problem in jmin2cdt function")  
        !call check_runtime(offset, num_buffers, memlen,               &  
        !                   1500,'30NOV1974 2400', '30DEC1974 2400',   &     ! gtm starting and ending time
        !                   39314880, 39840480, 15)                          !01OCT1974 0000-01OCT1975 0000
        !call assertEquals (offset, 5856, "Porblem in check_runtime offset")
        !call assertEquals (num_buffers, 2, "Porblem in check_runtime num_buffers")
        !call assertEquals (memlen(2), 1380, "Porblem in check_runtime remainder")
        return
    end subroutine
    
end module    