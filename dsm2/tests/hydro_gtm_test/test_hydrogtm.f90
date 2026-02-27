module test_hydro_gtm
    use floweq1d_ds, only: ec_to_density
    use testdrive, only: new_unittest, unittest_type, check, error_type

    public :: collect_hydro_gtm

contains
    subroutine collect_hydro_gtm(testsuite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("calc_ec_to_density", test_ec_to_density) &
                    ]
    end subroutine collect_hydro_gtm

    subroutine test_ec_to_density(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
        real*8 :: ec, density
        real*8 :: expected_result
        real*8, parameter :: tolerance = 1.0d-4

        ! --- Case 1: Standard Calculation ---
        ec = 10000.0
        call ec_to_density(ec, density)
        expected_result = 1.0042181
        call check(error, &
           actual=density, &
           expected=expected_result, &
           message="standard ec to density calculation check failed", &
           thr=tolerance)

    end subroutine test_ec_to_density
end module test_hydro_gtm


program tester_hydro_gtm
    use, intrinsic :: iso_fortran_env, only: error_unit
    use testdrive, only: run_testsuite, new_testsuite, testsuite_type
    use test_hydro_gtm

    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0
    testsuites = [ &
                 new_testsuite("hydro_gtm", collect_hydro_gtm) &
                 ]
    do is = 1, size(testsuites)
        write (error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

end program tester_hydro_gtm