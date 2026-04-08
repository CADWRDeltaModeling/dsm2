module test_hydro_gtm
    use floweq1d_ds, only: ec_to_density
    use common_xsect, only: calc_layer_centroid
    use testdrive, only: new_unittest, unittest_type, check, error_type

    public :: collect_hydro_gtm

contains
    subroutine collect_hydro_gtm(testsuite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("calc_ec_to_density", test_ec_to_density), &
                    new_unittest("calc_layer_centroid", test_calc_centroid) &
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

    subroutine test_calc_centroid(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
        real*8 :: prev_centroid, elev, prev_elev, width, prev_width, prev_area, centroid_z
        real*8 :: expected_result
        real*8, parameter :: tolerance = 1.0d-4

        ! --- Case 1: Standard Calculation ---
        prev_centroid = 1.2
        elev = 3.0
        prev_elev = 2.0
        width = 5.0
        prev_width = 4.0
        prev_area = 15.0

        call calc_layer_centroid(prev_centroid, elev, prev_elev, width, prev_width, prev_area, centroid_z)
        expected_result = 1.504282
        call check(error, &
           actual=centroid_z, &
           expected=expected_result, &
           message="standard centroid calculation check failed", &
           thr=tolerance)

    end subroutine test_calc_centroid
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

    if (stat > 0) then
        write(error_unit, *) "TOTAL FAILURES:", stat
        stop 1  ! Force the OS to see the failure
    else
        write(error_unit, *) "ALL TESTS PASSED"
    end if

end program tester_hydro_gtm