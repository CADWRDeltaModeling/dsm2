module test_hydro
    use fourpt
    use testdrive

    public :: collect_hydro

contains
    subroutine collect_hydro(testsuite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("prepare_hydro", test_prepare_hydro) &
                    ]
    end subroutine collect_hydro

    subroutine test_prepare_hydro(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
        call prepare_hydro()

    end subroutine test_prepare_hydro
end module test_hydro

program tester
    use, intrinsic :: iso_fortran_env, only: error_unit
    use testdrive, only: run_testsuite, new_testsuite, testsuite_type
    use test_hydro, only: collect_hydro

    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
                 new_testsuite("hydro", collect_hydro) &
                 ]

    do is = 1, size(testsuites)
        write (error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write (error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if

end program tester
