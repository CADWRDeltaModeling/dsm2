module pydsm2
    use network, only: MaxLocations
    use runtime_data, only: julmin, end_julmin
    use chstatus, only: Q
    use fourpt
    use pydsm2utilities
    implicit none
!f2py integer*4 :: end_julmin
!f2py integer*4 :: julmin
!f2py integer, parameter :: MaxLocations = 5000
!f2py real*8 :: Q(MaxLocations)

contains
    subroutine py_fourpt_init(inp_file) bind(C)
        character, intent(in) :: inp_file(:)
        call prepare_hydro()
        call string_copy_c_f(inp_file, init_input_file)
        call fourpt_init()
    end subroutine

    subroutine py_fourpt_step()
        call fourpt_step()
    end subroutine py_fourpt_step

    subroutine py_fourpt_winddown()
        call fourpt_winddown()
    end subroutine
end module pydsm2
