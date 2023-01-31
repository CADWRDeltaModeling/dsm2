module pydsm2gtm
    use iso_c_binding
    use gtm_precision, only: gtm_real
    use common_variables, only: gtm_start_jmin, gtm_end_jmin, gtm_time_interval
    use pydsm2utilities
    use dsm2gtm, only: gtm_prepare1, gtm_prepare2, gtm_prepare_loop, &
                       gtm_loop, gtm_wrapup, init_input_file
    implicit none
!f2py integer, parameter :: gtm_real = 8
!f2py real(gtm_real) :: gtm_start_jmin
!f2py real(gtm_real) :: gtm_end_jmin
!f2py real(gtm_real) :: gtm_time_interval

contains
    subroutine py_gtm_init(inp_file) bind(C)
        character, intent(in) :: inp_file(:)
        call gtm_prepare1()
        print *, 'prepare1'
        call string_copy_c_f(inp_file, init_input_file)
        print *, 'gtm_init_input_file', init_input_file
        call gtm_prepare2()
        print *, 'prepare2'
        call gtm_prepare_loop()
        print *, 'prepare3'
    end subroutine

    subroutine py_gtm_loop()
        call gtm_loop()
    end subroutine

    subroutine py_gtm_wrapup()
        call gtm_wrapup()
    end subroutine
end module pydsm2gtm
