module pydsm2gtm
    use iso_c_binding
    ! use network, only: MaxLocations
    ! use runtime_data, only: julmin, end_julmin
    ! use chstatus, only: WS, Q, H
    ! use gates, only: Gate, GateDevice, gateArray, setFree
    ! use fourpt
    use pydsm2utilities
    ! use update_network
    use dsm2gtm, only: gtm_prepare1, gtm_prepare2, gtm_prepare_loop, &
                       gtm_loop, gtm_wrapup, init_input_file
    implicit none

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
