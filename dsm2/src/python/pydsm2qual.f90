module pydsm2qual
    use iso_c_binding
    ! use network, only: MaxLocations
    use runtime_data, only: julmin, end_julmin
    ! use chstatus, only: WS, Q, H
    ! use gates, only: Gate, GateDevice, gateArray, setFree
    ! use fourpt
    use pydsm2utilities
    ! use update_network
    use dsm2qual, only: qual_prepare1, qual_prepare2, qual_prepare_loop, &
                        qual_loop, qual_wrapup, init_input_file
    implicit none
!f2py integer*4 :: end_julmin
!f2py integer*4 :: julmin

contains
    subroutine py_qual_init(inp_file) bind(C)
        character, intent(in) :: inp_file(:)
        call qual_prepare1()
        print *, 'prepare1'
        call string_copy_c_f(inp_file, init_input_file)
        print *, 'qual_init_input_file', init_input_file
        call qual_prepare2()
        print *, 'prepare2'
        call qual_prepare_loop()
        print *, 'prepare3'
    end subroutine

    subroutine py_qual_loop()
        call qual_loop()
    end subroutine

    subroutine py_qual_wrapup()
        call qual_wrapup()
    end subroutine
end module pydsm2qual
