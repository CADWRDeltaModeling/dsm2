module pydsm2
    use iso_c_binding
    use network, only: MaxLocations
    use runtime_data, only: julmin, end_julmin
    use chstatus, only: WS, Q, H
    use gates, only: Gate, GateDevice, gateArray, setFree
    use fourpt
    use pydsm2utilities
    use update_network
    implicit none
!f2py integer*4 :: end_julmin
!f2py integer*4 :: julmin
!f2py integer, parameter :: MaxLocations = 5000
!f2py real*8 :: WS(MaxLocations)
!f2py real*8 :: Q(MaxLocations)
!f2py real*8 :: H(MaxLocations)

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

    subroutine py_fourpt_step_before_updatenetwork()
        call fourpt_step_before_updatenetwork()
    end subroutine

    subroutine py_fourpt_step_after_updatenetwork()
        call fourpt_step_after_updatenetwork()
    end subroutine

    subroutine py_updatenetwork_prepare()
        logical OK
        OK = UpdateNetworkPrepare()
    end subroutine

    subroutine py_updatenetwork_loop()
        call UpdateNetworkLoop()
    end subroutine

    subroutine py_updatenetwork_wrapup()
        logical OK
        updated = UpdateNetworkWrapup()
    end subroutine

    subroutine py_gate_set_free(gate_index, is_free)
        integer, intent(in) :: gate_index
        integer, intent(in) :: is_free
        logical*4 :: is_free_logical
        type(Gate) :: inGate

        !TODO: No safety check for the index
        inGate = gateArray(gate_index)
        if (is_free == 0) then
            is_free_logical = .false.
        else
            is_free_logical = .true.
        end if
        call setFree(inGate, is_free_logical)
    end subroutine

    subroutine py_set_gate_device_elev(gate_index, device_index, elev)
        integer, intent(in) :: gate_index
        integer, intent(in) :: device_index
        real*8, intent(in) :: elev
        type(Gate), pointer :: current_gate
        type(GateDevice), pointer :: current_device

        current_gate => gateArray(gate_index)
        current_device => current_gate%Devices(device_index)
        current_device%baseElev = elev
    end subroutine

end module pydsm2
