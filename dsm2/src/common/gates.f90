!<license>
!!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
!!    Department of Water Resources.
!!    This file is part of DSM2.

!!    The Delta Simulation Model 2 (DSM2) is free software:
!!    you can redistribute it and/or modify
!!    it under the terms of the GNU General Public License as published by
!!    the Free Software Foundation, either version 3 of the License, or
!!    (at your option) any later version.

!!    DSM2 is distributed in the hope that it will be useful,
!!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!!    GNU General Public License for more details.

!!    You should have received a copy of the GNU General Public License
!!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
!!</license>

!-----Module: Gates

module Gates
    use type_defs
    use constants
    use array_limits
    use gates_data
    !FIXME: use network
    implicit none
    public :: gateArray, nGate, NTotalDevice, MAX_DEV, GATE_OPEN, GATE_CLOSE, GATE_FREE, UNIDIR_TO_NODE, UNIDIR_FROM_NODE, WEIR, PIPE, Gate, GateDevice

contains  !!========================================================!

    !== Public (GateInit) ===================================================

    subroutine setFree(inGate, isfree)
        !-----Sets or unsets a gate to the GATE_FREE condition, which means that the
        !-----gate has been physically uninstalled and the connection reverts to
        !-----an "equal stage" internal boundary condition. This routine takes care
        !-----of the clean-up required in order to make the transition.
        !-----use Gates, only: Gate, GATE_OPEN
        implicit none
        type(Gate):: inGate
        logical*4:: isfree
        integer :: i
        ! fixme: is this correct??
        if (isfree .and. .not. inGate%free) then ! gate is non-redundantly free
            do i = 1, inGate%nDevice
                inGate%Devices(i)%flow = 0.D0 ! fixme: this may be unacceptable?
            end do
        else if ((.not. isfree) .and. inGate%free) then
            ! gate is non-redundantly installed
        end if
        inGate%free = isfree
        return
    end subroutine

    !== Public (gateIndex) =========
    !     Given a gate name, obtain its internal index in gateArray or
    !     miss_val_i if the name is not a gate name in the current model.
    !     Element-by-element search, so this routine is expensive.
    integer function gateIndex(gateName)
        use constants
        implicit none
        integer :: igate
        character:: gateName*32
        character:: lowerName*32
        lowerName = gateName
        call locase(lowerName)
        do igate = 1, nGate
            if (lowerName == gateName) then
                gateIndex = igate
                return
            end if
        end do
        gateIndex = miss_val_i
        return
    end function

    !== Public (deviceIndex) =======
    !     Given a gate, converts a character name of a device into the index
    !     of the device. If the gate does not contain the device name, miss_val_i
    !     is returned. Element-by-element search, so this routine is expensive.
    integer function deviceIndex(parentgate, devName)

        implicit none
        type(Gate) :: parentgate
        character:: devName*32
        character:: parname*32 = ' '
        integer :: idev
        do idev = 1, parentgate%nDevice
            parname = parentgate%devices(idev)%name
            call locase(parname)
            if (parname == devName) then
                deviceIndex = idev
                return
            end if
        end do
        deviceIndex = miss_val_i
        return
    end function

    subroutine deviceTypeString(typeString, devType)
        use IO_Units
        implicit none
        character*32, intent(out) :: TypeString
        integer, intent(in) :: devType
        TypeString = " "
        if (devType == WEIR) then
            TypeString = "weir"
        else if (devType == PIPE) then
            TypeString = "pipe"
        else
            write (unit_error, '(a,i)') &
                "Gate device type not recognized (in function deviceTypeString):", &
                devType
            call exit(3)
        end if
        return
    end subroutine

    subroutine controlTypeString(typeString, controlType)
        use IO_Units
        implicit none
        character*32, intent(out) :: TypeString
        integer, intent(in) :: controlType
        if (controlType == UNIDIR_TO_NODE) then
            typeString = "unidir to node"
        else if (controlType == UNIDIR_FROM_NODE) then
            typeString = "unidir from node"
        else
            write (unit_error, '(a)') "Gate control type not recognized "// &
                "(in function controlTypeString)"
            call exit(3)
        end if
        return
    end subroutine

end module Gates
