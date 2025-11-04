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

    subroutine process_gate(id, &
                            name, &
                            ObjConnTypeName, &
                            ObjConnID, &
                            nodeConn)
        use IO_Units
        use logging
        use constants
        use grid_data
        use mod_name_to_objno
        implicit none
        integer &
            ID, &
            ObjConnType, &       ! connected to channel, reservoir, etc.
            NodeConn, &          ! node connected to
            channo, &
            resno, &
            counter, &
            i

        character &
            name*32, &
            prev_name*32, &
            ObjConnID*32, &       ! name of reservoir, number of channel
            channoStr*10
        character(len=16) :: ObjConnTypeName

        logical :: useObj

        ObjConnType = obj_type_code(ObjConnTypeName)
        ngate = ngate + 1
        if (ngate .gt. max_gates) then
            write (unit_error, 630) &
                'Too many gates specified; max allowed is:', max_gates
            call exit(-1)
            return
        end if
630     format(/a, i5)
        gateArray(ngate)%ID = ID
        gateArray(ngate)%inUse = .true.
        call locase(name)
        call locase(objConnID)
        gateArray(ngate)%name = trim(name)
        gateArray(ngate)%objConnectedType = ObjConnType
        gateArray(ngate)%node = ext2intnode(NodeConn)
        gateArray(ngate)%install_datasource%source_type = const_data
        gateArray(ngate)%install_datasource%indx_ptr = 0 !fixme: is this is OK?
        gateArray(ngate)%install_datasource%value = 1.
        ObjConnID = trim(ObjConnID)
        call locase(ObjConnID)
        if ((ObjConnType .eq. OBJ_CHANNEL)) then
            ! channo = name_to_objno(ObjConnType, objConnID)
            read(ObjConnID,*) i
            channo = ext2int(i)
            gateArray(ngate)%objConnectedID = channo
        else if (ObjConnType .eq. OBJ_RESERVOIR) then
            ! resno = name_to_objno(ObjConnType, objConnID)
            do i = 1,nreser
                if (res_geom(i)%name .eq. objConnID) then
                    resno = i
                    exit
                end if
            end do
            gateArray(ngate)%objConnectedID = resno
            do i = 1, res_geom(resno)%nnodes
                if (res_geom(resno)%node_no(i) .eq. gateArray(ngate)%node) then
                    write (unit_error, 627) trim(name), trim(res_geom(resno)%name), &
                        node_geom(gateArray(ngate)%node)%node_ID
627                 format('Gate ', a, ' attached from reservoir ', a, ' to node ', &
                           i5, /'conflicts with a gate or reservoir connection '/ &
                           'defined between the same reservoir and node. '/ &
                           'Use a single gate or reservoir connection.')
                    call exit(1)
                end if
            end do
            res_geom(resno)%nnodes = res_geom(resno)%nnodes + 1
            res_geom(resno)%node_no(res_geom(resno)%nnodes) = gateArray(ngate)%node
            res_geom(resno)%isNodeGated(res_geom(resno)%nnodes) = .true.
            gateArray(ngate)%subLocation = res_geom(resno)%nnodes
        else
            write (unit_error, 628) name
628         format(/'Gate ', a, ' connected to an object that is not supported')
        end if

        gateArray(ngate)%flowDirection = 0.D0 ! fixme: depends on location upstream or down.
        if (print_level .ge. 3) &
            write (unit_screen, '(i5,1x,a,i10)') &
            ngate, &
            trim(gateArray(ngate)%name), &
            gateArray(ngate)%ID

        return
    end subroutine

    !================================================================

    subroutine process_gate_device( &
        gatename, &
        name, &
        structure_name, &
        nduplicate, &
        max_width, &
        base_elev, &
        height, &
        cffrom, &
        cfto, &
        default_op_name)
        use io_units
        use constants
        use mod_name_to_objno

        implicit none

        !-----local variables

        integer &
            gateID, &              ! gate ID
            gateno, &             ! counter for gates
            devno, &
            struct_type, &         ! type of structure (weir,pipe)
            control_type, &        ! flow control device (type of gate)
            count, &
            ndx, i, nw, &
            nout, &
            default_op, &
            get_objnumber       ! function to get object number


        real*8 &
            max_width, &
            base_elev, height, &
            nduplicate, &          ! number of dublicate structures
            CFfrom, CFto, &
            from_op, to_op

        character*32 &
            name, &
            gatename
        character*8 structure_name
        character*16 default_op_name

        call locase(name)
        call locase(gatename)
        call locase(structure_name)
        call locase(default_op_name)
        if (structure_name(1:4) .eq. 'weir') then
            struct_type = WEIR
        elseif (structure_name(1:4) .eq. 'pipe') then
            struct_type = PIPE
        else
            write (unit_error, *) "Gate structure not recognized: " &
                //structure_name//", Gate: "//trim(gatename) &
                //" Device: "//trim(name)
            call exit(-3)
        end if

        if (default_op_name .eq. 'gate_open') then
            default_op = GATE_OPEN
            to_op = 1.
            from_op = 1.
        elseif (default_op_name .eq. 'gate_close') then
            default_op = GATE_OPEN
            to_op = 0.
            from_op = 0.
        elseif (default_op_name .eq. 'unidir_to_node') then
            default_op = UNIDIR_TO_NODE
            to_op = 1.
            from_op = 0.
        elseif (default_op_name .eq. 'unidir_from_node') then
            default_op = UNIDIR_FROM_NODE
            to_op = 0.
            from_op = 1.0
        else
            write (unit_error, "('Unrecognized default operation for gate',1x, &
                              &a,' device ',a, ' op ',a)") trim(gatename), &
                trim(name), trim(default_op_name)
            call exit(-3)
            return
        end if
        gateNo = name_to_objno(obj_gate, gatename)
        devNo = gateArray(gateNo)%nDevice
        devNo = devNo + 1
        gateArray(gateNo)%nDevice = devNo
        call locase(name)
        gateArray(gateNo)%devices(devNo)%name = trim(name)
        gateArray(gateNo)%devices(devNo)%structureType = struct_type
        gateArray(gateNo)%devices(devNo)%flowCoefFromNode = CFfrom
        gateArray(gateNo)%devices(devNo)%flowCoefToNode = CFto
        gateArray(gateNo)%devices(devNo)%nduplicate = nduplicate
        gateArray(gateNo)%devices(devNo)%maxWidth = max_width
        gateArray(gateNo)%devices(devNo)%height = height
        gateArray(gateNo)%devices(devNo)%baseElev = base_elev

        gateArray(gateNo)%devices(devNo)%op_to_node_datasource%source_type = const_data
        !fixme: is this next line OK?
        gateArray(gateNo)%devices(devNo)%op_to_node_datasource%indx_ptr = 0
        gateArray(gateNo)%devices(devNo)%op_to_node_datasource%value = to_op

        gateArray(gateNo)%devices(devNo)%op_from_node_datasource%source_type = const_data
        gateArray(gateNo)%devices(devNo)%op_from_node_datasource%indx_ptr = 0    !fixme: is this OK?
        gateArray(gateNo)%devices(devNo)%op_from_node_datasource%value = from_op

        gateArray(gateNo)%devices(devNo)%width_datasource%source_type = const_data
        gateArray(gateNo)%devices(devNo)%width_datasource%indx_ptr = 0    !fixme: is this OK?
        gateArray(gateNo)%devices(devNo)%width_datasource%value = max_width
        gateArray(gateNo)%devices(devNo)%height_datasource%source_type = const_data
        gateArray(gateNo)%devices(devNo)%height_datasource%indx_ptr = 0    !fixme: is this OK?
        gateArray(gateNo)%devices(devNo)%height_datasource%value = height
        gateArray(gateNo)%devices(devNo)%elev_datasource%source_type = const_data
        gateArray(gateNo)%devices(devNo)%elev_datasource%indx_ptr = 0    !fixme: is this OK?
        gateArray(gateNo)%devices(devNo)%elev_datasource%value = base_elev
        gateArray(gateNo)%devices(devNo)%nduplicate_datasource%source_type = const_data
        gateArray(gateNo)%devices(devNo)%nduplicate_datasource%indx_ptr = 0 !fixme: is this OK?
        gateArray(gateNo)%devices(devNo)%nduplicate_datasource%value = nduplicate

        return
    end subroutine
end module Gates
