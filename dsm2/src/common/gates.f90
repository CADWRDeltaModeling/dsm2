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
    !FIXME: use network
    implicit none

    integer, parameter :: MaxNGate = 300 ! use network.f
    ! Maximums for pre-dimensioned arrays
    integer, parameter :: MAX_DEV = 10 ! Max no. of devices per gate
    integer, parameter :: MAX_GATES = MaxNGate ! Max no. of gates

    integer, save :: nGate    ! Actual number of gates calc'd at run time

    !---- Constants for structureType
    integer, parameter :: WEIR = 1
    integer, parameter :: PIPE = 2

    !-----gate operations/states
    integer, parameter :: GATE_OPEN = 1  ! device fully open to flow
    integer, parameter :: GATE_CLOSE = 0 ! device fully closed to flow
    integer, parameter :: GATE_FREE = 10 ! all devices removed, open channel flow
    integer, parameter :: UNIDIR_TO_NODE = 20   ! device open to flow to node, closed from node
    integer, parameter :: UNIDIR_FROM_NODE = 40 ! device open to flow from node, closed to node

    !-----constants for flow coeff direction
    integer, parameter :: FLOW_COEF_TO_NODE = 1
    integer, parameter :: FLOW_COEF_FROM_NODE = -1
    integer, parameter :: FLOW_COEF_TO_FROM_NODE = 0

    type GateDevice       !Keep variables in natural alignment.
        real*8 :: flow        ! fixme: initialization
        ! time of installation
        real*8 :: position = miss_val_r  ! position (in physical units) of any gate control
        ! e.g., height of a radial gate or
        ! crest elevation in a gate with adjustable bottom
        real*8 :: maxWidth        ! width or pipe radius
        real*8 :: baseElev        ! invert or crest or bottom elevation wrt datum
        real*8 :: height          ! height or maximum aperture height
        real*8 :: flowCoefToNode   ! flow coefficient (physical) in node direction
        real*8 :: flowCoefFromNode ! flow coeff from node to water body
        real*8 :: opCoefToNode = 1. ! time varying coefficient between [0,1] fixme: default?
        real*8 :: opCoefFromNode = 1. !  0 = closed, 1 = fully open
        integer*4 :: structureType ! type of gate structure (pipe, weir)%
        ! See structureType constants above for acceptable values
        real*8 :: nDuplicate = 0. ! number of identical structures treated as one device
        integer*4 :: gate         ! index of gate in which device appears (fixme: why?)
        integer*4 :: calcRow      ! Row (equation) in which gate device equation is expressed
        character :: name*32 = ' ' ! index of device in gate
        type(datasource_t) height_datasource  ! datasource that controls
        type(datasource_t) width_datasource  ! datasource that controls
        type(datasource_t) elev_datasource  ! datasource that controls
        type(datasource_t) nduplicate_datasource  ! datasource that controls
        type(datasource_t) op_to_node_datasource   ! datasource that controls op
        type(datasource_t) op_from_node_datasource ! in the direction indicated

    end type

    !Variables are in natural (8byte) alignment.
    type Gate
        real*8 flowDirection        ! orientation of flow. (+1.D0) if downstream/pos flow is from
        ! gate's connected object to node. (-1.D0) otherwise.
        real*8 flow
        real*8 flowPrev
        integer :: ID
        !  see note above on alignment
        logical*4 :: inUse = .false. ! use gate? Used to indicate that the gate is not even
        !  included in the model
        logical*4 :: free = .false. ! are all devices considered uninstalled? If so this gate,
        ! for the moment, will act like a simple node connection
        ! between two channels.
        integer*4 :: objConnectedType ! Object Type to which gate is connected (e.g. obj_channel,obj_reservoir)
        integer*4 :: objConnectedID   ! Internal number of object (channel, reservoir) to which gate is attached
        integer*4 :: objCompPoint     ! Index of computation point at water body
        integer*4 :: subLocation      ! location within objConnectedID:
        ! For reservoirs: index of associated reservoir connection
        ! For channels:   extremity (+1 for upstream, -1 for downstream)
        integer*4 :: node             ! Node to which gate is attached
        integer*4 :: nodeCompPoint    ! Index of (stage reference) computation point at node
        integer*4 :: nDevice = 0      ! number of devices in gate structure
        type(datasource_t) install_datasource ! in the direction indicated
        type(GateDevice), dimension(MAX_DEV) :: devices ! Array of devices in gate

        character :: name*32 = ' '   ! name of gate
    end type

    ! fixme is NTotalDevice needed? When is it initialized?
    integer, save :: NTotalDevice = 0

    type(Gate), dimension(MaxNGate), target, save :: gateArray

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
        implicit none
        integer &
            ID, &
            ObjConnType, &       ! connected to channel, reservoir, etc.
            NodeConn, &          ! node connected to
            name_to_objno, &      ! function to get object number
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
        integer, external :: ext2intnode
        integer, external :: obj_type_code

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
            channo = name_to_objno(ObjConnType, objConnID)
            gateArray(ngate)%objConnectedID = channo
        else if (ObjConnType .eq. OBJ_RESERVOIR) then
            resno = name_to_objno(ObjConnType, objConnID)
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

        integer, external :: name_to_objno

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
