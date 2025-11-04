module gates_data
    ! Data-only module for gate types and global gate arrays.
    use type_defs
    use constants
    implicit none

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
        real*8 :: maxWidth        ! width or pipe radius
        real*8 :: baseElev        ! invert or crest or bottom elevation wrt datum
        real*8 :: height          ! height or maximum aperture height
        real*8 :: flowCoefToNode   ! flow coefficient (physical) in node direction
        real*8 :: flowCoefFromNode ! flow coeff from node to water body
        real*8 :: opCoefToNode = 1. ! time varying coefficient between [0,1] fixme: default?
        real*8 :: opCoefFromNode = 1. !  0 = closed, 1 = fully open
        integer*4 :: structureType ! type of gate structure (pipe, weir)
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
        real*8 flow
        real*8 flowPrev
        integer :: ID
        logical*4 :: inUse = .false.
        logical*4 :: free = .false.
        integer*4 :: objConnectedType ! Object Type to which gate is connected (e.g. obj_channel,obj_reservoir)
        integer*4 :: objConnectedID   ! Internal number of object (channel, reservoir) to which gate is attached
        integer*4 :: objCompPoint     ! Index of computation point at water body
        integer*4 :: subLocation      ! location within objConnectedID
        integer*4 :: node             ! Node to which gate is attached
        integer*4 :: nodeCompPoint    ! Index of (stage reference) computation point at node
        integer*4 :: nDevice = 0      ! number of devices in gate structure
        type(datasource_t) install_datasource
        type(GateDevice), dimension(MAX_DEV) :: devices ! Array of devices in gate
        character :: name*32 = ' '
    end type

    integer, save :: NTotalDevice = 0

    type(Gate), dimension(MaxNGate), target, save :: gateArray

end module gates_data
