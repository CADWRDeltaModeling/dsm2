module array_limits
    !! Module defining array size limits.
    !!
    !! This module is intended to keep array size limits in one place.
    implicit none

    integer, parameter :: MAX_CHANNELS = 800
        !! Maximum number of channels
    integer, parameter :: MAX_NODES = MAX_CHANNELS + 10
        !! max number of nodes (junctions)
    integer, parameter :: MAX_RESERVOIRS = 100
        !! Maximum number of reservoirs

    integer, parameter :: MAX_CONSTITUENT = 24
        !! Maximum number of constituents
    integer, parameter :: MAX_CONQEXT = 12
        !! Maximum number of constituents for an external flow

    integer, parameter :: MaxNres = 100       ! max reservoirs allowed
    ! fixme: should be same as max_reservoirs
    ! in common.f
    integer, parameter :: MaxConnectingChannels = 5  ! Max connections at a node

    integer, parameter :: MaxNgate = 300

    integer, parameter :: MaxResConnectChannel = 50
        !! Maximum reservoir connections to channels/nodes
    integer, parameter :: MaxCompPts = 100
        !! Maximum computational points in a channel

    integer, parameter :: MAX_LOCATIONS = 25000
        !! Maximum number of compute locations

    integer, parameter :: NCOEF_TYPE = 10
        !! misc max values for non-conservative constituents

contains
end module
