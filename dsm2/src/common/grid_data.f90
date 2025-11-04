!!<license>
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

module grid_data
    !! Module to hold grid data
    use type_defs
    use array_limits
    implicit none

    integer :: max_xsects_tot        ! maximum number of cross sections total
    integer :: max_obj2obj           ! maximum number of object to object connections
    integer :: max_stgbnd            ! maximum number of stage boundaries
    integer :: nstgbnd               ! actual number of stage boundaries
    integer :: nflwbnd               ! actual number of flow boundaries
    integer :: nobj2obj              ! actual number of object to object connections
    integer :: nchans                ! actual number of channels
    integer :: nintnodes             ! actual number of interior nodes (non-boundary nodes)
    integer :: nnodes                ! actual number of total nodes (interior + boundary)
    integer :: nxsects               ! actual number of cross sections
    parameter( &
        max_xsects_tot=5*max_channels, &
        max_obj2obj=50, &
        max_stgbnd=5 &
        )

    !@#type(channel_t) :: chan_geom(0:max_channels)
    type(channel_t), allocatable :: chan_geom(:)    !@# From GTM (module common_variables)
    type(node_t) :: node_geom(0:max_nodes)
    type(xsect_t) :: xsect_geom(0:max_xsects_tot)

    real*8 :: &
        area_tolerance               ! max allowable ratio of virt_area(MSL) @ chan ends
    real*8 :: levee_slope            ! slope of levees for xsect width extrapolation

    integer :: nreser                ! actual number of reservoirs
    integer :: nres_connect          ! total number of reservoir connections across all reservoirs

    type(reservoir_t) :: res_geom(0:max_reservoirs)

    !-----Node id numbers

    integer :: node_id(0:max_nodes)
    common/node_i_dentification/node_id

    integer :: &
        nchan_list                   ! actual number of channel sequences

    integer, allocatable :: int2ext(:)
    integer :: resext2int(0:max_reservoirs)
    integer :: resint2ext(0:max_reservoirs)
    integer :: nodelist(0:max_nodes*2 + 1)

    !-----Direct object to object flow transfer
    type(obj2obj_t) :: obj2obj(max_obj2obj)

    !-----stage boundary object
    type(stgbnd_t) :: stgbnd(max_stgbnd)

    !-----quad points
    ! integer:: nquadpts = 3           ! number of quadrature points

    !-----used by virtual_xsect
    real(kind=4), allocatable :: chan_dx(:)  ! variable dx for dsm2

    !-----accounting and object names, value codes, period type names
    integer, parameter :: max_group_memberships = 20 ! max number of group memberships

    integer :: &
        max_qext                     ! maximum number of external flows
    integer :: nqext                 ! number of actual external flows

    parameter( &
        max_qext=1100 &
        )

    type(qext_t) :: qext(max_qext)

    integer :: const_qext(max_qext, max_conqext)
    integer :: n_conqext(max_qext)   ! number of constituents at external flow
    common/com_conqext/const_qext, n_conqext

contains
    ! Convert an external channel number to internal number
    ! using a binary search.
    integer function ext2int(extchan)
        use ifport
        ! use grid_data
        implicit none
        integer extchan
        ext2int = bsearchqq(loc(extchan), loc(int2ext(1)), nchans, SRT$INTEGER4)
        return
    end function

!     Compare two integers (e.g., as needed for qsort)
    integer(2) function compareInt(arg1, arg2)
        implicit none
        integer arg1, arg2
        compareInt = arg1 - arg2
        return
    end function

!-----Convert an external node number to an internal one using
!     binary search
    integer function ext2intnode(extnode)
        use ifport
        ! use grid_data
        implicit none
        integer extnode
        ext2intnode = bsearchqq(loc(extnode), loc(nodelist(1)), &
                                nintnodes, SRT$INTEGER4)
        if (ext2intnode .gt. 0) return
        ext2intnode = bsearchqq(loc(extnode), loc(nodelist(nintnodes + 1)), &
                                (nnodes - nintnodes), SRT$INTEGER4)
        if (ext2intnode .gt. 0) then
            ext2intnode = ext2intnode + nintnodes
        end if

        return
    end function

end module
