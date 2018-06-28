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
module network
    !      fixme: All of these "max" variables are redundant
    integer,parameter :: MaxChannels=800   ! max channels allowed,
                                           ! should be same as max_channels
										         ! in common.f
    integer,parameter :: MaxNres=100       ! max reservoirs allowed
                                           ! fixme: should be same as max_reservoirs
											     ! in common.f
    integer, parameter :: MaxConnectingChannels=5  ! Max connections at a node

    integer,parameter :: MaxNgate = 300
      
    integer,parameter :: MaxNodes=MaxChannels+10  ! max number of nodes (junctions)
    ! MaxResConnectChannel must be consistent with maxresnodes
    integer,parameter :: MaxResConnectChannel=50  ! Maximum reservoir connections to channels/nodes

    integer,save :: NumCh      ! actual number of channels
    integer,save :: Branch		! internal chan number of currently selected channel
	                            ! (many routines assume this has been set to the channel
							    ! to be worked on

    integer, parameter :: MaxLocations=5000   ! Max # compute locations in model
    integer, parameter :: MaxQuadPts=3        ! Max # of quadrature points
	                                            ! for integrating over reaches

end module

!===== EOF network.inc =================================================
