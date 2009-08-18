C!<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    The Delta Simulation Model 2 (DSM2) is free software: 
C!    you can redistribute it and/or modify
C!    it under the terms of the GNU General Public License as published by
C!    the Free Software Foundation, either version 3 of the License, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public License for more details.

C!    You should have received a copy of the GNU General Public License
C!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
C!</license>

*===== BOF network.inc ==================================================
*   Version 93.01, January, 1993

c      fixme: All of these "max" variables are redundant
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

      integer :: NumCh      ! actual number of channels
	  integer :: Branch		! internal chan number of currently selected channel
	                        ! (many routines assume this has been set to the channel
							! to be worked on

      integer, parameter :: MaxLocations=5000   ! Max # compute locations in model
      integer, parameter :: MaxQuadPts=3        ! Max # of quadrature points
	                                            ! for integrating over reaches


      common  /ChannelNetworkData/ NumCh, Branch
      save / ChannelNetworkData /


*===== EOF network.inc =================================================
