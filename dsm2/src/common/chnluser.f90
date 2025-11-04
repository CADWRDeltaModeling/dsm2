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

!===== BOF chnluser.inc =================================================
!   Version 93.01, January, 1993

!   Note: This include, when used, must follow "Network.inc".
module chnluser
    use array_limits
    implicit none
    integer :: NumUserLoc
    integer , allocatable:: UpUserPointer(:), DownUserPointer(:)
    real*8 :: UserWS(MAX_LOCATIONS), UserQ(MAX_LOCATIONS)
    character*16 :: UserLocationID(MAX_LOCATIONS)

end module
!   Definitions:
!     MaxChannels - maximum number of channels.
!     NumUserLoc - current total number of user locations.
!     UpUserPointer(i) - pointer to upstream most user-supplied
!                        location for channel "i".
!     DownUserPointer(i) - pointer to downstream most user-supplied
!                          location for channel "i".
!     UserWS(i) - water surface elevation at user location "i".
!     UserQ(i) - volumetric discharge at user location "i".
!     UserLocationID(i) - 16-character string ID for user-suppllied
!                         location i.
!===== EOF chnluser.inc =================================================
