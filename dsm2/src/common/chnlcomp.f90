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

!===== BOF chncomp.inc ================================================
!   Version 93.01, January, 1993
module chnlcomp
    !   Note: This include, when used, must follow  after "Network.inc".
    use network, only: MaxChannels, MaxLocations, MaxCompPts
    integer, save :: TotalCompLocations &
                     , NumberOfCompLocations(MaxChannels) &
                     , UpCompPointer(MaxChannels) &
                     , DownCompPointer(MaxChannels)
    real*8, save ::    CompLocation(MaxLocations) &
                    , ChannelNo(MaxLocations) &
                    , DummyArray(MaxLocations) &
                    , DummyArray2(MaxLocations) &
                    , AreaChannelComp(MaxChannels, MaxCompPts) ! Assume max of 10 compute points in any channel
    character*16, save:: DummyCharArray(MaxLocations)

!   Definitions:
!     TotalCompLocations - total number of computational locations
!                          in the current application.

!           for the current channel, i,
!     MaxChannels - maximum number of channels.
!     NumberOfCompLocations(i) - number of computational locations.
!     UpCompPointer(i) - global sequence number of most upstream
!                        location.
!     DownCompPointer(i) - global sequence number of most downstream
!                          location.

!            for the global sequence number, j,
!     CompLocation(j) - downstream distance coordinate.
!     DummyArray(j) - a REAL*8 value dependent upon context.
!     DummyArray2(j) - a REAL*8 value dependent upon context.
!     DummyCharArray(j) - a CHARACTER value dependent upon context.
end module
!===== EOF chncomp.inc ================================================
