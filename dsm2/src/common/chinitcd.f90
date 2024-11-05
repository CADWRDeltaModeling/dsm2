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

!===== BOF chinitcd.inc ==================================================
!   Version 93.01, January, 1993
module chinitcd
    use network
    integer, save, allocatable:: Locations(:), InitialConditionIndex(:)
    integer, save:: ChannelNumber(MaxChannels)
    integer, save:: FirstLocation(MaxChannels),NUserInitLocations(MaxChannels)
    real*8, save:: InitialX(MaxLocations)
    real*8, save:: InitialWS(MaxLocations), InitialQ(MaxLocations)
    logical, save:: InitCndInitialized
end module

!   Definitions:
!     ChannelNumber(j) - channel number corresponding to sequence number "j".
!     Locations(j) - number of locations in ChannelNumber(j).
!     FirstLocation(j) - sequence number of first location in ChannelNumber(j).
!     InitialConditionIndex( ChannelNumber(j) ) - sequence number corresponding
!                                                 to ChannelNumber(j).
!     InitialX(i) - stream distance at initial location "i".
!     InitialWS(i) - water surface elevation at initial location "i".
!     InitialQ(i) - volumetric discharge at initial location "i".

!===== EOF chinitcd.inc ==================================================
