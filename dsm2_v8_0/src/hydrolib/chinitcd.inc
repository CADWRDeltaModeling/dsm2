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

*===== BOF chinitcd.inc ==================================================
*   Version 93.01, January, 1993

*   Note: This include, if used, must appear after "Network.inc".

      INTEGER ChannelNumber(MaxChannels), Locations(MaxChannels)
      INTEGER FirstLocation(MaxChannels),NUserInitLocations(MaxChannels)
      INTEGER InitialConditionIndex(MaxChannels)
      REAL*8 InitialX(MaxLocations)
      REAL*8 InitialWS(MaxLocations), InitialQ(MaxLocations)
      LOGICAL InitCndInitialized
      COMMON / ChInitialFlow_I / ChannelNumber, Locations, FirstLocation,NUserInitLocations,
     &     InitialConditionIndex,
     &     InitCndInitialized

      COMMON / ChInitialFlow_r / 
     &     InitialX, InitialWS, InitialQ
      SAVE / ChInitialFlow_i /, /ChInitialFlow_r/


*   Definitions:
*     ChannelNumber(j) - channel number corresponding to sequence number "j".
*     Locations(j) - number of locations in ChannelNumber(j).
*     FirstLocation(j) - sequence number of first location in ChannelNumber(j).
*     InitialConditionIndex( ChannelNumber(j) ) - sequence number corresponding
*                                                 to ChannelNumber(j).
*     InitialX(i) - stream distance at initial location "i".
*     InitialWS(i) - water surface elevation at initial location "i".
*     InitialQ(i) - volumetric discharge at initial location "i".

*===== EOF chinitcd.inc ==================================================
