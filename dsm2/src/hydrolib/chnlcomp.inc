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

*===== BOF chncomp.inc ================================================
*   Version 93.01, January, 1993

*   Note: This include, when used, must follow  after "Network.inc".

      INTEGER TotalCompLocations
      INTEGER NumberOfCompLocations(MaxChannels)
      INTEGER UpCompPointer(MaxChannels)
      INTEGER DownCompPointer(MaxChannels)
      REAL*8    CompLocation(MaxLocations)
      REAL*8    DummyArray(MaxLocations), DummyArray2(MaxLocations)
      CHARACTER*16 DummyCharArray(MaxLocations)
      REAL*8  AreaChannelComp(MaxChannels, 30) ! Assume max of 10 compute points in any channel
      COMMON /ChCompLocations_i/
     &     TotalCompLocations,
     &     NumberOfCompLocations,
     &     UpCompPointer, DownCompPointer
      COMMON /ChCompLocations_r/
     &     CompLocation,
     &     DummyArray, DummyArray2,
     &     AreaChannelComp
      COMMON / DummyChar / DummyCharArray
      SAVE / ChCompLocations_i /, / ChCompLocations_r /, / DummyChar / 

*   Definitions:
*     TotalCompLocations - total number of computational locations
*                          in the current application.

*           for the current channel, i,
*     MaxChannels - maximum number of channels.
*     NumberOfCompLocations(i) - number of computational locations.
*     UpCompPointer(i) - global sequence number of most upstream
*                        location.
*     DownCompPointer(i) - global sequence number of most downstream
*                          location.

*            for the global sequence number, j,
*     CompLocation(j) - downstream distance coordinate.
*     DummyArray(j) - a REAL*8 value dependent upon context.
*     DummyArray2(j) - a REAL*8 value dependent upon context.
*     DummyCharArray(j) - a CHARACTER value dependent upon context.

*===== EOF chncomp.inc ================================================
