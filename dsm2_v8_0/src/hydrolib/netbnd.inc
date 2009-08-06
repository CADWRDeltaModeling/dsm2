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

*===== BOF netbnd =====================================================
*   Version 93.01, January, 1993

*     Note: "network.inc" must appear prior to this include file.

      INTEGER BndValues, NetBndValUnit
      INTEGER Channel(2*MaxChannels)
      INTEGER StartSeconds, bdt
      INTEGER OldTime, NewTime, CurrentTime
      REAL*8 Old(2*MaxChannels), New(2*MaxChannels)
      REAL*8 Current(2*MaxChannels),StreamBndValue(2*MaxChannels)
      LOGICAL ReadBoundaryValues

      COMMON / NetBndVal_i / BndValues, NetBndValUnit, Channel,
     &     StartSeconds, bdt,
     &     OldTime, NewTime, CurrentTime,
     &     ReadBoundaryValues
	   COMMON / NetBndVal_r /
     &     Old, New, Current,StreamBndValue
      SAVE   / NetBndVal_i/ , /NetBndVal_r/

*   Definitions:
*     BndValues - number of values to be read each time step.
*     NetBndValUnit - FORTRAN unit number.
*     Channel(i) - channel number, + upstream or - downstream end.
*     StartSeconds - starting elapse time, in seconds.
*     bdT - boundary-value time-series time increment.
*     OldTime - currently the time corresponding to oldest set of
*               boundary values in memory.
*     NewTime - currently the time corresponding to newest set of
*               boundary values in memory.
*     CurrentTime - current Network time, in seconds.
*     Old(i) - boundary values at OldTime.
*     New(i) - boundary values at NewTime.
*     Current(i) - boundary values at CurrentTime.
*     StreamBndValue(k) - boundary value computed from equation, possibly
*                         one upstream and one downstream value,
*                         sequentially, odd "k" are upstream,
*                         even are downstream.
*     ReadBoundaryValues - indicator,
*                          [.TRUE.] read values.
*                          [.FALSE.] don't read values.
*
*===== EOF netbnd =====================================================
