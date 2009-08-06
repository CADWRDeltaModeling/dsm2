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

*===== BOF netdense.inc===================================================
*   Version 93.01, January, 1993

*     Note: "network.inc" must appear prior to this include file.

      INTEGER FortranUnit
      INTEGER StartSeconds, dT
      INTEGER OldTime, NewTime, CurrentTime
      REAL*8 Old(2*MaxChannels), New(2*MaxChannels)
      REAL*8 Current(2*MaxChannels),StreamBndValue(2*MaxChannels)
      LOGICAL ReadBoundaryValues

      COMMON / NetDense_i / FortranUnit,
     &     StartSeconds, dT,
     &     OldTime, NewTime, CurrentTime
	  COMMON / NetDense_r/
     &     Old, New, Current,StreamBndValue,
     &     ReadBoundaryValues
      SAVE   / NetDense_i /, /NetDense_r/

*   Definitions:
*     FortranUnit - Fortran unit number for density data.
*     StartSeconds - starting elapse time, in seconds.
*     dT - boundary-value time-series time increment.
*     OldTime - currently the time corresponding to oldest set of
*               boundary values in memory.
*     NewTime - currently the time corresponding to newest set of
*               boundary values in memory.
*     CurrentTime - current Network time, in seconds.
*     Old(i) - boundary values at OldTime.
*     New(i) - boundary values at NewTime.
*     Current(i) - boundary values at CurrentTime.
*     ReadBoundaryValues - indicator,
*                          [.TRUE.] read values.
*                          [.FALSE.] don't read values.
*
*===== EOF netdense.inc ==================================================
