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

*===== BOF chstatus.inc ==================================================
*   Version 93.01, January, 1993

*   Note: This include, if used, must appear after "Network.inc".

      REAL*8,target :: WS(MaxLocations)
	  REAL*8,target :: Q(MaxLocations)
	  REAL*8,target :: H(MaxLocations)
      REAL*8 Rho1(MaxLocations), Rho2(MaxLocations)
      REAL*8 QOld(MaxLocations)
      LOGICAL ConvergedSteady(MaxChannels)
      COMMON / ChnlFlowStatus /  WS, Q, QOLD,H, Rho1, Rho2
	  COMMON /ChnlFlowConvStatus/ConvergedSteady
      SAVE / ChnlFlowStatus /,/ChnlFlowConvStatus/

*   Definitions:
*     WS(i) - water surface elevation at computational location "i".
*     Q(i) - volumetric discharge at computational location "i".
*     H(i) - depth of flow at computational location "i".
*     Rho1(i) - density at computational location "i", at the
*               begining of the current time increment.
*     Rho1(i) - density at computational location "i", at the
*               the end of the current time increment.
*     ConvergedSteady(m) - .TRUE. if steady solution converged,
*                           otherwise .FALSE.

*===== EOF chstatus.inc ==================================================
