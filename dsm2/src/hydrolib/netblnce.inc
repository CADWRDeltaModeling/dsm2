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

*===== BOF netblnce.inc ================================================
*   Version 93.01, January, 1993

*   Note: 'network.inc' must appear before this file.

      REAL*8 InitialVolume(MaxChannels), InitialMass(MaxChannels)
      REAL*8 Volume(MaxChannels), Mass(MaxChannels)
      REAL*8 JnctVolFlow(MaxChannels), JnctMassFlow(MaxChannels)
      REAL*8 LateralVolFlow(MaxChannels), LateralMassFlow(MaxChannels)

      COMMON / NetworkBalance /
     &     InitialVolume,     InitialMass,
     &     Volume,            Mass,
     &     JnctVolFlow,    JnctMassFlow,
     &     LateralVolFlow, LateralMassFlow

      SAVE / NetworkBalance /

*   Definitions:
*     InitialVolume(i) - initial volume of fluid in channel i.
*     InitialMass(i) - initial mass of fluid in channel i.
*     Volume(i) - current volume of fluid in channel i.
*     Mass(i) - current mass of fluid in channel i.
*     JnctVolFlow(i) - net change of volume through ends of channel i.
*     JnctMassFlow(i) - net change of mass through ends of channel i.
*     LateralVolFlow(i) - net change of volume resulting from flows
*                         distributed along reaches of channel i.
*     LateralMassFlow(i) - net change of mass resulting from flows
*                         distributed along reaches of channel i.

*===== EOF netblnce.inc ================================================
