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

*===== BOF strmcnst.inc =================================================
*   Version 93.01, January, 1993

*   Note: "network.inc" must precede this include.

      INTEGER ConnectingExtremity( 1+MaxConnectingChannels )
      INTEGER ConnectingChannels
      INTEGER ConditionCode

      REAL*8 ConstraintCoef( 2+2*MaxConnectingChannels )
      REAL*8 ConstraintRightSide
      REAL*8 Discharge( 1+MaxConnectingChannels )
      REAL*8 WSElev( 1+MaxConnectingChannels )

      COMMON  /UserConstraintVariables_I/ 
     &     ConnectingExtremity,
     &     ConditionCode,ConnectingChannels

	  COMMON /UserConstraintVariables_R/
     &      ConstraintCoef,
     &     ConstraintRightSide,
     &     Discharge, WSElev

      SAVE / UserConstraintVariables_I /,
     &      /UserConstraintVariables_R/

*   Definitions:
*
*     ConditionCode - boundary-condition code.
*
*     ConstraintCoef(i) - implicit constraint-equation coefficients,
*                        multipliers of the following:
*       [1] discharge at constraint cross section,
*       [2] water-surface elevation at constraint cross section,
*       [3] discharge at first connecting node,
*       [4] water-surface elevation at first connecting cross section,
*       [3] discharge at second connecting node,
*       [4] water-surface elevation at second connecting cross section,
*           above pairs repeated for all connecting cross sections...
*
*     ConstraintRightSide - right-hand side of constraint equation.
*
*     Discharge(i) - current discharge at
*          [1] constraint node
*          [2] connection to first connecting channel,
*          [3] connection to second connecting channel,
*              repeating for all connectioning channels...
*
*     WSElev(i) - current water-surface elevation at
*          [1] constraint node
*          [2] connection to first connecting channel,
*          [3] connection to second connecting channel,
*              repeating for all connectioning channels...
*
*     ConnectingChannels - number of connecting channels
*
*     ConnectingExtremity(i) - index indicating
*
*          [+1] upstream end of channel, or
*          [-1] downstream end of channel,
*
*             for connecting channel i, where i indicates
*
*          [1] constraint channel,
*          [2] first connecting channel,
*          [3] second connecting channel,
*              repeating for all channels...
*
*
*     Note discharge sign convention:
*
*     Discharge is considered positive if flowing from upstream
*     to downstream end of channel.  ConnectingExtremity will be
*     +1 for and upstream end and -1 for a downstream end.
*
*===== EOF strmcnst.inc ================================================
