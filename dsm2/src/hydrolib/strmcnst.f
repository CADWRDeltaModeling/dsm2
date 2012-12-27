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

!===== BOF strmcnst.inc =================================================
!   Version 93.01, January, 1993
module strmcnst
    use network
    implicit none
    integer, save:: ConnectingExtremity( 1+MaxConnectingChannels )
    integer, save:: ConnectingChannels
    integer, save:: ConditionCode

    real*8, save:: ConstraintCoef( 2+2*MaxConnectingChannels )
    real*8, save:: ConstraintRightSide
    real*8, save:: Discharge( 1+MaxConnectingChannels )
    real*8, save:: WSElev( 1+MaxConnectingChannels )
end module
!   Definitions:
!
!     ConditionCode - boundary-condition code.
!
!     ConstraintCoef(i) - implicit constraint-equation coefficients,
!                        multipliers of the following:
!       [1] discharge at constraint cross section,
!       [2] water-surface elevation at constraint cross section,
!       [3] discharge at first connecting node,
!       [4] water-surface elevation at first connecting cross section,
!       [3] discharge at second connecting node,
!       [4] water-surface elevation at second connecting cross section,
!           above pairs repeated for all connecting cross sections...
!
!     ConstraintRightSide - right-hand side of constraint equation.
!
!     Discharge(i) - current discharge at
!          [1] constraint node
!          [2] connection to first connecting channel,
!          [3] connection to second connecting channel,
!              repeating for all connectioning channels...
!
!     WSElev(i) - current water-surface elevation at
!          [1] constraint node
!          [2] connection to first connecting channel,
!          [3] connection to second connecting channel,
!              repeating for all connectioning channels...
!
!     ConnectingChannels - number of connecting channels
!
!     ConnectingExtremity(i) - index indicating
!
!          [+1] upstream end of channel, or
!          [-1] downstream end of channel,
!
!             for connecting channel i, where i indicates
!
!          [1] constraint channel,
!          [2] first connecting channel,
!          [3] second connecting channel,
!              repeating for all channels...
!
!
!     Note discharge sign convention:
!
!     Discharge is considered positive if flowing from upstream
!     to downstream end of channel.  ConnectingExtremity will be
!     +1 for and upstream end and -1 for a downstream end.
!
!===== EOF strmcnst.inc ================================================
