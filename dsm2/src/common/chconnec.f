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

!===== BOF chconnec.inc ================================================
!   Version 93.01, January, 1993
module chconnec
    use network
    !   Note: This include, when used, must appear after "Network.inc".
    real*8, save::    Ares(MaxNres), &
        Yres(MaxNres), &
        Dres(MaxNres), &
        ReservoirCoeff(MaxNres,MaxResConnectChannel,2), &
        QRes(MaxNres,MaxResConnectChannel),      &
        YResOld(MaxNres), &
        QResOld(MaxNres,MaxResConnectChannel), &
        VResOld(MaxNres)
    integer, save::  ResConnectingChannels(MaxNres, MaxResConnectChannel)

    integer, save:: UpBoundaryCode(MaxChannels) &
        ,UpNumberOfConnections(MaxChannels) &
        ,UpConnection(MaxChannels*MaxConnectingChannels) &
        ,DownBoundaryCode(MaxChannels) &
        ,DownNumberOfConnections(MaxChannels) &
        ,DownConnection(MaxChannels*MaxConnectingChannels) &
        ,nodeSumQChan(MaxNodes)
    real*8,save::   dX(MaxChannels)

end module

!===== EOF chconnec.inc ================================================
