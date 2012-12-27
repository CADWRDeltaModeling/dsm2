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

!===== BOF netbnd =====================================================
!   Version 93.01, January, 1993

module netbnd
    use network
    implicit none
    integer,save:: BndValues, NetBndValUnit
    integer,save:: Channel(2*MaxChannels)
    integer,save :: StartSeconds, bdt
    integer,save :: OldTime, NewTime, CurrentTime
    real*8,save :: Old(2*MaxChannels), New(2*MaxChannels)
    real*8,save :: Current(2*MaxChannels),StreamBndValue(2*MaxChannels)
    logical,save :: ReadBoundaryValues
end module
!   Definitions:
!     BndValues - number of values to be read each time step.
!     NetBndValUnit - FORTRAN unit number.
!     Channel(i) - channel number, + upstream or - downstream end.
!     StartSeconds - starting elapse time, in seconds.
!     bdT - boundary-value time-series time increment.
!     OldTime - currently the time corresponding to oldest set of
!               boundary values in memory.
!     NewTime - currently the time corresponding to newest set of
!               boundary values in memory.
!     CurrentTime - current Network time, in seconds.
!     Old(i) - boundary values at OldTime.
!     New(i) - boundary values at NewTime.
!     Current(i) - boundary values at CurrentTime.
!     StreamBndValue(k) - boundary value computed from equation, possibly
!                         one upstream and one downstream value,
!                         sequentially, odd "k" are upstream,
!                         even are downstream.
!     ReadBoundaryValues - indicator,
!                          [.TRUE.] read values.
!                          [.FALSE.] don't read values.
!
!===== EOF netbnd =====================================================
