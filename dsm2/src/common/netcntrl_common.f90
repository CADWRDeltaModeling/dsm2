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

!===== BOF netcntrl.inc ================================================
!   Version 93.01, January, 1993

module netcntrl_common
    use constants
    use network
    implicit none

    real*8 :: Theta = 0.6D0
    integer :: Terms
    real*8 :: ToleranceQ = 0.0005D0
    real*8 :: ToleranceZ = 0.0005D0
    logical :: VariableSinuosity = .false.
    logical :: VariableDensity = .false.
    integer :: MaxIterations = 50
    integer :: LuInc = 1

    real*8 :: QuadPt(MaxQuadPts)
    real*8 :: QuadWt(MaxQuadPts)

end module netcntrl_common
