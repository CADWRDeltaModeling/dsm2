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
    use network
    implicit none
    real*8, save:: Theta
    integer, save:: Terms
    real*8, save:: ToleranceQ
    real*8, save:: ToleranceZ
    logical, save:: VariableSinuosity
    logical, save:: VariableDensity
    integer, save:: MaxIterations
    integer, save:: LuInc

    real*8, save:: QuadPt(MaxQuadPts)
    real*8, save:: QuadWt(MaxQuadPts)

    !c-----defaults

    data &
        variabledensity /.false./ &
        ,variablesinuosity /.false./ &
        ,theta /0.6D0/ &
        ,maxiterations /50/ &
        ,luinc /1/ &
        ,toleranceq /0.0005/ &
        ,tolerancez /0.0005/
end module netcntrl_common
