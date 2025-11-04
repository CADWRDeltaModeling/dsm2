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

module qual_param
      implicit none

      integer, parameter :: NOBR=800, NOSC=15, NOPR=82
      integer, parameter :: MAXNUMRES=100, MAXRESJUNC=50
      integer, parameter :: ihout=50, irhout=51
      integer, parameter :: idoout=52, irdoout=53
      integer, parameter :: ialgout=54, iralgout=55
      real*8, parameter :: DX0=500.D0
end module qual_param