!<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    DSM2 is free software: you can redistribute it and/or modify
C!    it under the terms of the GNU General Public License as published by
C!    the Free Software Foundation, either version 3 of the License, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public License for more details.

C!    You should have received a copy of the GNU General Public License
C!    along with DSM2.  If not, see <http://www.gnu.org/licenses/>.
!</license>


      Module PhysicalConstants
	real*8,save :: gravity  !Acceleration due to gravity, in user specified units
	real*8,save :: sqrt2g
	real*8,parameter :: pi=3.14159265358979

      contains

	logical function verify_gravity_terms()
	use io_units
	implicit none
	verify_gravity_terms = .true.
	if(abs(gravity - 32.2) .gt. 0.1 .and. abs(gravity - 9.8) .gt. 0.1 )then
	  write(unit_error, "('a,i,a,a')") "Gravity value of ", gravity, " not expected.",
     &   " Either change units or alter model to accomodate new units."
        verify_gravity_terms= .false.
	  return
	end if
      sqrt2g=sqrt(2.D0*gravity)
      return
	end function

	End Module PhysicalConstants






