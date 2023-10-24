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


module PhysicalConstants
    real*8,save :: gravity  !Acceleration due to gravity, in user specified units
    real*8,save :: sqrt2g
    real*8,parameter :: pi=3.14159265358979

contains

    logical function verify_gravity_terms()
        use io_units
        implicit none
        verify_gravity_terms = .true.
        if(abs(gravity - 32.2) > 0.1 .and. abs(gravity - 9.8) > 0.1 ) then
            write(unit_error, "('a,i,a,a')") "Gravity value of ", gravity, " not expected.", &
                " Either change units or alter model to accomodate new units."
            verify_gravity_terms= .false.
            return
        end if
        sqrt2g=sqrt(2.D0*gravity)
        return
    end function

end module PhysicalConstants






