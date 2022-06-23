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
module oprule_management
contains
    logical function InitOpRules()
        implicit none
!        character*801 line

        call init_parser_f()
!        open(66,file="c:\delta\studies\historic\oprules.inp")
!        do while (.not. EOF(66))
!        read(66,'(a)')line
!          linelen=len_trim(line)
!          if (linelen .eq. 800) print*, "op rule too long"
!          if (linelen .gt. 0) then
!                  print*,trim(line)
!            call parse_rule(line)
!            end if
!      end do
        InitOpRules = .true.
        return
    end function

end module
