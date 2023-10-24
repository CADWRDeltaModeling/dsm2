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




subroutine assign_output_units(units, param_name)
      implicit none
      character*(*) units, param_name
      character*(16) param
      param=trim(param_name)
      call locase(param)
      if (index(Param, 'flow') .gt. 0 .or. &
          index(Param, 'pump') .gt. 0) then
          units='cfs'
      else if (Param(1:3) .eq. 'vel' ) then
          units='ft/s'
      else if (Param .eq. 'stage') then
          units='feet'
      else if (Param .eq. 'elev') then
          units='feet'
      else if (Param .eq. 'height') then
          units='feet'
      else if (Param .eq. 'width') then
          units='feet'
      else if (Param .eq. 'tds') then
          units='ppm'
      else if (index(Param, 'weir-pos') .gt. 0) then
          units=' '
      else if (index(Param, 'pipe-pos') .gt. 0) then
          units=' '
      else if (Param .eq. 'ec') then
          units='umhos/cm'
      else if (Param .eq. 'do') then
          units='mg/l'
      else if (Param .eq. 'nh3') then
          units='mg/l'
      else if (Param .eq. 'organic_n') then
          units='mg/l'
      else if (Param .eq. 'no2') then
          units='mg/l'
      else if (Param .eq. 'no3') then
          units='mg/l'
      else if (Param .eq. 'bod') then
          units='mg/l'
      else if (Param .eq. 'organic_p') then
          units='mg/l'
      else if (Param .eq. 'po4') then
          units='mg/l'
      else if (Param .eq. 'algae') then
          units='mg/l'
      else if (Param .eq. 'temp') then
          units='deg c'
      else
          units=' '
      endif
end subroutine


