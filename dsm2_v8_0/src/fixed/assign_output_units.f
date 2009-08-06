C!<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    The Delta Simulation Model 2 (DSM2) is free software: 
C!    you can redistribute it and/or modify
C!    it under the terms of the GNU General Public License as published by
C!    the Free Software Foundation, either version 3 of the License, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public License for more details.

C!    You should have received a copy of the GNU General Public License
C!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
C!</license>




      subroutine assign_output_units(units, param_name)
      implicit none
      character*(*) units, param_name
      character*(16) param
      param=trim(param_name)
      call locase(param)
      if (index(Param, 'flow') .gt. 0 .or.
     &    index(Param, 'pump') .gt. 0) then
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
      else if (Param .eq. 'nh3-n') then
          units='mg/l'
      else if (Param .eq. 'org-n') then
          units='mg/l'
      else if (Param .eq. 'no2-n') then
          units='mg/l'
      else if (Param .eq. 'no3-n') then
          units='mg/l'
      else if (Param .eq. 'bod') then
          units='mg/l'
      else if (Param .eq. 'org-p') then
          units='mg/l'
      else if (Param .eq. 'po4-p') then
          units='mg/l'
      else if (Param .eq. 'algae') then
          units='mg/l'
      else if (Param .eq. 'temp') then
          units='deg c'
      else   
          units=' '
      endif      
      end subroutine


