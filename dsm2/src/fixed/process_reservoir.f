C!<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    DSM2 is free software: you can redistribute it and/or modify
C!    it under the terms of the GNU General Public !<license as published by
C!    the Free Software Foundation, either version 3 of the !<license, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public !<license for more details.

C!    You should have received a copy of the GNU General Public !<license
C!    along with DSM2.  If not, see <http://www.gnu.org/!<licenses/>.
C!</license>

      subroutine process_reservoir(id,reser_name,reser_area,reser_botelv)
      use grid_data
      use logging
      use io_units
      implicit none
      integer id   
      real*4               !todo: this should be real*8
     &     reser_area
     &     ,reser_botelv

      character
     &     reser_name*32 
      
      nreser=nreser+1
      if (nreser .gt. max_reservoirs) then
          write(unit_error,630)
     &         'Reservoir number too high; max allowed is:',
     &         max_reservoirs
          call exit(-1)
          return
      endif
      res_geom(nreser).id=ID
      res_geom(nreser).inUse=.true.
      res_geom(nreser).name=trim(reser_name)
      res_geom(nreser).area=reser_area
      res_geom(nreser).botelv=reser_botelv
      if (print_level .ge. 3)
     &    write(unit_screen,'(i5,1x,a)')
     &         nreser,trim(res_geom(nreser).name)   
 630   format(/a,i5)
      return
      end subroutine