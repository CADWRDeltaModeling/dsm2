!<license>
!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
!    Department of Water Resources.
!    This file is part of DSM2.

!    The Delta Simulation Model 2 (DSM2) is free software:
!    you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.

!    DSM2 is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.

!    You should have received a copy of the GNU General Public License
!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
!</license>

!-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine get_model_date(jmin, date)
      implicit none
      integer*4 jmin
      character*(*) date
      character*9 ndate
      character*4 ntime
      call convert2stringdates(jmin,ndate,ntime)
      date = ndate(1:9) // char(0)
      return
      end
!-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine get_model_time(jmin, time)
      implicit none
      integer*4 jmin
      character*(*) time
      character*9 ndate
      character*4 ntime
      call convert2stringdates(jmin,ndate,ntime)
      time = ntime(1:4) // char(0)
      return
      end
