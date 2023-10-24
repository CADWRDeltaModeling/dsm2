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

c-----structure definitions for DSM2-PTM

      structure /part_injection_s/
         integer      node      ! Location where particles are injected
         integer      nparts    ! Number of particles injected at location
         character*15 type      ! particle type (e.g. 'striped bass', 'salmon')
         character*15 slength   ! Length of time from the start of the simulation when parts are injected e.g. 15min, 1day
         character*15 length    ! Length of injection e.g. 15min, 1day
         character*14 start_date  ! start date and time
         character*14 end_date    ! end date and time
         integer*4    start_julmin ! start julian minute
         integer*4    end_julmin ! end julian minute
         integer*4    length_julmin ! injection length in minutes
      end structure


