<license>
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
</license>

c-----DSS interval information
      integer*4
     &     jmin_15min           ! julian minute of end-of-period for 15MIN data
     &     ,jmin_1hour
     &     ,jmin_1day
     &     ,jmin_1week
     &     ,jmin_1month
     &     ,jmin_1year
     &     ,jmin_15min_prev     ! previous value of jmin_15min
     &     ,jmin_1hour_prev
     &     ,jmin_1day_prev
     &     ,jmin_1week_prev
     &     ,jmin_1month_prev
     &     ,jmin_1year_prev

      common /dss_intvl_i/
     &     jmin_15min           ! julian minute of end-of-period for 15MIN data
     &     ,jmin_1hour
     &     ,jmin_1day
     &     ,jmin_1week
     &     ,jmin_1month
     &     ,jmin_1year
     &     ,jmin_15min_prev     ! previous value of jmin_15min
     &     ,jmin_1hour_prev
     &     ,jmin_1day_prev
     &     ,jmin_1week_prev
     &     ,jmin_1month_prev
     &     ,jmin_1year_prev