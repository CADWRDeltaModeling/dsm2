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

      subroutine update_intervals

c-----Update the julian minute values representing the end of each
c-----DSS interval.
      use runtime_data
      use constants
      implicit none

      include 'intervals.inc'
      integer*4
     &     incr_intvl          ! increment julian minute by interval function

      jmin_15min=incr_intvl(julmin,'15MIN',NEAREST_BOUNDARY)
      jmin_15min_prev=incr_intvl(julmin,'-15MIN',NEAREST_BOUNDARY)

      jmin_1hour=incr_intvl(julmin,'1HOUR',NEAREST_BOUNDARY)
      jmin_1hour_prev=incr_intvl(julmin,'-1HOUR',NEAREST_BOUNDARY)

      jmin_1day=incr_intvl(julmin,'1DAY',NEAREST_BOUNDARY)
      jmin_1day_prev=incr_intvl(julmin,'-1DAY',NEAREST_BOUNDARY)

      jmin_1week=incr_intvl(julmin,'1WEEK',NEAREST_BOUNDARY)
      jmin_1week_prev=incr_intvl(julmin,'-1WEEK',NEAREST_BOUNDARY)

      jmin_1month=incr_intvl(julmin,'1MON',NEAREST_BOUNDARY)
      jmin_1month_prev=incr_intvl(julmin,'-1MON',NEAREST_BOUNDARY)

      jmin_1year=incr_intvl(julmin,'1YEAR',NEAREST_BOUNDARY)
      jmin_1year_prev=incr_intvl(julmin,'-1YEAR',NEAREST_BOUNDARY)

      return
      end
