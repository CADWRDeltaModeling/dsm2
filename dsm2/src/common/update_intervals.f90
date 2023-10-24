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

      subroutine update_intervals

!-----Update the julian minute values representing the end of each
!-----DSS interval.
      use runtime_data
      use constants
      use intervals
      use utilities, only: incr_intvl
      implicit none

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
