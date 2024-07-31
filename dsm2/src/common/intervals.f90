!!<license>
!!    Copyright (C) 1996, 1997, 1998, 2001, 2007 State of California,
!!    Department of Water Resources.
!!    This file is part of DSM2.

!!    DSM2 is free software: you can redistribute it and/or modify
!!    it under the terms of the GNU General Public !<license as published by
!!    the Free Software Foundation, either version 3 of the !<license, or
!!    (at your option) any later version.

!!    DSM2 is distributed in the hope that it will be useful,
!!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!!    GNU General Public !<license for more details.

!!    You should have received a copy of the GNU General Public !<license
!!    along with DSM2.  If not, see <http://www.gnu.org/!<licenses/>.
!!</license>
module intervals
!-----DSS interval information
      integer*4 &
          jmin_15min, &          ! julian minute of end-of-period for 15MIN data &
          jmin_1hour, &
          jmin_1day, &
          jmin_1week, &
          jmin_1month, &
          jmin_1year, &
          jmin_15min_prev, &    ! previous value of jmin_15min &
          jmin_1hour_prev, &
          jmin_1day_prev, &
          jmin_1week_prev, &
          jmin_1month_prev, &
          jmin_1year_prev
 end module
