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
module dss
      integer &
          max_inp_min &
          ,max_inp_hour &
          ,max_inp_day &
          ,max_inp_week &
          ,max_inp_month &
          ,max_inp_year &
          ,max_inp_irr &
          ,max_out_min &
          ,max_out_hour &
          ,max_out_day &
          ,max_out_week &
          ,max_out_month &
          ,max_out_year &
          ,mins15 &
          ,hrs &
          ,dys &
          ,wks &
          ,mths &
          ,yrs &
          ,irrs &
          ,maxinpsize

      parameter ( &
          max_inp_min = 100  &   ! maximum input paths for 15minute intervals &
          ,max_inp_hour = 100 &  ! maximum input paths for hour intervals &
          ,max_inp_day = 3000 &   ! maximum input paths for day intervals &
          ,max_inp_week = 5 &   ! maximum input paths for week intervals &
          ,max_inp_month = 3000 &! maximum input paths for month intervals &
          ,max_inp_year = 3200 &! maximum input paths for year intervals !@todo was 10, changed because of constant &
          ,max_inp_irr = 250 &  ! maximum input paths for irregular intervals &
          ,max_out_min = 1000 &  ! maximum output paths for 15minute intervals &
          ,max_out_hour = 1000 & ! maximum output paths for hour intervals &
          ,max_out_day = 1000 &  ! maximum output paths for day intervals &
          ,max_out_week = 10 &  ! maximum output paths for week intervals &
          ,max_out_month = 200 &! maximum output paths for month intervals &
          ,max_out_year = 10 &  ! maximum output paths for year intervals
!-----each of the following should be 2 or greater &
          ,mins15 = 4*24*30 &   ! number of values in a 15MIN interval (30 days worth) &
          ,hrs = 24*30 &        ! 30 days of hourly values &
          ,dys = 35 &           ! NOTE: if you change these so that mins15 is no longer &
          ,wks = 5 &            !   the longest length block, you must resize outdata_arr &
          ,mths = 13 &          !   in wrt_outpaths.f &
          ,yrs = 3 &
          ,irrs = 10 &
          ,maxinpsize = max(mins15,hrs,dys,wks,mths,yrs,irrs) &
          )

end module
