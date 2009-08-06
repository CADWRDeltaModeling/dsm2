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

c-----Define arrays to be used as buffers for data values, data
c-----quality flags from DSS, and julian minute timestamp.
c-----Use one array for each kind of data interval for DSS.

      type(dataqual_t)
     &     datain_15min(mins15,max_inp_min) ! 15 minute data
     &     ,datain_1hour(hrs,max_inp_hour) ! hourly data
     &     ,datain_1day(dys,max_inp_day) ! daily data
     &     ,datain_1week(wks,max_inp_week) ! weekly data
     &     ,datain_1month(mths,max_inp_month) ! monthly data
     &     ,datain_1year(yrs,max_inp_year) ! yearly data
     &     ,datain_irr(irrs,max_inp_irr) ! irregular data block

      common /dssinbuffers_s/
     &     datain_15min
     &     ,datain_1hour
     &     ,datain_1day
     &     ,datain_1week
     &     ,datain_1month
     &     ,datain_1year
     &     ,datain_irr

c-----pointer back to location in global pathinput structure
      integer
     &     ptin_min15(max_inp_min)
     &     ,ptin_hour1(max_inp_hour)
     &     ,ptin_day1(max_inp_day)
     &     ,ptin_week1(max_inp_week)
     &     ,ptin_month1(max_inp_month)
     &     ,ptin_year1(max_inp_year)
     &     ,ptin_irr(max_inp_irr)

c-----number of paths in each buffer group
      integer
     &     npthsin_min15
     &     ,npthsin_hour1
     &     ,npthsin_day1
     &     ,npthsin_week1
     &     ,npthsin_month1
     &     ,npthsin_year1
     &     ,npthsin_irr

      common /dssinbuffers_i/
     &     ptin_min15
     &     ,ptin_hour1
     &     ,ptin_day1
     &     ,ptin_week1
     &     ,ptin_month1
     &     ,ptin_year1
     &     ,ptin_irr
     &     ,npthsin_min15
     &     ,npthsin_hour1
     &     ,npthsin_day1
     &     ,npthsin_week1
     &     ,npthsin_month1
     &     ,npthsin_year1
     &     ,npthsin_irr
