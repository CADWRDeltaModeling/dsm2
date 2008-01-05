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

c-----Define arrays to be used as buffers for data from DSS.
c-----Use one array for each kind of data interval for DSS.

c-----scratch file unit numbers
      integer
     &     unit_min15
     &     ,unit_hour1
     &     ,unit_day1
     &     ,unit_week1
     &     ,unit_month1
     &     ,unit_year1

      parameter (
     &     unit_min15=31
     &     ,unit_hour1=32
     &     ,unit_day1=33
     &     ,unit_week1=34
     &     ,unit_month1=35
     &     ,unit_year1=36
     &     )

c-----output data buffers
c-----the zero'th location is used to store the value from the
c-----previous model time step, for interpolation with odd time steps
      real*8
     &     dataout_minutes15(0:mins15,max_out_min) ! 15 minute data for one day
     &     ,dataout_hours(0:hrs,max_out_hour) ! hourly data for one month
     &     ,dataout_days(0:dys,max_out_day) ! daily data for one year
     &     ,dataout_weeks(0:wks,max_out_week) ! weekly data for one decade
     &     ,dataout_months(0:mths,max_out_month) ! monthly data for one decade
     &     ,dataout_years(0:yrs,max_out_year) ! yearly data for one century

      common /dssoutbuffers_r/  dataout_minutes15, dataout_hours,
     &     dataout_days, dataout_weeks, dataout_months, dataout_years

c-----pointer back to location in pathoutput structure
      integer*4
     &     ptout_min15(max_out_min)
     &     ,ptout_hour1(max_out_hour)
     &     ,ptout_day1(max_out_day)
     &     ,ptout_week1(max_out_week)
     &     ,ptout_month1(max_out_month)
     &     ,ptout_year1(max_out_year)

c-----the starting date/time of each block of data, in julian minutes
      integer*4
     &     julstout_minutes15
     &     ,julstout_hours1
     &     ,julstout_days1
     &     ,julstout_weeks1
     &     ,julstout_months1
     &     ,julstout_years1

c-----the number of values that went into an average value over the
c-----time interval
      integer
     &     nave_min15(max_out_min)
     &     ,nave_hour1(max_out_hour)
     &     ,nave_day1(max_out_day)
     &     ,nave_week1(max_out_week)
     &     ,nave_month1(max_out_month)
     &     ,nave_year1(max_out_year)

c-----number of paths in each buffer group
      integer
     &     npthsout_min15
     &     ,npthsout_hour1
     &     ,npthsout_day1
     &     ,npthsout_week1
     &     ,npthsout_month1
     &     ,npthsout_year1

c-----whether each buffer group needs to write to a tmp binary file
      logical
     &     need_tmpfile_min15
     &     ,need_tmpfile_hour1
     &     ,need_tmpfile_day1
     &     ,need_tmpfile_week1
     &     ,need_tmpfile_month1
     &     ,need_tmpfile_year1

c-----scratch output filenames
      character
     &     scratch_file_array*80(6)

      common /dssoutbuffers_i/
     &     ptout_min15
     &     ,ptout_hour1
     &     ,ptout_day1
     &     ,ptout_week1
     &     ,ptout_month1
     &     ,ptout_year1
     &     ,julstout_minutes15
     &     ,julstout_hours1
     &     ,julstout_days1
     &     ,julstout_weeks1
     &     ,julstout_months1
     &     ,julstout_years1
     &     ,nave_min15
     &     ,nave_hour1
     &     ,nave_day1
     &     ,nave_week1
     &     ,nave_month1
     &     ,nave_year1
     &     ,npthsout_min15
     &     ,npthsout_hour1
     &     ,npthsout_day1
     &     ,npthsout_week1
     &     ,npthsout_month1
     &     ,npthsout_year1
     &     ,need_tmpfile_min15
     &     ,need_tmpfile_hour1
     &     ,need_tmpfile_day1
     &     ,need_tmpfile_week1
     &     ,need_tmpfile_month1
     &     ,need_tmpfile_year1

      common /dssoutbuffers_c/
     &     scratch_file_array
