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

!-----Define arrays to be used as buffers for data from DSS.
!-----Use one array for each kind of data interval for DSS.
module mod_writedss
    use dss
!-----scratch file unit numbers
      integer, parameter::&
          unit_min15=31, &
          unit_hour1=32, &
          unit_day1=33, &
          unit_week1=34, &
          unit_month1=35, &
          unit_year1=36

!-----output data buffers
!-----the zero'th location is used to store the value from the
!-----previous model time step, for interpolation with odd time steps
      real*8 &
          dataout_minutes15(0:mins15,max_out_min), & ! 15 minute data for one day &
          dataout_hours(0:hrs,max_out_hour), & ! hourly data for one month &
          dataout_days(0:dys,max_out_day), & ! daily data for one year &
          dataout_weeks(0:wks,max_out_week), & ! weekly data for one decade &
          dataout_months(0:mths,max_out_month), & ! monthly data for one decade &
          dataout_years(0:yrs,max_out_year)  ! yearly data for one century

!-----pointer back to location in pathoutput structure
      integer*4 &
          ptout_min15(max_out_min), &
          ptout_hour1(max_out_hour), &
          ptout_day1(max_out_day), &
          ptout_week1(max_out_week), &
          ptout_month1(max_out_month), &
          ptout_year1(max_out_year)

!-----the starting date/time of each block of data, in julian minutes
      integer*4 &
          julstout_minutes15, &
          julstout_hours1, &
          julstout_days1, &
          julstout_weeks1, &
          julstout_months1, &
          julstout_years1

!-----the number of values that went into an average value over the
!-----time interval
      integer &
          nave_min15(max_out_min), &
          nave_hour1(max_out_hour), &
          nave_day1(max_out_day), &
          nave_week1(max_out_week), &
          nave_month1(max_out_month), &
          nave_year1(max_out_year)

!-----number of paths in each buffer group
      integer &
          npthsout_min15, &
          npthsout_hour1, &
          npthsout_day1, &
          npthsout_week1, &
          npthsout_month1, &
          npthsout_year1

!-----whether each buffer group needs to write to a tmp binary file
      logical &
          need_tmpfile_min15, &
          need_tmpfile_hour1, &
          need_tmpfile_day1, &
          need_tmpfile_week1, &
          need_tmpfile_month1, &
          need_tmpfile_year1

!-----scratch output filenames
      character &
          scratch_file_array*80(6)

contains

      subroutine writedss(pathnumber, cdt, in_values, nvals)

!-----Write out a block of data to DSS
      use io_units
      use runtime_data
      use iopath_data
      implicit none


      logical,dimension(max_dssoutfiles) :: isopen = .false.
      character &
          cdt*(*), &              ! date/time for start of data &
          cdatx*9, &            ! date &
          ctimx*4, &            ! time &
          ca*32, cb*32, cc*32, cd*32, ce*32, cf*32, &
          cunits*8            ! units (e.g. FEET, CFS)

      integer &
          nvals ,istat, iostat, &
          pathnumber, &         ! global pathnumber &
          npath,na,nb,nc,nd,ne,nf

      integer i
      real*8 in_values(nvals)
      real*4 values(nvals)


      if (nvals .le. 0) return
      do i =1,nvals
         values(i) = sngl(in_values(i))
      end do

      cdatx=cdt(1:9)
      ctimx=cdt(11:14)

      if (.not. isopen(pathoutput(pathnumber)%ndx_file)) then
!--------Open the DSS file for writing
!--------exclusive write lock, works faster over NFS
!@@@         call zset('WLOCK','ON',0)
!--------set module name as data creator
         call zset('PROGRAM',dsm2_name,0)
!--------preset for a very large .dss file
         call zset('SIZE', ' ', 1000000)

         call zopen (ifltab_out(1,pathoutput(pathnumber)%ndx_file), &
             pathoutput(pathnumber)%filename, iostat)
         if (iostat .gt. 0) then
            write(unit_error,'(a,a)') 'Unable to open the file ', &
                pathoutput(pathnumber)%filename
            call exit(2)
         endif
         isopen(pathoutput(pathnumber)%ndx_file)=.true.
      endif

!-----write the time block
      cunits=pathoutput(pathnumber)%units
      call upcase(cunits)
      call zsrts(ifltab_out(1,pathoutput(pathnumber)%ndx_file), &
          pathoutput(pathnumber)%path, &
          cdatx, ctimx, nvals, values, cunits, &
          per_type_names(pathoutput(pathnumber)%per_type), 0, istat)

      return
      end subroutine

end module
