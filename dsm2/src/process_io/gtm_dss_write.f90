!<license>
!    Copyright (C) 2017 State of California,
!    Department of Water Resources.
!    This file is part of DSM2-GTM.
!
!    The Delta Simulation Model 2 (DSM2) - General Transport Model (GTM)
!    is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    DSM2 is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
!</license>
!> This module is used to write out a block of data to DSS file
!>@ingroup process_io

module gtm_dss_write

    use gtm_dss
    use constants

    !-----scratch file unit numbers
    integer, parameter :: unit_min15 = 31,  &
                          unit_hour1 = 32,  &
                          unit_day1 = 33,   &
                          unit_week1 = 34,  &
                          unit_month1 = 35, &
                          unit_year1 = 36

    integer, parameter :: max_out_min = 1000,   &  ! maximum output paths for 15minute intervals
                          max_out_hour = 200,   &  ! maximum output paths for hour intervals
                          max_out_day = 1000,   &  ! maximum output paths for day intervals
                          max_out_week = 10,    &  ! maximum output paths for week intervals
                          max_out_month = 200,  &  ! maximum output paths for month intervals
                          max_out_year = 10        ! maximum output paths for year intervals

    !-----output data buffers
    !-----the zero'th location is used to store the value from the
    !-----previous model time step, for interpolation with odd time steps
    real(gtm_real) :: dataout_minutes15(0:mins15,max_out_min)   ! 15 minute data for one day
    real(gtm_real) :: dataout_hours(0:hrs,max_out_hour)         ! hourly data for one month
    real(gtm_real) :: dataout_days(0:dys,max_out_day)           ! daily data for one year
    real(gtm_real) :: dataout_weeks(0:wks,max_out_week)         ! weekly data for one decade
    real(gtm_real) :: dataout_months(0:mths,max_out_month)      ! monthly data for one decade
    real(gtm_real) :: dataout_years(0:yrs,max_out_year)         ! yearly data for one century

    !-----pointer back to location in pathoutput structure
    integer :: ptout_min15(max_out_min)
    integer :: ptout_hour1(max_out_hour)
    integer :: ptout_day1(max_out_day)
    integer :: ptout_week1(max_out_week)
    integer :: ptout_month1(max_out_month)
    integer :: ptout_year1(max_out_year)

    !-----the starting date/time of each block of data, in julian minutes
    integer :: julstout_minutes15, &
               julstout_hours1,    &
               julstout_days1,     &
               julstout_weeks1,    &
               julstout_months1,   &
               julstout_years1

    !-----the number of values that went into an average value over the
    !-----time interval
    integer :: nave_min15(max_out_min)
    integer :: nave_hour1(max_out_hour)
    integer :: nave_day1(max_out_day)
    integer :: nave_week1(max_out_week)
    integer :: nave_month1(max_out_month)
    integer :: nave_year1(max_out_year)


    !-----number of paths in each buffer group
    integer :: npthsout_min15,  &
               npthsout_hour1,  &
               npthsout_day1,   &
               npthsout_week1,  &
               npthsout_month1, &
               npthsout_year1

    !-----whether each buffer group needs to write to a tmp binary file
    logical :: need_tmpfile_min15,  &
               need_tmpfile_hour1,  &
               need_tmpfile_day1,   &
               need_tmpfile_week1,  &
               need_tmpfile_month1, &
               need_tmpfile_year1

    !-----scratch output filenames
    character :: scratch_file_array*80(6)

    contains

    !> write out a block of data to DSS file
    subroutine writedss(pathnumber, cdt, in_values, nvals)
         use common_vars, only: unit_error
         use common_gtm_vars, only: pathoutput, max_dssoutfiles, ifltab_out,  per_type_names
         implicit none
         logical,dimension(max_dssoutfiles) :: isopen = .false.
         character :: cdt*(*), &               ! date/time for start of data &
                      cdatx*9, &               ! date &
                      ctimx*4, &               ! time &
                      ca*32, cb*32, cc*32, &
                      cd*32, ce*32, cf*32, &
                      cunits*8                 ! units (e.g. FEET, CFS)

         integer :: nvals ,istat, iostat,  &
                    pathnumber,            &    ! global pathnumber
                    npath,na,nb,nc,nd,ne,nf

         integer :: i
         real(gtm_real) :: in_values(nvals)
         real*4 :: values(nvals)

         if (nvals .le. 0) return
         do i = 1, nvals
            values(i) = sngl(in_values(i))
         end do

         cdatx = cdt(1:9)
         ctimx = cdt(11:14)

         if (.not. isopen(pathoutput(pathnumber)%ndx_file)) then
         !--------Open the DSS file for writing
         !--------exclusive write lock, works faster over NFS
         !@@@         call zset('WLOCK','ON',0)
         !--------set module name as data creator
             call zset('PROGRAM','GTM',0)
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
         cunits = pathoutput(pathnumber)%units
         call upcase(cunits)
         call zsrts(ifltab_out(1,pathoutput(pathnumber)%ndx_file), &
                    pathoutput(pathnumber)%path,                   &
                    cdatx, ctimx, nvals, values, cunits,           &
                    per_type_names(pathoutput(pathnumber)%per_type), 0, istat)

         return
    end subroutine

end module