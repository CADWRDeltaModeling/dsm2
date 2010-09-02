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

      module runtime_data
      use type_defs
      include '../common/version.fi'  ! version of hydro, qual, and ptm      
      
c-----dates, timestep
c-----Note: julian minutes are minutes from 01jan1900 0000 (31dec1899 2400)
      integer
     &     max_print_dates      ! maximum number of start/stop output date/times            
     &     ,time_step           ! time step in minutes
     &     ,prev_time_step      ! previous time step in minutes
      integer :: nprints = 1    ! number of start/stop output date/times


      integer*4
     &     julmin               ! current model time in julian minutes
     &     ,prev_julmin         ! previous model time in julian minutes
     &     ,start_julmin        ! model start time, julian minutes
     &     ,end_julmin          ! model end time, julian minutes
     &     ,jul_generic_date    ! julian minute of generic_dt
     &     ,tf_start_julmin     ! (hydro) when to start writing tidefile, jul mins

      parameter (
     &     max_print_dates=10
     &     )
      character*14 :: current_date = ' '   ! current date/time (corresponds to julmin)
      character*14 :: run_start_date = ' ' ! date/time of start of run
      character*14 :: run_end_date = ' '   ! date/time of end of run
      character*14 :: tf_start_date = ' '  ! (hydro) date/time of when to start writing tidefile
      character*14 :: print_start_date(max_print_dates) = ' '! date/time of print starts
      character*14 :: print_end_date(max_print_dates) = ' ' ! date/time of print ends

c-----alternate method: instead of start/end dates, specify run length
c-----in form, e.g. 5day_3hour.  Model will generate end date/times.
      character*80 :: run_length = ' '

c-----time step
      character*80 :: time_step_intvl_hydro = ' '
      character*80 :: time_step_intvl_qual = ' '
      character*80 :: time_step_intvl_ptm = ' '

c-----flush output interval
      character*80 :: flush_intvl = '5DAY'

c-----display time interval
      character*80 :: display_intvl = ' '
      
      
c-----Program name and version number, set in the main routine
c-----of Hydro, Qual and PTM

      character*48 :: model_name
      character*5  :: dsm2_name
      character*24
     &     ,restart_version     ! the version of the program that produced the restart file
     &     ,tidefile_version    ! the version of the program that produced the tidefile
      


c-----runtime identification and run date/time, set in read_fixed
      integer*4
     &     dsm2_module          ! module ID
     &     ,irid                ! run ID as integer
     &     ,irdt                ! run date/time as integer: YYMMDDhhmm
      character
     &     crid*13              ! run ID as character
     &     ,crdt14*14           ! run date/time as character: DDMMMYYYY hhmm
     &     ,crdt10*10           ! run date/time as character: YYMMDDhhmm
      
c-----input sections structure
      type(form_t) hdr_form(max_sections)

c-----titles
      integer
     &     max_titles           ! maximum number of titles allowed
     &     ,ntitles             ! actual number of titles

      parameter (
     &     max_titles=30
     &     )

      character*80
     &     title(max_titles)


      contains
      
      
      
      
      subroutine initialize_runtimes
      use io_units
      use constants
      implicit none
      integer cdt2jmin
      
      character
     &     diff2dates*14       ! return DSS date given start and diff
     &     ,jmin2cdt*14         ! julian minute to char function
c-----correct start date for odd minutes (not multiple of 15 minutes)
      start_julmin=cdt2jmin(run_start_date)
	if( start_julmin .ne. (start_julmin/15)*15) then
         write(unit_error,*)"Start time must be aligned with " //
     &     "15MIN interval(0000,0015...)"
	end if
 605  format(/a,' date incorrect: ',a)
c-----calculate ending time if run length, rather than
c-----start/end times are given
      if (run_length .ne. ' ') then
c--------run length should be in form: '20hour' or '5day'
         run_end_date=diff2dates(run_start_date,run_length)
      endif                     ! start/end char dates given
      end_julmin=cdt2jmin(run_end_date)

      if (len_trim(run_start_date) .eq. 0)then
         write(unit_error,*)'Start date missing'
         call exit(-3)
      endif
      if (len_trim(run_end_date) .eq. 0)then
         write(unit_error,*)'End date missing'
         call exit(-3)
      endif

c-----check validity of start and end julian minutes
      if (start_julmin .ge. end_julmin) then
         write(unit_error,"('Starting date: ',a9,
     &        ' equal to or after ending date: ',a9,'or one/both may be missing')")
     &        run_start_date,run_end_date
         call exit(-3)
      endif
      if (start_julmin .eq. miss_val_i) then
         write(unit_error,605) 'Starting',run_start_date
         call exit(-3)
      endif
      if (end_julmin .eq. miss_val_i) then
         write(unit_error,605) 'Ending',run_end_date
         call exit(-3)
      endif

c-----Tidefile date to when to start writing tidefile (hydro)
      if (dsm2_module .eq. hydro) then
         if (tf_start_date .eq. ' ') then
            tf_start_julmin=start_julmin
         else
c-----------correct tf start date for odd minutes (not multiple of tidefile interval)
            tf_start_julmin=cdt2jmin(tf_start_date)
            tf_start_julmin=(tf_start_julmin/15)*15
            tf_start_julmin=max(start_julmin,tf_start_julmin) ! correct for too-soon tf start
         endif
      endif
      tf_start_date = jmin2cdt(start_julmin)
      
      end subroutine 
      
      end module

