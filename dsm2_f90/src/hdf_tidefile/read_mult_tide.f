C!<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    DSM2 is free software: you can redistribute it and/or modify
C!    it under the terms of the GNU General Public !<license as published by
C!    the Free Software Foundation, either version 3 of the !<license, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public !<license for more details.

C!    You should have received a copy of the GNU General Public !<license
C!    along with DSM2.  If not, see <http://www.gnu.org/!<licenses/>.
C!</license>

      subroutine read_mult_tide

c-----Read multiple Hydro tidefiles.  Each tidefile can have multiple
c-----tides.  A tide is flow data averaged between the last and
c-----this timestamp for each tide.
c-----Determine if data is available in current or new tidefile;
c-----read tidefile if necessary and store data.
      use io_units
      use logging
      use common_tide
      use runtime_data
      use iopath_data
      implicit none

c-----local variables
      character
     &     filenm*150           ! current tidefile name
     &     ,jmin2cdt*14         ! julian minute to char function

      integer
     &     i                    ! loop indices
     &     ,first_used_tidefile ! first tidefile number that is being used
     &     ,prev_tidefile
     &     ,prev_read_tidetime

      logical
     &     new_tidefile         ! true if new tidefile
     &     ,foundtime

      integer, external :: GetCurrentTideTime
      integer, external :: SetHDF5ToTime

      external jmin2cdt

      save first_used_tidefile,prev_tidefile
      data first_used_tidefile /miss_val_i/,
     &     prev_tidefile /miss_val_i/


      foundtime = .false.
      do i=max(current_tidefile,1),nintides
         if (julmin .ge. tide_files(i).start_julmin .and.
     &        julmin .le. tide_files(i).end_julmin) then
            new_tidefile=current_tidefile .ne. i
	      if (new_tidefile .and. current_tidefile .ne. miss_val_i)
     &         call CloseHDF5()
	      prev_tidefile=current_tidefile
            current_tidefile=i
            if (first_used_tidefile .eq. miss_val_i) then
		     first_used_tidefile=i
            end if
            foundtime = .true.
	      exit
         endif
      enddo

      if (.not. foundtime)then
610     format(/'Unable to find a tidefile for current time: ',a)
        write(unit_error,610) current_date
        call exit(2)
      end if

      filenm=tide_files(current_tidefile).filename
      if (new_tidefile) then
	   !@todo: why do we read_tide_head? We want some specific dated info, 
	   !       but mostly this is just dangerous. 
	   !       It would be better to have a smaller read of any
	   !       info that actually changes between tidefiles.
         call read_tide_head(filenm, .true.)
         if (print_level.ge.1) then
            write(unit_screen,922) trim(filenm),  current_date
            write(unit_output,922) trim(filenm), current_date
 922        format(/'Opened a new tidefile: ',/a
     &           /' model time: ',a)
         endif
         ! When a transition occurs at t0, the old tidefile is read for the
	   ! time step that reads t0. Here, we are about to read/calculate
	   ! t1. Need to reload the data from t0 as if it came from the new
	   ! tidefile, and then reconcile any differences in ProcessTide.
         call ReadDataFromHDF5(tide_files(current_tidefile).start_julmin)
	   call process_tide(new_tidefile,first_used_tidefile,current_tidefile)
         prev_read_tidetime = getCurrentTideTime() ! forces a second read based on julmin
      endif

      new_tidefile = .false.
c-----read tide flows
	if (julmin .gt. prev_read_tidetime )then
	  ! flow values must be updated, rather than reused
	   call ReadDataFromHDF5(julmin)
	   prev_read_tidetime = getCurrentTideTime()
         call process_tide(new_tidefile,first_used_tidefile,current_tidefile)
      end if


c	     call ReportOpenData()
      
	return
      end


      



