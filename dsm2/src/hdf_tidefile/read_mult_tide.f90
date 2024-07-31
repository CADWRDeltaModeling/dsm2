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

subroutine read_mult_tide

!-----Read multiple Hydro tidefiles.  Each tidefile can have multiple
!-----tides.  A tide is flow data averaged between the last and
!-----this timestamp for each tide.
!-----Determine if data is available in current or new tidefile;
!-----read tidefile if necessary and store data.
      use io_units
      use logging
      use common_tide
      use runtime_data
      use iopath_data
      implicit none

!-----local variables
      character &
          filenm*150, &           ! current tidefile name
          jmin2cdt*14         ! julian minute to char function

      integer &
          i                    ! loop indices


      integer, save :: prev_read_tidetime = miss_val_i
      integer, save :: first_used_tidefile = miss_val_i
      integer, save :: prev_tidefile = miss_val_i

      logical &
          new_tidefile, &         ! true if new tidefile
          foundtime


      integer, external :: GetCurrentTideTime
      integer, external :: SetHDF5ToTime

      external jmin2cdt

      foundtime = .false.
      do i=max(current_tidefile,1),nintides
         if (julmin .ge. tide_files(i).start_julmin .and. &
             julmin .le. tide_files(i).end_julmin) then
              new_tidefile=current_tidefile .ne. i
	      if (new_tidefile .and. current_tidefile .ne. miss_val_i) &
              call CloseHDF5()
              prev_tidefile=current_tidefile
              current_tidefile=i
              if (first_used_tidefile .eq. miss_val_i) then
                  first_used_tidefile=i
              end if
              foundtime = .true.
	      exit
         endif
      enddo
      if (.not. foundtime) then
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
 922        format(/'Opened a new tidefile: ',/a &
                /' model time: ',a)
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
!-----read tide flows
	if (julmin .gt. prev_read_tidetime ) then
	  ! flow values must be updated, rather than reused
	   call ReadDataFromHDF5(julmin)
	   prev_read_tidetime = getCurrentTideTime()
       call process_tide(new_tidefile,first_used_tidefile,current_tidefile)
    end if


	return
end subroutine






