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

      subroutine buffer_input_tidefile()
      use input_storage_fortran
      use constants
      use io_units
      use runtime_data
      use common_tide
      use utilities
      implicit none
      integer :: nitem
      integer :: icount
      character*128 iofile
      integer :: ierror = 0

      character*(16) :: sdate,edate
      integer :: i
      integer :: n_tidefiles_used

      nitem = tidefile_buffer_size()
      if (nitem .eq. 0)then
          write(unit_error,*)"No input tidefiles listed."
          call exit(-3)
      end if
      do icount = 1,nitem
         call tidefile_query_from_buffer(icount,sdate,edate,iofile,ierror)
         call process_tidefile(sdate,edate,iofile)
      end do
      print *,"Number of tidefiles: ", nitem

      ! Check the numbering and order of tidefiles
      n_tidefiles_used = 0
      if (nintides .le. 0) then
         write(unit_error, '(a)') 'No input tides given, run stopped.'
         call exit(-3)
      endif
      do i=1,nintides
	   n_tidefiles_used = n_tidefiles_used + 1
	   ! This exit statement allows nonexistent tidefiles to be listed
	   if (tide_files(i).end_julmin .ge. end_julmin) exit
      enddo
      nintides = n_tidefiles_used
	if (nintides .gt. 1) then
        do i=2,nintides
           if (tide_files(i).start_julmin .ne. tide_files(i-1).end_julmin) then
	        write(unit_error,*) "Tidefile dates must be ordered in time, "
     &              // "with no gaps or overlap in start/end dates"
	        call exit(-3)
	     end if
	  end do
	end if

c-----make sure run dates are spanned
      if (dsm2_module .eq. qual .or. dsm2_module .eq. ptm) then
	  if (  tide_files(1).start_julmin .gt. start_julmin
     &   .or. tide_files(nintides).end_julmin .lt. end_julmin) then
	    write(unit_error,*)"Error...dates for tidefiles do not cover period of simulation:"
	    Write(unit_error,921) run_start_date,run_end_date
 921      format(' Model run:',
     &        '    start date: ',a14,
     &        '    end date: ',a14)
          Write(unit_error,931) jmin2cdt(tide_files(1).start_julmin),
     &       jmin2cdt(tide_files(nintides).end_julmin)
 931      format(' Tidefiles:',
     &        '    start date: ',a14,
     &        '    end date: ',a14)
	    call exit(-3)
	  end if
	end if
	! todo: eli
	tide_files(1).start_julmin = start_julmin
      return
      end subroutine