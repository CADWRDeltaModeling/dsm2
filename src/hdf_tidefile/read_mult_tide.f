C!    Copyright (C) 1996, 1997, 1998 State of California,
C!    Department of Water Resources.

c!    Delta Simulation Model 2 (DSM2): A River, Estuary, and Land
c!    numerical model.  No protection claimed in original FOURPT and
c!    Branched Lagrangian Transport Model (BLTM) code written by the
c!    United States Geological Survey.  Protection claimed in the
c!    routines and files listed in the accompanying file "Protect.txt".
c!    If you did not receive a copy of this file contact Dr. Paul
c!    Hutton, below.
c!
c!    This program is licensed to you under the terms of the GNU General
c!    Public License, version 2, as published by the Free Software
c!    Foundation.
c!
c!    You should have received a copy of the GNU General Public License
c!    along with this program; if not, contact Dr. Paul Hutton, below,
c!    or the Free Software Foundation, 675 Mass Ave, Cambridge, MA
c!    02139, USA.
c!
c!    THIS SOFTWARE AND DOCUMENTATION ARE PROVIDED BY THE CALIFORNIA
c!    DEPARTMENT OF WATER RESOURCES AND CONTRIBUTORS "AS IS" AND ANY
c!    EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
c!    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
c!    PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE CALIFORNIA
c!    DEPARTMENT OF WATER RESOURCES OR ITS CONTRIBUTORS BE LIABLE FOR
c!    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
c!    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
c!    OR SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS; OR
c!    BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
c!    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
c!    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
c!    USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
c!    DAMAGE.
c!
c!    For more information about DSM2, contact:
c!
c!    Dr. Paul Hutton
c!    California Dept. of Water Resources
c!    Division of Planning, Delta Modeling Section
c!    1416 Ninth Street
c!    Sacramento, CA  95814
c!    916-653-5601
c!    hutton@water.ca.gov
c!
c!    or see our home page: http://wwwdelmod.water.ca.gov/

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


      



