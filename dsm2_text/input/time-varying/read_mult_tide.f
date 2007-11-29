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

      implicit none

      include '../fixed/common.f'
      include 'common_tide.f'
      include 'tide.inc'

c-----local variables
      character
     &     filenm*150           ! current tidefile name
     &     ,jmin2cdt*14         ! julian minute to char function
     &     ,ctmp*14             ! scratch variable

      integer
     &     i                    ! loop indices
     &     ,tide_block_no       ! tide block number within tidefile
     &     ,first_used_tidefile ! first tidefile number that is being used
     &     ,current_tidefile    ! current tidefile number that is being used
     &     ,lnblnk              ! intrinsic last non-blank function

      integer*4
     &     TideTimeAdj          ! adjustment between tidefile and model time

      logical
     &     new_tidefile         ! true if new tidefile
     &     ,recycle_tidefile    ! true if tidefile should be recycled (rewound)

      external jmin2cdt

      save first_used_tidefile,current_tidefile,tide_block_no,
     &     TideTimeAdj
      data current_tidefile /0/,
     &     first_used_tidefile /0/,
     &     tide_block_no /0/,
     &     TideTimeAdj /0/

      do i=max(current_tidefile,1),nintides
         if (julmin .ge. tide_files(i).start_julmin .and.
     &        julmin .le. tide_files(i).end_julmin) then
            new_tidefile=current_tidefile .ne. i
            recycle_tidefile=.not. new_tidefile .and.
     &           repeating_tidefile .and.
     &           julmin+TideTimeAdj .gt. tide_files(i).end_julmin_file
            current_tidefile=i
            if (first_used_tidefile .eq. 0) first_used_tidefile=i
            goto 15
         endif
      enddo
c-----couldn't find a tidefile with start earlier than current model time,
c-----and end after current model time
 610  format(/'Unable to find a tidefile for current time: ',a)
      write(unit_error,610) current_dt
      call exit(2)

 15   continue                  ! found a usable tidefile
      filenm=tide_files(current_tidefile).filename
      if (new_tidefile) then
         call read_tide_head(filenm, .true.)
         tide_block_no=0
         if (repeating_tidefile) then
c-----------a new, repeating tidefile should always start on the first
c-----------tideblock; adjust the time offset accordingly
            TideTimeAdj=tide_files(current_tidefile).start_julmin -
     &           julmin
c@@@            -( (julmin-tide_files(current_tidefile).start_julmin)
c@@@     &           / tide_cycle_length_mins) * tide_cycle_length_mins
         else
            TideTimeAdj=0
         endif
         TideTimeAdj=TideTimeAdj +
     &        tide_files(current_tidefile).start_julmin_file -
     &        tide_files(current_tidefile).start_julmin -
     &        tide_files(current_tidefile).interval
         if (print_level.ge.1) then
            ctmp=jmin2cdt(julmin+TideTimeAdj)
            write(unit_screen,922) 'Opened a new',
     &           filenm(:lnblnk(filenm)), current_dt,ctmp
            write(unit_output,922) 'Opened a new',
     &           filenm(:lnblnk(filenm)), current_dt,ctmp
 922        format(/a,' hydro-binary tidefile: '/a
     &           /' model time: ',a,' tide time: ',a)
         endif
      endif

      if (recycle_tidefile) then
         TideTimeAdj=TideTimeAdj-tide_cycle_length_mins
         if (print_level.ge.2) then
            ctmp=jmin2cdt(julmin+TideTimeAdj)
            write(unit_screen,922) 'Recycled',
     &           filenm(:lnblnk(filenm)), current_dt,ctmp
            write(unit_output,922) 'Recycled',
     &           filenm(:lnblnk(filenm)), current_dt,ctmp
         endif
      endif

      if (warmup_run .and. .not. repeating_tidefile) then
c--------always use first time period's flows
         TideTimeAdj=tide_files(current_tidefile).start_julmin -
     &           julmin
      endif

c-----read tide flows and process the data for each module (qual and ptm)
      call read_tide_flow(julmin+TideTimeAdj,new_tidefile,
     &     recycle_tidefile,first_used_tidefile,current_tidefile,
     &     tide_block_no,.true.)

      return

      end
