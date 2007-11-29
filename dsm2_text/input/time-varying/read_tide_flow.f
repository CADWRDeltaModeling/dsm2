C!    Copyright (C) 1996, 1997, 1998 State of California,
C!    Department of Water Resources.
C!
C!    Delta Simulation Model 2 (DSM2): A River, Estuary, and Land
C!    numerical model.  No protection claimed in original FOURPT and
C!    Branched Lagrangian Transport Model (BLTM) code written by the
C!    United States Geological Survey.  Protection claimed in the
C!    routines and files listed in the accompanying file "Protect.txt".
C!    If you did not receive a copy of this file contact Tara Smith,
C!    below.
C!
C!    This program is licensed to you under the terms of the GNU General
C!    Public License, version 2, as published by the Free Software
C!    Foundation.
C!
C!    You should have received a copy of the GNU General Public License
C!    along with this program; if not, contact Tara Smith, below,
C!    or the Free Software Foundation, 675 Mass Ave, Cambridge, MA
C!    02139, USA.
C!
C!    THIS SOFTWARE AND DOCUMENTATION ARE PROVIDED BY THE CALIFORNIA
C!    DEPARTMENT OF WATER RESOURCES AND CONTRIBUTORS "AS IS" AND ANY
C!    EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
C!    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
C!    PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE CALIFORNIA
C!    DEPARTMENT OF WATER RESOURCES OR ITS CONTRIBUTORS BE LIABLE FOR
C!    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
C!    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
C!    OR SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS; OR
C!    BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
C!    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
C!    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
C!    USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
C!    DAMAGE.
C!
C!    For more information about DSM2, contact:
C!
C!    Tara Smith
C!    California Dept. of Water Resources
C!    Division of Planning, Delta Modeling Section
C!    1416 Ninth Street
C!    Sacramento, CA  95814
C!    916-653-9885
C!    tara@water.ca.gov
C!
C!    or see our home page: http://baydeltaoffice.water.ca.gov/modeling/deltamodeling/


      subroutine read_tide_flow(DesiredTime,new_tidefile,
     &     recycle_tidefile,first_used_tidefile,current_tidefile,
     &     tide_block_no,process_data)

c-----Read the time-varying flow values from a DSM2-Hydro
c-----tide (binary) file.  This routine assumes that the correct
c-----tidefile is being used for the desired time specified, however,
c-----it will search within the tidefile for the correct tide block.

      implicit none

      include '../fixed/common.f'
      include 'common_tide.f'
      include 'tide.inc'

c-----arguments

      integer*4 DesiredTime     ! desired time of tide block, in julian minutes (INPUT)
      logical new_tidefile      ! true if new tidefile (INPUT)
     &     ,recycle_tidefile    ! true if tidefile should be recycled (rewound)
     &     ,process_data        ! check and process tide data
      integer current_tidefile  ! current tidefile number that is being used (INPUT)
     &     ,first_used_tidefile ! number of first tidefile that was used
     &     ,tide_block_no       ! tide block number used (INPUT & RETURNED)

c-----local variables

      character*14 cdt1,cdt2,cdt3 ! datetimes
     &     ,jmin2cdt            ! returns char datetime given julian minute

      integer unit_tide         ! binary tide file unit number
     &     ,ndx                 ! external flow global index number
     &     ,i                   ! loop index
     &     ,lnblnk              ! intrinsic last non-blank function

      integer*4 OldTideTime     ! time of previous tide block
      save OldTideTime

      character*150 filenm
c      integer nnn

      data OldTideTime /0/

      unit_tide=io_files(qual,io_tide,io_read).unit

c-----start from beginning of tide file if needed
      if (OldTideTime .gt. DesiredTime) then
         inquire(unit=unit_tide,name=filenm)
         OldTideTime=TideTime
         close(unit_tide)
         call read_tide_head(filenm, process_data)
         tide_block_no=0
         TideTime=0
      endif
c-----The last condition was added by Ganesh
c-----!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      do while (TideTime .lt. DesiredTime .or.
     &     DesiredTime .eq. tide_length .or.
     &     DesiredTime .eq. tide_start
     &     .or.  TideTime .le. Start_julmin)
         OldTideTime=TideTime
         read(unit_tide,end=100) TideTime ! time at end of tide averaged block
c--------instantaneous values at start of tide block
         read(unit_tide) Eresv  ! reservoir elevation
         read(unit_tide) Ychan  ! channel depth at each end
         read(unit_tide) Achan  ! channel cross-sectional flow area at each end
         read(unit_tide) Achan_Avg ! average channel cross-sectional flow area

c--------the averages are between this TideTime and previous TideTime
         read(unit_tide) QResv  ! average reservoir flow thru gates
         read(unit_tide) (obj2obj(i).flow_avg,i=1,nobj2obj)
         read(unit_tide) Qchan  ! average channel flow at each end
         read(unit_tide) nqext_changed ! external flows
         if (nqext_changed .gt. 0) then
            read(unit_tide) (
     &           ndx,           ! external flow index number
     &           qext(ndx).avg, ! average external flow value
     &           i=1,nqext_changed)
         endif
         tide_block_no=tide_block_no+1
         if (process_data) then
            if (print_level .ge. 5) then
               cdt1=jmin2cdt(Tidetime)
               write(unit_screen,923)
     &              tide_block_no,cdt1,current_dt
               write(unit_output,923)
     &              tide_block_no,cdt1,current_dt
 923           format(' Read hydro-binary tidefile block: ',i5
     &              /' Tide block time to: ',a,' Current model time: ',a)
            endif
            call process_tide(new_tidefile,recycle_tidefile,
     &           first_used_tidefile,current_tidefile,tide_block_no)
         endif
         if (DesiredTime .eq. tide_start) return
      enddo

      return

 100  continue                  ! EOF on tide file
      close(unit_tide)
      if (process_data) then
         cdt1=jmin2cdt(tide_files(current_tidefile).start_julmin_file)
         cdt2=jmin2cdt(tide_files(current_tidefile).end_julmin_file)
         cdt3=jmin2cdt(DesiredTime)
         write(unit_error,620)
     &        tide_files(current_tidefile).filename
     &        (:lnblnk(tide_files(current_tidefile).filename)),
     &        cdt3,cdt1,cdt2
 620     format(/'Software error in read_tide_flow for tidefile '/a
     &        /'Wanted tide data for time ',a,' but tidefile range is '
     &        /a,'->',a)
         call exit(2)
      endif

      return

      end
