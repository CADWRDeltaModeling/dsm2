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

 
      subroutine process_tide(new_tidefile,recycle_tidefile,
     &     first_used_tidefile,current_tidefile,tide_block_no)

c-----Process Hydro tidefile data into arrays needed by Qual.

      implicit none

      include 'param.inc'
      include '../hydro/network.inc'
      include '../input/fixed/common.f'
      include '../input/time-varying/common_tide.f'
      include '../input/time-varying/tide.inc'
      include 'bltm1.inc'
      include 'bltm3.inc'
      include 'bltm2.inc'

c-----arguments
      logical
     &     new_tidefile         ! true if new tidefile (INPUT)
      integer
     &     first_used_tidefile  ! first used tidefile number (INPUT)
     &     ,current_tidefile    ! current tidefile number (INPUT)
     &     ,tide_block_no       ! tide block number within tidefile (INPUT)

c-----local variables
      integer
     &     i,j,k                ! array indices
     &     ,old_tide_block_no   ! old tide block number

      integer*4 next_smoothing  ! when next smoothing should occur
     &     ,incr_intvl          ! increment julian minute by interval function

      logical
     &     smoothing_needed     ! smoothing needed between tidefiles
     &     ,new_tide            ! new tide block being used
     &     ,recycle_tidefile    ! true if tidefile should be recycled (rewound)

      real
     &     correct_fact,totv,totnewv
     &     ,totsysv
     &     ,totsysnewv

      real * 8 mass_before(MAX_CONSTITUENT),mass_after(MAX_CONSTITUENT)

      integer cons_no
      save old_tide_block_no

      ! initialize the mass_correction_factor
      do i=1,neq
         mass_correction_factor(i)=1.
      enddo

      new_tide=(tide_block_no .ne. old_tide_block_no) .or. new_tidefile
      old_tide_block_no=tide_block_no
c-----smoothing is needed if a new tidefile is read, or if a tidefile
c-----is being recycled, or if tidecycle_length time has passed
      if (next_smoothing .eq. 0) then
         next_smoothing=incr_intvl(start_julmin,tide_cycle_length,
     &        TO_BOUNDARY)
      endif
      smoothing_needed=(new_tidefile .and.
     &     current_tidefile-first_used_tidefile .gt. 0) .or.
     &     recycle_tidefile .or. julmin .ge. next_smoothing

      if (new_tidefile .and.
     &     current_tidefile-first_used_tidefile .eq. 0) then
         do i=1,nres
            resvol(i)=(eresv(i)-hres(i))*ares(i)
         enddo
      endif

      if (new_tide) then
c--------assign flows and concentrations to objects
c--------reservoirs
         do k=1, maxresnodes
            do j=1, max_reservoirs
               qres(j,k)=qresv(j,k)
            enddo
         enddo
      endif                     ! end new tide

      if (smoothing_needed) then
         IF(MASS_TRACKING)THEN
            call determine_mass_in_delta(mass_before)
         endif
         if (print_level .ge. 3) then
            write(unit_screen,*) 'Smoothing tidefile at time ',current_dt
         endif
         next_smoothing=incr_intvl(next_smoothing,tide_cycle_length,
     &        TO_BOUNDARY)
         totsysv=0.
         totsysnewv=0.0
         do n=1,nbrch
            totv=0.
            correct_fact=1.
            do k=1,ns(n)
               totv=totv+gpv(n,k)
            enddo
            totnewv=achan_avg(n) * chan_geom(int2ext(n)).length
            totsysv=totsysv+totv
            totsysnewv=totsysnewv+totnewv
            correct_fact=totnewv/totv

            do k=1,ns(n)
               gpv(n,k)=gpv(n,k)*correct_fact
            enddo
            do i=1,nxsec(n)
               gvu(n,i)=gvu(n,i)*correct_fact
            enddo
            if (print_level.ge.7) then
               write(unit_output,960)int2ext(n),totv,totnewv,correct_fact
 960           format(1P ' ADJUSTING VOL  FOR CH:',i4,' OLD V=',e14.7,
     &              ' NEW V=',e14.7,' CORRECTION FACT=',0p f10.7)
            endif
         enddo

         if (print_level .ge. 4) then
            write(unit_screen,965) totsysv,totsysnewv,totsysnewv/totsysv
 965        format(1P ' OLD SYS V=',e14.7,' NEW SYS V=',e14.7,
     &           ' CORRECTION FACT=',0P f10.7)
         endif

c--------Now adjust the reservoirs

         do i=1,nres
            resvol(i)=(eresv(i)-hres(i))*ares(i)
         enddo
         IF(MASS_TRACKING)THEN
            call determine_mass_in_delta(mass_after)
            do cons_no=1,neq
               mass_correction_factor(cons_no)=mass_after(cons_no)/mass_before(cons_no)
               TOTOLDCONSTIT(CONS_NO)=TOTOLDCONSTIT(CONS_NO)*mass_correction_factor(cons_no)
               TOTCONSTITENTER(CONS_NO)=TOTCONSTITENTER(CONS_NO)*mass_correction_factor(cons_no)
               TOTCONSTITEXPORT(CONS_NO)=TOTCONSTITEXPORT(CONS_NO)*mass_correction_factor(cons_no)
               TOTCONSTITCHDEP(CONS_NO)=TOTCONSTITCHDEP(CONS_NO)*mass_correction_factor(cons_no)
               TOTCONSTITPUMP(CONS_NO)=TOTCONSTITPUMP(CONS_NO)*mass_correction_factor(cons_no)
               do i=1, num_masstrack_boundary_lines
                  totmass_passed_through_boundary_line(i,CONS_NO)=
     &                 totmass_passed_through_boundary_line(i,CONS_NO)*
     &                 mass_correction_factor(cons_no)
               enddo
            enddo
         endif
      endif
      

      return
      end

      subroutine check_tidefile(dim_res,dim_chan,n_res,n_chan
     &     ,tidefile)

c-----Check Hydro tidefile for size compatibility with Qual.

      implicit none

      include '../input/fixed/common.f'
      include 'param.inc'
      include 'bltm1.inc'
      include 'bltm3.inc'
      include 'bltm2.inc'

c-----arguments
      integer
     &     dim_res,dim_chan     ! reservoir and channel array dimensions
     &     ,n_res,n_chan        ! reservoir and channels used in tidefile

      character*(*) tidefile    ! tidefile name

 610  format(/'Incorrect ',a,' ',a,' in tidefile:'/a
     &     /'tidefile was ',i4,', Qual uses ',i4)

      if (dim_res .ne. maxnumres) then
         write(unit_error,610) 'reservoir', 'dimension',
     &        tidefile,dim_res,maxnumres
         call exit(2)
      else if (dim_chan .ne. nobr) then
         write(unit_error,610) 'channel', 'dimension',
     &        tidefile,dim_chan,nobr
         call exit(2)
      else if (n_res .ne. nres) then
         write(unit_error,610) 'reservoir', 'number',
     &        tidefile,n_res,nres
         call exit(2)
      else if (n_chan .ne. nbrch) then
         write(unit_error,610) 'channel', 'number',
     &        tidefile,n_chan,nbrch
         call exit(2)
      endif

      return
      end
