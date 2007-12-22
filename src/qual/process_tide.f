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
 
      subroutine process_tide(
     &     new_tidefile,
     &     first_used_tidefile,
     &     tidefile_ndx)

c-----Process Hydro tidefile data into arrays needed by Qual.
      Use IO_Units
      use common_qual
      use common_tide
      use logging
      use runtime_data
      use grid_data      
      implicit none

      include 'param.inc'
      include '../hydrolib/network.inc'

      include 'bltm1.inc'
      include 'bltm3.inc'
      include 'bltm2.inc'

c-----arguments
      logical
     &     new_tidefile         ! true if new tidefile (INPUT)
      integer
     &     first_used_tidefile  ! first used tidefile number (INPUT)
     &     ,tidefile_ndx    ! current tidefile number (INPUT)


c-----local variables
      integer
     &     i,j,k                ! array indices


      logical
     &     smoothing_needed     ! smoothing needed between tidefiles

      real*8
     &     correct_fact,totv,totnewv
     &     ,totsysv
     &     ,totsysnewv

      real*8 mass_before(MAX_CONSTITUENT),mass_after(MAX_CONSTITUENT)

      integer cons_no

      ! initialize the mass_correction_factor
      do i=1,neq
         mass_correction_factor(i)=1.
      enddo

c-----smoothing is needed if a new tidefile is read

      smoothing_needed=(new_tidefile .and.
     &     tidefile_ndx-first_used_tidefile .gt. 0)
     
      if (new_tidefile) then
	   !@fixme is this the right condition? was same as smoothing_needed
         do i=1,nreser
            resvol(i)=(eresv(i)-hres(i))*ares(i)
         enddo
      endif

      if (.true.) then
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
            write(unit_screen,*) 'Smoothing tidefile at time ',current_date
         endif

         totsysv=0.
         totsysnewv=0.0
         do n=1,nbrch
            totv=0.
            correct_fact=1.
            do k=1,ns(n)
               totv=totv+gpv(n,k)
            enddo
            totnewv=achan_avg(n) * chan_geom(n).length
            totsysv=totsysv+totv
            totsysnewv=totsysnewv+totnewv
	      if (totv .gt. 0.) then !@todo prevents a floating point error
               correct_fact=totnewv/totv
            else 
	         correct_fact=1.0
            end if
            do k=1,ns(n)
              gpv(n,k)=gpv(n,k)*correct_fact
            enddo
            do i=1,nxsec(n)
              gvu(n,i)=gvu(n,i)*correct_fact
            enddo
            if (print_level.ge.7) then
               write(unit_output,960)chan_geom(n).chan_no,totv,totnewv,correct_fact
 960           format(1P ' ADJUSTING VOL  FOR CH:',i4,' OLD V=',e14.7,
     &           ' NEW V=',e14.7,' CORRECTION FACT=',0p f10.7)
	      end if
         enddo

         if (print_level .ge. 4) then
            write(unit_screen,965) totsysv,totsysnewv,totsysnewv/totsysv
 965        format(1P ' OLD SYS V=',e14.7,' NEW SYS V=',e14.7,
     &           ' CORRECTION FACT=',0P f10.7)
         endif

c--------Now adjust the reservoirs

         do i=1,nreser
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
      use io_units
      use grid_data
      implicit none

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
      else if (n_res .ne. nreser) then
         write(unit_error,610) 'reservoir', 'number',
     &        tidefile,n_res,nreser
         call exit(2)
      else if (n_chan .ne. nbrch) then
         write(unit_error,610) 'channel', 'number',
     &        tidefile,n_chan,nbrch
         call exit(2)
      endif

      return
      end
