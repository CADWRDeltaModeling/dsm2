C!    Copyright (C) 1996, 1997, 1998 State of California,
C!    Department of Water Resources.
C!
C!    Delta Simulation Model 2 (DSM2): A River, Estuary, and Land
C!    numerical model.  No protection claimed in original FOURPT and
C!    Branched Lagrangian Transport Model (BLTM) code written by the
C!    United States Geological Survey.  Protection claimed in the
C!    routines and files listed in the accompanying file "Protect.txt".
C!    If you did not receive a copy of this file contact Dr. Paul
C!    Hutton, below.
C!
C!    This program is licensed to you under the terms of the GNU General
C!    Public License, version 2, as published by the Free Software
C!    Foundation.
C!
C!    You should have received a copy of the GNU General Public License
C!    along with this program; if not, contact Dr. Paul Hutton, below,
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
C!    Dr. Paul Hutton
C!    California Dept. of Water Resources
C!    Division of Planning, Delta Modeling Section
C!    1416 Ninth Street
C!    Sacramento, CA  95814
C!    916-653-5601
C!    hutton@water.ca.gov
C!
C!    or see our home page: http://wwwdelmod.water.ca.gov/

      subroutine check_fixed_qual(istat)

c-----Check the fixed input for omissions and errors before starting
c-----the model run.  Supply default values where possible.  Translate
c-----from nodes to channel numbers, and from external channel numbers
c-----to internal.  Write to Qual arrays.
      use IO_Units

	use rate_coeff_assignment,only:rate_coeffs_to_waterbodies,check_rate_for_waterbody
      implicit none

      include 'common.f'
      include 'common_qual.inc'
      include '../../qual/param.inc'
      include '../../qual/bltm1.inc'
      include '../../qual/bltm3.inc'
      include '../../qual/bltm2.inc'

      include '../time-varying/dss.inc'
      include '../time-varying/readdss.inc'
      include '../time-varying/writedss.inc'
      include '../time-varying/common_tide.f'

c-----Local variables

      logical
     &     reorder              ! true if reordering for obj2obj occurred

      integer
     &     istat                ! status of call (returned)
     &     ,i,j,ij              ! indices
     &     ,k,l                 ! indices
     &     ,ires				  ! reservoir index
     &     ,chan,intnode        ! channel, node, xsect numbers
     &     ,loccarr             ! string array location function

      integer*4
     &     incr_intvl           ! increment julian minute by interval function

      character
     &     ctemp(max_reservoirs)*20 ! scratch array

	character errm*128 !error message,Jon 

 605  format(/'Warning: no rate coefficients given for reservoir ',a/)
 607  format(/'Error: unknown reservoir name given for rate coeffients:'
     &     /a)

      if (time_step_intvl_qual .ne. ' ') then
c--------time_step_intvl should be in form: '15min' or '1hour'
         time_step=incr_intvl(0,time_step_intvl_qual,TO_BOUNDARY)
      else
         time_step=15           ! 15 minutes is default time step
      endif

      if (.not. dispersion) then
         do chan=1,max_channels
            chan_geom(chan).disp = 0
         end do
      end if

c-----set BLTM values
      nbrch=nchans
      neq = no_of_constituent

      do i=1,nbrch
         nxsec(i)=2
         dqq(i)=chan_geom(i).disp
      enddo

c-----flag qual internal (not on boundary) nodes
      do intnode=1,nnodes
            if (node_geom(intnode).nup + node_geom(intnode).ndown .gt. 1) then
               node_geom(intnode).qual_int=.true.
            endif
            do i=1,node_geom(intnode).nup
               chan=node_geom(intnode).upstream(i)
               jncu(chan)=intnode
            enddo

            do i=1,node_geom(intnode).ndown
               chan=node_geom(intnode).downstream(i)
               jncd(chan)=intnode
            enddo
      enddo

c-----Now the reservoirs

      nconres=0
      do ires=1,nreser
c--------channel connections to this reservoir
         ares(ires)=res_geom(ires).area*1.e6
         hres(ires)=res_geom(ires).botelv
         nresjunc(ires)=res_geom(ires).nnodes
         do j=1,res_geom(ires).nnodes
            reschjunc(ires,j)=res_geom(ires).node_no(j)
            lresjunc(ires,j)=res_geom(ires).node_no(j)
            ij=lresjunc(ires,j)

            nconres(ij)=nconres(ij)+1 ! Number of reservoirs connected to node ij

C-----------For each junction figure out which reservoir (ires) is connected
C-----------Also figure out in the list of connecting junctions to
C-----------this reservoir, which one is this (J)

            lconres(ij,nconres(ij),1)=ires
            lconres(ij,nconres(ij),2)=j
         enddo

      enddo


c     here all the rate coefficient values are transferred from the assignment stuctures
c     based on groups to the waterbody-based arrays
      call rate_coeffs_to_waterbodies(istat,errm)

	if (istat.ne.0) then

	   write(unit_error,'(a)') errm

         return

	end if

	if (.not.(check_rate_for_waterbody(unit_error))) then
         write(unit_screen,*) "fatal error in checking nonconservative constitutes rate for waterbody"
	   call exit(2)
	end if



c-----Half saturation const. for light:
c-----unit conversion from 1/minute to 1/hour

      klight_half = klight_half * 60.

C-----Initialize constituent locations
      mtds  = 0
      mec   = 0
      mcl   = 0
      mbod  = 0
      mdo   = 0
      morgn = 0
      mnh3  = 0
      mno2  = 0
      mno3  = 0
      morgp = 0
      mpo4  = 0
      malg  = 0
      mtemp = 0
C-----Loop through list mapping constituent locations
      do i= 1,no_of_constituent
c--------check that group is given
         if (constituents(i).conservative) then
            if (constituents(i).group_ndx .eq. miss_val_i) then
               write(unit_error, 620) trim(constituents(i).name)
               goto 900
 620           format(/'Error: the constituent ',a,' must have specified a group')
            endif
         endif
         if (constituents(i).name .eq.'tds') then
            mtds=i
         elseif (constituents(i).name .eq.'ec') then
            mec=i
         elseif (constituents(i).name .eq.'cl') then
            mcl=i
c-----------now nonconservative constituents
         elseif (constituents(i).name .eq.'do') then
            mdo=i
            constituent_ptr(mdo)=ncc_do
         elseif (constituents(i).name .eq.'organic_n') then
            morgn=i
            constituent_ptr(morgn)=ncc_organic_n
         elseif (constituents(i).name .eq.'nh3') then
            mnh3=i
            constituent_ptr(mnh3)=ncc_nh3
         elseif (constituents(i).name .eq.'no2') then
            mno2=i
            constituent_ptr(mno2)=ncc_no2
         elseif (constituents(i).name .eq.'no3') then
            mno3=i
            constituent_ptr(mno3)=ncc_no3
         elseif (constituents(i).name .eq.'organic_p') then
            morgp=i
            constituent_ptr(morgp)=ncc_organic_p
         elseif (constituents(i).name .eq.'po4') then
            mpo4=i
            constituent_ptr(mpo4)=ncc_po4
         elseif (constituents(i).name .eq.'algae') then
            malg=i
            constituent_ptr(malg)=ncc_algae
         elseif (constituents(i).name .eq.'bod') then
            mbod=i
            constituent_ptr(mbod)=ncc_bod
         elseif (index(constituents(i).name,'temp') .gt. 0) then
            constituents(i).name='temp'
            mtemp=i
            constituent_ptr(mtemp)=ncc_temp
         else
            if (.not. constituents(i).conservative) then
               write(unit_error,610) trim(constituents(i).name)
 610           format ('Error:  Not one of the expected nonconservative constituents,'/
     &              ' so can not be simulated: Constituent name is: ',a/)
               goto 900
            endif
         endif
      enddo

      return
 900  continue                  ! here for fatal error
      istat=-2
      return

      end
