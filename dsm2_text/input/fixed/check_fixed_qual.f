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


      subroutine check_fixed_qual(istat)

c-----Check the fixed input for omissions and errors before starting
c-----the model run.  Supply default values where possible.  Translate
c-----from nodes to channel numbers, and from external channel numbers
c-----to internal.  Write to Qual arrays.

      implicit none

      include 'common.f'

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
     &     ,chan,node           ! channel, node, xsect numbers
c     &     ,numnode             ! Number of nodes
     &     ,from_number,to_number ! internal numbers for swapping
     &     ,n_reorder           ! number of reordering loop circuits
     &     ,nk
     &     ,n1
     &     ,lnblnk              ! last nonblank intrinsic function
     &     ,loccarr             ! string array location function

      integer*4
     &     incr_intvl           ! increment julian minute by interval function

      character
     &     ctemp(max_reservoirs)*20 ! scratch array

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
         dqq(i)=chan_geom(int2ext(i)).disp
      enddo

c-----have to renumber nodes for bltm
c-----internal nodes have to be numbered first

c-----First figure out the number of internal nodes

      numnode=0
      njnct=0
      do node=1,max_nodes
         if (node_geom(node).nup +  node_geom(node).ndown .ge. 1) then
c-----------node exists
            numnode=numnode+1
            if ((node_geom(node).nup + node_geom(node).ndown).ne.1)then ! internal node
               njnct=njnct+1
            endif
         endif
      enddo

c-----Now renumber the nodes (internal ones first)

      nk=0
      nl=numnode+1

      do n1=1,max_nodes
         nconres(n1)=0
         node=n1
         if (node_geom(node).nup +  node_geom(node).ndown .ge. 1) then
c-----------node exists
            if ((node_geom(node).nup + node_geom(node).ndown).ne.1)then ! internal node
               nk=nk+1
               nodedsm2qual(node)=nk
               nodequal2dsm(nk)=node
            else                ! external node
               nl=nl-1
               nodedsm2qual(node)=nl
               nodequal2dsm(nl)=node
            endif
         endif
      enddo

  
c-----reorder internal node numbers to ensure that obj2obj
c-----transfers are handled properly

      n_reorder=0
 100  continue                  ! one-trip do while (do until)
      reorder=.false.
      do i=1,nobj2obj
         if (obj2obj(i).from.object .eq. obj_node .and.
     &        obj2obj(i).to.object .eq. obj_node) then
            from_number=nodedsm2qual(obj2obj(i).from.object_no)
            to_number=nodedsm2qual(obj2obj(i).to.object_no)
            if (from_number .gt. to_number) then ! these must be reversed
               reorder=.true.
               nodedsm2qual(obj2obj(i).from.object_no)=to_number
               nodedsm2qual(obj2obj(i).to.object_no)=from_number
               nodequal2dsm(to_number)=obj2obj(i).from.object_no
               nodequal2dsm(from_number)=obj2obj(i).to.object_no
            endif
         endif
      enddo
      if (reorder) n_reorder=n_reorder+1
      if (reorder .and.
     &     n_reorder .le. 10) goto 100
      if (n_reorder .gt. 10) then
         write(unit_error,*)
     &        'Too many obj2obj reordering loops, probably infinite loop.'
         goto 900
      endif

      do n1=1,max_nodes
         node=n1
         if (node_geom(node).nup +  node_geom(node).ndown .ge. 1) then
c-----------node exists
            do i=1,node_geom(node).nup
               chan=ext2int(node_geom(node).upstream(i))
               jncu(chan)=nodedsm2qual(node)
            enddo

            do i=1,node_geom(node).ndown
               chan=ext2int(node_geom(node).downstream(i))
               jncd(chan)=nodedsm2qual(node)
            enddo
         endif
      enddo

c-----Now come the reservoirs

      nres=0
      do i=1,max_reservoirs
c--------channel connections to this reservoir
         if (res_geom(i).nnodes .gt. 0) then
            nres=nres+1
            ares(nres)=res_geom(nres).area*1.e6
            hres(nres)=res_geom(nres).botelv
            nresjunc(nres)=res_geom(nres).nnodes
            do j=1,res_geom(nres).nnodes
               reschjunc(nres,j)=res_geom(nres).node_no(j)
               lresjunc(nres,j)=nodedsm2qual(reschjunc(nres,j))
               ij=lresjunc(nres,j)

               nconres(ij)=nconres(ij)+1 ! Number of reservoirs connected to node ij

C--------------For each junction figure out which reservoir (nres) is connected
C--------------Also figure out in the list of connecting junctions to
C--------------this reservoir, which one is this (J)

               lconres(ij,nconres(ij),1)=nres
               lconres(ij,nconres(ij),2)=j
            enddo

            if (no_of_nonconserve_constituent .gt. 0) then
c--------------Now transfer the rate coefficients
               j=loccarr(res_geom(i).name,coeff_res_name,num_res,EXACT_MATCH)
               if (j .gt. 0) then
c-----------------found a match
                  do k=1, max_constituent
                     do l=1,ncoef_type
                        rcoef_res(k,l,i)=rcoef_res_temp(k,l,j)
                     enddo
                  enddo
               else
c-----------------did not find a match
                  write(unit_error,605)
     &                 res_geom(i).name(:lnblnk(res_geom(i).name))
               endif
            endif
         endif
      enddo

c-----check for bogus rservoir name given in rate coefficients
c-----apparently passing slices of structures in calls, chokes
      do i=1,max_reservoirs
         ctemp(i)=res_geom(i).name
      enddo
      do i=1,num_res
         k=loccarr(coeff_res_name(i),ctemp,max_reservoirs,EXACT_MATCH)
         if (k .le. 0) then     ! bogus reservoir name for rate coefficients
            write(unit_error, 607) coeff_res_name(i)
            goto 900
         endif
      enddo

c-----convert rate coefficients from rate per day to rate per hour
      do k = 1, max_channels
         do i = 1, max_constituent
            do j = 1, ncoef_type
               rcoef_chan(i,j,k)=rcoef_chan(i,j,k)/24.
            enddo
         enddo
      enddo
      do k = 1, max_reservoirs
         do i = 1, max_constituent
            do j = 1, ncoef_type
               rcoef_res(i,j,k)=rcoef_res(i,j,k)/24.
            enddo
         enddo
      enddo

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
c--------check that at least one of location name, or node, or type is given
         if (constituents(i).conservative) then
            if (constituents(i).loc_name .eq. ' ' .and.
     &           constituents(i).object_no .eq. 0 .and.
     &           constituents(i).acct_ndx .eq. 0) then
               write(unit_error, 620) constituents(i).constituent
     &              (:lnblnk(constituents(i).constituent))
               goto 900
 620           format(/'Error: the constituent ',a,' must have specified at least',
     &              /'one of location name, node, or type.')
            endif
         endif
         if (constituents(i).constituent .eq.'tds') then
            mtds=i
         elseif (constituents(i).constituent .eq.'ec') then
            mec=i
         elseif (constituents(i).constituent .eq.'cl') then
            mcl=i
c-----------now nonconservative constituents
         elseif (constituents(i).constituent .eq.'do') then
            mdo=i
            constituent_ptr(mdo)=ncc_do
         elseif (constituents(i).constituent .eq.'organic_n') then
            morgn=i
            constituent_ptr(morgn)=ncc_organic_n
         elseif (constituents(i).constituent .eq.'nh3') then
            mnh3=i
            constituent_ptr(mnh3)=ncc_nh3
         elseif (constituents(i).constituent .eq.'no2') then
            mno2=i
            constituent_ptr(mno2)=ncc_no2
         elseif (constituents(i).constituent .eq.'no3') then
            mno3=i
            constituent_ptr(mno3)=ncc_no3
         elseif (constituents(i).constituent .eq.'organic_p') then
            morgp=i
            constituent_ptr(morgp)=ncc_organic_p
         elseif (constituents(i).constituent .eq.'po4') then
            mpo4=i
            constituent_ptr(mpo4)=ncc_po4
         elseif (constituents(i).constituent .eq.'algae') then
            malg=i
            constituent_ptr(malg)=ncc_algae
         elseif (constituents(i).constituent .eq.'bod') then
            mbod=i
            constituent_ptr(mbod)=ncc_bod
         elseif (index(constituents(i).constituent,'temp') .gt. 0) then
            constituents(i).constituent='temp'
            mtemp=i
            constituent_ptr(mtemp)=ncc_temp
         else
            if (.not. constituents(i).conservative) then
               write(unit_error,610) constituents(i).constituent
     &              (:lnblnk(constituents(i).constituent))
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
