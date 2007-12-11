C!    Copyright (C) 1996, 1997, 1998 State of California,
C!    Department of Water Resources.

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

      real*8 function get_output(ptr)

c-----Get the desired output variable from the particular DSM module
      Use IO_Units
	Use Groups, Only: groupArray
      implicit none

c-----arguments

      integer
     &     ptr                  ! output path pointer

c-----global variables

      include '../input/fixed/common.f'
      include '../input/time-varying/common_tide.f'
      include 'param.inc'
      include 'bltm1.inc'
      include 'bltm2.inc'

c-----local variables

      byte object               ! object type (node, channel, ...)

      integer
     &     qualchan             ! dsm and bltm channel numbers
     &     ,object_no           ! object number
     &     ,hydro_res_no        ! hydro reservoir number
     &     ,hydro_node_no       ! hydro node number
     &     ,nx                  ! number of cross sections for channel
     &     ,i                   ! loop index
     &     ,lnblnk              ! last non blank intrinsic function

      integer
     &     const_no             ! constituent number

      real*8
     &     chan_qual            ! Qual function to return channel concentration
     &     ,node_qual           ! Qual function to return node concentration
     &     ,val_x               ! interpolated value statement function
     &     ,val_up,val_down     ! value at upstream and downstream end of chan
     &     ,chan_dist           ! distance along channel
     &     ,chan_len            ! channel length
     &     ,objflow,massrate(max_constituent) ! flow and massrate at an object

c-----statement function to interpolate value along channel
      val_x(val_up,val_down,chan_dist,chan_len)=val_up-(val_up
     &     -val_down)*(chan_dist/chan_len)

      object=pathoutput(ptr).object
      object_no=pathoutput(ptr).object_no
      if (object .eq. obj_channel) then ! output at channel requested
         qualchan=object_no
      endif
      const_no=pathoutput(ptr).const_ndx
      if (const_no. gt. 0) then ! water quality constituent requested
         if (object .eq. obj_channel) then ! output at channel requested
            get_output=chan_qual(qualchan,pathoutput(ptr).chan_dist,const_no)
         else if (object .eq. obj_node) then ! output at node requested
            get_output=node_qual(object_no,const_no)
         else if (object .eq. obj_reservoir) then ! output in reservoir requested
            get_output=cres(object_no,const_no)
         elseif (index(pathoutput(ptr).meas_type, 'mass') .gt. 0) then
         else                   ! error, can't figure out where this output is
            write(unit_error, '(a/a/a)') 'Unable to produce output for path:',
     &           '  ' // pathoutput(ptr).path(:lnblnk(pathoutput(ptr).path)),
     &           '  No location given.'
            call exit(2)
         endif
      elseif (pathoutput(ptr).meas_type .eq. 'stage') then
         if (object .eq. obj_channel) then ! channel output
            nx=chan_geom(object_no).nxsect ! last X-Section (i.e. downstream)
	      nx=2 !  fixme: the whole chan_geom(..).nxsect and .bottomelev is bogus and
	      ! should be removed
            get_output=val_x(
     &           ychan(qualchan,1)+chan_geom(object_no).bottomelev(1),
     &           ychan(qualchan,2)+chan_geom(object_no).bottomelev(nx),
     &           dble(pathoutput(ptr).chan_dist),
     &           dble(chan_geom(object_no).length))
         else if (object .eq. obj_reservoir) then ! reservoir output
            hydro_res_no=pathoutput(ptr).object_no
            get_output=eresv(hydro_res_no)
         endif
      elseif (pathoutput(ptr).meas_type .eq. 'flow') then
         if (object .eq. obj_channel) then
            get_output=val_x(qchan(qualchan,1),qchan(qualchan,2),
     &           dble(pathoutput(ptr).chan_dist),
     &           dble(chan_geom(object_no).length))
         else if (object .eq. obj_reservoir) then
            hydro_res_no=pathoutput(ptr).object_no
            hydro_node_no=pathoutput(ptr).res_node_no
            get_output=-qres(hydro_res_no,hydro_node_no) ! + qres: flow from res to chan
         endif
      elseif (pathoutput(ptr).meas_type .eq. 'pump') then ! all reservoir pumping
         call res_rate(object_no,FROM_OBJ,NO_CONNECT,objflow,massrate)
         get_output=objflow
      elseif (pathoutput(ptr).meas_type .eq. 'flow-net') then ! net reservoir pump+gate flow
         if (object .eq. obj_reservoir) then
            get_output=0.0
            do i=1,res_geom(pathoutput(ptr).object_no).nnodes
               hydro_node_no=res_geom(pathoutput(ptr).object_no).node_no(i)
               get_output=get_output-qres(hydro_res_no,i) ! + qres: flow from res to chan
            enddo

            call res_rate(object_no,FROM_OBJ,NO_CONNECT,objflow,massrate)
            get_output=get_output+objflow
         endif
      else
         write(unit_error, 600)
     &        trim(pathoutput(ptr).meas_type),
     &        trim(groupArray(pathoutput(ptr).source_group_ndx).name)
 600     format ('Error: The following constituent'
     &        ' is not simulated in this run; run stopped'
     &        /'Constituent name: ',a
     &        /'Constituent source group: ',a)
         call exit(1)

      endif

      return
      end

      real*8 function chan_qual(qualchan, chan_distance, const_no)
      Use IO_Units
c-----this routine reports water quality of DSM2-Qual channel

      implicit none

c-----subroutine arguments

      integer
     &     qualchan             ! channel no.
     &     ,chan_distance       ! location within a channel

      integer
     &     const_no             ! constituent number

c-----common blocks

      include 'param.inc'
      include '../input/fixed/common.f'
      include '../input/fixed/common_qual.inc'
      include 'bltm1.inc'
      include 'bltm2.inc'
      include 'bltm3.inc'

c-----local variables

      integer
     &     k                   ! loop index

      real*8
     &     xdist

c	fixme: qualchan --- what is it?
      xdist=dble(chan_distance)/dble(chan_geom(qualchan).length)
     &     *(nxsec(qualchan)-1)+1.
      if (chan_distance.eq.0) then
c--------pick the first parcel concentration
         chan_qual=gpt(const_no,1,qualchan)
         return
      else if (xdist.ge.px(ns(qualchan),qualchan)) then
c--------pick the last parcel concentration
         chan_qual=gpt(const_no,ns(qualchan),qualchan)
         return
      else
         do k=2,ns(qualchan)
            if (xdist. le. px(k,qualchan)) then
               chan_qual=gpt(const_no,k-1,qualchan)
               return
            end if
         end do
         write(unit_error, *)
     &        'Error inside chan_qual.f; run stopped.'
         call exit(1)
      end if

      return
      end

      real*8 function node_qual(intnode,const_no)
      Use IO_Units
c-----Return the mixed quality at a node

      implicit none

c-----arguments

      integer
     &     intnode              ! internal node number
      integer
     &     const_no             ! constituent number

c-----common blocks

      include 'param.inc'
      include '../input/fixed/common.f'
      include 'bltm1.inc'
      include 'bltm2.inc'
      include 'bltm3.inc'

      if (node_geom(intnode).qual_int) then   ! internal node (and already mixed)
         node_qual=cj(const_no,intnode)
      else                      ! external node
         if (numup(intnode) .eq. 1) then ! upstream boundary node
            node_qual=gpt(const_no,1,listup(intnode,1))
         else if (numdown(intnode) .eq. 1) then ! downstream boundary node
            node_qual=gpt(const_no,ns(listdown(intnode,1)), listdown(intnode,1))
         else
            write(unit_error, 610) node_geom(intnode).node_ID
 610        format(/'Error in node_qual, DSM2 node number ',i3)
            call exit(1)
         endif
      endif

      return
      end
