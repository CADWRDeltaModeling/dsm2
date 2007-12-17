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

      REAL*8 function get_output(ptr)

c-----Get the desired output variable from the particular DSM module
      use Gates, only:gateArray
	use IO_Units
      implicit none

c-----arguments

      integer
     &     ptr                  ! output path pointer

c-----global variables

      include '../fixed/common.f'
      include 'network.inc'
      include 'chconnec.inc'

c-----local variables

      integer
     &      intchan             ! internal channel numbers
     &     ,nodeup,nodedown     ! Hydro upstream and downstream 'node' number
     &     ,hydrores            ! Hydro reservoir number
     &     ,StreamEndNode       ! function to return node numbers
     &     ,ngpoints
     &     ,closest_node
     &     ,i                   ! loop index

      REAL*8
     &     GlobalStreamFlow    ! Hydro function to return flow
     &     ,GlobalStreamSurfaceElevation ! Hydro function to return stage
     &     ,ChannelVelocity     ! Hydro function to return velocity

      REAL*8 reservoir_source_sink
      external reservoir_source_sink

c-----statement function to interpolate value along channel
c      val_x(val_up,val_down,chan_dist,chan_len)=val_up-(val_up
c     &     -val_down)*(chan_dist/chan_len)

      if (pathoutput(ptr).object .eq. obj_channel) then ! output is from channel

         intchan=pathoutput(ptr).object_no
         nodedown=-StreamEndNode(-intchan)
         nodeup=StreamEndNode(intchan)
         ngpoints=nodedown-nodeup+1
         if(ngpoints.lt.2)then
            write(unit_error,901)chan_geom(intchan).chan_no,ngpoints
 901        format(' Error in output specification in channel:',i6/
     &             ' Number of grid points=',i6)
            call exit(2)
         endif
         closest_node=int(dfloat(nodeup)+dfloat(pathoutput(ptr).chan_dist)/
     &        float(chan_geom(intchan).length)*(dfloat(nodedown)-
     &        float(nodeup))+0.5)
         if (closest_node.lt.nodeup .or. closest_node.gt.nodedown) then
            write(unit_error,902)chan_geom(intchan).chan_no,pathoutput(ptr).chan_dist,
     &        chan_geom(intchan).length
 902        format('Error in output specification for channel=',i6/
     &             'output specified for distance=',i10/
     &             'channel length=',i10)
            call exit(2)
         endif

         if (pathoutput(ptr).meas_type .eq. 'stage') then
            get_output=globalStreamSurfaceElevation(closest_node)
         else if (pathoutput(ptr).meas_type(1:3) .eq. 'vel') then
            get_output=ChannelVelocity(intchan,dfloat(pathoutput(ptr).chan_dist))
         else if (pathoutput(ptr).meas_type .eq. 'flow') then
            get_output=globalStreamFlow(closest_node)
         endif
      else if (pathoutput(ptr).object .eq. obj_reservoir) then ! output is from reservoir
         hydrores=pathoutput(ptr).object_no
         nodeup=pathoutput(ptr).res_node_no
         if (nodeup .gt. 0) then ! flow to node
            get_output=-qres(hydrores,nodeup) ! + qres: flow from res to chan
         else if (pathoutput(ptr).meas_type .eq. 'stage') then ! stage of reservoir
            get_output=yres(hydrores)
         else if (pathoutput(ptr).meas_type .eq. 'flow-net') then ! net flow in/out of reservoir
            get_output=reservoir_source_sink(pathoutput(ptr).object_no, ALL_FLOWS)
            do i=1,res_geom(hydrores).nnodes
               get_output=get_output-qres(hydrores,i)
            enddo
         else if (pathoutput(ptr).meas_type .eq. 'pump') then ! net pumping out of reservoir
            get_output=reservoir_source_sink(pathoutput(ptr).object_no, ALL_FLOWS)
         endif

	else if (pathoutput(ptr).object .eq. obj_gate) then
	   if(pathoutput(ptr).meas_type .eq. 'pos') then
	       ! //@todo: 'pos' is deprecated
	       get_output=gateArray(pathoutput(ptr).object_no).Devices(
     &	     pathoutput(ptr).gate_device).opCoefToNode
	   else if(pathoutput(ptr).meas_type .eq. 'flow') then
     	       get_output=gateArray(pathoutput(ptr).object_no).flow
	   else if(pathoutput(ptr).meas_type .eq. 'device-flow') then
   	       get_output=gateArray(pathoutput(ptr).object_no).Devices(
     &	     pathoutput(ptr).gate_device).flow
	   else if(pathoutput(ptr).meas_type .eq. 'op-to-node') then
     	       get_output=gateArray(pathoutput(ptr).object_no).Devices(
     &	     pathoutput(ptr).gate_device).opCoefToNode
	   else if(pathoutput(ptr).meas_type .eq. 'op-from-node') then
     	       get_output=gateArray(pathoutput(ptr).object_no).Devices(
     &	     pathoutput(ptr).gate_device).opCoefFromNode
	   else if(pathoutput(ptr).meas_type .eq. 'position') then
     	       get_output=gateArray(pathoutput(ptr).object_no).Devices(
     &	     pathoutput(ptr).gate_device).position
	   else if(pathoutput(ptr).meas_type .eq. 'install') then
	       get_output=1.0
	       if(gateArray(pathoutput(ptr).object_no).free) get_output=0.
	   else if(pathoutput(ptr).meas_type .eq. 'height') then
   	       get_output=gateArray(pathoutput(ptr).object_no).Devices(
     &	     pathoutput(ptr).gate_device).height
	   else if(pathoutput(ptr).meas_type .eq. 'elev') then
   	       get_output=gateArray(pathoutput(ptr).object_no).Devices(
     &	     pathoutput(ptr).gate_device).baseElev
	   else
	       get_output=miss_val_r
	   end if
      endif

      return
      end
