C!<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    DSM2 is free software: you can redistribute it and/or modify
C!    it under the terms of the GNU General Public !<license as published by
C!    the Free Software Foundation, either version 3 of the !<license, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public !<license for more details.

C!    You should have received a copy of the GNU General Public !<license
C!    along with DSM2.  If not, see <http://www.gnu.org/!<licenses/>.
C!</license>

      REAL*8 function get_output(ptr)

c-----Get the desired output variable from the particular DSM module
      use Gates, only:gateArray
	use IO_Units
	use grid_data
	use iopath_data
	use constants
      implicit none

c-----arguments

      integer
     &     ptr                  ! output path pointer

c-----global variables

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

      if (pathoutput(ptr).obj_type .eq. obj_channel) then ! output is from channel

         intchan=pathoutput(ptr).obj_no
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
      else if (pathoutput(ptr).obj_type .eq. obj_reservoir) then ! output is from reservoir
         hydrores=pathoutput(ptr).obj_no
         nodeup=pathoutput(ptr).res_node_no
         if (nodeup .gt. 0) then ! flow to node
            get_output=-qres(hydrores,nodeup) ! + qres: flow from res to chan
         else if (pathoutput(ptr).meas_type .eq. 'stage') then ! stage of reservoir
            get_output=yres(hydrores)
         else if (pathoutput(ptr).meas_type .eq. 'flow-net') then ! net flow in/out of reservoir
            get_output=reservoir_source_sink(pathoutput(ptr).obj_no, ALL_FLOWS)
            do i=1,res_geom(hydrores).nnodes
               get_output=get_output-qres(hydrores,i)
            enddo
         else if (pathoutput(ptr).meas_type .eq. 'pump') then ! net pumping out of reservoir
            get_output=reservoir_source_sink(pathoutput(ptr).obj_no, ALL_FLOWS)
         endif

	else if (pathoutput(ptr).obj_type .eq. obj_gate) then
	   if(pathoutput(ptr).meas_type .eq. 'pos') then
	       ! //@todo: 'pos' is deprecated
	       get_output=gateArray(pathoutput(ptr).obj_no).Devices(
     &	     pathoutput(ptr).gate_device).opCoefToNode
	   else if(pathoutput(ptr).meas_type .eq. 'flow') then
     	       get_output=gateArray(pathoutput(ptr).obj_no).flow
	   else if(pathoutput(ptr).meas_type .eq. 'device-flow') then
   	       get_output=gateArray(pathoutput(ptr).obj_no).Devices(
     &	     pathoutput(ptr).gate_device).flow
	   else if(pathoutput(ptr).meas_type .eq. 'op-to-node') then
     	       get_output=gateArray(pathoutput(ptr).obj_no).Devices(
     &	     pathoutput(ptr).gate_device).opCoefToNode
	   else if(pathoutput(ptr).meas_type .eq. 'op-from-node') then
     	       get_output=gateArray(pathoutput(ptr).obj_no).Devices(
     &	     pathoutput(ptr).gate_device).opCoefFromNode
	   else if(pathoutput(ptr).meas_type .eq. 'position') then
     	       get_output=gateArray(pathoutput(ptr).obj_no).Devices(
     &	     pathoutput(ptr).gate_device).position
	   else if(pathoutput(ptr).meas_type .eq. 'install') then
	       get_output=1.0
	       if(gateArray(pathoutput(ptr).obj_no).free) get_output=0.
	   else if(pathoutput(ptr).meas_type .eq. 'height') then
   	       get_output=gateArray(pathoutput(ptr).obj_no).Devices(
     &	     pathoutput(ptr).gate_device).height
	   else if(pathoutput(ptr).meas_type .eq. 'elev') then
   	       get_output=gateArray(pathoutput(ptr).obj_no).Devices(
     &	     pathoutput(ptr).gate_device).baseElev
	   else
	       get_output=miss_val_r
	   end if
      endif

      return
      end
