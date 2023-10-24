!!<license>
!!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
!!    Department of Water Resources.
!!    This file is part of DSM2.

!!    The Delta Simulation Model 2 (DSM2) is free software:
!!    you can redistribute it and/or modify
!!    it under the terms of the GNU General Public License as published by
!!    the Free Software Foundation, either version 3 of the License, or
!!    (at your option) any later version.

!!    DSM2 is distributed in the hope that it will be useful,
!!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!!    GNU General Public License for more details.

!!    You should have received a copy of the GNU General Public License
!!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
!!</license>

real*8 function get_output(ptr)

    !-----Get the desired output variable from the particular DSM module
    use Gates, only: gateArray
    use IO_Units
    use grid_data
    use iopath_data
    use constants
    use chconnec
    use netbnd, only: reservoir_source_sink
    use channel_schematic, only: StreamEndNode
    use chstatus, only: &
        GlobalStreamFlow &  ! Hydro function to return flow &
        , GlobalStreamSurfaceElevation! Hydro function to return stage &
    use tidefile, only: ChannelVelocity     ! Hydro function to return velocity
    implicit none

    !-----arguments

    integer &
        ptr                  ! output path pointer

    !-----global variables

    !-----local variables

    integer &
        intchan &            ! internal channel numbers &
        , nodeup, nodedown &    ! Hydro upstream and downstream 'node' number &
        , hydrores &          ! Hydro reservoir number
        , ngpoints &
        , i                   ! loop index

    integer node1, node2    !up and down global comp. node
    real*8 &
        val_x &              ! interpolated value statement function &
        , val_up, val_down &    ! value at upstream and downstream end of chan &
        , reach_dist &          ! distance in a reach (not channel) &
        , reach_len &          ! reach length &
        , Q_interp &         !interpolated discharge &
        , Z_interp            !interpolated stage
    !-----statement function to interpolate value along channel
    val_x(val_up, val_down, reach_dist, reach_len) = val_up - (val_up &
                                                               - val_down)*(reach_dist/reach_len)

    if (pathoutput(ptr)%obj_type == obj_channel) then ! output is from channel

        intchan = pathoutput(ptr)%obj_no
        nodedown = -StreamEndNode(-intchan)
        nodeup = StreamEndNode(intchan)
        ngpoints = nodedown - nodeup + 1
        if (ngpoints < 2) then
            write (unit_error, 901) chan_geom(intchan)%chan_no, ngpoints
901         format(' Error in output specification in channel:', i6/ &
                   ' Number of grid points=', i6)
            call exit(2)
        end if
        !         closest_node=int(dfloat(nodeup)+dfloat(pathoutput(ptr)%chan_dist)/
        !     &        dfloat(chan_geom(intchan)%length)*(dfloat(nodedown)-
        !     &        dfloat(nodeup))+0.5)
        node1 = int(dfloat(nodeup) + dfloat(pathoutput(ptr)%chan_dist)/ &
                    dfloat(chan_geom(intchan)%length)*(dfloat(nodedown) - &
                                                       dfloat(nodeup)))
        if (node1 == nodedown) node1 = node1 - 1
        node2 = node1 + 1
        reach_len = dfloat(chan_geom(intchan)%length)/(dfloat(nodedown) - &
                                                       dfloat(nodeup))
        reach_dist = dfloat(pathoutput(ptr)%chan_dist) - reach_len*(node1 - nodeup)

        if (node1 < nodeup .or. node2 > nodedown .or. &
            pathoutput(ptr)%chan_dist > chan_geom(intchan)%length) then
            write (unit_error, 902) chan_geom(intchan)%chan_no, &
                pathoutput(ptr)%chan_dist, &
                chan_geom(intchan)%length
902         format('Error in output specification for channel=', i6/ &
                   'output specified for distance=', i10/ &
                   'channel length=', i10)
            call exit(2)
        end if
        if (pathoutput(ptr)%meas_type == 'stage') then
            get_output = val_x( &
                         globalStreamSurfaceElevation(node1), &
                         globalStreamSurfaceElevation(node2), &
                         reach_dist, &
                         reach_len)
        else if (pathoutput(ptr)%meas_type(1:3) == 'vel') then
            get_output = ChannelVelocity(intchan, dfloat(pathoutput(ptr)%chan_dist))
        else if (pathoutput(ptr)%meas_type == 'flow') then
            get_output = val_x( &
                         globalStreamFlow(node1), &
                         globalStreamFlow(node2), &
                         reach_dist, &
                         reach_len)
        end if
        !else if (pathoutput(ptr)%obj_type .eq. obj_qext) then
        !   get_output=qext(pathoutput(ptr)%obj_no)
    else if (pathoutput(ptr)%obj_type == obj_reservoir) then ! output is from reservoir
        hydrores = pathoutput(ptr)%obj_no
        nodeup = pathoutput(ptr)%res_node_no
        if (nodeup > 0) then ! flow to node
            do i = 1, res_geom(hydrores)%nnodes
                if (res_geom(hydrores)%node_no(i) == nodeup) then
                    get_output = -qres(hydrores, i)
                end if
            end do
            !get_output=-qres(hydrores,nodeup) ! + qres: flow from res to chan
        else if (pathoutput(ptr)%meas_type == 'stage') then ! stage of reservoir
            get_output = yres(hydrores)
        else if (pathoutput(ptr)%meas_type == 'flow-net') then ! net flow in/out of reservoir
            get_output = reservoir_source_sink(pathoutput(ptr)%obj_no, ALL_FLOWS)
            do i = 1, res_geom(hydrores)%nnodes
                get_output = get_output - qres(hydrores, i)
            end do
        else if (pathoutput(ptr)%meas_type == 'flow-source') then ! net source in/out of reservoir
            get_output = reservoir_source_sink(pathoutput(ptr)%obj_no, QEXT_FLOWS)
        else if (pathoutput(ptr)%meas_type == 'pump') then ! net pumping out of reservoir
            get_output = reservoir_source_sink(pathoutput(ptr)%obj_no, ALL_FLOWS)
        end if

    else if (pathoutput(ptr)%obj_type == obj_gate) then
        if (pathoutput(ptr)%meas_type == 'pos') then
            ! //@todo: 'pos' is deprecated
            get_output = gateArray(pathoutput(ptr)%obj_no)%Devices( &
                         pathoutput(ptr)%gate_device)%opCoefToNode
        else if (pathoutput(ptr)%meas_type == 'flow') then
            get_output = gateArray(pathoutput(ptr)%obj_no)%flow
        else if (pathoutput(ptr)%meas_type == 'device-flow') then
            get_output = gateArray(pathoutput(ptr)%obj_no)%Devices( &
                         pathoutput(ptr)%gate_device)%flow
        else if (pathoutput(ptr)%meas_type == 'op-to-node' .or. &
                 pathoutput(ptr)%meas_type == 'op_to_node') then
            get_output = gateArray(pathoutput(ptr)%obj_no)%Devices( &
                         pathoutput(ptr)%gate_device)%opCoefToNode
        else if (pathoutput(ptr)%meas_type == 'op-from-node' .or. &
                 pathoutput(ptr)%meas_type == 'op_from_node') then
            get_output = gateArray(pathoutput(ptr)%obj_no)%Devices( &
                         pathoutput(ptr)%gate_device)%opCoefFromNode
        else if (pathoutput(ptr)%meas_type == 'position') then
            get_output = gateArray(pathoutput(ptr)%obj_no)%Devices( &
                         pathoutput(ptr)%gate_device)%position
        else if (pathoutput(ptr)%meas_type == 'install') then
            get_output = 1.0
            if (gateArray(pathoutput(ptr)%obj_no)%free) get_output = 0.
        else if (pathoutput(ptr)%meas_type == 'height') then
            get_output = gateArray(pathoutput(ptr)%obj_no)%Devices( &
                         pathoutput(ptr)%gate_device)%height
        else if (pathoutput(ptr)%meas_type == 'elev') then
            get_output = gateArray(pathoutput(ptr)%obj_no)%Devices( &
                         pathoutput(ptr)%gate_device)%baseElev
        else if (pathoutput(ptr)%meas_type == 'width') then
            get_output = gateArray(pathoutput(ptr)%obj_no)%Devices( &
                         pathoutput(ptr)%gate_device)%maxWidth
        else
            get_output = miss_val_r
        end if
    end if

    return
end
