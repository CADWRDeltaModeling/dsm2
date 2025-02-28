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

!     functions to query the hydro model time
!     fixme: the jliymd function is expensive, and it might be
!            better for hydro to keep track of its own time
integer function getModelTime()
    use runtime_data
    implicit none
    getModelTime = julmin
    return
end function

integer function getModelJulianDay()
    implicit none
    integer, parameter :: MIN_PER_DAY = 60*24
    integer getModelTime
    getModelJulianDay = getModelTime()/MIN_PER_DAY
    return
end function

integer function getModelYear()
    implicit none
    integer m, d, y
    integer getModelJulianDay
    call jliymd(getModelJulianDay(), y, m, d)
    getModelYear = y
    return
end function

integer function getModelMonth()
    implicit none
    integer m, d, y
    integer getModelJulianDay
    call jliymd(getModelJulianDay(), y, m, d)
    getModelMonth = m
    return
end function

integer function getModelDay()
    implicit none
    integer m, d, y
    integer getModelJulianDay
    call jliymd(getModelJulianDay(), y, m, d)
    getModelDay = d
    return
end function

integer function getModelDayOfYear()
    implicit none
    integer m, d, y, yearstart, jday
    integer getModelJulianDay, iymdjl
    jday = getModelJulianDay()
    call jliymd(jday, y, m, d)
    yearstart = iymdjl(y, 1, 1)
    getModelDayOfYear = jday - yearstart
    return
end function

integer function getModelMinuteOfDay()
    use runtime_data
    implicit none
    integer, parameter :: MIN_PER_DAY = 60*24
    getModelMinuteOfDay = mod(julmin, MIN_PER_DAY)
    return
end function

integer function getModelHour()
    use runtime_data
    implicit none
    integer, parameter :: MIN_PER_HOUR = 60
    integer getModelMinuteOfDay
    getModelHour = (getModelMinuteOfDay()/MIN_PER_HOUR)
    return
end function

integer function getReferenceMinuteOfYear(mon, day, hour, min)
    implicit none

    integer iymdjl  ! function converts ymd to julian day
    integer yr, dayyr, jday, mon, day, hour, min, getModelYear, yearstart
    integer, parameter :: MIN_PER_DAY = 60*24

    yr = getModelYear()
    jday = iymdjl(yr, mon, day)
    yearstart = iymdjl(yr, 1, 1)
    dayyr = jday - yearstart
    getReferenceMinuteOfYear = MIN_PER_DAY*dayyr + 60*hour + min
    return
end function

integer function getModelMinuteOfYear()
    implicit none
    integer, parameter :: MIN_PER_DAY = 60*24
    integer getModelMinuteOfDay, getModelDayOfYear
    getModelMinuteOfYear = MIN_PER_DAY*getModelDayOfYear() + getModelMinuteOfDay()
    return
end function

integer function getModelMinute()
    implicit none
    integer, parameter :: MIN_PER_HOUR = 60
    integer getModelMinuteOfDay
    getModelMinute = mod(getModelMinuteOfDay(), MIN_PER_HOUR)
    return
end function

integer function getModelTicks()
    use runtime_data
    implicit none

    getModelTicks = julmin;
    return
end function

subroutine set_datasource(source, expr, val, timedep)
    use type_defs
    use constants
    implicit none

    type(datasource_t) source
    integer expr
    real*8 val
    logical timedep
    source.indx_ptr = expr
    source.value = val
    if (timedep) then
        source.source_type = expression_data
    else
        source.source_type = const_data
    end if
    return
end subroutine

real*8 function chan_ec_val(chan_num, x_dist)
    use gtm_subs, only: get_select_cell_with_x, get_output_channel_vals_continue
    use common_dsm2_vars, only: output_ec_oprule
    use common_variables, only: n_chan, n_segm, chan_geom, segm, cell
    use state_variables, only: conc
    implicit none
    integer, intent(in) :: chan_num
    real*8, intent(inout) :: x_dist
    integer :: num_out_cell
    integer :: chan_num_copy(1)
    real*8 :: x_dist_copy(1)

    num_out_cell=1
    chan_num_copy(1) = chan_num
    x_dist_copy(1) = x_dist
    
    call get_select_cell_with_x(output_ec_oprule(:)%out_chan_cell,  &
                                output_ec_oprule(:)%x_from_lo_face, &
                                output_ec_oprule(:)%calc_option,    &
                                num_out_cell, chan_num_copy, x_dist_copy)
    chan_ec_val = 0.0
    call get_output_channel_vals_continue(chan_ec_val,                         &
                                          output_ec_oprule(1)%x_from_lo_face,  &
                                          output_ec_oprule(1)%calc_option,     &
                                          output_ec_oprule(1)%out_chan_cell,   &
                                          output_ec_oprule(1)%i_var)                          
    return
end function


subroutine chan_comp_point(intchan, distance, &
                           comp_points, weights)

!-----Purpose: Wrapper to CompPointAtDist that uses arrays so that
!     the arguments are pass-by-reference (mainly for calls from C)
!fixme: probably could change the interface of CompPointAtDist and
! do away with this wrapper
    use channel_schematic, only: CompPointAtDist
    implicit none

!   Arguments:
    integer :: intchan  ! Channel where comp point is being requested
    real*8  :: distance     ! Downstream distance along intchan
    integer :: comp_points(2)
    real*8 :: weights(2)
    call CompPointAtDist(intchan, distance, comp_points(1), &
                         comp_points(2), weights(1), weights(2))
    return
end subroutine

integer function resNdx(name)
    use grid_data
    implicit none
    integer i
    character*(*) name
    resNdx = miss_val_i
    call locase(name)
    do i = 1, nreser
        if (res_geom(i)%name .eq. trim(name)) then
            resNdx = i
            exit
        end if
    end do
    return
end function

integer function resConnectNdx(res_ndx, internal_node_no)
    use grid_data
    implicit none
    integer i
    integer :: res_ndx
    integer :: internal_node_no
    resConnectNdx = miss_val_i
    do i = 1, res_geom(res_ndx)%nnodes
        if (res_geom(res_ndx)%node_no(i) .eq. internal_node_no) then
            resConnectNdx = i
            exit
        end if
    end do
    return
end function

integer function gateNdx(name)
    use Gates, only: GateArray, nGate
    use constants
    implicit none

    integer i
    character*(*) name
    gateNdx = miss_val_i
    call locase(name)
    do i = 1, nGate
        if (name .eq. GateArray(i)%name) then
            gateNdx = i
            exit
        end if
    end do
    return
end function

integer function deviceNdx(gatendx, devname)
    use Gates, only: GateArray, deviceIndex
    implicit none
    integer gatendx
    character*(*) devname
    character*32 ldevname
    ldevName = devName
    call locase(ldevname)
    deviceNdx = deviceIndex(GateArray(gatendx), ldevName)
    return
end function

!     return the constant, so FORTRAN and C can share it
integer function direct_to_node()
    use GATES
    implicit none
    direct_to_node = FLOW_COEF_TO_NODE
    return
end function

!     return the constant, so FORTRAN and C can share it
integer function direct_from_node()
    use GATES
    implicit none
    direct_from_node = FLOW_COEF_FROM_NODE
    return
end function

!     return the constant, so FORTRAN and C can share it
integer function direct_to_from_node()
    use GATES
    implicit none
    direct_to_from_node = FLOW_COEF_TO_FROM_NODE
    return
end function

real*8 function get_external_flow(ndx)
    use grid_data
    implicit none

    integer ndx
    get_external_flow = qext(ndx)%flow
    return
end function

subroutine set_external_flow(ndx, val)
    use grid_data
    implicit none

    integer ndx
    real*8 val
    qext(ndx)%flow = val
    return
end subroutine

subroutine set_external_flow_datasource(ndx, expr, val, timedep)
    use grid_data
    implicit none
    integer ndx, expr
    real*8 val
    logical timedep
    call set_datasource(qext(ndx)%datasource, expr, val, timedep)
    return
end subroutine

real*8 function get_transfer_flow(ndx)
    use grid_data
    implicit none
    integer ndx
    get_transfer_flow = obj2obj(ndx)%flow
    return
end function

subroutine set_transfer_flow(ndx, val)
    use grid_data
    implicit none
    integer ndx
    real*8 val
    obj2obj(ndx)%flow = val
    return
end subroutine

subroutine set_transfer_flow_datasource(ndx, expr, val, timedep)
    use grid_data
    implicit none
    integer ndx, expr
    real*8 val
    logical timedep
    call set_datasource(obj2obj(ndx)%datasource, expr, val, timedep)
    return
end subroutine

subroutine set_gate_install(ndx, install)
    use Gates, only: GateArray, setFree
    implicit none
    integer ndx
    real*8 install
    if (install .eq. 0.D0) then
        call setFree(GateArray(ndx), .true.)
    else
        call setFree(GateArray(ndx), .false.)
    end if
    return
end subroutine

subroutine set_gate_install_datasource(gndx, expr, val, timedep)
    use Gates, only: GateArray
    implicit none
    integer gndx
    integer expr
    real*8 val
    logical timedep
    call set_datasource( &
        GateArray(gndx)%install_datasource, expr, val, timedep)
    return
end subroutine

real*8 function is_gate_install(ndx)
    use Gates, only: GateArray
    implicit none
    integer ndx
    if (GateArray(ndx)%free) then
        is_gate_install = 0.0
    else
        is_gate_install = 1.0
    end if
    return
end function

real(8) function get_device_op_coef(gndx, devndx, direction)
    use Gates, only: GateArray
    implicit none
    integer gndx, devndx, direction
    integer direct_to_node, direct_from_node, direct_to_from_node
    get_device_op_coef = -901.0
    if (direction .eq. direct_to_node()) then
        get_device_op_coef = GateArray(gndx)%Devices(devndx)%opCoefToNode
    else if (direction .eq. direct_from_node()) then
        get_device_op_coef = GateArray(gndx)%Devices(devndx)%opCoefFromNode
    else
        if (direction .eq. direct_to_from_node()) then
            get_device_op_coef = (GateArray(gndx)%Devices(devndx)%opCoefFromNode + &
                                  GateArray(gndx)%Devices(devndx)%opCoefFromNode)/2.D0
        end if
    end if
    return
end function

subroutine set_device_op_coef(gndx, devndx, direction, val)
    use Gates, only: GateArray
    implicit none
    integer gndx, devndx, direction
    integer direct_to_node, direct_from_node, direct_to_from_node
    real(8) val
    if (direction .eq. direct_to_node()) then
        GateArray(gndx)%Devices(devndx)%opCoefToNode = val
    else if (direction .eq. direct_from_node()) then
        GateArray(gndx)%Devices(devndx)%opCoefFromNode = val
    else if (direction .eq. direct_to_from_node()) then
        GateArray(gndx)%Devices(devndx)%opCoefToNode = val
        GateArray(gndx)%Devices(devndx)%opCoefFromNode = val
    end if
    return
end subroutine

subroutine set_device_op_datasource(gndx, devndx, direction, expr, val, timedep)
    use Gates, only: GateArray
    implicit none
    integer gndx, devndx, direction
    integer direct_to_node, direct_from_node, direct_to_from_node
    integer expr
    real*8 val
    logical timedep
    if (direction .eq. direct_to_node()) then
        call set_datasource( &
            GateArray(gndx)%Devices(devndx)%op_to_node_datasource, expr, val, timedep)
    else if (direction .eq. direct_from_node()) then
        call set_datasource( &
            GateArray(gndx)%Devices(devndx)%op_from_node_datasource, expr, val, timedep)
    else if (direction .eq. direct_to_from_node()) then
        call set_datasource( &
            GateArray(gndx)%Devices(devndx)%op_from_node_datasource, expr, val, timedep)
        call set_datasource( &
            GateArray(gndx)%Devices(devndx)%op_to_node_datasource, expr, val, timedep)
    end if
    return
end subroutine

real(8) function get_device_height(gndx, devndx)
    use Gates, only: GateArray
    implicit none
    integer gndx, devndx
    get_device_height = GateArray(gndx)%Devices(devndx)%height
    return
end function

subroutine set_device_height(gndx, devndx, val)
    use Gates, only: GateArray
    implicit none
    integer gndx, devndx
    real(8) val
    GateArray(gndx)%Devices(devndx)%height = val
end subroutine

subroutine set_device_height_datasource(gndx, devndx, expr, val, timedep)
    use Gates, only: GateArray
    implicit none
    integer gndx, devndx
    integer expr
    real*8 val
    logical timedep
    call set_datasource( &
        GateArray(gndx)%Devices(devndx)%height_datasource, expr, val, timedep)
    return
end subroutine

real(8) function get_device_width(gndx, devndx)
    use Gates, only: GateArray
    implicit none
    integer gndx, devndx
    get_device_width = GateArray(gndx)%Devices(devndx)%maxWidth
    return
end function

subroutine set_device_width(gndx, devndx, val)
    use Gates, only: GateArray
    implicit none
    integer gndx, devndx
    real(8) val
    GateArray(gndx)%Devices(devndx)%maxWidth = val
    return
end subroutine

subroutine set_device_width_datasource(gndx, devndx, expr, val, timedep)
    use Gates, only: GateArray
    implicit none
    integer gndx, devndx
    integer expr
    real*8 val
    logical timedep
    call set_datasource( &
        GateArray(gndx)%Devices(devndx)%width_datasource, expr, val, timedep)
    return
end subroutine

real(8) function get_device_nduplicate(gndx, devndx)
    use Gates, only: GateArray
    implicit none
    integer gndx, devndx
    get_device_nduplicate = GateArray(gndx)%Devices(devndx)%nduplicate
    return
end function

subroutine set_device_nduplicate(gndx, devndx, val)
    use Gates, only: GateArray
    implicit none
    integer gndx, devndx
    real(8) val
    GateArray(gndx)%Devices(devndx)%nduplicate = nint(val)
    return
end subroutine

real(8) function get_device_elev(gndx, devndx)
    use Gates, only: GateArray
    implicit none
    integer gndx, devndx
    get_device_elev = GateArray(gndx)%Devices(devndx)%baseElev
    return
end function

subroutine set_device_elev(gndx, devndx, val)
    use Gates, only: GateArray
    implicit none
    integer gndx, devndx
    real(8) val
    GateArray(gndx)%Devices(devndx)%baseElev = val
    return
end subroutine

subroutine set_device_elev_datasource(gndx, devndx, expr, val, timedep)
    use Gates, only: GateArray
    implicit none
    integer gndx, devndx
    integer expr
    real*8 val
    logical timedep
    call set_datasource( &
        GateArray(gndx)%Devices(devndx)%elev_datasource, expr, val, timedep)
    return
end subroutine

real(8) function get_device_flow_coef(gndx, devndx, direct)
    use Gates, only: GateArray
    use IO_Units
    implicit none
    integer gndx, devndx, direct
    integer direct_to_node, direct_from_node
    if (direct .eq. direct_to_node()) then
        get_device_flow_coef = GateArray(gndx)%Devices(devndx)%flowCoefToNode
    else if (direct .eq. direct_from_node()) then
        get_device_flow_coef = GateArray(gndx)%Devices(devndx)%flowCoefFromNode
    else
        write (unit_error, *) "Flow direction not recognized in get_device_flow_coef"
        call exit(3)
    end if
    return
end function

subroutine set_device_flow_coef(gndx, devndx, direct, val)
    use Gates, only: GateArray
    use IO_Units
    implicit none
    integer gndx, devndx, direct
    integer direct_to_node, direct_from_node
    real(8) val
    if (direct .eq. direct_to_node()) then
        GateArray(gndx)%Devices(devndx)%flowCoefToNode = val
    else if (direct .eq. direct_from_node()) then
        GateArray(gndx)%Devices(devndx)%flowCoefFromNode = val
    else
        write (unit_error, *) "Flow direction not recognized in set_device_flow_coef"
        call exit(3)
    end if
    return
end subroutine

real*8 function value_from_inputpath(i)
    use iopath_data
    implicit none
    integer i
    value_from_inputpath = pathinput(i)%value
    return
end function

integer function ts_index(name)
    use iopath_data
    implicit none
    character*(*) name
    integer i
    ts_index = -1
    do i = 1, ninpaths
        if (trim(pathinput(i)%name) .eq. trim(name)) then
            ts_index = i
            return
        end if
    end do
end function

integer function qext_index(name)
    use grid_data
    use constants
    implicit none

    character*(*) name
    integer i
    qext_index = miss_val_i
    do i = 1, nqext
        if (qext(i)%name .eq. name) then
            qext_index = i
            return
        end if
    end do
end function

integer function transfer_index(name)
    use constants
    use grid_data
    implicit none

    character*(*) name
    integer i
    transfer_index = miss_val_i
    do i = 1, nobj2obj
        if (obj2obj(i)%name .eq. name) then
            transfer_index = i
            return
        end if
    end do
end function

real*8 function channel_length(intno)
    use grid_data
    implicit none
    integer intno
    channel_length = chan_geom(intno) .length
    return
end function
