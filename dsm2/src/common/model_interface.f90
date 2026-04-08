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
module model_interface
    use iso_c_binding
    use utilities, only: cstring_to_fstring
    implicit none

    interface
        real*8 function get_expression_data(expr) bind(C, name="get_expression_data")
            integer :: expr
        end function
    end interface
contains
integer function getModelTime() bind(C, name="get_model_time")
    use runtime_data
    implicit none
    getModelTime = julmin
    return
end function

integer function getModelJulianDay() bind(C, name="get_model_julian_day")
    implicit none
    integer, parameter :: MIN_PER_DAY = 60*24
    getModelJulianDay = getModelTime()/MIN_PER_DAY
    return
end function

integer function getModelYear() bind(C, name="get_model_year")
    implicit none
    integer m, d, y
    call jliymd(getModelJulianDay(), y, m, d)
    getModelYear = y
    return
end function

integer function getModelMonth() bind(C, name="get_model_month")
    implicit none
    integer m, d, y
    call jliymd(getModelJulianDay(), y, m, d)
    getModelMonth = m
    return
end function

integer function getModelDay() bind(C, name="get_model_day")
    implicit none
    integer m, d, y
    call jliymd(getModelJulianDay(), y, m, d)
    getModelDay = d
    return
end function

integer function getModelDayOfYear() bind(C, name="get_model_day_of_year")
    implicit none
    integer m, d, y, yearstart, jday
    integer iymdjl
    jday = getModelJulianDay()
    call jliymd(jday, y, m, d)
    yearstart = iymdjl(y, 1, 1)
    getModelDayOfYear = jday - yearstart
    return
end function

integer function getModelMinuteOfDay() bind(C, name="get_model_minute_of_day")
    use runtime_data
    implicit none
    integer, parameter :: MIN_PER_DAY = 60*24
    getModelMinuteOfDay = mod(julmin, MIN_PER_DAY)
    return
end function

integer function getModelHour() bind(C, name="get_model_hour")
    use runtime_data
    implicit none
    integer, parameter :: MIN_PER_HOUR = 60
    getModelHour = (getModelMinuteOfDay()/MIN_PER_HOUR)
    return
end function

integer function getReferenceMinuteOfYear(mon, day, hour, min) bind(C, name="get_reference_minute_of_year")
    implicit none

    integer iymdjl  ! function converts ymd to julian day
    integer yr, dayyr, jday, mon, day, hour, min, yearstart
    integer, parameter :: MIN_PER_DAY = 60*24

    yr = getModelYear()
    jday = iymdjl(yr, mon, day)
    yearstart = iymdjl(yr, 1, 1)
    dayyr = jday - yearstart
    getReferenceMinuteOfYear = MIN_PER_DAY*dayyr + 60*hour + min
    return
end function

integer function getModelMinuteOfYear() bind(C, name="get_model_minute_of_year")
    implicit none
    integer, parameter :: MIN_PER_DAY = 60*24
    getModelMinuteOfYear = MIN_PER_DAY*getModelDayOfYear() + getModelMinuteOfDay()
    return
end function

integer function getModelMinute() bind(C, name="get_model_minute")
    implicit none
    integer, parameter :: MIN_PER_HOUR = 60
    getModelMinute = mod(getModelMinuteOfDay(), MIN_PER_HOUR)
    return
end function

integer function getModelTicks() bind(C, name="get_model_ticks")
    use runtime_data
    implicit none

    getModelTicks = julmin;
    return
end function

subroutine set_datasource(source, expr, val, timedep) bind(C, name="set_datasource")
    use constants
    use type_defs, only: datasource_t
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


subroutine chan_comp_point(intchan, distance, &
                           comp_points, weights) bind(C, name="chan_comp_point")

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

integer function resNdx(name, len) bind(C, name="reservoir_index")
    use grid_data
    implicit none
    integer i
    character(kind=c_char), dimension(*) :: name
    integer(kind=c_size_t), value :: len
    character(len=len) :: f_string
    resNdx = miss_val_i
    f_string = cstring_to_fstring(name, len)
    call locase(f_string)
    do i = 1, nreser
        if (res_geom(i)%name .eq. trim(f_string)) then
            resNdx = i
            exit
        end if
    end do
    return
end function

integer function resConnectNdx(res_ndx, internal_node_no) bind(C, name="reservoir_connect_index")
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

integer function gateNdx(name, len) bind(C, name="gate_index")
    use gates_data, only: gateArray, nGate
    use constants
    implicit none

    character(kind=c_char), dimension(*) :: name
    integer(kind=c_size_t), value :: len
    integer i
    character(len=len) :: f_string
    f_string = cstring_to_fstring(name, len)
    gateNdx = miss_val_i
    call locase(f_string)
    do i = 1, nGate
        if (f_string .eq. GateArray(i)%name) then
            gateNdx = i
            exit
        end if
    end do
    return
end function

integer function deviceNdx(gatendx, c_devname, len) bind(C, name="device_index")
    use gates_data, only: GateArray
    use gates, only: deviceIndex
    implicit none
    integer gatendx
    character(kind=c_char), dimension(*) :: c_devname
    integer(kind=c_size_t), value :: len
    character*32 ldevname

    ldevname = cstring_to_fstring(c_devname, len)
    call locase(ldevname)
    deviceNdx = deviceIndex(GateArray(gatendx), ldevname)
    return
end function

!     return the constant, so FORTRAN and C can share it
integer function direct_to_node() bind(C, name="direct_to_node")
    use gates_data
    implicit none
    direct_to_node = FLOW_COEF_TO_NODE
    return
end function

!     return the constant, so FORTRAN and C can share it
integer function direct_from_node() bind(C, name="direct_from_node")
    use gates_data
    implicit none
    direct_from_node = FLOW_COEF_FROM_NODE
    return
end function

!     return the constant, so FORTRAN and C can share it
integer function direct_to_from_node() bind(C, name="direct_to_from_node")
    use gates_data
    implicit none
    direct_to_from_node = FLOW_COEF_TO_FROM_NODE
    return
end function

real*8 function get_external_flow(ndx) bind(C, name="get_external_flow")
    use grid_data
    implicit none

    integer ndx
    get_external_flow = qext(ndx)%flow
    return
end function

subroutine set_external_flow(ndx, val) bind(C, name="set_external_flow")
    use grid_data
    implicit none

    integer ndx
    real*8 val
    qext(ndx)%flow = val
    return
end subroutine

subroutine set_external_flow_datasource(ndx, expr, val, timedep) bind(C, name="set_external_flow_datasource")
    use grid_data
    implicit none
    integer ndx, expr
    real*8 val
    logical timedep
    call set_datasource(qext(ndx)%datasource, expr, val, timedep)
    return
end subroutine

real*8 function get_transfer_flow(ndx) bind(C, name="get_transfer_flow")
    use grid_data
    implicit none
    integer ndx
    get_transfer_flow = obj2obj(ndx)%flow
    return
end function

subroutine set_transfer_flow(ndx, val) bind(C, name="set_transfer_flow")
    use grid_data
    implicit none
    integer ndx
    real*8 val
    obj2obj(ndx)%flow = val
    return
end subroutine

subroutine set_transfer_flow_datasource(ndx, expr, val, timedep) bind(C, name="set_transfer_flow_datasource")   
    use grid_data
    implicit none
    integer ndx, expr
    real*8 val
    logical timedep
    call set_datasource(obj2obj(ndx)%datasource, expr, val, timedep)
    return
end subroutine

subroutine set_gate_install(ndx, install) bind(C, name="set_gate_install")
    use gates_data, only: GateArray
    use gates, only: setFree
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

subroutine set_gate_install_datasource(gndx, expr, val, timedep) bind(C, name="set_gate_install_datasource")
    use gates_data, only: GateArray
    implicit none
    integer gndx
    integer expr
    real*8 val
    logical timedep
    call set_datasource( &
        GateArray(gndx)%install_datasource, expr, val, timedep)
    return
end subroutine

real*8 function is_gate_install(ndx) bind(C, name="is_gate_install")
    use gates_data, only: GateArray
    implicit none
    integer ndx
    if (GateArray(ndx)%free) then
        is_gate_install = 0.0
    else
        is_gate_install = 1.0
    end if
    return
end function

real(8) function get_device_op_coef(gndx, devndx, direction) bind(C, name="get_device_op_coef")
    use gates_data, only: GateArray
    implicit none
    integer gndx, devndx, direction
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

subroutine set_device_op_coef(gndx, devndx, direction, val) bind(C, name="set_device_op_coef")
    use gates_data, only: GateArray
    implicit none
    integer gndx, devndx, direction
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

subroutine set_device_op_datasource(gndx, devndx, direction, expr, val, timedep) bind(C, name="set_device_op_datasource")
    use gates_data, only: GateArray
    implicit none
    integer gndx, devndx, direction
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

real(8) function get_device_height(gndx, devndx) bind(C, name="get_device_height")
    use gates_data, only: GateArray
    implicit none
    integer gndx, devndx
    get_device_height = GateArray(gndx)%Devices(devndx)%height
    return
end function

subroutine set_device_height(gndx, devndx, val) bind(C, name="set_device_height")
    use gates_data, only: GateArray
    implicit none
    integer gndx, devndx
    real(8) val
    GateArray(gndx)%Devices(devndx)%height = val
end subroutine

subroutine set_device_height_datasource(gndx, devndx, expr, val, timedep) bind(C, name="set_device_height_datasource")
    use gates_data, only: GateArray
    implicit none
    integer gndx, devndx
    integer expr
    real*8 val
    logical timedep
    call set_datasource( &
        GateArray(gndx)%Devices(devndx)%height_datasource, expr, val, timedep)
    return
end subroutine

real(8) function get_device_width(gndx, devndx) bind(C, name="get_device_width")
    use gates_data, only: GateArray
    implicit none
    integer gndx, devndx
    get_device_width = GateArray(gndx)%Devices(devndx)%maxWidth
    return
end function

subroutine set_device_width(gndx, devndx, val) bind(C, name="set_device_width")
    use gates_data, only: GateArray
    implicit none
    integer gndx, devndx
    real(8) val
    GateArray(gndx)%Devices(devndx)%maxWidth = val
    return
end subroutine

subroutine set_device_width_datasource(gndx, devndx, expr, val, timedep) bind(C, name="set_device_width_datasource")
    use gates_data, only: GateArray
    implicit none
    integer gndx, devndx
    integer expr
    real*8 val
    logical timedep
    call set_datasource( &
        GateArray(gndx)%Devices(devndx)%width_datasource, expr, val, timedep)
    return
end subroutine

real(8) function get_device_nduplicate(gndx, devndx) bind(C, name="get_device_nduplicate")
    use gates_data, only: GateArray
    implicit none
    integer gndx, devndx
    get_device_nduplicate = GateArray(gndx)%Devices(devndx)%nduplicate
    return
end function

subroutine set_device_nduplicate(gndx, devndx, val) bind(C, name="set_device_nduplicate")
    use gates_data, only: GateArray
    implicit none
    integer gndx, devndx
    real(8) val
    GateArray(gndx)%Devices(devndx)%nduplicate = nint(val)
    return
end subroutine

subroutine set_device_nduplicate_datasource(gndx, devndx, expr, val, timedep) bind(C, name="set_device_nduplicate_datasource")
    use gates_data, only: GateArray
    implicit none
    integer gndx, devndx
    integer expr
    real*8 val
    logical(kind=c_bool), value :: timedep
    call set_datasource( &
        GateArray(gndx)%Devices(devndx)%nduplicate_datasource, expr, val, timedep /= 0_c_bool)
    return
end subroutine

real(8) function get_device_elev(gndx, devndx) bind(C, name="get_device_elev")
    use gates_data, only: GateArray
    implicit none
    integer gndx, devndx
    get_device_elev = GateArray(gndx)%Devices(devndx)%baseElev
    return
end function

subroutine set_device_elev(gndx, devndx, val) bind(C, name="set_device_elev")
    use gates_data, only: GateArray
    implicit none
    integer gndx, devndx
    real(8) val
    GateArray(gndx)%Devices(devndx)%baseElev = val
    return
end subroutine

subroutine set_device_elev_datasource(gndx, devndx, expr, val, timedep) bind(C, name="set_device_elev_datasource")
    use gates_data, only: GateArray
    implicit none
    integer gndx, devndx
    integer expr
    real*8 val
    logical timedep
    call set_datasource( &
        GateArray(gndx)%Devices(devndx)%elev_datasource, expr, val, timedep)
    return
end subroutine

real(8) function get_device_flow_coef(gndx, devndx, direct) bind(C, name="get_device_flow_coef")
    use gates_data, only: GateArray
    use IO_Units
    implicit none
    integer gndx, devndx, direct
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

subroutine set_device_flow_coef(gndx, devndx, direct, val) bind(C, name="set_device_flow_coef")
    use gates_data, only: GateArray
    use IO_Units
    implicit none
    integer gndx, devndx, direct
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

real*8 function value_from_inputpath(i) bind(C, name="value_from_inputpath")
    use iopath_data
    implicit none
    integer i
    value_from_inputpath = pathinput(i)%value
    return
end function

integer function ts_index(c_str, len) bind(C, name="ts_index")
    use iopath_data
    implicit none
    character(kind=c_char), dimension(*) :: c_str
    integer(kind=c_size_t), value :: len

    character(len=len) :: f_string
    integer :: i

    f_string = cstring_to_fstring(c_str, len)
    ts_index = -1
    do i = 1, ninpaths
        if (trim(pathinput(i)%name) .eq. trim(f_string)) then
            ts_index = i
            return
        end if
    end do
end function

integer function qext_index(name, len) bind(C, name="qext_index")
    use grid_data
    use constants
    implicit none

    character(kind=c_char), dimension(*) :: name
    integer(kind=c_size_t), value :: len

    integer i
    character(len=len) :: f_string
    f_string = cstring_to_fstring(name, len)
    qext_index = miss_val_i
    do i = 1, nqext
        if (qext(i)%name .eq. f_string) then
            qext_index = i
            return
        end if
    end do
end function

integer function transfer_index(name) bind(C, name="transfer_index")
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

real*8 function channel_length(intno) bind(C, name="channel_length")
    use grid_data
    implicit none
    integer intno
    channel_length = chan_geom(intno) .length
    return
end function

real*8 function fetch_data(source) bind(C, name="fetch_data")
      use constants
      use iopath_data
      use type_defs, only: datasource_t
      implicit none
!----- Fetch time varying data from a data source such as
!      DSS, an expression or a constant value

    type(datasource_t) ::  source

      if (source.source_type .eq. const_data)  then
       fetch_data=source.value
      else if (source.source_type .eq. dss_data) then   !fetch from dss path
        fetch_data=pathinput(source.indx_ptr).value
      else if (source.source_type .eq. expression_data) then
        fetch_data=get_expression_data(source.indx_ptr)
    else
      fetch_data=miss_val_r
    end if
    return
    end function

end module model_interface