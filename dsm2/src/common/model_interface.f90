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

! Level of the operating rule log (0 off, 1 events, 2 + action values at every advance).
! The oprule_log_level scalar wins; otherwise print_level 4 and 5 or more give 1 and 2, and anything lower gives 0.
integer function get_oprule_log_level() bind(C, name="get_oprule_log_level")
    use logging, only: print_level, oprule_log_level
    use constants, only: miss_val_i
    implicit none
    if (oprule_log_level .ge. 0) then
        get_oprule_log_level = min(2, oprule_log_level)
    else if (print_level .eq. miss_val_i) then
        get_oprule_log_level = 0
    else
        get_oprule_log_level = max(0, min(2, print_level - 3))
    end if
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

integer function transfer_index(name, len) bind(C, name="transfer_index")
    use constants
    use grid_data
    implicit none

    character(kind=c_char), dimension(*) :: name
    integer(kind=c_size_t), value :: len
    integer i
    transfer_index = miss_val_i
    do i = 1, nobj2obj
        if (obj2obj(i)%name .eq. cstring_to_fstring(name, len)) then
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

! ---- Options of the operating rule log and the tide file gate state (SCALAR table).
! Defaults apply when a scalar is not set. See OPRULE_LOG_HDF5_PLAN.md section 10.

! Copies text into a C buffer with a terminating NUL; returns the number of characters copied.
integer function copy_to_cbuf(text, buf, buflen)
    implicit none
    character(len=*), intent(in) :: text
    character(kind=c_char), dimension(*) :: buf
    integer buflen
    integer n, i
    n = max(0, min(len_trim(text), buflen - 1))
    do i = 1, n
        buf(i) = text(i:i)
    end do
    buf(n + 1) = c_null_char
    copy_to_cbuf = n
    return
end function

integer function get_oprule_log_file(buf, buflen) bind(C, name="get_oprule_log_file")
    use logging, only: oprule_log_file
    implicit none
    character(kind=c_char), dimension(*) :: buf
    integer buflen
    get_oprule_log_file = copy_to_cbuf(oprule_log_file, buf, buflen)
    return
end function

! Also write the text log oprule_log.txt (a debugging copy of the events; default off).
integer function get_oprule_log_text() bind(C, name="get_oprule_log_text")
    use logging, only: oprule_log_text
    implicit none
    get_oprule_log_text = 0
    if (oprule_log_text .ge. 0) get_oprule_log_text = oprule_log_text
    return
end function

integer function get_oprule_log_devices() bind(C, name="get_oprule_log_devices")
    use logging, only: oprule_log_devices
    implicit none
    get_oprule_log_devices = 1
    if (oprule_log_devices .ge. 0) get_oprule_log_devices = oprule_log_devices
    return
end function

integer function get_oprule_log_context() bind(C, name="get_oprule_log_context")
    use logging, only: oprule_log_context
    implicit none
    get_oprule_log_context = 1
    if (oprule_log_context .ge. 0) get_oprule_log_context = oprule_log_context
    return
end function

real*8 function get_oprule_log_tol_op() bind(C, name="get_oprule_log_tol_op")
    use logging, only: oprule_log_tol_op
    implicit none
    get_oprule_log_tol_op = 0.001d0
    if (oprule_log_tol_op .ge. 0.d0) get_oprule_log_tol_op = oprule_log_tol_op
    return
end function

real*8 function get_oprule_log_tol_dim() bind(C, name="get_oprule_log_tol_dim")
    use logging, only: oprule_log_tol_dim
    implicit none
    get_oprule_log_tol_dim = 0.01d0
    if (oprule_log_tol_dim .ge. 0.d0) get_oprule_log_tol_dim = oprule_log_tol_dim
    return
end function

integer function get_oprule_log_trace_interval() bind(C, name="get_oprule_log_trace_interval")
    use logging, only: oprule_log_trace_interval
    implicit none
    get_oprule_log_trace_interval = 0
    if (oprule_log_trace_interval .ge. 0) get_oprule_log_trace_interval = oprule_log_trace_interval
    return
end function

real*8 function get_oprule_log_flush_hours() bind(C, name="get_oprule_log_flush_hours")
    use logging, only: oprule_log_flush_hours
    implicit none
    get_oprule_log_flush_hours = 24.d0
    if (oprule_log_flush_hours .gt. 0.d0) get_oprule_log_flush_hours = oprule_log_flush_hours
    return
end function

! Which gate state series go in the tide file: 0 off, 1 end, 2 mean, 3 both (default).
integer function get_tidefile_gate_state() bind(C, name="get_tidefile_gate_state")
    use iopath_data, only: tidefile_gate_state
    implicit none
    get_tidefile_gate_state = 3
    if (tidefile_gate_state .ge. 0) get_tidefile_gate_state = tidefile_gate_state
    return
end function

! Name of the hydro tide file as given in the IO_FILE table (empty if none).
integer function get_hydro_tidefile_name(buf, buflen) bind(C, name="get_hydro_tidefile_name")
    use constants
    use iopath_data, only: io_files
    implicit none
    character(kind=c_char), dimension(*) :: buf
    integer buflen
    get_hydro_tidefile_name = 0
    buf(1) = c_null_char
    if (io_files(hydro, io_hdf5, io_write)%use) then
        get_hydro_tidefile_name = copy_to_cbuf(io_files(hydro, io_hdf5, io_write)%filename, buf, buflen)
    end if
    return
end function

! ---- Gate tables and state for the operating rule log

integer function get_gate_count() bind(C, name="get_gate_count")
    use gates_data, only: nGate
    implicit none
    get_gate_count = nGate
    return
end function

integer function get_gate_device_count(gndx) bind(C, name="get_gate_device_count")
    use gates_data, only: GateArray
    implicit none
    integer gndx
    get_gate_device_count = GateArray(gndx)%nDevice
    return
end function

integer function get_gate_name(gndx, buf, buflen) bind(C, name="get_gate_name")
    use gates_data, only: GateArray
    implicit none
    integer gndx, buflen
    character(kind=c_char), dimension(*) :: buf
    get_gate_name = copy_to_cbuf(GateArray(gndx)%name, buf, buflen)
    return
end function

integer function get_device_name(gndx, devndx, buf, buflen) bind(C, name="get_device_name")
    use gates_data, only: GateArray
    implicit none
    integer gndx, devndx, buflen
    character(kind=c_char), dimension(*) :: buf
    get_device_name = copy_to_cbuf(GateArray(gndx)%Devices(devndx)%name, buf, buflen)
    return
end function

! 1 weir, 2 pipe
integer function get_device_structure_type(gndx, devndx) bind(C, name="get_device_structure_type")
    use gates_data, only: GateArray
    implicit none
    integer gndx, devndx
    get_device_structure_type = GateArray(gndx)%Devices(devndx)%structureType
    return
end function

! What a gate is connected to: external channel number or reservoir name, and external node number.
integer function get_gate_object_name(gndx, buf, buflen) bind(C, name="get_gate_object_name")
    use gates_data, only: GateArray
    use constants
    use grid_data, only: chan_geom, res_geom
    implicit none
    integer gndx, buflen
    character(kind=c_char), dimension(*) :: buf
    character(len=48) :: label
    label = ' '
    if (GateArray(gndx)%objConnectedType .eq. obj_channel) then
        write (label, '(a,i0)') 'channel ', chan_geom(GateArray(gndx)%objConnectedID)%chan_no
    else if (GateArray(gndx)%objConnectedType .eq. obj_reservoir) then
        label = 'reservoir ' // trim(res_geom(GateArray(gndx)%objConnectedID)%name)
    end if
    get_gate_object_name = copy_to_cbuf(label, buf, buflen)
    return
end function

integer function get_gate_node_id(gndx) bind(C, name="get_gate_node_id")
    use gates_data, only: GateArray
    use grid_data, only: node_id
    implicit none
    integer gndx
    get_gate_node_id = node_id(GateArray(gndx)%node)
    return
end function

! Connection of a gate: type (obj_channel or obj_reservoir), internal channel or reservoir number,
! computation point in the water body (channels) and at the node.
subroutine get_gate_connection(gndx, objtype, objid, compoint, nodecompoint) bind(C, name="get_gate_connection")
    use gates_data, only: GateArray
    implicit none
    integer gndx, objtype, objid, compoint, nodecompoint
    objtype = GateArray(gndx)%objConnectedType
    objid = GateArray(gndx)%objConnectedID
    compoint = GateArray(gndx)%objCompPoint
    nodecompoint = GateArray(gndx)%nodeCompPoint
    return
end subroutine

! Total flow through the gate as of the last solve (the missing value when the gate is uninstalled).
real*8 function get_gate_flow(gndx) bind(C, name="get_gate_flow")
    use gates_data, only: GateArray
    implicit none
    integer gndx
    get_gate_flow = GateArray(gndx)%flow
    return
end function

! Value of a gate device property as the solver uses it. prop: 1 op to node, 2 op from node, 3 height,
! 4 elevation, 5 width, 6 nDuplicate, 7 gate install (devndx ignored). Returns the missing value for
! another prop.
real*8 function get_device_property(gndx, devndx, prop) bind(C, name="get_device_property")
    use gates_data, only: GateArray
    use constants, only: miss_val_r
    implicit none
    integer gndx, devndx, prop
    get_device_property = miss_val_r
    select case (prop)
    case (1)
        get_device_property = GateArray(gndx)%Devices(devndx)%opCoefToNode
    case (2)
        get_device_property = GateArray(gndx)%Devices(devndx)%opCoefFromNode
    case (3)
        get_device_property = GateArray(gndx)%Devices(devndx)%height
    case (4)
        get_device_property = GateArray(gndx)%Devices(devndx)%baseElev
    case (5)
        get_device_property = GateArray(gndx)%Devices(devndx)%maxWidth
    case (6)
        get_device_property = GateArray(gndx)%Devices(devndx)%nDuplicate
    case (7)
        get_device_property = 1.d0
        if (GateArray(gndx)%free) get_device_property = 0.d0
    end select
    return
end function

! Source of a gate device property (same prop numbers as get_device_property): 0 none, 1 constant,
! 2 time series, 3 expression. buf receives the series name or an expression label.
integer function get_device_source(gndx, devndx, prop, buf, buflen) bind(C, name="get_device_source")
    use gates_data, only: GateArray
    use constants
    use iopath_data, only: pathinput
    use type_defs, only: datasource_t
    implicit none
    integer gndx, devndx, prop, buflen
    character(kind=c_char), dimension(*) :: buf
    type(datasource_t) :: src
    character(len=48) :: label
    integer n
    get_device_source = 0
    buf(1) = c_null_char
    select case (prop)
    case (1)
        src = GateArray(gndx)%Devices(devndx)%op_to_node_datasource
    case (2)
        src = GateArray(gndx)%Devices(devndx)%op_from_node_datasource
    case (3)
        src = GateArray(gndx)%Devices(devndx)%height_datasource
    case (4)
        src = GateArray(gndx)%Devices(devndx)%elev_datasource
    case (5)
        src = GateArray(gndx)%Devices(devndx)%width_datasource
    case (6)
        src = GateArray(gndx)%Devices(devndx)%nduplicate_datasource
    case (7)
        src = GateArray(gndx)%install_datasource
    case default
        return
    end select
    if (src%source_type .eq. const_data) then
        get_device_source = 1
    else if (src%source_type .eq. dss_data) then
        get_device_source = 2
        n = copy_to_cbuf(pathinput(src%indx_ptr)%name, buf, buflen)
    else if (src%source_type .eq. expression_data) then
        get_device_source = 3
        write (label, '(a,i0,a)') 'expression(index=', src%indx_ptr, ')'
        n = copy_to_cbuf(label, buf, buflen)
    end if
    return
end function

end module model_interface