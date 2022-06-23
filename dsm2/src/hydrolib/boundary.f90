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

module netbnd
    use network
    use tvd
    implicit none
    integer, save:: BndValues
    integer, save:: NetBndValUnit
    integer, save:: Channel(2*MaxChannels)
    integer, save :: StartSeconds
    integer, save :: bdt
    integer, save :: OldTime
    integer, save :: NewTime
    integer, save :: CurrentTime
    real*8, save :: Old(2*MaxChannels)
    real*8, save :: New(2*MaxChannels)
    real*8, save :: Current(2*MaxChannels)
    real*8, save ::StreamBndValue(2*MaxChannels)
    logical, save :: ReadBoundaryValues
!   Definitions:
!     BndValues - number of values to be read each time step.
!     NetBndValUnit - FORTRAN unit number.
!     Channel(i) - channel number, + upstream or - downstream end.
!     StartSeconds - starting elapse time, in seconds.
!     bdT - boundary-value time-series time increment.
!     OldTime - currently the time corresponding to oldest set of
!               boundary values in memory.
!     NewTime - currently the time corresponding to newest set of
!               boundary values in memory.
!     CurrentTime - current Network time, in seconds.
!     Old(i) - boundary values at OldTime.
!     New(i) - boundary values at NewTime.
!     Current(i) - boundary values at CurrentTime.
!     StreamBndValue(k) - boundary value computed from equation, possibly
!                         one upstream and one downstream value,
!                         sequentially, odd "k" are upstream,
!                         even are downstream.
!     ReadBoundaryValues - indicator,
!                          [.TRUE.] read values.
!                          [.FALSE.] don't read values.
!
!===== EOF netbnd =====================================================

contains

    !== Public (SetBoundaryValues) ================================================

    logical function SetBoundaryValuesFromData()
        use grid_data
        use network
        use dss
        use netcntrl, only: NetworkSeconds
        implicit none

        !   Purpose:  Master routine for setting user-supplied boundary values as
        !             well as object to object flows.

        !   Argument definitions:

        !   Routines by module:

        integer :: i                 ! loop index
        !   Programmed by: Lew DeLong
        !   Date:          Aug   1992
        !   Modified by:   Ralph Finch, Eli Ateljevich
        !   Last modified: April 2004

        !-----Implementation -----------------------------------------------------

        !-----update external and internal flows for last time step
        do i = 1, nqext
            qext(i)%prev_flow = qext(i)%flow
        end do

        do i = 1, nobj2obj
            obj2obj(i)%prev_flow = obj2obj(i)%flow
        end do

        !-----check if new data needs to be read from DSS for each interval group
        !-----fill in fourpt 'boundary' array from DSS buffer

        StreamBndValue = 0.0        ! initialize boundary objects and arrays to zero
        do i = 1, max_qext
            qext(i)%flow = 0.0
        end do

        !-----update the data stored in the inputpaths
        call UpdateTimeVaryingData

        !-----store the data in boundary objects
        call store_values

        !-----update boundary array with stage and external flows
        !-----call apply_boundary_values

        SetBoundaryValuesFromData = .true.

        return
    end function

    !== Public (UpdateTimeVaryingData) ================================================
    subroutine UpdateTimeVaryingData()
        use type_defs
        use iopath_data
        use dss
        use mod_readdss
        implicit none
        !   Module data:
        !include '../fixed/defs.f'

        integer :: i

        if (npthsin_min15 > 0) then
            call readtvd(max_inp_min, mins15, npthsin_min15, ptin_min15, &
                         datain_15min)
        end if

        if (npthsin_hour1 > 0) then
            call readtvd(max_inp_hour, hrs, npthsin_hour1, ptin_hour1, &
                         datain_1hour)
        end if

        if (npthsin_day1 > 0) then
            call readtvd(max_inp_day, dys, npthsin_day1, ptin_day1, &
                         datain_1day)
        end if

        if (npthsin_month1 > 0) then
            call readtvd(max_inp_month, mths, npthsin_month1, ptin_month1, &
                         datain_1month)
        end if

        if (npthsin_year1 > 0) then
            call readtvd(max_inp_year, yrs, npthsin_year1, ptin_year1, &
                         datain_1year)
        end if

        if (npthsin_irr > 0) then
            call readtvd(max_inp_irr, irrs, npthsin_irr, ptin_irr, &
                         datain_irr)
        end if

        do i = 1, ninpaths
            call get_inp_data(i) ! get input data from buffers
        end do

    end subroutine

    !== Public (store_values) ================================================
    subroutine store_values
        use Gates, only: gateArray, gate, ngate, setFree
        use IO_Units
        use grid_data
        use network

        use netcntrl
        use chconnec
        implicit none

        !-----Fill time-varying data arrays for FourPt, and fill external
        !-----flow structure for tidefile

        !-----local variables

        integer :: i
        integer :: j

        real*8 :: &
            fetch_data
        real*8 :: install
        type(gate), pointer :: currentGate

619     format(/'FREE_GATE value specified in time-varying input for gate ', a &
                /'at time ', a, &
                ' but either the free width or depth' &
                /'were not given in the input.')

620     format(/'Invalid value for gate operation encountered in path:' &
                /a &
                /'Value is: ', g10.3 &
                /'date/time: ', a)

630     format(/'CALC mode specified in input time series for gate ', a &
                /' device ', a, ' at time ', a, &
                /' but this gate has no operating rule affecting gate position')

        !-----object-to-object flow values
        do i = 1, nobj2obj
            !fixme: ask Ralph/Parviz if they know why this if statement is here
            !if (obj2obj(i)%datasource.indx_ptr .ne. 0) then
            ! todo: dangerous!!!! multiplication is just for debug
            obj2obj(i)%flow = fetch_data(obj2obj(i)%datasource)
            !endif
        end do

        !-----stage boundaries
        do i = 1, nstgbnd
            stgbnd(i)%value = fetch_data(stgbnd(i)%datasource)
        end do

        !-----external flows
        do i = 1, nqext
            qext(i)%flow = fetch_data(qext(i)%datasource)
        end do

        !-----gate controls
        do i = 1, nGate
            currentGate => gateArray(i)
            do j = 1, currentGate%nDevice
                currentGate%Devices(j)%opCoefToNode = fetch_data( &
                                                      currentGate%Devices(j)%op_to_node_datasource)
                currentGate%Devices(j)%opCoefFromNode = fetch_data( &
                                                        currentGate%Devices(j)%op_from_node_datasource)
                currentGate%Devices(j)%baseElev = fetch_data( &
                                                  currentGate%Devices(j)%elev_datasource)
                currentGate%Devices(j)%height = fetch_data( &
                                                currentGate%Devices(j)%height_datasource)
                currentGate%Devices(j)%maxWidth = fetch_data( &
                                                  currentGate%Devices(j)%width_datasource)
                install = fetch_data(currentGate%install_datasource)
                !            call setFree(currentGate,install.eq. 0.D0)
            end do
        end do

        return
    end subroutine

    ! todo: gate_free, fetching data from expression

    !== Public (ApplyBoundaryValues) ================================================

    subroutine ApplyBoundaryValues()
        use grid_data
        use constants
        use network

        use chconnec
        implicit none

        !   Purpose:  Move data from boundary objects to hydro arrays and data structures.
        integer :: intchan
        integer :: i
        integer :: obj_type
        integer :: node
        integer :: updown
        integer :: code !debug
        real(8) :: bvalue

        !-----object-to-object flows that involve nodes
        !-----(reservoir obj2obj flows are handled in the reservoir calcs)
        do i = 1, nobj2obj
            !--------from a node
            if (obj2obj(i)%from_obj%obj_type == obj_node) then
                intchan = obj2obj(i)%from_obj%hydrochan ! neg channel number -> downstream end
                intchan = node_geom(obj2obj(i)%from_obj%obj_no)%sumQchan
                updown = sign(1, intchan)
                intchan = abs(intchan)
                bvalue = obj2obj(i)%flow
                !-----------note sign: from flow is subtracted
                if (updown > 0) then
                    ! upstream end of channel connected to node
                    StreamBndValue(intchan*2 - 1) = &
                        StreamBndValue(intchan*2 - 1) - bvalue
                else
                    if (DownBoundaryCode(intchan) == 2) then ! external node
                        ! flow is at exterior node, and at downstream end,
                        ! invert sign of flow for 4pt convention
                        bvalue = -bvalue  ! todo: this is not very helpful, and doesn't match dsm2
                    end if
                    ! downstream end of channel connected to node
                    StreamBndValue(intchan*2) = &
                        StreamBndValue(intchan*2) - bvalue
                end if
            end if

            !--------to a node
            if (obj2obj(i)%to_obj%obj_type == obj_node) then
                intchan = obj2obj(i)%to_obj%hydrochan ! - channel number denotes downstream end connected
                intchan = node_geom(obj2obj(i)%to_obj%obj_no)%sumQchan
                updown = sign(1, intchan)
                intchan = abs(intchan)
                bvalue = obj2obj(i)%flow
                if (updown > 0) then ! upstream end of channel connected to node
                    StreamBndValue(intchan*2 - 1) = &
                        StreamBndValue(intchan*2 - 1) + bvalue
                else                ! downstream end of channel connected to node
                    if (DownBoundaryCode(intchan) == 2) then ! external node
                        ! flow is at exterior node, and at downstream end,
                        ! invert sign of flow for 4pt convention
                        bvalue = -bvalue  ! todo: this is not very helpful, and doesn't match dsm2
                    end if
                    StreamBndValue(intchan*2) = &
                        StreamBndValue(intchan*2) + bvalue
                end if
            end if
        end do

        do i = 1, nqext
            obj_type = qext(i)%attach_obj_type
            bvalue = qext(i)%flow
            if (obj_type == obj_node) then
                intchan = node_geom(qext(i)%attach_obj_no)%sumQChan
                if (intchan > 0) then
                    ! neg channel number denotes downstream end
                    StreamBndValue(intchan*2 - 1) = &
                        StreamBndValue(intchan*2 - 1) + bvalue
                    ! source/sink or stage boundary for channel
                else                ! downstream end of channel connected to node
                    intchan = -intchan
                    if (DownBoundaryCode(intchan) == 2) then ! external node
                        ! flow is at exterior node, and at downstream end,
                        ! invert sign of flow for 4pt convention
                        bvalue = -bvalue                !fixme: is qext correct?
                    end if
                    StreamBndValue(intchan*2) = StreamBndValue(intchan*2) + bvalue
                end if
            end if
        end do

        do i = 1, nstgbnd
            bvalue = stgbnd(i)%value
            node = stgbnd(i)%node
            if (node_geom(node)%nup > 0) then
                !upstream node is attached
                intchan = node_geom(node)%upstream(1)
                StreamBndValue(intchan*2 - 1) = bvalue
            else
                intchan = node_geom(node)%downstream(1)
                StreamBndValue(intchan*2) = bvalue
            end if
        end do

        return
    end subroutine

    real*8 function reservoir_source_sink_prev( &
        reservoir_no &
        , acct_ndx &
        )

        !-----Given a reservoir number, add the sources and sinks
        !-----from previous time step and return the value.
        use grid_data
        use constants
        implicit none

        !-----argument
        integer :: reservoir_no            ! accounting index, if 0 ignore [INPUT]
        integer :: acct_ndx            ! accounting index, if 0 ignore [INPUT]

        !-----includes

        !-----local variables
        integer :: &
            i                ! external/internal flow index
        integer :: qndx                ! external/internal flow index

        reservoir_source_sink_prev = 0.0

        !-----external flows
        if (acct_ndx == 0 .or. &
            acct_ndx == ALL_FLOWS .or. &
            acct_ndx == QEXT_FLOWS) then
            i = 1
            do while (res_geom(reservoir_no)%qext(i) > 0) ! todo: bad substitute for nqext
                qndx = res_geom(reservoir_no)%qext(i)
                reservoir_source_sink_prev = reservoir_source_sink_prev + &
                                             qext(qndx)%prev_flow
                i = i + 1
            end do
        end if

        !-----internal flows
        if (acct_ndx == 0 .or. &
            acct_ndx == ALL_FLOWS .or. &
            acct_ndx == QINT_FLOWS) then
            i = 1
            do while (res_geom(reservoir_no)%qinternal(i) /= 0)
                qndx = res_geom(reservoir_no)%qinternal(i)
                if (obj2obj(qndx)%from_obj%obj_type == obj_reservoir &
                    .and. obj2obj(qndx)%from_obj%obj_no == reservoir_no) then
                    ! from this reservoir
                    reservoir_source_sink_prev = reservoir_source_sink_prev - &
                                                 obj2obj(qndx)%prev_flow
                end if
                if (obj2obj(qndx)%to_obj%obj_type == obj_reservoir &
                    .and. obj2obj(qndx)%to_obj%obj_no == reservoir_no) then
                    reservoir_source_sink_prev = reservoir_source_sink_prev + &
                                                 obj2obj(qndx)%prev_flow
                end if
                i = i + 1
            end do
        end if

        return
    end function

    real*8 function reservoir_source_sink( &
        reservoir_no &
        , acct_ndx &
        )

        !-----Given a reservoir number, add the sources and sinks
        !-----to that reservoir and return the value
        use grid_data
        use constants
        implicit none

        !-----argument
        integer :: reservoir_no            ! accounting index, if 0 ignore [INPUT]
        integer :: acct_ndx            ! accounting index, if 0 ignore [INPUT]

        !-----includes

        !-----local variables
        integer :: &
            i                ! external/internal flow index
        integer :: qndx                ! external/internal flow index

        reservoir_source_sink = 0.0

        !-----external flows
        if (acct_ndx == 0 .or. &
            acct_ndx == ALL_FLOWS .or. &
            acct_ndx == QEXT_FLOWS) then
            i = 1
            do while (res_geom(reservoir_no)%qext(i) > 0) ! todo: unclear way looping res.nqext
                qndx = res_geom(reservoir_no)%qext(i)
                reservoir_source_sink = reservoir_source_sink + &
                                        qext(qndx)%flow
                i = i + 1
            end do
        end if

        !-----internal flows
        if (acct_ndx == 0 .or. &
            acct_ndx == ALL_FLOWS .or. &
            acct_ndx == QINT_FLOWS) then
            i = 1
            do while (res_geom(reservoir_no)%qinternal(i) > 0)
                !todo: unclear substitute for looping nqext
                qndx = res_geom(reservoir_no)%qinternal(i)
                if (obj2obj(qndx)%from_obj%obj_type == obj_reservoir &
                    .and. obj2obj(qndx)%from_obj%obj_no == reservoir_no) then
                    ! from reservoir
                    reservoir_source_sink = reservoir_source_sink - &
                                            obj2obj(qndx)%flow
                end if
                if (obj2obj(qndx)%to_obj%obj_type == obj_reservoir &
                    .and. obj2obj(qndx)%to_obj%obj_no == reservoir_no) then
                    ! to reservoir
                    reservoir_source_sink = reservoir_source_sink + &
                                            obj2obj(qndx)%flow
                end if
                i = i + 1
            end do
        end if

        return
    end function

    !== Public (UpstreamBoundaryValue) =====================================

    real*8 function UpstreamBoundaryValue()

        implicit none

        !   Purpose:  Return a boundary value, computed from a simple boundary
        !             equation, for the upstream end of the current channel.

        !   Arguments:

        !   Argument definitions:

        !   Local Variables:

        !   Routines by module:

        !   Programmed by: Lew DeLong
        !   Date:          February 1991
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        UpstreamBoundaryValue = StreamBndValue(2*Branch - 1)

        return
    end function

    !== Public (DownstreamBoundaryValue) ===================================

    real*8 function DownstreamBoundaryValue()

        implicit none

        !   Purpose:  Return a boundary value, computed from a simple boundary
        !             equation, for the downstream end of the current channel.

        !   Arguments:

        !   Argument definitions:

        !   Local Variables:

        !   Routines by module:

        !   Programmed by: Lew DeLong
        !   Date:          February 1991
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        DownstreamBoundaryValue = StreamBndValue(2*Branch)

        return
    end function
end module
