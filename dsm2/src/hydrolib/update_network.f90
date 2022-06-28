module update_network
    implicit none
contains
    !== Public (UpdateNetwork) =============================================

    logical function UpdateNetwork()
        use Gates, only: NGate, GateArray

        use IO_Units
        use grid_data
        use network
        use netcntrl
        use chconnec
        use solver
        use chstatus
        use netbnd, only: SetBoundaryValuesFromData, ApplyBoundaryValues
        use channel_schematic &
            , only: NumberOfChannels, OpenChannel, CloseChannel
        use gate_calc, only: CalculateGateFlow
        use reservoirs, only: CalculateReservoirFlow

        implicit none

        !   Purpose:  Determine values of discharge and water surface elevation
        !             within a network of open channels at a new time.

        !   Arguments:

        !   Argument Definitions:

        !   Local variables:
        integer ChannelNumber
        logical OK &
            , ClosedIteration     ! indicator that iteration has been closed
        real*8 increment
        !   Routines by module:

        !   Programmed by: Lew DeLong
        !   Date:          February 1991
        !   Modified by:   Lew DeLong
        !   Last modified: December 1992
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        UpdateNetwork = .false.
        increment = dble(NetworkTimeIncrement())
        !-----Load data from time series to boundary objects
        OK = SetBoundaryValuesFromData()

        !-----Advance gate operating rules, which may alter boundary values
        call AdvanceOpRuleActions(increment)

        !-----Boundary values are final, apply to mdel
        call ApplyBoundaryValues()

        !-----Initialize (to zero) known part of right hand side vector and
        !-----elapsed iterations

        ClosedIteration = .false.
        Iteration = 0
        Rescale = 1.D0
        XOld = 0.0

        do while (.not. ClosedIteration)
!--------Begin iteration loop and clear matrix
            OK = IncrementNetworkIteration()
            if (OK) then
                OK = InitializeMatrix()
            end if

            !--------Set iteration-specific part of
            !        right hand side to zero
            XAdj = 0.

            if (NGate > 0) then
                !-----------Calculate flow through all the gates
                OK = CalculateGateFlow()
            end if

            do ChannelNumber = 1, NumberOfChannels()
                if (OpenChannel(ChannelNumber)) then
                    if (UpdateChannel()) then
                        OK = CloseChannel()
                    else
                        write (UNIT_ERROR, *) ' Update of channel', &
                            chan_geom(ChannelNumber)%chan_no, ' failed...'
                        return
                    end if
                else
                    OK = CloseChannel()
                end if

            end do

            if (Nreser > 0) then
                !-----------Calculate Reservoir flows
                OK = CalculateReservoirFlow()
            end if

            !--------Solve for incremental change in dependent variables.

            if (SolveFourPt()) then
            else
                write (UNIT_ERROR, *) ' *** Error (UpdateNetwork)'
                write (UNIT_ERROR, *) ' Failed to solve matrix...'
                return
            end if

            !--------Update dependent variables and check for closure.
            closediteration = NetworkClosure()
        end do

        !-----End iteration loop.
        OK = CloseNetworkIteration()

        !-----Test operating rules for activation/finalization
        call StepOpRuleExpressions(increment)
        call TestOpRuleActivation(increment)

        UpdateNetwork = .true.

        return
    end function

    logical function UpdateNetworkPrepare()
        use netcntrl, only: NetworkTimeIncrement
        use netbnd, only: SetBoundaryValuesFromData, ApplyBoundaryValues
        implicit none

        real*8 increment
        logical OK

        UpdateNetworkPrepare = .false.
        increment = dble(NetworkTimeIncrement())
        !-----Load data from time series to boundary objects
        OK = SetBoundaryValuesFromData()

        !-----Advance gate operating rules, which may alter boundary values
        call AdvanceOpRuleActions(increment)

        !-----Boundary values are final, apply to mdel
        call ApplyBoundaryValues()
    end function

    logical function UpdateNetworkWrapup()
        use netcntrl, only: NetworkTimeIncrement, CloseNetworkIteration
        implicit none

        logical OK
        real*8 increment

        increment = dble(NetworkTimeIncrement())
        !-----End iteration loop.
        OK = CloseNetworkIteration()

        !-----Test operating rules for activation/finalization
        call StepOpRuleExpressions(increment)
        call TestOpRuleActivation(increment)

        UpdateNetworkWrapup = .true.
    end function

    subroutine UpdateNetworkLoop()
        use netcntrl, only: Iteration, IncrementNetworkIteration
        use solver ! , only: Rescale, XOld
        use grid_data, only: chan_geom, nreser
        use channel_schematic &
            , only: NumberOfChannels, OpenChannel, CloseChannel
        use Gates, only: NGate
        use gate_calc, only: CalculateGateFlow
        use reservoirs, only: CalculateReservoirFlow
        use IO_Units

        implicit none

        logical OK, ClosedIteration
        integer ChannelNumber

        !-----Initialize (to zero) known part of right hand side vector and
        !-----elapsed iterations
        ClosedIteration = .false.
        Iteration = 0
        Rescale = 1.D0
        XOld = 0.0

        do while (.not. ClosedIteration)
!--------Begin iteration loop and clear matrix
            OK = IncrementNetworkIteration()
            if (OK) then
                OK = InitializeMatrix()
            end if

            !--------Set iteration-specific part of
            !        right hand side to zero
            XAdj = 0.

            if (NGate > 0) then
                !-----------Calculate flow through all the gates
                OK = CalculateGateFlow()
            end if

            do ChannelNumber = 1, NumberOfChannels()
                if (OpenChannel(ChannelNumber)) then
                    if (UpdateChannel()) then
                        OK = CloseChannel()
                    else
                        write (UNIT_ERROR, *) ' Update of channel', &
                            chan_geom(ChannelNumber)%chan_no, ' failed...'
                        return
                    end if
                else
                    OK = CloseChannel()
                end if

            end do

            if (Nreser > 0) then
                !-----------Calculate Reservoir flows
                OK = CalculateReservoirFlow()
            end if

            !--------Solve for incremental change in dependent variables.

            if (SolveFourPt()) then
            else
                write (UNIT_ERROR, *) ' *** Error (UpdateNetwork)'
                write (UNIT_ERROR, *) ' Failed to solve matrix...'
                return
            end if

            !--------Update dependent variables and check for closure.
            closediteration = NetworkClosure()
        end do
    end subroutine

    !== Public (UpdateChannel) =============================================

    logical function UpdateChannel()
        use IO_Units
        use channel_schematic, only: UpstreamPointer, DownstreamPointer
        use floweq1d, only: DynamicWaveEq, DynamicWaveEqDS
        use netcntrl, only: VariableStreamSinuosity
        implicit none

        !   Purpose:  Update dependent variables defining a channel.  In this
        !             implementation, dependent variables are flow and water-
        !             surface elevation.

        !   Arguments:

        !   Argument definitions:

        !   Module data:

        !   Local variables:
        integer Node, I

        !   Routines by module:

        !   Programmed by: Lew DeLong
        !   Date:          February 1991
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        UpdateChannel = .true.

        !-----Compute and store constraint-equation coefficients for
        !      upstream end of channel.

        if (SetConstraint(+1)) then
        else
            UpdateChannel = .false.
            write (UNIT_ERROR, *) ' *** Error (UpdateChannel)'
            write (UNIT_ERROR, *) ' Attempt to set upstream constraint failed...'
        end if

        !-----Compute and store intermediate general-equation coeficients for ...

        Node = 0

        if (.not. VariableStreamSinuosity()) then

            !--------Constant sinuosity and water density.

            do 100 I = UpstreamPointer(), DownstreamPointer() - 1
                Node = Node + 1
                if (.not. DynamicWaveEq(Node, Node + 1)) then
                    UpdateChannel = .false.
                    write (UNIT_ERROR, *) ' *** Error (UpdateChannel)'
                    write (UNIT_ERROR, *) ' Failed to update node...', Node
                end if
100             continue

                else

                !--------Variable sinuosity and water density.

                do 150 I = UpstreamPointer(), DownstreamPointer() - 1
                    Node = Node + 1
                    if (DynamicWaveEqDS(Node, Node + 1)) then
                    else
                        UpdateChannel = .false.
                        write (UNIT_ERROR, *) ' *** Error (UpdateChannel)'
                        write (UNIT_ERROR, *) ' Failed to update node...', Node
                    end if
150                 continue

                    end if

                    !-----Compute and store constraint-equation coefficients for
                    !      downstream end of channel.

                    if (SetConstraint(-1)) then
                    else
                        UpdateChannel = .false.
                        write (UNIT_ERROR, *) ' *** Error (UpdateChannel)'
                        write (UNIT_ERROR, *) ' Attempt to set downstream constraint failed...'
                    end if

                    return
                    end function

                    !== Public (SetConstraint) =====================================

                    logical function SetConstraint(Extremity)
                        use IO_Units
                        use channel_constraints, only: ForceStreamSurface, ForceStreamFlow, &
                                                       ForceSumStreamFlow, ForceEqualStreamSurface
                        use channel_schematic, only: UpstreamCode, DownStreamCode

                        implicit none

                        !   Purpose:  Select and compute appropriate channel
                        !             constraint-equation
                        !             for one end of channel
                        !   Arguments:

                        !   Argument definitions:

                        !   Local Variables:
                        integer Code, Extremity, ConstraintCode
                        logical OK

                        !   Programmed by: Lew DeLong
                        !   Date:          February 1991
                        !   Modified by:   Lew DeLong
                        !   Last modified: December 1992
                        !   Version 93.01, January, 1993

                        !-----Implementation -----------------------------------------------------

                        SetConstraint = .false.

                        !-----Boundary-condition code key
                        !
                        !     Last Digit = 1 : Upstream
                        !                  2 : Downstream
                        !
                        !
                        !       water surface  * =implemented in DSM2
                        !          1,2*               explicitly known
                        !          4                  self-setting (downstream only)
                        !         11,12*              equal to another water surface
                        ! ........31,32               3-parameter relation
                        ! ........41,42               (reserved for four-parameter rating)
                        ! ........51,52*              user-defined (e.g. gate)
                        !
                        !         Note: User-defined codes can have four digits, last
                        !         two of which will be 51 or 52
                        !****-
                        !-----Set channel-extremity code to upstream.

                        if (Extremity == 1) then
                            ConstraintCode = UpstreamCode()
                        else if (Extremity == -1) then
                            ConstraintCode = DownstreamCode()
                        else
                            write (UNIT_ERROR, *) ' *** Error (SetConstraint)'
                            write (UNIT_ERROR, *) ' Inappropriate extremity code (not +1/-1).'
                            return
                        end if

                        !-----Get upstream boundary-condition code.
                        Code = MOD(ConstraintCode, 100)
                        OK = .false.

                        !-----Set appropriate constraint.
                        if (Code == 1) then
                            if (ForceStreamSurface(Extremity)) then
                            else
                                return
                            end if
                        else if (Code == 2) then
                            if (ForceStreamFlow(Extremity)) then
                            else
                                return
                            end if

                        else if (Code == 11) then
                            if (ForceEqualStreamSurface(Extremity)) then
                            else
                                return
                            end if

                        else if (Code == 12) then
                            if (ForceSumStreamFlow(Extremity)) then
                            else
                                return
                            end if

                        else if (Code == 51 .or. Code == 52) then
                            if (IABS(MOD(ConstraintCode/100, 100)) == 4) then
                            else
                                return
                            end if
                        else
                            write (UNIT_ERROR, *) ' *** Error (SetConstraint)'
                            write (UNIT_ERROR, *) ' Condition code ', Code, ' not implemented.'
                            return

                        end if

                        SetConstraint = .true.

                        return
                    end function

                    !== Public (NetworkClosure) ============================================

                    logical function NetworkClosure()
                        use Gates, only: NGate, GateArray
                        use grid_data
                        use network
                        use netcntrl
                        use chconnec
                        use chstatus, only: SetStreamSurfaceElevation, SetStreamFlow, &
                                            StreamFlow, StreamSurfaceElevation
                        use channel_schematic, only: NumberOfStreamLocations, NumberOfChannels, &
                                                     OpenChannel, CloseChannel
                        use solveutil, only: IncrementalZ, IncrementalQ
                        use solver, only: ResEqRow, X, Rescale, NormClose
                        implicit none

                        !   Purpose:  Check for closure of iteration on a set of equations
                        !             describing flow in a network of open channels, and
                        !             update dependent variables.

                        !   Arguments:

                        !   Argument definitions:

                        !   Module data:

                        !   Local Variables:
                        integer I, J
                        integer ChannelQ, NodeQ, ChannelZ, NodeZ
                        integer ChannelNumber, Node
                        real*8 SmallQ
                        parameter(SmallQ=1000.0)
                        real*8 Change, CurrentValue, CurrentRatioQ, CurrentChangeZ
                        real*8 MaxChangeZ, MaxRatioQ
                        logical OK

                        !   Programmed by: Lew DeLong
                        !   Date:          February 1991
                        !   Modified by:   Lew DeLong
                        !   Last modified: December 1992
                        !   Version 93.01, January, 1993

                        !-----Implementation -----------------------------------------------------

                        MaxRatioQ = 0.0
                        MaxChangeZ = 0.0
                        MaxChangeQ = 0.0

                        do 200 I = 1, NumberOfChannels()

                            ChannelNumber = I

                            if (OpenChannel(ChannelNumber)) then

                                do 100 J = 1, NumberOfStreamLocations()
                                    Node = J

                                    !--------------Check for closure on flow.

                                    Change = IncrementalQ(Node)
                                    CurrentValue = StreamFlow(Node)

                                    if (ABS(CurrentValue) > SmallQ) then
                                        !-----------------Flow is greater than a small flow.
                                        CurrentRatioQ = ABS(Change/CurrentValue)
                                    else
                                        !-----------------Flow is smaller than a small flow.
                                        CurrentRatioQ = ABS(Change/SmallQ)
                                    end if

                                    if (ABS(Change) > MaxChangeQ) then
                                        MaxChangeQ = ABS(Change)
                                        MaxChangeChannelQ = ChannelNumber
                                    end if
                                    if (CurrentRatioQ > MaxRatioQ) then
                                        MaxRatioQ = CurrentRatioQ
                                        NodeQ = Node
                                        ChannelQ = ChannelNumber
                                    end if

                                    !--------------Update flow.
                                    OK = SetStreamFlow(Node, CurrentValue + Change)

                                    !--------------Check for closure on water-surface elevation.

                                    Change = IncrementalZ(Node)
                                    CurrentChangeZ = ABS(Change)
                                    CurrentValue = StreamSurfaceElevation(Node)

                                    if (CurrentChangeZ > MaxChangeZ) then
                                        MaxChangeZ = CurrentChangeZ
                                        NodeZ = Node
                                        ChannelZ = ChannelNumber
                                    end if

                                    !--------------Update water-surface elevation.
                                    OK = SetStreamSurfaceElevation(Node, CurrentValue + Change)

100                                 continue

                                    OK = CloseChannel()

                                    else
                                    end if

200                                 continue

                                    do i = 1, Nreser
                                        !--------Adjust reservoir height from iteration
                                        !     channel Z is already adjusted
                                        Change = X(ResEqRow(i))
                                        Yres(i) = Yres(i) + Change
                                        do j = 1, res_geom(i)%nnodes
                                            QRes(i, j) = QRes(i, j) + X(ResEqRow(i) + j)
                                        end do
                                    end do

                                    !-----Adjust gate device flows from iteration
                                    do i = 1, NGate
                                        do j = 1, GateArray(i)%nDevice
                                            Change = X(GateArray(i)%devices(j)%calcRow)
                                            GateArray(i)%devices(j)%flow = GateArray(i)%devices(j)%flow &
                                                                           + Change        ! calcRow =device column in X
                                        end do
                                    end do

                                    !-----Check for overall closure and store closure information.
                                    MaxRatioQ = MaxRatioQ/rescale
                                    MaxChangeZ = MaxChangeZ/rescale

                                    if (StoreNetworkClosure( &
                                        MaxRatioQ, ChannelQ, NodeQ, &
                                        MaxChangeZ, ChannelZ, NodeZ, &
                                        NormClose) &
                                        ) then
                                        NetworkClosure = .true.
                                    else
                                        NetworkClosure = .false.
                                    end if

                                    !-----Write intermediate results.

                                    OK = IntermediateNetworkResults()

                                    return
                                    end function

                                    !== Public (IntermediateNetworkResults) ================================

                                    logical function IntermediateNetworkResults()
                                        use IO_Units
                                        use runtime_data
                                        use grid_data
                                        use netcntrl
                                        implicit none

                                        !   Purpose:  Report values of variables, current iteration.

                                        !   Arguments:

                                        !   Argument definitions:

                                        !   Local Variables:
                                        integer :: PrintUnit
                                        logical :: OK

                                        !   Routines by module:
                                        !   Intrinsics:
                                        integer :: MOD
                                        intrinsic MOD

                                        !   Programmed by: Lew DeLong
                                        !   Date:          February 1991
                                        !   Modified by:   Barry Wicktom (use of master file names added)
                                        !   Last modified: 1/7/92
                                        !   Version 93.01, January, 1993

                                        !-----Implementation -----------------------------------------------------

                                        PrintUnit = unit_output

                                        !   Output formats:
2001                                    format(////'  Time          ....Q....            ....Z....'/ &
                                                '  Step Iter. Channel  Section     Channel  Section')

                                        !---- Write network status to print file if requested.

                                        if (PrintLevel == 37) then
                                            if (Iteration == 1) write (PrintUnit, 2001) ! write header
                                            OK = ReportNetworkIterationStatus()
                                            OK = ReportNetworkStatus()

                                        end if

                                        !---- Write iteration-status header to screen.

                                        TimeStep = NetworkTimeStep()

                                        if (NetworkPrintLevel() >= 4) then
                                            if ( &
                                                EndIteration &
                                                .and. &
                                                TimeStep == 1 &
                                                ) then

                                                write (*, 2001)

                                            end if
                                        end if

                                        !---- Write iteration status to screen.
                                        if (EndIteration) then
                                            if (NetworkPrintLevel() >= 4) then
                                                OK = ScreenNetworkStatus()
                                            end if
                                            if (NetworkIteration() >= MaxNetworkIterations()) then
                                                if (NetworkPrintLevel() >= 1) then
                                                    write (unit_screen, 610) current_date, NetworkIteration()
                                                    write (unit_output, 610) current_date, NetworkIteration()
610                                                 format('Note: at ', a, &
                                                           ' network iterations at maximum (', i3, ').')
                                                end if
                                            end if
                                        end if

                                        IntermediateNetworkResults = .true.

                                        return
                                    end function IntermediateNetworkResults
                                    !==  Public (ReportNetworkStatus) ======================================

                                    logical function ReportNetworkStatus()
                                        use IO_Units
                                        use grid_data
                                        use chstatus
                                        use channel_schematic
                                        use netcntrl
                                        implicit none

                                        !   Purpose:  Report current status of flow in a network of open channels.

                                        !   Arguments:

                                        !   Argument definitions:

                                        !   Module data:

                                        !   Local Variables:
                                        integer :: PrintUnit
                                        integer :: I
                                        integer :: J
                                        integer :: Node
                                        integer :: ChannelNumber
                                        integer :: Channels
                                        logical :: OK

                                        !   Routines by module:

                                        !   Programmed by: Lew DeLong
                                        !   Date:          November 1990
                                        !   Modified by:   Barry Wicktom (use of master file names added)
                                        !   Last modified: Tues  01-07-1992
                                        !   Version 93.01, January, 1993

                                        !-----Implementation -----------------------------------------------------

                                        PrintUnit = unit_output

                                        ReportNetworkStatus = .false.

                                        Channels = NumberOfChannels()

                                        do I = 1, Channels

                                            ChannelNumber = I

                                            if (Channels > 1) then
                                                write (PrintUnit, *) ' '
                                                write (PrintUnit, *) '        Channel...', chan_geom(ChannelNumber)%chan_no
                                            end if

                                            if (OpenChannel(ChannelNumber)) then

                                                write (PrintUnit, *) ' '
                                                write (PrintUnit, *) &
                                                    'CompCxNo  Location   Discharge   WS_Elev     Depth'

                                                do J = 1, NumberOfStreamLocations()
                                                    Node = J

                                                    write (PrintUnit, '(I4,2X,4F11.2)') &
                                                        Node, StreamDistance(Node), &
                                                        StreamFlow(Node), StreamSurfaceElevation(Node), &
                                                        StreamDepth(Node)

                                                end do

                                            else

                                                write (UNIT_ERROR, *) ' ***Error (ReportSystemStatus)'
                                                write (UNIT_ERROR, *) ' Failed to open channel ', chan_geom(ChannelNumber)%chan_no
                                                write (unit_error, *) '***Error (ReportSystemStatus)'
                                                write (unit_error, *) 'Failed to open channel ', chan_geom(ChannelNumber)%chan_no
                                                call EXIT(1)

                                            end if

                                            OK = CloseChannel()

                                        end do

                                        ReportNetworkStatus = .true.

                                        return
                                    end function ReportNetworkStatus

                                    !== Private (ReportNetworkIterationStatus) =============================

                                    logical function ReportNetworkIterationStatus()
                                        use IO_Units
                                        use netcntrl
                                        implicit none

                                        !   Purpose:  Write network iteration information to print file.

                                        !   Arguments:

                                        !   Argument definitions:

                                        !   Local Variables:
                                        integer :: PrintUnit
                                        character(len=1)::Star
                                        character(len=1)::Blank
                                        parameter(Star='*', Blank=' ')

                                        !   Programmed by: Lew DeLong
                                        !   Date:          March 1991
                                        !   Modified by:  Barry Wicktom (use of master file names added)
                                        !   Last modified: 1/7/92
                                        !   Version 93.01, January, 1993

                                        !-----Implementation -----------------------------------------------------

                                        PrintUnit = unit_output

                                        !   Output formats:

2000                                    format(2I5, 2I8, A1, 4X, 2I8, A1)

                                        TimeStep = NetworkTimeStep()
                                        if (CloseQ) then
                                            if (CloseZ) then

                                                write (PrintUnit, 2000) TimeStep, Iteration, &
                                                    ChannelRatioQ, NodeRatioQ, Blank, &
                                                    ChannelChangeZ, NodeChangeZ, Blank

                                            else

                                                write (PrintUnit, 2000) TimeStep, Iteration, &
                                                    ChannelRatioQ, NodeRatioQ, Blank, &
                                                    ChannelChangeZ, NodeChangeZ, Star

                                            end if

                                        else if (CloseZ) then

                                            write (PrintUnit, 2000) TimeStep, Iteration, &
                                                ChannelRatioQ, NodeRatioQ, Star, &
                                                ChannelChangeZ, NodeChangeZ, Blank

                                        else

                                            write (PrintUnit, 2000) TimeStep, Iteration, &
                                                ChannelRatioQ, NodeRatioQ, Star, &
                                                ChannelChangeZ, NodeChangeZ, Star

                                        end if

                                        ReportNetworkIterationStatus = .true.

                                        return
                                    end function ReportNetworkIterationStatus

!==== EOF fourpt =======================================================
                                    end module
