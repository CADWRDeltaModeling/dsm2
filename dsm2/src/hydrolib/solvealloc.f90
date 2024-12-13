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
!!
!!      For information about the solver routines, contact:
!!      Eli Ateljevich
!!
!****-SPARSE COPYRIGHT *************
!  Revision and copyright information.
!
!  Copyright (c) 1985,86,87,88
!  by Kenneth S. Kundert and the University of California.
!
!  Permission to use, copy, modify, and distribute this software and
!  its documentation for any purpose and without fee is hereby granted,
!  provided that the copyright notices appear in all copies and
!  supporting documentation and that the authors and the University of
!  California are properly credited.  The authors and the University of
!  California make no representations as to the suitability of this
!  software for any purpose.  It is provided `as is', without express
!  or implied warranty.
!

!***********************************************************************
!
!
!
!***********************************************************************
module solvealloc
    use solver
    implicit none
contains
    !== Public (InitializeSolver) ==========================================

    logical function InitializeSolver()
        use IO_Units
        use Gates, only: NGate,GateArray
        use grid_data
        use klu
        use network

        use chconnec
        use channel_schematic, only: TotalStreamLocations
        implicit none

        !   Purpose: Initialize the solution matrix

        !   Arguments:

        !   Argument definitions:

        !   Module data:

        !   Local Variables:
        integer,parameter :: Zero=0
        integer error             ! error indicator for matrix allocation routine
        integer rowcounter        ! counter for reservoir rows and gate rows
        integer i                 ! loop index

        !-----Implementation -----------------------------------------------------

        InitializeSolver = .false.

        TotalChanRows = 2*TotalStreamLocations()
        rowcounter=0
        do i=1,nreser
            rowcounter=rowcounter+res_geom(i)%nnodes+1
        end do
        TotalChanResRows=TotalChanRows + rowcounter
        rowcounter=0
        do i=1,ngate
            rowcounter=rowcounter+GateArray(i)%nDevice
        end do

        Equations = TotalChanResRows + rowcounter
        k_common=klu_fortran_init()
        RowScale=1.D0

        !-----Initialize Matrix Scale Factors
        do i=1,Equations
            if (Mod(i,2) == 0 .and. i <= TotalChanRows ) then
                ColumnScale(i)=ZscaleFactor
            else
                ColumnScale(i)=1.0
            end if
        end do

        !-----Set up matrix-shape indicies.
        if( ReserveMatrix() ) then
        else
            write(UNIT_ERROR,*) ' ReserveMatrix failed...'
            return
        end if

        !-----Initialize 'first iteration' index.

        InitializeSolver = .true.
        return
    end function


    !==   Private (ReserveMatrix) =============================================

    logical function ReserveMatrix()
        use Gates, only:gateArray,Gate,NGate
        use IO_Units
        use grid_data
        use constants
        use klu
        use network
        use chnlcomp
        use chconnec
        use netcntrl
        use channel_schematic
        implicit none

        !     Purpose:  To reserve elements in the sparse matrix and create pointers
        !     to the elements which will be used later when filling the matrix.
        !

        !     Arguments:

        !     Argument definitions:


        !     Local Variables:

        integer II,J,JJ, K, L, M, N
        integer I                     ! cumulative counter of rows
        integer ResRow,ConnectCol,con
        integer CodeUp,CodeDown,ChannelNumber,NodeContinuityRow,ChannelCol
        integer MaxConnections
        parameter (MaxConnections = MaxConnectingChannels)
        integer ConnectingNodes,Node,ConstraintNode
        integer NodeReferenceChannel,ConnectingChannel,ConnectCompPt
        integer ResChannel
        integer :: ZnodeCol  ! Index of column representing stage at the node
                             ! connected to a gate or reservoir. Note that
					         ! nodes don't really have a stage variable -- this
					         ! column is really the column representing stage
                             ! a reference channel computational node.
        integer :: ZobjCol   ! Index of column representing stage at the water body
	                             ! attached to a gate or reservoir.
        integer :: QobjCol   ! Index of column represents the channel computational node
	                             ! or Reservoir connection whose flow represents flow through
	                             ! the gate.
        integer :: QdevCol   ! Column representing the variable for flow through an
                             ! individual gate device.


        integer devrow

        integer FlowOffset  ! used to help represent a flow column
        integer StageOffset ! used to help represent a stage column
        integer,parameter :: DF=2     ! # of state variables on grid (flow, stage)
        integer,parameter :: PlusOne= 1
        integer,parameter :: MinusOne= -1
        integer,parameter :: One=1

        logical OK
        integer UpConstraintRow
        integer DownConstraintRow

        integer indx

        integer objType,objID
        type(Gate),pointer :: currentGate

        integer :: nmasseq, ndynmeq, ncptrs, nreseq, ngateeq
        !***** Solver
        !-----Boundary-condition code          Use...
        !
        !     water surface
        !     1                        explicitly known
        !     4                        self-setting (downstream only)
        !     11                        equal to another water surface
        !     ........31                        3-parameter relation
        !     ........41                        (reserved for four-parameter rating)
        !     ........51                        user-defined
        !
        !     flow
        !     2                        explicitly known
        !     12                        sum of flows equal zero
        !     ........32                        3-parameter relation
        !     ........42                        (reserved for four-parameter rating)
        !     ........52                        user-defined

        !     Programmed by: Eli Ateljevich
        !     Version 98.01, July, 1998

        !-----Implementation -----------------------------------------------------

        ReserveMatrix = .false.

        !-----I = current row number.
        I=0

        !-----J = current equation number.
        J=0

        !-----K = current constraint pointer number
        K=0

        if(not(allocated(UpConstraintEq))) allocate(UpConstraintEq(NumberOfChannels()))
        if(not(allocated(DownConstraintEq))) allocate(DownConstraintEq(NumberOfChannels()))
        if(not(allocated(UpConstraintIndex))) allocate(UpConstraintIndex(NumberOfChannels()))
        if(not(allocated(DownConstraintIndex))) allocate(DownConstraintIndex(NumberOfChannels()))
        if(not(allocated(EqPointer))) allocate(EqPointer(NumberOfChannels()))

        do 300 M=1,NumberOfChannels()
            ChannelNumber = M
            UpConstraintRow=0
            DownConstraintRow=0

            if( OpenChannel( ChannelNumber ) ) then

                !-----------Check upstream flow constraint

                CodeUp = MOD( UpstreamCode(), 100 )

                if( CodeUp == 1 &
                    .or. &
                    CodeUp == 11 &
                    .or. &
                    CodeUp == 31 &
                    .or. &
                    CodeUp == 51  ) then

                    !--------------Upstream stage-type boundary

                    !--------------First mass equation
                    I=I+1
                    J=J+1
                    MASSEQ(1,J) = set_position_in_matrix(I,I)
                    MASSEQ(2,J) = set_position_in_matrix(I,I+1)
                    MASSEQ(3,J) = set_position_in_matrix(I,I+2)
                    MASSEQ(4,J) = set_position_in_matrix(I,I+3)
                    MASSEQROW(J)=I
                    RowScale(I) = MassScaleFactor

                    I=I+1
                    UpConstraintRow = I
                    UpConstraintEq(M) = I
                    RowScale(i) = 1.D0

                else if( CodeUp == 32   .or. CodeUp==12 &
                    .or.  CodeUp==2 .or. &
                    CodeUp == 52 ) then

                    !--------------Upstream flow-type boundary
                    I=I+1
                    UpConstraintRow = I
                    UpConstraintEq(M) = I
                    RowScale(i) = 1



                    !--------------First Mass Equation
                    I=I+1
                    J=J+1
                    MASSEQROW(J)=I

                    MASSEQ(1,J) = set_position_in_matrix(I,I-1)
                    MASSEQ(2,J) = set_position_in_matrix(I,I)
                    MASSEQ(3,J) = set_position_in_matrix(I,I+1)
                    MASSEQ(4,J) = set_position_in_matrix(I,I+2)
                    RowScale(I) = MassScaleFactor

                end if
                EqPointer( ChannelNumber ) = J

                !-----------if more than one subdomain in
                !     channel

                if( NumberOfStreamLocations() > 2 ) then
                    do 100 N=UpstreamPointer()+2,DownstreamPointer()

                        !-----------------dynamic equation
                        I=I+1
                        DYNMEQ(1,J) = set_position_in_matrix(I,I-2)
                        DYNMEQ(2,J) = set_position_in_matrix(I,I-1)
                        DYNMEQ(3,J) = set_position_in_matrix(I,I)
                        DYNMEQ(4,J) = set_position_in_matrix(I,I+1)
                        DYNMEQROW(J)= I
                        RowScale(i) = DynScaleFactor

                        !-----------------general subdomain.
                        J = J + 1

                        !-----------------mass equation.
                        I = I+1
                        MASSEQ(1,J) = set_position_in_matrix(I,I-1)
                        MASSEQ(2,J) = set_position_in_matrix(I,I)
                        MASSEQ(3,J) = set_position_in_matrix(I,I+1)
                        MASSEQ(4,J) = set_position_in_matrix(I,I+2)
                        MASSEQROW(J)=I
                        RowScale(I) = MassScaleFactor

100                 continue
                end if

                !-----------Check downstream flow constraint
                CodeDown = MOD( DownstreamCode(), 100 )

                if( CodeDown == 2 &
                    .or. &
                    CodeDown == 12 &
                    .or. &
                    CodeDown == 32 &
                    .or. &
                    CodeDown == 52  ) then

                    !--------------Downstream flow-type boundary
                    I=I+1
                    DownConstraintRow = I
                    DownConstraintEq(M)=I
                    RowScale(i) = 1


                    !--------------Last Dynamic Eq
                    I=I+1
                    DYNMEQ(1,J) = set_position_in_matrix(I,I-3)
                    DYNMEQ(2,J) = set_position_in_matrix(I,I-2)
                    DYNMEQ(3,J) = set_position_in_matrix(I,I-1)
                    DYNMEQ(4,J) = set_position_in_matrix(I,I)
                    DYNMEQROW(J)=I
                    RowScale(i) = DynScaleFactor

                else
                    !     Eli: Check
                    !--------------Downstream stage-type boundary
                    I=I+1
                    DYNMEQ(1,J) = set_position_in_matrix(I,I-2)
                    DYNMEQ(2,J) = set_position_in_matrix(I,I-1)
                    DYNMEQ(3,J) = set_position_in_matrix(I,I)
                    DYNMEQ(4,J) = set_position_in_matrix(I,I+1)
                    DYNMEQROW(J)=I
                    RowScale(i) = DynScaleFactor
                    I = I+1
                    DownConstraintRow = I
                    DownConstraintEq(M)=I
                    RowScale(i) = 1.D0
                end if


                !     These lines are very common as diagnostics

                !     if (UpConstraintRow .le. 1349 .and. DownConstraintRow .ge. 1347) then
                !     write(unit_screen,110)chan_geom(m)%chan_no,M,
                !     &    UpConstraintRow,DownConstraintRow,CodeUp,CodeDown
                !     110  format("Channel: ",i5," int. no: ",i5, //"Up constraint row: ",i5,
                !     &    " Down: ",i5," Code up: ",i5," Code down",i5)
                !     pause
                !     Endif




                !-----------Reserve matrix elements for constraints
                !-----------based on constraint type
                !-----------and connections

                !-----------Upstream end of channel.
                if (UpConstraintRow/=0) then
                    if (codeup == 1) then
                        !-----------------Known water surface
                        !-----------------Single constraint at (i,i)
                        K=K+1
                        UpConstraintIndex(M)=K
                        ConstraintPointers(K)=set_position_in_matrix(UpConstraintRow,UpConstraintRow)

                    elseif (codeup == 2) then
                        !-----------------Known flow
                        !-----------------Flow constraint. at (i,i)% Possible reservoir stage
                        !-----------------components added later

                        K=K+1
                        UpConstraintIndex(M)=K
                        ConstraintPointers(K)=set_position_in_matrix(UpConstraintRow,UpConstraintRow)

                    elseif (codeup == 4) then
                    !-----------------Self Starting(downstream only)
                    !-----------------Eli: Don't understand this yet

                    elseif (codeup == 11) then
                        !-----------------Equal Water Surfaces
                        !-----------------Element at (i,i), plus stage at a single connecting node, which
                        !-----------------must be identified
                        K=K+1
                        UpConstraintIndex(M)=K
                        ConstraintPointers(K)=set_position_in_matrix(UpConstraintRow,UpConstraintRow)


                        ConstraintNode = UpstreamPointer()
                        ConnectingNodes = UpstreamConnections()
                        if( ConnectingNodes <= MaxConnections ) then
                        else
                            write(UNIT_ERROR,*) ' ####error(ReserveMatrix)'
                            write(UNIT_ERROR,*) ' Upstream end Channel...', CurrentChannel()
                            write(UNIT_ERROR,*) ' Number of connections (',ConnectingNodes,')'
                            write(UNIT_ERROR,*) ' exceeds maximum (',MaxConnections,').'
                            write(UNIT_ERROR,*) ' Abnormal program end.'
                            call exit(1)
                        end if

                        if (ConnectingNodes > 0) then
                            ConnectingChannel = UpstreamConnect(One)
                            if (ConnectingChannel/=0) then
                                Node = IABS(StreamEndNode(ConnectingChannel))
                            else
                            !-----------------------No channel ... not actually attached
                            end if
                            K=K+1
                            ConstraintPointers(K)=set_position_in_matrix(UpConstraintRow,DF*Node)
                        end if

                    elseif (codeup == 12) then
                        !-----------------Sum of Flow
                        !-----------------Flow at (i,i). Possible reservoir stage components
                        !-----------------added later
                        K=K+1
                        UpConstraintIndex(M)=K
                        ConstraintPointers(K)=set_position_in_matrix(UpConstraintRow,UpConstraintRow)
                        ConnectingNodes = UpstreamConnections()
                        ConstraintNode = UpstreamPointer()
                        if (ConnectingNodes>0) then
                            do 130 L=1,ConnectingNodes
                                Node = StreamEndNode(UpstreamConnect(L))
                                ConnectCol = 2 * IABS(Node) - 1
                                K=K+1
                                ConstraintPointers(K)=set_position_in_matrix(UpConstraintRow,ConnectCol)
130                         continue
                        end if

                    elseif (codeup == 31 .or. &
                        codeup == 32) then
                        !-----------------Three Param surface or flow
                        !-----------------Includes local flow, local surface
                        !-----------------one connecting surface.

                        ConstraintNode = UpstreamPointer()
                        ConnectingNodes = UpstreamConnections()
                        ConnectingChannel =UpstreamConnect(One)
                        ConnectCol = DF * IABS(StreamEndNode(ConnectingChannel))
                        K=K+1
                        UpConstraintIndex(M)=K

                        !-----------------Flow at constraint node
                        FlowOffset = 0
                        if (CodeUp == 31) FlowOffset = MinusOne
                        ConstraintPointers(K)= set_position_in_matrix( &
                            UpConstraintRow,UpConstraintRow+FlowOffset)

                        !-----------------Stage at constraint node
                        StageOffset=0
                        if (CodeUp == 32) StageOffset = PlusOne
                        K=K+1
                        ConstraintPointers(K) = set_position_in_matrix( &
                            UpConstraintRow,UpConstraintRow+StageOffset)

                        !-----------------Stage at connecting node
                        K=K+1
                        ConstraintPointers(K) = set_position_in_matrix( &
                            UpConstraintRow,ConnectCol)

                    elseif (codeup == 51 .or. &
                        codeup == 52) then
                        !-----------------User Defined flow or surface
                        !-----------------For generality, flow and surface elements are reserved for
                        !-----------------the constraint row and for each connecting channel. It is very
                        !-----------------likely that only a few of these will be used (e.g., orifice
                        !-----------------equation uses only three)
                        RowScale(UpConstraintRow)=1.D0
                        ConstraintNode = UpstreamPointer()
                        ConnectingNodes =UpstreamConnections()
                        FlowOffset = 0
                        if (CodeUp == 51) FlowOffset = MinusOne
                        StageOffset = 0
                        if (CodeUp == 52) StageOffset = PlusOne
                        !-----------------Flow at Constraint
                        K=K+1
                        UpConstraintIndex(M)=K

                        ConstraintPointers(K)= set_position_in_matrix( &
                            UpConstraintRow,DF*ConstraintNode-1)
                        !-----------------Stage at Constraint
                        K=K+1
                        ConstraintPointers(K)= set_position_in_matrix( &
                            UpConstraintRow,DF*ConstraintNode)
                        !-----------------Now Flow and Stage at each connecting node

                        if(ConnectingNodes>0) then
                            do 160 L=1,ConnectingNodes
                                Node = StreamEndNode(UpstreamConnect(L))
                                        ! This is the "stage" column
                                ConnectCol = iabs(Node)*DF
                                K=K+1
                                FlowOffset=-1
                                ConstraintPointers(K)=set_position_in_matrix( &
                                    UpConstraintRow,ConnectCol+FlowOffset)
                                K=K+1
                                ConstraintPointers(K)=set_position_in_matrix( &
                                    UpConstraintRow,ConnectCol)
160                         continue
                        end if

                    else
                        write(UNIT_ERROR,*) ' *** Error (ReserveMatrix)'
                        write(UNIT_ERROR,*) 'Channel Number ', M
                        write(UNIT_ERROR,*) ' Constraint Type (Condition Number) Not Supported.'
                        call exit(1)
                    endif

                else
                    write(UNIT_ERROR,*) ' *** Error (ReserveMatrix)'
                    write(UNIT_ERROR,*) 'Channel Number ', M
                    write(UNIT_ERROR,*) ' Bad Constraint Row'
                    call exit(1)
                end if

                !-----------Downstream end of channel.
                if (DownConstraintRow/=0) then

                    if (codedown == 1) then
                        !-----------------Known water surface
                        !-----------------Single constraint at (i,i)
                        K=K+1
                        DownConstraintIndex(M)=K
                        ConstraintPointers(K)=set_position_in_matrix(DownConstraintRow,DownConstraintRow)

                    elseif (codedown == 2) then
                        !-----------------Known flow
                        !-----------------Flow constraint. at (i,i). Possible reservoir stage
                        !-----------------components added later

                        K=K+1
                        DownConstraintIndex(M)=K
                        ConstraintPointers(K)=set_position_in_matrix(DownConstraintRow,DownConstraintRow)

                    elseif (codedown == 4) then
                    !-----------------Self Starting(downstream only)
                    !-----------------Eli: Don't understand this yet

                    elseif (codedown == 11) then
                        !-----------------Equal Water Surfaces
                        !-----------------Element at (i,i), plus stage at a single connecting node, which
                        !-----------------must be identified
                        K=K+1
                        DownConstraintIndex(M)=K
                        ConstraintPointers(K)=set_position_in_matrix( &
                            DownConstraintRow,DownConstraintRow)

                        ConstraintNode = DownstreamPointer()
                        ConnectingNodes = DownstreamConnections()
                        if( ConnectingNodes > MaxConnections ) then
                            write(UNIT_ERROR,*) ' ####error(ReserveMatrix)'
                            write(UNIT_ERROR,*) ' Downstream end Channel...',  &
                                CurrentChannel()
                            write(UNIT_ERROR,*) ' Number of connections (', &
                                ConnectingNodes,')'
                            write(UNIT_ERROR,*) ' exceeds maximum (',MaxConnections,').'
                            write(UNIT_ERROR,*) ' Abnormal program end.'
                            call exit(1)
                        end if

                        if (ConnectingNodes > 0) then
                            ConnectingChannel = DownstreamConnect(One)
                            if(ConnectingChannel/=0) then
                                Node = IABS(StreamEndNode(ConnectingChannel))
                            else
                            !-----------------------No channel ... not actually attached
                            end if

                            K=K+1
                            ConstraintPointers(K)=set_position_in_matrix( &
                                DownConstraintRow,DF*Node)
                        end if

                    elseif (codedown == 12) then
                        !-----------------Sum of Flow.
                        !-----------------Flow at (i,i). Possible stage at (i,i+1) and connecting reservoirs.
                        !-----------------Also Flow at each connecting node
                        K=K+1
                        DownConstraintIndex(M)=K
                        ConstraintPointers(K)=set_position_in_matrix( &
                            DownConstraintRow,DownConstraintRow)

                        ConnectingNodes = DownstreamConnections()
                        if(ConnectingNodes <= MaxConnections) then
                        else
                            write(UNIT_ERROR,*) ' ####error(ReserveMatrix)'
                            write(UNIT_ERROR,*) ' Downstream end Channel...', &
                                CurrentChannel()
                            write(UNIT_ERROR,*) ' Number of connections (', &
                                ConnectingNodes,')'
                            write(UNIT_ERROR,*) ' exceeds maximum (',MaxConnections,').'
                            write(UNIT_ERROR,*) ' Abnormal program end.'
                            call exit(1)
                        end if

                        ConstraintNode = DownstreamPointer()

                        if (ConnectingNodes>0) then
                            do 220 L=1,ConnectingNodes
                                Node = StreamEndNode(DownstreamConnect(L))
                                ConnectCol = DF*IABS(Node) - 1
                                K=K+1
                                ConstraintPointers(K)=set_position_in_matrix( &
                                    DownConstraintRow,ConnectCol)
220                         continue
                        end if

                    elseif (codedown == 31 .or. &
                        codedown == 32) then
                        !-----------------Three Param surface or flow
                        !-----------------Three elements are reserved, including two surface
                        !-----------------elements and one flow element.

                        ConstraintNode = DownstreamPointer()
                        ConnectingNodes =DownstreamConnections()
                        ConnectingChannel =DownstreamConnect(One)
                        ConnectCol = DF * abs(StreamEndNode(ConnectingChannel))
                        K=K+1
                        DownConstraintIndex(M)=K

                        !-----------------Flow at constraint node
                        FlowOffset = 0
                        if (CodeDown == 31) then
                            FlowOffset = MinusOne
                        end if
                        ConstraintPointers(K)= set_position_in_matrix( &
                            DownConstraintRow,DownConstraintRow+FlowOffset)

                        !-----------------Stage at constraint
                        StageOffset=0
                        if (CodeDown == 32) then
                            StageOffset = PlusOne
                        end if
                        K=K+1
                        ConstraintPointers(K) = set_position_in_matrix( &
                            DownConstraintRow,DownConstraintRow+StageOffset)

                        !-----------------Stage at connecting node
                        K=K+1
                        ConstraintPointers(K) = set_position_in_matrix( &
                            DownConstraintRow,ConnectCol)

                    elseif (codedown == 51 .or. &
                        codedown == 52) then
                        !-----------------User Defined flow or surface also currently used for gates
                        !-----------------For generality, flow and surface elements are reserved at the
                        !-----------------the boundary constraint row for each connecting channel. It is
                        !-----------------likely that only a few of these will be used (e.g., orifice
                        !-----------------equation uses only three)
                        RowScale(DownConstraintRow)=1.D0
                        ConstraintNode = DownstreamPointer()
                        ConnectingNodes =DownstreamConnections()
                        FlowOffset = 0
                        if (CodeDown == 51) FlowOffset = MinusOne
                        StageOffset = 0
                        if (CodeDown == 52) StageOffset = PlusOne
                        !-----------------Flow at Constraint
                        K=K+1
                        DownConstraintIndex(M)=K
                        ConstraintPointers(K)= set_position_in_matrix( &
                            DownConstraintRow, DF*ConstraintNode-1)

                        !-----------------Stage at Constraint
                        K=K+1
                        ConstraintPointers(K)= set_position_in_matrix( &
                            DownConstraintRow,DF*ConstraintNode)

                        !-----------------Now Flow and Stage at each connecting node
                        ConstraintNode = DownstreamPointer()

                        do 250 L=1,ConnectingNodes
                            Node = StreamEndNode(DownstreamConnect(L))
                            ConnectCol=abs(Node)*DF ! This is the "stage" column
                            K=K+1
                            ConstraintPointers(K)=set_position_in_matrix( &
                                DownConstraintRow,ConnectCol-1)
                            K=K+1
                            ConstraintPointers(K)=set_position_in_matrix(  &
                                DownConstraintRow,ConnectCol)
250                     continue
                    else
                        write(UNIT_ERROR,*) ' *** Error (ReserveMatrix)'
                        write(UNIT_ERROR,*) 'Channel Number ', M
                        write(UNIT_ERROR,*)  &
                            ' Constraint Type (Condition Number) Not Supported.'
                        call exit(1)
                    endif

                else
                    write(UNIT_ERROR,*) ' *** Error (ReserveMatrix)'
                    write(UNIT_ERROR,*) 'Channel Number ', M
                    write(UNIT_ERROR,*) ' Bad Constraint Row'
                    call exit(1)
                end if

                OK = CloseChannel()

            else
                write(UNIT_ERROR,*) ' ***Error (ReserveMatrix)'
                write(UNIT_ERROR,*) ' Could not open channel...', &
                    chan_geom(ChannelNumber)%chan_no
                call exit(1)
            end if

300     continue


        !-----Matrix Elements are now allocated for elements involving

        !-----only channels. The remainder of the
        !-----matrix consists of elements involving reservoirs,
        !-----gates, transfers, etc.

        !-----only channels. The remainder of the matrix consists
        !----- of elements involving reservoirs, gates, transfers, etc.



        !------- Allocate elements for reservoir mass balance and
        !        connection equations. Gates involving reservoirs will
        !        be allocated later.
        nmasseq=j
        ndynmeq=j
        ncptrs=k

        if(Nreser>0) then
            !--------K: index of current matrix entry
            K=0
            ResRow=TotalChanRows

            do II = 1,Nreser
                I=I+1
                ResRow=ResRow+1     ! Increment for the reservoir. ResRow now
                                    ! points to the mass balance equation for the
                                    ! reservoir, and column=resrow is the column
                                    ! corresponding to reservoir stage

                RowScale(ResRow)=ResScaleFactor
                ColumnScale(ResRow)=ZScaleFactor
                ResEqRow(ii)=ResRow
                !-----------First element is on diagonal and represents the reservoir
                !-----------water surface height
                K=K+1
                ResEqIndex(II) = K
                ResEqPointer(K) = set_position_in_matrix(ResRow,ResRow)

                !-----Reserve stage elements for connecting channels
                !     Entries will occur in the row corresponding
                !     to the reservoir mass balance, the row describing the reservoir
                !     equation and the row corresponding to the channel (actually node)
                !     continuity equation.

                do JJ =1, res_geom(ii)%nconnect
                    I=I+1
                    RowScale(ResRow+jj) = ResConnectScaleFactor
                    ResChannel = ResConnectingChannels(ii,jj)
                    if(ResChannel>0) then
                        NodeContinuityRow=UpConstraintEq(ResChannel)
                    else
                        NodeContinuityRow=DownConstraintEq(-ResChannel)
                    end if
                    !todo: This iabs call was for downstream external nodes
                    ChannelCol = iabs(StreamEndNode(ResChannel))*DF ! "Z" entry
                    ! contribution to reservoir mass
                    K=K+1
                    ResEqPointer(K) = set_position_in_matrix(ResRow,ResRow+jj)
                    ! reservoir equation at connection
                    ! ZRes
                    K=K+1
                    ResEqPointer(K) = set_position_in_matrix(ResRow+jj,ResRow)
                                   ! QRes
                    K=K+1
                    ResEqPointer(K) = set_position_in_matrix(ResRow+jj,ResRow+jj)
                                   ! ZChan
                    K=K+1
                    ResEqPointer(K) = set_position_in_matrix(ResRow+jj,ChannelCol)
                    K=K+1            ! contribution to node mass
                    ResEqPointer(K) = set_position_in_matrix(NodeContinuityRow,ResRow+jj)
                enddo
                ResRow=ResRow+res_geom(ii)%nnodes
            enddo
        end if

        !     Allocate elements for the gates. Gates are implemented as the sum of a number
        !     of flows from devices; the summation equation and the individual gate
        !     device equations each has its own row.
        nreseq=k ! number of reservoir equations

        devRow=TotalChanResRows ! starting row for device equations minus 1
        K=0
        if(NGate>0) then
            do II = 1,NGate
                currentGate=>gateArray(II)
                objType=currentGate%objConnectedType
                objID=currentGate%objConnectedID
                ! Find the reference channel for the node connected to this gate
                ! and corresponding computational node and column
                node=currentGate%node
                NodeReferenceChannel=node_geom(currentGate%node)%sumQChan
                if (NodeReferenceChannel < 0) then                   !!!!!!!????????
                    NodeContinuityRow=DownConstraintEq(-NodeReferenceChannel)
                else
                    NodeContinuityRow=UpConstraintEq(NodeReferenceChannel)
                end if
                ConnectCompPt = currentGate%nodeCompPoint
                ZnodeCol=DF*ConnectCompPt   ! Col for stage at reference channel
                if(gateArray(II)%flowDirection > 0) then
                    ZObjCol=DF*DownCompPointer(objID)
                else
                    ZObjCol=DF*UpCompPointer(objID)
                end if
                QObjCol=ZObjCol-1

                !	      Allocate equations in GateEqRow, which represents:
                !	       1) the channel flow or reservoir connection flow as the
                !		      sum of device flow:
                !		      Qchan=signconvention*sum(Qdev1+Qdev2)
                !                or
                !            2) an equal stage constraint if the gate is uninstalled
                !           Note that Qchan, Zobj and Znode columns are allocated, but
                !           will never all be used at once -- in case (1), the Qchan
                !           column is used and in case (2) the Zobj and Znode columns are used
                if(objType==obj_channel) then
                    if(currentGate%flowDirection == 1.D0) then      !downstream end
                        GateEqRow(ii)=DownConstraintEq(objID)
                        indx=DownConstraintIndex(objID)
                    elseif( currentGate%flowDirection == -1.D0) then !upstream end
                        GateEqRow(ii)=UpConstraintEq(objID)
                        indx=UpConstraintIndex(objID)
                    else
                        write(unit_error,310) currentGate%name
310                     format(/'Flow direction not set correctly for gate',a)
                        call exit(2)
                    end if
                    K=K+1
                    GateEqIndex(II)=K
                    GateEqPointer(K)=ConstraintPointers(indx) ! Q water body
                    K=K+1
                    GateEqPointer(K)=ConstraintPointers(indx+1) ! Z water body
                    K=K+1
                    GateEqPointer(K)=ConstraintPointers(indx+3) ! Z at node (reference channel)
                else if(objType==obj_reservoir) then
                    ResRow=ResEqRow(objID)         ! Reservoir volume equation
                    con=GateArray(ii)%subLocation  ! Reservoir connection (index)
                    GateEqRow(ii)=ResRow+con
                    ZobjCol=ResRow             ! Column for stage at gated water body
                    ZnodeCol=DF*ConnectCompPt  ! Col for stage at reference channel
                    QObjCol=ResRow+con       ! Col representing total flow through
			                                                                ! the gated reservoir connection
                    K=K+1
                    I=I+1
                    GateEqIndex(II)=K
                    GateEqPointer(K) = set_position_in_matrix(GateEqRow(ii), QobjCol)
                    K=K+1
                    GateEqPointer(K) = set_position_in_matrix(GateEqRow(ii), ZobjCol)
                    K=K+1
                    GateEqPointer(K) = set_position_in_matrix(GateEqRow(ii), ZnodeCol)
                                   ! gate flow contribution to node continuity
                    K=K+1
                    GateEqPointer(K) = set_position_in_matrix(NodeContinuityRow,QobjCol)
                    ! gate flow contribution to reservoir mass
                    K=K+1
                    GateEqPointer(K) = set_position_in_matrix(ResRow, QobjCol)
                end if
                !           Allocate matrix elements for the equations representing individual devices

                !  Add weir/pipe equations for each device in the gate
                do jj=1,currentGate%nDevice
                    I=I+1
                    devrow=devrow+1
                    QdevCol=devrow
                    currentGate%Devices(jj)%calcRow=devrow
                    ! Contribution of device total gate flow
                    K=K+1
                    GateEqPointer(K)=set_position_in_matrix(gateEqRow(ii),QdevCol)
                    K=K+1
                    GateEqPointer(K)=set_position_in_matrix( devrow, QdevCol)
                    K=K+1
                    GateEqPointer(K)=set_position_in_matrix(devrow,ZobjCol)
                    K=K+1
                    GateEqPointer(K)=set_position_in_matrix(devrow,ZnodeCol)
                    RowScale(devrow)=1./(128.)
                end do

            end do
        end if
        ngateeq = k
        if (k > MaxGatePtr) then
            write(UNIT_ERROR,*)"Maximum number of matrix elements allocated for gates"
            write(UNIT_ERROR,*)" exceeded. Reallocate MaxGatePtr"
            call exit(3)
        end if

        !-----End of allocation. Check for error and exit.
        if( I== Equations ) then
            call done_adding_to_coo()
            call coo2csc(Equations)
            call update_pointers_dim4(masseq,nmasseq)
            call update_pointers_dim4(dynmeq,ndynmeq)
            call update_pointers(ConstraintPointers, ncptrs)
            call update_pointers(ResEqPointer, nreseq)
            call update_pointers(GateEqPointer, ngateeq)
            ReserveMatrix = .true.

        else

            write(UNIT_ERROR,*) ' *** Error (ReserveMatrix)'
            write(UNIT_ERROR,*) I,' rows allocated, ',equations,' equations expected...'
            write(UNIT_ERROR,*) ' Abnormal program end.'
            call exit(1)

        end if

        return
    end function
    !-    function forwarding call to sfGetElement but after adding non-zero to coo
    function set_position_in_matrix(row,column)
        use klu
        use network
        implicit none
        integer row, column, val
        integer set_position_in_matrix
        integer,external :: sfGetElement

        if (equations > 44000) then ! size max sqrt of 2 billion
            print *, "Maximum solve size exceeded. Need an upgrade to 64 bit integers"
          !exit(1022);
        endif


        val = (row-1)*equations+(column-1) ! unique value encoded for each i,j
        call add_nonzero_to_coo(row, column, val)
        set_position_in_matrix=val

    end function

!==== EOF solvefpt.f =========================================
end module
