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

!     Routines for calculating external boundary conditions and internal
!     compatibility equations for channels in the network, including putting
!     corresponding equations in the computational matrix.
!

!== Public (ForceStreamSurface) ========================================
module channel_constraints
    implicit none
contains
    logical function ForceStreamSurface( Extremity )
        use solver
        use chconnec
        use chstatus
        use channel_schematic
        use netcntrl
        use solveutil
        use netbnd, only: upstreamboundaryvalue, downstreamboundaryvalue
      
        implicit none

        !   Purpose:  Compute and store constraint-equation coefficients
        !             forcing a known stream surface elevation.

        !   Arguments:
        integer Extremity

        !   Argument definitions:
        !     Extremity - index indicating
        !                   [+1] upstream end of current channel.
        !                   [-1] downstream end of current channel.

        !   Module data:

        !   Local Variables:
        integer Row, ConstraintNode
        integer Channel, ConstraintIndex,ConstraintPointer

        real*8 One, DesiredElevation
        parameter ( One = 1.0 )
        logical OK

        !   Routines by module:


        !   Programmed by: Lew DeLong
        !   Date:          February 1991
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------
        Channel=CurrentChannel()

        if( Extremity == +1 ) then
            !--------Upstream constraint.
            ConstraintNode = 1
            Row = UpstreamConstraintRow()

            ConstraintIndex = UpConstraintIndex(Channel)
            ConstraintPointer = ConstraintPointers(ConstraintIndex)

            OK = StoreAtLocation( ConstraintPointer, One )
            DesiredElevation = UpstreamBoundaryValue()

        else if( Extremity == -1 ) then

            !--------Downstream constraint.
            ConstraintNode = NumberOfStreamLocations()
            Row = DownstreamConstraintRow()
            ConstraintIndex = DownConstraintIndex(Channel)
            ConstraintPointer = ConstraintPointers(ConstraintIndex)

            OK = StoreAtLocation(ConstraintPointer, One )
            DesiredElevation = DownstreamBoundaryValue()

        end if

        OK = StoreAtRow( Row, &
            DesiredElevation - StreamSurfaceElevation( ConstraintNode ) &
            )

        ForceStreamSurface = .true.

        return
    end function ForceStreamSurface

    !== Public (ForceStreamFlow) ===========================================

    logical function ForceStreamFlow( Extremity )
        use chconnec
        use solver
        use chstatus
        use netcntrl
        use channel_schematic
        use solveutil
        use netbnd, only: upstreamboundaryvalue,downstreamboundaryvalue
        implicit none

        !   Purpose:  Compute and store constraint-equation coefficients
        !             forcing a known streamflow.

        !   Arguments:
        integer Extremity

        !   Argument definitions:
        !     Extremity - index indicating
        !                   [+1] upstream end of current channel.
        !                   [-1] downstream end of current channel.


        !   Local Variables:
        integer Row, ConstraintNode, Channel
        integer ConstraintIndex,ConstraintPointer
        real*8    One, MinusOne, DesiredFlow
        parameter ( One = 1.0, MinusOne = -1.0 )
        logical OK

        !   Routines by module:

        !   Programmed by: Lew DeLong
        !   Date:          February 1991
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        !-----Note: flow conservation is written from the point of
        !      view of the node receiving the flow.

        Channel=CurrentChannel()

        if( Extremity == +1 ) then
            !--------Note: flow conservation is written from the point of
            !      view of the node receiving the flow.
            !--------Upstream constraint.
            ConstraintNode = 1
            Row = UpstreamConstraintRow()
            ConstraintIndex = UpConstraintIndex(Channel)
            ConstraintPointer = ConstraintPointers(ConstraintIndex)
            DesiredFlow = - UpstreamBoundaryValue() + StreamFlow( ConstraintNode )
            OK = StoreAtLocation( ConstraintPointer, MinusOne )
        else if( Extremity == -1 ) then
            !--------Downstream constraint.
            ConstraintNode = NumberOfStreamLocations()
            Row = DownstreamConstraintRow()
            ConstraintIndex = DownConstraintIndex(Channel)
            ConstraintPointer = ConstraintPointers(ConstraintIndex)
            DesiredFlow = DownStreamBoundaryValue() - StreamFlow( ConstraintNode )
            OK = StoreAtLocation( ConstraintPointer,One)
        end if

        OK = StoreAtRow(Row, DesiredFlow)

        ForceStreamFlow = .true.

        return
    end function ForceStreamFlow

    !== Public (ForceSumStreamFlow) ========================================

    logical function ForceSumStreamFlow( Extremity )
        use IO_Units
        use chconnec
        use solver
        use solveutil
        use channel_schematic
        use chstatus, only: downstreampointer, globalstreamflow
        use netbnd, only: upstreamboundaryvalue, downstreamboundaryvalue
        implicit none

        !   Purpose:  Compute and store constraint-equation coefficients
        !             forcing the sum of stream flows at a location to zero.

        !   Arguments:
        integer Extremity

        !   Argument definitions:
        !     Extremity - index indicating
        !                   [+1] upstream end of current channel.
        !                   [-1] downstream end of current channel.


        !   Local Variables:
        integer MaxConnections
        integer Channel,ConstraintIndex,ConstraintPointer
        parameter ( MaxConnections = MaxConnectingChannels )
        integer I, ConnectingNodes, Node(MaxConnections)
        integer ConstraintNode, DF
        integer Row, AbsNode
        real*8   One, MinusOne, Residual, Sign
        parameter ( MinusOne=-1.0, One = 1.0, DF = 2 )
        logical OK

        !   Routines by module:


        !   Programmed by: Lew DeLong
        !   Date:          February 1991
        !   Modified by:   Lew DeLong
        !   Last modified: August   1992
        !   Modified by:   Parviz Nader
        !   Last modified: December 1992
        !   Modified by:   Eli Ateljevich
        !   Last modified: July 1998
        !   Version 93.01, January, 1993

        !   Intrinsics:
        integer   IABS
        intrinsic IABS, DFLOAT

        !-----Implementation -----------------------------------------------------

        ForceSumStreamFlow = .false.

        !-----Note, by convention, any flow into a junction is positive.
        !     Constraint equation is written from the perspective of
        !     the junction.
        Channel=CurrentChannel()
        if( Extremity == +1 ) then

            !--------Upstream constraint.
            ConstraintNode = UpstreamPointer()
            Row = UpstreamConstraintRow()
            ConstraintIndex = UpConstraintIndex(Channel)

            ConstraintPointer = ConstraintPointers(ConstraintIndex)

            OK = StoreAtLocation( ConstraintPointer, MinusOne )
            OK = AddAtRow( Row, - UpstreamBoundaryValue() )

            ConnectingNodes = UpstreamConnections()
            if( ConnectingNodes <= MaxConnections ) then
            else
                write(UNIT_ERROR,*) ' ####error(ForceSumStreamFlow)'
                write(UNIT_ERROR,*) ' Upstream end Channel...', CurrentChannel()
                write(UNIT_ERROR,*) ' Number of connections (',ConnectingNodes,')'
                write(UNIT_ERROR,*) ' exceeds maximum (',MaxConnections,').'
                write(UNIT_ERROR,*) ' Abnormal program end.'
                call EXIT(1)
            end if

            if( ConnectingNodes > 0 ) then
                do  I=1,ConnectingNodes
                    Node(I) = StreamEndNode( UpstreamConnect(I) )
                end do
            else
            end if

        else if( Extremity == -1 ) then

            !--------Downstream constraint.

            ConstraintNode = DownstreamPointer()
            Row = DownstreamConstraintRow()
            ConstraintIndex = DownConstraintIndex(Channel)
            ConstraintPointer = ConstraintPointers(ConstraintIndex)

            OK = StoreAtLocation( ConstraintPointer, One )

            OK = AddAtRow( Row, -DownstreamBoundaryValue() )

            ConnectingNodes = DownstreamConnections()
            if( ConnectingNodes <= MaxConnections ) then
            else
                write(UNIT_ERROR,*) ' ####error(ForceSumStreamFlow)'
                write(UNIT_ERROR,*) ' Downstream end Channel...', CurrentChannel()
                write(UNIT_ERROR,*) ' Number of connections (',ConnectingNodes,')'
                write(UNIT_ERROR,*) ' exceeds maximum (',MaxConnections,').'
                write(UNIT_ERROR,*) ' Abnormal program end.'
                call EXIT(1)
            end if
            if( ConnectingNodes > 0 ) then
                do I=1,ConnectingNodes
                    Node(I) = StreamEndNode( DownstreamConnect(I) )
                end do
            end if

        else
            write(UNIT_ERROR,*) ' *** Error (ForceSumStreamFlow)'
            write(UNIT_ERROR,*) ' Extremity not equal +1 or -1 ...'
            return
        end if

        Residual = 0.0

        !-----Connecting Nodes
        if( ConnectingNodes > 0 ) then

            do  I=1,ConnectingNodes

                AbsNode = IABS(Node(I))
                if (Node(i)>0) then
                    Sign = -1.0
                else
                    Sign = 1.0
                end if
                ConstraintIndex = ConstraintIndex + 1
                ConstraintPointer = ConstraintPointers(ConstraintIndex)

                OK = StoreAtLocation( ConstraintPointer,  Sign )

                Residual = Residual &
                    + Sign * GlobalStreamFlow( AbsNode )

            end do

        end if

        if( Extremity == +1 ) then

            OK = AddAtRow( &
                Row, + GlobalStreamFlow( ConstraintNode ) - Residual )
        else
            OK = AddAtRow( &
                Row, - GlobalStreamFlow( ConstraintNode ) - Residual )
        end if

        ForceSumStreamFlow = .true.

        return
    end function ForceSumStreamFlow


    !== Public (ForceEqualStreamSurface) ===================================

    logical function ForceEqualStreamSurface( Extremity )
        use IO_Units
        use chconnec
        use solver
        use solveutil &
            , only:  UpstreamConstraintRow, DownstreamConstraintRow &
            , StoreAtLocation, StoreAtRow
        use chstatus &
            , only: GlobalStreamSurfaceElevation
        use channel_schematic &
            , only: UpstreamPointer, DownstreamPointer,CurrentChannel &
            ,UpstreamConnect, UpstreamConnections &
            ,DownstreamConnect, DownstreamConnections &
            ,StreamEndNode
        use netcntrl
        use netbnd &
            , only: UpstreamBoundaryValue, DownstreamBoundaryValue
        implicit none

        !   Purpose:  Compute and store constraint-equation coefficients forcing
        !             equal stream-surface elevations at two cross sections.

        !   Arguments:
        integer Extremity

        !   Argument definitions:
        !     Extremity - index indicating
        !                   [+1] upstream end of current channel.
        !                   [-1] downstream end of current channel.


        !   Local Variables:
        integer First
        parameter (First = 1)
        integer ConnectingNodes
        integer ConstraintNode, DF
        integer Zero, Row, Node
        integer ConnectingChannel
        integer Channel,ConstraintIndex,ConstraintPointer

        real*8   One, MinusOne, Fall
        parameter ( Zero = 0, One = 1.0, MinusOne = -1.0, DF = 2 )
        logical OK

        !   Routines by module:





        !   Programmed by: Lew DeLong
        !   Date:          February 1991
        !   Modified by:   Lew DeLong
        !   Last modified: August   1992
        !   Modified by:   Eli Ateljevich
        !   Last modified: July  1998
        !   Version 93.01, January, 1993

        !   Intrinsics:
        integer   IABS
        intrinsic IABS

        !-----Implementation -----------------------------------------------------
        Channel = CurrentChannel()
        ForceEqualStreamSurface = .false.

        if( Extremity == +1 ) then

            !--------Upstream constraint.

            ConstraintNode = UpstreamPointer()
            Row = UpstreamConstraintRow()
            ConstraintIndex = UpConstraintIndex(Channel)
            ConstraintPointer = ConstraintPointers(ConstraintIndex)

            OK = StoreAtLocation( ConstraintPointer, One )
            Fall = UpstreamBoundaryValue()
            ConnectingNodes = UpstreamConnections()
            ConnectingChannel = UpstreamConnect(First)

        else if( Extremity == -1 ) then

            !--------Downstream constraint.


            ConstraintNode = DownstreamPointer()
            Row = DownstreamConstraintRow()
            ConstraintIndex =DownConstraintIndex(Channel)
            ConstraintPointer = ConstraintPointers(ConstraintIndex)

            OK = StoreAtLocation( ConstraintPointer, One )
            Fall = DownstreamBoundaryValue()
            ConnectingNodes = DownstreamConnections()
            ConnectingChannel = DownstreamConnect(First)

        else
            write(UNIT_ERROR,*) ' *** Error (ForceEqualStreamSurface)'
            write(UNIT_ERROR,*) ' Extremity not equal +1 or -1 ...'
            return
        end if

        !-----Determine node number of attached channel.

        if( ConnectingChannel /= 0 ) then

            Node = IABS( StreamEndNode( ConnectingChannel ) )

        else

            !--------No channel number ... not actually attached.

            OK = StoreAtRow( &
                Row, Fall - GlobalStreamSurfaceElevation( ConstraintNode ) &
                )

        end if


        if( ConnectingNodes > 0 ) then
            ConstraintIndex = ConstraintIndex + 1
            ConstraintPointer = ConstraintPointers(ConstraintIndex)

            OK = StoreAtLocation( ConstraintPointer, MinusOne )

            OK = StoreAtRow( &
                Row, &
                Fall &
                + GlobalStreamSurfaceElevation( Node ) &
                - GlobalStreamSurfaceElevation( ConstraintNode ) &
                )

        else

            write(UNIT_ERROR,*) ' *** Error (ForceEqualStreamSurface)'
            write(UNIT_ERROR,*) ' Connections = ', ConnectingNodes

        end if

        ForceEqualStreamSurface = .true.

        return
    end function ForceEqualStreamSurface

    !== Private (SetStreamConstraintVariables) =============================

    logical function SetStreamConstraintVariables( Extremity )
        use IO_Units
        use strmcnst
        use channel_schematic &
            , only: UpstreamPointer, DownstreamPointer,CurrentChannel &
            ,UpstreamConnect, UpstreamConnections &
            ,DownstreamConnect, DownstreamConnections &
            ,StreamEndNode, UpstreamCode, DownstreamCode
        use chstatus &
            ,only: GlobalStreamSurfaceElevation, GlobalStreamFlow
        implicit none

        !   Purpose:  Set value of variables that may be used by
        !             user-programmed constraints.

        !   Arguments:
        integer Extremity

        !   Argument definitions:
        !     Extremity - index indicating
        !                   [+1] upstream end of current channel.
        !                   [-1] downstream end of current channel.

        !   Local Variables:
        integer I
        integer ConnectingNodes
        integer ConstraintNode
        integer ConnectingNode
        integer ConnectingChannel

        !   Routines by module:

        !   Intrinsics:
        integer   IABS
        intrinsic IABS

        !   Programmed by: Lew DeLong
        !   Date:          Oct   1992
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        SetStreamConstraintVariables = .false.

        ConnectingExtremity(1) = Extremity

        if( Extremity == +1 ) then

            !--------Upstream constraint.

            ConstraintNode = UpstreamPointer()
            ConnectingNodes = UpstreamConnections()
            ConditionCode = UpstreamCode()

        else if( Extremity == -1 ) then

            !--------Downstream constraint.

            ConstraintNode = DownstreamPointer()
            ConnectingNodes = DownstreamConnections()
            ConditionCode = DownstreamCode()

        else
            write(UNIT_ERROR,*) ' *** Error(SetStreamConstraintVariables)'
            write(UNIT_ERROR,*) ' Channel...',CurrentChannel()
            write(UNIT_ERROR,*) ' Extremity not equal +1 or -1 ...'
            return
        end if

        ConnectingChannels = ConnectingNodes
        Discharge(1) = GlobalStreamFlow( ConstraintNode )
        WSElev(1) = GlobalStreamSurfaceElevation( ConstraintNode )

        if( ConnectingNodes > 0 ) then
            if( ConnectingNodes <= MaxConnectingChannels ) then

                if( Extremity == +1 ) then

                    do  I=1,ConnectingNodes

                        ConnectingChannel = UpstreamConnect(I)

                        ConnectingNode = &
                            IABS( StreamEndNode( ConnectingChannel ) )

                        Discharge(I+1) = GlobalStreamFlow( ConnectingNode )

                        WSElev(I+1) = &
                            GlobalStreamSurfaceElevation( ConnectingNode )

                        ConnectingExtremity(I+1) = &
                            ConnectingChannel / IABS( ConnectingChannel )

                    end do

                else

                    do  I=1,ConnectingNodes

                        ConnectingChannel = DownstreamConnect(I)

                        ConnectingNode = &
                            IABS( StreamEndNode( ConnectingChannel ) )

                        Discharge(I+1) = GlobalStreamFlow( ConnectingNode )

                        WSElev(I+1) = &
                            GlobalStreamSurfaceElevation( ConnectingNode )

                        ConnectingExtremity(I+1) = &
                            ConnectingChannel / IABS( ConnectingChannel )

                    end do

                end if

            else
                write(UNIT_ERROR,*) ' ####Error(SetStreamConstraintVariables)'
                write(UNIT_ERROR,*) ' Channel...',CurrentChannel()
                write(UNIT_ERROR,*) ' Number of connections must be fewer than...', &
                    MaxConnectingChannels
                write(UNIT_ERROR,*) ' Currently, connections = ', ConnectingNodes
                return
            end if

        end if

        !-----Initialize global solution coefficients.

        ConstraintRightSide = 0.0

        do  I=1,2+2*MaxConnectingChannels

            ConstraintCoef(I) = 0.0

        end do

        SetStreamConstraintVariables = .true.

        return
    end function SetStreamConstraintVariables

    !== Private (SetUserStreamConstraintCoef) ===============================

    logical function SetUserStreamConstraintCoef()
        use IO_Units
        use chconnec
        use solver
        use solveutil
        use strmcnst
        use channel_schematic
        implicit none

        !   Purpose:  Place coefficients computed by specific user-programmed
        !             constraints in global solution matrix.

        !   Arguments:

        !   Argument definitions:

        !   Local Variables:
        integer Extremity, Offset, Row
        integer Channel,ConstraintIndex,ConstraintPointer
        integer I, ConnectingNodes, Node(MaxConnectingChannels)
        integer ConstraintNode
        logical OK

        !   Routines by module:

        !**** Local:

        !   Intrinsics:
        integer   IABS, MOD
        intrinsic IABS, MOD

        !   Programmed by: Lew DeLong
        !   Date:          Oct   1992
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------
        SetUserStreamConstraintCoef = .false.

        Channel=CurrentChannel()

        Extremity = ConnectingExtremity(1)

        if( Extremity == +1 ) then

            !--------Upstream constraint.

            ConstraintNode = UpstreamPointer()
            Row = UpstreamConstraintRow()

75      continue
        ConstraintIndex = UpConstraintIndex(Channel)

        ConstraintPointer = ConstraintPointers(ConstraintIndex)

        ConnectingNodes = UpstreamConnections()
        if( ConnectingNodes > 0 ) then
            do I=1,ConnectingNodes
                Node(I) = IABS(StreamEndNode( UpstreamConnect(I) ))
            end do
        else
        end if

    else if( Extremity == -1 ) then

        !--------Downstream constraint.

        ConstraintNode = DownstreamPointer()
        Row = DownstreamConstraintRow()
        ConstraintIndex = DownConstraintIndex(Channel)
        ConstraintPointer = ConstraintPointers(ConstraintIndex)

        ConnectingNodes = DownstreamConnections()
        if( ConnectingNodes > 0 ) then
            do  I=1,ConnectingNodes
                Node(I) = IABS(StreamEndNode( DownstreamConnect(I) ))
            end do
        else
        end if

    else
        write(UNIT_ERROR,*) ' *** Error(SetUserStreamConstraintCoef)'
        write(UNIT_ERROR,*) ' Extremity not equal +1 or -1 ...'
        write(UNIT_ERROR,*) ' Channel...', CurrentChannel()
        return
    end if

    !-----Determine if flow or water-surface constraint, set offset.

    if( MOD( ConditionCode, 10 ) == 2 ) then
        Offset = 0
    else
        Offset = -1
    end if

    !-----Set coefficients for constraint node.

    !-----Constraint-node coefficients for

    !          discharge, and
    OK = StoreAtLocation( ConstraintPointer, ConstraintCoef(1) )

    !          water surface.
    ConstraintIndex = ConstraintIndex+1
    ConstraintPointer = ConstraintPointers(ConstraintIndex)
    OK = StoreAtLocation( ConstraintPointer, ConstraintCoef(2) )

    !-----Right-hand side.

    OK = StoreAtRow( Row, ConstraintRightSide )

    if( ConnectingNodes > 0 ) then

        do  I=1,ConnectingNodes

            !-----------discharge coefficient.
            ConstraintIndex = ConstraintIndex+1
            ConstraintPointer = ConstraintPointers(ConstraintIndex)
            OK = StoreAtLocation( ConstraintPointer, &
                ConstraintCoef(I+2) )

            !-----------water-surface coeeficient.
            ConstraintIndex = ConstraintIndex+1
            ConstraintPointer = ConstraintPointers(ConstraintIndex)
            OK = StoreAtLocation( &
                ConstraintPointer, ConstraintCoef(I+3) )

        end do

    end if
    SetUserStreamConstraintCoef = .true.

    return
end function SetUserStreamConstraintCoef

end module
