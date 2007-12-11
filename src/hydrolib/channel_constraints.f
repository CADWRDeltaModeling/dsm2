C!    Copyright (C) 1996-1999 State of California,
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
C!    DSM2 - SPARSE LIBRARY INTERFACE COPYRIGHT
C!    Copyright (C) 1998-1999 Eli Ateljevich
C!
C!    Note that the routines below which contain part of an interface to
C!    the SPARSE matrix library were created by Eli Ateljevich.
C!
C!    The SPARSE matrix library was created by Kenneth S. Kundert and
C!    the University of California for which copyright information is
C!    given below.

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
C!
C!    For information about the solver routines, contact:
C!    Eli Ateljevich
C!    (510) 843-1569
C!
*****-SPARSE COPYRIGHT *************
*  Revision and copyright information.
*
*  Copyright (c) 1985,86,87,88
*  by Kenneth S. Kundert and the University of California.
*
*  Permission to use, copy, modify, and distribute this software and
*  its documentation for any purpose and without fee is hereby granted,
*  provided that the copyright notices appear in all copies and
*  supporting documentation and that the authors and the University of
*  California are properly credited.  The authors and the University of
*  California make no representations as to the suitability of this
*  software for any purpose.  It is provided `as is', without express
*  or implied warranty.

************************************************************************
*
*
*
************************************************************************

*==== BOF chcnstrt =====================================================

*     Routines for calculating external boundary conditions and internal
*     compatibility equations for channels in the network, including putting
*     corresponding equations in the computational matrix.
*     

*== Public (ForceStreamSurface) ========================================

      LOGICAL FUNCTION ForceStreamSurface( Extremity )

      IMPLICIT NONE

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'solver.inc'
      INCLUDE 'chconnec.inc'

*   Purpose:  Compute and store constraint-equation coefficients
*             forcing a known stream surface elevation.

*   Arguments:
      INTEGER Extremity

*   Argument definitions:
*     Extremity - index indicating
*                   [+1] upstream end of current channel.
*                   [-1] downstream end of current channel.

*   Module data:

*   Local Variables:
      INTEGER Row, ConstraintNode
      INTEGER Channel, ConstraintIndex,ConstraintPointer

      REAL*8 One, DesiredElevation
      PARAMETER ( One = 1.0 )
      LOGICAL OK

*   Routines by module:

***** Channel flow status:
      REAL*8     StreamSurfaceElevation
      EXTERNAL StreamSurfaceElevation

***** Channel schematic:
      INTEGER  NumberOfStreamLocations,CurrentChannel
      EXTERNAL NumberOfStreamLocations,CurrentChannel

***** Network control:
      REAL*8     UpstreamBoundaryValue, DownstreamBoundaryValue
      EXTERNAL UpstreamBoundaryValue, DownstreamBoundaryValue

***** Solver:
      INTEGER  UpstreamConstraintRow, DownstreamConstraintRow
      EXTERNAL UpstreamConstraintRow, DownstreamConstraintRow

      LOGICAL  StoreAtLocation, StoreAtRow
      EXTERNAL StoreAtLocation, StoreAtRow

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------
      Channel=CurrentChannel()

      IF( Extremity .EQ. +1 ) THEN
*--------Upstream constraint.
         ConstraintNode = 1
         Row = UpstreamConstraintRow()

         ConstraintIndex = UpConstraintIndex(Channel)
         ConstraintPointer = ConstraintPointers(ConstraintIndex)

         OK = StoreAtLocation( ConstraintPointer, One )
         DesiredElevation = UpstreamBoundaryValue()

      ELSE IF( Extremity .EQ. -1 ) THEN

*--------Downstream constraint.
         ConstraintNode = NumberOfStreamLocations()
         Row = DownstreamConstraintRow()
         ConstraintIndex = DownConstraintIndex(Channel)
         ConstraintPointer = ConstraintPointers(ConstraintIndex)

         OK = StoreAtLocation(ConstraintPointer, One )
         DesiredElevation = DownstreamBoundaryValue()

      END IF

      OK = StoreAtRow( Row,
     &     DesiredElevation - StreamSurfaceElevation( ConstraintNode )
     &     )

      ForceStreamSurface = .TRUE.

      RETURN
      END

*== Public (ForceStreamFlow) ===========================================

      LOGICAL FUNCTION ForceStreamFlow( Extremity )

      IMPLICIT NONE

*   Purpose:  Compute and store constraint-equation coefficients
*             forcing a known streamflow.

*   Arguments:
      INTEGER Extremity

*   Argument definitions:
*     Extremity - index indicating
*                   [+1] upstream end of current channel.
*                   [-1] downstream end of current channel.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chconnec.inc'
      INCLUDE 'solver.inc'

*   Local Variables:
      INTEGER Row, ConstraintNode, Channel
      INTEGER ConstraintIndex,ConstraintPointer
      REAL*8    One, MinusOne, DesiredFlow
      PARAMETER ( One = 1.0, MinusOne = -1.0 )
      LOGICAL OK

*   Routines by module:

***** Channel flow status:
      REAL*8     StreamFlow
      EXTERNAL StreamFlow

***** Channel schematic:
      INTEGER  NumberOfStreamLocations
      EXTERNAL NumberOfStreamLocations

      INTEGER CurrentChannel
      EXTERNAL CurrentChannel

***** Network control:
      REAL*8     UpstreamBoundaryValue, DownstreamBoundaryValue,StreamSurfaceElevation
      EXTERNAL UpstreamBoundaryValue, DownstreamBoundaryValue,StreamSurfaceElevation

***** Solver:
      INTEGER  UpstreamConstraintRow, DownstreamConstraintRow
      EXTERNAL UpstreamConstraintRow, DownstreamConstraintRow

      LOGICAL  StoreAtLocation, StoreAtRow
      EXTERNAL StoreAtLocation, StoreAtRow

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

*-----Note: flow conservation is written from the point of
*      view of the node receiving the flow.

      Channel=CurrentChannel()

      IF( Extremity .EQ. +1 ) THEN

*--------Note: flow conservation is written from the point of
*      view of the node receiving the flow.

*--------Upstream constraint.

         ConstraintNode = 1
         Row = UpstreamConstraintRow()
         ConstraintIndex = UpConstraintIndex(Channel)
         ConstraintPointer = ConstraintPointers(ConstraintIndex)
         DesiredFlow = - UpstreamBoundaryValue() + StreamFlow( ConstraintNode )
         OK = StoreAtLocation( ConstraintPointer, MinusOne )

      ELSE IF( Extremity .EQ. -1 ) THEN
*--------Downstream constraint.

         ConstraintNode = NumberOfStreamLocations()
         Row = DownstreamConstraintRow()
         ConstraintIndex = DownConstraintIndex(Channel)
         ConstraintPointer = ConstraintPointers(ConstraintIndex)
         DesiredFlow = DownStreamBoundaryValue() - StreamFlow( ConstraintNode )
         OK = StoreAtLocation( ConstraintPointer,One)
      END IF

      OK = StoreAtRow(Row, DesiredFlow)

      ForceStreamFlow = .TRUE.

      RETURN
      END

*== Public (ForceSumStreamFlow) ========================================

      LOGICAL FUNCTION ForceSumStreamFlow( Extremity )
      Use IO_Units
      IMPLICIT NONE

*   Purpose:  Compute and store constraint-equation coefficients
*             forcing the sum of stream flows at a location to zero.

*   Arguments:
      INTEGER Extremity

*   Argument definitions:
*     Extremity - index indicating
*                   [+1] upstream end of current channel.
*                   [-1] downstream end of current channel.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'solver.inc'
      INCLUDE 'chconnec.inc'

*   Local Variables:
      INTEGER MaxConnections
      INTEGER Channel,ConstraintIndex,ConstraintPointer
      PARAMETER ( MaxConnections = MaxConnectingChannels )
      INTEGER I, ConnectingNodes, Node(MaxConnections)
      INTEGER ConstraintNode, DF
      INTEGER Row, AbsNode
      REAL*8   One, MinusOne, Residual, Sign
      PARAMETER ( MinusOne=-1.0, One = 1.0, DF = 2 )
      LOGICAL OK

*   Routines by module:

      INTEGER  NetworkTimeStep, NetworkTimeIncrement, NetworkIteration
      EXTERNAL NetworkTimeStep, NetworkTimeIncrement, NetworkIteration

***** Channel flow status:
      REAL*8     GlobalStreamFlow
      EXTERNAL GlobalStreamFlow

***** Channel schematic:
      INTEGER  CurrentChannel
      EXTERNAL CurrentChannel

      INTEGER  UpstreamPointer, DownstreamPointer
      EXTERNAL UpstreamPointer, DownstreamPointer

      INTEGER  UpstreamConnect, UpstreamConnections
      EXTERNAL UpstreamConnect, UpstreamConnections

      INTEGER  DownstreamConnect, DownstreamConnections
      EXTERNAL DownstreamConnect, DownstreamConnections

      INTEGER  StreamEndNode
      EXTERNAL StreamEndNode

***** Network control:
      REAL*8     UpstreamBoundaryValue, DownstreamBoundaryValue
      EXTERNAL UpstreamBoundaryValue, DownstreamBoundaryValue

      REAL*8     GlobalStreamSurfaceElevation
      EXTERNAL GlobalStreamSurfaceElevation

***** Solver:
      INTEGER  UpstreamConstraintRow, DownstreamConstraintRow
      EXTERNAL UpstreamConstraintRow, DownstreamConstraintRow

      LOGICAL  StoreAtLocation, StoreAtRow, AddAtRow
      EXTERNAL StoreAtLocation, StoreAtRow, AddAtRow

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:   Lew DeLong
*   Last modified: August   1992
*   Modified by:   Parviz Nader
*   Last modified: December 1992
*   Modified by:   Eli Ateljevich
*   Last modified: July 1998
*   Version 93.01, January, 1993

*   Intrinsics:
      INTEGER   IABS
      INTRINSIC IABS, DFLOAT

*-----Implementation -----------------------------------------------------

      ForceSumStreamFlow = .FALSE.

*-----Note, by convention, any flow into a junction is positive.
*     Constraint equation is written from the perspective of
*     the junction.
      Channel=CurrentChannel()
      IF( Extremity .EQ. +1 ) THEN

*--------Upstream constraint.
         ConstraintNode = UpstreamPointer()
         Row = UpstreamConstraintRow()
         ConstraintIndex = UpConstraintIndex(Channel)

         ConstraintPointer = ConstraintPointers(ConstraintIndex)

         OK = StoreAtLocation( ConstraintPointer, MinusOne )
         OK = AddAtRow( Row, - UpstreamBoundaryValue() )

         ConnectingNodes = UpstreamConnections()
         IF( ConnectingNodes .LE. MaxConnections ) THEN
         ELSE
            WRITE(UNIT_ERROR,*) ' ####error(ForceSumStreamFlow)'
            WRITE(UNIT_ERROR,*) ' Upstream end Channel...', CurrentChannel()
            WRITE(UNIT_ERROR,*) ' Number of connections (',ConnectingNodes,')'
            WRITE(UNIT_ERROR,*) ' exceeds maximum (',MaxConnections,').'
            WRITE(UNIT_ERROR,*) ' Abnormal program end.'
            CALL EXIT(1)
         END IF

         IF( ConnectingNodes .GT. 0 ) THEN
            DO 100 I=1,ConnectingNodes
               Node(I) = StreamEndNode( UpstreamConnect(I) )
 100        CONTINUE
         ELSE
         END IF

      ELSE IF( Extremity .EQ. -1 ) THEN

*--------Downstream constraint.

         ConstraintNode = DownstreamPointer()
         Row = DownstreamConstraintRow()
         ConstraintIndex = DownConstraintIndex(Channel)
         ConstraintPointer = ConstraintPointers(ConstraintIndex)

         OK = StoreAtLocation( ConstraintPointer, One )

         OK = AddAtRow( Row, -DownstreamBoundaryValue() )

         ConnectingNodes = DownstreamConnections()
         IF( ConnectingNodes .LE. MaxConnections ) THEN
         ELSE
            WRITE(UNIT_ERROR,*) ' ####error(ForceSumStreamFlow)'
            WRITE(UNIT_ERROR,*) ' Downstream end Channel...', CurrentChannel()
            WRITE(UNIT_ERROR,*) ' Number of connections (',ConnectingNodes,')'
            WRITE(UNIT_ERROR,*) ' exceeds maximum (',MaxConnections,').'
            WRITE(UNIT_ERROR,*) ' Abnormal program end.'
            CALL EXIT(1)
         END IF
         IF( ConnectingNodes .GT. 0 ) THEN
            DO 200 I=1,ConnectingNodes
               Node(I) = StreamEndNode( DownstreamConnect(I) )
 200        CONTINUE
         END IF

      ELSE
         WRITE(UNIT_ERROR,*) ' *** Error (ForceSumStreamFlow)'
         WRITE(UNIT_ERROR,*) ' Extremity not equal +1 or -1 ...'
         RETURN
      END IF

      Residual = 0.0

*-----Connecting Nodes
      IF( ConnectingNodes .GT. 0 ) THEN

         DO 300 I=1,ConnectingNodes

            AbsNode = IABS(Node(I))
            If (Node(i).gt.0) Then
               Sign = -1.0
            Else
               Sign = 1.0
            End If
            ConstraintIndex = ConstraintIndex + 1
	    ConstraintPointer = ConstraintPointers(ConstraintIndex)

            OK = StoreAtLocation( ConstraintPointer,  Sign )

            Residual = Residual
     &           + Sign * GlobalStreamFlow( AbsNode )

 300     CONTINUE

      END IF

      IF( Extremity .EQ. +1 ) THEN

         OK = AddAtRow(
     &        Row, + GlobalStreamFlow( ConstraintNode ) - Residual )
      ELSE
         OK = AddAtRow(
     &        Row, - GlobalStreamFlow( ConstraintNode ) - Residual )
      END IF

      ForceSumStreamFlow = .TRUE.

      RETURN
      END


*== Public (ForceEqualStreamSurface) ===================================

      LOGICAL FUNCTION ForceEqualStreamSurface( Extremity )
      use IO_Units
      IMPLICIT NONE

*   Purpose:  Compute and store constraint-equation coefficients forcing
*             equal stream-surface elevations at two cross sections.

*   Arguments:
      INTEGER Extremity

*   Argument definitions:
*     Extremity - index indicating
*                   [+1] upstream end of current channel.
*                   [-1] downstream end of current channel.

*   Module data:

      INCLUDE 'network.inc'
      INCLUDE 'chconnec.inc'
      INCLUDE 'solver.inc'

*   Local Variables:
      INTEGER First
      PARAMETER (First = 1)
      INTEGER ConnectingNodes
      INTEGER ConstraintNode, DF
      INTEGER Zero, Row, Node
      INTEGER ConnectingChannel
      INTEGER Channel,ConstraintIndex,ConstraintPointer

      REAL*8   One, MinusOne, Fall
      PARAMETER ( Zero = 0, One = 1.0, MinusOne = -1.0, DF = 2 )
      LOGICAL OK

*   Routines by module:

***** Channel flow status:
      REAL*8     GlobalStreamSurfaceElevation
      EXTERNAL GlobalStreamSurfaceElevation

***** Channel schematic:
      INTEGER  UpstreamPointer, DownstreamPointer,CurrentChannel
      EXTERNAL UpstreamPointer, DownstreamPointer,CurrentChannel

      INTEGER  UpstreamConnect, UpstreamConnections
      EXTERNAL UpstreamConnect, UpstreamConnections

      INTEGER  DownstreamConnect, DownstreamConnections
      EXTERNAL DownstreamConnect, DownstreamConnections

      INTEGER  StreamEndNode
      EXTERNAL StreamEndNode

***** Network control:
      REAL*8     UpstreamBoundaryValue, DownstreamBoundaryValue
      EXTERNAL UpstreamBoundaryValue, DownstreamBoundaryValue

***** Solver:
      INTEGER  UpstreamConstraintRow, DownstreamConstraintRow
      EXTERNAL UpstreamConstraintRow, DownstreamConstraintRow

      LOGICAL  StoreAtLocation, StoreAtRow
      EXTERNAL StoreAtLocation, StoreAtRow

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:   Lew DeLong
*   Last modified: August   1992
*   Modified by:   Eli Ateljevich
*   Last modified: July  1998
*   Version 93.01, January, 1993

*   Intrinsics:
      integer   IABS
      intrinsic IABS

*-----Implementation -----------------------------------------------------
      Channel = CurrentChannel()
      ForceEqualStreamSurface = .FALSE.

      if( Extremity .EQ. +1 ) then

*--------Upstream constraint.

         ConstraintNode = UpstreamPointer()
         Row = UpstreamConstraintRow()
         ConstraintIndex = UpConstraintIndex(Channel)
         ConstraintPointer = ConstraintPointers(ConstraintIndex)

         OK = StoreAtLocation( ConstraintPointer, One )
         Fall = UpstreamBoundaryValue()
         ConnectingNodes = UpstreamConnections()
         ConnectingChannel = UpstreamConnect(First)

      else if( Extremity .EQ. -1 ) then

*--------Downstream constraint.


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

*-----Determine node number of attached channel.

      IF( ConnectingChannel .NE. 0 ) THEN

         Node = IABS( StreamEndNode( ConnectingChannel ) )

      ELSE

*--------No channel number ... not actually attached.

         OK = StoreAtRow(
     &        Row, Fall - GlobalStreamSurfaceElevation( ConstraintNode )
     &        )

      END IF

      if (Channel .eq. 402 .or. Channel .eq. 403 ) then
c      print*,"Other node: ", GlobalStreamSurfaceElevation(Node), " this node: ", 
c     &        GlobalStreamSurfaceElevation(ConstraintNode), "Ref node: ",GlobalStreamSurfaceElevation(
c     &       IABS(StreamEndNode(346)))
      end if

      IF( ConnectingNodes .GT. 0 ) THEN
         ConstraintIndex = ConstraintIndex + 1
         ConstraintPointer = ConstraintPointers(ConstraintIndex)

         OK = StoreAtLocation( ConstraintPointer, MinusOne )

         OK = StoreAtRow(
     &        Row,
     &        Fall
     &        + GlobalStreamSurfaceElevation( Node )
     &        - GlobalStreamSurfaceElevation( ConstraintNode )
     &        )

      ELSE

         WRITE(UNIT_ERROR,*) ' *** Error (ForceEqualStreamSurface)'
         WRITE(UNIT_ERROR,*) ' Connections = ', ConnectingNodes

      END IF

      ForceEqualStreamSurface = .TRUE.

      RETURN
      END

*== Private (SetStreamConstraintVariables) =============================

      LOGICAL FUNCTION SetStreamConstraintVariables( Extremity )
      use IO_Units
      IMPLICIT NONE

*   Purpose:  Set value of variables that may be used by
*             user-programmed constraints.

*   Arguments:
      INTEGER Extremity

*   Argument definitions:
*     Extremity - index indicating
*                   [+1] upstream end of current channel.
*                   [-1] downstream end of current channel.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'strmcnst.inc'


*   Local Variables:
      INTEGER I
      INTEGER ConnectingNodes
      INTEGER ConstraintNode
      INTEGER ConnectingNode
      INTEGER ConnectingChannel

*   Routines by module:

***** Channel flow status:
      REAL*8     GlobalStreamSurfaceElevation
      REAL*8     GlobalStreamFlow
      EXTERNAL GlobalStreamSurfaceElevation, GlobalStreamFlow

***** Channel schematic:
      INTEGER  UpstreamPointer, DownstreamPointer
      EXTERNAL UpstreamPointer, DownstreamPointer

      INTEGER  UpstreamConnect, UpstreamConnections
      EXTERNAL UpstreamConnect, UpstreamConnections

      INTEGER  DownstreamConnect, DownstreamConnections
      EXTERNAL DownstreamConnect, DownstreamConnections

      INTEGER  StreamEndNode, CurrentChannel, DownstreamCode
      EXTERNAL StreamEndNode, CurrentChannel, DownstreamCode

      INTEGER  UpstreamCode
      EXTERNAL UpstreamCode

*   Intrinsics:
      INTEGER   IABS
      INTRINSIC IABS

*   Programmed by: Lew DeLong
*   Date:          Oct   1992
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      SetStreamConstraintVariables = .FALSE.

      ConnectingExtremity(1) = Extremity

      IF( Extremity .EQ. +1 ) THEN

*--------Upstream constraint.

         ConstraintNode = UpstreamPointer()
         ConnectingNodes = UpstreamConnections()
         ConditionCode = UpstreamCode()

      ELSE IF( Extremity .EQ. -1 ) THEN

*--------Downstream constraint.

         ConstraintNode = DownstreamPointer()
         ConnectingNodes = DownstreamConnections()
         ConditionCode = DownstreamCode()

      ELSE
         WRITE(UNIT_ERROR,*) ' *** Error(SetStreamConstraintVariables)'
         WRITE(UNIT_ERROR,*) ' Channel...',CurrentChannel()
         WRITE(UNIT_ERROR,*) ' Extremity not equal +1 or -1 ...'
         RETURN
      END IF

      ConnectingChannels = ConnectingNodes
      Discharge(1) = GlobalStreamFlow( ConstraintNode )
      WSElev(1) = GlobalStreamSurfaceElevation( ConstraintNode )

      IF( ConnectingNodes .GT. 0 ) THEN
         IF( ConnectingNodes .LE. MaxConnectingChannels ) THEN

            IF( Extremity .EQ. +1 ) THEN

               DO 100 I=1,ConnectingNodes

                  ConnectingChannel = UpstreamConnect(I)

                  ConnectingNode =
     &                 IABS( StreamEndNode( ConnectingChannel ) )

                  Discharge(I+1) = GlobalStreamFlow( ConnectingNode )

                  WSElev(I+1) =
     &                 GlobalStreamSurfaceElevation( ConnectingNode )

                  ConnectingExtremity(I+1) =
     &                 ConnectingChannel / IABS( ConnectingChannel )

 100           CONTINUE

            ELSE

               DO 200 I=1,ConnectingNodes

                  ConnectingChannel = DownstreamConnect(I)

                  ConnectingNode =
     &                 IABS( StreamEndNode( ConnectingChannel ) )

                  Discharge(I+1) = GlobalStreamFlow( ConnectingNode )

                  WSElev(I+1) =
     &                 GlobalStreamSurfaceElevation( ConnectingNode )

                  ConnectingExtremity(I+1) =
     &                 ConnectingChannel / IABS( ConnectingChannel )

 200           CONTINUE

            END IF

         ELSE
            WRITE(UNIT_ERROR,*) ' ####Error(SetStreamConstraintVariables)'
            WRITE(UNIT_ERROR,*) ' Channel...',CurrentChannel()
            WRITE(UNIT_ERROR,*) ' Number of connections must be fewer than...',
     &           MaxConnectingChannels
            WRITE(UNIT_ERROR,*) ' Currently, connections = ', ConnectingNodes
            RETURN
         END IF

      END IF

*-----Initialize global solution coefficients.

      ConstraintRightSide = 0.0

      DO 300 I=1,2+2*MaxConnectingChannels

         ConstraintCoef(I) = 0.0

 300  CONTINUE

      SetStreamConstraintVariables = .TRUE.

      RETURN
      END

*== Private (SetUserStreamConstraintCoef) ===============================

      LOGICAL FUNCTION SetUserStreamConstraintCoef()
      use IO_Units
      IMPLICIT NONE

*   Purpose:  Place coefficients computed by specific user-programmed
*             constraints in global solution matrix.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'strmcnst.inc'
      INCLUDE 'chconnec.inc'
      INCLUDE 'solver.inc'


*   Local Variables:
      INTEGER Extremity, Offset, Row
      INTEGER Channel,ConstraintIndex,ConstraintPointer
      INTEGER I, ConnectingNodes, Node(MaxConnectingChannels)
      INTEGER ConstraintNode
      LOGICAL OK

*   Routines by module:

***** Local:

***** Channel schematic:
      INTEGER  CurrentChannel
      EXTERNAL CurrentChannel

      INTEGER  UpstreamPointer, DownstreamPointer
      EXTERNAL UpstreamPointer, DownstreamPointer

      INTEGER  UpstreamConnect, UpstreamConnections
      EXTERNAL UpstreamConnect, UpstreamConnections

      INTEGER  DownstreamConnect, DownstreamConnections
      EXTERNAL DownstreamConnect, DownstreamConnections

      INTEGER  StreamEndNode
      EXTERNAL StreamEndNode

***** Solver:
      INTEGER  UpstreamConstraintRow, DownstreamConstraintRow
      EXTERNAL UpstreamConstraintRow, DownstreamConstraintRow

      LOGICAL  StoreAtLocation, StoreAtRow
      EXTERNAL StoreAtLocation, StoreAtRow

*   Intrinsics:
      INTEGER   IABS, MOD
      INTRINSIC IABS, MOD

*   Programmed by: Lew DeLong
*   Date:          Oct   1992
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------
      SetUserStreamConstraintCoef = .FALSE.

      Channel=CurrentChannel()

      Extremity = ConnectingExtremity(1)

      IF( Extremity .EQ. +1 ) THEN

*--------Upstream constraint.

         ConstraintNode = UpstreamPointer()
         Row = UpstreamConstraintRow()

 75      Continue
         ConstraintIndex = UpConstraintIndex(Channel)

         ConstraintPointer = ConstraintPointers(ConstraintIndex)

         ConnectingNodes = UpstreamConnections()
         IF( ConnectingNodes .GT. 0 ) THEN
            DO 100 I=1,ConnectingNodes
               Node(I) = IABS(StreamEndNode( UpstreamConnect(I) ))
 100        CONTINUE
         ELSE
         END IF

      ELSE IF( Extremity .EQ. -1 ) THEN

*--------Downstream constraint.

         ConstraintNode = DownstreamPointer()
         Row = DownstreamConstraintRow()
         ConstraintIndex = DownConstraintIndex(Channel)
         ConstraintPointer = ConstraintPointers(ConstraintIndex)

         ConnectingNodes = DownstreamConnections()
         IF( ConnectingNodes .GT. 0 ) THEN
            DO 200 I=1,ConnectingNodes
               Node(I) = IABS(StreamEndNode( DownstreamConnect(I) ))
 200        CONTINUE
         ELSE
         END IF

      ELSE
         WRITE(UNIT_ERROR,*) ' *** Error(SetUserStreamConstraintCoef)'
         WRITE(UNIT_ERROR,*) ' Extremity not equal +1 or -1 ...'
         WRITE(UNIT_ERROR,*) ' Channel...', CurrentChannel()
         RETURN
      END IF

*-----Determine if flow or water-surface constraint, set offset.

      IF( MOD( ConditionCode, 10 ) .EQ. 2 ) THEN
         Offset = 0
      ELSE
         Offset = -1
      END IF

*-----Set coefficients for constraint node.

*-----Constraint-node coefficients for

*          discharge, and
      OK = StoreAtLocation( ConstraintPointer, ConstraintCoef(1) )

*          water surface.
      ConstraintIndex = ConstraintIndex+1
      ConstraintPointer = ConstraintPointers(ConstraintIndex)
      OK = StoreAtLocation( ConstraintPointer, ConstraintCoef(2) )

*-----Right-hand side.

      OK = StoreAtRow( Row, ConstraintRightSide )

      IF( ConnectingNodes .GT. 0 ) THEN

         DO 300 I=1,ConnectingNodes

*-----------discharge coefficient.
            ConstraintIndex = ConstraintIndex+1
            ConstraintPointer = ConstraintPointers(ConstraintIndex)
            OK = StoreAtLocation( ConstraintPointer,
     &           ConstraintCoef(I+2) )

*-----------water-surface coeeficient.
            ConstraintIndex = ConstraintIndex+1
            ConstraintPointer = ConstraintPointers(ConstraintIndex)
            OK = StoreAtLocation(
     &           ConstraintPointer, ConstraintCoef(I+3) )

 300     CONTINUE

      END IF
      SetUserStreamConstraintCoef = .TRUE.

      RETURN
      END

