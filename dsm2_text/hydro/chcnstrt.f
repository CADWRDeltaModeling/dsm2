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

*   Module data:

*  'network.inc'
*     MaxChannels - maximum number of channels.
*     NumCh - current number of channels.
*     Branch - current selected or active channel.
*     MaxLocations - maximum number of computational or user locations.

*  'chcnstrt.inc'
*     Reverse(i) - .True. if a companion 3-parameter table exists for
*                  negative flow, false otherwise.  Refers to upstream
*                  end of channel if i is odd, downstream if i is even.
*                  Channel number is INT( (i + 1) / 2 ).

*     ParmRating3 - .TRUE. if any 3-parameter ratings exist, .FALSE.
*                   otherwise.
*

*     UserConstraint - .TRUE. if any user-programmed constraints
*                      exist, false otherwise.

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

      include '../input/fixed/misc.f'

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
         OK = StoreAtRow( Row, - UpstreamBoundaryValue() )

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

         OK = StoreAtRow( Row, -DownstreamBoundaryValue() )

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

*== Public (CalculateReservoirFlow) ===================================

      LOGICAL FUNCTION CalculateReservoirFlow()

      IMPLICIT NONE

*   Purpose:  Compute flow between reservoirs and channels

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chconnec.inc'
      INCLUDE 'solver.inc'
      INCLUDE '../input/fixed/common.f'

*   Local Variables:
      integer lnblnk            ! intrinsic
      character*14 last_current_dt ! last date stamp of reservoir inflow msg
      INTEGER ChannelNode,ChannelConnect,ChannelRow
      INTEGER ResIndex,ResRow,ObjIndex
      INTEGER i,j,k,l,DF
      Parameter(DF = 2)
      INTEGER Iteration
      REAL*8 dQobjdZ1,Theta     ! MUST BE REAL*8, passed directly to sparse
      Real*8 dQresdZ            ! ...This is the real*8 scalar version for sparse
      REAL*8 ResCoeff1,ResCoeff2
      REAL*8 dVdtKnown
      REAL*8 knownresflow,knownobjflow

      REAL*8 y1,y2,dy,dT
      REAL*8 resopcoeff           ! reservoir operating coefficient, for active gate
      logical OK

      Real*8 SplineThresh,Mult,S1,S2,S3
      Parameter(SplineThresh=1.E-2,Mult=2.0) ! SplineThresh: Threshold at which spline
                                             ! is used to appoximate reservoir equation

*   Routines by module:

      INTEGER  NetworkTimeStep, NetworkTimeIncrement,
     &     NetworkIteration,
     &     NumberOfChannels,StreamEndNode

      EXTERNAL NetworkTimeStep, NetworkTimeIncrement,
     &     NetworkIteration,
     &     NumberOfChannels,StreamEndNode

      REAL*8     GlobalStreamSurfaceElevation,NetworkTheta
      EXTERNAL GlobalStreamSurfaceElevation,NetworkTheta

      REAL*8 reservoir_source_sink,reservoir_source_sink_prev
      EXTERNAL reservoir_source_sink,reservoir_source_sink_prev

      LOGICAL AddAtLocation,StoreAtRow,AddAtRow,ForwardElim
      External AddAtLocation,StoreAtRow,AddAtRow,ForwardElim

      LOGICAL First             ! first time routine called
      DATA First /.true./

*   Programmed by: Parviz Nader
*   Date:          August 1992
*   Modified by:   Ralph Finch
*   Last modified: November 1997

*-----Implementation -----------------------------------------------------

      CalculateReservoirFlow=.FALSE.
      dT=NetworkTimeIncrement()
      Theta = NetworkTheta()
      Iteration = NetworkIteration()

      DO i=1,Nres

         ResIndex = ResEqIndex(i)
         ResRow = TotalNonResRows + i

*--------The next two things must only be done once:
*--------Add the "dV/dt" part of Sum Q = dV/dt

         If (Iteration.eq.1)InitialYRes(i) = Yres(i)
         dy = Yres(i) - InitialYRes(i)
         dVdtknown = dy*ARes(i)/dt
         OK = StoreAtRow(ResRow,-dVdtknown)
         OK = AddAtLocation(ResEqPointer(ResIndex),ARes(i)/dt)
         ResIndex = ResIndex+1
*--------Add contributions from external sources. These
*--------are fixed and weighted in time. Note sources and sinks
*--------are positive into reservoir.
         KnownResFlow=theta * reservoir_source_sink(i,QEXT_FLOWS) +
     &        (1.-theta) * reservoir_source_sink_prev(i,QEXT_FLOWS)

         OK = AddAtRow(ResRow, KnownResFlow)

         DO j=1,NconnectReservoir(i)
            if (ReservoirGate(i,j) .eq. 0) then
               resopcoeff=1.0
            else
               resopcoeff=GateOperatingCoeff(ReservoirGate(i,j))
            endif

            ChannelConnect=ResConnectingChannels(i,j)
            ChannelNode = StreamEndNode( ChannelConnect )
            ChannelRow = DF * ChannelNode - 1

            y1=GlobalStreamSurfaceElevation(ChannelNode)
            y2=Yres(i)

            dy = y2 - y1

            IF (dy.GT.0) THEN

*--------------Flow from the reservoir to the channel

               ResCoeff2 = resopcoeff*ReservoirCoeff(i,j,2)

               If(dy .gt. SplineThresh)Then
*-----------------normal case, use orifice equation
                  QRes(i,j)=ResCoeff2*SQRT(dy)
                  dQResdZRes(i,j) = 0.5 * ResCoeff2/SQRT(dy)
               Else
*-----------------pathological case where derivative ->inf.
c-----------------Use spline, which requires knowing ResCoeff1 and ResCoeff2
                  If (Abs(ResCoeff2) .LT. 1.E-4)Then
                     QRes(i,j) = 0.
                     dQResdZRes(i,j) = 0.
                  Else
                     ResCoeff1 = resopcoeff*ReservoirCoeff(i,j,1)
                     If(Abs(ResCoeff1) .LT. 1.E-4)ResCoeff1=ResCoeff2
                     If (ResCoeff2.GE.ResCoeff1)Then
                        s1 = (0.5*Mult*ResCoeff2)/sqrt(SplineThresh)
                        s2 = ResCoeff2*(2.5 - Mult)/SplineThresh**1.5
                        s3 = (0.5*ResCoeff2*(Mult-3.))/SplineThresh**2.5
                     Else
                        s1 = (0.5*Mult*ResCoeff1)/sqrt(SplineThresh)
                        s2 = (2.5*ResCoeff2 - Mult*ResCoeff1)/SplineThresh**1.5
                        s3 = (-1.5*ResCoeff2 +0.5*Mult*ResCoeff1)/SplineThresh**2.5
                     End If

                     QRes(i,j) = S1*dy + S2*dy**2.+ S3*dy**3.
     &
                     dQResDZRes(i,j) = S1 + 2*S2*dy
     &                    + 3.*S3*dy**2.
                  End If

               End If

            ELSE

*--------------Flow from channel to the reservoir
               ResCoeff1 = ReservoirCoeff(i,j,1)*resopcoeff

               If(dy .lt. -SplineThresh) Then
*-----------------normal case, use orifice equation
                  QRes(i,j)=-ResCoeff1*SQRT(-dy)
                  dQResdZRes(i,j) = 0.5 * ResCoeff1/SQRT(-dy)

               Else
*-----------------pathological case where derivative ->inf.
*-----------------Use spline.
                  If (Abs(ResCoeff1) .LT. 1.E-4)Then
                     QRes(i,j) = 0.
                     dQResdZRes(i,j) = 0.
                  Else
                     ResCoeff2 = resopcoeff*ReservoirCoeff(i,j,1)
                     If (Abs(ResCoeff2) .LT. 1.E-4)ResCoeff2 = ResCoeff1
                     If (ResCoeff1.GE.ResCoeff2)Then
                        s1 = (0.5*Mult*ResCoeff1)/sqrt(SplineThresh)
                        s2 = ResCoeff1*(Mult - 2.5)/SplineThresh**1.5
                        s3 = (0.5*ResCoeff1*(Mult-3.))/SplineThresh**2.5
                     Else
                        s1 = (0.5*Mult*ResCoeff2)/sqrt(SplineThresh)
                        s2 = (Mult*ResCoeff2 - 2.5*ResCoeff1)/SplineThresh**1.5
                        s3 = (-1.5*ResCoeff1 +0.5*Mult*ResCoeff2)/SplineThresh**2.5
                     End If

                     QRes(i,j) = S1*dy + S2*dy**2.+ S3*dy**3.
     &
                     dQResDZRes(i,j) = S1 + 2*S2*dy
     &                    + 3.*S3*dy**2.
                  End If

               End if
c--------------inflow limit
c--------------internally, flows to a reservoir from channel are negative;
c--------------but the flow limit is a positive number
               if (res_geom(i).maxq2res(j) .ne. miss_val_r .and.
     &              qres(i,j) .lt. 0.0 .and.
     &              -qres(i,j) .gt. res_geom(i).maxq2res(j)) then

                  if (print_level .ge. 2 .and.
     &                 current_dt .ne. last_current_dt) then
                     last_current_dt=current_dt ! to eliminate duplicate msgs for same time
                     write(unit_error,9001)
     &                    current_dt,
     &                    res_geom(i).name(:lnblnk(res_geom(i).name)),
     &                    res_geom(i).node_no(j),abs(qres(i,j)),
     &                    res_geom(i).maxq2res(j)
 9001                format('At ',a,' reservoir ', a,
     &                    ' inflow at node ',i3
     &                    /' was ',f8.0,' cfs; reduced to',f8.0)
                  endif
                  qres(i,j)=-res_geom(i).maxq2res(j)
                  dQResdZRes(i,j) = 0.0
               endif

            ENDIF

*-----------The following will be used for time-integrated
*-----------calculations. InitialQRes is flow at the "beginning"
*-----------of the time step (ie, based on the previous iteration).
            If (Iteration.eq.1) InitialQRes(i,j) = Qres(i,j)

            KnownResFlow=theta * QRes(i,j) +
     &           (1.-theta) * InitialQRes(i,j)

*-----------Add matrix elements.
*-----------The first two are in the row corresponding to the reservoir
*-----------The second two are in the row corresponding to the channel.
*-----------Note dQResdZchan = -dQResdZRes(i,j).
            If (ForwardElim())Then
               dQResdZ = dQResdZRes(i,j)
               Call sfAdd4Reservoir(ResEqPointer(ResIndex),dQResdZ*Theta,dQResdZ)
            End If
            ResIndex = ResIndex+4

*-----------Add RHS element in the row corresponding to the channel
            OK = AddAtRow(ChannelRow,-1.*Qres(i,j))
*-----------Add RHS element in the row corresponding to the reservoir
            OK = AddAtRow(ResRow,-1.*KnownResFlow)
         ENDDO

      ENDDO

c-----account for object-to-object flows
      do i=1,nobj2obj
         ObjIndex = obj2objEqIndex(i)
         if (obj2obj(i).from.object .eq. obj_reservoir .and.
     &        obj2obj(i).to.object .eq. obj_reservoir .and.
     &        obj2obj(i).constant_value .eq. head_diff) then
c-----------Flow for this connection is governed by the head difference
            k=obj2obj(i).from.object_no
            l=obj2obj(i).to.object_no
            y1=YRes(k)
            y2=YRes(l)
c-----------checkit: sign
            if (y1 .ge. y2) then
c--------------flow in the positive direction
               obj2obj(i).flow = obj2obj(i).from.coeff * sqrt(y1-y2)
               dQobjdZ1 = 0.5 * obj2obj(i).from.coeff / sqrt(y1-y2)
            else
c--------------flow in the negative direction
               obj2obj(i).flow = -obj2obj(i).to.coeff * sqrt(y2-y1)
               dQobjdZ1 = - 0.5 * obj2obj(i).to.coeff / sqrt(y2-y1)
            endif

*-----------Add matrix elements for obj2obj flows.
*-----------The first two are in the row corresponding to "from" reservoir
*-----------The second two are in the row corresponding to "to" reservoir
*-----------Note dQobjdZ2 = -dQobjdZ1
            Call sfAdd4Real(obj2objEqPointer(objIndex),dQobjdZ1 * theta)
         End If

         KnownObjFlow=theta * obj2obj(i).flow +
     &      (1.-theta) * obj2obj(i).prev_flow
*--------Add obj2obj elements to RHS. Do simultaneously for
*--------both head-dependent and non-head-dependent transfers
*--------Reservoir mass conservation is written with flow OUT
*--------of reservoir positive.
         if (obj2obj(i).from.object .eq. obj_reservoir) then
            ResRow = TotalNonResRows + obj2obj(i).from.object_no
            OK = AddAtRow(ResRow, -KnownObjFlow)
         end if

         if (obj2obj(i).to.object .eq. obj_reservoir) then
            ResRow = TotalNonResRows + obj2obj(i).to.object_no
            OK = AddAtRow(ResRow, KnownObjFlow)
         end if
      enddo
      Branch = 0
      CalculateReservoirFlow=.TRUE.
      RETURN
      END

*== Public (ForceEqualStreamSurface) ===================================

      LOGICAL FUNCTION ForceEqualStreamSurface( Extremity )

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

      include '../input/fixed/misc.f'
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
      INTEGER   IABS
      INTRINSIC IABS

*-----Implementation -----------------------------------------------------
      Channel = CurrentChannel()
      ForceEqualStreamSurface = .FALSE.

      IF( Extremity .EQ. +1 ) THEN

*--------Upstream constraint.

         ConstraintNode = UpstreamPointer()
         Row = UpstreamConstraintRow()
         ConstraintIndex = UpConstraintIndex(Channel)
         ConstraintPointer = ConstraintPointers(ConstraintIndex)

         OK = StoreAtLocation( ConstraintPointer, One )
         Fall = UpstreamBoundaryValue()
         ConnectingNodes = UpstreamConnections()
         ConnectingChannel = UpstreamConnect(First)

      ELSE IF( Extremity .EQ. -1 ) THEN

*--------Downstream constraint.

         ConstraintNode = DownstreamPointer()
         Row = DownstreamConstraintRow()
         ConstraintIndex =DownConstraintIndex(Channel)
         ConstraintPointer = ConstraintPointers(ConstraintIndex)

         OK = StoreAtLocation( ConstraintPointer, One )
         Fall = DownstreamBoundaryValue()
         ConnectingNodes = DownstreamConnections()
         ConnectingChannel = DownstreamConnect(First)

      ELSE
         WRITE(UNIT_ERROR,*) ' *** Error (ForceEqualStreamSurface)'
         WRITE(UNIT_ERROR,*) ' Extremity not equal +1 or -1 ...'
         RETURN
      END IF

*-----Determine node number of attached channel.

      IF( ConnectingChannel .NE. 0 ) THEN

         Node = IABS( StreamEndNode( ConnectingChannel ) )

      ELSE

*--------No channel number ... not actually attached.

         OK = StoreAtRow(
     &        Row, Fall - GlobalStreamSurfaceElevation( ConstraintNode )
     &        )

      END IF

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

      include '../input/fixed/misc.f'

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

      include '../input/fixed/misc.f'

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

*== Public (CalculateGateFlow) ==================================

      LOGICAL FUNCTION CalculateGateFlow()

      IMPLICIT NONE

*   Purpose:  Compute solution coefficients constraining
*             discharge to the orifice equation:
*
*             Q = C A SQRT(2g dh)
*
*             where Q  = discharge,
*                   C  = Flow coefficient
*                   A  = Flow area in the gate
*                   g  = acceleration of gravity
*                  dh  = head gradient

*   Arguments:

*   Argument definitions:
*     Extremity - index indicating
*                   [+1] upstream end of current channel.
*                   [-1] downstream end of current channel.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chconnec.inc'
      INCLUDE 'strmcnst.inc'

      include '../input/fixed/misc.f'

*   Local Variables:
      REAL*8    Q1,QGateCalc
      INTEGER GateNum,ChannelNumber
      INTEGER Extremity
      REAL*8    z1,z2
      LOGICAL OK

*   Routines by module:

***** Local:
      LOGICAL   OpenChannel, CloseChannel
      EXTERNAL  OpenChannel, CloseChannel

      LOGICAL   SetStreamConstraintVariables,SetUserStreamConstraintCoef
      EXTERNAL  SetStreamConstraintVariables,SetUserStreamConstraintCoef

      LOGICAL   GateFlow
      EXTERNAL  GateFlow

*   Intrinsics:

*   Programmed by: Parviz Nader
*   Date:          Dec   1993
*   Modified by:

*---  Implementation -----------------------------------------------------

      CalculateGateFlow = .FALSE.

      DO GateNum = 1,NGate
         ChannelNumber = GateChan(GateNum)
         if (ChannelNumber .le. 0) goto 100
         OK = OpenChannel(ChannelNumber)
         IF (GateLocation(GateNum).EQ.1) THEN
            Extremity=1
         ELSEIF (GateLocation(GateNum).EQ.2) THEN
            Extremity=-1
         ELSE
            WRITE(UNIT_ERROR,*) ' *** Error(CalculateGateFlow)'
            WRITE(UNIT_ERROR,*) ' Extremity not equal +1 or -1 ...'
            CALL EXIT(1)
         ENDIF

*--------Get required data.
         OK = SetStreamConstraintVariables(Extremity)

         Q1 =  Discharge(1)
         z1=WSElev(1)
         z2=WSElev(2)

         If (GatePosition(GateNum) .eq. GATE_FREE)Then
c-----------Gate is not operation, flow is free
c-----------node condition reverts to equal water surface
            ConstraintCoef(1) =   0.0
            ConstraintCoef(2) = 1.0
            ConstraintCoef(4) = -1.0
            ConstraintRightSide = z2-z1
         Else
c-----------Gate is in operation, calculate gate flow and
c-----------coefficients
            OK=GateFlow(Extremity,z1,z2,GateNum)
            QGateCalc=QGate(GateNum)
            ConstraintCoef(1) =   1.0
            ConstraintCoef(2) = dqdz1gate(GateNum)
            ConstraintCoef(4) = dQdZ2Gate(GateNum)
            ConstraintRightSide = QGateCalc-Q1
         End If

C--------Insert the coefficients
         OK = SetUserStreamConstraintCoef()
         OK = CloseChannel()
 100     continue
      ENDDO

      CalculateGateFlow = .TRUE.

      RETURN
      END

*== Public (GateFlow) ===================================

      LOGICAL FUNCTION GateFlow(Extremity,z1,z2,GateNum)

      IMPLICIT NONE

*    Purpose: This function calculates flow through circular (pipe) and weir
*             gates based on upstream (y1) and dowstream (y2) water
*             surface elevation.

*   Arguments:
      REAL*8    z1,z2
      INTEGER GateNum,Extremity

*   Argument definitions:
*          y1  -  Water surface elevation at the upstream end of the gate
*          y2  -  Water surface elevation at the downstream end of the gate
*          z1  -  Water surface elevation near the gate in the channel
*          z2  -  Water surface elevation near the gate at the junction
*       QGate  -  Flow through the gate
*   dQdZ1Gate  -  Partial derivative of Q with respect to z1
*   dQdZ2Gate  -  Partial derivative of Q with respect to z2

*     FlowArea -  FlowArea through the gate
*  GateTopWidth-  Top width of the flow in the gate
*     GateNum  -  Gate Number

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chconnec.inc'
      include '../input/fixed/misc.f'

***** Network control:
      REAL*8     AccelerationGravity
      EXTERNAL AccelerationGravity

*   Local Variables:
      REAL*8     y1, y2,GateCoef_pipe,GateCoef_weir,value
      REAL*8     d, dely, ylarge, ysmall, r, ypipe, angle
      REAL*8     FlowAreaWeir, FlowAreaPipe
      REAL*8     GateTopWidthWeir,GateTopWidthPipe
      REAL*8     QGateWeir,QGatePipe
      LOGICAL  Seaward
      REAL*8     CxA,CxT

*   Routines by module:

***** Channel schematic:

*   Programmed by: Mohammad Rayej
*   Date:          January 1993
*   Modified by:   Parviz Nader
*   Date:          Dec   1993
*   Modified by:
*   Last modified:

*-----Implementation -----------------------------------------------------

      GateFlow=.FALSE.

      IF (Extremity.EQ.1) THEN
*--------Gate at the upstream end of the channel
         y1=z2
         y2=z1
      ELSEIF (Extremity.EQ.-1) THEN
*--------Gate at the downward end of the channel
         y1=z1
         y2=z2
      ENDIF

*-----Determine if flow is seaward or landward.

      CurrentDeltaH(GateNum)=(y1-y2)

      if (y1.GT.y2) THEN        ! seaward flow

         Seaward=.TRUE.
         ylarge=y1
         ysmall=y2
         GateCoef_pipe=GateSeaCoef_pipe(GateNum)
         GateCoef_weir=GateSeaCoef_weir(GateNum)
      ELSE                      ! landward flow
         Seaward=.FALSE.
         ylarge=y2
         ysmall=y1
         GateCoef_pipe=GateLandCoef_pipe(GateNum)
         GateCoef_weir=GateLandCoef_weir(GateNum)
      ENDIF

***---Adjust due to gate scheduling

      GateCoef_pipe=GateCoef_pipe*GateOperatingCoeff(GateNum)
      GateCoef_weir=GateCoef_weir*GateOperatingCoeff(GateNum)

c-----adjust for number of gates open out of total
      GateCoef_weir=GateCoef_weir*NumberofGatesFraction(GateNum)

      FlowAreaWeir=0.
      FlowAreaPipe=0.
      GateTopWidthWeir=0.
      GateTopWidthPipe=0.
      QGateWeir=0.
      QGatePipe=0.

      IF ((WidthWeirSea(GateNum)+WidthWeirLand(GateNum))
     &     .gt. 0.) THEN
*     Weir gate

         d=DepthWeirCrest(GateNum)

         IF (Seaward) then
            GateTopWidthWeir=WidthWeirSea(GateNum)
         ELSE
            GateTopWidthWeir=WidthWeirLand(GateNum)
         ENDIF

         if (ysmall.GE.d) then
*     Submerged flow
            dely=ylarge-ysmall
            FlowAreaWeir=GateTopWidthWeir*(ylarge-d)
         ELSEif (ysmall.LT.d .AND. ylarge.GE.d) then
*     Free flow
            dely=ylarge-d
            FlowAreaWeir=GateTopWidthWeir*(ylarge-d)
         ELSE
*     dry weir
            dely=0.
            FlowAreaWeir=0.
         ENDIF

         QGateWeir=GateCoef_weir*FlowAreaWeir*SQRT(dely)

      ENDIF

      IF (NumberofPipes(GateNum).GT.0) THEN

*--------circular pipe (orifice) gate

         d=DepthInvertPipe(GateNum)
         r=PipeRadius(GateNum)

         if (ysmall.GE.d) then
*     Submerged flow
            dely=ylarge-ysmall
         ELSEif (ysmall.LT.d .AND. ylarge.GE.d) then
*     Free flow
            dely=ylarge-d
         ELSE
*     dry pipe
            dely=0.
         ENDIF

         ypipe=ylarge-d

         if (ypipe.GT.0.AND.ypipe.LT.2.*r) THEN
*    partial flow
            angle=ACOS(1.-ypipe/r)
            FlowAreaPipe=r**2*angle-r*(r-ypipe)*SIN(angle)
            GateTopWidthPipe=2.*r*SIN(angle)
         ELSEif (ypipe.GT.2.*r) THEN
*    full flow
            FlowAreaPipe=r**2*3.1415926
            GateTopWidthPipe=0.
         ELSE
            FlowAreaPipe=0.
            GateTopWidthPipe=0.
         ENDIF
         FlowAreaPipe=FlowAreaPipe*dfloat(NumberofPipes(GateNum))
         GateTopWidthPipe=GateTopWidthPipe*dfloat(NumberofPipes(GateNum))
         QGatePipe=GateCoef_pipe*FlowAreaPipe*SQRT(dely)
      ENDIF

c-----FlowArea=FlowAreaWeir+FlowAreaPipe
c-----GateTopWidth=GateTopWidthWeir+GateTopWidthPipe

c-----Introduce two varibales:
c-----CxA = Cweir*Aweir+Cpipe*Apipe
c-----CxT=  Cweir*TopWweir +Cpipe*TopWpipe

      CxA=GateCoef_weir*FlowAreaWeir+GateCoef_pipe*FlowAreaPipe
      CxT=GateCoef_weir*GateTopWidthWeir+GateCoef_pipe*GateTopWidthPipe
      QGate(GateNum)=QGateWeir+QGatePipe

      IF (Extremity.EQ.1) THEN
*----    Gate placed at the upstream end of the channel

         if (z2.GT.z1) THEN
            value=(z2-z1)**0.5
c-----------dQdZ1Gate(GateNum) = 0.5*GateCoef*FlowArea/value
c-----------dQdZ2Gate(GateNum) = GateCoef*(-0.5*FlowArea/value+GateTopWidth*value)
            dQdZ1Gate(GateNum) = 0.5*CxA/value
            dQdZ2Gate(GateNum) = -0.5*CxA/value+CxT*value
         ELSEIF (Z1.GT.Z2) THEN
            value=(z1-z2)**0.5
c-----------dQdZ1Gate(GateNum) = GateCoef*(0.5*FlowArea/value+GateTopWidth*value)
c-----------dQdZ2Gate(GateNum) = -0.5*GateCoef*FlowArea/value
            dQdZ1Gate(GateNum) = 0.5*CxA/value+CxT*value
            dQdZ2Gate(GateNum) = -0.5*CxA/value
         ELSE
            dQdZ1Gate(GateNum) = 1.e5
            dQdZ2Gate(GateNum) = -1.e5
         ENDIF
      ELSEIF (Extremity.EQ.-1) THEN
*----    Gate placed at the downstream end of the channel

         if (z1.GT.z2) THEN
            value=(z1-z2)**0.5
c-----------dQdZ1Gate(GateNum) = -GateCoef*(0.5*FlowArea/value+GateTopWidth*value)
c-----------dQdZ2Gate(GateNum) = 0.5*GateCoef*FlowArea/value
            dQdZ1Gate(GateNum) = -0.5*CxA/value-CxT*value
            dQdZ2Gate(GateNum) = 0.5*CxA/value

         ELSEif (z2.GT.z1) THEN
            value=(z2-z1)**0.5
c-----------dQdZ1Gate(GateNum) = -0.5*GateCoef*FlowArea/value
c-----------dQdZ2Gate(GateNum) = GateCoef*(0.5*FlowArea/value+GateTopWidth*value)
            dQdZ1Gate(GateNum) = -0.5*CxA/value
            dQdZ2Gate(GateNum) = 0.5*CxA/value+CxT*value
         ELSE
            dQdZ1Gate(GateNum) = -1.e5
            dQdZ2Gate(GateNum) = 1.e5
         ENDIF
      ENDIF

*-----Change Sign if landward flow

      IF (.NOT. Seaward) QGate(GateNum)=-QGate(GateNum)

      GateFlow=.TRUE.
      RETURN
      END

