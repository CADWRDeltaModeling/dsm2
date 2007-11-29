C!    Copyright (C) 1996, 1997, 1998 State of California,
C!    Department of Water Resources.
C!
C!    Delta Simulation Model 2 (DSM2): A River, Estuary, and Land
C!    numerical model.  No protection claimed in original FOURPT and
C!    Branched Lagrangian Transport Model (BLTM) code written by the
C!    United States Geological Survey.  Protection claimed in the
C!    routines and files listed in the accompanying file "Protect.txt".
C!    If you did not receive a copy of this file contact Tara Smith,
C!    below.
C!
C!    This program is licensed to you under the terms of the GNU General
C!    Public License, version 2, as published by the Free Software
C!    Foundation.
C!
C!    You should have received a copy of the GNU General Public License
C!    along with this program; if not, contact Tara Smith, below,
C!    or the Free Software Foundation, 675 Mass Ave, Cambridge, MA
C!    02139, USA.
C!
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
C!    Tara Smith
C!    California Dept. of Water Resources
C!    Division of Planning, Delta Modeling Section
C!    1416 Ninth Street
C!    Sacramento, CA  95814
C!    916-653-9885
C!    tara@water.ca.gov
C!
C!    or see our home page: http://baydeltaoffice.water.ca.gov/modeling/deltamodeling/

C!      For information about the solver routines, contact:
C!      Eli Ateljevich
C!      (510) 843-1569
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
*

************************************************************************
*
*
*
************************************************************************

*== Private (ReserveMatrix) =============================================

      LOGICAL FUNCTION ReserveMatrix()

      IMPLICIT NONE

*   Purpose:  To reserve elements in the sparse matrix and create pointers
*             to the elements which will be used later when filling the matrix.
*

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      include 'chconnec.inc'
      INCLUDE 'solver.inc'
      Include 'solver2.inc'
      INCLUDE '../input/fixed/common.f'

*   Local Variables:

      INTEGER I,II,III, J,JJ, K, L, M, N
      INTEGER ResRow,ConnectCol
      INTEGER CodeUp,CodeDown,ChannelNumber,ChannelRow
      INTEGER MaxConnections
      PARAMETER (MaxConnections = MaxConnectingChannels)
      INTEGER ConnectingNodes,Node,ConstraintNode
      INTEGER ConnectingChannel
      INTEGER Res1,Res2,ResChannel
      INTEGER FlowOffset,StageOffset
      INTEGER DF,PlusOne,MinusOne,First
      PARAMETER (DF = 2,PlusOne = 1,MinusOne= -1,First=1)
      LOGICAL OK
      INTEGER UpConstraintRow
      INTEGER DownConstraintRow

*   Routines by module:

***** Network control:
      INTEGER  NetworkPrintLevel
      EXTERNAL NetworkprintLevel

***** Channel schematic:
      INTEGER  UpstreamCode, DownstreamCode, NumberOfStreamLocations
      EXTERNAL UpstreamCode, DownstreamCode, NumberOfStreamLocations

      INTEGER  NumberOfChannels, UpstreamPointer, DownstreamPointer
      EXTERNAL NumberOfChannels, UpstreamPointer, DownstreamPointer

      INTEGER  UpstreamConnect, DownstreamConnect
      EXTERNAL UpstreamConnect, DownstreamConnect

      INTEGER  UpstreamConnections, DownstreamConnections
      EXTERNAL UpstreamConnections, DownstreamConnections

      INTEGER  StreamEndNode,TotalStreamLocations,CurrentChannel
      EXTERNAL StreamEndNode,TotalStreamLocations,CurrentChannel

      LOGICAL  OpenChannel, CloseChannel
      EXTERNAL OpenChannel, CloseChannel

      INTEGER sfGetElement
      EXTERNAL sfGetElement

***** Local:

*   Intrinsics:
      INTEGER    MOD
      INTRINSIC  MOD

*-----Boundary-condition code          Use...
*
*       water surface
*          1                        explicitly known
*          4                        self-setting (downstream only)
*         11                        equal to another water surface
* ........31                        3-parameter relation
* ........41                        (reserved for four-parameter rating)
* ........51                        user-defined
*
*       flow
*          2                        explicitly known
*         12                        sum of flows equal zero
* ........32                        3-parameter relation
* ........42                        (reserved for four-parameter rating)
* ........52                        user-defined

*   Programmed by: Eli Ateljevich
*   Version 98.01, July, 1998

*-----Implementation -----------------------------------------------------

      ReserveMatrix = .FALSE.

*-----I = current row number.
      I = 0

*-----J = current equation number.
      J = 0

*-----K = current constraint pointer number
      K=0

      DO 300 M=1,NumberOfChannels()
         ChannelNumber = M
         UpConstraintRow=0
         DownConstraintRow=0

         IF( OpenChannel( ChannelNumber ) ) THEN

*-----------Check upstream flow constraint

            CodeUp = MOD( UpstreamCode(), 100 )

            IF( CodeUp .EQ. 1
     &           .OR.
     &           CodeUp .EQ. 11
     &           .OR.
     &           CodeUp .EQ. 31
     &           .OR.
     &           CodeUp .EQ. 51  ) THEN

*--------------Upstream stage-type boundary

*--------------First mass equation
               I=I+1
               J=J+1
               MASSEQ(1,J) = sfGetElement(matrix,I,I)
               MASSEQ(2,J) = sfGetElement(matrix,I,I+1)
               MASSEQ(3,J) = sfGetElement(matrix,I,I+2)
               MASSEQ(4,J) = sfGetElement(matrix,I,I+3)
               MASSEQROW(J)=I
               RowScale(I) = MassScaleFactor

               I=I+1
               UpConstraintRow = I
               UpConstraintEq(M) = I

            ELSE IF( CodeUp .EQ. 32   .OR. CodeUp.EQ.12
     &              .OR.  CodeUp.EQ.2 .OR.
     &              CodeUp .EQ. 52 ) THEN

*--------------Upstream flow-type boundary
               I=I+1
               UpConstraintRow = I
               UpConstraintEq(M) = I

*--------------First Mass Equation
               I=I+1
               J=J+1
               MASSEQROW(J)=I

               MASSEQ(1,J) = sfGetElement(matrix,I,I-1)
               MASSEQ(2,J) = sfGetElement(matrix,I,I)
               MASSEQ(3,J) = sfGetElement(matrix,I,I+1)
               MASSEQ(4,J) = sfGetElement(matrix,I,I+2)
               RowScale(I) = MassScaleFactor

            END IF
            EqPointer( ChannelNumber ) = J

*-----------If more than one subdomain in
*               channel

            IF( NumberOfStreamLocations() .GT. 2 ) THEN
               DO 100 N=UpstreamPointer()+2,DownstreamPointer()

*-----------------dynamic equation
                  I=I+1
                  DYNMEQ(1,J) = sfGetElement(matrix,I,I-2)
                  DYNMEQ(2,J) = sfGetElement(matrix,I,I-1)
                  DYNMEQ(3,J) = sfGetElement(matrix,I,I)
                  DYNMEQ(4,J) = sfGetElement(matrix,I,I+1)
                  DYNMEQROW(J)= I
                  RowScale(i) = DynScaleFactor

*-----------------general subdomain.
                  J = J + 1

*-----------------mass equation.
                  I = I+1
                  MASSEQ(1,J) = sfGetElement(matrix,I,I-1)
                  MASSEQ(2,J) = sfGetElement(matrix,I,I)
                  MASSEQ(3,J) = sfGetElement(matrix,I,I+1)
                  MASSEQ(4,J) = sfGetElement(matrix,I,I+2)
                  MASSEQROW(J)=I
                  RowScale(I) = MassScaleFactor

 100           CONTINUE
            END IF

*-----------Check downstream flow constraint
            CodeDown = MOD( DownstreamCode(), 100 )

            IF( CodeDown .EQ. 2
     &           .OR.
     &           CodeDown .EQ. 12
     &           .OR.
     &           CodeDown .EQ. 32
     &           .OR.
     &           CodeDown .EQ. 52  ) THEN

*--------------Downstream flow-type boundary
               I=I+1
               DownConstraintRow = I
               DownConstraintEq(M)=I

*--------------Last Dynamic Eq
               I=I+1
               DYNMEQ(1,J) = sfGetElement(matrix,I,I-3)
               DYNMEQ(2,J) = sfGetElement(matrix,I,I-2)
               DYNMEQ(3,J) = sfGetElement(matrix,I,I-1)
               DYNMEQ(4,J) = sfGetElement(matrix,I,I)
               DYNMEQROW(J)=I
               RowScale(i) = DynScaleFactor

            ELSE
* Eli: Check
*--------------Downstream stage-type boundary
               I=I+1
               DYNMEQ(1,J) = sfGetElement(matrix,I,I-2)
               DYNMEQ(2,J) = sfGetElement(matrix,I,I-1)
               DYNMEQ(3,J) = sfGetElement(matrix,I,I)
               DYNMEQ(4,J) = sfGetElement(matrix,I,I+1)
               DYNMEQROW(J)=I
               RowScale(i) = DynScaleFactor

               I = I+1
               DownConstraintRow = I
               DownConstraintEq(M)=I

            END IF


c     These lines are very common as diagnostics

c    

c	If (UpConstraintRow.LE. 1589 .and. DownConstraintRow.GE.1589)Then

c	print*,Int2Ext(M),M,UpConstraintRow,DownConstraintRow,CodeUp,CodeDown

c	pause

c	Endif




*-----------Reserve matrix elements for constraints
*-----------based on constraint type
*-----------and connections

*-----------Upstream end of channel.

            If (UpConstraintRow.NE.0) Then
               if (codeup .eq. 1) then
*-----------------Known water surface
*-----------------Single constraint at (i,i)
                  K=K+1
                  UpConstraintIndex(M)=K
                  ConstraintPointers(K)=sfGetElement(matrix,UpConstraintRow,UpConstraintRow)
                  RowScale(UpConstraintRow) = ZUnscaleFactor

               elseif (codeup .eq. 2) then
*-----------------Known flow
*-----------------Flow constraint. at (i,i). Possible reservoir stage
*-----------------components added later

                  K=K+1
                  UpConstraintIndex(M)=K
                  ConstraintPointers(K)=sfGetElement(matrix,UpConstraintRow,UpConstraintRow)

               elseif (codeup .eq. 4) then
*-----------------Self Starting(downstream only)
*-----------------Eli: Don't understand this yet

               elseif (codeup .eq. 11) then
*-----------------Equal Water Surfaces
*-----------------Element at (i,i), plus stage at a single connecting node, which
*-----------------must be identified
                  K=K+1
                  UpConstraintIndex(M)=K
                  ConstraintPointers(K)=sfGetElement(matrix,UpConstraintRow,UpConstraintRow)
                  RowScale(UpConstraintRow) = ZUnscaleFactor

                  ConstraintNode = UpstreamPointer()
                  ConnectingNodes = UpstreamConnections()
                  IF( ConnectingNodes .LE. MaxConnections ) THEN
                  ELSE
                     WRITE(UNIT_ERROR,*) ' ####error(ReserveMatrix)'
                     WRITE(UNIT_ERROR,*) ' Upstream end Channel...', CurrentChannel()
                     WRITE(UNIT_ERROR,*) ' Number of connections (',ConnectingNodes,')'
                     WRITE(UNIT_ERROR,*) ' exceeds maximum (',MaxConnections,').'
                     WRITE(UNIT_ERROR,*) ' Abnormal program end.'
                     CALL EXIT(1)
                  END IF

                  If (ConnectingNodes .GT. 0) Then
                     ConnectingChannel = UpstreamConnect(First)
                     If (ConnectingChannel.NE.0) Then
                        Node = IABS(StreamEndNode(ConnectingChannel))
                     Else
*-----------------------No channel ... not actually attached
                     End If
                     K=K+1
                     ConstraintPointers(K)=sfGetElement(matrix,UpConstraintRow,DF*Node)
                  End If

               elseif (codeup .eq. 12) then
*-----------------Sum of Flow
*-----------------Flow at (i,i). Possible reservoir stage components
*-----------------added later

                  K=K+1
                  UpConstraintIndex(M)=K
                  ConstraintPointers(K)=sfGetElement(matrix,UpConstraintRow,UpConstraintRow)

                  ConnectingNodes = UpstreamConnections()
                  ConstraintNode = UpstreamPointer()

                  If (ConnectingNodes.gt.0) Then
                     Do 130 L=1,ConnectingNodes
                        Node = StreamEndNode(UpstreamConnect(L))
                        ConnectCol = 2 * IABS(Node) - 1
                        K=K+1
                        ConstraintPointers(K)=sfGetElement(matrix,UpConstraintRow,ConnectCol)
 130                 Continue
                  End If

               elseif (codeup .eq. 31 .or.
     &                 codeup .eq. 32) then
*-----------------Three Param surface or flow
*-----------------Includes local flow, local surface
*-----------------one connecting surface.

                  ConstraintNode = UpstreamPointer()
                  ConnectingNodes = UpstreamConnections()
                  ConnectingChannel =UpstreamConnect(First)
                  ConnectCol = DF * IABS(StreamEndNode(ConnectingChannel))
                  K=K+1
                  UpConstraintIndex(M)=K

*-----------------Flow at constraint node
                  FlowOffset = 0
                  If (CodeUp .EQ. 31) FlowOffset = MinusOne
                  ConstraintPointers(K)= sfGetElement(matrix,UpConstraintRow,UpConstraintRow+FlowOffset)

*-----------------Stage at constraint node
                  StageOffset=0
                  If (CodeUp .EQ. 32) StageOffset = PlusOne
                  K=K+1
                  ConstraintPointers(K) = sfGetElement(Matrix,UpConstraintRow,UpConstraintRow+StageOffset)

*-----------------Stage at connecting node
                  K=K+1
                  ConstraintPointers(K) = sfGetElement(Matrix,UpConstraintRow,ConnectCol)

               elseif (codeup .eq. 51 .or.
     &                 codeup .eq. 52) then
*-----------------User Defined flow or surface
*-----------------For generality, flow and surface elements are reserved for
*-----------------the constraint row and for each connecting channel. It is very
*-----------------likely that only a few of these will be used (e.g., orifice
*-----------------equation uses only three)
                  ConstraintNode = UpstreamPointer()
                  ConnectingNodes =UpstreamConnections()
                  FlowOffset = 0
                  If (CodeUp .EQ. 51) FlowOffset = MinusOne
                  StageOffset = 0
                  If (CodeUp .EQ. 52) StageOffset = PlusOne
*-----------------Flow at Constraint
                  K=K+1
                  UpConstraintIndex(M)=K

                  ConstraintPointers(K)= sfGetElement(matrix,UpConstraintRow,UpConstraintRow+FlowOffset)
*-----------------Stage at Constraint
                  K=K+1
                  ConstraintPointers(K)= sfGetElement(matrix,UpConstraintRow,UpConstraintRow+StageOffset)

*-----------------Now Flow and Stage at each connecting node
                  ConstraintNode = UpstreamPointer()
                  If(ConnectingNodes.gt.0) then
                     Do 160 L=1,ConnectingNodes
                        Node = StreamEndNode(UpstreamConnect(L))
                        ConnectCol = IABS(Node)*DF

                        K=K+1
                        ConstraintPointers(K)=sfGetElement(matrix,UpConstraintRow,ConnectCol-1)
                        K=K+1
                        ConstraintPointers(K)=sfGetElement(matrix,UpConstraintRow,ConnectCol)
 160                 Continue
                  End If

               else
                  WRITE(UNIT_ERROR,*) ' *** Error (ReserveMatrix)'
                  WRITE(UNIT_ERROR,*) 'Channel Number ', M
                  WRITE(UNIT_ERROR,*) ' Constraint Type (Condition Number) Not Supported.'
                  CALL EXIT(1)
               Endif

            ELSE
               WRITE(UNIT_ERROR,*) ' *** Error (ReserveMatrix)'
               WRITE(UNIT_ERROR,*) 'Channel Number ', M
               WRITE(UNIT_ERROR,*) ' Bad Constraint Row'
               CALL EXIT(1)
            END IF

*-----------Downstream end of channel.
            If (DownConstraintRow.NE.0) Then

               if (codedown .eq. 1) then
*-----------------Known water surface
*-----------------Single constraint at (i,i)
                  K=K+1
                  DownConstraintIndex(M)=K
                  ConstraintPointers(K)=sfGetElement(matrix,DownConstraintRow,DownConstraintRow)
                  RowScale(DownConstraintRow) = ZUnscaleFactor

               elseif (codedown .eq. 2) then
*-----------------Known flow
*-----------------Flow constraint. at (i,i). Possible reservoir stage
*-----------------components added later

                  K=K+1
                  DownConstraintIndex(M)=K
                  ConstraintPointers(K)=sfGetElement(matrix,DownConstraintRow,DownConstraintRow)

               elseif (codedown .eq. 4) then
*-----------------Self Starting(downstream only)
*-----------------Eli: Don't understand this yet

               elseif (codedown .eq. 11) then
*-----------------Equal Water Surfaces
*-----------------Element at (i,i), plus stage at a single connecting node, which
*-----------------must be identified
                  K=K+1
                  DownConstraintIndex(M)=K
                  ConstraintPointers(K)=sfGetElement(matrix,DownConstraintRow,DownConstraintRow)
                  RowScale(DownConstraintRow) = ZUnscaleFactor

                  ConstraintNode = DownstreamPointer()
                  ConnectingNodes = DownstreamConnections()
                  IF( ConnectingNodes .LE. MaxConnections ) THEN
                  ELSE
                     WRITE(UNIT_ERROR,*) ' ####error(ReserveMatrix)'
                     WRITE(UNIT_ERROR,*) ' Downstream end Channel...', CurrentChannel()
                     WRITE(UNIT_ERROR,*) ' Number of connections (',ConnectingNodes,')'
                     WRITE(UNIT_ERROR,*) ' exceeds maximum (',MaxConnections,').'
                     WRITE(UNIT_ERROR,*) ' Abnormal program end.'
                     CALL EXIT(1)
                  END IF

                  If (ConnectingNodes .GT. 0) Then
                     ConnectingChannel = DownstreamConnect(First)
                     If(ConnectingChannel.NE.0) Then
                        Node = IABS(StreamEndNode(ConnectingChannel))
                     Else
*-----------------------No channel ... not actually attached
                     End If

                     K=K+1
                     ConstraintPointers(K)=sfGetElement(matrix,DownConstraintRow,DF*Node)
                  End If

               elseif (codedown .eq. 12) then
*-----------------Sum of Flow.
*-----------------Flow at (i,i). Possible stage at (i,i+1) and connecting reservoirs.
*-----------------Also Flow at each connecting node
                  K=K+1
                  DownConstraintIndex(M)=K
                  ConstraintPointers(K)=sfGetElement(matrix,DownConstraintRow,DownConstraintRow)

                  ConnectingNodes = DownstreamConnections()
                  IF(ConnectingNodes .LE. MaxConnections) THEN
                  ELSE
                     WRITE(UNIT_ERROR,*) ' ####error(ReserveMatrix)'
                     WRITE(UNIT_ERROR,*) ' Downstream end Channel...', CurrentChannel()
                     WRITE(UNIT_ERROR,*) ' Number of connections (',ConnectingNodes,')'
                     WRITE(UNIT_ERROR,*) ' exceeds maximum (',MaxConnections,').'
                     WRITE(UNIT_ERROR,*) ' Abnormal program end.'
                     CALL EXIT(1)
                  END IF

                  ConstraintNode = DownstreamPointer()

                  If (ConnectingNodes.gt.0) Then
                     Do 220 L=1,ConnectingNodes
                        Node = StreamEndNode(DownstreamConnect(L))
                        ConnectCol = DF*IABS(Node) - 1
                        K=K+1
                        ConstraintPointers(K)=sfGetElement(matrix,DownConstraintRow,ConnectCol)
 220                 Continue
                  End If

               elseif (codedown .eq. 31 .or.
     &                 codedown .eq. 32) then
*-----------------Three Param surface or flow
*-----------------Three elements are reserved, including two surface
*-----------------elements and one flow element.

                  ConstraintNode = DownstreamPointer()
                  ConnectingNodes =DownstreamConnections()
                  ConnectingChannel =DownstreamConnect(First)
                  ConnectCol = DF * IABS(StreamEndNode(ConnectingChannel))
                  K=K+1
                  DownConstraintIndex(M)=K

*-----------------Flow at constraint node
                  FlowOffset = 0
                  If (CodeDown .EQ. 31)Then
                     FlowOffset = MinusOne
                  End If
                  ConstraintPointers(K)= sfGetElement(matrix,DownConstraintRow,DownConstraintRow+FlowOffset)

*-----------------Stage at constraint
                  StageOffset=0
                  If (CodeDown .EQ. 32) Then
                     StageOffset = PlusOne
                  End If
                  K=K+1
                  ConstraintPointers(K) = sfGetElement(Matrix,DownConstraintRow,DownConstraintRow+StageOffset)

*-----------------Stage at connecting node
                  K=K+1
                  ConstraintPointers(K) = sfGetElement(Matrix,DownConstraintRow,ConnectCol)

               elseif (codedown .eq. 51 .or.
     &                 codedown .eq. 52) then
*-----------------User Defined flow or surface
*-----------------For generality, flow and surface elements are reserved for
*-----------------the constraint row and for each connecting channel. It is very
*-----------------likely that only a few of these will be used (e.g., orifice
*-----------------equation uses only three)
                  ConstraintNode = DownstreamPointer()
                  ConnectingNodes =DownstreamConnections()
                  FlowOffset = 0
                  If (CodeDown .EQ. 51) FlowOffset = MinusOne
                  StageOffset = 0
                  If (CodeDown .EQ. 52) StageOffset = PlusOne
*-----------------Flow at Constraint
                  K=K+1
                  DownConstraintIndex(M)=K
                  ConstraintPointers(K)= sfGetElement(matrix,DownConstraintRow,DownConstraintRow+FlowOffset)

*-----------------Stage at Constraint
                  K=K+1
                  ConstraintPointers(K)= sfGetElement(matrix,DownConstraintRow,DownConstraintRow+StageOffset)

*-----------------Now Flow and Stage at each connecting node
                  ConstraintNode = DownstreamPointer()

                  Do 250 L=1,ConnectingNodes
                     Node = StreamEndNode(DownstreamConnect(L))
                     ConnectCol = IABS(Node)*DF
                     K=K+1
                     ConstraintPointers(K)=sfGetElement(matrix,DownConstraintRow,ConnectCol-1)
                     K=K+1
                     ConstraintPointers(K)=sfGetElement(matrix,DownConstraintRow,ConnectCol)
*--------------------Eli: Is this right?
 250              Continue

               else
                  WRITE(UNIT_ERROR,*) ' *** Error (ReserveMatrix)'
                  WRITE(UNIT_ERROR,*) 'Channel Number ', M
                  WRITE(UNIT_ERROR,*) ' Constraint Type (Condition Number) Not Supported.'
                  CALL EXIT(1)
               Endif

            ELSE
               WRITE(UNIT_ERROR,*) ' *** Error (ReserveMatrix)'
               WRITE(UNIT_ERROR,*) 'Channel Number ', M
               WRITE(UNIT_ERROR,*) ' Bad Constraint Row'
               CALL EXIT(1)
            END IF

            OK = CloseChannel()

         ELSE
            WRITE(UNIT_ERROR,*) ' ***Error (ReserveMatrix)'
            WRITE(UNIT_ERROR,*) ' Could not open channel...',int2ext(ChannelNumber)
            CALL EXIT(1)
         END IF

 300  CONTINUE

*-----Matrix Elements are now allocated for elements involving
*-----only channels. The remainder of the
*-----matrix consists of elements involving reservoirs calculations

      If(Nres.GT.0) Then
*--------K: current constraint pointer index
         K=0

         Do II = 1,Nres
            I=I+1
            ResRow = II + TotalNonResRows

*-----------First element is on diagonal and represents the reservoir
*-----------height
            K=K+1
            ResEqIndex(II) = K
            ResEqPointer(K) = sfGetElement(matrix,ResRow,ResRow)

*-----------Next reserve stage elements for connecting channels
*-----------Entries will occur in the row corresponding
*-----------to the reservoir and the row corresponding to the channel
*-----------ChannelRow is a Z row, subtract 1 to get a Q row.

            Do JJ =1, NconnectReservoir(II)
               ResChannel = ResConnectingChannels(ii,jj)
               ChannelRow = StreamEndNode( ResChannel ) * DF
               K=K+1
               ResEqPointer(K) = sfGetElement(matrix,ResRow,ResRow)
               K=K+1
               ResEqPointer(K) = sfGetElement(matrix,ResRow,ChannelRow)
               K=K+1
               ResEqPointer(K) = sfGetElement(matrix,ChannelRow-1,ResRow)
               K=K+1
               ResEqPointer(K) = sfGetElement(matrix,ChannelRow-1,ChannelRow)
            enddo
         enddo
      End If

*-----Next go through obj2obj transfers and find reservoir
*-----to reservoir transfers which
*-----depend on head. Reserve elements in the
*-----reservoir rows.

      If (nobj2obj.gt.0)Then
         K=0
         Do III=1,nobj2obj
            If ((obj2obj(iii).from.object .eq. obj_reservoir)
     &           .and. (obj2obj(iii).to.object .eq. obj_reservoir)
     &           .and. (obj2obj(iii).constant_value .eq. HEAD_DIFF) ) then
*--------------transfer is res2res and head dependent
               Res1 = obj2obj(iii).from.object_no + TotalNonResRows
               Res2 = obj2obj(iii).to.object_no + TotalNonResRows

               K=K+1
               obj2objEqIndex(iii)=K
               obj2objEqPointer(K)=sfGetElement(matrix, Res1,Res1)
               K=K+1
               obj2objEqPointer(K)=sfGetElement(matrix, Res2,Res2)
               K=K+1
               obj2objEqPointer(K)=sfGetElement(matrix, Res1,Res2)
               K=K+1
               obj2objEqPointer(K)=sfGetElement(matrix, Res2,Res1)
            End If
         enddo
      End If

*-----End of allocation. Check for error and exit.
      IF( I.EQ. Equations ) THEN

         ReserveMatrix = .TRUE.

      ELSE

         WRITE(UNIT_ERROR,*) ' *** Error (ReserveMatrix)'
         WRITE(UNIT_ERROR,*) I,' rows, ',Equations,' equations...'
         WRITE(UNIT_ERROR,*) ' Abnormal program end.'
         CALL EXIT(1)

      END IF

      RETURN
      END

*==== EOF solvefpt.f ============================================================

