C!    DSM2 - SPARSE LIBRARY INTERFACE COPYRIGHT
C!    Copyright (C) 1998, 1999 Eli Ateljevich

C!    Note that the routines contained below
C!    were created by Eli Ateljevich and comprise part of
C!    an interface to the SPARSE matrix library, created
C!    by Kenneth S. Kundert and the University of California
C!    for which copyright information is given below

C!    This program is licensed to you under the terms of the GNU General
C!    Public License, version 2, as published by the Free Software
C!    Foundation.
C!
C!    You should have received a copy of the GNU General Public License
C!    along with this program; if not, contact Dr. Paul Hutton, below,
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

*==   Private (ReserveMatrix) =============================================

      logical function ReserveMatrix()
      use Gates, only:gateArray,Gate,NGate
      use IO_Units
      implicit none

*     Purpose:  To reserve elements in the sparse matrix and create pointers
*     to the elements which will be used later when filling the matrix.
*     

*     Arguments:

*     Argument definitions:


*     Module data:
      include 'network.inc' 
	include 'chnlcomp.inc'
      include 'chconnec.inc'
      include 'solver.inc'
      include '../input/fixed/common.f' ! only nreser

*     Local Variables:

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

      Integer indx

      integer objType,objID
      Type(Gate),pointer :: currentGate

*     Routines

***** Network control:
      integer, external ::  NetworkPrintLevel

***** Channel schematic:
      integer,external :: UpstreamCode, DownstreamCode, 
     &                    NumberOfStreamLocations,NumberOfChannels,
     &                    UpstreamPointer,DownstreamPointer

      integer,external :: UpstreamConnect, DownstreamConnect, 
     &                    UpstreamConnections, DownstreamConnections

      integer,external :: StreamEndNode,TotalStreamLocations,CurrentChannel

      logical,external :: OpenChannel, CloseChannel

****** Solver
      integer,external :: sfGetElement

*-----Boundary-condition code          Use...
*     
*     water surface
*     1                        explicitly known
*     4                        self-setting (downstream only)
*     11                        equal to another water surface
*     ........31                        3-parameter relation
*     ........41                        (reserved for four-parameter rating)
*     ........51                        user-defined
*     
*     flow
*     2                        explicitly known
*     12                        sum of flows equal zero
*     ........32                        3-parameter relation
*     ........42                        (reserved for four-parameter rating)
*     ........52                        user-defined

*     Programmed by: Eli Ateljevich
*     Version 98.01, July, 1998

*-----Implementation -----------------------------------------------------

      ReserveMatrix = .FALSE.

*-----I = current row number.
      I=0

*-----J = current equation number.
      J=0

*-----K = current constraint pointer number
      K=0

      do 300 M=1,NumberOfChannels()
         ChannelNumber = M
         UpConstraintRow=0
         DownConstraintRow=0

         if( OpenChannel( ChannelNumber ) ) then

*-----------Check upstream flow constraint

            CodeUp = MOD( UpstreamCode(), 100 )

            if( CodeUp .EQ. 1
     &           .OR.
     &           CodeUp .EQ. 11
     &           .OR.
     &           CodeUp .EQ. 31
     &           .OR.
     &           CodeUp .EQ. 51  ) then

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
               RowScale(i) = 1.D0

            else if( CodeUp .EQ. 32   .OR. CodeUp.EQ.12
     &              .OR.  CodeUp.EQ.2 .OR.
     &              CodeUp .EQ. 52 ) then

*--------------Upstream flow-type boundary
               I=I+1
               UpConstraintRow = I
               UpConstraintEq(M) = I
               RowScale(i) = 1



*--------------First Mass Equation
               I=I+1
               J=J+1
               MASSEQROW(J)=I

               MASSEQ(1,J) = sfGetElement(matrix,I,I-1)
               MASSEQ(2,J) = sfGetElement(matrix,I,I)
               MASSEQ(3,J) = sfGetElement(matrix,I,I+1)
               MASSEQ(4,J) = sfGetElement(matrix,I,I+2)
               RowScale(I) = MassScaleFactor

            end if
            EqPointer( ChannelNumber ) = J

*-----------if more than one subdomain in
*     channel

            if( NumberOfStreamLocations() .GT. 2 ) then
               do 100 N=UpstreamPointer()+2,DownstreamPointer()

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

 100           continue
            end if

*-----------Check downstream flow constraint
            CodeDown = MOD( DownstreamCode(), 100 )

            if( CodeDown .EQ. 2
     &           .OR.
     &           CodeDown .EQ. 12
     &           .OR.
     &           CodeDown .EQ. 32
     &           .OR.
     &           CodeDown .EQ. 52  ) then

*--------------Downstream flow-type boundary
               I=I+1
               DownConstraintRow = I
               DownConstraintEq(M)=I
               RowScale(i) = 1


*--------------Last Dynamic Eq
               I=I+1
               DYNMEQ(1,J) = sfGetElement(matrix,I,I-3)
               DYNMEQ(2,J) = sfGetElement(matrix,I,I-2)
               DYNMEQ(3,J) = sfGetElement(matrix,I,I-1)
               DYNMEQ(4,J) = sfGetElement(matrix,I,I)
               DYNMEQROW(J)=I
               RowScale(i) = DynScaleFactor

            else
*     Eli: Check
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
               RowScale(i) = 1.D0
            end if


c     These lines are very common as diagnostics
            
c     if (UpConstraintRow .le. 1349 .and. DownConstraintRow .ge. 1347)then
c     write(unit_screen,110)chan_geom(m).chan_no,M,
c     &    UpConstraintRow,DownConstraintRow,CodeUp,CodeDown
c     110  format("Channel: ",i5," int. no: ",i5, //"Up constraint row: ",i5,
c     &    " Down: ",i5," Code up: ",i5," Code down",i5)
c     pause
c     Endif




*-----------Reserve matrix elements for constraints
*-----------based on constraint type
*-----------and connections

*-----------Upstream end of channel.

            if (UpConstraintRow.NE.0) then
               if (codeup .eq. 1) then
*-----------------Known water surface
*-----------------Single constraint at (i,i)
                  K=K+1
                  UpConstraintIndex(M)=K
                  ConstraintPointers(K)=sfGetElement(matrix,UpConstraintRow,UpConstraintRow)

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


                  ConstraintNode = UpstreamPointer()
                  ConnectingNodes = UpstreamConnections()
                  if( ConnectingNodes .LE. MaxConnections ) then
                  else
                     write(UNIT_ERROR,*) ' ####error(ReserveMatrix)'
                     write(UNIT_ERROR,*) ' Upstream end Channel...', CurrentChannel()
                     write(UNIT_ERROR,*) ' Number of connections (',ConnectingNodes,')'
                     write(UNIT_ERROR,*) ' exceeds maximum (',MaxConnections,').'
                     write(UNIT_ERROR,*) ' Abnormal program end.'
                     call exit(1)
                  end if

                  if (ConnectingNodes .GT. 0) then
                     ConnectingChannel = UpstreamConnect(One)
                     if (ConnectingChannel.NE.0) then
                        Node = IABS(StreamEndNode(ConnectingChannel))
                     else
*-----------------------No channel ... not actually attached
                     end if
                     K=K+1
                     ConstraintPointers(K)=sfGetElement(matrix,UpConstraintRow,DF*Node)
                  end if

               elseif (codeup .eq. 12) then
*-----------------Sum of Flow
*-----------------Flow at (i,i). Possible reservoir stage components
*-----------------added later
                  K=K+1
                  UpConstraintIndex(M)=K
                  ConstraintPointers(K)=sfGetElement(matrix,UpConstraintRow,UpConstraintRow)
                  ConnectingNodes = UpstreamConnections()
                  ConstraintNode = UpstreamPointer()
                  if (ConnectingNodes.gt.0) then
                     Do 130 L=1,ConnectingNodes
                        Node = StreamEndNode(UpstreamConnect(L))
                        ConnectCol = 2 * IABS(Node) - 1
                        K=K+1
                        ConstraintPointers(K)=sfGetElement(matrix,UpConstraintRow,ConnectCol)
 130                 Continue
                  end if

               elseif (codeup .eq. 31 .or.
     &                 codeup .eq. 32) then
*-----------------Three Param surface or flow
*-----------------Includes local flow, local surface
*-----------------one connecting surface.

                  ConstraintNode = UpstreamPointer()
                  ConnectingNodes = UpstreamConnections()
                  ConnectingChannel =UpstreamConnect(One)
                  ConnectCol = DF * IABS(StreamEndNode(ConnectingChannel))
                  K=K+1
                  UpConstraintIndex(M)=K

*-----------------Flow at constraint node
                  FlowOffset = 0
                  if (CodeUp .EQ. 31) FlowOffset = MinusOne
                  ConstraintPointers(K)= sfGetElement(matrix,
     &                 UpConstraintRow,UpConstraintRow+FlowOffset)

*-----------------Stage at constraint node
                  StageOffset=0
                  if (CodeUp .EQ. 32) StageOffset = PlusOne
                  K=K+1
                  ConstraintPointers(K) = sfGetElement(Matrix,
     &                 UpConstraintRow,UpConstraintRow+StageOffset)

*-----------------Stage at connecting node
                  K=K+1
                  ConstraintPointers(K) = sfGetElement(Matrix,
     &                 UpConstraintRow,ConnectCol)

               elseif (codeup .eq. 51 .or.
     &                 codeup .eq. 52) then
*-----------------User Defined flow or surface
*-----------------For generality, flow and surface elements are reserved for
*-----------------the constraint row and for each connecting channel. It is very
*-----------------likely that only a few of these will be used (e.g., orifice
*-----------------equation uses only three)
                  RowScale(UpConstraintRow)=1.D0
                  ConstraintNode = UpstreamPointer()
                  ConnectingNodes =UpstreamConnections()
                  FlowOffset = 0
                  if (CodeUp .EQ. 51) FlowOffset = MinusOne
                  StageOffset = 0
                  if (CodeUp .EQ. 52) StageOffset = PlusOne
*-----------------Flow at Constraint
                  K=K+1
                  UpConstraintIndex(M)=K
                  
                  ConstraintPointers(K)= sfGetElement(matrix,
     &                 UpConstraintRow,DF*ConstraintNode-1)
*-----------------Stage at Constraint
                  K=K+1
                  ConstraintPointers(K)= sfGetElement(matrix,
     &                 UpConstraintRow,DF*ConstraintNode)
*-----------------Now Flow and Stage at each connecting node

                  if(ConnectingNodes.gt.0) then
                     Do 160 L=1,ConnectingNodes
                        Node = StreamEndNode(UpstreamConnect(L))
                                ! This is the "stage" column
                        ConnectCol = iabs(Node)*DF 
                        K=K+1
                        FlowOffset=-1
                        ConstraintPointers(K)=sfGetElement(matrix,
     &                       UpConstraintRow,ConnectCol+FlowOffset)
                        K=K+1
                        ConstraintPointers(K)=sfGetElement(matrix,
     &                       UpConstraintRow,ConnectCol)
 160                 Continue
                  end if

               else
                  write(UNIT_ERROR,*) ' *** Error (ReserveMatrix)'
                  write(UNIT_ERROR,*) 'Channel Number ', M
                  write(UNIT_ERROR,*) ' Constraint Type (Condition Number) Not Supported.'
                  call exit(1)
               Endif

            else
               write(UNIT_ERROR,*) ' *** Error (ReserveMatrix)'
               write(UNIT_ERROR,*) 'Channel Number ', M
               write(UNIT_ERROR,*) ' Bad Constraint Row'
               call exit(1)
            end if

*-----------Downstream end of channel.
            if (DownConstraintRow.NE.0) then

               if (codedown .eq. 1) then
*-----------------Known water surface
*-----------------Single constraint at (i,i)
                  K=K+1
                  DownConstraintIndex(M)=K
                  ConstraintPointers(K)=sfGetElement(matrix,DownConstraintRow,DownConstraintRow)

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
                  ConstraintPointers(K)=sfGetElement(matrix,
     &               DownConstraintRow,DownConstraintRow)

                  ConstraintNode = DownstreamPointer()
                  ConnectingNodes = DownstreamConnections()
                  if( ConnectingNodes .GT. MaxConnections ) then
                     write(UNIT_ERROR,*) ' ####error(ReserveMatrix)'
                     write(UNIT_ERROR,*) ' Downstream end Channel...', 
     &                  CurrentChannel()
                     write(UNIT_ERROR,*) ' Number of connections (',
     &                  ConnectingNodes,')'
                     write(UNIT_ERROR,*) ' exceeds maximum (',MaxConnections,').'
                     write(UNIT_ERROR,*) ' Abnormal program end.'
                     call exit(1)
                  end if

                  if (ConnectingNodes .GT. 0) then
                     ConnectingChannel = DownstreamConnect(One)
                     if(ConnectingChannel.NE.0) then
                        Node = IABS(StreamEndNode(ConnectingChannel))
                     else
*-----------------------No channel ... not actually attached
                     end if

                     K=K+1
                     ConstraintPointers(K)=sfGetElement(matrix,
     &                   DownConstraintRow,DF*Node)
                  end if

               elseif (codedown .eq. 12) then
*-----------------Sum of Flow.
*-----------------Flow at (i,i). Possible stage at (i,i+1) and connecting reservoirs.
*-----------------Also Flow at each connecting node
                  K=K+1
                  DownConstraintIndex(M)=K
                  ConstraintPointers(K)=sfGetElement(matrix,
     &                  DownConstraintRow,DownConstraintRow)

                  ConnectingNodes = DownstreamConnections()
                  if(ConnectingNodes .LE. MaxConnections) then
                  else
                     write(UNIT_ERROR,*) ' ####error(ReserveMatrix)'
                     write(UNIT_ERROR,*) ' Downstream end Channel...',
     &                 CurrentChannel()
                     write(UNIT_ERROR,*) ' Number of connections (',
     &                 ConnectingNodes,')'
                     write(UNIT_ERROR,*) ' exceeds maximum (',MaxConnections,').'
                     write(UNIT_ERROR,*) ' Abnormal program end.'
                     call exit(1)
                  end if

                  ConstraintNode = DownstreamPointer()

                  if (ConnectingNodes.gt.0) then
                     Do 220 L=1,ConnectingNodes
                        Node = StreamEndNode(DownstreamConnect(L))
                        ConnectCol = DF*IABS(Node) - 1
                        K=K+1
                        ConstraintPointers(K)=sfGetElement(matrix,
     &                      DownConstraintRow,ConnectCol)
 220                 Continue
                  end if

               elseif (codedown .eq. 31 .or.
     &                 codedown .eq. 32) then
*-----------------Three Param surface or flow
*-----------------Three elements are reserved, including two surface
*-----------------elements and one flow element.

                  ConstraintNode = DownstreamPointer()
                  ConnectingNodes =DownstreamConnections()
                  ConnectingChannel =DownstreamConnect(One)
                  ConnectCol = DF * abs(StreamEndNode(ConnectingChannel))
                  K=K+1
                  DownConstraintIndex(M)=K

*-----------------Flow at constraint node
                  FlowOffset = 0
                  if (CodeDown .EQ. 31)then
                     FlowOffset = MinusOne
                  end if
                  ConstraintPointers(K)= sfGetElement(matrix,
     &               DownConstraintRow,DownConstraintRow+FlowOffset)

*-----------------Stage at constraint
                  StageOffset=0
                  if (CodeDown .EQ. 32) then
                     StageOffset = PlusOne
                  end if
                  K=K+1
                  ConstraintPointers(K) = sfGetElement(Matrix,
     &                DownConstraintRow,DownConstraintRow+StageOffset)

*-----------------Stage at connecting node
                  K=K+1
                  ConstraintPointers(K) = sfGetElement(Matrix,
     &                DownConstraintRow,ConnectCol)

               elseif (codedown .eq. 51 .or.
     &                 codedown .eq. 52) then
*-----------------User Defined flow or surface also currently used for gates 
*-----------------For generality, flow and surface elements are reserved at the 
*-----------------the boundary constraint row for each connecting channel. It is 
*-----------------likely that only a few of these will be used (e.g., orifice
*-----------------equation uses only three)
                  RowScale(DownConstraintRow)=1.D0
                  ConstraintNode = DownstreamPointer()
                  ConnectingNodes =DownstreamConnections()
                  FlowOffset = 0
                  if (CodeDown .EQ. 51) FlowOffset = MinusOne
                  StageOffset = 0
                  if (CodeDown .EQ. 52) StageOffset = PlusOne
*-----------------Flow at Constraint
                  K=K+1
                  DownConstraintIndex(M)=K
                  ConstraintPointers(K)= sfGetElement(matrix,
     &                   DownConstraintRow, DF*ConstraintNode-1)

*-----------------Stage at Constraint
                  K=K+1
                  ConstraintPointers(K)= sfGetElement(matrix,
     &                   DownConstraintRow,DF*ConstraintNode)

*-----------------Now Flow and Stage at each connecting node
                  ConstraintNode = DownstreamPointer()

                  Do 250 L=1,ConnectingNodes
                     Node = StreamEndNode(DownstreamConnect(L))
                     ConnectCol=abs(Node)*DF ! This is the "stage" column
                     K=K+1
                     ConstraintPointers(K)=sfGetElement(matrix,
     &                  DownConstraintRow,ConnectCol-1)
                     K=K+1
                     ConstraintPointers(K)=sfGetElement(matrix, 
     &                  DownConstraintRow,ConnectCol)
 250              Continue
               else
                  write(UNIT_ERROR,*) ' *** Error (ReserveMatrix)'
                  write(UNIT_ERROR,*) 'Channel Number ', M
                  write(UNIT_ERROR,*) 
     &               ' Constraint Type (Condition Number) Not Supported.'
                  call exit(1)
               Endif

            else
               write(UNIT_ERROR,*) ' *** Error (ReserveMatrix)'
               write(UNIT_ERROR,*) 'Channel Number ', M
               write(UNIT_ERROR,*) ' Bad Constraint Row'
               call exit(1)
            end if

            OK = CloseChannel()

         else
            write(UNIT_ERROR,*) ' ***Error (ReserveMatrix)'
            write(UNIT_ERROR,*) ' Could not open channel...',
     &           chan_geom(ChannelNumber).chan_no
            call exit(1)
         end if

 300  continue


*-----Matrix Elements are now allocated for elements involving

*-----only channels. The remainder of the
*-----matrix consists of elements involving reservoirs, 
*-----gates, transfers, etc.

*-----only channels. The remainder of the matrix consists
*----- of elements involving reservoirs, gates, transfers, etc. 



*------- Allocate elements for reservoir mass balance and 
*        connection equations. Gates involving reservoirs will
*        be allocated later.

      if(Nreser.GT.0) then
*--------K: index of current matrix entry
         K=0
         ResRow=TotalChanRows

         Do II = 1,Nreser
            I=I+1
            ResRow=ResRow+1     ! Increment for the reservoir. ResRow now
                                ! points to the mass balance equation for the
                                ! reservoir, and column=resrow is the column
                                ! corresponding to reservoir stage

            RowScale(ResRow)=ResScaleFactor
            ColumnScale(ResRow)=ZScaleFactor
            ResEqRow(ii)=ResRow
*-----------First element is on diagonal and represents the reservoir
*-----------water surface height
            K=K+1
            ResEqIndex(II) = K
            ResEqPointer(K) = sfGetElement(matrix,ResRow,ResRow)

*-----Reserve stage elements for connecting channels
*     Entries will occur in the row corresponding
*     to the reservoir mass balance, the row describing the reservoir
*     equation and the row corresponding to the channel (actually node) 
*     continuity equation.

            Do JJ =1, res_geom(ii).nconnect
               I=I+1
	         RowScale(ResRow+jj) = ResConnectScaleFactor
               ResChannel = ResConnectingChannels(ii,jj)
               if(ResChannel.GT.0)then
                  NodeContinuityRow=UpConstraintEq(ResChannel)
               else
                  NodeContinuityRow=DownConstraintEq(-ResChannel)
               end if
               ChannelCol = StreamEndNode(ResChannel)*DF ! "Z" entry 
               ! contribution to reservoir mass
               K=K+1
               ResEqPointer(K) = sfGetElement(matrix,ResRow,ResRow+jj)
               ! reservoir equation at connection
	         ! ZRes
               K=K+1
               ResEqPointer(K) = sfGetElement(matrix,ResRow+jj,ResRow) 
               ! QRes
			 K=K+1
               ResEqPointer(K) = sfGetElement(matrix,ResRow+jj,ResRow+jj) 
               ! ZChan
			 K=K+1
               ResEqPointer(K) = sfGetElement(matrix,ResRow+jj,ChannelCol) 
               K=K+1            ! contribution to node mass
               ResEqPointer(K) = sfGetElement(matrix,NodeContinuityRow,ResRow+jj)
            enddo
            ResRow=ResRow+res_geom(ii).nnodes
         enddo
      end if

c     Allocate elements for the gates. Gates are implemented as the sum of a number
c     of flows from devices; the summation equation and the individual gate 
c     device equations each has its own row.
      devRow=TotalChanResRows ! starting row for device equations minus 1
      K=0
      if(NGate.GT.0)then
         Do II = 1,NGate
            currentGate=>gateArray(II)
            objType=currentGate.objConnectedType
            objID=currentGate.objConnectedID
	      ! Find the reference channel for the node connected to this gate
	      ! and corresponding computational node and column
            node=currentGate.node
            NodeReferenceChannel=node_geom(currentGate.node).sumQChan
            if (NodeReferenceChannel .lt. 0) then                   !!!!!!!????????
               NodeContinuityRow=DownConstraintEq(-NodeReferenceChannel)    
            else
               NodeContinuityRow=UpConstraintEq(NodeReferenceChannel)
            end if
            ConnectCompPt = currentGate.nodeCompPoint
            ZnodeCol=DF*ConnectCompPt   ! Col for stage at reference channel
            if(gateArray(II).flowDirection .gt. 0)then
	         ZObjCol=DF*DownCompPointer(objID)
	      else
	         ZObjCol=DF*UpCompPointer(objID)
	      end if
            QObjCol=ZObjCol-1

c	      Allocate equations in GateEqRow, which represents:
c	       1) the channel flow or reservoir connection flow as the
c		      sum of device flow:
c		      Qchan=signconvention*sum(Qdev1+Qdev2)
c                or
c            2) an equal stage constraint if the gate is uninstalled
c           Note that Qchan, Zobj and Znode columns are allocated, but
c           will never all be used at once -- in case (1), the Qchan 
c           column is used and in case (2) the Zobj and Znode columns are used
            if(objType.EQ.obj_channel)then
               if(currentGate.flowDirection .EQ. 1.D0) then      !downstream end
                  GateEqRow(ii)=DownConstraintEq(objID)
                  indx=DownConstraintIndex(objID)
               elseif( currentGate.flowDirection .EQ. -1.D0)then !upstream end
                  GateEqRow(ii)=UpConstraintEq(objID)
                  indx=UpConstraintIndex(objID)
               else
                  write(unit_error,310) currentGate.name
 310              format(/'Flow direction not set correctly for gate',a)
                  call exit(2)
               end if
               K=K+1
               GateEqIndex(II)=K
               GateEqPointer(K)=ConstraintPointers(indx) ! Q water body
               K=K+1
               GateEqPointer(K)=ConstraintPointers(indx+1) ! Z water body
               K=K+1
               GateEqPointer(K)=ConstraintPointers(indx+3) ! Z at node (reference channel)
            else if(objType.EQ.obj_reservoir)then
               ResRow=ResEqRow(objID)         ! Reservoir volume equation
               con=GateArray(ii).subLocation  ! Reservoir connection (index)
               GateEqRow(ii)=ResRow+con
	         ZobjCol=ResRow             ! Column for stage at gated water body
               ZnodeCol=DF*ConnectCompPt  ! Col for stage at reference channel
	         QObjCol=ResRow+con       ! Col representing total flow through 
			                          ! the gated reservoir connection
               K=K+1
               I=I+1
               GateEqIndex(II)=K
               GateEqPointer(K) = sfGetElement(matrix,GateEqRow(ii), QobjCol)
               K=K+1              
               GateEqPointer(K) = sfGetElement(matrix,GateEqRow(ii), ZobjCol)
               K=K+1                 
               GateEqPointer(K) = sfGetElement(matrix,GateEqRow(ii), ZnodeCol)  
               ! gate flow contribution to node continuity
			 K=K+1
               GateEqPointer(K) = sfGetElement(matrix,NodeContinuityRow,QobjCol)
               ! gate flow contribution to reservoir mass
               K=K+1         
               GateEqPointer(K) = sfGetElement(matrix,ResRow, QobjCol)
            end if
c           Allocate matrix elements for the equations representing individual devices

	      !  Add weir/pipe equations for each device in the gate
            do jj=1,currentGate.nDevice
	         I=I+1
		     devrow=devrow+1
	         QdevCol=devrow
	         currentGate.Devices(jj).calcRow=devrow
	         ! Contribution of device total gate flow 
	         K=K+1
	         GateEqPointer(K)=sfGetElement(matrix,gateEqRow(ii),QdevCol)
               K=K+1
               GateEqPointer(K)=sfGetElement(matrix, devrow, QdevCol) 
               K=K+1
               GateEqPointer(K)=sfGetElement(matrix,devrow,ZobjCol)
               K=K+1
               GateEqPointer(K)=sfGetElement(matrix,devrow,ZnodeCol)
               RowScale(devrow)=1./(128.)
            end do

         end do
      end if

      if (k .gt. MaxGatePtr)then
	   write(UNIT_ERROR,*)"Maximum number of matrix elements allocated for gates"
	   write(UNIT_ERROR,*)" exceeded. Reallocate MaxGatePtr"
	   call exit(3)
      end if

*-----End of allocation. Check for error and exit.
      if( I.EQ. Equations ) then

         ReserveMatrix = .TRUE.

      else

         write(UNIT_ERROR,*) ' *** Error (ReserveMatrix)'
         write(UNIT_ERROR,*) I,' rows allocated, ',equations,' equations expected...'
         write(UNIT_ERROR,*) ' Abnormal program end.'
         call exit(1)

      end if

      return
      end

*==== EOF solvefpt.f =========================================

