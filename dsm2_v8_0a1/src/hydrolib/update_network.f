*== Public (UpdateNetwork) =============================================

      LOGICAL FUNCTION UpdateNetwork()
      Use Gates,only: NGate, GateArray

      use IO_Units
      use grid_data
      IMPLICIT NONE

*   Purpose:  Determine values of discharge and water surface elevation
*             within a network of open channels at a new time.

*   Arguments:

*   Argument Definitions:

*   Module data:


      INCLUDE '../hydrolib/network.inc'
      INCLUDE '../hydrolib/netcntrl.inc'
      INCLUDE '../hydrolib/chconnec.inc'
      INCLUDE '../hydrolib/solver.inc'

      INCLUDE '../hydrolib/chstatus.inc'

      INCLUDE '../hydrolib/netbnd.inc'
*   Local variables:
      INTEGER ChannelNumber
      LOGICAL OK
     &     ,ClosedIteration     ! indicator that iteration has been closed
*   Routines by module:

***** Channel schematic:
      INTEGER,EXTERNAL ::  NumberOfChannels,NetworkTimeIncrement

      LOGICAL  OpenChannel, CloseChannel
      EXTERNAL OpenChannel, CloseChannel

***** Network control:
      LOGICAL  OpenNetworkIteration, CloseNetworkIteration
      LOGICAL  VariableStreamDensity
      EXTERNAL VariableStreamDensity
      EXTERNAL OpenNetworkIteration, CloseNetworkIteration

      LOGICAL  IncrementNetworkIteration
      EXTERNAL IncrementNetworkIteration

      LOGICAL  IncrementNetworkTimeStep, SetNetworkBoundaryEqValues
      EXTERNAL IncrementNetworkTimeStep, SetNetworkBoundaryEqValues

      LOGICAL  SetBoundaryValuesFromData,ApplyBoundaryValues
      EXTERNAL SetBoundaryValuesFromData,ApplyBoundaryValues

      INTEGER  NetworkIteration
      EXTERNAL NetworkIteration

      LOGICAL  CalculateReservoirFlow
      EXTERNAL CalculateReservoirFlow

      LOGICAL  CalculateGateFlow
      EXTERNAL CalculateGateFlow

      LOGICAL StepOpRuleExpressions,AdvanceOpRuleActions
      EXTERNAL StepOpRuleExpressions,AdvanceOpRuleActions

      LOGICAL TestOpRuleActivation
      EXTERNAL TestOpRuleActivation



***** Solver:
      LOGICAL  SolveFourPt
      EXTERNAL SolveFourPt

***** Locals:
      LOGICAL  UpdateChannel, NetworkClosure
      EXTERNAL UpdateChannel, NetworkClosure

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:   Lew DeLong
*   Last modified: December 1992
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      UpdateNetwork = .FALSE.

c-----Load data from time series to boundary objects
      OK = SetBoundaryValuesFromData()

c-----Advance gate operating rules, which may alter boundary values
      OK = AdvanceOpRuleActions(dfloat(NetworkTimeIncrement()))
c-----Boundary values are final, apply to mdel    
      OK = ApplyBoundaryValues()

c-----Initialize (to zero) known part of right hand side vector and
c-----elapsed iterations 
      
      ClosedIteration=.false.
      Iteration=0
      Rescale=1.D0
      XOld=0.0
      do 100 while (.not. ClosedIteration)

*--------Begin iteration loop and clear matrix
         OK = IncrementNetworkIteration()

*--------Set iteration-specific part of 
*        right hand side to zero
         XAdj=0.

         if(NGate.GT.0)then
c-----------Calculate flow through all the gates
            OK = CalculateGateFlow()
         end if

         do 200 ChannelNumber = 1, NumberOfChannels()
            if( OpenChannel(ChannelNumber) ) then
               if( UpdateChannel() ) then
                  OK = CloseChannel()
               else
                  write(UNIT_ERROR,*) ' Update of channel',
     &                 chan_geom(ChannelNumber).chan_no,' failed...'
                  return
               end if
            else
               OK = CloseChannel()
            end if
	

 200     continue

         
         if(Nreser.GT.0)then
c-----------Calculate Reservoir flows
            OK = CalculateReservoirFlow()
         end if

*--------Solve for incremental change in dependent variables.

         if( SolveFourPt() ) then
         else
            write(UNIT_ERROR,*) ' *** Error (UpdateNetwork)'
            write(UNIT_ERROR,*) ' Failed to solve matrix...'
            return
         end if

*--------Update dependent variables and check for closure.
         closediteration=NetworkClosure()
 100  end do

*-----End iteration loop.
      OK = CloseNetworkIteration()

*-----Test operating rules for activation/finalization
      OK = StepOpRuleExpressions(dfloat(NetworkTimeIncrement()))
      OK = TestOpRuleActivation(dfloat(NetworkTimeIncrement()))

      UpdateNetwork = .TRUE.

      return
      end

*== Public (UpdateChannel) =============================================

      LOGICAL FUNCTION UpdateChannel()
      use IO_Units
      IMPLICIT NONE

*   Purpose:  Update dependent variables defining a channel.  In this
*             implementation, dependent variables are flow and water-
*             surface elevation.

*   Arguments:

*   Argument definitions:

*   Module data:

*   Local variables:
      INTEGER Node, I

*   Routines by module:

***** Channel schematic:
      INTEGER  UpstreamPointer, DownstreamPointer
      EXTERNAL UpstreamPointer, DownstreamPointer

***** Flow equations:
      LOGICAL  DynamicWaveEq,DynamicWaveEqDS
      EXTERNAL DynamicWaveEq,DynamicWaveEqDS

***** Network control:
      LOGICAL  VariableStreamSinuosity
      EXTERNAL VariableStreamSinuosity

***** Locals:
      LOGICAL  SetConstraint
      EXTERNAL SetConstraint

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      UpdateChannel = .TRUE.

*-----Compute and store constraint-equation coefficients for
*      upstream end of channel.

      IF( SetConstraint(+1) ) THEN
      ELSE
         UpdateChannel = .FALSE.
         WRITE(UNIT_ERROR,*) ' *** Error (UpdateChannel)'
         WRITE(UNIT_ERROR,*) ' Attempt to set upstream constraint failed...'
      END IF

*-----Compute and store intermediate general-equation coeficients for ...

      Node = 0

      if( .NOT. VariableStreamSinuosity() ) then

*--------Constant sinuosity and water density.

         do 100 I=UpstreamPointer(),DownstreamPointer()-1
            Node = Node + 1
            if( .not. DynamicWaveEq(Node, Node+1) ) then
               UpdateChannel = .FALSE.
               write(UNIT_ERROR,*) ' *** Error (UpdateChannel)'
               write(UNIT_ERROR,*) ' Failed to update node...',Node
            end if
 100     continue

      else

*--------Variable sinuosity and water density.

         do 150 I=UpstreamPointer(),DownstreamPointer()-1
            Node = Node + 1
            if( DynamicWaveEqDS(Node, Node+1) )then
            else
               UpdateChannel = .FALSE.
               WRITE(UNIT_ERROR,*) ' *** Error (UpdateChannel)'
               WRITE(UNIT_ERROR,*) ' Failed to update node...',Node
            end if
 150     continue

      end if

*-----Compute and store constraint-equation coefficients for
*      downstream end of channel.

      IF( SetConstraint(-1) ) THEN
      ELSE
         UpdateChannel = .FALSE.
         WRITE(UNIT_ERROR,*) ' *** Error (UpdateChannel)'
         WRITE(UNIT_ERROR,*) ' Attempt to set downstream constraint failed...'
      END IF

      RETURN
      END

*== Public (SetConstraint) =====================================

      LOGICAL FUNCTION SetConstraint(Extremity)
      use IO_Units
      IMPLICIT NONE

*   Purpose:  Select and compute appropriate channel
*             constraint-equation
*             for one end of channel
*   Arguments:

*   Argument definitions:

*   Local Variables:
      INTEGER Code, Extremity, ConstraintCode
      Logical OK

*   Routines by module:

***** Channel constraint:
      LOGICAL   ForceStreamSurface, ForceStreamFlow
      EXTERNAL  ForceStreamSurface, ForceStreamFlow

      LOGICAL   ForceSumStreamFlow, ForceEqualStreamSurface
      EXTERNAL  ForceSumStreamFlow, ForceEqualStreamSurface

***** Channel schematic:
      INTEGER   UpstreamCode,DownStreamCode
      EXTERNAL  UpstreamCode,DownStreamCode

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:   Lew DeLong
*   Last modified: December 1992
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      SetConstraint = .FALSE.

*-----Boundary-condition code key
*     
*     Last Digit = 1 : Upstream
*                  2 : Downstream
*     
*     
*       water surface  * =implemented in DSM2
*          1,2*               explicitly known
*          4                  self-setting (downstream only)
*         11,12*              equal to another water surface
* ........31,32               3-parameter relation
* ........41,42               (reserved for four-parameter rating)
* ........51,52*              user-defined (e.g. gate)
*     
*         Note: User-defined codes can have four digits, last
*         two of which will be 51 or 52
*****-
*-----Set channel-extremity code to upstream.

      If(Extremity .EQ. 1)Then
         ConstraintCode = UpstreamCode()
      Else If(Extremity .EQ. -1)Then
         ConstraintCode = DownstreamCode()
      Else
         WRITE(UNIT_ERROR,*) ' *** Error (SetConstraint)'
         WRITE(UNIT_ERROR,*) ' Inappropriate extremity code (not +1/-1).'
         RETURN
      End If

*-----Get upstream boundary-condition code.
      Code = MOD(ConstraintCode, 100)
      OK = .FALSE.

*-----Set appropriate constraint.
      If (Code.EQ.1)Then
         IF( ForceStreamSurface(Extremity) ) THEN
         ELSE
            RETURN
         END IF
      Else If (Code.EQ.2)Then
         IF( ForceStreamFlow(Extremity) ) THEN
         ELSE
            RETURN
         END IF

      Else If (Code.EQ.11)Then
         IF( ForceEqualStreamSurface(Extremity) ) THEN
         ELSE
            RETURN
         END IF

      Else If (Code.EQ.12)Then
         IF( ForceSumStreamFlow(Extremity) ) THEN
         ELSE
            RETURN
         END IF

      Else If (Code.EQ.51 .OR.  Code.EQ.52)Then
         IF(IABS( MOD( ConstraintCode/100 , 100 ) ) .EQ.4 ) THEN
         ELSE
            RETURN
         END IF
      Else
         WRITE(UNIT_ERROR,*) ' *** Error (SetConstraint)'
         WRITE(UNIT_ERROR,*) ' Condition code ',Code,' not implemented.'
         RETURN

      End If

      SetConstraint = .True.

      RETURN
      END

*== Public (NetworkClosure) ============================================

      LOGICAL FUNCTION NetworkClosure()
      Use Gates, only: NGate, GateArray
      use grid_data
      IMPLICIT NONE

*   Purpose:  Check for closure of iteration on a set of equations
*             describing flow in a network of open channels, and
*             update dependent variables.

*   Arguments:

*   Argument definitions:

*   Module data:

*   Local Variables:
      INTEGER I, J
      INTEGER ChannelQ, NodeQ, ChannelZ, NodeZ
      INTEGER ChannelNumber, Node
      REAL*8    SmallQ
      PARAMETER (SmallQ = 1000.0)
      REAL*8    Change, CurrentValue, CurrentRatioQ, CurrentChangeZ
      REAL*8    MaxChangeZ, MaxRatioQ
      LOGICAL OK

*   Routines by module:
      INCLUDE '../hydrolib/network.inc'
      INCLUDE '../hydrolib/netcntrl.inc'
      INCLUDE '../hydrolib/chconnec.inc'
      INCLUDE '../hydrolib/solver.inc'

***** Channel flow status:
      LOGICAL  SetStreamSurfaceElevation, SetStreamFlow
      EXTERNAL SetStreamSurfaceElevation, SetStreamFlow

      REAL*8     StreamFlow, StreamSurfaceElevation
      EXTERNAL StreamFlow, StreamSurfaceElevation

      LOGICAL  StoreNetworkClosure
      EXTERNAL StoreNetworkClosure

***** Channel schematic:
      INTEGER  NumberOfStreamLocations
      EXTERNAL NumberOfStreamLocations

      INTEGER  NumberOfChannels, MaxNetworkIterations
      EXTERNAL NumberOfChannels, MaxNetworkIterations

      LOGICAL  OpenChannel, CloseChannel
      EXTERNAL OpenChannel, CloseChannel

***** Solver:
      REAL*8     IncrementalZ, IncrementalQ,IncrementalZRes
      EXTERNAL IncrementalZ, IncrementalQ,IncrementalZRes

***** Network control:
      LOGICAL  IntermediateNetworkResults
      EXTERNAL IntermediateNetworkResults

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:   Lew DeLong
*   Last modified: December 1992
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      MaxRatioQ  = 0.0
      MaxChangeZ = 0.0
      MaxChangeQ = 0.0

      DO 200 I=1,NumberOfChannels()

         ChannelNumber = I

         IF( OpenChannel(ChannelNumber) ) Then

            DO 100 J = 1,NumberOfStreamLocations()
               Node = J

*--------------Check for closure on flow.

               Change = IncrementalQ(Node)
               CurrentValue = StreamFlow(Node)

               IF( ABS(CurrentValue) .GT. SmallQ  ) THEN
*-----------------Flow is greater than a small flow.
                  CurrentRatioQ = ABS(Change/CurrentValue)
               ELSE
*-----------------Flow is smaller than a small flow.
                  CurrentRatioQ = ABS(Change/SmallQ)
               END IF

               IF(ABS(Change).GT. MaxChangeQ) THEN
                  MaxChangeQ=ABS(Change)
                  MaxChangeChannelQ=ChannelNumber
               ENDIF
               IF( CurrentRatioQ .GT. MaxRatioQ ) THEN
                  MaxRatioQ = CurrentRatioQ
                  NodeQ = Node
                  ChannelQ = ChannelNumber
               END IF

*--------------Update flow.
               OK = SetStreamFlow( Node, CurrentValue + Change )

*--------------Check for closure on water-surface elevation.

               Change = IncrementalZ(Node)
               CurrentChangeZ = ABS(Change)
               CurrentValue = StreamSurfaceElevation(Node)

               IF( CurrentChangeZ .GT. MaxChangeZ ) THEN
                  MaxChangeZ = CurrentChangeZ
                  NodeZ = Node
                  ChannelZ = ChannelNumber
               END IF

*--------------Update water-surface elevation.
               OK = SetStreamSurfaceElevation(Node, CurrentValue + Change)

 100        CONTINUE

            OK = CloseChannel()

         ELSE
         END IF

 200  CONTINUE

      do i = 1,Nreser
*--------Adjust reservoir height from iteration
*     channel Z is already adjusted
         Change=X(ResEqRow(i))
         Yres(i) = Yres(i) + Change
         do j=1,res_geom(i).nnodes
            QRes(i,j)=QRes(i,j) + X(ResEqRow(i)+j)
         end do
      end Do

*-----Adjust gate device flows from iteration
      do i=1,NGate
         do j=1,GateArray(i).nDevice
            Change=X(GateArray(i).devices(j).calcRow)
            GateArray(i).devices(j).flow=GateArray(i).devices(j).flow
     &           +Change        ! calcRow =device column in X
         end do
      end do

*-----Check for overall closure and store closure information.
      MaxRatioQ = MaxRatioQ/rescale
      MaxChangeZ = MaxChangeZ/rescale

      IF( StoreNetworkClosure(
     &     MaxRatioQ, ChannelQ, NodeQ,
     &     MaxChangeZ, ChannelZ, NodeZ
     &     )
     &     ) THEN
         NetworkClosure = .TRUE.
      ELSE
         NetworkClosure = .FALSE.
      END IF

*-----Write intermediate results.

      OK = IntermediateNetworkResults()

      RETURN
      END

*==== EOF fourpt =======================================================
