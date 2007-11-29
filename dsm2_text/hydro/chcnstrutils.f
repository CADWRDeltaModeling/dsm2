C!    Copyright (C) 1996-1999 State of California,
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
C!    DSM2 - SPARSE LIBRARY INTERFACE COPYRIGHT
C!    AdjustReservoir() Copyright (C) 1998-1999 Eli Ateljevich
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
C!    Tara Smith
C!    California Dept. of Water Resources
C!    Division of Planning, Delta Modeling Section
C!    1416 Ninth Street
C!    Sacramento, CA  95814
C!    916-653-9885
C!    tara@water.ca.gov
C!
C!    or see our home page: http://baydeltaoffice.water.ca.gov/modeling/deltamodeling/
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

*== Public (AdjustReservoirFlow) ===================================

      LOGICAL FUNCTION AdjustReservoirFlow()

      IMPLICIT NONE

*   Purpose:  Adjust flow between reservoirs and channels
*             To make sure that continuity is satisified
*             at the junctions.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chconnec.inc'
      INCLUDE 'chstatus.inc'

      include '../input/fixed/common.f'
      include '../input/time-varying/tide.inc'

*   Local Variables:
      INTEGER i,j,k,l,m,Extremity
      REAL*8    dZres,dZChan
      integer lnblnk
      real*8 reservoir_source_sink
      external reservoir_source_sink

*   Routines by module:

      INTEGER  NumberOfStreamLocations,NumberOfChannels
      EXTERNAL NumberOfStreamLocations,NumberOfChannels

      INTEGER  UpstreamConnections,UpstreamConnect
      INTEGER  DownstreamConnections,DownstreamConnect

      EXTERNAL UpstreamConnections,UpstreamConnect
      EXTERNAL DownstreamConnections,DownstreamConnect

      INTEGER  UpstreamPointer, DownstreamPointer,StreamEndNode
      EXTERNAL UpstreamPointer, DownstreamPointer,StreamEndNode

      REAL*8 StreamSurfaceElevation
      EXTERNAL StreamSurfaceElevation
***** Solver:
      REAL*8     IncrementalZ,GlobalStreamFlow,IncrementalZRes
      EXTERNAL IncrementalZ,GlobalStreamFlow,IncrementalZRes

      INTEGER  NetworkTimeIncrement
      EXTERNAL NetworkTimeIncrement

*   Programmed by: Parviz Nader
*   Date:          December 1994
*   Modified by:
*   Last modified:

*-----Implementation -----------------------------------------------------

      AdjustReservoirFlow=.FALSE.
      DO Branch = 1, NumberOfChannels()
         DO m=1,2
            ReservoirFlow(Branch,m)=0.
         ENDDO
      ENDDO

      Do i = 1,Nres
         TotalResFlow(i)=0.
         dZRes = IncrementalZRes(i)

*--------Test for dry reservoir and warn/exit

         IF (Yres(i) .lt. Dres(i)) THEN
            WRITE(UNIT_ERROR,9001)
     &           res_geom(i).name(:lnblnk(res_geom(i).name)),
     &           Yres(i),Dres(i)
 9001       FORMAT(' Reservoir ',a,' Dried up.  Yres=',
     &           f10.3,' Dres=',f10.3)
            CALL EXIT(1)
         ENDIF
         If (res_geom(i).maxstage .ne. miss_val_r .and.
     &        Yres(i) .gt. res_geom(i).maxstage) then
            write(unit_error, 9002)
     &           res_geom(i).name(:lnblnk(res_geom(i).name)),
     &           yres(i),res_geom(i).maxstage
 9002       format(' Reservoir ',a,' stage ',f10.3
     &           /' exceeded maximum stage of ',f10.3)
            call exit(1)
         endif

*--------Add external flow
         TotalResFlow(i) = TotalResFlow(i) - reservoir_source_sink(i,QEXT_FLOWS)

*--------Calculate flow for each connection
         DO j=1,NconnectReservoir(i)
            k=ResConnectingChannels(i,j)
            IF (k.GT.0) THEN
c--------------Reservoir connected to the upstream end of the channel
               Extremity=1
               l=1
               m=1
               Branch=k
            ELSE
c--------------Reservoir connected to the downstream end of the channel
               Extremity=-1
               k=ABS(k)
               Branch=k
               l=NumberOfStreamLocations()
               m=2
            ENDIF

            dZchan = IncrementalZ(l)
            QRes(i,j) = QRes(i,j) + dQResdZres(i,j)*dZres - dQResdZRes(i,j)*dZchan

C-----------Save these for water balance and tracking.
C-----------Remove ReservoirFlow?
            TotalResFlow(i)=TotalResFlow(i)+QRes(i,j)
            ReservoirFlow(Branch,m)=ReservoirFlow(Branch,m)+QRes(i,j)
            Branch=0
         End Do
      End Do

c-----add object-to-object flows into reservoir flow vector
      do i=1,nobj2obj
         if (obj2obj(i).from.object .eq. obj_reservoir) then
            k=obj2obj(i).from.object_no
            TotalResFlow(k)=TotalResFlow(k)+obj2obj(i).flow
         endif
         if (obj2obj(i).to.object .eq. obj_reservoir) then
            k=obj2obj(i).to.object_no
            TotalResFlow(k)=TotalResFlow(k)-obj2obj(i).flow
         endif
      enddo

      Branch = 0
      AdjustReservoirFlow=.TRUE.

      RETURN
      END

*== Public (CheckSteadyState) ===================================

      LOGICAL FUNCTION CheckSteadyState()

      IMPLICIT NONE

*   Purpose: Check stage at locations to see if steady-state condition
*   has been reached

      include '../input/fixed/common.f'

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chnlcomp.inc'
      INCLUDE 'chstatus.inc'
      INCLUDE 'chconnec.inc'
      INCLUDE '../input/time-varying/common_tide.f'

*   Local Variables:

      INTEGER i,NumLoc,Problem_chan,Problem_Grid_point,n
      REAL*8    OldWS(MaxLocations),TotalDiff,AvgDiff,MaxDiff
      real*8    max_volume_ratio,volume_ratio
      integer prob_vol_chan
      logical OK

      DATA OldWS / MaxLocations * miss_val_r /

*   Routines by module:
      LOGICAL    Compute_ChArea
      EXTERNAL   Compute_ChArea
***** Channel schematic:

*   Programmed by: Parviz Nader
*   Date:          April 1994
*   Modified by:   Ralph Finch
*   Last modified: September 1996

*-----Implementation -----------------------------------------------------

      CheckSteadyState=.FALSE.

      NumLoc=UpCompPointer(NumCh)+NumberofCompLocations(NumCh)-1
      if (OldWS(1) .ne. miss_val_r) THEN ! don't check convergence first time around
         TotalDiff=0.
         maxdiff=0.
         Do n=1,Numch
            do i=UpCompPointer(n),UpCompPointer(n)+NumberofCompLocations(n)-1
               TotalDiff=TotalDiff+ABS(WS(i)-OldWS(i))
               if(MaxDiff.lt.ABS(WS(i)-OldWS(i))) then
                  MaxDiff=ABS(WS(i)-OldWS(i))
                  Problem_chan=int2ext(n)
                  Problem_Grid_Point=i-UpCompPointer(n)+1
               endif
            enddo
         ENDDO
         max_volume_ratio=0.
         OK=Compute_ChArea()
         do n=1,numch
            QchNet(n)=QchNet(n)*dfloat(time_step)/dfloat(tide_cycle_length_mins)
            volume_ratio=QchNet(n)*dfloat(tide_cycle_length_mins)*60./
     &           (AChan_Avg(n)*dfloat(chan_geom(int2ext(n)).length))
c! Note: QchNet at this point is the amount of water volume gained(+) or lost(-)
c!       within a tidal cycle. If the model reaches a true dynamic state then
c!       QchNet should in fact be zero for all channels.
c!       A volume_ratio=0.01 means that it takes 100 tidal days for this channel
c!       to double in volume and a value of -0.01 means that the same channel
c!       would in fact dry up in 100 tidal days. (This is in case no other
c!       adjustments are done)
            if(abs(volume_ratio) .gt. 0.01 .and. print_level .ge. 3)then
               write(*,901) int2ext(n),qchnet(n)
 901           format(' chan=',i4,' qnet=',f15.5)
            endif
            if(abs(volume_ratio).gt.max_volume_ratio)then
               max_volume_ratio=abs(volume_ratio)
               prob_vol_chan=int2ext(n)
            endif
            QchNet(n)=0.
         enddo

         AvgDiff=TotalDiff/DFLOAT(NumLoc)
         if (print_level .ge. 2) then
            write(unit_screen,900)AvgDiff,Repeat_Stage_Tol,MaxDiff,
     &           Problem_chan,Problem_Grid_point
 900        format(' Average stage diff for repeating tide=',f10.4,
     &           '  Tolerance=',f10.4/
     &           ' Max stage diff  =',f10.4,' For channel =',i5,5x,'  Grid Point #',i3)
            write(unit_screen,902)max_volume_ratio,prob_vol_chan,
     &           Repeat_Flow_Tol
 902        format(' Max flow ratio  =',f10.4,' For channel =',i5,5x, '  Tolerance=',f10.4)
         endif
         IF (AvgDiff .LE. Repeat_Stage_Tol .and.
     &        max_volume_ratio .le. Repeat_Flow_Tol) THEN
            CheckSteadyState=.TRUE.
         ENDIF
      ENDIF
      DO i=1, NumLoc
         OldWS(i)=WS(i)
      ENDDO

      RETURN
      END

*== Public (InitGates) ===================================

      LOGICAL FUNCTION InitGates()

      IMPLICIT NONE

*   Purpose: Modify Condition Codes for channels with gates

*   Arguments:

*   Argument definitions:
*     Extremity - index indicating
*                   [+1] upstream end of current channel.
*                   [-1] downstream end of current channel.
*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chconnec.inc'

      include '../input/fixed/common.f'

*   Local Variables:

      INTEGER  ListCh(MaxConnectingChannels), NewListCh(MaxConnectingChannels)
      INTEGER ChannelNumber,Extremity,NumConnectChan,Code
      INTEGER NumStageChannels, ListStageChannels(10)
      INTEGER NumSumFlow,SumFlowChannel,GateCode
      INTEGER i,j,ich
      LOGICAL OK

*   Routines by module:

***** Channel schematic:
      INTEGER  NumberOfChannels
      EXTERNAL NumberOfChannels

      LOGICAL  OpenChannel, CloseChannel, ChangeBoundaryCode,
     &     ListGateChannels, ChangeChannelConnections,
     &     ListChannelConnections
      EXTERNAL OpenChannel, CloseChannel, ChangeBoundaryCode,
     &     ListGateChannels, ChangeChannelConnections,
     &     ListChannelConnections

*   Programmed by: Parviz Nader
*   Date:          November 1992
*   Modified by:
*   Last modified:

*-----Implementation -----------------------------------------------------

      InitGates=.FALSE.

      DO  ChannelNumber = 1, NumberOfChannels()
         IF (OpenChannel(ChannelNumber)) then
            DO j=1,2
               if (j.eq.1) THEN
                  Extremity=1
               ELSE
                  Extremity=-1
               ENDIF
               IF( GateNumber(ChannelNumber,j).ne.0) then
*-----------------Gate exists at this end of the channel
                  OK=ListGateChannels(ChannelNumber,Extremity,NumStageChannels,
     &                 ListStageChannels,NumSumFlow,SumFlowChannel,GateCode)
               ELSE
                  GOTO 20
               ENDIF
               IF (NumSumFlow.EQ.1) THEN
*-----------------A connecting channel with sum of flow condition already exists
*-----------------This is very easy. Just change the condition code.
                  OK = ChangeBoundaryCode(ChannelNumber,Extremity,10452)
                  GOTO 20
               ELSEIF (NumSumFlow.EQ.0) THEN
*-----------------A connecting channel with sum of flow condition does not exist
*-----------------Change one of the condition codes from the neighboring channels
*-----------------to Sum of flow condition

                  OK = ListChannelConnections(ChannelNumber,Extremity,NumConnectChan,ListCh,Code)
*-----------------switch the list
                  ich=ListCh(1)
                  ListCh(1)=ChannelNumber

                  if (ich.GT.0) THEN
                     OK = ChangeChannelConnections(ich,1,NumConnectChan,ListCh)
                     OK = ChangeBoundaryCode(ich,1,12)
                  ELSE
                     ich=-ich
                     OK = ChangeChannelConnections(ich,-1,NumConnectChan,ListCh)
                     OK = ChangeBoundaryCode(ich,-1,12)
                     ich=-ich
                  ENDIF

                  DO i=1, NumConnectChan
                     NewListCh(i)=  ListCh(i)
                  ENDDO

                  ListCh(1)=ich

                  DO i=1, NumConnectChan
                     ich=NewListCh(i)
                     if (ich.GT.0) THEN
                        OK = ChangeChannelConnections(ich,1,1,ListCh)
                     ELSE
                        ich=-ich
                        OK = ChangeChannelConnections(ich,-1,1,ListCh)
                     ENDIF
                  ENDDO
               ELSE
                  WRITE(UNIT_ERROR,908)Branch
 908              FORMAT(' ERROR... Problem with placement of gate in channel: ',i4)
               ENDIF

c--------------OK = ChangeChannelConnections(ChannelNumber,Extremity,0,ListCh)

               OK = ChangeBoundaryCode(ChannelNumber,Extremity,10452)

 20            CONTINUE
            ENDDO
            OK = CloseChannel()
         ELSE
            WRITE(UNIT_ERROR,904) int2ext(ChannelNumber)
 904        FORMAT(' ERROR...  Channel Number:',i4,' already open')
            CALL EXIT(1)
         ENDIF
      ENDDO

      InitGates=.TRUE.
      RETURN
      END

*== Public (ListGateChannels) ===================================

      LOGICAL FUNCTION ListGateChannels (ChannelNumber,Extremity,NumStageChannels,
     &     ListStageChannels,NumSumFlow,SumFlowChannel,GateCode)

      IMPLICIT NONE

*   Purpose:

*-----Figure out how many channels are connected to this channel at
*-----the gated end.  Also determine how many connecting channels have
*-----'Equal stage' boundary code and if there is a connecting channel
*-----with 'Sum of Flow' Boundary Code.  Also find out stage from
*-----which channel should be used for upstream end of the gate.

*   Arguments:
      INTEGER ChannelNumber,Extremity,NumStageChannels
      INTEGER ListStageChannels(10),NumSumFlow,SumFlowChannel,GateCode

*   Argument definitions:
*     ChannelNumber  - Channel Number
*     Extremity - index indicating
*                   [+1] upstream end of current channel.
*                   [-1] downstream end of current channel.
*     NumStageChannels  - Number of channels connected to the gated end
*                         with 'Equal Stage' Boundary Code
*     ListStageChannels - List of channels connected to the gated end
*                         with 'Equal Stage' Boundary Code
*     SumFlowChannel    - Channel connected to the gated end
*                         with 'Equal Stage' Boundary Code
*     GateCode          - Boundary Code for the channel at the gated end

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chconnec.inc'

      include '../input/fixed/common.f'

*   Local Variables:
      INTEGER i,ListCh(MaxConnectingChannels),ich,Code,NumConnectChan,index
      LOGICAL OK

*   Routines by module:

***** Channel schematic:

      INTEGER  UpstreamConnections, UpstreamConnect
      EXTERNAL UpstreamConnections, UpstreamConnect

      INTEGER  DownstreamConnections, DownstreamConnect
      EXTERNAL DownstreamConnections, DownstreamConnect

      LOGICAL  ListChannelConnections
      EXTERNAL ListChannelConnections

*   Programmed by: Parviz Nader
*   Date:          December 1992
*   Modified by:
*   Last modified:

*-----Implementation -----------------------------------------------------

      ListGateChannels=.FALSE.
      IF (Extremity.EQ.1) then
         index=1
      ELSEIF (Extremity.EQ.-1) then
         index=2
      ELSE
         WRITE(UNIT_ERROR,*) ' *** Error(ListGateChannels)'
         WRITE(UNIT_ERROR,*) ' Extremity not equal +1 or -1 ...'
         CALL EXIT(1)
      ENDIF
      NumStageChannels=0
      Do i=1, MaxConnectingChannels
         ListStageChannels(i)=0
         ListCh(i)=0
      ENDDO
      NumSumFlow=0
      SumFlowChannel=0

      OK = ListChannelConnections(ChannelNumber,Extremity,NumConnectChan,ListCh,GateCode)

*-----Determine which channel should be used for stage at the upstream end of the gate

      DO i=1, NumConnectChan
         ich= ListCh(i)
         if (ich.GT.0) THEN
            Code = MOD( UpBoundaryCode(ich), 100 )
         ELSE
            Code = MOD( DownBoundaryCode(-ich), 100 )
         ENDIF

         IF (Code.EQ.12) THEN
*-----------Sum of Flow Condition specified

            IF (NumSumFlow.NE.0) THEN
               WRITE(UNIT_ERROR,903) int2ext(ChannelNumber)
 903           FORMAT(' Error with geometry description near channel:',i4)
               CALL EXIT(1)
            ENDIF
            NumSumFlow=1
            SumFlowChannel=ich

*-----------This is a little tricky. A channel which has a 'Equal Stage'
*-----------Boundary code, is not aware of any other connecting channel
*-----------with 'Equal Stage' code. This is a way to check this
*-----------but it's hard to explain.

            IF (UpnumberOfConnections(ich).GT.1) THEN
               NumStageChannels = 1
            ENDIF

         ELSEIF (Code.EQ.11) THEN
*-----------Equal stage condition specified
            NumStageChannels= NumStageChannels + 1
            ListStageChannels(i)=ich
            IF (NumStageChannels.eq.1) GatePickChannel(ChannelNumber,index)=ich
         ENDIF
      ENDDO

      IF (GatePickChannel(ChannelNumber,index).eq.0) Then
         GatePickChannel(ChannelNumber,index)=SumFlowChannel
      ENDIF

      IF (NumSumFlow.EQ.0  .AND. NumStageChannels.EQ.0) THEN

         WRITE(UNIT_ERROR,901) int2ext(ChannelNumber)
 901     FORMAT(' ERROR.. problem with the placement of the gate in channel ',i4,/,
     &        '         You are probably placing a gate at the boundary.  ')
         CALL EXIT(1)
      ENDIF
      ListGateChannels=.TRUE.

      RETURN
      END

*== Public (ListChannelConnections) ===================================

      LOGICAL FUNCTION ListChannelConnections(ChannNum,Extremity,NumConnectChan,ListCh,Code)

      IMPLICIT NONE

*   Purpose:  List the connecting channels

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chconnec.inc'

      include '../input/fixed/misc.f'

*   Arguments:
      INTEGER Extremity, ChannNum,NumConnectChan,ListCh(MaxConnectingChannels),Code

*   Argument definitions:
*     ChannNum  - Channel Number
*
*     Extremity - index indicating
*                   [+1] upstream end of current channel.
*                   [-1] downstream end of current channel.
*     NumConnectChan - Number of connecting channels
*     ListCh - List of connecting channels
*     Code   - Boundary Code at this end of channel

*   Local Variables:

      INTEGER     i
*   Routines by module:

***** Channel schematic:

*   Programmed by: Parviz Nader
*   Date:          December 1992
*   Modified by:
*   Last modified:

*-----Implementation -----------------------------------------------------

      IF (Extremity .EQ. 1) then
         NumConnectChan=UpNumberOfConnections(ChannNum)
         DO i=1,NumConnectChan
            ListCh(i) = UpConnection((ChannNum-1) * MaxConnectingChannels + i)
         ENDDO
         Code = UpBoundaryCode(ChannNum)
      ELSEIF (Extremity .EQ. -1) then
         NumConnectChan=DownNumberOfConnections(ChannNum)
         DO i=1,NumConnectChan
            ListCh(i) = DownConnection((ChannNum-1) * MaxConnectingChannels + i)
         ENDDO
         Code = DownBoundaryCode(ChannNum)
      ELSE
         WRITE(UNIT_ERROR,*) ' *** Error(ListChannelConnections)'
         WRITE(UNIT_ERROR,*) ' Extremity not equal +1 or -1 ...'
         CALL EXIT(1)
      ENDIF

      ListChannelConnections=.TRUE.
      RETURN
      END

*== Public (ChangeChannelConnections) ===================================

      LOGICAL FUNCTION ChangeChannelConnections(ChannNum,Extremity,NumConnectChan,ListCh)

      IMPLICIT NONE

*   Purpose:  Change the connecting channels

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chconnec.inc'

      include '../input/fixed/misc.f'

*   Arguments:
      INTEGER Extremity, ChannNum,NumConnectChan,ListCh(MaxConnectingChannels)

*   Argument definitions:
*     ChannNum  - Channel Number
*
*     Extremity - index indicating
*                   [+1] upstream end of current channel.
*                   [-1] downstream end of current channel.
*     NumConnectChan - Number of connecting channels
*     ListCh - List of connecting channels

*   Local Variables:

      INTEGER   i

*   Routines by module:

***** Channel schematic:

      INTEGER   ChangeBoundaryCode
      EXTERNAL  ChangeBoundaryCode

*   Programmed by: Parviz Nader
*   Date:          December 1992
*   Modified by:
*   Last modified:

*-----Implementation -----------------------------------------------------

      IF (Extremity .EQ. 1) then
         UpNumberOfConnections(ChannNum)=NumConnectChan
         DO i=1,NumConnectChan
            UpConnection((ChannNum-1) * MaxConnectingChannels + i) = ListCh(i)
         ENDDO
      ELSEIF (Extremity .EQ. -1) then
         DownNumberOfConnections(ChannNum)=NumConnectChan
         DO i=1,NumConnectChan
            DownConnection((ChannNum-1) * MaxConnectingChannels + i) = ListCh(i)
         ENDDO
      ELSE
         WRITE(UNIT_ERROR,*) ' *** Error(ChangeChannelConnections)'
         WRITE(UNIT_ERROR,*) ' Extremity not equal +1 or -1 ...'
         CALL EXIT(1)
      ENDIF

      ChangeChannelConnections=.TRUE.
      RETURN
      END

*== Public (ChangeBoundaryCode) ===================================

      LOGICAL FUNCTION ChangeBoundaryCode(ChannNum,Extremity,Code)

      IMPLICIT NONE

*   Purpose:  Change the boundary code on a channel

*   Arguments:
      INTEGER Extremity, ChannNum,Code

*   Argument definitions:
*     ChannNum  - Channel Number
*
*     Extremity - index indicating
*                   [+1] upstream end of current channel.
*                   [-1] downstream end of current channel.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chconnec.inc'

      include '../input/fixed/misc.f'

*   Local Variables:

*   Routines by module:

***** Channel schematic:

*   Programmed by: Parviz Nader
*   Date:          November 1992
*   Modified by:
*   Last modified:

*-----Implementation -----------------------------------------------------

      IF (Extremity .EQ. 1) then

*--------Change the boundary code at the upstream end of the channel
         UpBoundaryCode(ChannNum)= Code
      ELSEIF (Extremity .EQ. -1) then
*--------Change the boundary code at the downstream end of the channel
         DownBoundaryCode(ChannNum)=Code
      ELSE
         WRITE(UNIT_ERROR,*) ' *** Error(ChangeBoundaryCode)'
         WRITE(UNIT_ERROR,*) ' Extremity not equal +1 or -1 ...'
         CALL EXIT(1)
      ENDIF

      ChangeBoundaryCode=.TRUE.
      RETURN
      END

*== Public (GateSchedule) ===================================

      LOGICAL FUNCTION GateSchedule()

      IMPLICIT NONE

* Purpose: This function calculates a flow coefficient for each gate
*          based on given time schedule or other criteria. It varies
*          the coefficient gradually within the 'lapse' time.

*   Arguments:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chconnec.inc'
      include '../input/fixed/common.f'

*   Local Variables:
      INTEGER  GateNum,n,ChannelNumber,Extremity,Delt,extchan
      real*8 x

*   Routines by module:

      Integer NetworkIteration
      External NetworkIteration

      REAL*8     TidalTime, ChannelVelocity
      EXTERNAL TidalTime, ChannelVelocity

      LOGICAL AddAtLocation
      External AddAtLocation

*   Programmed by: Mohammad Rayej
*   Date:          February 1993
*   Modified by:   Ralph Finch
*   Last modified: June 1994

* ----------------------------------------------------------------------------

      DO n=1,NumGatesOperating
         GateNum=ListGateOperating(n)
         Delt=julmin-GateOperatingTime(GateNum)
         IF (Delt .LT. GateLapseTime(GateNum) .AND. GateLapseTime(GateNum).GT.0) THEN
*-----------gradual gate operation
            IF (GatePosition(GateNum) .eq. GATE_OPEN) THEN
*--------------gradual opening
               If (GateLapseTime(GateNum).GT.0)Then
               GateOperatingCoeff(GateNum)=dfloat(Delt)/(GateLapseTime(GateNum))
               Else
               GateOperatingCoeff(GateNum)=1.0
               End If
            ELSE IF (GatePosition(GateNum) .eq. GATE_CLOSE) THEN
*--------------gradual closing
               If (GateLapseTime(GateNum).GT.0)Then
               GateOperatingCoeff(GateNum)=1.-dfloat(Delt)/(GateLapseTime(GateNum))
               Else 
               GateOperatingCoeff(GateNum)=0.0
               End If
            ENDIF
         ELSE                   ! fully open or close
            IF (GatePosition(GateNum) .eq. GATE_OPEN) THEN
               GateOperatingCoeff(GateNum)=1.0
            ELSE IF (GatePosition(GateNum) .eq. GATE_CLOSE) THEN
               GateOperatingCoeff(GateNum)=0.0
            ENDIF
         ENDIF
      ENDDO

      DO n=1,NumSpecialGates
         GateNum=ListSpecialGates(n)

         IF (GateLocation(GateNum).EQ.1) THEN
            Extremity=1
         ELSEIF (GateLocation(GateNum).EQ.2) THEN
            Extremity=-1
         ELSE
            WRITE(UNIT_ERROR,*) ' *** Error(GateSchedule)'
            WRITE(UNIT_ERROR,*) ' Extremity not equal +1 or -1 ...'
            GateSchedule=.FALSE.
            return
         ENDIF
         ChannelNumber = GateChan(GateNum)
         extchan=int2ext(ChannelNumber)
         if(Extremity.eq.1)then
            x=0.
         else
            x=dfloat(chan_geom(extchan).length)
         endif
         CurrentVelocity(GateNum)=ChannelVelocity(ChannelNumber,x)

         IF (GatePosition(GateNum) .eq. GATE_OPEN) THEN ! gate now open
*-----------Check the Velocity and overridding criteria
            IF (
     &           (GateSpecialPos(GateNum) .eq. GATE_CALC .and.
     &           (julmin-GateOperatingTime(GateNum)) .gt. GateLapseTime(GateNum) .and.
     &           CurrentVelocity(GateNum) .LT. VelocityClose(GateNum)) .or.
     &           GateSpecialPos(GateNum) .eq. GATE_CLOSE) THEN
*--------------Close the Gate

               GatePosition(GateNum)=GATE_CLOSE
               GateOperatingTime(GateNum)=julmin
            ENDIF
         ELSE                   ! gate now closed
*-----------Check the Delta H criteria and overriding criteria
            IF (
     &           (GateSpecialPos(GateNum) .eq. GATE_CALC .and.
     &           (julmin-GateOperatingTime(GateNum)) .gt. GateLapseTime(GateNum) .and.
     &           CurrentDeltaH(GateNum) .GT. DeltaHOpen(GateNum)) .or.
     &           GateSpecialPos(GateNum) .eq. GATE_OPEN) THEN
*--------------Open the gate

               GatePosition(GateNum)=GATE_OPEN
               GateOperatingTime(GateNum)=julmin
            ENDIF
         ENDIF
         Delt=julmin-GateOperatingTime(GateNum)
         IF (Delt .LT. GateLapseTime(GateNum)) THEN
*-----------gradual gate operation
            IF (GatePosition(GateNum) .eq. GATE_OPEN) THEN
*--------------gradual opening
               If (GateLapseTime(GateNum).GT.0)Then
                  GateOperatingCoeff(GateNum)=dfloat(Delt)/(GateLapseTime(GateNum))
               Else
                  GateOperatingCoeff(GateNum)=1.0
               End If
            ELSE IF (GatePosition(GateNum) .eq. GATE_CLOSE) THEN
*--------------gradual closing
               If (GateLapseTime(GateNum).GT.0)Then
                  GateOperatingCoeff(GateNum)=1.-dfloat(Delt)/(GateLapseTime(GateNum))
               Else
                  GateOperatingCoeff(GateNum)=0.0
               End If
            ENDIF
         ELSE                   ! fully open or close
            IF (GatePosition(GateNum) .eq. GATE_OPEN) THEN
               GateOperatingCoeff(GateNum)=1.0
            ELSE IF (GatePosition(GateNum) .eq. GATE_CLOSE) THEN
               GateOperatingCoeff(GateNum)=0.0
            ENDIF
         ENDIF
      ENDDO

      GateSchedule=.TRUE.

      RETURN
      END

*== Public (ChannelVelocity) ===================================

      REAL*8 FUNCTION ChannelVelocity(ChannNum,x)

      IMPLICIT NONE

*   Purpose:  Compute velocity of flow in the channels.

*   Arguments:
      INTEGER ChannNum

*   Argument definitions:
*     ChannNum  - ChannelNumber
*     Extremity - index indicating
*                   [+1] upstream end of current channel.
*                   [-1] downstream end of current channel.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chconnec.inc'
      INCLUDE 'chcxrec1.inc'

      include '../input/fixed/common.f'

*   Local Variables:
      REAL*8    y,x,xx,h,Area,Q
      LOGICAL OK

*   Routines by module:

      INTEGER  NumberOfStreamLocations
      REAL*8     BtmElev,CxArea
      LOGICAL  OpenChannel,CloseChannel
      REAL*8     GlobalStreamSurfaceElevation,GlobalStreamFlow
      INTEGER  StreamEndNode,nodeup,nodedown,closest_node,extchan

      EXTERNAL NumberOfStreamLocations
      EXTERNAL BtmElev,CxArea
      EXTERNAL OpenChannel,CloseChannel
      EXTERNAL GlobalStreamSurfaceElevation,GlobalStreamFlow,StreamEndNode

*   Programmed by: Parviz Nader
*   Date:          March  1993
*   Modified by:
*   Last modified:

*-----Implementation -----------------------------------------------------

      OK = OpenChannel(ChannNum)
      extchan=int2ext(ChannNum)
      nodedown=-StreamEndNode(-ChannNum)
      nodeup=StreamEndNode(ChannNum)
      closest_node=int(dfloat(nodeup)+x/dfloat(chan_geom(extchan).length)*
     &     (dfloat(nodedown)-dfloat(nodeup))+0.5)

      xx=dfloat(closest_node-nodeup)/dfloat(nodedown-nodeup)*
     &     dfloat(chan_geom(extchan).length)

      y=globalStreamSurfaceElevation(closest_node)
      h=y-BtmElev(xx)
      Area=CxArea(xx,h)
      Q=globalStreamFlow(closest_node)
      ChannelVelocity=Q/Area
      OK = CloseChannel()

      RETURN
      END

*== Public (AverageFlow) ===================================

      LOGICAL FUNCTION AverageFlow()

      IMPLICIT NONE

*   Purpose:  To calculate the average flow in the channels and
*             reservoirs, so it can be stored in a hydro file
*             for later use in a transport model.

*   Arguments:

*   Argument definitions:
*     HydroTimeInterval - Time interval for hydro file
*     Nsample   - Number of time steps within a unit time in hydro file

*   Module data:
      INCLUDE '../input/fixed/common.f'
      INCLUDE '../input/time-varying/tide.inc'
      INCLUDE 'network.inc'
      INCLUDE '../input/time-varying/common_tide.f'
      INCLUDE 'chnlcomp.inc'
      INCLUDE 'chconnec.inc'
      INCLUDE 'chstatus.inc'
      INCLUDE 'netcntrl.inc'

*   Local Variables:
      INTEGER i,j,Up, Down
      logical OK
      integer*4
     &     next_hydro_interval
     &     ,incr_intvl          ! increment julian minute by interval function

*   Routines by module:

***** Channel flow status:
      INTEGER  NumberofChannels
      EXTERNAL NumberofChannels

      LOGICAL  CalculateReservoirFlow
      EXTERNAL CalculateReservoirFlow

      LOGICAL  WriteHydroFile
      EXTERNAL WriteHydroFile

      data next_hydro_interval /0/

*   Programmed by: Parviz Nader
*   Date:          October 1994

*-----Implementation -----------------------------------------------------

      AverageFlow = .FALSE.
c-----figure out the first interval to start tidefiles; if the model run
c-----didn't start on an even time boundary, this will be delayed
      if (next_hydro_interval .eq. 0) then
         next_hydro_interval=incr_intvl(start_julmin,
     &        io_files(hydro,io_tide,io_write).interval, NEAREST_BOUNDARY)

         if (next_hydro_interval .ne. start_julmin) then
            next_hydro_interval=next_hydro_interval+HydroTimeInterval
         endif
      endif

      IF (julmin-time_step .ge. next_hydro_interval) THEN
C--------Initialize
         next_hydro_interval=next_hydro_interval+HydroTimeInterval

         DO Branch=1,NumberofChannels()
            QChan(Branch,1)=0.    !QChan is period average, not inst. value
            QChan(Branch,2)=0.
         ENDDO

         DO i=1,Nres
            DO j=1,NconnectReservoir(i)
               QResv(i,j)=0.
            ENDDO
         ENDDO

         DO i=1,max_qext
            qext(i).avg=0.
         ENDDO

         do i=1,nobj2obj
            obj2obj(i).flow_avg=0.
         enddo
      ENDIF

      DO Branch=1,NumberofChannels()
         Up=UpCompPointer(Branch)
         Down=DownCompPointer(Branch)
         QChan(Branch,1)=QChan(Branch,1)+
     &        (theta*Q(Up)+(1.-theta)*QOld(Up))
         QChan(Branch,2)=QChan(Branch,2)+
     &        (theta*Q(Down)+(1.-theta)*QOld(Down))
      ENDDO

      DO i=1,nqext
         qext(i).avg=qext(i).avg+theta*qext(i).flow +
     &        (1.-theta)*qext(i).prev_flow
      ENDDO

      DO i=1,Nres
         DO j=1,NconnectReservoir(i)
            QResv(i,j)=QResv(i,j)+ theta*Qres(i,j)+(1.-theta)*
     &           QresOld(i,j)    !QResv is a time-ave version of QRes used in qual
         ENDDO
      ENDDO

      DO i=1,nobj2obj
         obj2obj(i).flow_avg=obj2obj(i).flow_avg +
     &        theta*obj2obj(i).flow + (1.-theta)*obj2obj(i).prev_flow
      ENDDO

      IF (julmin .ge. next_hydro_interval) THEN
         DO Branch=1,NumberofChannels()
            Up=UpCompPointer(Branch)
            Down=DownCompPointer(Branch)
            QChan(Branch,1)=QChan(Branch,1)/FLOAT(NSample)
            QChan(Branch,2)=QChan(Branch,2)/FLOAT(NSample)
         ENDDO

         DO i=1,nqext
            qext(i).avg=qext(i).avg/FLOAT(NSample)
         ENDDO

         DO i=1,Nres
            DO j=1,NconnectReservoir(i)
               QResv(i,j)=QResv(i,j)/FLOAT(NSample)
            ENDDO
         ENDDO

         do i=1,nobj2obj
            obj2obj(i).flow_avg=obj2obj(i).flow_avg/DFLOAT(NSample)
         enddo

         if(io_files(hydro,io_tide,io_write).use) then
            OK = WriteHydroFile()
         endif
      ENDIF
      AverageFlow = .true.

      Branch=0
      RETURN
      END

*== Public (Calculate_Chan_Net_Flow) ===================================

      LOGICAL FUNCTION Calculate_Chan_Net_Flow()

      IMPLICIT NONE

*   Purpose:  To calculate the average net flow in the channels, to
*             decide if the model has reached dynamic steady state.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE '../input/fixed/common.f'
      INCLUDE 'network.inc'
      INCLUDE '../input/time-varying/common_tide.f'
      INCLUDE 'chnlcomp.inc'
      INCLUDE 'chconnec.inc'
      INCLUDE 'chstatus.inc'
      INCLUDE 'netcntrl.inc'

*   Local Variables:
      INTEGER Up, Down

*   Routines by module:

***** Channel flow status:
      INTEGER  NumberofChannels
      EXTERNAL NumberofChannels

*   Programmed by: Parviz Nader
*   Date:          December 1997

*-----Implementation -----------------------------------------------------

      Calculate_Chan_Net_Flow = .FALSE.
      DO Branch=1,NumberofChannels()
         Up=UpCompPointer(Branch)
         Down=DownCompPointer(Branch)
         QChNet(Branch)=QChNet(Branch)+
     &        (theta*Q(Up)+(1.-theta)*QOLD(Up))-
     &        (theta*Q(Down)+(1.-theta)*QOLD(Down))
      ENDDO
      Calculate_Chan_Net_Flow = .true.

      Branch=0
      RETURN
      END

*== Public (WriteHydroFile) ===================================

      LOGICAL FUNCTION WriteHydroFile()

      IMPLICIT NONE

*   Purpose:  To write information in a hydro file
*             For later use in a transport model

*   Module data:

      INCLUDE 'network.inc'
      INCLUDE 'chconnec.inc'
      INCLUDE 'chnlcomp.inc'
      INCLUDE 'chstatus.inc'
      INCLUDE 'chcxtbl.inc'
      include '../input/fixed/common.f'
      include '../input/time-varying/dss.inc'
      include '../input/time-varying/readdss.inc'
      INCLUDE '../input/time-varying/tide.inc'
      INCLUDE '../input/time-varying/common_tide.f'
*   Local Variables:
      INTEGER i,Up, Down, unit_hydro,nn,lnblnk,j,ptr
      logical first_call        ! first call to routine
      save unit_hydro,first_call
      real*8 xx,zz

*   Routines by module:

***** Channel flow status:
      INTEGER  NumberofChannels
      EXTERNAL NumberofChannels

      REAL*8    CxArea, ChannelWidth, BtmElev
      EXTERNAL CxArea, ChannelWidth, BtmElev


      real*8 ChanLen, delx,zavg

      data first_call /.true./

*   Programmed by: Parviz Nader
*   Date:          October 1994
*   Modified by: Ralph Finch
*   Date: November 1997
*-----Implementation -------------------------------------------------

      WriteHydroFile = .FALSE.

      DO Branch=1,NumberofChannels()
         Up=UpCompPointer(Branch)
         Down=DownCompPointer(Branch)
         i = int2ext(Branch)
         YChan(Branch,1)=WS(Up)-chan_geom(i).bottomelev(1)
         nn= chan_geom(i).nxsect ! last X-Section (i.e. downstream)
         YChan(Branch,2)=WS(Down)-chan_geom(i).bottomelev(nn)
         AChan_Avg(Branch)=0.
         AChan(Branch,1)=CxArea(Dble(0.),Dble(YChan(Branch,1)))
         ChanLen = dfloat(chan_geom(i).length)
         AChan(Branch,2)=CxArea(ChanLen,Dble(YChan(Branch,2)))
         delx=dfloat(chan_geom(i).length)/dfloat(Down-Up)
         DO j=Up, Down-1
            xx=(dfloat(j-Up)+0.5)*delx
            zz=(H(j)+H(j+1))/2.
c            zz=zavg-BtmElev(xx)
            AChan_Avg(Branch)=AChan_Avg(Branch)+CxArea(xx,zz)
         ENDDO
         AChan_Avg(Branch)=AChan_Avg(Branch)/FLOAT(Down-Up)
      ENDDO
      Branch=0
      DO i=1,NRes
         EResv(i)=YRes(i)
      End Do

      IF (first_call) THEN
C--------Open the hydro file and write the headers and the instantaneous
c--------values for the first tide block
         first_call=.false.
         unit_hydro=io_files(hydro,io_tide,io_write).unit
         open(
     &        unit=unit_hydro
     &        ,file=io_files(hydro,io_tide,io_write).filename
     &        ,status='unknown'
     &        ,form='unformatted'
c 
     &        ,convert="big_endian"                           !! <NT> Comment this line for UNIX version of executable
     &        )

c--------header information for checking later in Qual and PTM
         WRITE(unit_hydro)'Hydro Version ' // dsm2_version
         WRITE(unit_hydro)MaxNres,MaxChannels
         WRITE(unit_hydro)Nres,NumberofChannels()
         WRITE(unit_hydro)repeating_tide
c--------minimal geometry info
         WRITE(unit_hydro) int2ext,ext2int
         WRITE(unit_hydro) (chan_geom(i).bottomelev(1),
     &        chan_geom(i).bottomelev(2),i=1,max_channels)
         WRITE(unit_hydro) (node_geom(i).qint,node_geom(i).qext,
     &        i=1,max_nodes)
         WRITE(unit_hydro) (res_geom(i).qint,res_geom(i).qext,
     &        i=1,max_reservoirs)

c--------object-to-object (internal) flow info. Structures must be written 
c        component by component for the PC to be able to write "big-endian"
c        (UNIX-compatible) tidefiles

         WRITE(unit_hydro) nobj2obj
         WRITE(unit_hydro) (
     &          obj2obj(i).obj_name,
     &          obj2obj(i).from.object,
     &          obj2obj(i).from.obj_name,
     &          obj2obj(i).from.object_no,
     &          obj2obj(i).from.hydrochan,
     &          obj2obj(i).from.acct_name,
     &          obj2obj(i).from.acct_ndx,
     &          obj2obj(i).from.mass_frac,
     &          obj2obj(i).from.coeff,
     &          obj2obj(i).to.object,
     &          obj2obj(i).to.obj_name,
     &          obj2obj(i).to.object_no,
     &          obj2obj(i).to.hydrochan,
     &          obj2obj(i).to.acct_name,
     &          obj2obj(i).to.acct_ndx,
     &          obj2obj(i).to.mass_frac,
     &          obj2obj(i).to.coeff,
     &          obj2obj(i).constant_value,
     &          obj2obj(i).label,
     &          obj2obj(i).in_no,
     &          obj2obj(i).flow,
     &          obj2obj(i).prev_flow,
     &          obj2obj(i).flow_avg,
     &          obj2obj(i).constituent_conc,
     &          i=1,max_obj2obj)


c--------external flow info
         write(unit_hydro) nqext
         write(unit_hydro) (
     &          qext(i).flow,
     &          qext(i).prev_flow,
     &          qext(i).avg,
     &          qext(i).prev_avg,
     &          qext(i).in_no,
     &          qext(i).changed_ndx,
     &          qext(i).obj_name,
     &          qext(i).attach.object,
     &          qext(i).attach.obj_name,
     &          qext(i).attach.object_no,
     &          qext(i).acct_name,
     &          qext(i).acct_ndx,
     &          qext(i).mass_frac,
     &          i=1,max_qext)


c--------stage boundaries; write out node number and name
         write(unit_hydro) nstgbnd
         write(unit_hydro) (stgbnd(i).name,stgbnd(i).node,i=1,max_stgbnd)

c--------the start of the first time-varying tideblock
         WRITE(unit_hydro)start_Julmin+HydroTimeInterval ! time stamp is time at end of tideblock
c--------instantaneous values; at start of tide averaged block
         WRITE(unit_hydro)EResv
         WRITE(unit_hydro)YChan
         WRITE(unit_hydro)AChan
         WRITE(unit_hydro)AChan_Avg
c--------initialize previous external average flows so as to force
c--------dump of all external flows first time through
         do i=1,nqext
            qext(i).prev_avg=miss_val_r
         enddo
      ELSE
C--------Write the time-varying results to the hydro file

C--------Figure out how many external flows have changed
C--------Save only the values which have changed

         nqext_changed=0
         DO i=1,nqext
            IF (qext(i).avg .ne. qext(i).prev_avg) then
               nqext_changed=nqext_changed+1
               qext(nqext_changed).changed_ndx=i
            ENDIF
            qext(i).prev_avg=qext(i).avg
         ENDDO
         WRITE(unit_hydro) QResv
         WRITE(unit_hydro) (obj2obj(i).flow_avg,i=1,nobj2obj)
         WRITE(unit_hydro) QChan
         WRITE(unit_hydro) nqext_changed
         IF (nqext_changed .gt. 0) then
c-----------trap flows with no object or type
            do i=1,nqext_changed
               ptr=qext(qext(i).changed_ndx).in_no
               if (pathinput(ptr).object .eq. 0) then
                  write(unit_error,610)
     &                 pathinput(ptr).path(:lnblnk(pathinput(ptr).path)),
     &                 qext(qext(i).changed_ndx).avg
 610              format(/'Warning--no object type specified for external flow:'
     &                 /a
     &                 /'Flow value is ',f8.1)
               endif
               if (pathinput(ptr).acct_ndx .eq. 0) then
                  write(unit_error,620)
     &                 pathinput(ptr).path(:lnblnk(pathinput(ptr).path)),
     &                 qext(qext(i).changed_ndx).avg
 620              format(/'Warning--no flow accounting type specified for external flow:'
     &                 /a
     &                 /'Flow value is ',f8.1)
               endif
            enddo

            WRITE(unit_hydro) (qext(i).changed_ndx,
     &           qext(qext(i).changed_ndx).avg,
     &           i=1,nqext_changed)
         ENDIF

         if (julmin+HydroTimeInterval .le. end_julmin) then
c-----------instantaneous values, start of next tideblock
            WRITE(unit_hydro)Julmin+HydroTimeInterval ! time stamp at end of tideblock
            WRITE(unit_hydro)EResv
            WRITE(unit_hydro)YChan
            WRITE(unit_hydro)AChan
            WRITE(unit_hydro)AChan_Avg
         endif

      ENDIF

      WriteHydroFile = .TRUE.

      RETURN
      END

*== Public (Compute_ChArea) ===================================

      LOGICAL FUNCTION Compute_ChArea()

      IMPLICIT NONE

*   Purpose:  Compute channel area at the two ends of a channel and the average value
*             for all the channels
*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chconnec.inc'
      INCLUDE 'chnlcomp.inc'
      INCLUDE 'chstatus.inc'
      INCLUDE 'chcxtbl.inc'
      include '../input/fixed/common.f'
      include '../input/time-varying/common_tide.f'
*   Local Variables:
      INTEGER i,Up, Down, nn, j
      real*8 xx,zz

*   Routines by module:

***** Channel flow status:
      INTEGER  NumberofChannels
      EXTERNAL NumberofChannels

      REAL*8     CxArea, ChannelWidth, BtmElev
      EXTERNAL CxArea, ChannelWidth, BtmElev

      real*8 delx,zavg

*   Programmed by: Parviz Nader
*   Date:          December 97

*-----Implementation -------------------------------------------------

      Compute_ChArea = .FALSE.

      DO Branch=1,NumberofChannels()
         Up=UpCompPointer(Branch)
         Down=DownCompPointer(Branch)
         i = int2ext(Branch)
         YChan(Branch,1)=WS(Up)-chan_geom(i).bottomelev(1)
         nn= chan_geom(i).nxsect ! last X-Section (i.e. downstream)
         YChan(Branch,2)=WS(Down)-chan_geom(i).bottomelev(nn)
         AChan_Avg(Branch)=0.
         AChan(Branch,1)=CxArea(Dble(0.),Dble(YChan(Branch,1)))
         AChan(Branch,2)=CxArea(dfloat(chan_geom(i).length),Dble(YChan(Branch,2)))
         delx=dfloat(chan_geom(i).length)/dfloat(Down-Up)
         DO j=Up, Down-1
            xx=(dfloat(j-Up)+0.5)*delx
            zavg=(ws(j)+ws(j+1))/2.
            zz=zavg-BtmElev(xx)
            AChan_Avg(Branch)=AChan_Avg(Branch)+CxArea(xx,zz)
         ENDDO
         AChan_Avg(Branch)=AChan_Avg(Branch)/DFLOAT(Down-Up)
      ENDDO
      Branch=0
      Compute_ChArea=.true.
      return
      end
