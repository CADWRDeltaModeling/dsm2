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


*===== BOF chconnec.inc ================================================
*   Version 93.01, January, 1993

*   Note: This include, when used, must appear after "Network.inc".

      REAL*8    Ares,Yres,Dres,TotalResFlow,ReservoirCoeff
      REAL*8    InitialYRes,InitialQRes,AverageResFlow,ReservoirFlow
      REAL*8     QRes, QresOld,Filter,dQResdZres
      INTEGER NRes,NConnectReservoir, ResConnectingChannels
      INTEGER NumConnectReservoir,ListConnectingReservoirs,ListConnectingResJunctions
      INTEGER ReservoirFlag,OldNetworkTimeStep
      INTEGER OldNetworkIteration
      INTEGER ReservoirGate     ! contains gate number if reservoir has operating gate
	  INTEGER TotalNonResRows
	      COMMON /IntReservoirProperties/
     &     NRes,NConnectReservoir(MaxNres),
     &     ResConnectingChannels(MaxNres,MaxResConnectChannel),
     &     NumConnectReservoir(MaxChannels,2),ListConnectingReservoirs(MaxChannels,2,4),
     &     ListConnectingResJunctions(MaxChannels,2,4),
     &     ReservoirFlag(MaxChannels),
     &     OldNetworkTimeStep,OldNetworkIteration,
     &     ReservoirGate(MaxNres,MaxResConnectChannel),TotalNonResRows
       SAVE   /IntReservoirProperties/

	   COMMON /RealReservoirProperties/
     &     Ares(MaxNres),Yres(MaxNres),Filter,
     &     Dres(MaxNres),TotalResFlow(MaxNres),
     &     ReservoirCoeff(MaxNres,MaxResConnectChannel,2),
     &     InitialQRes(MaxNres,MaxResConnectChannel),
     &     InitialYRes(MaxNres),
     &     AverageResFlow(MaxNres,MaxResConnectChannel),
     &     ReservoirFlow(MaxChannels,2),
     &     QRes(MaxNres,MaxResConnectChannel),dQresdZres(MaxNres,MaxResConnectChannel),
     &     QResOld(MaxNres,MaxResConnectChannel)
		SAVE /RealReservoirProperties/

*-----Gate data
      INTEGER  GateChan, GateNumber,GateSpecialPos, 
     &      GateLocation, GatePickChannel,GatePosition,
     &      GateLapseTime,GateOperatingTime,ListGateOperating,
     &      NGate,NumGatesOperating,NumSpecialGates,NumberofPipes,
     &      ListSpecialGates

      REAL*8   DepthFree,DepthInvertPipe,DepthWeirCrest,
     &     PipeRadius,NumberofGatesFraction, 
     &     WidthFree,WidthWeirSea, WidthWeirLand,
     &     GateSeaCoef_Weir,GateLandCoef_Weir,
     &     GateSeaCoef_Pipe,GateLandCoef_Pipe, QGate,
     &     GateOperatingCoeff

      REAL*8  DeltaHOpen,VelocityClose,CurrentDeltaH,
     &     CurrentVelocity,dQdZ1Gate,dQdZ2Gate
  
      COMMON /GateProperties_R/
     &     WidthWeirSea(MaxNGate), WidthWeirLand(MaxNGate),
     &     DepthWeirCrest(MaxNGate),WidthFree(MaxNGate),DepthFree(MaxNGate),
     &     PipeRadius(MaxNGate),DepthInvertPipe(MaxNGate),
     &     GateSeaCoef_Weir(MaxNGate),GateLandCoef_Weir(MaxNGate),
     &     GateSeaCoef_Pipe(MaxNGate),GateLandCoef_Pipe(MaxNGate),
     &     QGate(MaxNGate),
     &     dQdZ1Gate(MaxNGate),dQdZ2Gate(MaxNGate),
     &     CurrentDeltaH(MaxNGate),CurrentVelocity(MaxNGate),
     &     DeltaHOpen(MaxNGate),VelocityClose(MaxNgate)
      COMMON /GateProperties_I/
     &     NumGatesOperating,ListGateOperating(MaxNGate),
     &     NumSpecialGates,ListSpecialGates(MaxNGate),
     &     GateSpecialPos(MaxNGate),GatePosition(MaxNGate),
     &     GateOperatingTime(MaxNGate),GateLapseTime(MaxNgate),
     &     GateOperatingCoeff(MaxNgate),
     &     NumberofGatesFraction(MaxNGate), GateChan(MaxNGate),
     &     NumberofPipes(MaxNGate),GateNumber(MaxChannels,2),
     &     GatePickChannel(MaxChannels,2), GateLocation(MaxNGate),
     &     NGate

      SAVE   /GateProperties_R/,/GateProperties_I/

      REAL*8   QExtBranch, QChNet
      INTEGER HydroTimeInterval,NSample

      COMMON /DerivedFlows_R/
     &     QchNet(MaxChannels),
     &     QExtBranch(MaxChannels)
      COMMON /DerivedFlows_I/ HydroTimeInterval,NSample

      INTEGER UpBoundaryCode(MaxChannels)
      INTEGER UpNumberOfConnections(MaxChannels)
      INTEGER UpConnection(MaxChannels*MaxConnectingChannels)
      INTEGER DownBoundaryCode(MaxChannels)
      INTEGER DownNumberOfConnections(MaxChannels)
      INTEGER DownConnection(MaxChannels*MaxConnectingChannels)
      INTEGER InitialApprox(MaxChannels)
      INTEGER AdditionalVariables(MaxChannels)
      REAL*8   dX(MaxChannels)
      LOGICAL KeepAllCx(MaxChannels)
      COMMON /ChannelConnections_I/
     &     UpBoundaryCode, UpNumberOfConnections,
     &     UpConnection,
     &     DownBoundaryCode, DownNumberOfConnections,
     &     DownConnection,
     &     InitialApprox, AdditionalVariables,
     &     KeepAllCx
	        COMMON /ChannelConnections_R/ dX
      SAVE / ChannelConnections_I /, /ChannelConnections_R/


*   Definitions:
*        for the current channel i ...
*     UpBoundaryCode(i) - upstream boundary-condition code.
*                         (See codes below.)
*     UpNumberOfConnections(i) - number of channels connected to
*                                upstream end.
*     UpConnection((i-1)*J+k) - number of kth channel connected to
*                               upstream end,
*                               where j = MaxConnectingChannels.
*     DownBoundaryCode(i) - downstream boundary-condition code.
*                           (See codes below.)
*     DownNumberOfConnections(i) - number of channels connected to
*                                  downstream end.
*     DownConnection((i-1)*J+k) - number of kth channel connected to
*                                 downstream end,
*                                 where j = MaxConnectingChannels.
*
*     InitialApprox(i) - initial approximation index, indicating
*                        approximation of intial conditions from:
*         [0] user input (schematic data file),
*         [1] normal depth,
*         [2] steady state,
*         [3] normal depth w/adverse slopes removed,
*         [4] maximum ws elevation for branch,
*         [5] model dependent file, or
*         [6] no approximation attempted.
*
*      AdditionalVariables(i) - number of variables in addition to
*                               watersurface elevation and discharge
*                               (not currently used).
*      dX(i) - desired spacing of computational locations.
*      KeepAllCx(i) - index indicating,
*               [.TRUE.] - keep all user-supplied locations as
*                          computational locations.
*               [.FALSE.] - do not keep intermediate user-supplied
*                           locations as computational locations.
*
*     Boundary-condition codes
*     for upstream and downstream ends of channels:
*
*     [ 1] known water-surface elevation.
*     [ 2] known volumetric discharge.
*     [ 4] self-setting (downstream only).
*     [11] water-surface elevation equal to that of connecting channel.
*     [12] sum of discharges equal zero.
*     [31] 3-parameter rating applied to water-surface elevation.
*     [32] 3-parameter rating applied to discharge.
*     [51] user-programmed, applied to water-surface elevation.
*     [52] user-programmed, applied to discharge.

*===== EOF chconnec.inc ================================================
