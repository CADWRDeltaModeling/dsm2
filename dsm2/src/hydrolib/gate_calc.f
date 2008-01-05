C!<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    DSM2 is free software: you can redistribute it and/or modify
C!    it under the terms of the GNU General Public !<license as published by
C!    the Free Software Foundation, either version 3 of the !<license, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public !<license for more details.

C!    You should have received a copy of the GNU General Public !<license
C!    along with DSM2.  If not, see <http://www.gnu.org/!<licenses/>.
C!</license>
c     All variable definitions are in the Gates module

*==   Public (CalculateGateFlow) ==================================

      LOGICAL FUNCTION CalculateGateFlow()
      USE GATES, only: GateArray, Gate,Ngate,GateDevice,ApplyDevicePosition,
     &     WEIR, PIPE
      use IO_Units
      use constants
      use grid_data
      IMPLICIT NONE

*     Purpose:  Compute solution components constraining
*     involving gates. The gate equations are in two parts. First, flow
*     through individual gate devices (gates/weirs) are obtained 
*     from either the orifice equation:
*     
*     Qdev - sign(C A SQRT(2g abs(dh) ), dh) = 0
*             or the transformed form of this equation
*     Qdev*abs(Qdev) - 2g(C A)**2 (dh)

*     where Qdev  = discharge through device
*     C  = Flow coefficient
*     A  = Flow area in the gate
*     g  = acceleration of gravity
*     dh = z1-z2  = head gradient between:
*               z1 = stage "above" the gate in water body
*               z2 = stage at connected node

*     The untransformed form is singular in the neighborhood of dh=0
*     and not very amenable to Newton solvers in this range. 
*     The transformed form is rank deficient in
*     this neighborhood but probably more Newton-solvable
*     To choose the transformed form (default for now), leave the
*     following compiler directive defined.

! Next line is a compile directive
DEC$ DEFINE GATETRANSFORM

*     Channel or reservoir connection flow is the sum of all 
*     the gate device flows:
*     s*Qchan - Qdev1 - Qdev2...=0 
*     The sign parameter s converts the gate flow direction convention to
*     the channel/resrvoir flow direction convention



*     Arguments:

*     Argument definitions:
*     Extremity - index indicating
*     [+1] upstream end of current channel.
*     [-1] downstream end of current channel.

*     Module data:
      include 'network.inc'
      include 'chconnec.inc'
      include 'strmcnst.inc'
      include 'solver.inc'

*     Local Variables:
      Real*8  Theta,OneMTheta
      Real*8  q1,z1,z2,coef1,coef2,coef3,QCalc,RHS
      Real*8  s
      Real*8  CxA
      Real*8  gateEqResid,dGateEqdZ1,dGateEqdZ2
      Real*8  KnownResFlow
      Real*8  QDevice
      Real*8,parameter  :: SINGULAR_FLOW=1.D-1


      Integer Gt, Dv            ! Iterators for gates and devices
      Integer ResRow,gatendx,devndx,extremity,resno
      Logical OK
      Integer NodeContinuityRow,NodeReferenceChannel

      Type(Gate), pointer ::  currentGate
      Type(GateDevice),pointer    ::  currentDevice  

*     Routines by module:

***** Local:

      Real*8, external :: GlobalStreamSurfaceElevation, GlobalStreamFlow
      Integer,external :: StreamEndNode
      Logical,external :: OpenChannel, CloseChannel
      Logical,external :: AddAtRow, StoreAtRow, ForwardElim, AddAtLocation
      Logical, external :: GateFlow
      Real*8, external :: NetworkTheta

      Integer, external :: NetworkIteration

*     Programmed by: Eli Ateljevich based on code by Parviz Nader

*---  Implementation -----------------------------------------------------

      CalculateGateFlow = .FALSE.

      Theta=NetworkTheta()
      OneMTheta=1.D0 - Theta

      DO 100 gt = 1,NGate
         currentGate=>gateArray(gt)
         s=currentGate.flowDirection
         extremity = Int4(-s)	
         gatendx = GateEqIndex(gt)

c--------Get required data concerning gate.
         if (currentGate.objConnectedType .eq. obj_channel) then
            z1 = GlobalStreamSurfaceElevation(currentGate.objCompPoint)
            q1 = GlobalStreamFlow(currentGate.objCompPoint)
            devndx=gatendx+3    ! index in GateEqPointer just before first device	                                 
         elseif (currentGate.objConnectedType .eq. obj_reservoir) then
            z1 = Yres(currentGate.objConnectedID)
            q1=  QRes(currentGate.objConnectedID,currentGate.subLocation)
            devndx=gatendx+5    ! index in GateEqPointer just before first device
         end if

         z2 = GlobalStreamSurfaceElevation(currentGate.nodeCompPoint)
         gateArray(Gt).flow=0.D0
         if (currentGate.free) then
            gateArray(Gt).flow=miss_val_r
c-----------Gate is not operation, flow is free
c-----------node condition reverts to equal water surface

            if (currentGate.objConnectedType .eq. obj_reservoir)then
            ! not allowed for reservoirs (fixme: is there an acceptable behavior?)
               write(unit_error,'(/a/a)') 
     &              'Fully uninstalled gate not allowed for reservoirs',
     &              currentGate.name
               call exit(2)
            end if

            RHS = z2-z1
            OK=AddAtRow(GateEqRow(gt), RHS)
            if (forwardelim()) then
               coef1 = 0.0
               coef2 = 1.0
               coef3 = -1.0
               OK = AddAtLocation(GateEqPointer(gatendx),coef1)
               OK = AddAtLocation(GateEqPointer(gatendx+1),coef2)
               OK = AddAtLocation(GateEqPointer(gatendx+2),coef3)
            end if
            ! For each device, put in a trivial equation specifying that the device
            ! flow is zero. These equations are not meaningful, but must be included
            ! so that the matrix remains the same size.
            do dv = 1,currentGate.nDevice
	         currentDevice=>currentGate.Devices(dv)
               QDevice=currentDevice.flow
               GateEqResid=QDevice
               coef1=1.D0
               coef2=0.D0
               coef3=0.D0

               OK=AddAtRow(currentDevice.calcRow, -GateEqResid)
               if (forwardelim())then !Insert the matrix coefficients
                  OK = AddAtLocation(GateEqPointer(devndx+1),coef1) ! Gate Equation for device
                  OK = AddAtLocation(GateEqPointer(devndx+2),coef2)
                  OK = AddAtLocation(GateEqPointer(devndx+3),coef3)
                  devndx=devndx+4
               end if
            end do            
         else    ! Not gate free. Gate is in operation
            
c     Coeficient for the channel or reservoir flow in the equation
c     that expresses channel flow as the sum of all the gate device flows:
c     s*Qchan - Qdev1 - Qdev2...=0 
c     The sign parameter s converts the gate flow direction convention to
c     the channel/resrvoir flow direction convention

            OK = AddAtRow(GateEqRow(gt),-s*q1)
            if (forwardelim())then
               OK = AddAtLocation(GateEqPointer(gatendx),s)
            end if

c---------------Calculate flow and derivatives for each device in this gate
            do dv = 1,currentGate.nDevice
	         currentDevice=>currentGate.devices(dv)
	         call ApplyDevicePosition(currentDevice)
	         if (currentDevice.structureType .eq. PIPE)then
                 call calcPipeFlow(currentDevice,z1,z2,qCalc,dGateEqdZ1,dGateEqdZ2,CxA)
               else 
	           call calcWeirFlow(currentDevice,z1,z2,qCalc,dGateEqdZ1,dGateEqdZ2,CxA)
               end if
			 QDevice=currentDevice.flow 
               if (CxA .gt. 0 .and. abs(QDevice) .lt. SINGULAR_FLOW) then
                  currentDevice.flow = sign(SINGULAR_FLOW,z1-z2)
                  QDevice=currentDevice.flow
	         end if
               if (CxA .gt. 0.)then
                  GateEqResid=QDevice-qcalc
		        coef1=1.D0
! Next line is a compile directive
!DEC$ IF DEFINED(GATETRANSFORM)
                  GateEqResid= abs(QDevice)*QDevice-abs(qCalc)*qCalc
                  coef1=2.D0*abs(QDevice)
!DEC$ END IF

                  coef2=dGateEqdZ1
                  coef3=dGateEqdZ2
               else
                  GateEqResid=QDevice
                  coef1=1.D0
                  coef2=0.0
                  coef3=0.0
               end if
               ! Known part of device contribution to total gate flow and 
               ! the residual of the device equation.
               OK=AddAtRow(GateEqRow(gt), QDevice) 
               OK=AddAtRow(currentDevice.calcRow, -GateEqResid)
               if (forwardelim())then !Insert the matrix coefficients
                                ! contribution of device to total chan/res flow through gate
                  OK = AddAtLocation(GateEqPointer(devndx),-1.D0)  
                  OK = AddAtLocation(GateEqPointer(devndx+1),coef1) ! Gate Equation for device
                  OK = AddAtLocation(GateEqPointer(devndx+2),coef2)
                  OK = AddAtLocation(GateEqPointer(devndx+3),coef3)
                  devndx=devndx+4
               end if
	         currentGate.flow = currentGate.flow+QDevice
            end do            
         end if


c-----------If gate is at a reservoir, must also account for gate flow in the
c-----------reservoir mass balance and also must put the gate flow into QRes
         if (currentGate.objConnectedType .eq. obj_reservoir) then
                                ! if reservoir gate, the gate row is
            resno=currentGate.objConnectedID
            NodeReferenceChannel=node_geom(currentGate.node).sumQChan
            if (NodeReferenceChannel .lt. 0) then !!!!!!!????????
               NodeContinuityRow=DownConstraintEq(-NodeReferenceChannel)    
            else
               NodeContinuityRow=UpConstraintEq(NodeReferenceChannel)
            end if
            ResRow = ResEqRow(resno)
            OK=AddAtRow(NodeContinuityRow, -q1) ! the node sum of flow	                
            KnownResFlow = theta*q1 
     &           +OneMTheta*QResOld(currentGate.objConnectedID,
     &           currentGate.subLocation)
            OK = AddAtRow( ResRow, -KnownResFlow) 
            if (forwardelim())then
               OK = AddAtLocation(GateEqPointer(gatendx+3),1.D0)
               OK = AddAtLocation(GateEqPointer(gatendx+4),theta)
            end if              ! Reservoir
         end if                 ! Gate is in operation
c	   print*, gateArray(gt).name," flow: ",gateArray(gt).flow," Object flow: ",q1
c         gateArray(gt).flow=q1

 100  enddo                     ! Gate Loop       

      CalculateGateFlow = .TRUE.

      return
      end function

*==   Public (calcPipeFlow) ===================================

      subroutine calcPipeFlow(pipe,z1,z2,qPipe,dGateEqdZ1,dGateEqdZ2,CxA)
      use Gates, only: GateDevice
	use PhysicalConstants, only: pi, sqrt2g
      implicit none

*     Purpose: This function calculates flow/parameters through circular pipes 
*     based on upstream (z1) and dowstream (z2) water
*     surface elevation.

*     Arguments:
      REAL*8    z1,z2

*     Argument definitions:
*     pipe  -  Pipe being calculated
*     z1  -  Water surface elevation in channel
*     z2  -  Water surface elevation at node
*     QPipe  -  Flow through the gate (out)
*     dQdZ1 - Derivative of flow with respect to stage at water body (chan,res)
*     dQdZ2 - derivative of flow with respect to node reference stage

*     Module data:
      include 'network.inc'     !   available through Gates module
      include 'chconnec.inc'

***** Network control:

*     Local Variables:
      REAL*8 coefPipe,coefOp,value
	REAL*8 dGateEqdZ1,dGateEqdZ2
      REAL*8 zinvert, r, flowDepth, angle
	REAL*8 dh, absdh
	REAL*8 dHeadDiffdZ1, dHeadDiffdZ2
	REAL*8 dAbsHeadDiffdZ1,dAbsHeadDiffdZ2
      REAL*8     FlowToNode
      REAL*8     FlowArea
      REAL*8     TopWidth
      REAL*8     Qpipe
      REAL*8     m1,m2          ! indicator variables to indicate whether
                                ! stage is higher in the water body (m1=1,m2=0) or
                                ! node (m1=0, m2=1)

      REAL*8     CxA,CxT1,CxT2
      Type (GateDevice) :: pipe

*-----Implementation -----------------------------------------------------

*-----Determine if flow is to or from node.

*-----circular pipe (orifice) gate
      zinvert=pipe.baseElev
      r=pipe.maxWidth

      if (z1.GE.z2 ) then       ! flow from water body to node
         coefPipe=pipe.flowCoefToNode
         flowToNode=1.
	   coefOp=pipe.opCoefToNode
         if (z1.ge. zinvert) then
            flowDepth=z1-zinvert
            m1=1.               !  weight for contribution of z1 to flow area
            m2=0.               !  weight for contribution of z2 to flow area
            dHeadDiffdZ1=1.      !  contribution of z1 to head
            if (z2 .gt. zinvert) then ! submerged, z2 is used for head difference
               dh=z1-z2
               dHeadDiffdZ2=-1.
            else
               dh = z1 - zinvert
               dHeadDiffdZ2=0.
            end if
         else
            dh=0.
            flowDepth=0.
            m1 = 0.
            m2 = 0.
            dHeadDiffdZ1 = 0.
            dHeadDiffdZ2 = 0.
         end if
	   absdh=dh
	   dAbsHeadDiffdZ1=dHeadDiffdZ1
	   dAbsHeadDiffdZ2=dHeadDiffdZ2
      else if (z1 .lt. z2) then !  flow from node to water body
         coefPipe=pipe.flowCoefFromNode
         flowToNode = -1.
	   coefOp=pipe.opCoefFromNode
         if (z2 .ge. zinvert) then
            m1=0.
            m2=1.
            dHeadDiffdZ2=-1.
            flowDepth=z2-zinvert
            if (z1 .gt. zinvert) then
               dh = z1-z2
               dHeadDiffdZ1 = 1
            else
               dh = zinvert-z2
               dHeadDiffdZ1=0.
            end if
         else                   ! dry weir
            m1 = 0.
            m2 = 0.
            dh = 0.
            flowDepth = 0.
            dHeadDiffdZ1 = 0.
            dHeadDiffdZ2 = 0.
         end if
	   absdh=-dh
	   dAbsHeadDiffdZ1=-dHeadDiffdZ1
	   dAbsHeadDiffdZ2=-dHeadDiffdZ2

      end if

***   ---Adjust due to gate scheduling and number of duplicate gates
      coefPipe=coefPipe*pipe.nDuplicate*coefOp*sqrt2g

c-----adjust for number of gates open out of total
      FlowArea=0.
      TopWidth=0.
      QPipe=0.

      if (flowDepth.GT.0.AND.flowDepth.LT.2.*r) then
*     partial flow
         angle=ACOS(1.-flowDepth/r) 
         FlowArea=r**2*angle-r*(r-flowDepth)*SIN(angle)
         TopWidth=2.*r*SIN(angle)
      elseif (flowDepth.GT.2.*r) then
*     full flow
         FlowArea=r**2*pi
         TopWidth=0.
      else
         FlowArea=0.
         TopWidth=0.
      end if
      

*-----Calculate overall Coefficient * Area term and derivative plus pipe flow
      CxA=coefPipe*FlowArea
      CxT1=coefPipe*TopWidth
      CxT2=CxT1*m2
      CxT1=CxT1*m1

*-----The value calculated below  is oriented from water body to node

      QPipe=flowToNode*CxA*sqrt(absdh)
      
c-----Next line is a compiler directive
!DEC$ IF DEFINED(GATETRANSFORM)
c      GateEqResid= abs(QDevice)*QDevice-CxA**2.*dh
      value=0. ! prevents "variable unused" compiler warning
      dGateEqdZ1=-2.*CxA*CxT1*dh-CxA**2.*dHeadDiffdZ1
      dGateEqdZ2=-2.*CxA*CxT2*dh-CxA**2.*dHeadDiffdZ2

!DEC$ ELSE
      value = .5/sqrt(max(absdh,1.D-12)) ! a large number
	! minus sign is because gate equation is Q - Qcalc and this is the Qcalc part
      dGateEqdZ1 = -flowToNode*(CxA*value*dAbsHeadDiffdZ1+CxT1*sqrt(absdh)) !sign(,dh)
      dGateEqdZ2 = -flowToNode*(CxA*value*dAbsHeadDiffdZ2+CxT2*sqrt(absdh)) !sign(sqrt(absdh),dh)
!DEC$ ENDIF

      RETURN
      END SUBROUTINE

*==   Public (calcWeirFlow) ===================================

      Subroutine calcWeirFlow(weir,z1,z2,qWeir,dGateEqdZ1,dGateEqdZ2,CxA)
      use Gates, only: GateDevice
	use PhysicalConstants, only:sqrt2g
      IMPLICIT NONE

*     Purpose: This function calculates flow through circular (pipe) and weir
*     gates based on upstream (y1) and dowstream (y2) water
*     surface elevation.

*     Arguments:
      REAL*8    z1,z2,dGateEqdZ1,dGateEqdZ2

*     Argument definitions:
*     z1  -  Water surface elevation in channel
*     z2  -  Water surface elevation at node
*     qWeir  -  Flow through the weir
*     dQdZ1 - Derivative of flow with respect to stage at water body (chan,res)
*     dQdZ2 - derivative of flow with respect to node reference stage

*     FlowArea -  FlowArea through the gate
*     GateTopWidth-  Top width of the flow in the gate

*     Module data:
      include 'network.inc'     !available through gates module
      include 'chconnec.inc'

***** Network control:

*     Local Variables:
      REAL*8 coefWeir,coefOp
      REAL*8 d, dh, absdh, flowDepth
      REAL*8 dHeadDiffdZ1,dHeadDiffdZ2,dAbsHeadDiffdZ1,dAbsHeadDiffdZ2,value
      REAL*8 FlowToNode
      REAL*8 TopWidth
      REAL*8 QWeir
      REAL*8 CxA,CxT1,CxT2
      REAL*8 m1,m2              ! indicator variables to indicate whether
                                ! stage is higher in the water body (m1=1,m2=0) or
                                ! node (m1=0, m2=1) or both below datum (m1=m2=0)


      Type(GateDevice) :: weir

*     Programmed by: Eli Ateljevich
*     Date:          August 2001

*-----Implementation -----------------------------------------------------

      d=weir.baseElev

      if (z1.GE.z2 ) then       ! flow from water body to node
         coefWeir=weir.flowCoefToNode
         topWidth=weir.maxWidth
         coefOp=weir.opCoefToNode
	   flowToNode=1.
         if (z1.ge. d) then	       
            flowDepth=z1-d
            m1=1.               !  weight for contribution of z1 to flow area
            m2=0.               !  weight for contribution of z2 to flow area
            dHeadDiffdZ1 =  1.      !  contribution of z1 to head
            if (z2 .gt. d) then ! submerged, z2 is used for head difference
	         dh=z1-z2
               dHeadDiffdZ2=-1.
            else                ! free flow, weir crest used for head, not z2
	         dh=z1-d
               dHeadDiffdZ2=0.
            end if
         else                   !dry
            dh=0.
            flowDepth=0.
            m1 = 0.
            m2 = 0.
            dHeadDiffdZ1 = 0.
            dHeadDiffdZ2 = 0.
         end if
	   absdh=dh
	   dAbsHeadDiffdZ1=dHeadDiffdZ1
	   dAbsHeadDiffdZ2=dHeadDiffdZ2
      else if (z1 .lt. z2)  then !  flow from node to water body
         coefWeir=weir.flowCoefFromNode
         TopWidth=weir.maxWidth
         flowToNode = -1.
         coefOp=weir.opCoefFromNode
         if (z2 .ge. d) then
            m1=0.
            m2=1.
            dHeadDiffdZ2 = -1.
            flowDepth=z2-d
            if (z1 .gt. d) then
               dh = z1-z2
               dHeadDiffdZ1 = 1.
            else
               dh = d-z2
               dHeadDiffdZ1 = 0.
            end if
         else                   ! dry weir
            m1 = 0.
            m2 = 0.
            dh = 0.
            flowDepth = 0.
            dHeadDiffdZ1 = 0.
            dHeadDiffdZ2 = 0.
         end if
	   absdh=-dh
	   dAbsHeadDiffdZ1=-dHeadDiffdZ1
	   dAbsHeadDiffdZ2=-dHeadDiffdZ2

      end if

*-----Adjust due to gate scheduling and number of duplicate devices

      coefWeir=coefWeir*weir.nduplicate*coefOp*sqrt2g

*-----The value calculated below  is oriented from water body to node

      if (flowDepth .lt. weir.height)then  ! Area depends on free surface 
         CxT1=coefWeir*TopWidth
         CxA=CxT1*flowDepth
         CxT2=CxT1*m2
         CxT1=CxT1*m1
      else                              ! Area limited by height of gate apperature
         CxT1=0.
	   CxT2=0.
	   CxA=weir.height*coefWeir
	end if

*-----The value calculated below  is oriented from water body to node
* 
      QWeir=  flowToNode*CxA*sqrt(absdh) ! directed to node   
!DEC$ IF DEFINED (GATETRANSFORM)
c      GateEqResid= abs(QDevice)*QDevice-CxA**2.*dh
      value=0. ! prevents "variable unused" compiler warning
      dGateEqdZ1=-2.*CxA*CxT1*dh-CxA**2.*dHeadDiffdZ1
      dGateEqdZ2=-2.*CxA*CxT2*dh-CxA**2.*dHeadDiffdZ2
!DEC$ ELSE      
      value = .5/sqrt(max(absdh,1.D-12)) ! a large number
      dGateEqdZ1 = -flowToNode*(CxA*value*dAbsHeadDiffdZ1+CxT1*sqrt(absdh)) !sign(sqrt(absdh),dh)
      dGateEqdZ2 = -flowToNode*(CxA*value*dAbsHeadDiffdZ2+CxT2*sqrt(absdh)) !sign(sqrt(absdh),dh)
!DEC$ END IF

      RETURN 
      END SUBROUTINE


*=======subroutine AssignGateCompPoints
      
      subroutine AssignGateCompPoints()

*     Purpose: Finds the grid computation points of the water body
*     and node attached to the gates, and enters this info
*     in the gate structure for later use in gate computations

      use Gates, only: gateArray,Gate,NGate
      use grid_data
      use constants
      implicit none

      include 'network.inc'
      include 'chconnec.inc'
      integer gt
      type(Gate),pointer :: currentGate
      integer extremity
      real*8 s
      
      integer, external :: StreamEndNode

      do gt = 1,NGate
         currentGate=>gateArray(gt)
         s=currentGate.flowDirection
         extremity = Int4(-s)	
         if (currentGate.objConnectedType .eq. obj_channel) then
            currentGate.objCompPoint=
     &           iabs(StreamEndNode( extremity*currentGate.objConnectedID))
         elseif (currentGate.objConnectedType .eq. obj_reservoir) then
                                ! q1=  QRes(currentGate.objConnectedID,currentGate.subLocation)
         end if
         currentGate.nodeCompPoint=IABS(StreamEndNode(
     &        node_geom(currentGate.node).sumQChan ))       
      end do

      return 
      end subroutine
