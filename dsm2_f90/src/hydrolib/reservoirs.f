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
C!

*========= BOF reservoirs.f
c
c          routines for retrieving and calculating reservoir flow 
c    

      real*8 function get_res_flow(resndx, conn)
      implicit none
	include 'network.inc'
      include 'chconnec.inc'
	integer resndx,conn
	get_res_flow = QRes(resndx,conn)
      return
	end function

      real*8 function get_res_surf_elev(resndx)
      implicit none
	include 'network.inc'
      include 'chconnec.inc'
	integer resndx
	get_res_surf_elev = YRes(resndx)
      return
	end function

*== Public (CalculateReservoirFlow) ===================================

      logical function CalculateReservoirFlow()
      use IO_Units
      use grid_data
      use constants
      implicit none

*   Purpose:  Compute flow between reservoirs and channels

*   Arguments:

*   Argument definitions:

*   Module data:
      include 'network.inc'
      include 'chconnec.inc'
      include 'solver.inc'

*   Local Variables:
      integer ChanCompPt,ChannelConnect
      integer ResIndex,ResRow,NodeContinuityRow
      integer i,j,DF
      parameter(DF = 2)
      integer Iteration
      real*8 Theta
      real*8 dVdtKnown
      real*8 knownresflow
	real*8 ResEqResidual,dResEqdZres
	real*8 dResEqdQ,coefSqrtTwoG

      real*8 y1,y2,dy,q1,dT
      logical OK

*   Routines by module:

      integer  NetworkTimeStep, NetworkTimeIncrement,
     &     NetworkIteration,
     &     NumberOfChannels,StreamEndNode

      external NetworkTimeStep, NetworkTimeIncrement,
     &     NetworkIteration,
     &     NumberOfChannels,StreamEndNode

      real*8     GlobalStreamSurfaceElevation,NetworkTheta
      external GlobalStreamSurfaceElevation,NetworkTheta

      real*8 reservoir_source_sink,reservoir_source_sink_prev
      external reservoir_source_sink,reservoir_source_sink_prev

      logical AddAtLocation,StoreAtRow,AddAtRow,ForwardElim
      External AddAtLocation,StoreAtRow,AddAtRow,ForwardElim



*   Programmed by: Parviz Nader
*   Date:          August 1992
*   Modified by:   Ralph Finch
*   Last modified: November 1997

*-----Implementation -----------------------------------------------------

      CalculateReservoirFlow=.FALSE.
      dT=NetworkTimeIncrement()
      Theta = NetworkTheta()
      Iteration = NetworkIteration()

      DO i=1,Nreser
         ResIndex = ResEqIndex(i)
         ResRow = ResEqRow(i)            ! The mass balance row


*--------The "dV/dt" part of Sum Q = dV/dt
         dVdtknown = (Yres(i)-YResOld(i))*ARes(i)/dt
         OK = AddAtRow(ResRow,-dVdtknown)
         OK = AddAtLocation(ResEqPointer(ResIndex),ARes(i)/dt)
         ResIndex = ResIndex+1
*--------Add contributions from external sources to the reservoir mass
*        balance. These are weighted in time. Note sign convention 
*        of sources and sinks: positive into reservoir.
         KnownResFlow=theta * reservoir_source_sink(i,ALL_FLOWS) +
     &        (1.-theta) * reservoir_source_sink_prev(i,ALL_FLOWS)
         OK = AddAtRow(ResRow, KnownResFlow)

         DO j=1,res_geom(i).nConnect
            ChannelConnect=ResConnectingChannels(i,j)
            if(ChannelConnect.GT.0)then
               NodeContinuityRow=UpConstraintEq(ChannelConnect)
            else
               NodeContinuityRow=DownConstraintEq(-ChannelConnect)
            end if

            ChanCompPt = IABS(StreamEndNode(ChannelConnect))

            y1=Yres(i)
            y2=GlobalStreamSurfaceElevation(ChanCompPt)

            q1=QRes(i,j)
            dy = y1 - y2
            if (dy .ge. 0)then
	         coefSqrtTwoG=ReservoirCoeff(i,j,1)
	      else
	         coefSqrtTwoG=ReservoirCoeff(i,j,2)
	      end if
	      if(coefSqrtTwoG .eq. 0)then
	         ResEqResidual=q1
	         dResEqdQ=1.D0
	         dResEqdZres=0.D0
	      else
              ResEqResidual=((abs(q1)*q1)/(coefSqrtTwoG**2) - dy)
 		     dResEqdQ=(2.*abs(q1)/(coefSqrtTwoG**2.))
              dResEqdZres=-1
            end if

c-----------Save for diagnostics
c          if(dResEqdQ .le. 1.D-6)dResEqdQ=1.D-6 ! protect against bad scaling
c	      if (i.eq.4)write (unit_screen,"('Iter: ',i5,' Res: ',a15,
c     &         1x,2i4,' dy,y1,y2: ',3f8.5,/,f13.2,f10.2,f15.6)")NetworkIteration(),
c     &         res_geom(i).name,i,j,dy, y1, y2, coefSqrtTwoG, QRes(i,j), ResEqResidual


            KnownResFlow=theta*q1 + (1.-theta)*QResOld(i,j)

*-----------Add matrix elements.
*           The first is the flow contribution to the reservoir continuity equation.
*           Next are the Zres Q and Zchan part of the reservoir equation. 
*           Finally, the flow term to the node continuity equation is 1.
            If (ForwardElim())Then
               call sfAdd5Reservoir(ResEqPointer(ResIndex),theta,
     &            dResEqdZres,dResEqdQ,-dResEqdZres,1.D0)
            End If
            ResIndex = ResIndex+5
*-----------Add this res flow to rhs on the row representing the node continuity condition
            OK = AddAtRow(NodeContinuityRow,-1.*q1)
*-----------Add this res flow to rhs on the row representing reservoir mass balance
            OK = AddAtRow(ResRow,-1.*KnownResFlow)
*-----------Subtract residual from reservoir equation rhs.
	      OK = AddAtRow(ResRow+j,-ResEqResidual)
         ENDDO

      ENDDO
      Branch = 0
      CalculateReservoirFlow=.TRUE.
      RETURN
      END



*== Public (CalculateReservoirFlow) ===================================

      logical FUNCTION InitReservoirFlow()
      use IO_Units
      use grid_data
      implicit none

*   Purpose:  Compute flow between reservoirs and channels

*   Arguments:

*   Argument definitions:

*   Module data:
      include 'network.inc'
      include 'chconnec.inc'
      include 'solver.inc'


*   Local Variables:
      integer ChanCompPt,ChannelConnect
      integer i,j,DF
      parameter(DF = 2)
      real*8 y1,y2,dy

*   Routines by module:

      integer  NetworkTimeStep, NetworkTimeIncrement,
     &     NetworkIteration,
     &     NumberOfChannels,StreamEndNode

      external NetworkTimeStep, NetworkTimeIncrement,
     &     NetworkIteration,
     &     NumberOfChannels,StreamEndNode

      real*8     GlobalStreamSurfaceElevation
      external GlobalStreamSurfaceElevation


*   Programmed by: Parviz Nader
*   Date:          August 1992
*   Modified by:   Ralph Finch
*   Last modified: November 1997

*-----Implementation -----------------------------------------------------

      InitReservoirFlow=.FALSE.

      do i=1,Nreser
 
         do j=1,res_geom(i).nConnect
            ChannelConnect=ResConnectingChannels(i,j)
            ChanCompPt = IABS(StreamEndNode(ChannelConnect))

            y1=Yres(i)
            y2=GlobalStreamSurfaceElevation(ChanCompPt)
            dy = y1 - y2
            if (dy .ge. 0)then
	         QRes(i,j)=ReservoirCoeff(i,j,1)*sqrt(dy)
	      else
	         QRes(i,j)=-ReservoirCoeff(i,j,2)*sqrt(-dy)
	      end if
         end do

      end do
	QResOld=QRes
      Branch = 0
      InitReservoirFlow=.TRUE.
      return
      end




