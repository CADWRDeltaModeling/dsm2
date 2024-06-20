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

!========= BOF reservoirs.f
!
!          routines for retrieving and calculating reservoir flow
!
module reservoirs
    use netcntrl, only: NetworkTimeStep, NetworkTimeIncrement, NetworkTheta, &
        NetworkIteration, &
        VariableStreamDensity, &
        NetworkQuadPts, NetworkQuadPtWt
    use channel_schematic, only: NumberOfChannels,StreamEndNode
    !use chstatus, only: NumberOfChannels, GlobalStreamSurfaceElevation
    use chstatus, only: GlobalStreamSurfaceElevation ! FixMe NumberOfChannels,not found Xiao
    implicit none
contains
    real*8 function get_res_flow(resndx, conn)
        use chconnec
        implicit none

        integer resndx,conn
        get_res_flow = QRes(resndx,conn)
        return
    end function

    real*8 function get_res_surf_elev(resndx)
        use chconnec
        implicit none

        integer resndx
        get_res_surf_elev = YRes(resndx)
        return
    end function

    !== Public (CalculateReservoirFlow) ===================================

    logical function CalculateReservoirFlow()
        use IO_Units
        use grid_data
        use constants
        use klu
        use chconnec
        use solver
        use solveutil
        use chstatus, only: GlobalStreamSurfaceElevation
        use netcntrl, only: NetworkTheta
        use netbnd, only: reservoir_source_sink, reservoir_source_sink_prev
        use reservoir_geometry
        implicit none

        !   Purpose:  Compute flow between reservoirs and channels

        !   Arguments:

        !   Argument definitions:



        !   Local Variables:
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
        real*8 reser_area, reser_vol
        logical OK

        !   Routines by module:
        !   Programmed by: Parviz Nader
        !   Date:          August 1992
        !   Modified by:   Ralph Finch
        !   Last modified: November 1997

        !-----Implementation -----------------------------------------------------

        CalculateReservoirFlow=.false.
        dT=NetworkTimeIncrement()
        Theta = NetworkTheta()
        Iteration = NetworkIteration()

        do i=1,Nreser
            ResIndex = ResEqIndex(i)
            ResRow = ResEqRow(i)            ! The mass balance row

            call calculateReservoirGeometry(i, Yres(i), reser_area, reser_vol)
            !--------The "dV/dt" part of Sum Q = dV/dt
            !---------dVdtknown = (Yres(i)-YResOld(i))*ARes(i)/dt
            dVdtknown = (reser_vol-VResOld(i))/dt
            OK = AddAtRow(ResRow,-dVdtknown)
            !----------OK = AddAtLocation(ResEqPointer(ResIndex),ARes(i)/dt)
            OK = AddAtLocation(ResEqPointer(ResIndex),reser_area/dt)
            ResIndex = ResIndex+1
            !--------Add contributions from external sources to the reservoir mass
            !        balance. These are weighted in time. Note sign convention
            !        of sources and sinks: positive into reservoir.
            KnownResFlow=theta * reservoir_source_sink(i,ALL_FLOWS) + &
                (1.-theta) * reservoir_source_sink_prev(i,ALL_FLOWS)
            OK = AddAtRow(ResRow, KnownResFlow)

            do j=1,res_geom(i)%nConnect
                ChannelConnect=ResConnectingChannels(i,j)
                if(ChannelConnect>0) then
                    NodeContinuityRow=UpConstraintEq(ChannelConnect)
                else
                    NodeContinuityRow=DownConstraintEq(-ChannelConnect)
                end if

                ChanCompPt = IABS(StreamEndNode(ChannelConnect))

                y1=Yres(i)
                y2=GlobalStreamSurfaceElevation(ChanCompPt)

                q1=QRes(i,j)
                dy = y1 - y2
                if (dy >= 0) then
                    coefSqrtTwoG=ReservoirCoeff(i,j,1)
                else
                    coefSqrtTwoG=ReservoirCoeff(i,j,2)
                end if
                if(coefSqrtTwoG == 0) then
                    ResEqResidual=q1
                    dResEqdQ=1.D0
                    dResEqdZres=0.D0
                else
                    ResEqResidual=((abs(q1)*q1)/(coefSqrtTwoG**2) - dy)
                    dResEqdQ=(2.*abs(q1)/(coefSqrtTwoG**2.))
                    dResEqdZres=-1
                end if

                KnownResFlow=theta*q1 + (1.-theta)*QResOld(i,j)

                !-----------Add matrix elements.
                !           The first is the flow contribution to the reservoir continuity equation.
                !           Next are the Zres Q and Zchan part of the reservoir equation.
                !           Finally, the flow term to the node continuity equation is 1.
                if (ForwardElim()) then
                    call add_to_matrix(ResEqPointer(ResIndex),theta)
                    call add_to_matrix(ResEqPointer(ResIndex+1),dResEqdZres)
                    call add_to_matrix(ResEqPointer(ResIndex+2),dResEqdQ)
                    call add_to_matrix(ResEqPointer(ResIndex+3),-dResEqdZres)
                    call add_to_matrix(ResEqPointer(ResIndex+4),1.D0)
                end if
                ResIndex = ResIndex+5
                !-----------Add this res flow to rhs on the row representing the node continuity condition
                OK = AddAtRow(NodeContinuityRow,-1.*q1)
                !-----------Add this res flow to rhs on the row representing reservoir mass balance
                OK = AddAtRow(ResRow,-1.*KnownResFlow)
                !-----------Subtract residual from reservoir equation rhs.
                OK = AddAtRow(ResRow+j,-ResEqResidual)
            enddo

        enddo
        Branch = 0
        CalculateReservoirFlow=.true.
        return
    end function



    !== Public (CalculateReservoirFlow) ===================================

    logical function InitReservoirFlow()
        use IO_Units
        use grid_data
        use chconnec
        use solver
        use netcntrl
        implicit none

        !   Purpose:  Compute flow between reservoirs and channels

        !   Arguments:

        !   Argument definitions:

        !   Local Variables:
        integer ChanCompPt,ChannelConnect
        integer i,j,DF
        parameter(DF = 2)
        real*8 y1,y2,dy

        !   Routines by module:

        !   Programmed by: Parviz Nader
        !   Date:          August 1992
        !   Modified by:   Ralph Finch
        !   Last modified: November 1997

        !-----Implementation -----------------------------------------------------

        InitReservoirFlow=.false.

        do i=1,Nreser

            do j=1,res_geom(i)%nConnect
                ChannelConnect=ResConnectingChannels(i,j)
                ChanCompPt = IABS(StreamEndNode(ChannelConnect))

                y1=Yres(i)
                y2=GlobalStreamSurfaceElevation(ChanCompPt)
                dy = y1 - y2
                if (dy >= 0) then
                    QRes(i,j)=ReservoirCoeff(i,j,1)*sqrt(dy)
                else
                    QRes(i,j)=-ReservoirCoeff(i,j,2)*sqrt(-dy)
                end if
            end do

        end do
        QResOld=QRes
        Branch = 0
        InitReservoirFlow=.true.
        return
    end function

end module
