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

!==== BOF floweq1d =====================================================
module floweq1d_ds
    use chstatus, only: WriteNetworkRestartFile,StreamFlow, StreamDepth,  &
        StreamSurfaceElevation, &
        OldStreamDensity, NewStreamDensity
    use channel_xsect_tbl, only: ChannelWidth, CxArea, Beta, Conveyance, dConveyance, &
        CxCentroid, dCxCentroid, &
        AreaWtSinuosity,  QWtSinuosity, &
        dAreaWtSinuosity, dQWtSinuosity
    use channel_schematic, only: StreamDistance
    use netcntrl, only:  NetworkQuadPts, NetworkQuadPtWt,  &
        NetworkIteration, NetworkTheta, NetworkTimeIncrement

    use solveutil, only: StoreDynmKnown, StoreDynmAdjust,  &
        StoreMassKnown, StoreMassAdjust, &
        ForwardElim

    implicit none

contains


!Referece for density calculation:
    !Equation 9 from https://www.jodc.go.jp/info/ioc_doc/UNESCO_tech/046148eb.pdf
    subroutine ec_to_density(ec, density)
        implicit none
        real*8, intent(in) :: ec
        real*8, intent(out) :: density
        !local variables
        real*8 :: K0, K1, K2, K3, K4, K5
        real*8 :: b0, b1, b2, b3, b4
        real*8 :: c0, c1, c2
        real*8 :: d0
        real*8 :: temp  !reference temperature for density calculation, deg C
        real*8 :: pure_water_rho
        real*8 :: ec_sea
        real*8 :: ec_to_PSU
        real*8 :: R

        K0 = 0.012
        K1 = -0.2174
        K2 = 25.3283
        K3 = 13.7714
        K4 = -6.4788
        K5 = 2.5842
        temp = 25.0
        ec_sea = 53087.0
        R = ec/ec_sea
        ec_to_PSU = K0 + K1*R**0.5 + K2*R + K3*R**1.5 + K4*R**2 + K5*R**2.5

        pure_water_rho = 1000.0
        b0 = 8.224493e-1
        b1 = -4.0899e-3
        b2 = 7.6438e-5
        b3 = -8.2467e-7
        b4 = 5.3875e-9
        c0 = -5.72466e-3
        c1 = 1.0227e-4
        c2 = -1.6546e-6
        d0 = 4.8314e-4

        density = (pure_water_rho + (b0 + b1*temp + b2*temp**2 + b3*temp**3 + b4*temp**4)*ec_to_PSU + &
            (c0 + c1*temp + c2*temp**2)*(ec_to_PSU)**(1.5) + &
            d0*(ec_to_PSU)**2)/1000.0

        return
    end subroutine


    subroutine get_density(chan, dist, density)
        use runtime_data, only: prev_julmin
        use, intrinsic :: ieee_arithmetic
        use netcntrl_common, only: VariableDensity
        use gtm_vars, only: gtm_start_jmin
        use gtm_subs, only: chan_ec_val

        implicit none
        integer, intent(in) :: chan !channel number
        real*8, intent(inout) :: dist !distance along channel
        real*8, intent(out) :: density
        !local variables
        real*8 :: ec

        if (prev_julmin .le. gtm_start_jmin) then ! before GTM start time
            density = 1.0
        else
            if (.not. VariableDensity) then
                density = 1.0
            else
                !get EC from GTM
                ec = chan_ec_val(chan, dist)
                call ec_to_density(ec, density)
                if (density .gt. 1.25) then
                    density = 1.0 
                    print *, 'Warning: computed density greater than 1.25 g/cm3, reset to 1.0 g/cm3'
                end if

                if (density .lt. 1.0) then 
                    density = 1.0
                    print *, 'Warning: computed density less than 1.0 g/cm3, reset to 1.0 g/cm3'
                end if

                if (ieee_is_nan(density)) then
                    density = 1.0
                    print*, 'Channel number:', chan, ' distance:', dist, 'ec=:', ec
                    print *, 'Warning: computed density is NaN, reset to 1.0 g/cm3'
                end if
            end if

        end if

    end subroutine


    !== Public (DynamicWaveEqDS) =============================================

    logical function DynamicWaveEqDS(Up, Down)
        use PhysicalConstants,only: gravity
        use solver
        use klu
        use channel_schematic, only: CurrentChannel
        implicit none

        !   Purpose:  Compute and store coefficients of an integral form
        !             of the dynamic-wave equation (for computing
        !             1-D unsteady flow in open channels).

        !   Program notes:
        !     Variables are integrated over time using an average weighted
        !     by Theta.  Equal weight would be given to new and old time steps
        !     when Theta = 0.5.  A Newton-Raphson technique is used in the
        !     computation of equation coefficients with the exception of the
        !     momentum coefficient Beta which is estimated by successive
        !     substitution.  Integration in space is accomplished by numerical
        !     quadrature.  Maximum number of quadrature points is currently
        !     set by the parameter MaxQuadPts.  Water density is allowed to vary
        !     with time and distance.  Channel sinuosity is allowed to vary
        !     with depth of flow and distance.

        !     This routine allows solution with constant coefficient matrix
        !     when Forward is .FALSE..

        !   Arguments:
        integer Up, Down

        !   Argument definitions:
        !     Up - upstream location number.
        !     Down - downstream location number.

        !   Local Variables:

        integer J, J2, K, II
        integer Nodes, Iteration, QuadPts
        parameter (Nodes = 2)
        real*8 G
        real*8 N(Nodes), DNDX(Nodes)
        real*8 QuadWt, QuadPt
        real*8 X1, X2, Q1, Q2, Z1, Z2, H1, H2
        real*8 Width1, Width2, Area1, Area2
        real*8 Velocity1, Velocity2
        real*8 BetaVelocity1, BetaVelocity2
        real*8 OldRho1, NewRho1, OldRho2, NewRho2, OldRho, NewRho
        real*8 OlddRhodX, NewdRhodX
        real*8 ZBar, dZBardX
        real*8 Coef(8)
        real*8 DX, DT, Theta, OneMinusTheta
        real*8 CoefMassKnown, CoefMassAdjust
        real*8 CoefDynmKnown, CoefDynmAdjust
        real*8 Xdist, Q, H, Z, DZDX, dist1, dist2
        real*8 Ma, dMadH, Mq, dMqdH
        real*8 Width, Area, Conv, ConvSq
        real*8 DXxDT, AbsQxAreaByConvSq, AbsQxQbyConvSq, DConvDZ
        logical OK, Forward

        !   Routines by module:


        !   Intrinsics:
        real*8      ABS
        intrinsic ABS

        !   Programmed by: Lew DeLong
        !   Date:          February 1991
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        Iteration = NetworkIteration()

        G = gravity

        Forward = ForwardElim()

        !-----Current flow variables.

        !          upstream face
        Q1 = StreamFlow(Up)
        Z1 = StreamSurfaceElevation(Up)
        H1 = StreamDepth(Up)
        dist1 = StreamDistance(Up) - StreamDistance(1)
        call get_density(CurrentChannel(), dist1, NewRho1)
        ! NewRho1 = 1.0

        !          downstream face
        Q2 = StreamFlow(Down)
        Z2 = StreamSurfaceElevation(Down)
        H2 = StreamDepth(Down)
        dist2 = StreamDistance(Down) - StreamDistance(1)
        call get_density(CurrentChannel(), dist2, NewRho2)
        ! NewRho2 = 1.0

        !-----Current channel properties.

        !          upstream face
        X1 = StreamDistance(Up)
        Width1 = ChannelWidth(X1,Z1)
        Area1 = CxArea(X1,Z1)
        Velocity1 = Q1 / Area1
        BetaVelocity1 = Beta(X1,Z1) * Velocity1

        !          downstream face
        X2 = StreamDistance(Down)
        Width2 = ChannelWidth(X2,Z2)
        Area2 = CxArea(X2,Z2)
        Velocity2 = Q2 / Area2
        BetaVelocity2 = Beta(X2,Z2) * Velocity2

        !-----Time increment and weighting.
        DT = DFLOAT( NetworkTimeIncrement() )
        Theta = NetworkTheta()
        OneMinusTheta = 1.0 - Theta

        !-----Spatial increment.
        DX = X2 - X1

        !-----Water-surface slope, assumed constant from X1 to X2.
        DZDX = (Z2 - Z1) / DX

        if( Forward ) then

            !--------Mass equation.

            !  Coef(1)*Q1 + Coef(2)*Z1 + Coef(3)*Q2 + Coef(4)*Z2 = MassConst + MassAdjust

            !--------Dynamic equation.

            !  Coef(5)*Q1 + Coef(6)*Z1 + Coef(7)*Q2 + Coef(8)*Z2 = DynmConst + DynmAdjust

            !--------Contribution from upstream and downstream faces,
            !        integrated in time.

            !--------mass-equation coefficients for change in Q1, Z1, Q2, & Z2.
            Coef(1) = - Theta * NewRho1 * DT
            Coef(2) =   0.0
            Coef(3) =   Theta * NewRho2 * DT
            Coef(4) =   0.0

            !--------dynamic equation coefficients for change in Q1, Z1, Q2, & Z2.
            Coef(5) =-Theta * 2.0 *     NewRho1       * BetaVelocity1 * DT
            Coef(6) = Theta *Velocity1* NewRho1* Width1*BetaVelocity1 * DT
            Coef(7) = Theta * 2.0 *     NewRho2       * BetaVelocity2 * DT
            Coef(8) =-Theta *Velocity2* NewRho2* Width2*BetaVelocity2 * DT

        end if

        if( Iteration > 1 ) then
        else

            ! OldRho1 = OldStreamDensity(Up)
            ! OldRho2 = OldStreamDensity(Down)
            call get_density(CurrentChannel(), dist1, OldRho1)
            call get_density(CurrentChannel(), dist2, OldRho2)

            CoefMassKnown = OneMinusTheta* (OldRho1*Q1 - OldRho2*Q2) * DT

            CoefDynmKnown = OneMinusTheta * ( &
                OldRho1 * Q1 * BetaVelocity1 &
                - OldRho2 * Q2 * BetaVelocity2 &
                ) * DT

        end if

        CoefMassAdjust = Theta * (NewRho1*Q1 - NewRho2*Q2 ) * DT

        CoefDynmAdjust = Theta * ( &
            NewRho1 * Q1 * BetaVelocity1 &
            - NewRho2 * Q2 * BetaVelocity2 &
            ) * DT

        !-----Contribution from spatial dimension,
        !        integrated in space or time and space.

        !-----Derivative of interpolation functions with respect
        !            to space, constant across subdomain.
        DNDX(1) = -1.0 / DX
        DNDX(2) = -DNDX(1)

        QuadPts = NetworkQuadPts()

        do 200 K=1,QuadPts

            !--------Estimate quadrature-point values.

            call NetworkQuadPtWt( K, QuadPt, QuadWt )

            !--------Interpolation functions.
            N(1) = 1.0 - QuadPt
            N(2) = QuadPt

            !--------Location of quadrature point.
            Xdist = N(1) * X1 + N(2) * X2

            !--------Dependent variables.
            Q = N(1) * Q1 + N(2) * Q2
            H = N(1) * H1 + N(2) * H2
            Z = N(1) * Z1 + N(2) * Z2

            !--------Channel geometry variables.
            Width = ChannelWidth( Xdist, Z )
            Area = CxArea( Xdist, Z )

            !--------Density and density gradient.
            NewRho = N(1) * NewRho1 + N(2) * NewRho2
            NewdRhodX = DNDX(1) * NewRho1 + DNDX(2) * NewRho2

            !--------Distance from centroid to water surface.
            ZBar = CxCentroid(Xdist,Z)
            dZBardX = dCxCentroid(Xdist,Z)

            !--------Friction-slope terms.
            Conv = Conveyance(Xdist,Z)
            ConvSq = Conv * Conv
            DConvDZ = dConveyance(Xdist,Z)

            AbsQxQbyConvSq =    ABS(Q) / ConvSq
            AbsQxAreaByConvSq = AbsQxQbyConvSq * Area
            AbsQxQbyConvSq =    AbsQxQbyConvSq * Q

            !--------Sinuosity.
            ! Ma = AreaWtSinuosity(Xdist,Z)
            ! dMadH = dAreaWtSinuosity(Xdist,Z)
            ! Mq = QWtSinuosity(Xdist,Z)
            ! dMqdH = dQWtSinuosity(Xdist,Z)
            Ma = 1.0
            dMadH = 0.0
            Mq = 1.0
            dMqdH = 0.0

            !--------Product of space and time increments.
            DXxDT = DX * DT

            if( Forward ) then
                do 100 J=1,2

                    J2 = J*2

                    !--------------mass equation, coefficients for change in Z,
                    !              coefficient numbers 2 & 4.

                    Coef(J2) = Coef(J2) + QuadWt *    ( &
                        Ma  * Width + Area * dMadH &
                        ) * NewRho * DX * N(J)

                    !--------------dynamic equation, coefficients for change in Q,
                    !              coefficient numbers 5 and 7.

                    Coef(J2+3) = Coef(J2+3) + QuadWt *    ( &
                        NewRho * Mq * DX &
                        + Theta * NewRho * DXxDT * G * 2.0 * AbsQxAreaByConvSq &
                        ) * N(J)

                    !--------------dynamic equation, coefficients for change in Z
                    !              coefficient numbers 6 and 8.

                    Coef(J2+4) = Coef(J2+4) + QuadWt *            ( &
                        NewRho * Q * dMqdH &
                        +    Theta * G * DXxDT * ( &

                        NewRho * ( &
                        Width * DZDX * N(J) &
                        + Area   *    DNDX(J) &
                        ) &

                        + ( &
                        NewdRhodX * ( &
                        ZBar * Width + Area * dZBardX &
                        ) &

                        + ( &
                        NewRho * Q * AbsQxAreaByConvSq * ( &
                        Width / Area - 2.0 * DConvDZ / Conv &
                        ) &
                        ) &
                        )  * N(J) &

                        ) &
                        )

100             continue
            end if

            if( Iteration /= 1 ) then
            else

                OldRho = N(1) * OldRho1 + N(2) * OldRho2
                OlddRhodX = DNDX(1) * OldRho1 + DNDX(2) * OldRho2

                CoefMassKnown = CoefMassKnown + QuadWt * &
                    OldRho * Area * Ma * DX

                CoefDynmKnown = CoefDynmKnown + QuadWt *    ( &
                    OldRho * Q * Mq * DX &
                    - OneMinusTheta * DXxDT * G * Area * ( &
                    OldRho * (DZDX + AbsQxQbyConvSq) &
                    + ZBar * OlddRhodX &
                    ) &
                    )

            end if

            CoefMassAdjust = CoefMassAdjust - QuadWt * &
                NewRho * Area * Ma * DX

            CoefDynmAdjust = CoefDynmAdjust + QuadWt *    ( &
                - NewRho * Q * Mq * DX &
                - Theta * DXxDT * G * Area * ( &
                NewRho * (DZDX + AbsQxQbyConvSq) &
                + ZBar * NewdRhodX &
                ) &
                )

200     continue

        !-----Store computed coefficients.

        if( Forward ) then
            do ii=1,4
                call add_to_matrix(masseq(ii,Up + EqPointer(Branch) - 1),Coef(ii))
            end do
            do ii=1,4
                call add_to_matrix(dynmeq(ii,Up + EqPointer(Branch) - 1),Coef(ii+4))
            end do
        end if

        OK = StoreMassAdjust( Up, CoefMassAdjust )
        OK = StoreDynmAdjust( Up, CoefDynmAdjust )

        if( Iteration > 1 ) then
        else

            OK = StoreMassKnown( Up, CoefMassKnown )
            OK = StoreDynmKnown( Up, CoefDynmKnown )

        end if

        DynamicWaveEqDS = .true.

        return
    end function


end module
!==== EOF floweq1d_ds =====================================================