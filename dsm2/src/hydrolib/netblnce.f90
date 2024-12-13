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

!===== BOF netblnce.inc ================================================
!   Version 93.01, January, 1993

!   Note: 'network.inc' must appear before this file.
module netblnce
    use network
    use channel_schematic, only: OpenChannel, CloseChannel, &
        NumberOfChannels, StreamDistance, &
        NumberOfStreamLocations
    use netcntrl, only: NetworkTimeIncrement, NetworkTheta, &
        VariableStreamDensity, &
        NetworkQuadPts, NetworkQuadPtWt
    use channel_xsect_tbl, only: CxArea
    use chstatus, only: StreamFlow, &
        NewStreamDensity, OldStreamDensity, &
        EstNewStreamDensity, EstOldStreamDensity, &
        StreamSurfaceElevation
    implicit none

    real*8, save, allocatable:: InitialVolume(:), InitialMass(:), &
                                Volume(:), Mass(:), &
                                JnctVolFlow(:), JnctMassFlow(:), &
                                LateralVolFlow(:), LateralMassFlow(:)

!   Definitions:
!     InitialVolume(i) - initial volume of fluid in channel i.
!     InitialMass(i) - initial mass of fluid in channel i.
!     Volume(i) - current volume of fluid in channel i.
!     Mass(i) - current mass of fluid in channel i.
!     JnctVolFlow(i) - net change of volume through ends of channel i.
!     JnctMassFlow(i) - net change of mass through ends of channel i.
!     LateralVolFlow(i) - net change of volume resulting from flows
!                         distributed along reaches of channel i.
!     LateralMassFlow(i) - net change of mass resulting from flows
!                         distributed along reaches of channel i.

!===== EOF netblnce.inc ================================================
contains
    !===== BOF netblnce ====================================================

    !   Module data:

    !    'network.inc'
    !     MaxChannels - maximum number of channels.
    !     NumCh - current number of channels.
    !     Branch - current selected or active channel.
    !     MaxLocations - maximum number of computational or user locations.

    !    'netblnce.inc'
    !     InitialVolume(i) - initial volume of fluid in channel i.
    !     InitialMass(i) - initial mass of fluid in channel i.
    !     Volume(i) - current volume of fluid in channel i.
    !     Mass(i) - current mass of fluid in channel i.
    !     JnctVolFlow(i) - net change of volume through ends of channel i.
    !     JnctMassFlow(i) - net change of mass through ends of channel i.
    !     LateralVolFlow(i) - net change of volume resulting from flows
    !                         distributed along reaches of channel i.
    !     LateralVolMass(i) - net change of mass resulting from flows
    !                         distributed along reaches of channel i.

    !== Public (InitNetBalance) ============================================

    logical function InitNetBalance()
        implicit none


        !   Purpose: Initialize  volume (and mass balance if variable density)
        !            module for a network of open channels.

        !   Arguments:

        !   Argument definitions:

        !   Local Variables:
        integer I, J, K, QuadPts, Locations
        real*8    dT, OneMinusTheta
        real*8    Pt, Wt
        real*8    X1, X2, X, dX
        real*8    Z1, Z2, Z
        logical OK

        !   Routines by module:


        !**** Local:


        !   Programmed by: Lew DeLong
        !   Date:          May   1992
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        !-----Get number of quadrature points for numerical integration.

        if(not(allocated(InitialVolume))) allocate(InitialVolume(Numch))
        if(not(allocated(InitialMass))) allocate(InitialMass(Numch))
        if(not(allocated(Volume))) allocate(Volume(Numch))
        if(not(allocated(Mass))) allocate(Mass(NumCh))
        if(not(allocated(JnctVolFlow))) allocate(JnctVolFlow(Numch))
        if(not(allocated(JnctMassFlow))) allocate(JnctMassFlow(Numch))
        if(not(allocated(LateralVolFlow))) allocate(LateralVolFlow(Numch))
        if(not(allocated(LateralMassFlow))) allocate(LateralMassFlow(Numch))

        QuadPts = NetworkQuadPts()
        dT = DFLOAT(NetworkTimeIncrement())
        OneMinusTheta = 1.0 - NetworkTheta()

        if( VariableStreamDensity() ) then

            !--------Density is variable.

            do 400 I=1,NumberOfChannels()

                OK = OpenChannel(I)
                Locations = NumberOfStreamLocations()

                InitialVolume(I) = 0.0
                InitialMass(I)   = 0.0
                JnctVolFlow(I)   = OneMinusTheta * dT * ( &
                    StreamFlow(1) - StreamFlow( Locations ) &
                    )

                JnctMassFlow(I)  = OneMinusTheta * dT * ( &
                    StreamFlow(1) * OldStreamDensity(1) &
                    - StreamFlow( Locations ) * OldStreamDensity(Locations) &
                    )

                X1 = StreamDistance(1)
                !            H1 = StreamDepth(1)
                Z1 = StreamSurfaceElevation(1)

                do 300 J=1,Locations-1

                    X2 = StreamDistance(J+1)
                    !               H2 = StreamDepth(J+1)
                    Z2 = StreamSurfaceElevation(J+1)

                    dX = X2 - X1

                    do 200 K=1,QuadPts

                        call NetworkQuadPtWt( K, Pt, Wt )
                        X = (1.0-Pt)*X1 + Pt*X2

                        !-----------------Assume Z varies linearly with channel length.
                        Z = (1.0-Pt)*Z1 + Pt*Z2

                        InitialVolume(I) = InitialVolume(I) &
                            + CxArea( X, Z ) * dX * Wt

                        InitialMass(I) = InitialMass(I) &
                            + EstOldStreamDensity( X ) * dX * Wt

200                 continue

                    X1 = X2
                    Z1 = Z2

300             continue

                OK = CloseChannel()

400         continue

        else

            !--------Density is constant.

            do 800 I=1,NumberOfChannels()

                OK = OpenChannel(I)
                Locations = NumberOfStreamLocations()

                InitialVolume(I) = 0.0
                JnctVolFlow(I)   = OneMinusTheta * dT * ( &
                    StreamFlow(1) - StreamFlow( Locations ) &
                    )

                X1 = StreamDistance(1)
                !            H1 = StreamDepth(1)
                Z1 = StreamSurfaceElevation(1)

                do 700 J=1,NumberOfStreamLocations()-1

                    X2 = StreamDistance(J+1)
                    !               H2 = StreamDepth(J+1)
                    Z2 = StreamSurfaceElevation(J+1)

                    dX = X2 - X1

                    do 600 K=1,QuadPts

                        call NetworkQuadPtWt( K, Pt, Wt )
                        X = (1.0-Pt)*X1 + Pt*X2

                        !-----------------Assume Z varies linearly with channel length.
                        Z = (1.0-Pt)*Z1 + Pt*Z2

                        InitialVolume(I) = InitialVolume(I) &
                            + CxArea( X, Z ) * dX * Wt

600                 continue

                    X1 = X2
                    Z1 = Z2

700             continue

                OK = CloseChannel()

800         continue

        end if

        InitNetBalance = .true.

        return
    end function

    !== Public (UpdateNetBalance) ================================================

    logical function UpdateNetBalance()

        implicit none

        !   Purpose:  Compute net inflow to a network of open channels.

        !   Arguments:

        !   Argument definitions:

        !   Local Variables:
        integer I, Locations
        real*8    dT
        logical OK



        !**** Local:


        !   Programmed by: Lew DeLong
        !   Date:          May   1992
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        UpdateNetBalance = .true.

        dT = DFLOAT(NetworkTimeIncrement())

        if( VariableStreamDensity() ) then

            do 100 I=1,NumberOfChannels()

                OK = OpenChannel(I)

                Locations = NumberOfStreamLocations()

                JnctMassFlow(I) = JnctMassFlow(I) &
                    + ( &
                    StreamFlow(1) * NewStreamDensity(1) &
                    - StreamFlow(Locations) * NewStreamDensity(Locations) &
                    ) * dT

                JnctVolFlow(I) = JnctVolFlow(I) &
                    + ( &
                    StreamFlow(1) &
                    - StreamFlow( Locations ) &
                    ) * dT

                OK = CloseChannel()

100         continue

        else

            do 300 I=1,NumberOfChannels()

                OK = OpenChannel(I)

                Locations = NumberOfStreamLocations()

                JnctVolFlow(I) = JnctVolFlow(I) &
                    + ( &
                    StreamFlow(1) &
                    - StreamFlow( Locations ) &
                    ) * dT

                OK = CloseChannel()

300         continue

        end if

        return
    end function

    !== Public (ReportNetBalance) ================================================

    logical function ReportNetBalance()
        use IO_Units
        implicit none

        !   Purpose: Compute and report volume (and mass balance if variable density)
        !            for a network of open channels.

        !   Purpose: Initialize  volume (and mass balance if variable density)
        !            module for a network of open channels.

        !   Arguments:

        !   Argument definitions:


        !   Local Variables:
        integer I, J, K, QuadPts, Locations, U
        real*8    OneMinusTheta, dT
        real*8    Pt, Wt
        real*8    X1, X2, X, dX
        real*8    Z1, Z2, Z
        real*8    Difference, TotalInitialVolume, TotalVolume
        real*8    TotalJnctVolFlow, TotalLateralVolFlow
        real*8    TotalInitialMass, TotalMass
        real*8    TotalJnctMassFlow, TotalLateralMassFlow
        real*8    TotalDifference
        logical OK

        !   Routines by module:



        !**** Local:


        !   Programmed by: Lew DeLong
        !   Date:          May   1992
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        U = unit_output

        !-----Get number of quadrature points for numerical integration.

        QuadPts = NetworkQuadPts()
        OneMinusTheta = 1.0 - NetworkTheta()
        dT =  DFLOAT(NetworkTimeIncrement())

        if( VariableStreamDensity() ) then

            !--------Density is variable.

            do 400 I=1,NumberOfChannels()

                Volume(I) = 0.0
                Mass(I)   = 0.0

                OK = OpenChannel(I)
                Locations = NumberOfStreamLocations()

                JnctVolFlow(I)   = JnctVolFlow(I) &
                    - OneMinusTheta * dT * ( &
                    StreamFlow(1) - StreamFlow( Locations ) &
                    )

                JnctMassFlow(I)  = OneMinusTheta * dT * ( &
                    StreamFlow(1) * NewStreamDensity(1) &
                    - StreamFlow( Locations ) * NewStreamDensity(Locations) &
                    )
                X1 = StreamDistance(1)
                !            H1 = StreamDepth(1)
                Z1 = StreamSurfaceElevation(1)

                do 300 J=1,Locations-1

                    X2 = StreamDistance(J+1)
                    !               H2 = StreamDepth(J+1)
                    Z2 = StreamSurfaceElevation(J+1)

                    dX = X2 - X1

                    do 200 K=1,QuadPts

                        call NetworkQuadPtWt( K, Pt, Wt )
                        X = (1.0-Pt)*X1 + Pt*X2

                        !-----------------Assume Z varies linearly with channel length.
                        Z = (1.0-Pt)*Z1 + Pt*Z2

                        Volume(I) = Volume(I) &
                            + CxArea( X, Z ) * dX * Wt

                        Mass(I) = Mass(I) &
                            + EstNewStreamDensity( X ) * dX * Wt

200                 continue

                    X1 = X2
                    Z1 = Z2

300             continue

                OK = CloseChannel()

400         continue

        else

            !--------Density is constant.

            do 800 I=1,NumberOfChannels()

                Volume(I) = 0.0

                OK = OpenChannel(I)
                Locations = NumberOfStreamLocations()

                JnctVolFlow(I)   = JnctVolFlow(I) &
                    - OneMinusTheta * dT * ( &
                    StreamFlow(1) - StreamFlow( Locations ) &
                    )

                X1 = StreamDistance(1)
                !            H1 = StreamDepth(1)
                Z1 = StreamSurfaceElevation(1)

                do 700 J=1,NumberOfStreamLocations()-1

                    X2 = StreamDistance(J+1)
                    !               H2 = StreamDepth(J+1)
                    Z2 = StreamSurfaceElevation(J+1)

                    dX = X2 - X1

                    do 600 K=1,QuadPts

                        call NetworkQuadPtWt( K, Pt, Wt )
                        X = (1.0-Pt)*X1 + Pt*X2

                        !-----------------Assume Z varies linearly with channel length.
                        Z = (1.0-Pt)*Z1 + Pt*Z2

                        Volume(I) = Volume(I) &
                            + CxArea( X, Z ) * dX * Wt

600                 continue

                    X1 = X2
                    Z1 = Z2

700             continue

                OK = CloseChannel()

800         continue

        end if

        !-----Write results to screen and print file.

        if(VariableStreamDensity() ) then

            write(U,*) ' '
            write(U,*) ' '
            write(U,*) '                                 ___Mass Balance___'
            write(U,*) ' '
            write(U,*) &
                ' Channel   Initial mass.   Final mass.    Junction', &
                '       Lateral    Difference'
            write(U,*) ' '

            TotalDifference      = 0.0
            TotalInitialMass     = 0.0
            TotalMass            = 0.0
            TotalJnctMassFlow    = 0.0
            TotalLateralMassFlow = 0.0

            do 900 I=1,NumberOfChannels()

                Difference = Mass(I) - InitialMass(I) - JnctMassFlow(I) &
                    - LateralMassFlow(I)

                TotalDifference = TotalDifference + Difference
                TotalInitialMass = TotalInitialMass + InitialMass(I)
                TotalMass = TotalMass + Mass(I)
                TotalJnctMassFlow = TotalJnctMassFlow + JnctMassFlow(I)
                TotalLateralMassFlow = TotalLateralMassFlow &
                    + LateralMassFlow(I)

                if( NumberOfChannels() > 1 ) then
                    write(U,'(1X,I7,5F14.2)')  I, &
                        InitialMass(I), Mass(I), JnctMassFlow(I), &
                        LateralMassFlow(I),  Difference
                end if

900         continue

            if( NumberOfChannels() > 1 ) then
                write(U,*) &
                    ' -------------------------------------------------------------', &
                    '--------------------'
            end if

            write(U,'(8X,5F14.2)') &
                TotalInitialMass, TotalMass, TotalJnctMassFlow, &
                TotalLateralMassFlow, TotalDifference

        end if

        write(U,*) ' '
        write(U,*) ' '
        write(U,*) '                               ___Volume Balance___'
        write(U,*) ' '
        write(U,*) &
            ' Channel   Initial vol.    Final vol.     Junction', &
            '       Lateral    Difference'
        write(U,*) ' '

        TotalDifference     = 0.0
        TotalInitialVolume  = 0.0
        TotalVolume         = 0.0
        TotalJnctVolFlow    = 0.0
        TotalLateralVolFlow = 0.0

        do 1000 I=1,NumberOfChannels()

            Difference = Volume(I) - InitialVolume(I) - JnctVolFlow(I) &
                - LateralVolFlow(I)

            TotalDifference = TotalDifference + Difference
            TotalInitialVolume = TotalInitialVolume + InitialVolume(I)
            TotalVolume = TotalVolume + Volume(I)
            TotalJnctVolFlow = TotalJnctVolFlow + JnctVolFlow(I)
            TotalLateralVolFlow = TotalLateralVolFlow &
                + LateralVolFlow(I)

            if( NumberOfChannels() > 1 ) then
                write(U,'(1X,I7,5F14.2)') I, &
                    InitialVolume(I), Volume(I), JnctVolFlow(I), &
                    LateralVolFlow(I),  Difference
            end if

1000    continue

        if( NumberOfChannels() > 1 ) then
            write(U,*) &
                ' -------------------------------------------------------------', &
                '--------------------'
        end if

        write(U,'(8X,5F14.2)') &
            TotalInitialVolume, TotalVolume, TotalJnctVolFlow, &
            TotalLateralVolFlow, TotalDifference
        write(U,*) ' '

        ReportNetBalance = .true.

        return
    end function
end module
!===== EOF netblnce ====================================================
