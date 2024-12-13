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

!===== BOF chcxtbl.inc ================================================
!   Version 93.01, January, 1993

!   Note: If used, this include must appear after "Network.inc".
module channel_xsect_tbl
    use network
    implicit none

#if defined(hydro_1000)
    integer, parameter ::     MaxTables=25000, MaxLinesPerTable=21, MaxLines=MaxLinesPerTable*MaxTables
#else
    integer, parameter ::     MaxTables=5000, MaxLinesPerTable=21, MaxLines=MaxLinesPerTable*MaxTables
#endif
    integer, save, allocatable:: FirstTable(:), LastTable(:), Lines(:)
    integer, save:: Offset(MaxTables)
    real*8, save::  XDistance(MaxTables)
    real*8, save::  Datum(MaxTables)
    real*8, save::  Depth(MaxLines)
    real*8, save::  Width(MaxLines), A(MaxLines), K(MaxLines)
    real*8, save::  P(MaxLines), N(MaxLines)
    real*8, save::  Bta(MaxLines), MA(MaxLines), MQ(MaxLines)
    real*8, save, allocatable:: OneOverManning(:)
    logical, save, allocatable:: Rectangular(:), Prismatic(:)
    logical, save:: Print, WriteIntrp, HermiteBtm

    integer, save:: N1, N2, NP(4), PreviousBranch, PreviousX, PreviousH100
    integer, save:: DegreesOfFreedom
    real*8, save::    Xi, Eta, dH, Shape(4), dShapeDX(4)
    character*16, save:: ID(MaxTables)


    real*8, save::   XHerm(2*MaxTables), ZHerm(2*MaxTables)

contains
    !
    !
    !   Note by Eli Ateljevich, DWR: Many of the interpolation routines used here have
    !   been subsequently changed or fallen into disuse in DSM2. The model no longer
    !   conforms with the HYDIE standard
    !
    !   Module data:
    !
    !     Program Note:
    !
    !     By convention, the order for addressing known points bracketing
    !     the interpolation point is the following:
    !
    !              4      <------------       3
    !                                         ^
    !                         *               |         downstream ----->
    !
    !              1      ------------>       2
    !
    !
    !              ^                          ^
    !             (N1)                       (N2)
    !
    !***********************************************************************



    !=======================================================================
    !   Public: BtmElev
    !=======================================================================

    real*8 function BtmElev(X)
        use common_xsect
        implicit none

        !   Purpose:
        !     Estimate channel-bottom elevation in the current channel
        !     at X downstream distance.

        !   Arguments:
        real*8    X

        !   Argument definitions:
        !     X - downstream distance in current channel.
        !   Local Variables:
        integer &
            vsecno              ! virtual cross-section number (within channel)

        !   Routines by module:

        !**** Local:

        !   Programmed by: Lew DeLong
        !   Date:          OCT   1993
        !   Modified by:   Brad Tom
        !   Last modified: October 10, 1996
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        vsecno = nint(X / virt_deltax(Branch))+1
        BtmElev = virt_min_elev(minelev_index(Branch)+vsecno-1)

        return
    end function

    real*8 function BtmElevAtLocationNumber(LocationNumber)
        use common_xsect
        implicit none

        !   Arguments:
        integer    LocationNumber

        BtmElevAtLocationNumber = virt_min_elev(minelev_index(Branch)+LocationNumber-1)

        return
    end function
    !======= CUBIC ROOT FAST CALCULATIONS
    double precision function CBRTC(X)
        implicit none
        double precision X
        interface
            double precision function CBRT(Y)
                !DEC$ ATTRIBUTES C, ALIAS:'cbrt' :: CBRT
                double precision Y
            end function CBRT
        end interface
        CBRTC = CBRT(%VAL(X))
    end function
    !======================================================================
    !     Calculate width, area, wetted perimeter etc in one call
    !======================================================================
    subroutine calculateChannelGeometryAspects(X,Z,ChannelWidth, CxArea, Conv, dConveyance)
        use IO_Units
        use logging
        use common_xsect, disabled => Small !@# Small declared below.
        use chnlcomp
        implicit none
        !   Arguments:
        real*8    X,Z,ChannelWidth, CxArea, WetPerimeter, Conv, dConveyance
        !   Argument definitions:
        !     X      - downstream distance in current channel.
        !     Z      - changed to Elevation based!

        !----local variables
        real*8  &
            x1 &                 ! interpolation variables &
            ,x2 &
            ,y1 &
            ,y2
        integer &
            vsecno &              ! number of virtual section (within channel) &
            ,virtelev &           ! number of virtual elevation (within channel) &
            ,veindex &            ! index of virtual elevation array &
            ,dindex &             ! function to calculate xsect prop. array index &
            ,di                  ! stores value of dindex
        real*8  &
            a1 &                  ! area of lower layer &
            ,b1 &                 ! width of lower layer (base of trapezoid) &
            ,b2 &                 ! interpolated width (trapezoid top width) &
            ,CBRTCAREAWP

        real*8 effectiven,dwetperimeter,deffectiven
        real*8 AOWP
        !   Local Variables:
        real*8 R53,R23
        parameter (R53 = 5.0/3.0, R23 = 2.0/3.0)
        real*8 Small
        parameter (Small = 1.00e-6)
        real*8 slope,dx,dwp
        !----statement function to interpolate wrt two points
        !interp(x1,x2,y1,y2,Z) =-((y2-y1)/(x2-x1))*(x2-Z) + y2
        call find_layer_index( &
            X &
            ,Z &
            ,Branch &
            ,vsecno &
            ,virtelev &
            ,veindex &
            )


        di=chan_index(Branch) + (vsecno-1)*num_layers(Branch) + virtelev-1
        x1=virt_elevation(veindex)
        x2=virt_elevation(veindex+1)
        dx=x2-x1
        if ( abs(dx) <= Small) then
            write(unit_error,*) 'Channel width, wet perimeter, dwetperimeter division by zero'
        end if

        slope=(Z-x2)/dx
        y1=virt_width(di)
        y2=virt_width(di+1)
        ChannelWidth = (y2-y1)*slope+y2 !interp(x1,x2,y1,y2,Z)

        a1=virt_area(di)
        b1=virt_width(di)
        b2=ChannelWidth
        CxArea = a1+(0.5*(b1+b2))*(Z-x1)
        if (vsecno>MaxCompPts) then
            write(unit_error,*) 'Increase MaxCompPts. vsecno greater than MaxCompPts:', vsecno, '>', MaxCompPts
            call exit(2)
        end if
        AreaChannelComp(Branch,vsecno)=CxArea

        EffectiveN = OneOverManning(Branch)

        y1=virt_wet_p(di)
        y2=virt_wet_p(di+1)
        dwp=y2-y1
        WetPerimeter = dwp*slope+y2


        if (WetPerimeter > Small) then
            AOWP=CxArea/WetPerimeter
            CBRTCAREAWP = CBRTC(AOWP*AOWP)
            Conv = 1.486*CxArea * EffectiveN * CBRTCAREAWP
            dWetPerimeter = dwp/dx
            dEffectiveN = 0.
            dConveyance = 1.486 * ( &
                R53 * ChannelWidth * EffectiveN &
                * CBRTCAREAWP &
                - R23 * dWetPerimeter * EffectiveN &
                * AOWP*CBRTCAREAWP &
                + dEffectiveN * CxArea * CBRTCAREAWP &
                )


        else
            Conv=0.0
            dConveyance = 0.0
        end if

        if( dConveyance < 0.0 ) then
            dConveyance = Conveyance(X,Z+1.) - Conveyance(X,Z)!-- couldn't fix it right away Nicky Sandhu -> FIXME:
            if( dConveyance < 0.0 ) then
                if (print_level >= 4) then
                    write(unit_error,610) chan_geom(Branch)%chan_no,X,Z, &
                        ChannelWidth,CxArea,Conv, &
                        EffectiveN,dEffectiveN, &
                        WetPerimeter, dWetPerimeter
610                 format(/'Warning in dConveyance; negative gradient with depth,' &
                        /' consider reworking adjacent cross section(s).' &
                        /' Channel...',i3 &
                        /' X, Z...',2f10.2,1p &
                        /' Width, Area, Conveyance...',3g10.3, &
                        /' one over effective n, 1/(dn/dZ)...',2g10.3 &
                        /' P, dP/dZ...',2g10.3)
                end if
            end if
        end if

    end subroutine
    !=======================================================================
    !   Public: ChannelWidth
    !=======================================================================

    real*8 function ChannelWidth(X,Z)
        use IO_Units
        use common_xsect
        implicit none

        !   Purpose:
        !     Estimate channel width in the current channel at X downstream distance and
        !     at H distance above the lowest point in the cross section.

        !   Arguments:
        real*8    X,Z

        !   Argument definitions:
        !     X      - downstream distance in current channel.
        !     Z      - changed to Elevation based!
        !   Module data:


        !   Functions:
        logical  CxShapeFunction
        external CxShapeFunction

        !   Subroutines:

        !   Programmed by: Lew DeLong
        !   Date:          July  1991
        !   Modified by:   Brad Tom
        !   Last modified: October 10, 1996
        !   Version 93.01, January, 1993

        !----local variables
        real*8  &
            x1  &                 ! interpolation variables &
            ,x2 &
            ,y1 &
            ,y2 &
            ,interp              ! interpolation function
        integer &
            vsecno  &             ! number of virtual section (within channel) &
            ,virtelev &           ! number of virtual elevation (within channel) &
            ,veindex &            ! index of virtual elevation array &
            ,dindex &             ! function to calculate xsect prop. array index &
            ,di                  ! stores value of dindex

        !----statement function to calculate indices of virtual data arrays
        dindex(Branch,vsecno,virtelev) &
            =chan_index(Branch) + (vsecno-1)*num_layers(Branch) + virtelev-1
        !----statement function to interpolate wrt two points
        interp(x1,x2,y1,y2,Z) =-((y2-y1)/(x2-x1))*(x2-Z) + y2

        call find_layer_index( &
            X &
            ,Z &
            ,Branch &
            ,vsecno &
            ,virtelev &
            ,veindex &
            )


        di=dindex(Branch,vsecno,virtelev)
        x1=virt_elevation(veindex)
        x2=virt_elevation(veindex+1)
        y1=virt_width(di)
        y2=virt_width(di+1)
        ChannelWidth = interp(x1,x2,y1,y2,Z)
        if (x1==x2) then
            write(unit_error,*) 'ChannelWidth division by zero'
        endif

        return
    end function


    !=======================================================================
    !   Public: CxArea
    !=======================================================================

    real*8 function CxArea(X, Z)
        use io_units
        use common_xsect, only: chan_index, num_layers, &
            virt_elevation, virt_width, virt_area
        implicit none

        !   Purpose:
        !     Estimate cross-sectional area in the current channel, at X downstream
        !     distance, limited by the lowest point in the channel and a
        !     distance H above the lowest point.

        !   Arguments:
        real*8    X, Z              ! h-height of trapezoid

        !   Argument definitions:
        !     X - downstream distance.
        !     Z - changed to Elevation based!

        !   Routines by module:

        !**** Local:
        real*8  &
            x1 &                  ! interpolation variables &
            ,x2

        integer &
            vsecno &              ! number of virtual section (within channel) &
            ,virtelev &           ! number of virtual elevation (within channel) &
            ,veindex &            ! index of virtual elevation array &
            ,di                  ! stores value of dindex
        real*8  &
            a1 &                  ! area of lower layer &
            ,b1 &                 ! width of lower layer (base of trapezoid) &
            ,b2                  ! interpolated width (trapezoid top width)
        real*8 localChannelWidth
        real*8 Small
        parameter (Small = 1.00e-6)
        real*8 slope,dx,y1,y2
        !   Intrinsics:

        !   Programmed by: Lew DeLong
        !   Date:          July  1991
        !   Modified by:   Brad Tom
        !   Last modified: October 10, 1996
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        !----statement function to calculate indices of virtual data arrays
        call find_layer_index( &
            X &
            ,Z &
            ,Branch &
            ,vsecno &
            ,virtelev &
            ,veindex &
            )


        di=chan_index(Branch) + (vsecno-1)*num_layers(Branch) + virtelev-1
        x1=virt_elevation(veindex)
        x2=virt_elevation(veindex+1)
        dx=x2-x1
        if ( abs(dx) <= Small) then
            write(unit_error,*) 'Channel width, wet perimeter, dwetperimeter division by zero'
        endif

        slope=(Z-x2)/dx
        y1=virt_width(di)
        y2=virt_width(di+1)
        localChannelWidth = (y2-y1)*slope+y2 !interp(x1,x2,y1,y2,Z)

        a1=virt_area(di)
        b1=virt_width(di)
        b2=localChannelWidth
        CxArea = a1+(0.5*(b1+b2))*(Z-x1)

        return
    end function



    !=======================================================================
    !   Public: CxCentroid
    !=======================================================================

    real*8 function CxCentroid(X, H)
        use IO_Units
        use common_xsect, disabled => Small !@# Small declared below.
        implicit none

        !   Purpose:
        !     Estimate distance from the water surface to the cross-section
        !     centroid, in the current channel, at X downstream distance,
        !     limited by the lowest point in the channel and a
        !     distance H above the lowest point.

        !   Arguments:
        real*8    X, H

        !   Argument definitions:
        !     X - downstream distance.
        !     H - distance above lowest point in cross section.


        !   Local variables:
        real*8 Small
        parameter (Small = 1.00e-6)

        !   Routines by module:

        integer &
            vsecno  &             ! number of virtual section (within channel) &
            ,virtelev &           ! number of virtual elevation (within channel) &
            ,veindex &            ! index of virtual elevation array &
            ,dindex &             ! function to calculate xsect prop. array index &
            ,di                  ! stores value of dindex
        real*8  &
            trap_height &         ! height of trapezoid &
            ,trap_top_width &     ! top width of trapezoid &
            ,trap_bot_width &     ! bottom width of trapezoid &
            ,Aprev &              ! area of lower layer &
            ,Arect &              ! area of rectangular region &
            ,Atria &              ! area of triangular region &
            ,Cprev &              ! Z centroid of lower layer &
            ,Crect &              ! Z centroid of rectangular region &
            ,Ctria               ! Z centroid of triangular region

        !   Intrinsics:

        !   Programmed by: Lew DeLong
        !   Date:          July  1991
        !   Modified by:   Barry Wicktom, Brad Tom
        !   Last modified: October 10, 1996
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        !----statement function to calculate indices of virtual data arrays
        dindex(Branch,vsecno,virtelev) &
            =chan_index(Branch) + (vsecno-1)*num_layers(Branch) + virtelev-1

        call find_layer_index( &
            X &
            ,H &
            ,Branch &
            ,vsecno &
            ,virtelev &
            ,veindex &
            )

        di=dindex(branch,vsecno,virtelev)
        trap_height=  H -virt_elevation(veindex)
        trap_top_width = ChannelWidth(X,H)
        trap_bot_width = virt_width(di)

        if (virtelev == 1) then
            Aprev = 0.0
        elseif (virtelev > 1) then
            Aprev = virt_area(di-1)
        endif

        Arect = min(trap_top_width,trap_bot_width ) * trap_height
        Atria = abs( 0.5 * (trap_top_width-trap_bot_width)*trap_height)
        Cprev = virt_z_centroid(di-1)
        Crect = virt_elevation(veindex) + trap_height/2
        if ( trap_top_width > trap_bot_width) then
            Ctria = virt_elevation(veindex) + (2/3)*trap_height
        elseif ( trap_top_width < trap_bot_width ) then
            Ctria = virt_elevation(veindex) + (1/3)*trap_height
        elseif ( trap_top_width == trap_bot_width ) then
            Ctria = 0.0
        endif
        CxCentroid = H-( &
            ((Aprev*Cprev) + (Arect*Crect) + (Atria*Ctria)) / (Aprev+Arect+Atria) &
            )

        if (aprev+arect+atria== 0.0) then
            write(unit_error,*) 'cxcentroid division by zero'
        endif

        return
    end function

    !=======================================================================
    !   Public: dCxCentroid
    !=======================================================================

    real*8 function dCxCentroid(X, H)
        use IO_Units
        use common_xsect, disabled => Small !@# Small declared below.
        implicit none

        !   Purpose:
        !     Estimate d(centroid distance from surface)/dH,
        !     in the current channel, at X downstream distance,
        !     limited by the lowest point in the channel and a
        !     distance H above the lowest point.

        !   Arguments:
        real*8    X, H

        !   Argument definitions:
        !     X - downstream distance.
        !     H - distance above lowest point in cross section.

        !   Local variables:
        real*8 Small
        parameter (Small = 1.00e-6)
        !@@@      INTEGER I, L1, L2
        !@@@      REAL*8    XiM, DX
        !@@@      REAL*8    Z1, Z2, WZ1, WZ2, Z2MZ1
        real*8  &
            x1 &                  ! interpolation variables &
            ,x2 &
            ,y1 &
            ,y2


        integer &
            vsecno &              ! number of virtual section (within channel) &
            ,virtelev &           ! number of virtual elevation (within channel) &
            ,veindex &            ! index of virtual elevation array &
            ,dindex &             ! function to calculate xsect prop. array index &
            ,di                  ! stores value of dindex


        !   Intrinsics:

        !   Programmed by: Lew DeLong
        !   Date:          July  1991
        !   Modified by:   Barry Wicktom, Brad Tom
        !   Last modified: October 10, 1996
        !   Version 93.01, January, 1993

        !----statement function to calculate indices of virtual data arrays
        dindex(Branch,vsecno,virtelev) &
            =chan_index(Branch) + (vsecno-1)*num_layers(Branch) + virtelev-1

        call find_layer_index( &
            X &
            ,H &
            ,Branch &
            ,vsecno &
            ,virtelev &
            ,veindex &
            )


        di=dindex(branch,vsecno,virtelev)
        x1=virt_elevation(veindex)
        x2=virt_elevation(veindex+1)
        y1=virt_z_centroid(di)
        y2=virt_z_centroid(di+1)

        dCxCentroid = H- (y2-y1)/(x2-x1)

        if (x1==x2) then
            write(unit_error,*) 'dCxcentroid division by zero'
        endif
        return
    end function


    !=======================================================================
    !   Public: Conveyance
    !=======================================================================

    real*8 function Conveyance(X, Z)

        implicit none

        !   Purpose:
        !     Estimate sinuosity-weighted conveyance in the current channel,
        !     at X downstream distance, limited by the lowest point in the channel
        !     and a distance H above the lowest point.

        !   Arguments:
        real*8    X, Z

        !   Argument definitions:
        !     X - downstream distance.
        !     H - distance above lowest point in cross section.
        !   Local Variables:
        real*8 R53,R23
        parameter (R53 = 5.0/3.0, R23 = 2.0/3.0)
        real*8 Small
        parameter (Small = 1.00e-6)


        !   Intrinsics:

        !   Programmed by: Lew DeLong
        !   Date:          August 1994
        !   Modified by:   Brad Tom
        !   Last modified: October 10, 1996

        !-----Implementation -----------------------------------------------------

        if (WetPerimeter(X,Z)>Small) then

            Conveyance = 1.486*(CxArea(X,Z))**R53 * &
                (EffectiveN(X,Z)/(WetPerimeter(X,Z))**R23)

        else
            Conveyance = 0.0
        endif

        return
    end function

    !=======================================================================
    !   Public: dConveyance
    !=======================================================================

    real*8 function dConveyance(X, Z)
        use IO_Units
        use logging
        use common_xsect, only: chan_geom   !@# Without "only:", Small causes conflict.
        use channel_schematic, only: CurrentChannel
        implicit none

        !   Purpose:
        !     Estimate d(sinuosity-weighted conveyance)/dH in the current channel,
        !     at X downstream distance, limited by the lowest point in the channel
        !     and a distance H above the lowest point.

        !   Arguments:
        real*8    X, Z &
            ,EN,Ar,Per

        !   Argument definitions:
        !     X - downstream distance.
        !     H - distance above lowest point in cross section.

        !   Local Variables:
        real*8 R53,R23
        parameter (R53 = 5.0/3.0, R23 = 2.0/3.0)
        real*8 Small
        parameter (Small = 1.00e-6)

        !   Intrinsics:

        !   Programmed by: Lew DeLong
        !   Date:          August 1994
        !   Modified by:   Brad Tom
        !   Last modified: October 10, 1996

        !-----Implementation -----------------------------------------------------

        if (wetperimeter(x,Z)>small) then

            EN=EffectiveN(X,Z)
            Ar=CxArea(X,Z)
            Per=WetPerimeter(X,Z)

            dConveyance = 1.486 * ( &
                R53 * ChannelWidth(X,Z) * EN &
                * ( Ar / Per )**R23 &
                - R23 * dWetPerimeter(X,Z) * EN &
                * ( Ar  / Per )**R53 &
                + dEffectiveN(X,Z) * Ar**R53 &
                /  Per**R23 &
                )


        else
            dConveyance = 0.0
        endif

        if( dConveyance < 0.0 ) then
            !         IF( H .GT. 0.5 ) THEN
            !            dConveyance = Conveyance(X,H+.5) - Conveyance(X,H-.5)
            !         ELSE
            dConveyance = Conveyance(X,Z+1.) - Conveyance(X,Z)
            !         END IF
            if( dConveyance < 0.0 ) then
                if (print_level >= 4) then
                    write(unit_error,610) chan_geom(Branch)%chan_no,X,Z, &
                        ChannelWidth(X,Z),CxArea(X,Z),Conveyance(X,Z), &
                        EffectiveN(X,Z),dEffectiveN(X,Z), &
                        WetPerimeter(X,Z), dWetPerimeter(X,Z)
610                 format(/'Warning in dConveyance; negative gradient with depth,' &
                        /' consider reworking adjacent cross section(s).' &
                        /' Channel...',i3 &
                        /' X, Z...',2f10.2,1p &
                        /' Width, Area, Conveyance...',3g10.3, &
                        /' one over effective n, 1/(dn/dZ)...',2g10.3 &
                        /' P, dP/dZ...',2g10.3)
                !            call exit(2)
                end if
            end if
        end if

        return
    end function

    !=======================================================================
    !   Public: EffectiveN
    !=======================================================================

    real*8 function EffectiveN(X, H)

        implicit none

        !   Purpose:
        !     Estimate one over effective Manning's "n" in the current channel,
        !     at X downstream distance,  for the area limited by the lowest point
        !     in the channel and a distance H above the lowest point.

        !   Arguments:
        real*8    X, H

        !   Argument definitions:
        !     X - downstream distance.
        !     H - distance above lowest point in cross section.
        !   Local Variables:


        !   Routines by module:

        !**** Local:

        !   Intrinsics:

        !   Programmed by: Lew DeLong
        !   Date:          July  1991
        !   Modified by:   Brad Tom
        !   Last modified: October 10, 1996
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        EffectiveN = OneOverManning(Branch)

        return
    end function

    !=======================================================================
    !   Public: dEffectiveN
    !=======================================================================

    real*8 function dEffectiveN(X, H)

        implicit none

        !   Purpose:
        !     Estimate d(EffectiveN)/dH in the current channel,
        !     at X downstream distance,  for the area limited by the lowest point
        !     in the channel and a distance H above the lowest point.

        !   Arguments:
        real*8    X, H

        !   Argument definitions:
        !     X - downstream distance.
        !     H - distance above lowest point in cross section.

        !   Local Variables:


        !   Routines by module:

        !**** Local:


        !   Intrinsics:

        !   Programmed by: Lew DeLong
        !   Date:          July  1991
        !   Modified by:   Brad Tom
        !   Last modified: October 10, 1996
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        dEffectiveN = 0.
        return
    end function


    !=======================================================================
    !   Public: WetPerimeter
    !=======================================================================

    real*8 function WetPerimeter(X, Z)
        use IO_Units
        use common_xsect
        implicit none

        !   Purpose:
        !     Estimate wetted perimeter of a cross section in the current channel,
        !     at X downstream distance, limited by the lowest point in the channel
        !     and a distance H above the lowest point.

        !   Arguments:
        real*8    X, Z

        !   Argument definitions:
        !     X - downstream distance.
        !     H - distance above lowest point in cross section.
        !   Routines by module:

        !**** Local:


        !   Intrinsics:

        !   Programmed by: Lew DeLong
        !   Date:          July  1991
        !   Modified by:   Brad Tom
        !   Last modified: October 10, 1996
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        real*8  &
            x1 &                  ! interpolation variables &
            ,x2 &
            ,y1 &
            ,y2 &
            ,interp              ! interpolation function
        integer &
            vsecno &              ! number of virtual section (within channel) &
            ,virtelev &           ! number of virtual elevation (within channel) &
            ,veindex &            ! index of virtual elevation array &
            ,dindex &             ! function to calculate xsect prop. array index &
            ,di                  ! stores value of dindex

        !----statement function to calculate indices of virtual data arrays
        dindex(Branch,vsecno,virtelev) &
            =chan_index(Branch) + (vsecno-1)*num_layers(Branch) + virtelev-1
        !----statement function to interpolate wrt two points
        interp(x1,x2,y1,y2,Z) =-((y2-y1)/(x2-x1))*(x2-Z) + y2

        call find_layer_index( &
            X &
            ,Z &
            ,Branch &
            ,vsecno &
            ,virtelev &
            ,veindex &
            )

        di=dindex(branch,vsecno,virtelev)
        x1=virt_elevation(veindex)
        x2=virt_elevation(veindex+1)
        y1=virt_wet_p(di)
        y2=virt_wet_p(di+1)
        WetPerimeter = interp(x1,x2,y1,y2,Z)

        if (x1-x2== 0.0) then
            write(unit_error,*) 'wetperimeter division by zero'
        endif

        return
    end function

    !=======================================================================
    !   Public: dWetPerimeter
    !=======================================================================

    real*8 function dWetPerimeter(X, Z)
        use IO_Units
        use common_xsect
        implicit none

        !   Purpose:
        !     Estimate d(WetPerimeter)/dH of a cross section in the current channel,
        !     at X downstream distance, limited by the lowest point in the channel
        !     and a distance H above the lowest point.

        !   Arguments:
        real*8    X, Z

        !   Argument definitions:
        !     X - downstream distance.
        !     H - distance above lowest point in cross section.

        !   Local Variables:
        real*8  &
            x1 &                  ! interpolation variables &
            ,x2 &
            ,y1 &
            ,y2
        integer &
            vsecno &              ! number of virtual section (within channel) &
            ,virtelev &           ! number of virtual elevation (within channel) &
            ,veindex &            ! index of virtual elevation array &
            ,dindex &             ! function to calculate xsect prop. array index &
            ,di                  ! stores value of dindex

        !   Routines by module:

        !**** Local:

        !   Intrinsics:

        !   Programmed by: Lew DeLong
        !   Date:          July  1991
        !   Modified by:   Brad Tom
        !   Last modified: October 10, 1996
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        !----statement function to calculate indices of virtual data arrays
        dindex(Branch,vsecno,virtelev) &
            =chan_index(Branch) + (vsecno-1)*num_layers(Branch) + virtelev-1

        call find_layer_index( &
            X &
            ,Z &
            ,Branch &
            ,vsecno &
            ,virtelev &
            ,veindex &
            )

        di=dindex(branch,vsecno,virtelev)
        x1=virt_elevation(veindex)
        x2=virt_elevation(veindex+1)
        y1=virt_wet_p(di)
        y2=virt_wet_p(di+1)

        dWetPerimeter = (y2-y1)/(x2-x1)
        if (x2-x1== 0.0) then
            write(unit_error,*) 'dwetperimeter division by zero'
        endif
        return
    end function

    !=======================================================================
    !   Public: Beta
    !=======================================================================

    real*8 function Beta(X, H)

        implicit none

        !   Purpose:
        !     Estimate the momentum coefficient in the current channel,
        !     at X downstream distance, limited by the lowest point in the channel
        !     and a distance H above the lowest point.

        !   Arguments:
        real*8    X, H

        !   Argument definitions:
        !     X - downstream distance.
        !     H - distance above lowest point in cross section.

        !   Local Variables:

        !   Routines by module:

        !**** Local:


        !   Intrinsics:

        !   Programmed by: Lew DeLong
        !   Date:          July  1991
        !   Modified by:   Brad Tom
        !   Last modified: October 10, 1996
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        Beta = 1.


        return
    end function

    !=======================================================================
    !   Public: dBeta
    !=======================================================================

    real*8 function dBeta(X, H)

        implicit none

        !   Purpose:
        !     Estimate d(Beta)/dH in the current channel,
        !     at X downstream distance, limited by the lowest point in the channel
        !     and a distance H above the lowest point.

        !   Arguments:
        real*8    X, H

        !   Argument definitions:
        !     X - downstream distance.
        !     H - distance above lowest point in cross section.

        !   Local Variables:


        !   Routines by module:

        !**** Local:

        !   Intrinsics:

        !   Programmed by: Lew DeLong
        !   Date:          July  1991
        !   Modified by:   Brad Tom
        !   Last modified: October 10, 1996
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        dBeta = 0.0


        return
    end function

    !=======================================================================
    !   Public: AreaWtSinuosity
    !=======================================================================

    real*8 function AreaWtSinuosity(X, H)
        use IO_Units
        implicit none

        !   Purpose:
        !     Estimate area-weighted sinuosity in the current channel,
        !     at X downstream distance,  for the area limited by the lowest point
        !     in the channel and a distance H above the lowest point.

        !   Arguments:
        real*8    X, H

        !   Argument definitions:
        !     X - downstream distance.
        !     H - distance above lowest point in cross section.

        !   Local Variables:
        integer I


        !   Intrinsics:

        !   Programmed by: Lew DeLong
        !   Date:          July  1991
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------
        if(not(allocated(Rectangular))) allocate(Rectangular(NumCh))
        if(not(allocated(Prismatic))) allocate(Prismatic(NumCh))
        Rectangular = .false.
        Prismatic = .false.

        if(Rectangular(Branch).and.Prismatic(Branch)) then
            AreaWtSinuosity = 1.0
        elseif(CxShapeFunction(X,H)) then
            AreaWtSinuosity = 0.0
            do 100 I=1,DegreesOfFreedom
                AreaWtSinuosity = AreaWtSinuosity+Shape(I)*MA(NP(I))
100         continue
        else
            write(unit_error,*) '*** error(AreaWtSinuosity)',Branch,X,H
            stop
        end if

        return
    end function

    !=======================================================================
    !   Public: dAreaWtSinuosity
    !=======================================================================

    real*8 function dAreaWtSinuosity(X, H)
        use IO_Units
        implicit none

        !   Purpose:
        !     Estimate d(area-weighted sinuosity)/dH in the current channel,
        !     at X downstream distance,  for the area limited by the lowest point
        !     in the channel and a distance H above the lowest point.

        !   Arguments:
        real*8    X, H

        !   Argument definitions:
        !     X - downstream distance.
        !     H - distance above lowest point in cross section.

        !   Local Variables:
        integer I


        !   Intrinsics:

        !   Programmed by: Lew DeLong
        !   Date:          July  1991
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        if(Rectangular(Branch).and.Prismatic(Branch)) then
            dAreaWtSinuosity = 0.0
            return
        elseif(CxShapeFunction(X,H)) then
            dAreaWtSinuosity = 0.0
            do 100 I=1,DegreesOfFreedom
                dAreaWtSinuosity = dAreaWtSinuosity+dShapeDX(I)*MA(NP(I))
100         continue
        else
            write(unit_error,*) '*** error(dAreaWtSinuosity)',Branch,X,H
            stop
        end if

        dAreaWtSinuosity = dAreaWtSinuosity/dH

        return
    end function

    !=======================================================================
    !   Public: QWtSinuosity
    !=======================================================================

    real*8 function QWtSinuosity(X, H)
        use IO_Units
        implicit none

        !   Purpose:
        !     Estimate discharge-weighted sinuosity in the current channel,
        !     at X downstream distance,  for the area limited by the lowest point
        !     in the channel and a distance H above the lowest point.

        !   Arguments:
        real*8    X, H

        !   Argument definitions:
        !     X - downstream distance.
        !     H - distance above lowest point in cross section.

        !   Local Variables:
        integer I



        !   Intrinsics:

        !   Programmed by: Lew DeLong
        !   Date:          July  1991
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        if(Rectangular(Branch).and.Prismatic(Branch)) then
            QWtSinuosity = 1.0
        elseif(CxShapeFunction(X,H)) then
            QWtSinuosity = 0.0
            do 100 I=1,DegreesOfFreedom
                QWtSinuosity = QWtSinuosity+Shape(I)*MQ(NP(I))
100         continue
        else
            write(unit_error,*) '*** error(QWtSinuosity)',Branch,X,H
            stop
        end if

        return
    end function

    !=======================================================================
    !   Public: dQWtSinuosity
    !=======================================================================

    real*8 function dQWtSinuosity(X, H)
        use IO_Units
        implicit none

        !   Purpose:
        !     Estimate d(discharge-weighted sinuosity)/dH in the current channel,
        !     at X downstream distance,  for the area limited by the lowest point
        !     in the channel and a distance H above the lowest point.

        !   Arguments:
        real*8    X, H

        !   Argument definitions:
        !     X - downstream distance.
        !     H - distance above lowest point in cross section.

        !   Local Variables:
        integer I


        !   Intrinsics:

        !   Programmed by: Lew DeLong
        !   Date:          July  1991
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        if(Rectangular(Branch).and.Prismatic(Branch)) then
            dQWtSinuosity = 0.0
            return
        elseif(CxShapeFunction(X,H)) then
            dQWtSinuosity = 0.0
            do 100 I=1,DegreesOfFreedom
                dQWtSinuosity = dQWtSinuosity+dShapeDX(I)*MQ(NP(I))
100         continue
        else
            write(unit_error,*) '*** error(dQWtSinuosity)',Branch,X,H
            stop
        end if
        dQWtSinuosity = dQWtSinuosity/dH

        return
    end function

    !=======================================================================
    !   Private: CxShapeFunction
    !=======================================================================

    logical function CxShapeFunction(X, H)
        use IO_Units
        !use chstatus, only: WriteNetworkRestartFile FIXME: N. Sandhu
        use channel_schematic, only: CloseChannel

        implicit none

        !   Purpose:
        !     Compute common factors governing the interpolation of hydraulic
        !     properties at a point.

        !   Arguments:
        real*8    X, H

        !   Argument definitions:
        !     X - downstream distance.
        !     H - distance above lowest point in cross section.

        !   Local Variables:

        logical ok

        !   Routines by module:


        !   Intrinsics:

        !   Programmed by: Lew DeLong
        !   Date:          July  1991
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        CxShapeFunction = .false.

        if(     Branch /= PreviousBranch &
            .or. &
            INT(X) /= PreviousX &
            .or. &
            INT(100.0*H) /= PreviousH100   ) then

            PreviousBranch = Branch
            PreviousX      = INT(X)
            PreviousH100   = INT(100.0*H)

            !--------Determine relative longitudinal location of a point
            !       within a branch.
            if(SetXi(X)) then
                !-----------Determine relative vertical location of the point.
                if(SetEta(X,H)) then
                    !--------------Compute bi-linear shape functions.
                    if(BiLinearShapeFunction()) then
                    end if
                end if
            else
                ok = CloseChannel()
                write(unit_error,*) ' Flushing series buffer...'

                !--------------Write network restart file. FIXME: N. Sandhu
                !write(unit_error,*) ' Writing restart file if requested...'
                !ok = WriteNetworkRestartFile()

                write(unit_error,*) '*** error(CxShapeFunction)..branch,X',Branch,X
                write(unit_error,*) ' '
                write(unit_error,*) ' Abnormal program end.'
                call EXIT(2)
            end if
        end if

        CxShapeFunction = .true.

        return
    end function

    !=======================================================================
    !   Private: SetXi
    !=======================================================================

    logical function SetXi(X)
        use IO_Units
        implicit none

        !   Purpose:
        !     Locate current point in relation to X locations of existing tables.

        !   Arguments:
        real*8    X

        !   Argument definitions:
        !     X - downstream distance.

        !   Local Variables:
        integer I, II

        !   Routines by module:

        !**** Local:

        !   Intrinsics:
        integer   INT
        intrinsic INT

        !   Programmed by: Lew DeLong
        !   Date:          July  1991
        !   Modified by:
        !   Last modified: Sept  1993
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        SetXi = .false.

        do II=FirstTable(Branch),LastTable(Branch)
            I = II
            if( X < XDistance(I) ) exit
        end do
        if (X < XDistance(I)) then
            if(INT(X)/=INT(XDistance(LastTable(Branch)))) then
                write(unit_error,*) ' *** warning(SetXi)'
                write(unit_error,*) X,' > maximum XDistance = ', &
                    XDistance( LastTable(Branch) )
            end if
        end if

    if( INT(X) /= INT(XDistance(FirstTable(Branch))) ) then
        if( I /= FirstTable(Branch) ) then
            N1 = I-1
            N2 = I
            Xi = (X-XDistance(N1))/(XDistance(N2)-XDistance(N1))
        else
            write(unit_error,*) '*** error(SetXi)'
            write(unit_error,*) X, '< minimum XDistance = ', &
                XDistance(FirstTable(Branch))
            Xi = 0.0
        end if
    else

        N1 = FirstTable(Branch)
        N2 = N1 + 1
        Xi = 0.0

    end if

    DegreesOfFreedom = 4

    SetXi = .true.

    return
end function

!=======================================================================
!   Private: SetEta
!=======================================================================

logical function SetEta(X, H)
    use IO_Units
    implicit none

    !   Purpose:
    !     Locate current point (H) in relation to depths tabulated at
    !     XDistance(N1) and XDistance(N2).  N1 and N2 must be current.

    !   Arguments:
    real*8    X, H

    !   Argument definitions:
    !     X - downstream distance.
    !     H - distance above lowest point in cross section.

    !   Local Variables:
    integer I, L1, L2
    real*8    XiM

    !   Routines by module:

    !**** Local:

    !   Intrinsics:

    !   Programmed by: Lew DeLong
    !   Date:          July  1991
    !   Modified by:
    !   Last modified:
    !   Version 93.01, January, 1993

    !-----Implementation -----------------------------------------------------

    SetEta = .false.
    XiM = 1.0-Xi

    !---- Determine which pair of upstream-downstream points bracket the
    !     interpolation point on the top and then exit the loop.

    L1 = Offset(N1)-1
    L2 = Offset(N2)-1
    do 100 I=1,Lines(Branch)
        L1 = L1+1
        L2 = L2+1

        if(H < (XiM*Depth(L1)+Xi*Depth(L2))) go to 200
100 continue

    if(H > (XiM*Depth(L1)+Xi*Depth(L2) + 1.0) ) then
        write(unit_error,*) ' '
        write(unit_error,*) ' *** warning(SetEta)'
        write(unit_error,*) ' Channel =',Branch,' X = ',X
        write(unit_error,*) ' ',H,' > maximum tabulated depth,'
        write(unit_error,*) ' (the line connecting ',Depth(L1), &
            ' and ',Depth(L2),') ...'
    end if

200 continue

    if(I/=1) then
        NP(1) = L1-1
        NP(2) = L2-1
        NP(3) = L2
        NP(4) = L1
        dH =      XiM*(Depth(NP(4))-Depth(NP(1))) &
            +Xi*(Depth(NP(3))-Depth(NP(2)))
        Eta = (H-XiM*Depth(NP(1))-Xi*Depth(NP(2)))/dH

        if(Eta<-1.0e-06) then
            write(unit_error,*) ' '
            write(unit_error,*) 'SetEta...'
            write(unit_error,*) 'eta = ',Eta
            write(unit_error,*) 'depths = '
            write(*,'(4F12.2)') (Depth(NP(I)),I=1,4)
            write(unit_error,*) 'dH = ',dH,'  Xi = ',Xi
        end if

    else
        write(unit_error,*) ' '
        write(unit_error,*) '*** error(SetEta)'
        write(unit_error,*) ' Channel =',Branch,' X = ',X
        write(unit_error,*) ' ',H, '< minimum depth,'
        write(unit_error,*) ' (the average of ',Depth(Offset(N1)),' and ', &
            Depth(Offset(N2)),') ...'

        return
    end if

    SetEta = .true.

    return
end function

!=======================================================================
!   Private: BiLinearShapeFunction
!=======================================================================

logical function BiLinearShapeFunction()

    implicit none

    !   Purpose:
    !     Compute bi-linear shape functions in local coordinates.

    !   Arguments:

    !   Argument definitions:

    !   Local Variables:

    !   Routines by module:

    !**** Local:
    real*8 fact,sum
    integer i

    !   Intrinsics:

    !   Programmed by: Lew DeLong
    !   Date:          July  1991
    !   Modified by:
    !   Last modified:
    !   Version 93.01, January, 1993

    !-----Implementation -----------------------------------------------------

    Shape(1)    = (1.0-Xi)*(1.0-Eta)
    dShapeDX(1) = -1.0+Xi
    Shape(2)    = Xi*(1.0-Eta)
    dShapeDX(2) = -Xi

    if(DegreesOfFreedom==4) then
        Shape(3)    = Xi*Eta
        dShapeDX(3) = Xi
        Shape(4)    = (1.0-Xi)*Eta
        dShapeDX(4) = 1.0-Xi
        sum=Shape(1)+Shape(2)+Shape(3)+Shape(4)
        if(abs(sum-1.)>1.e-8) then
            fact=(1.-sum)/4.
            do i=1,4
                Shape(i)=Shape(i)+fact
            enddo
        endif
    end if

    BiLinearShapeFunction = .true.

    return
end function

!=======================================================================
!   Private: ReadCxProperties
!=======================================================================

logical function ReadCxProperties &
    ( INUNIT,M,NPRT, &
    DUM,LNUM,USR, &
    ICARD         )
    use IO_Units
    implicit none

    !   Purpose:
    !     Read hydraulic properties and geometry for 1 cross section.

    !   Arguments:
    integer INUNIT, USR, LNUM, M, NPRT
    character*80 DUM
    character*2 ICARD

    !   Argument definitions:
    !     INUNIT - FORTRAN unit number
    !     M      - current branch number
    !     NPRT   - screen output index
    !              [0] no screen output
    !              [1] write to screen
    !     DUM    - a line read from properties input file
    !     LNUM   - current line number
    !     USR    - current cross-section number
    !     ICARD  - record index
    !              [CH] branch header
    !              [HY] cross-section header
    !              [DP] properties record

    !   Local Variables:
    integer I, J, NPT, BRN
    real*8    R(8), R23, R53
    parameter (R23 = 2.0/3.0, R53 = 5.0/3.0)
    logical FIRST, FirstLine

    !   Variable definitions:
    !     R      - hydraulic properties of channel
    !              [1] depth
    !              [2] area
    !              [3] conveyance
    !              [4] beta
    !              [5] area-weighted sinuosity
    !              [6] conveyance-weighted sinuosity
    !              [7] width
    !              [8] wetted perimeter

    !   Routines by module:

    !**** Local:

    !   Intrinsics:

    !   Programmed by: Lew DeLong
    !   Date:          July  1991
    !   Modified by:
    !   Last modified:
    !   Version 93.01, January, 1993

    !-----Implementation -----------------------------------------------------

    ReadCxProperties = .false.

    NPT=0
    read(DUM,'(A2)') ICARD

    if(ICARD=='CH') then
        read(DUM,'(2X,I10)') BRN
        FIRST = .true.

        if(BRN/=M) then
            write(unit_error,*) 'Branch numbers are not in correct sequence.'
            write(unit_error,*) BRN,' changed to ',M,'....'
            BRN = M
        end if

        if(NPRT > 0 ) then
            write(unit_error,*) ' '
            write(unit_error,*) ' '
            write(unit_error,*) 'Currently reading...'
            write(*,'(A80)') DUM
        end if
        FirstTable(BRN) = USR+1
        read(INUNIT,'(80A)',end=50) DUM
        read(DUM,'(A2)') ICARD
        go to 60
50  continue

    write(unit_error,*)'***error(ReadCxProperties) unexpected end of file.'
    write(unit_error,*) 'Branch',BRN
    ICARD = 'EN'

60 continue
   end if

   if(ICARD=='HY') then
       USR = USR+1

       FirstLine = .true.

       if(USR>MaxTables) then
           write(unit_error,*) '***error (ReadCxProperties)'
           write(unit_error,*) 'Maximum number of tables (', &
               MaxTables,') exceeded.'
           write(unit_error,*) 'Returning...'
       end if

       read(DUM,'(3X,A16,1X,F10.0,1X,F10.0)') &
           ID(USR),XDistance(USR),Datum(USR)

       if(NPRT>0) then
           write(*,'(I10,F16.2)') USR,XDistance(USR)
       end if

       Offset(USR) = LNUM+1

       if((LNUM+Lines(BRN)) > MaxLines) then
           write(unit_error,*) '***error (ReadCxProperties)'
           write(unit_error,*) 'Maximum number of lines exceeded...'
           write(unit_error,*) 'Cross section ',USR
           write(unit_error,*) 'Current line number = ',LNUM
           write(unit_error,*) 'Returning...'
           return
       end if

   else
       write(unit_error,*) '***ERROR (ReadCxProperties) expecting HY record.'
       write(unit_error,*) 'Returning...'
       ReadCxProperties = .false.
       return
   end if

   do 100 I =1,Lines(BRN)+1
       read(INUNIT,'(80A)',end=102) DUM
       read(DUM,'(A2)') ICARD

       if(ICARD/='DP') then

           if(FIRST) then
               FIRST = .false.
               Lines(BRN) = NPT
           else

               if(Lines(BRN)>MaxLinesPerTable) then
                   write(unit_error,*) '***error (ReadCxProperties)'
                   write(unit_error,*) 'Lines per table exceeded.'
                   write(unit_error,*) 'Current = ',Lines(BRN), &
                       ', MaxLinesPerTable = ',MaxLinesPerTable
                   write(unit_error,*) 'Returning...'
                   return
               end if

           end if
           go to 102

       else if(ICARD=='DP') then
           LNUM = LNUM+1
           NPT = NPT+1

           if(NPT<=Lines(BRN)) then
               read(DUM, &
                   '(3X,F10.0,2(1X,E13.6),  3(1X,F5.0),   2(1X,F7.0))') &
                   (R(J),J=1,8)

               if(NPRT>1) then
                   write(*, &
                       '(3X,F10.2,2(1X,E13.6),  3(1X,F5.2),   2(1X,F7.2))') &
                       (R(J),J=1,8)
               end if

               !--------------Assign hydraulic properties.
               Depth(LNUM) = R(1)
               A(LNUM)     = R(2)
               K(LNUM)     = R(3)
               Bta(LNUM)   = R(4)
               MA(LNUM)    = R(5)
               MQ(LNUM)    = R(6)
               Width(LNUM) = R(7)
               P(LNUM)     = R(8)

               !--------------Check for adverse conveyance gradient with depth.
               if( .not. FirstLine ) then

                   if( K(LNUM) <= K(LNUM-1) ) then
                       write(unit_error,*) ' ***error (ReadCxProperties) adverse ', &
                           'conveyance gradient,'
                       write(unit_error,*) ' Channel...',M, ' Cross section...',ID(USR), &
                           ' conveyance...',K(LNUM)
                   end if

               else
                   FirstLine = .false.
               end if

               !--------------Compute one over effective n.
               if(A(LNUM)>0.1) then
                   N(LNUM) = (P(LNUM)**R23*K(LNUM))/(1.486*A(LNUM)**R53)
               else
                   N(LNUM) = 0.0
               end if

           else
               write(unit_error,*) 'Too many lines...'
               write(unit_error,*) 'I = ',I,'  Lines = ',Lines(BRN)
           end if

       end if

100 continue
    write(unit_error,*) '***ERROR (ReadCxProperties)'
    write(unit_error,*) &
        'Maximum number of lines per table (',Lines(BRN),') exceeded.'
    write(unit_error,*) 'Last line read...'
    write(*,'(A80)') DUM
    write(unit_error,*) 'I = ',I,' NPT = ',NPT,'LNUM = ',LNUM
    write(unit_error,*) 'Returning ...'
    ReadCxProperties = .false.
    return
102 continue

    if(ICARD=='DP') then
        ICARD = 'EN'
    end if

    LastTable(BRN) = USR
    if(NPT==Lines(BRN)) then

        !--------The following statements correct for zero values of
        !         momentum & sinuosity coefficients when the first
        !         value in the input table is at zero area.

        if(Bta(Offset(USR))<1.0e-06) &
            Bta(Offset(USR)) = Bta(Offset(USR)+1)
        if(MA(Offset(USR))<1.0e-06) &
            MA(Offset(USR)) = MA(Offset(USR)+1)
        if(MQ(Offset(USR))<1.0e-06) &
            MQ(Offset(USR)) = MQ(Offset(USR)+1)
        if(N(Offset(USR))<1.0e-06) &
            N(Offset(USR)) = N(Offset(USR)+1)

        ReadCxProperties = .true.

    else if(NPT<Lines(BRN)) then
        write(unit_error,*) '***ERROR (ReadCxProperties)'
        write(unit_error,*) 'Fewer than expected (',Lines(BRN), &
            ') lines per cross section.'
    end if

    return
end function

!=======================================================================
!   Private: Read1stCxLine
!=======================================================================

logical function Read1stCxLine &
    ( &
    NPRT,DUM)
    use IO_Units
    use fileutil, only: GetFileUnit, GetFileName, &
     OpenOldText


    implicit none

    !   Purpose:
    !     Open a flat file and read an 80-character record.

    !   Arguments:
    integer NPRT
    character*80  DUM

    !   Argument definitions:
    !     NPRT - index for output,
    !            [0] no output,
    !            [1] echo input.
    !     DUM  - an 80-character record read from the file INAME.


    !   Module data:

    !   Local Variables:
    integer INUNIT
    character*12 INNAME, InternalFileName

    !   Intrinsics:

    !   Programmed by: Lew DeLong
    !   Date:          July  1991
    !   Modified by:
    !   Last modified:
    !   Version 93.01, January, 1993

    !-----Implementation -----------------------------------------------------

    InternalFileName = 'cxgeom.dat'

    INUNIT = GetFileUnit(InternalFileName)
    INNAME = GetFileName(InternalFileName)

    if(OpenOldText(INUNIT,INNAME)) then
        read(INUNIT,'(A80)') DUM
        read(DUM,*)     NPRT
        read(INUNIT,'(80A)') DUM
        Read1stCxLine = .true.
    else
        write(unit_error,*) '***ERROR (Read1stCxLine) file not opened.'
        write(*,'(A)') INNAME
        write(unit_error,*) 'Returning...'
        Read1stCxLine = .false.
    end if

    return
end function

!=======================================================================
!   Private: RiverFtToDownStream
!=======================================================================

logical function RiverFtToDownStream(M)
    use IO_Units
    implicit none

    !   Purpose:
    !     Convert river feet to downstream distance.

    !   Arguments:
    integer M

    !   Argument definitions:
    !     M - branch number.

    !   Local Variables:
    integer I, J

    !   Routines by module:

    !**** Local:

    !   Intrinsics:

    !   Programmed by: Lew DeLong
    !   Date:          July  1991
    !   Modified by:
    !   Last modified:
    !   Version 93.01, January, 1993

    !-----Implementation -----------------------------------------------------

    if( (XDistance(LastTable(M))-XDistance(FirstTable(M))) &
        < 1.0e-06 ) then

        do 100 I=FirstTable(M),LastTable(M)
            XDistance(I) = -XDistance(I)
100     continue

    end if

    do 200 I=FirstTable(M)+1,LastTable(M)
        if( (XDistance(I)-XDistance(I-1)) < 1.0e-06 ) then
            write(unit_error,*) '***ERROR (RiverFtToDownStream)'
            write(unit_error,*) 'X coordinates out of order...'
            write(unit_error,*) 'Branch',Branch
            write(*,'(I5,F15.2)') &
                (J,XDistance(J),J=FirstTable(M),LastTable(M))
            RiverFtToDownStream = .false.
            return
        end if

200 continue

    RiverFtToDownStream = .true.

    return
end function

!======================================================================
!   Public: AdjustChannelWidths
!=======================================================================

logical function AdjustChannelWidths()
    use IO_Units
    implicit none

    !   Purpose:
    !     Compute and tabulate cross-section widths consistent with
    !     cross-sectional tabulated areas.
    !

    !   Arguments:

    !   Argument definitions:

    !   Local Variables:
    integer I, II, ONE, TWO, M, END, CxCount, LineCount
    integer J
    real*8    dB( MaxLinesPerTable )

    !   Routines by module:

    !**** Local:

    !   Intrinsics:
    integer   INT
    intrinsic INT

    !   Programmed by: Lew DeLong
    !   Date:          Jan   1994

    !-----Implementation -----------------------------------------------------

    AdjustChannelWidths = .false.
    CxCount = 0
    LineCount = 0

    do 300 M=1,NumCh
        do 200 II=FirstTable(M),LastTable(M)

            CxCount = CxCount + 1

            !-----------Compute change in bottom width.
            ONE = Offset(II)
            TWO = ONE+1
            dB(1) = ( A(TWO) - A(ONE) &
                - .5*(Width(ONE)+Width(TWO))*(Depth(TWO)-Depth(ONE)) &
                ) / ( 2.0*(Depth(TWO)-Depth(ONE)) )

            !-----------Check for negative widths and reset if necessary.

            if( Width(ONE)+dB(1) > 0.0 ) then
            else

                dB(1) = 0.0

            end if

            !-----------Equate wetted perimeter, at the channel bottom, to bottom width.
            P(ONE) = Width(ONE) + dB(1)

            !-----------Equate values of sinuosity and beta at the bottom of the channel
            !         to those at the top of the lowest trapezoid.
            Bta(ONE) = Bta(TWO)
            MA(ONE)  = MA(TWO)
            MQ(ONE)  = MQ(TWO)

            !-----------Compute change in widths at the top of second and higher trapezoids.
            if(Lines(M)>=3) then
                END   = ONE+Lines(M)-2
                J = 1
                do 100 I=TWO,END

                    J = J + 1
                    dB(J) = (A(I+1)-A(I-1))/(Depth(I+1)-Depth(I-1)) &
                        -.25*(Width(I-1)+2.0*Width(I)+Width(I+1))

100             continue
            end if

            J = J + 1
            I = END + 1
            dB(J) = ( A(I) - A(I-1) &
                - .5*(Width(I)+Width(I-1))*(Depth(I)-Depth(I-1)) &
                ) / ( 2.0*(Depth(I)-Depth(I-1)) )

            J = 0
            do 110 I=ONE,END+1
                J = J + 1
                Width(I) = Width(I) + dB(J)
110         continue

            if( Print ) then
                write(unit_error,*) ' '
                write(unit_error,*) ' Cross section...',ID(CxCount)
                write(unit_error,*) '  Depth     Adjusted Width 1/Effective n'
                do 150 I=1,Lines(M)
                    LineCount = LineCount + 1
                    write(*,'(F6.2,6X,F10.2,4X,F6.3)') &
                        Depth(LineCount), Width(LineCount), N(LineCount)
150             continue
            end if

200     continue
300 continue

    AdjustChannelWidths = .true.

    return
end function



!== Public (InitChnlBtmHermites) ================================================

logical function InitChnlBtmHermites()
    use channel_schematic
    implicit none

    !   Purpose:  Initialize values necessary for interpolating
    !             channel bottom elevation and slope using
    !             hermite cubics.

    !   Arguments:

    !   Argument definitions:

    !   Local Variables:
    integer I, J, M
    integer Points, FirstPoint

    !   Routines by module:

    !**** Local:
    !   Intrinsics:

    !   Programmed by: Lew DeLong
    !   Date:          Oct   1993
    !   Modified by:
    !   Last modified:

    !-----Implementation -----------------------------------------------------

    InitChnlBtmHermites = .false.

    !---- Set known values.

    J = -1
    do 100 I=1,MaxTables
        J = J + 2
        XHerm(J) = XDistance(I)
        ZHerm(J) = Datum(I)
100 continue

    !---- Estimate gradients.

    do 500 M=1,NumberOfChannels()

        Points = 2 * (LastTable(M) - FirstTable(M) + 1)
        FirstPoint = 2 * FirstTable(M) - 1
        call HMMM( Points, XHerm(FirstPoint) )
        !       write(*,'(I4,2F12.4)') (I,XHerm(I),XHerm(I+1),I=1,Points,2)
        call HMMM( Points, ZHerm(FirstPoint) )
        !       write(*,'(I4,2F12.4)') (I,ZHerm(I),ZHerm(I+1),I=1,Points,2)
        call HGRAD( Points,XHerm(FirstPoint),ZHerm(FirstPoint) )
    !       write(*,'(I4,2F12.4)') (I,XHerm(I),XHerm(I+1),I=1,Points,2)

500 continue

    !     write(*,'(I4,4F12.4)')
    !    #     ( I,XHerm(I),XHerm(I+1),ZHerm(I),ZHerm(I+1),
    !    #      I=1,Points,2 )

    InitChnlBtmHermites = .true.

    return
end function

!== Public (HermBtmElev) ================================================

real*8 function HermBtmElev()

    implicit none

    !   Purpose:  Compute bottom elevation using Hermite interpolation.

    !   Arguments:

    !   Argument definitions:
    !   Local Variables:
    integer m
    real*8    dX

    !   Routines by module:

    !**** Local:

    !   Intrinsics:

    !   Programmed by: Lew DeLong
    !   Date:          Oct   1993
    !   Modified by:
    !   Last modified:

    !-----Implementation -----------------------------------------------------

    dX = XDistance(N2) - XDistance(N1)
    m = 2 * N1 - 1

    HermBtmElev = &
        (Xi-1.0)**2 * (2.0*Xi+1.0) * ZHerm(m  ) &
        +  Xi * (Xi-1.0)**2 * dX     * ZHerm(m+1) &
        -  Xi**2 * (2.0*Xi-3.0)      * ZHerm(m+2) &
        +  Xi**2 * (Xi-1.0) * dX     * ZHerm(m+3)

    return
end function



subroutine   HMMM &
    (NPT, &
    HDATA)
    implicit none

    !     + + + PURPOSE + + +

    !     Compute mean incremental changes in a sequence of numbers.
    !     The numbers are input as the odd elements of HDATA(). Mean
    !     incremental changes are computed and stored as even elements.

    !     + + + DUMMY ARGUMENTS + + +
    integer   NPT
    real*8   HDATA(NPT)

    !     + + + ARGUMENT DEFINITIONS + + +
    !     NPT    - total number of elements in HDATA()
    !     HDATA  - floating point array, odd elements of which
    !----are the sequence of numbers from which mean incremental
    !----changes will be computed

    !     + + + LOCAL VARIABLES + + +
    integer   I,MPT
    real*8   D,E,XP,XM,TEST

    !     + + + INTRINSICS + + +
    intrinsic ABS

    !     + + + END SPECIFICATIONS + + +

    HDATA(2)=HDATA(3)-HDATA(1)
    XM=HDATA(2)
    MPT=NPT-2
    if(MPT>3) then
        do 100 I=3,MPT,2
            XP=HDATA(I+2)-HDATA(I)
            D=XP+XM
            E=XP*XM
            TEST=ABS(D)
            if(TEST>1.0e-30.and.E>0.0) then
                HDATA(I+1)=2.0*E/D
            else
                HDATA(I+1)=0.0
            end if
            XM=XP
100     continue
        HDATA(NPT)=XP
    else
        HDATA(4)=HDATA(2)
    end if

    return
end subroutine

subroutine  HGRAD &
    (NPT, &
    HX,HY)
    implicit none

    !     + + + PURPOSE + + +
    !     Compute an array of gradients from previously
    !     computed incremental changes. Mean incremental changes in the values
    !     stored in odd elements of the arrays are input in corresponding
    !     even elements of the arrays. Thus:
    !     .
    !----Gradient = dY/dX = HY(even)/HX(even),
    !     .
    !----and are stored as
    !     .
    !----HY(even) = gradient.

    !     + + + DUMMY ARGUMENTS + + +
    integer   NPT
    real*8   HX(NPT),HY(NPT)

    !     + + + ARGUMENT DEFINITIONS + + +
    !     NPT    - number of elements in HX() & HY() arrays
    !     HX     - array of floating-point values, even elements are mean
    !----mean incremental change in values stored in odd elements.
    !     HY     - array of floating-point values similar to HX()

    !     + + + LOCAL VARIABLES + + +
    integer   I

    !     + + + END SPECIFICATIONS + + +

    do 100 I=2,NPT,2
        HY(I)=HY(I)/HX(I)
100 continue

    return
end subroutine
subroutine find_layer_index( &
    X &
    ,Z &
    ,Branch &
    ,vsecno &
    ,virtelev &
    ,veindex &
    )
    use IO_Units
    use common_xsect
    use runtime_data
    implicit none


    integer &
        Branch &              ! hydro channel number &
        ,virtelev &           ! virtual elevation number (within channel) &
        ,vsecno &             ! virtual xsect number (within channel) &
        ,veindex &            ! virtual elevation index &
        ,previous_elev_index(max_virt_xsects) ! used to store elevation index
    real*8 &
        X  &                  ! distance along channel (from FourPt) &
        ,Z                   ! changed to Elevation based

    save previous_elev_index

    data previous_elev_index /max_virt_xsects * 1/

    !----find the index of elevation of layer that is below H, and the
    !----virtual xsect number
    !----Check for negative depth
    vsecno = nint(X / virt_deltax(Branch))+1


    if (Z<virt_min_elev(minelev_index(Branch)+vsecno-1)) then
        write(unit_error,910) chan_geom(Branch)%chan_no,current_date,Z
910     format(' Error...channel', i4,' dried up at time or runtime instability developed',a,'; WS Elevation Z=',f10.3)
        call exit(13)
    endif


    virtelev=previous_elev_index(minelev_index(Branch)+vsecno-1)

    !----if upper level is below or at same elevation as H, move up
    do while (virtelev < num_layers(Branch) .and. &
        virt_elevation(elev_index(Branch)+virtelev) <= Z)
        virtelev=virtelev+1
    enddo
    !----if lower level is above H, move down
    do while (virtelev > 1 .and. &
        virt_elevation(elev_index(Branch)+virtelev-1) > Z)
        virtelev=virtelev-1
    enddo



    previous_elev_index(minelev_index(Branch)+vsecno-1) = virtelev
    veindex=elev_index(Branch)+virtelev-1
    if (Z > virt_elevation(elev_index(Branch)+num_layers(Branch)-1)) then
        write(unit_error,*) 'Error in find_layer_index'
        write(unit_error,610) chan_geom(Branch)%chan_no, &
            virt_elevation(elev_index(Branch)+num_layers(Branch)-1),Z
610     format('Top elevation in cross-section is too low or a runtime ' &
            /'instability developed (which may have to do with other inputs.' &
            /'If this is really a problem with the maximum xsect elevation,' &
            /'change variable ''max_layer_height'' in common_irreg_geom.f.' &
            /'Chan no. ',i3,' Chan top elev=',f6.2,' Z=',f6.2)
        call exit(2)
    endif

    return
end subroutine

!===== EOF chcxtbl ======================================================

end module
!
!     MaxTables        - maximum number of tables.
!     MaxLinesPerTable - maximum number of lines per table.
!     MaxLines         - maximum total number of lines in tables.
!
!     FirstTable(m) - table number of first table for a branch.
!     LastTable(m)  - table number of last table of a branch.
!     Lines(m) - number of lines per table for the curren branch.
!     m - branch number.
!
!     Offset(i) - pointer to first line of a table in properties arrays.
!     XDistance(i) - downstream-distance coordinate.
!     Datum(i) - elevation of bottom of the channel.
!     i - table number, same as user cross-section sequence number.
!
!     Depth(j) - depth of flow.
!     Width(j) - width of channel.
!     A(j)     - cross-sectional area.
!     K(j)     - sinuosity-weighted conveyance.
!     P(j)     - wetted perimeter.
!     N(j)     - one over effective Manning's "n".
!     Bta(j)   - momentum coefficient.
!     MA(j)    - area-weighted sinuosity.
!     MQ(j)    - discharge-weighted sinuosity.
!     j - line number.
!
!     N1 - adjacent upstream cross section.
!     N2 - adjacent downstream cross section.
!
!     NP(k)       - pointers to table positions in adjacent upstream and downstream
!                   property tables bracketing the estimation point.  Order is
!                   counter clockwise viewing the stream profile with upstream
!                   cross section on the left, downstream cross section on the
!                   right, and beginning with the lower of two bracketing points
!                   in the upstream cross section.
!     Shape(k)    - local bilinear shape function for the estimation point.
!     dShapeDX(k) - derivative of Shape with respect to the local depth coordinate.
!     k = degree of freedom.

!     DegreesOfFreedom - maximum number of degrees of freedom,
!                        four for bilinear interpolation.
!
!     Xi - stream-wise local coordinate of the estimation point.
!     Eta - depth-wise local coordinate of the estimation point.
!     dH - global depth corresponding to a local depth of 1 at the
!          estimation point.
!
!     PreviousBranch - branch of previous estimation point.
!     PreviousX      - int( X location ) at previous estimation point.
!     PreviousH100   - int( 100.0*depth ) at previous estimation point.

!     WriteIntrp - index, if .TRUE., write interpolated properties
!                  tables at computational locations.
!     HermiteBtm - index, if .TRUE., interpolate bottom elevation
!                  using hermites.

!===== BOF chcxtbl.inc ================================================
