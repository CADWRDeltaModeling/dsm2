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

!==== BOF linear =======================================================
module linear
    implicit none
contains
    !== Public (Linear1D) ==================================================

    subroutine Linear1D( &
        NumberOfEstimates, X, NumberOfKnown, KnownX, KnownY, &
        Y &
        )

        implicit none

        !   Purpose: Estimate values of an array of dependent variables
        !            corresponding to an array of independent variables base
        !            upon a pair of given dependent and independent arrays.
        !            Both dependent-variable arrays must be monotonically
        !            increasing.

        !   Arguments:
        integer NumberOfEstimates, NumberOfKnown
        real*8    X(NumberOfEstimates), Y(NumberOfEstimates)
        real*8    KnownX(NumberOfKnown), KnownY(NumberOfKnown)

        !   Argument definitions:
        !    NumberOfEstimates - requested number of estimates.
        !    NumberOfKnown - number of known paired data points.
        !    KnownX(i) - known value of X at point i.
        !    KnownY(i) - known value of Y at point i.
        !    X(j) - known value of X at point j.
        !    Y(j) - requested value of Y at point j.

        !   Local variables:
        integer I, CurrentPointer
        integer One
        parameter ( One = 1 )

        !   Programmed by: Lew DeLong
        !   Date:          January 1991
        !   Modified by:   Lew DeLong
        !   Last modified: December 1992
        !   Version 93.01, January, 1993

        !-----Implementation ---------------------------------------------------

        CurrentPointer = 1
        do 100 I = 1,NumberOfEstimates

            if( CurrentPointer < NumberOfKnown ) then

                CurrentPointer = LinearPointer1D( &
                    X(I), NumberOfKnown, KnownX, CurrentPointer &
                    )
            end if

            if( CurrentPointer > 0 &
                .and. &
                CurrentPointer < NumberOfKnown ) then

                !-----------Interpolation within range of known values.
                Y(I) = LinearInterp1D( &
                    X(I), KnownX(CurrentPointer), KnownY(CurrentPointer) &
                    )

            else if( CurrentPointer == 0 ) then

                !-----------Extrapolation, less than minimum known.
                Y(I) = LinearInterp1D( &
                    X(I), KnownX( One ), KnownY( One ) &
                    )
                CurrentPointer = 1

            else if( CurrentPointer >= NumberOfKnown ) then

                !-----------Extrapolation, greater than maximum known.
                Y(I) = LinearInterp1D( &
                    X(I), KnownX(NumberOfKnown-1), KnownY(NumberOfKnown-1) &
                    )

            end if

100     continue

        return
    end subroutine

    !== Public (LinearPointer1D) ===========================================

    integer function LinearPointer1D( &
        X, NumberOfKnown, KnownX, CurrentPointer &
        )

        implicit none

        !   Purpose:  Return the subscript of the value in the KnownX array
        !             closest to but less than X.  If X is less than all values
        !             of KnownX, a "0" is returned.  The search begins at the
        !             subscript CurrentPointer.  A"-1" is returned if an error
        !             is encountered.

        !   Arguments:
        integer NumberOfKnown, CurrentPointer
        real*8    X, KnownX(NumberOfKnown)

        !   Argument definitions:
        !    NumberOfKnown - number of known values of X.
        !    KnownX(i) - known value of X at point i.
        !    X - value of X for which a pointer is requested.
        !    CurrentPointer - current or starting value of pointer.

        !   Local Variables:
        integer I, J

        !   Routines by module:

        !   Programmed by: Lew DeLong
        !   Date:          January 1991
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation ---------------------------------------------------

        if( X >= KnownX( CurrentPointer ) ) then

            do 100 I=CurrentPointer+1, NumberOfKnown

                J = I
                if( KnownX(J) > X ) go to 102

100         continue

            !--------X is greater than all KnownX.
            LinearPointer1D = NumberOfKnown
            return

102     continue
        LinearPointer1D = J - 1

    else

        if( CurrentPointer == 1 ) then

            !-----------X is less than all values of KnownX.
            LinearPointer1D = 0

        else if( CurrentPointer > 1 ) then

            do 200 I=CurrentPointer-1, 1

                J = I
                if( X > KnownX(J) ) go to 202

200         continue

            !-----------X is less than all values of KnownX.
            LinearPointer1D = 0
            return

202     continue

        LinearPointer1D = J

    else

        !-----------Shouldn't get here!
        LinearPointer1D = -1

    end if

end if

return
end function

!== Public (LinearInterp1D) ============================================

real*8 function LinearInterp1D( &
    X, KnownX, KnownY &
    )
    use IO_Units
    implicit none

    !   Purpose:  Linearly interpolates or extrapolates value of a dependent
    !             variable based upon the value of a paired independent variable,
    !             given adjacent paired values of dependent and independent
    !             variables.

    !   Arguments:
    real*8 X, KnownX(2), KnownY(2)

    !   Argument definitions:
    !    KnownX(2) - known value of X at points adjacent to X.
    !    KnownY(2) - known value of Y corresponding to KnownX().
    !    X - a value of X for which a value of Y is requested.

    !   Module data:


    !   Local Variables:
    real*8 Small, dX
    parameter (Small = 1.0e-20)

    !   Routines by module:

    !   Intrinsics:
    real*8      ABS
    intrinsic ABS

    !   Programmed by: Lew DeLong
    !   Date:          January 1991
    !   Modified by:
    !   Last modified:
    !   Version 93.01, January, 1993

    !-----Implementation -----------------------------------------------------

    dX = KnownX(2) - KnownX(1)
    if ( ABS(dX) > Small ) then

        LinearInterp1D = KnownY(1) + &
            ( KnownY(2) - KnownY(1) ) * ( X - KnownX(1) ) / dX

    else if( ABS(X - KnownX(2)) < Small ) then
        LinearInterp1D = KnownX(2)
    else if( ABS(X - KnownX(1)) < Small ) then
        LinearInterp1D = KnownX(1)
    else

        !--------Shouldn't get here!
        write(UNIT_ERROR,*) ' ***Error (LinearInterp1D)...'
        write(UNIT_ERROR,*) ' Out of range ... ',X
        write(UNIT_ERROR,*) KnownX(1), KnownX(2)
        call EXIT(1)

    end if

    return
end function

!== Private (XINSRT) ===================================================

logical function XINSRT &
    ( MTB, UCXPB, USRX, DX, KEEP, MODEL, &
    CMPCX, XORD)

    implicit none

    !   Purpose:  This subprogram interpolates the location of cross sections
    !             necessary to maintain the distance between computational cross
    !             sections at intervals no greater than DX.  If the dummy argument
    !             KEEP is .FALSE., then x-coordinates are interpolated equally
    !             between the end sections.  Otherwise, each subreach is compared
    !             against DX and x-coordinate are interpolated for those reaches
    !             exceeding DX in length.  For such subreaches, x-coordinates are
    !             set equidistant between subreach end sections.

    !   Arguments:
    integer   MTB, UCXPB, MODEL, CMPCX
    real*8      DX, USRX(UCXPB), XORD(MTB)
    logical   KEEP

    !   Argument definitions:
    !     MTB    - Maximum number of XORD values.
    !     UCXPB  - Number of user cross sections for this branch.
    !     USRX   - Vector of x-coordinates giving the location of the user's
    !              sections.
    !     DX     - Maximum distance between computational cross sections.
    !     KEEP   - If .TRUE., then retain the user's cross sections for
    !                computational purposes.
    !              If .FALSE., then space computational sections equally throughout
    !                branch, ignoring the user's sections (for computations).
    !     MODEL  - Model for which output is to be generated:
    !              [1] Branch (no intermediate computational nodes).
    !              [2] OSW Training Model (one intermediate comp. node).
    !              [3] Hydraux (two intermediate comp. nodes).
    !     CMPCX  - Number of computational cross-sections.
    !     XORD   - Vector of x-coordinates of computational cross-sections and
    !                any intermediate nodes.

    !   Module data:

    !   Local variables:
    integer   I, J, REMMTB
    real*8      LENGTH
    logical   OK

    !   Routines by module:
    !   Programmed by:   David Thompson
    !   Date programmed: March, 1990
    !   Modified by:     Lew DeLong
    !   Last modified:   October, 1991
    !   Version 93.01, January, 1993

    !-----Implementation ------------------------------------------------------------

    !-----Case 1: Only two sections.
    if (UCXPB == 2) then
        LENGTH = USRX(UCXPB) - USRX(1)
        OK     = SUBDVD(LENGTH, DX, MTB, USRX(1), USRX(2), &
            CMPCX, XORD)
    !--------Case 2: Multiple sections, but user sections are not
    !       to be computational sections.
    else if (UCXPB > 2 .and. .not. KEEP) then
        LENGTH = USRX(UCXPB) - USRX(1)
        OK     = SUBDVD(LENGTH, DX, MTB, USRX(1), USRX(UCXPB), &
            CMPCX, XORD)
    !--------Case 3: Multiple sections and user sections are to be used as
    !       computational sections.
    else if (UCXPB > 2 .and. KEEP) then
        J = 1
        CMPCX = 0
        do 100 I = 1, UCXPB-1
            LENGTH = USRX(I+1) - USRX(I)
            REMMTB = MTB - CMPCX
            OK     = SUBDVD(LENGTH, DX, REMMTB, USRX(I), USRX(I+1), &
                CMPCX, XORD(J))
            !-----------This one is a bit sticky!  On first call to SUBDVD, CMPCX will include
            !           the first section in the branch.  On subsequent calls, the first
            !           section in the subreach will be counted.  Therefore, set J to CMPCX
            !           on the first call and then add one less than the count on subsequent
            !           calls.
            if (I == 1 ) then
                J = CMPCX
            else
                J = CMPCX + J - 1
            end if
100     continue
        !--------Now, set CMPCX to the final value.
        CMPCX = J
    end if

    !-----If a problem occurred (MTB exceeded), set XINSRT to false.
    if (OK) then
        XINSRT = .true.
    else
        XINSRT = .false.
    endif
    !
    return
end function

!== Private (SUBDVD) ===================================================

logical function SUBDVD &
    ( LENGTH, DX, REMMTB, USRX1, USRX2, &
    CMPCX, XORD)

    implicit none

    !   Purpose:
    !       This subprogram inserts computational cross sections between
    !       USRX1 and USRX2 if needed to conform to the maximum computational
    !       length of DX.

    !   Arguments:
    integer  CMPCX, REMMTB
    real*8     LENGTH, DX, XORD(REMMTB), USRX1, USRX2

    !   Argument definitions:
    !     LENGTH - Length of interval to be subdivided.
    !     DX     - Maximum distance between computational cross sections.
    !     REMMTB - Number of remaining entries in XORD.
    !     USRX1  - downstream distance to first user cross-section.
    !     USRX2  - downstream distance to second user cross-section.
    !     CMPCX  - Number of computational cross sections.
    !     XORD   - Vector of interpolated downstream distances.

    !   Module data:

    !   Local Variables:
    integer   I, NUMCDX
    real*8      CMPDX

    !   Routines by module:

    !**** Local:


    !       Programmed by:    David Thompson
    !       Date programmed:  March, 1990
    !       Modified by:      Lew DeLong
    !       Last modified:    December,  1990
    !   Version 93.01, January, 1993
    !-----Implementation ------------------------------------------------------------

    if (LENGTH <= DX) then

        CMPCX = 2
        if (CMPCX <= REMMTB) then
            XORD(1) = USRX1
            XORD(2) = USRX2
            SUBDVD  = .true.
        else
            SUBDVD = .false.
        end if

    else if (LENGTH > DX) then

        NUMCDX  = INT(LENGTH / DX)
        CMPDX   = LENGTH / REAL(NUMCDX)
        CMPCX   = NUMCDX + 1

        if (CMPCX <= REMMTB) then
            do 10 I = 2, CMPCX-1
                XORD(I) = USRX1 + REAL(I-1) * CMPDX
10          continue

            !-----------Set the ends.
            XORD(1) = USRX1
            XORD(CMPCX) = USRX2
            SUBDVD  = .true.
        end if

    end if

    return
end function

!=== EOF linear ===============================================================
end module
