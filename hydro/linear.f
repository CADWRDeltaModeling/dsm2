C!    Copyright (C) 1996, 1997, 1998 State of California,
C!    Department of Water Resources.
C!
C!    Delta Simulation Model 2 (DSM2): A River, Estuary, and Land
C!    numerical model.  No protection claimed in original FOURPT and
C!    Branched Lagrangian Transport Model (BLTM) code written by the
C!    United States Geological Survey.  Protection claimed in the
C!    routines and files listed in the accompanying file "Protect.txt".
C!    If you did not receive a copy of this file contact Dr. Paul
C!    Hutton, below.
C!
C!    This program is licensed to you under the terms of the GNU General
C!    Public License, version 2, as published by the Free Software
C!    Foundation.
C!
C!    You should have received a copy of the GNU General Public License
C!    along with this program; if not, contact Dr. Paul Hutton, below,
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
C!    Dr. Paul Hutton
C!    California Dept. of Water Resources
C!    Division of Planning, Delta Modeling Section
C!    1416 Ninth Street
C!    Sacramento, CA  95814
C!    916-653-5601
C!    hutton@water.ca.gov
C!
C!    or see our home page: http://wwwdelmod.water.ca.gov/

*==== BOF linear =======================================================

*== Public (Linear1D) ==================================================

      SUBROUTINE Linear1D(
     &     NumberOfEstimates, X, NumberOfKnown, KnownX, KnownY,
     &     Y
     &     )

      IMPLICIT NONE

*   Purpose: Estimate values of an array of dependent variables
*            corresponding to an array of independent variables base
*            upon a pair of given dependent and independent arrays.
*            Both dependent-variable arrays must be monotonically
*            increasing.

*   Arguments:
      INTEGER NumberOfEstimates, NumberOfKnown
      REAL*8    X(NumberOfEstimates), Y(NumberOfEstimates)
      REAL*8    KnownX(NumberOfKnown), KnownY(NumberOfKnown)

*   Argument definitions:
*    NumberOfEstimates - requested number of estimates.
*    NumberOfKnown - number of known paired data points.
*    KnownX(i) - known value of X at point i.
*    KnownY(i) - known value of Y at point i.
*    X(j) - known value of X at point j.
*    Y(j) - requested value of Y at point j.

*   Local variables:
      INTEGER I, CurrentPointer
      INTEGER One
      PARAMETER ( One = 1 )

*   Routines by module:

***** Local:
      INTEGER  LinearPointer1D
      EXTERNAL LinearPointer1D

      REAL*8     LinearInterp1D
      EXTERNAL LinearInterp1D

*   Programmed by: Lew DeLong
*   Date:          January 1991
*   Modified by:   Lew DeLong
*   Last modified: December 1992
*   Version 93.01, January, 1993

*-----Implementation ---------------------------------------------------

      CurrentPointer = 1
      DO 100 I = 1,NumberOfEstimates

         IF( CurrentPointer .LT. NumberOfKnown ) THEN

            CurrentPointer = LinearPointer1D(
     &           X(I), NumberOfKnown, KnownX, CurrentPointer
     &           )
         END IF

         IF( CurrentPointer .GT. 0
     &        .AND.
     &        CurrentPointer .LT. NumberOfKnown ) THEN

*-----------Interpolation within range of known values.
            Y(I) = LinearInterp1D(
     &           X(I), KnownX(CurrentPointer), KnownY(CurrentPointer)
     &           )

         ELSE IF( CurrentPointer .EQ. 0 ) THEN

*-----------Extrapolation, less than minimum known.
            Y(I) = LinearInterp1D(
     &           X(I), KnownX( One ), KnownY( One )
     &           )
            CurrentPointer = 1

         ELSE IF( CurrentPointer .GE. NumberOfKnown ) THEN

*-----------Extrapolation, greater than maximum known.
            Y(I) = LinearInterp1D(
     &           X(I), KnownX(NumberOfKnown-1), KnownY(NumberOfKnown-1)
     &           )

         END IF

 100  CONTINUE

      RETURN
      END

*== Public (LinearPointer1D) ===========================================

      INTEGER FUNCTION LinearPointer1D(
     &     X, NumberOfKnown, KnownX, CurrentPointer
     &     )

      IMPLICIT NONE

*   Purpose:  Return the subscript of the value in the KnownX array
*             closest to but less than X.  If X is less than all values
*             of KnownX, a "0" is returned.  The search begins at the
*             subscript CurrentPointer.  A"-1" is returned if an error
*             is encountered.

*   Arguments:
      INTEGER NumberOfKnown, CurrentPointer
      REAL*8    X, KnownX(NumberOfKnown)

*   Argument definitions:
*    NumberOfKnown - number of known values of X.
*    KnownX(i) - known value of X at point i.
*    X - value of X for which a pointer is requested.
*    CurrentPointer - current or starting value of pointer.

*   Local Variables:
      INTEGER I, J

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          January 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation ---------------------------------------------------

      IF( X .GE. KnownX( CurrentPointer ) ) THEN

         DO 100 I=CurrentPointer+1, NumberOfKnown

            J = I
            IF( KnownX(J) .GT. X ) GO TO 102

 100     CONTINUE

*--------X is greater than all KnownX.
         LinearPointer1D = NumberOfKnown
         RETURN

 102     CONTINUE
         LinearPointer1D = J - 1

      ELSE

         IF( CurrentPointer .EQ. 1 ) THEN

*-----------X is less than all values of KnownX.
            LinearPointer1D = 0

         ELSE IF( CurrentPointer .GT. 1 ) THEN

            DO 200 I=CurrentPointer-1, 1

               J = I
               IF( X .GT. KnownX(J) ) GO TO 202

 200        CONTINUE

*-----------X is less than all values of KnownX.
            LinearPointer1D = 0
            RETURN

 202        CONTINUE

            LinearPointer1D = J

         ELSE

*-----------Shouldn't get here!
            LinearPointer1D = -1

         END IF

      END IF

      RETURN
      END

*== Public (LinearInterp1D) ============================================

      REAL*8 FUNCTION LinearInterp1D(
     &     X, KnownX, KnownY
     &     )
      use IO_Units
      IMPLICIT NONE

*   Purpose:  Linearly interpolates or extrapolates value of a dependent
*             variable based upon the value of a paired independent variable,
*             given adjacent paired values of dependent and independent
*             variables.

*   Arguments:
      REAL*8 X, KnownX(2), KnownY(2)

*   Argument definitions:
*    KnownX(2) - known value of X at points adjacent to X.
*    KnownY(2) - known value of Y corresponding to KnownX().
*    X - a value of X for which a value of Y is requested.

*   Module data:


*   Local Variables:
      REAL*8 Small, dX
      PARAMETER (Small = 1.0E-20)

*   Routines by module:

*   Intrinsics:
      REAL*8      ABS
      INTRINSIC ABS

*   Programmed by: Lew DeLong
*   Date:          January 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      dX = KnownX(2) - KnownX(1)
      IF ( ABS(dX) .GT. Small ) THEN

         LinearInterp1D = KnownY(1) +
     &        ( KnownY(2) - KnownY(1) ) * ( X - KnownX(1) ) / dX

      ELSE IF( ABS(X - KnownX(2)) .LT. Small ) THEN
         LinearInterp1D = KnownX(2)
      ELSE IF( ABS(X - KnownX(1)) .LT. Small ) THEN
         LinearInterp1D = KnownX(1)
      ELSE

*--------Shouldn't get here!
         WRITE(UNIT_ERROR,*) ' ***Error (LinearInterp1D)...'
         WRITE(UNIT_ERROR,*) ' Out of range ... ',X
         WRITE(UNIT_ERROR,*) KnownX(1), KnownX(2)
         CALL EXIT(1)

      END IF

      RETURN
      END

*== Private (XINSRT) ===================================================

      LOGICAL FUNCTION XINSRT
     &     ( MTB, UCXPB, USRX, DX, KEEP, MODEL,
     &     CMPCX, XORD)

      IMPLICIT NONE

*   Purpose:  This subprogram interpolates the location of cross sections
*             necessary to maintain the distance between computational cross
*             sections at intervals no greater than DX.  If the dummy argument
*             KEEP is .FALSE., then x-coordinates are interpolated equally
*             between the end sections.  Otherwise, each subreach is compared
*             against DX and x-coordinate are interpolated for those reaches
*             exceeding DX in length.  For such subreaches, x-coordinates are
*             set equidistant between subreach end sections.

*   Arguments:
      INTEGER   MTB, UCXPB, MODEL, CMPCX
      REAL*8      DX, USRX(UCXPB), XORD(MTB)
      LOGICAL   KEEP

*   Argument definitions:
*     MTB    - Maximum number of XORD values.
*     UCXPB  - Number of user cross sections for this branch.
*     USRX   - Vector of x-coordinates giving the location of the user's
*              sections.
*     DX     - Maximum distance between computational cross sections.
*     KEEP   - If .TRUE., then retain the user's cross sections for
*                computational purposes.
*              If .FALSE., then space computational sections equally throughout
*                branch, ignoring the user's sections (for computations).
*     MODEL  - Model for which output is to be generated:
*              [1] Branch (no intermediate computational nodes).
*              [2] OSW Training Model (one intermediate comp. node).
*              [3] Hydraux (two intermediate comp. nodes).
*     CMPCX  - Number of computational cross-sections.
*     XORD   - Vector of x-coordinates of computational cross-sections and
*                any intermediate nodes.

*   Module data:

*   Local variables:
      INTEGER   I, J, REMMTB
      REAL*8      LENGTH
      LOGICAL   SUBDVD, OK

*   Routines by module:

***** Locals:
      EXTERNAL SUBDVD

*   Programmed by:   David Thompson
*   Date programmed: March, 1990
*   Modified by:     Lew DeLong
*   Last modified:   October, 1991
*   Version 93.01, January, 1993

*-----Implementation ------------------------------------------------------------

*-----Case 1: Only two sections.
      IF (UCXPB .EQ. 2) THEN
         LENGTH = USRX(UCXPB) - USRX(1)
         OK     = SUBDVD(LENGTH, DX, MTB, USRX(1), USRX(2),
     &        CMPCX, XORD)
*--------Case 2: Multiple sections, but user sections are not
*       to be computational sections.
      ELSE IF (UCXPB .GT. 2 .AND. .NOT. KEEP) THEN
         LENGTH = USRX(UCXPB) - USRX(1)
         OK     = SUBDVD(LENGTH, DX, MTB, USRX(1), USRX(UCXPB),
     &        CMPCX, XORD)
*--------Case 3: Multiple sections and user sections are to be used as
*       computational sections.
      ELSE IF (UCXPB .GT. 2 .AND. KEEP) THEN
         J = 1
         CMPCX = 0
         DO 100 I = 1, UCXPB-1
            LENGTH = USRX(I+1) - USRX(I)
            REMMTB = MTB - CMPCX
            OK     = SUBDVD(LENGTH, DX, REMMTB, USRX(I), USRX(I+1),
     &           CMPCX, XORD(J))
*-----------This one is a bit sticky!  On first call to SUBDVD, CMPCX will include
*           the first section in the branch.  On subsequent calls, the first
*           section in the subreach will be counted.  Therefore, set J to CMPCX
*           on the first call and then add one less than the count on subsequent
*           calls.
            IF (I .EQ. 1 ) THEN
               J = CMPCX
            ELSE
               J = CMPCX + J - 1
            END IF
 100     CONTINUE
*--------Now, set CMPCX to the final value.
         CMPCX = J
      END IF

*-----If a problem occurred (MTB exceeded), set XINSRT to false.
      IF (OK) THEN
         XINSRT = .TRUE.
      ELSE
         XINSRT = .FALSE.
      ENDIF
*
      RETURN
      END

*== Private (SUBDVD) ===================================================

      LOGICAL FUNCTION SUBDVD
     &     ( LENGTH, DX, REMMTB, USRX1, USRX2,
     &     CMPCX, XORD)

      IMPLICIT NONE

*   Purpose:
*       This subprogram inserts computational cross sections between
*       USRX1 and USRX2 if needed to conform to the maximum computational
*       length of DX.

*   Arguments:
      INTEGER  CMPCX, REMMTB
      REAL*8     LENGTH, DX, XORD(REMMTB), USRX1, USRX2

*   Argument definitions:
*     LENGTH - Length of interval to be subdivided.
*     DX     - Maximum distance between computational cross sections.
*     REMMTB - Number of remaining entries in XORD.
*     USRX1  - downstream distance to first user cross-section.
*     USRX2  - downstream distance to second user cross-section.
*     CMPCX  - Number of computational cross sections.
*     XORD   - Vector of interpolated downstream distances.

*   Module data:

*   Local Variables:
      INTEGER   I, NUMCDX
      REAL*8      CMPDX

*   Routines by module:

***** Local:


*       Programmed by:    David Thompson
*       Date programmed:  March, 1990
*       Modified by:      Lew DeLong
*       Last modified:    December,  1990
*   Version 93.01, January, 1993
*-----Implementation ------------------------------------------------------------

      IF (LENGTH .LE. DX) THEN

         CMPCX = 2
         IF (CMPCX .LE. REMMTB) THEN
            XORD(1) = USRX1
            XORD(2) = USRX2
            SUBDVD  = .TRUE.
         ELSE
            SUBDVD = .FALSE.
         END IF

      ELSE IF (LENGTH .GT. DX) THEN

         NUMCDX  = INT(LENGTH / DX)
         CMPDX   = LENGTH / REAL(NUMCDX)
         CMPCX   = NUMCDX + 1

         IF (CMPCX .LE. REMMTB) THEN
            DO 10 I = 2, CMPCX-1
               XORD(I) = USRX1 + REAL(I-1) * CMPDX
 10         CONTINUE

*-----------Set the ends.
            XORD(1) = USRX1
            XORD(CMPCX) = USRX2
            SUBDVD  = .TRUE.
         END IF

      END IF

      RETURN
      END

*=== EOF linear ===============================================================
