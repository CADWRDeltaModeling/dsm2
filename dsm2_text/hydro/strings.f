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


C==== BOF strings =============================================================
*******************************************************************************
*
*     STRING UTILITIES:  Provide the FORTRAN programmer with some utilities to
*       manipulate character strings.
*
*     Note:   All string routines are based on character strings (defined using
*             CHARACTER*N where N is an integer).  THESE ARE NOT CHARACTER
*             ARRAYS!  They are true character strings and may be subject to
*             portability problems depending on how well the host compiler
*             implements the FORTRAN 77 standard.
*
C==---Public (StrLen) ===========================================================

      INTEGER FUNCTION StrLen (
     &     String
     &     )

C-----Purpose:  This routine returns the actual length of the character string,
C-----excluding trailing blanks.  The name of the function and its
C-----operation is the same as the ANSI C standard function.

C-----Algorithm:  Start with the maximum length of the string.  Decrement the
C-----counter for each trailing character which is either a blank
C-----or a null (for C type strings).

      IMPLICIT NONE

C-----Arguments:

      CHARACTER*(*) String

C-----Local variables:

      INTEGER     Counter
      CHARACTER*1 Null

C-----Functions:

      CHARACTER*1 CHAR
      INTEGER     LEN

      INTRINSIC   CHAR, LEN

C-----Programmed by:  Dave Thompson
C-----Date:           Wed  11-21-1990
C-----Modified by:
C-----Last modified:
*   Version 93.01, January, 1993

C-----Implementation ------------------------------------------------------------

      Null    = CHAR(0)

      Counter = LEN(String) + 1

 10   Counter = Counter - 1
      IF ((String(Counter:Counter) .EQ. ' '   .OR.
     &     String(Counter:Counter) .EQ. Null) .AND.
     &     Counter                 .GE. 1)         GOTO 10

      StrLen = Counter

      RETURN
      END

C==---Public (LeftTrim) =========================================================

      CHARACTER*255 FUNCTION LeftTrim (
     &     String
     &     )

C-----Purpose:  This routine returns a string with the leading blanks removed.

C-----Algorithm:  Search the string for the first character not blank.  Then,
C-----copy the string, beginning at that offset, to the temporary
C-----string.  Finally, return the temporary string.  Currently, the
C-----maximum length of string this routine can accomodate is 256
C-----bytes.  This could be changed depending on requirements.

      IMPLICIT NONE

C-----Arguments:

      CHARACTER*(*) String

      include '../input/fixed/misc.f'

C-----Local variables:

      LOGICAL*1     Done
      INTEGER       I, J, MaxLength
      CHARACTER*255 TempString

C-----Functions:

      INTEGER   StrLen
      EXTERNAL  StrLen

C-----Programmed by:  Dave Thompson
C-----Date:           Wed  11-21-1990
C-----Modified by:    Dave Thompson
C-----Last modified:  Wed  12-12-1990
*   Version 93.01, January, 1993

C-----Implementation ------------------------------------------------------------

      MaxLength  = StrLen(String)

      IF (MaxLength .GT. 256) THEN

         WRITE(UNIT_ERROR,*) ' Error(LeftTrim): Incoming string too long.'
         WRITE(UNIT_ERROR,*) ' Maximum length  = 256'
         WRITE(UNIT_ERROR,*) ' Incoming length =', MaxLength

      ELSEIF (MaxLength .GT. 0) THEN

         IF (String(1:1) .EQ. ' ') THEN

C-----------Blanks are present at the beginning of the string.  Initialize.

            I     = 0
            Done  = .FALSE.

C-----------Count the leading blanks.

 10         I = I + 1

            IF (String(I:I) .NE. ' ') THEN

               J    = I - 1
               Done = .TRUE.

            ENDIF

            IF (.NOT. Done .AND. I .LT. MaxLength) GOTO 10

C-----------Now, move the string into a temporary string.  Begin with the first
C-----------non-blank character, and continue through MaxLength-J

            TempString = ' '

            DO 20 I = 1, MaxLength-J

               TempString(I:I) = String(I+J:I+J)

 20         CONTINUE

            LeftTrim = TempString

         ELSE

C-----------No blanks present at the beginning of the string.

            LeftTrim = String

         ENDIF

      ELSE

C--------Null string.

         LeftTrim = ' '

      ENDIF

      RETURN
      END

C==---Public (RightTrim) ========================================================

      CHARACTER*255 FUNCTION RightTrim (
     &     String
     &     )

C-----Purpose:  This routine removes trailing blanks from a character string.

C-----Algorithm:  This routine uses the StrLen function to determine the actual
C-----length of the incoming string (less any blanks or nulls).  It
C-----then simply copies all non-blank and non-null characters from
C-----the incoming string to a temporary string and it.  Currently,
C-----the routine can handle a string with a maximum length of 256
C-----bytes.

      IMPLICIT NONE

C-----Arguments:

      CHARACTER*(*) String

      include '../input/fixed/misc.f'

C-----Local variables:

      INTEGER       I, MaxLength
      CHARACTER*255 TempString

C-----Functions:

      INTEGER     StrLen
      EXTERNAL    StrLen

C-----Programmed by:  Dave Thompson
C-----Date:           Wed  11-21-1990
C-----Modified by:    Dave Thompson
C-----Last modified:  Wed  12-12-1990
*   Version 93.01, January, 1993

C-----Implementation ------------------------------------------------------------

      MaxLength = StrLen(String)

      IF (MaxLength .GT. 256) THEN

         WRITE(UNIT_ERROR,*) ' Error(RightTrim): Incoming string too long.'
         WRITE(UNIT_ERROR,*) ' Maximum length  = 256'
         WRITE(UNIT_ERROR,*) ' Incoming length =', MaxLength

      ELSE

         TempString = ' '

         DO 10 I = 1, MaxLength

            TempString(I:I) = String(I:I)

 10      CONTINUE

         RightTrim = TempString

      ENDIF

      RETURN
      END

C==---Public (StrToL) ===========================================================

      INTEGER*4 FUNCTION StrToL (
     &     String
     &     )

C-----Purpose:  Convert an incoming string to an long integer value.

      IMPLICIT NONE

C-----Arguments:

      CHARACTER*(*) String

C-----Functions:

      CHARACTER*255 LeftTrim, RightTrim
      EXTERNAL      LeftTrim, RightTrim

      include '../input/fixed/misc.f'

C-----Local variables:

      INTEGER       I, IOStatus
      CHARACTER*255 TempString

C-----Programmed by: Dave Thompson
C-----Date:          Wed  12-05-1990
C-----Modified by:   Dave Thompson
C-----Last modified: Wed  12-12-1990
*   Version 93.01, January, 1993

C-----Implementation ------------------------------------------------------------

      TempString = LeftTrim(String)
      TempString = RightTrim(TempString)

      READ (TempString, '(I80)', IOSTAT = IOStatus) I

      IF (IOStatus .NE. 0) THEN

         WRITE(UNIT_ERROR,*) ' Error(StrToL): Error reading an integer'

      ELSE

         StrToL = I

      ENDIF

      RETURN
      END

C==---Public (StrToD) ===========================================================

      DOUBLE PRECISION FUNCTION StrToD (
     &     String
     &     )

C-----Purpose:  Convert an incoming string to an double precision value.

      IMPLICIT NONE

C-----Arguments:

      CHARACTER*(*) String

C-----Functions:

      CHARACTER*255 LeftTrim, RightTrim
      EXTERNAL      LeftTrim, RightTrim

      include '../input/fixed/misc.f'

C-----Local variables:

      REAL*8        D
      INTEGER       IOStatus
      CHARACTER*255 TempString

C-----Programmed by: Dave Thompson
C-----Date:          Thu  12-06-1990
C-----Modified by:   Dave Thompson
C-----Last modified: Wed  12-12-1990
*   Version 93.01, January, 1993

C-----Implementation ------------------------------------------------------------

      TempString = LeftTrim(String)
      TempString = RightTrim(TempString)

      READ (TempString, '(D80.0)', IOSTAT = IOStatus) D

      IF (IOStatus .NE. 0) THEN

         WRITE(UNIT_ERROR,*) ' Error(StrToD): Error reading an double'

      ELSE

         StrToD = D

      ENDIF

      RETURN
      END

C==== EOF strings =============================================================
