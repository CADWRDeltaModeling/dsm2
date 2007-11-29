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


C=====INITABLE3 bof======================================================

C-----Purpose:
C-----Initialize the 3 parameter tables by openning the file unit
C     Programmed by: JM Fulford
C     Date: 12-91
C     Modified by:   Lew DeLong (use of master file names added)
C     Last modified: May   1992
*   Version 93.01, January, 1993

      LOGICAL FUNCTION INITABLE3()

      IMPLICIT NONE

C-----Module data:
      INCLUDE 'table3.inc'
      SAVE / TABLE3_i / , /TABLE3_r/

      include '../input/fixed/misc.f'

C-----Local Variables:
      CHARACTER*12  TBNAME,TYPE
      CHARACTER*12  InternalFileName

C-----Routines by module:

C-----Master file:
      INTEGER GetFileUnit
      CHARACTER*12 GetFileName
      EXTERNAL GetFileUnit, GetFileName

C-----Intrinsics:

      INITABLE3 = .TRUE.

C-----Get file name and unit from master.fil.
      InternalFileName = 'tables30.dat'
      TBNAME           = GetFileName( InternalFileName )
      TB3UNIT          = GetFileUnit( InternalFileName )

      OPEN (TB3UNIT,FILE=TBNAME,STATUS='OLD',ERR=999)
      READ(TB3UNIT,'(A12)') TYPE
      IF(TYPE.NE.TBNAME) GO TO 999
      GO TO 1000
 999  CONTINUE
      WRITE(UNIT_ERROR,*) ' Could not open 3-parameter-table file...',TBNAME
      INITABLE3 = .FALSE.
 1000 RETURN

      END

C=====INITABLE3 eof========================================================

C=====GETAB3 bof===========================================================C

C-----Purpose:
C-----point to table in the tables30.dat file that matches tableid and
C-----read that table contents into common block TABLE3
C     Programmed by: JM Fulford
C     Date: 12-91
C     Modified by:
C     Last modified:
*   Version 93.01, January, 1993

      LOGICAL FUNCTION GETAB3(TAB3ID)

      IMPLICIT NONE

C-----Arguments:
      INTEGER TAB3ID

C-----Argument definitions:
C     TAB3ID - integer identifier for table to get

C-----Module data:
      INCLUDE 'table3.inc'
      SAVE / TABLE3_r / ,/TABLE3_i/

C-----Local Variables:
      INTEGER I,IDENT
      REAL*8 NWSU
      CHARACTER*3 LABEL

C-----Local definitions
C     TBUNIT - unit that table file is openned on
C     LABEL  - label that identifies beginning of table
C     IDENT  - integer identifier for table

      GETAB3 = .FALSE.
 10   CONTINUE
      READ(TB3UNIT,'(A3)',END=999) LABEL
      IF(LABEL.NE.'TAB') GO TO 10
      READ(TB3UNIT,'(I10,1X,I2,1X,I2)') IDENT,NWS,NQ
      IF(IDENT.NE.TAB3ID) GO TO 10

      READ(TB3UNIT,'(F9.3)')BASEL
      READ(TB3UNIT,*,ERR=999)(WSDSTB(I),I=1,NWS)
      READ(TB3UNIT,*,ERR=999)(QTB(I),I=1,NQ)
      NWSU=NWS*NQ
      READ(TB3UNIT,*,ERR=999)(WSUSTB(I),I=1,NWSU)
*       WRITE(*,*)IDENT,NWS,NQ
*       WRITE(*,*)'BASEL=',BASEL
*       WRITE(*,*) (WSDSTB(I),I=1,NWS)
*       WRITE(*,*) (QTB(I),I=1,NQ)

      GETAB3 = .TRUE.

 999  CONTINUE
      REWIND (TB3UNIT)
      READ(TB3UNIT,'(A3)') LABEL

      RETURN
      END

C=====GETAB3 eof===========================================================

C=====BRACKET eof==========================================================

C-----Purpose:
C-----brackets the desired value in the table
C     Programmed by: JM Fulford
C     Date: 12-91
C     Modified by:
C     Last modified:
*   Version 93.01, January, 1993

      INTEGER FUNCTION BRACKET (VALUE,N,ARRAY)

      IMPLICIT NONE

C-----Arguments:
      INTEGER N
      REAL*8 VALUE,ARRAY(N)

C-----Argument definitions:
C     N      - size of array
C     VALUE  - value to bracket
C     ARRAY  - array to bracket value in

C-----Module data:  none

C-----Local Variables:
      INTEGER IFLG,I

C-----Routines by module:

C-----Local:
C     IFLG   - flags bracket location

C-----Intrinsics:

      IFLG=0
      I=0
      BRACKET = 0
 10   CONTINUE
      I=I+1
      IF(VALUE.LE.ARRAY(I)) IFLG=1
      IF(I.LT.N.AND.IFLG.EQ.0) GO TO 10
      IF(I.LE.1.AND.VALUE.LT.ARRAY(I)) THEN
         BRACKET =0
      ELSE IF(IFLG.EQ.0) THEN
         BRACKET = -1
      ELSE
         BRACKET = I
      ENDIF

      RETURN
      END

C=====BRACKET eof =========================================================

C=====TB3WSUS public =======================================================

C-----Purpose:
C-----interpolates water surface elevation from tabulated
C-----values in a 3 parameter table.  Interpolations for tables
C-----not located in tables30.dat or outside of tabulated values
C-----return -100,000. for tbswsus.
C     Programmed by: JM Fulford
C     Date: 12-91
C     Modified by:
C     Last modified:
*   Version 93.01, January, 1993

      REAL*8 FUNCTION TB3WSUS (STRUCTID,WSDS,Q)

      IMPLICIT NONE

C-----Arguments:
      INTEGER STRUCTID
      REAL*8 WSDS,Q

C-----Argument definitions:
C-----STRUCTID - 3 parameter table identifier
C-----WSDS   - water surface downstream
C-----Q      - discharge

C-----Module data:
      INCLUDE 'table3.inc'

      include '../input/fixed/misc.f'

C-----Local Variables:

C-----Routines by module:
      LOGICAL GETAB3
      INTEGER BRACKET
      REAL*8 INTERP3
      EXTERNAL BRACKET, INTERP3, GETAB3

C-----Local:
      INTEGER IQ,IWS,ERR
      REAL*8 WSDS0

C-----Intrinsics:

      ERR=0
      TB3WSUS=-100000.
      IF(GETAB3(STRUCTID)) THEN
         IQ=BRACKET(Q,NQ,QTB)
         WSDS0=WSDS-BASEL
         IWS=BRACKET(WSDS0,NWS,WSDSTB)
         IF(IQ.LE.0)THEN
            WRITE(UNIT_ERROR,*)'DISCHARGE OUT OF COMPUTED RANGE'
            ERR=1
         ENDIF
         IF(IWS.LE.0) THEN
            WRITE(UNIT_ERROR,*) 'DOWNSTREAM WATER SURFACE OUT OF COMPUTED RANGE'
            ERR=1
         ENDIF
         IF(ERR.NE.1) THEN
            TB3WSUS=INTERP3(WSDS0,IWS,Q,IQ)
            TB3WSUS=TB3WSUS+BASEL
         ENDIF
      ELSE
         WRITE(UNIT_ERROR,*)STRUCTID,' STRUCTURE NOT FOUND IN FILE'
      ENDIF

      RETURN
      END

C=====TB3WSUS eof =========================================================

C=====INTERP3 bof =========================================================

C-----Purpose:
C-----Linearly interpolation of a point on a plane.  Uses four known
C-----or tabulated points that describe a rectangle that bound the point.
C     Programmed by: JM Fulford
C     Date: 12-91
C     Modified by:
C     Last modified:
*   Version 93.01, January, 1993

      REAL*8 FUNCTION INTERP3(WSDS,IWS,Q,IQ)

      INTEGER IWS,IQ
      REAL*8 WSDS,Q

      INCLUDE 'table3.inc'

      INTEGER I(4),IWEIGH
      REAL*8 DELQ,DELWSDS,A,N(4),B
*-----^

      INTERP3=0.0
      DELQ=QTB(IQ)-QTB(IQ-1)
      DELWSDS=WSDSTB(IWS)-WSDSTB(IWS-1)
      A=(WSDS-WSDSTB(IWS-1))/DELWSDS
      B=(Q-QTB(IQ-1))/DELQ
      N(1)=(1.0-A)*(1.0-B)
      N(2)=A*(1.0-B)
      N(3)=A*B
      N(4)=(1.0-A)*B
      I(4)=(IQ-1)*NWS+IWS-1
      I(3)=(IQ-1)*NWS+IWS
      I(2)=(IQ-2)*NWS+IWS
      I(1)=(IQ-2)*NWS+IWS-1
      DO 10 IWEIGH=1,4
         INTERP3=INTERP3+N(IWEIGH)*WSUSTB(I(IWEIGH))
 10   CONTINUE

      RETURN
      END

C=====INTERP3 eof ==========================================================

C=====TB3DWSUS bof =========================================================

C-----Purpose:
C-----interpolates upstream water surface elevation and derivatives
C-----from tabulated values in a 3 parameter table.  Interpolations
C-----for tables not found in tables30.dat or for points outside of
C-----the tabulated values return -100000 for wsus, and 100000 for
C-----dq and dwsds.  ERR returned zero is no error, 1 error.
C     Programmed by: JM Fulford
C     Date: 12-91
C     Modified by:
C     Last modified:
*   Version 93.01, January, 1993

      SUBROUTINE TB3DWSUS
     &     (STRUCTID,WSDS,Q,
     &     WSUS,DWSDS,DQ,ERR)

      IMPLICIT NONE

C-----Arguments:
      INTEGER STRUCTID,ERR
      REAL*8 WSDS,Q,WSUS,DWSDS,DQ

C-----Argument definitions:
C     STRUCTID - 3 parameter table identifier
C     WSDS   - water surface downstream
C     Q      - discharge
C     WSUS   - water surface upstream
C     DWSDS  - derivative of WSUS with respect to WSDS
C     DQ     - derivative of WSUS with respect to Q

C-----Module data:
      INCLUDE 'table3.inc'

      include '../input/fixed/misc.f'

C-----Local Variables:

C-----Routines by module:
      LOGICAL GETAB3
      INTEGER BRACKET
      REAL*8 INTERP3
      EXTERNAL BRACKET, INTERP3, GETAB3, INTERPD3

C-----Local:
      INTEGER IQ,IWS
      REAL*8 WSDS0

C-----Intrinsics:

      ERR=0
      WSUS=-100000.
      DWSDS=100000.
      DQ=100000.
      IF(GETAB3(STRUCTID)) THEN
         IQ=BRACKET(Q,NQ,QTB)
         WSDS0=WSDS-BASEL
         IWS=BRACKET(WSDS0,NWS,WSDSTB)
         IF(IQ.LE.0)THEN
            WRITE(UNIT_ERROR,*)'DISCHARGE OUT OF COMPUTED RANGE'
            ERR=1
         ENDIF
         IF(IWS.LE.0) THEN
            WRITE(UNIT_ERROR,*) 'DOWNSTREAM WATER SURFACE OUT OF COMPUTED RANGE'
            ERR=1
         ENDIF
         IF(ERR.NE.1) THEN
            WSUS=INTERP3(WSDS0,IWS,Q,IQ)
            CALL INTERPD3(WSDS0,IWS,Q,IQ,DWSDS,DQ)
            WSUS=WSUS+BASEL
         ENDIF
      ELSE
         WRITE(UNIT_ERROR,*)STRUCTID,' STRUCTURE NOT FOUND IN FILE'
      ENDIF

      RETURN
      END

C=====TB3DWSUS eof ========================================================

C=====INTERPD3 bof ========================================================

C-----Purpose:
C     Programmed by: JM Fulford
C     Date: 12-91
C     Modified by:
C     Last modified:
*   Version 93.01, January, 1993

      SUBROUTINE INTERPD3
     &     (WSDS,IWS,Q,IQ,
     &     DWSDS,DQ)

      IMPLICIT NONE

C-----Arguments:
      INTEGER IWS,IQ
      REAL*8 WSDS,Q,DWSDS,DQ

C-----Argument definitions:

C-----Module data:
      INCLUDE 'table3.inc'
C-----Local Variables:

C-----Routines by module:

C-----Local:
      INTEGER I(4)
      REAL*8 DELQ,DELWSDS,A,B

C-----Intrinsics:
      DELQ=QTB(IQ)-QTB(IQ-1)
      DELWSDS=WSDSTB(IWS)-WSDSTB(IWS-1)
      A=(WSDS-WSDSTB(IWS-1))/DELWSDS
      B=(Q-QTB(IQ-1))/DELQ
      I(4)=(IQ-1)*NWS+IWS-1
      I(3)=(IQ-1)*NWS+IWS
      I(2)=(IQ-2)*NWS+IWS
      I(1)=(IQ-2)*NWS+IWS-1
      DWSDS=(1.0-B)*(WSUSTB(I(2))-WSUSTB(I(1)))
     &     +B*(WSUSTB(I(3))-WSUSTB(I(4)))
      DWSDS=DWSDS/DELWSDS
      DQ=(1.0-A)*(WSUSTB(I(4))-WSUSTB(I(1)))
     &     +A*(WSUSTB(I(3))-WSUSTB(I(2)))
      DQ=DQ/DELQ

      RETURN
      END

C=====INTERPD3 eof ========================================================

C=====TB3FLOW bof =========================================================

C-----Purpose:
C-----interpolates discharge from a 3 parameter table. Interpolations
C-----for tables not located in tables30.dat and outside of range of
C-----tabulated values return tb3flow = -1.0
C     Programmed by: JM Fulford
C     Date: 12-91
C     Modified by:
C     Last modified:
*   Version 93.01, January, 1993

      REAL*8 FUNCTION TB3FLOW (STRUCTID,WSUS,WSDS)

      IMPLICIT NONE

C-----Arguments:
      INTEGER STRUCTID
      REAL*8 WSDS,WSUS

C-----Argument definitions:
C-----STRUCTID - 3 parameter table identifier
C-----WSUS   - water surface upstream of structure
C-----WSDS   - water surface downstream  structure

C-----Argument definitions:

C-----Module data:
      INCLUDE 'table3.inc'

      include '../input/fixed/misc.f'

C-----Local Variables:
      INTEGER KFLG,IQ,IWS,I1,I2,I3,I4
      REAL*8 A,DELWSDS,DELQ,WSUS0,WSDS0,NTW,USWS

C-----Routines by module:
      LOGICAL GETAB3
      INTEGER BRACKET
      EXTERNAL GETAB3, BRACKET

C-----Local:

C-----Intrinsics:
      TB3FLOW=-1.0
      IF(GETAB3(STRUCTID)) THEN
         WSDS0=WSDS-BASEL
         IWS=BRACKET(WSDS0,NWS,WSDSTB)
         IF(IWS.LE.0)THEN
            WRITE(UNIT_ERROR,*)'DOWNSTREAM WATER SURFACE OUT OF TABLE RANGE'
         ELSE
            KFLG=0
            IQ=0
            WSUS0=WSUS-BASEL
            DELWSDS=WSDSTB(IWS)-WSDSTB(IWS-1)
            NTW=(WSDS0-WSDSTB(IWS-1))/DELWSDS
 10         CONTINUE
            IQ=IQ+1
            I3=IQ*NWS+IWS
            I4=I3-1
            USWS=WSUSTB(I4)+NTW*(WSUSTB(I3)-WSUSTB(I4))
            IF(USWS.GE.WSUS0) KFLG=1
            IF(IQ.LT.NQ.AND.KFLG.EQ.0) GO TO 10
            IF(IQ.GT.NQ.OR.KFLG.EQ.0) THEN
               WRITE(UNIT_ERROR,*)'DISCHARGE OUTSIDE OF TABLE RANGE'
            ELSE
               DELQ=QTB(IQ)-QTB(IQ-1)
               A=(WSDS0-WSDSTB(IWS-1))/DELWSDS
               I2=(IQ-1)*NWS+IWS
               I1=(IQ-1)*NWS+IWS-1
               TB3FLOW=(1.0-A)*(WSUSTB(I4)-WSUSTB(I1)) +
     &              A*(WSUSTB(I3)-WSUSTB(I2))
               TB3FLOW=(WSUS0-(1.0-A)*WSUSTB(I1)-A*WSUSTB(I2))/TB3FLOW
               TB3FLOW=QTB(IQ)+DELQ*TB3FLOW
            ENDIF
         ENDIF
      ELSE
         WRITE(UNIT_ERROR,*) STRUCTID,' STRUCTURE NOT FOUND IN FILE'
      ENDIF

      RETURN
      END

C=====TB3FLOW eof==========================================================

