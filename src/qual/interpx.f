<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    DSM2 is free software: you can redistribute it and/or modify
C!    it under the terms of the GNU General Public License as published by
C!    the Free Software Foundation, either version 3 of the License, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public License for more details.

C!    You should have received a copy of the GNU General Public License
C!    along with DSM2.  If not, see <http://www.gnu.org/licenses/>.
</license>

      SUBROUTINE INTERPX

C-----FLOW AND STAGE IS GIVEN EVERY HOUR, AND ONLY AT
C-----THE LANDWARD AND SEAWARD ENDS OF A CHANNEL

C-----THIS SUBROUTINE INTERPOLATES FLOW AND STAGE (FLOW AREA)
C-----FOR ALL THE CHANNELS AND ALL SECTIONS.
      use common_tide
      IMPLICIT NONE
      INCLUDE 'param.inc'
      INCLUDE '../hydrolib/network.inc'
      INCLUDE 'bltm1.inc'
      INCLUDE 'bltm3.inc'
      INCLUDE 'bltm2.inc'

C-----LOCAL VARIABLES

      INTEGER I1,I
      REAL*8 DXFACTOR,QLAND,QSEA
      REAL*8 ALAND,ASEA

      DO N=1,NBRCH
         I1=NXSEC(N)
         QLAND=QCHAN(N,1)
         QSEA =QCHAN(N,2)
C--------YLAND=YCHAN(N,1)
C--------YSEA =YCHAN(N,2)
         ALAND=ACHAN(N,1)
         ASEA =ACHAN(N,2)

         DO I=1,I1
            DXFACTOR=dble(I-1)/dble(I1-1)
            FLOW(N,1,I)=QLAND+(QSEA-QLAND)*DXFACTOR
C-----------FLOW(N,2,I)=(YLAND+(YSEA-YLAND)*DXFACTOR)*B(N)
            FLOW(N,2,I)=(ALAND+(ASEA-ALAND)*DXFACTOR)
            FLOW(N,3,I)=B(N)    ! This needs to be changed later.
            FLOW(N,4,I)= 0.
         ENDDO
      ENDDO
      CALL BALANCEFLO
      RETURN
      END
      
