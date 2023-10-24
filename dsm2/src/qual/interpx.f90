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

      SUBROUTINE INTERPX

!-----FLOW AND STAGE IS GIVEN EVERY HOUR, AND ONLY AT
!-----THE LANDWARD AND SEAWARD ENDS OF A CHANNEL

!-----THIS SUBROUTINE INTERPOLATES FLOW AND STAGE (FLOW AREA)
!-----FOR ALL THE CHANNELS AND ALL SECTIONS.
      use common_tide
      use network
      IMPLICIT NONE
      INCLUDE 'param.inc'
      INCLUDE 'bltm1.inc'
      INCLUDE 'bltm3.inc'
      INCLUDE 'bltm2.inc'

!-----LOCAL VARIABLES

      INTEGER I1,I
      REAL*8 DXFACTOR,QLAND,QSEA
      REAL*8 ALAND,ASEA

      DO N=1,NBRCH
         I1=NXSEC(N)
         QLAND=QCHAN(1,N)
         QSEA =QCHAN(2,N)
!--------YLAND=YCHAN(1,N)
!--------YSEA =YCHAN(2,N)
         ALAND=ACHAN(1,N)
         ASEA =ACHAN(2,N)

         DO I=1,I1
            DXFACTOR=dble(I-1)/dble(I1-1)
            FLOW(N,1,I)=QLAND+(QSEA-QLAND)*DXFACTOR
!-----------FLOW(N,2,I)=(YLAND+(YSEA-YLAND)*DXFACTOR)*B(N)
            FLOW(N,2,I)=(ALAND+(ASEA-ALAND)*DXFACTOR)
            FLOW(N,3,I)=B(N)    ! This needs to be changed later.
            FLOW(N,4,I)= 0.
         ENDDO
      ENDDO
      CALL BALANCEFLO
      RETURN
      END

