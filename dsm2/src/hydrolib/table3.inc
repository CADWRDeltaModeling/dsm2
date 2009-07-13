C!<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    The Delta Simulation Model 2 (DSM2) is free software: 
C!    you can redistribute it and/or modify
C!    it under the terms of the GNU General Public License as published by
C!    the Free Software Foundation, either version 3 of the License, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public License for more details.

C!    You should have received a copy of the GNU General Public License
C!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
C!</license>

C=====TABLE3.INC bof============================================================
*   Version 93.01, January, 1993

C-----Parameters
      INTEGER MAXDWN, MAXQ
      PARAMETER (MAXDWN=10, MAXQ=20)

C-----Common block
      INTEGER NWS,NQ,TB3UNIT
      REAL*8 QTB(MAXQ),WSDSTB(MAXDWN),WSUSTB(MAXQ*MAXDWN),BASEL
      COMMON / TABLE3_I / NWS,NQ,TB3UNIT
	  COMMON /TABLE3_R/QTB,WSDSTB,WSUSTB,BASEL
C-----SAVE / TABLE3 /

C-----variable definitions
C     MAXDWN - maximum no. of downstream water surface elevations
C     MAXQ   - maximum no. of discharges
C     NWS    - current no. of downstream water surface elevations
C     NUWS   - current no. of upstream water surface elevations
C     NQ     - current no. of discharges
C     TB3UNIT - FORTRAN unit number for tables30.dat.
C     WSDSTB - downstream water surface elevations, x ordinate on table
C     QTB    - discharges, y ordinate on table
C     WSUSTB - upstream water surface elevations, z ordinate on table
C     BASEL  - reference or base elevation

C=====TABLE3.INC eof=======================================================
