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

      integer chan_res          ! pointer whether channel or reservoir for kinetics
      common/chan_res_c/chan_res

      integer n, jtime, irc, mx, nrev, irev
      real*8 dt, DTT, a, w, q, resdepth

      common/b3_i/n, jtime, irc, mx,
     &     nrev,irev
      common/b3_r/ dt, DTT, a(nosc), w(nosc), q(nosc),
     &     resdepth

      INTEGER MALG, MORGN, MNH3, MNO2,MNO3,MORGP,
     &     MPO4,MBOD,MDO,MTEMP,MTDS,MCL
     &     ,MEC

      COMMON /BLK7/ 
     &     MALG, MORGN, MNH3, MNO2,MNO3,MORGP,
     &     MPO4,MBOD,MDO,MTEMP,MTDS,MCL,MEC

      REAL*8 SOLAR,
     &     TSOLHR,HA,DRYBLB,WETBLB,
     &     CLOUD,WIND,
     &     ATMPR

      COMMON /HEATEX1/ SOLAR,
     &     TSOLHR,HA,DRYBLB,WETBLB,
     &     CLOUD,WIND,
     &     ATMPR

      REAL*8 STS,STB,STR,DELTSL,EQTIME,RR
      REAL*8 DECLIN,ELEXP,CS,VPAIR

      INTEGER NL,IDAY
      COMMON /HEATEX2_I/NL,IDAY
      COMMON /HEATEX2_R/ STS,STB,STR,DELTSL,EQTIME,RR,
     &     DECLIN,ELEXP,CS,VPAIR

 
