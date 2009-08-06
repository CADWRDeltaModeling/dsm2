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

*===== BOF dambrk.inc =================================================
*   Version 93.01, January, 1993

*   Note: "network.inc" must precede this include.

      INTEGER n
      PARAMETER ( n = 2*MaxChannels )
      INTEGER ID(n)
      REAL    YD(n), BD(n)
      REAL    BT(n), TTB(n), ISS(n), YBM(n), YBRCH(n), TB(n)
      REAL    SC(n), SL(n), SH(n)
      REAL    GC(n), GA(n), GH(n)
      REAL    DC(n), DL(n), DH(n)
      REAL    QT(n)
      REAL    QB, QS, QG, QD

      COMMON  /CoefDAMBRK/ ID, YD, BD,
     &     BT, TTB, ISS, YBM, YBRCH, TB,
     &     SC, SL, SH,
     &     GC, GA, GH,
     &     DC, DL, DH,
     &     QB, QS, QG, QD, QT
      SAVE / CoefDAMBRK /

*   Definitions:
*
*                 dam
*
*     ID(i)     - associated boundary-condition code
*     YD(i)     - elevation of top of dam
*     BD(i)     - width of the reservoir at the dam
*
*
*                  breach
*
*     BT(i)     - final breach width
*     TTB(i)    - total time of breach formation, in hours
*     ISS(i)    - inverse of side slope of breach (horizontal/vertical)
*     YBM(i)    - final elevation of breach bottom
*     YBRCH(i)  - water surface elevation at which breach initiates
*     TB(i)     - time elapsed since start of breach
*
*
*                 uncontrolled spillway
*
*     SC(i)     - uncontrolled spillway coefficient
*     SL(i)     - spillway length
*     SH(i)     - spillway crest elevation
*
*
*                 gate
*
*     GC(i)     - gate discharge coefficient
*     GA(i)     - gate cross section area
*     GH(i)     - center-line elevation of gated spillway
*
*
*                 flow over dam
*
*     DC(i)     - coefficient fo flow over crest of dam
*     DL(i)     - length of dam less uncontrolled spillway length
*     DH(i)     - dam crest elevation
*
*
*                 flow through turbine
*
*     QT(i)     - constant flow through turbine
*
*
*     i         - associated sequence number
*
*
*                 computed discharges
*
*     QB        - discharge through breach
*     QS        - discharge over uncontrolled spillway
*     QG        - discharge through gate
*     QD        - discharge over dam
*

*===== EOF dambrk.inc ================================================
