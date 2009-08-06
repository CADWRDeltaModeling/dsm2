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

*===== BOF logcnst.inc ================================================
*   Version 93.01, January, 1993

*   Note: "network.inc" must precede this include.

      INTEGER n
      PARAMETER ( n = 2*MaxChannels )
      INTEGER ID(n)
      REAL    a(n), b(n), Zo(n)

      COMMON  /CoefLogStrmCnst/ ID, a, b, Zo

      SAVE / CoefLogStrmCnst /

*   Definitions:
*
*     ID(i)  - associated boundary-condition code
*             [.....02..]  semilog constraint.
*             [.....03..]  log constraint.
*     a(i)   - coefficient, intercept.
*     b(i)   - coefficient, slope.
*     Zo(i)  - reference datum.
*     i      - relation number.

*                   log constraint
*
*             Z = Zo + a * Q ** b, or
*
*                   semilog constraint
*
*             Z = Zo + a + b * LOG10 ( Q ),
*
*             where Q = discharge, and
*                   Z = water-surface elevation.
*

*===== EOF logcnst.inc ===============================================
