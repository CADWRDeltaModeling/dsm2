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

*===== BOF chcxtbl.inc ================================================
*   Version 93.01, January, 1993

*   Note: If used, this include must appear after "Network.inc".

      INTEGER     MaxTables, MaxLinesPerTable, MaxLines
      PARAMETER ( MaxTables = 5000,
     &     MaxLinesPerTable = 21,
     &     MaxLines = MaxLinesPerTable*MaxTables )

      INTEGER     FirstTable(MaxChannels), LastTable(MaxChannels)
      INTEGER     Lines(MaxChannels), Offset(MaxTables)
      REAL*8        XDistance(MaxTables)
      REAL*8 Datum(MaxTables)
      REAL*8        Depth(MaxLines)
      REAL*8        Width(MaxLines), A(MaxLines), K(MaxLines)
      REAL*8        P(MaxLines), N(MaxLines)
      REAL*8        Bta(MaxLines), MA(MaxLines), MQ(MaxLines)
      REAL*8        RectangleWidth(MaxChannels,2)
      REAL*8        OneOverManning(MaxChannels)
      LOGICAL     Rectangular(MaxChannels),Prismatic(MaxChannels)
      LOGICAL     Print, WriteIntrp, HermiteBtm
      COMMON   /HYDPRP/ XDistance, Datum, Depth, Width, A, K, P, N,
     &     Bta, MA,MQ,
     &     RectangleWidth,Rectangular,Prismatic,OneOverManning,
     &     Print,WriteIntrp, HermiteBtm
      COMMON   /HYDPT/ FirstTable, LastTable, Lines, Offset

      INTEGER N1, N2, NP(4), PreviousBranch, PreviousX, PreviousH100
      INTEGER DegreesOfFreedom
      REAL*8    Xi, Eta, dH, Shape(4), dShapeDX(4)
      COMMON /CMNFCT/ Xi, Eta, dH, Shape, dShapeDX, N1, N2, NP,
     &     PreviousBranch, PreviousX, PreviousH100,
     &     DegreesOfFreedom
      SAVE /HYDPRP/,/HYDPT/,/CMNFCT/

      CHARACTER*16 ID(MaxTables)
      COMMON /CXiD/ ID
      SAVE   /CXiD/

*
*     MaxTables        - maximum number of tables.
*     MaxLinesPerTable - maximum number of lines per table.
*     MaxLines         - maximum total number of lines in tables.
*
*     FirstTable(m) - table number of first table for a branch.
*     LastTable(m)  - table number of last table of a branch.
*     Lines(m) - number of lines per table for the curren branch.
*     m - branch number.
*
*     Offset(i) - pointer to first line of a table in properties arrays.
*     XDistance(i) - downstream-distance coordinate.
*     Datum(i) - elevation of bottom of the channel.
*     i - table number, same as user cross-section sequence number.
*
*     Depth(j) - depth of flow.
*     Width(j) - width of channel.
*     A(j)     - cross-sectional area.
*     K(j)     - sinuosity-weighted conveyance.
*     P(j)     - wetted perimeter.
*     N(j)     - one over effective Manning's "n".
*     Bta(j)   - momentum coefficient.
*     MA(j)    - area-weighted sinuosity.
*     MQ(j)    - discharge-weighted sinuosity.
*     j - line number.
*
*     N1 - adjacent upstream cross section.
*     N2 - adjacent downstream cross section.
*
*     NP(k)       - pointers to table positions in adjacent upstream and downstream
*                   property tables bracketing the estimation point.  Order is
*                   counter clockwise viewing the stream profile with upstream
*                   cross section on the left, downstream cross section on the
*                   right, and beginning with the lower of two bracketing points
*                   in the upstream cross section.
*     Shape(k)    - local bilinear shape function for the estimation point.
*     dShapeDX(k) - derivative of Shape with respect to the local depth coordinate.
*     k = degree of freedom.

*     DegreesOfFreedom - maximum number of degrees of freedom,
*                        four for bilinear interpolation.
*
*     Xi - stream-wise local coordinate of the estimation point.
*     Eta - depth-wise local coordinate of the estimation point.
*     dH - global depth corresponding to a local depth of 1 at the
*          estimation point.
*
*     PreviousBranch - branch of previous estimation point.
*     PreviousX      - int( X location ) at previous estimation point.
*     PreviousH100   - int( 100.0*depth ) at previous estimation point.

*     WriteIntrp - index, if .TRUE., write interpolated properties
*                  tables at computational locations.
*     HermiteBtm - index, if .TRUE., interpolate bottom elevation
*                  using hermites.

*===== BOF chcxtbl.inc ================================================
