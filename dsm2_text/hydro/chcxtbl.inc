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


*===== BOF chcxtbl.inc ================================================
*   Version 93.01, January, 1993

*   Note: If used, this include must appear after "Network.inc".

      INTEGER     MaxTables, MaxLinesPerTable, MaxLines
      PARAMETER ( MaxTables = 5000,
     &     MaxLinesPerTable = 21,
     &     MaxLines = MaxLinesPerTable*MaxTables )

      INTEGER     FirstTable(MaxChannels), LastTable(MaxChannels)
      INTEGER     Lines(MaxChannels), Offset(MaxTables)
      REAL*8        XDistance(MaxTables), Datum(MaxTables)
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
