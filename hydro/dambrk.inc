C!    Copyright (C) 1996, 1997, 1998 State of California,
C!    Department of Water Resources.
C!
C!    Delta Simulation Model 2 (DSM2): A River, Estuary, and Land
C!    numerical model.  No protection claimed in original FOURPT and
C!    Branched Lagrangian Transport Model (BLTM) code written by the
C!    United States Geological Survey.  Protection claimed in the
C!    routines and files listed in the accompanying file "Protect.txt".
C!    If you did not receive a copy of this file contact Dr. Paul
C!    Hutton, below.
C!
C!    This program is licensed to you under the terms of the GNU General
C!    Public License, version 2, as published by the Free Software
C!    Foundation.
C!
C!    You should have received a copy of the GNU General Public License
C!    along with this program; if not, contact Dr. Paul Hutton, below,
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
C!    Dr. Paul Hutton
C!    California Dept. of Water Resources
C!    Division of Planning, Delta Modeling Section
C!    1416 Ninth Street
C!    Sacramento, CA  95814
C!    916-653-5601
C!    hutton@water.ca.gov
C!
C!    or see our home page: http://wwwdelmod.water.ca.gov/

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
