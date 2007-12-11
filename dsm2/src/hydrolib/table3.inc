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
