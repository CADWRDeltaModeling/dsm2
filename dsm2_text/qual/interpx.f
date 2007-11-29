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


      SUBROUTINE INTERPX

C-----FLOW AND STAGE IS GIVEN EVERY HOUR, AND ONLY AT
C-----THE LANDWARD AND SEAWARD ENDS OF A CHANNEL

C-----THIS SUBROUTINE INTERPOLATES FLOW AND STAGE (FLOW AREA)
C-----FOR ALL THE CHANNELS AND ALL SECTIONS.

      IMPLICIT NONE
      INCLUDE 'param.inc'
      INCLUDE '../input/fixed/common.f'
      INCLUDE '../input/time-varying/common_tide.f'
      INCLUDE 'bltm1.inc'
      INCLUDE 'bltm3.inc'
      INCLUDE 'bltm2.inc'

C-----LOCAL VARIABLES

      INTEGER I1,I
      REAL DXFACTOR,QLAND,QSEA
C-----REAL YLAND,YSEA
      REAL ALAND,ASEA

      DO N=1,NBRCH
         I1=NXSEC(N)
         QLAND=QCHAN(N,1)
         QSEA =QCHAN(N,2)
C--------YLAND=YCHAN(N,1)
C--------YSEA =YCHAN(N,2)
         ALAND=ACHAN(N,1)
         ASEA =ACHAN(N,2)

         DO I=1,I1
            DXFACTOR=FLOAT(I-1)/FLOAT(I1-1)
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
      
