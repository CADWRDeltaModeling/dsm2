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


      SUBROUTINE BALANCEFLO

C-----This subroutine distributes any unbalanced flow
C-----in a junction to the connecting channels

      IMPLICIT NONE
      INCLUDE 'param.inc'
      INCLUDE '../input/fixed/common.f'
      INCLUDE 'bltm1.inc'
      INCLUDE 'bltm3.inc'
      INCLUDE 'bltm2.inc'

C-----Local variables

      INTEGER JN,KK, I
      real objflow,massrate(max_constituent) ! flow and massrate at object
      REAL*8 TOTFLO, TOTABSFLO

      DO 100 JN=1,NJNCT
C--------check if it is an ocean (stage) boundary
         do i=1,nstgbnd
            IF (nodequal2dsm(jn) .eq. stgbnd(i).node) GOTO 100
         enddo
         TOTFLO=0.0D0
         TOTABSFLO=0.0D0
         DO KK=1,NUMUP(JN)
            N=LISTUP(JN,KK)
            TOTFLO=TOTFLO-FLOW(N,1,1)
            TOTABSFLO=TOTABSFLO+ABS(FLOW(N,1,1))
c            If(NodeQual2dsm(Jn).eq.72)print*,"UP:",FLOW(N,1,1)
         ENDDO

         DO KK=1,NUMDOWN(JN)
            N=LISTDOWN(JN,KK)
            TOTFLO=TOTFLO+FLOW(N,1,NXSEC(N))
            TOTABSFLO=TOTABSFLO+ABS(FLOW(N,1,NXSEC(N)))
c          If(NodeQual2dsm(Jn).eq.72)print*,"DOWN:",FLOW(N,1,1)
         ENDDO

c--------external, internal flows, and rservoir-node flows
         call node_rate(jn,TO_OBJ,0,objflow,massrate)
         TOTFLO=TOTFLO+objflow
c          If(NodeQual2dsm(Jn).eq.72)print*,"FROM:",OBJFLOW
         call node_rate(jn,FROM_OBJ,0,objflow,massrate)
         TOTFLO=TOTFLO+objflow
c          If(NodeQual2dsm(Jn).eq.72)print*,"TO:",OBJFLOW
         IF(TOTABSFLO.EQ.0)GOTO 100

         IF(ABS(TOTFLO)/TOTABSFLO .GT. 1.0D-2)THEN
            WRITE(UNIT_ERROR,2010) CURRENT_DT,NODEQUAL2DSM(JN),TOTFLO
 2010       FORMAT('Continuity problem: ',a,'  node ',I3,
     &           '  net flow=',F10.2)
         ENDIF

C--------Now distribute any imbalance proportional to the flows
         DO KK=1,NUMUP(JN)
            N=LISTUP(JN,KK)
            FLOW(N,1,1)=FLOW(N,1,1)+
     &           TOTFLO*ABS(FLOW(N,1,1))/ABS(TOTABSFLO)
         ENDDO

         DO KK=1,NUMDOWN(JN)
            N=LISTDOWN(JN,KK)
            FLOW(N,1,NXSEC(N))=FLOW(N,1,NXSEC(N))-
     &           TOTFLO*ABS(FLOW(N,1,NXSEC(N)))/ABS(TOTABSFLO)
         ENDDO
 100  ENDDO

      RETURN
      END
