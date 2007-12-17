C!    Copyright (C) 1996, 1997, 1998 State of California,
C!    Department of Water Resources.

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

      SUBROUTINE BALANCEFLO

C-----This subroutine distributes any unbalanced flow
C-----in a junction to the connecting channels
      use IO_Units
      implicit none
      include 'param.inc'
      include '../fixed/common.f'
      include '../hdf_tidefile/common_tide.f'
      include 'bltm1.inc'
      include 'bltm3.inc'
      include 'bltm2.inc'

C-----Local variables

      integer jn,kk, I
      real*8 objflow,massrate(max_constituent) ! flow and massrate at object
      real*8 totflo, totabsflo
      real*8, parameter :: REL_CONTINUITY_TOL =1.D-4 ! fractional node imbalance allowed
      real*8, parameter :: ABS_CONTINUITY_TOL =5.D-2 ! exception for small absolute flows

      do 100 jn=1,nnodes
         if (node_geom(jn).qual_int) then
C-----------check if it is an ocean (stage) boundary
            do i=1,nstgbnd
               IF (jn .eq. stgbnd(i).node) GOTO 100
            enddo
            totflo=0.0D0
            totabsflo=0.0D0
            do kk=1,numup(jn)
               n=listup(jn,kk)
               totflo=totflo-flow(n,1,1)
               totabsflo=totabsflo+abs(flow(N,1,1))
            enddo

            do kk=1,numdown(jn)
               n=listdown(jn,kk)
               totflo=totflo+flow(N,1,nxsec(n))
               totabsflo=totabsflo+abs(flow(n,1,nxsec(n)))
            enddo

c-----------external, internal flows, and rservoir-node flows
            call node_rate(jn,TO_OBJ,0,objflow,massrate)
            totflo=totflo+objflow
            call node_rate(jn,FROM_OBJ,0,objflow,massrate)
            totflo=totflo+objflow
            if(totabsflo.NE.0)then
               if(abs(totflo)/totabsflo .GT. REL_CONTINUITY_TOL
     &              .and. abs(totflo) .gt. ABS_CONTINUITY_TOL )then !
                  if (julmin .le. start_julmin+tide_files(1).interval) then
                  write(unit_screen,2010) current_Date,
     &                 node_geom(jn).node_ID,totflo
                  write(unit_output,2010) current_Date,
     &                 node_geom(jn).node_ID,totflo
	            else  ! problem is after initialization, might be more serious
                  write(unit_error,2010) current_Date,
     &                 node_geom(jn).node_ID,totflo
			    endif
 2010             format('Continuity problem: ',a,'  node ',I3,
     &                 '  net flow=',F14.6)
                  if (julmin .le. start_julmin+tide_files(1).interval) 
     &				write(unit_screen,747)
                  if (julmin .le. start_julmin+tide_files(1).interval) 
     &				write(unit_output,747)
 747              format('If imbalance continues past first tideblock investigate.')
               endif

C--------------Now distribute any imbalance proportional to the flows
               do kk=1,numup(jn)
                  n=listup(jn,kk)
                  flow(n,1,1)=flow(n,1,1)+
     &                 totflo*abs(flow(n,1,1))/abs(totabsflo)
               enddo

               do kk=1,numdown(jn)
                  n=listdown(jn,kk)
                  flow(n,1,nxsec(n))=flow(n,1,nxsec(n))-
     &                 totflo*abs(flow(n,1,nxsec(n)))/ABS(totabsflo)
               enddo
            endif
         endif
 100  enddo

      return
      end
