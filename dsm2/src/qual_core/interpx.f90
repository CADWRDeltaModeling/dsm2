!!<license>
!!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
!!    Department of Water Resources.
!!    This file is part of DSM2.

!!    The Delta Simulation Model 2 (DSM2) is free software:
!!    you can redistribute it and/or modify
!!    it under the terms of the GNU General Public License as published by
!!    the Free Software Foundation, either version 3 of the License, or
!!    (at your option) any later version.

!!    DSM2 is distributed in the hope that it will be useful,
!!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!!    GNU General Public License for more details.

!!    You should have received a copy of the GNU General Public License
!!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
!!</license>

module qual_interpx
      use qual_param
      use bltm
      use mod_qual_node
contains

      SUBROUTINE INTERPX

!-----FLOW AND STAGE IS GIVEN EVERY HOUR, AND ONLY AT
!-----THE LANDWARD AND SEAWARD ENDS OF A CHANNEL

!-----THIS SUBROUTINE INTERPOLATES FLOW AND STAGE (FLOW AREA)
!-----FOR ALL THE CHANNELS AND ALL SECTIONS.
      use common_tide
      use network
      IMPLICIT NONE

!-----LOCAL VARIABLES

      INTEGER I1,I,N
      REAL*8 DXFACTOR,QLAND,QSEA
      REAL*8 ALAND,ASEA

      DO N=1,NBRCH
         I1=NXSEC(N)
         QLAND=QCHAN(1,N)
         QSEA =QCHAN(2,N)
!--------YLAND=YCHAN(1,N)
!--------YSEA =YCHAN(2,N)
         ALAND=ACHAN(1,N)
         ASEA =ACHAN(2,N)

         DO I=1,I1
            DXFACTOR=dble(I-1)/dble(I1-1)
            FLOW(N,1,I)=QLAND+(QSEA-QLAND)*DXFACTOR
!-----------FLOW(N,2,I)=(YLAND+(YSEA-YLAND)*DXFACTOR)*B(N)
            FLOW(N,2,I)=(ALAND+(ASEA-ALAND)*DXFACTOR)
            FLOW(N,3,I)=B(N)    ! This needs to be changed later.
            FLOW(N,4,I)= 0.
         ENDDO
      ENDDO
      CALL BALANCEFLO
      RETURN
      END

      SUBROUTINE BALANCEFLO

!-----This subroutine distributes any unbalanced flow
!-----in a junction to the connecting channels
      use IO_Units
      use grid_data
      use common_tide
      use runtime_data
      implicit none

!-----Local variables

      integer jn,kk, I, n
      real*8 objflow,massrate(max_constituent) ! flow and massrate at object
      real*8 totflo, totabsflo
      real*8, parameter :: REL_CONTINUITY_TOL =1.D-4 ! fractional node imbalance allowed
      real*8, parameter :: ABS_CONTINUITY_TOL =5.D-3 ! exception for small absolute flows

      do 100 jn=1,nnodes
         if (node_geom(jn).qual_int) then
!-----------check if it is an ocean (stage) boundary
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

!-----------external, internal flows, and rservoir-node flows
            call node_rate(jn,TO_OBJ,0,objflow,massrate)
            totflo=totflo+objflow
            call node_rate(jn,FROM_OBJ,0,objflow,massrate)
            totflo=totflo+objflow
            if(totabsflo .NE. 0.d0)then
               if(abs(totflo)/totabsflo .GT. REL_CONTINUITY_TOL &
                   .and. abs(totflo) .gt. ABS_CONTINUITY_TOL )then !
                  if (julmin .le. start_julmin+tide_files(1).interval) then
                  write(unit_screen,2010) current_Date, &
                      node_geom(jn).node_ID,totflo
                  write(unit_output,2010) current_Date, &
                      node_geom(jn).node_ID,totflo
	            else  ! problem is after initialization, might be more serious
                  write(unit_error,2010) current_Date, &
                      node_geom(jn).node_ID,totflo
			    endif
 2010             format('Continuity problem: ',a,'  node ',I3, &
                      '  net flow=',F14.6)
                  if (julmin .le. start_julmin+tide_files(1).interval) &
     				write(unit_screen,747)
                  if (julmin .le. start_julmin+tide_files(1).interval) &
     				write(unit_output,747)
 747              format('If imbalance continues past first tideblock investigate.')
               endif

!--------------Now distribute any imbalance proportional to the flows
               do kk=1,numup(jn)
                  n=listup(jn,kk)
                  flow(n,1,1)=flow(n,1,1)+ &
                      totflo*abs(flow(n,1,1))/abs(totabsflo)
               enddo

               do kk=1,numdown(jn)
                  n=listdown(jn,kk)
                  flow(n,1,nxsec(n))=flow(n,1,nxsec(n))- &
                      totflo*abs(flow(n,1,nxsec(n)))/ABS(totabsflo)
               enddo
            endif
         endif
 100  enddo

      return
      end
end module qual_interpx