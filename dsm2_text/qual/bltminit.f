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

      SUBROUTINE BLTMINIT

C     + + + PURPOSE + + +
C     This is used for initializing all of the variables inside BLTM,
C     Also read the input data

C     + + + LOCAL VARIABLE DEFINITIONS + + +
C     A(I)       average area in each subreach I (sq m)
C     CJ(L)      conc. of L at junction M
C     DQQ(N)     dispersion factor for branch N (D/U*U*DT)
C     DQV       minimum dispersive velocity (m/s or ft/s)
C     DT      time step size (hours)
C     DVD(N)     unknown volume in outflow at d/s end
C     DVU(N)     unknown volume in outflow at u/s end
C     FLOW(N,M,I)    flow field at grid I averaged over time step
C-----M=1 for discharge (cu m/s or ft/s)
C-----M=2 for cross sectional area (sq m or ft)
C-----M=3 for top width (m or ft)
C-----M=4 for trib. flow (cu m/s or ft/s)
C     DX(N,I)     length of subreach I in branch N
C     GPDC(L,K,N)    increase of L in parcel N,K due to reaction LR(L)
C     GPDF(L,K,N)    increase of L in parcel N,K due to dispersion
C     GPH(N,K)     time in hours since parcel K entered branch N
C     GPT(L,K,N)     concentration of constituent L in parcel N,K
C     GPTD(N,L)    flux of L at ds end of branch N
C     GPTI(L,K,N)    conc. of L as parcel K entered branch N
C     GPTR(L,K,N)    increase of L in parcel N,K due to tribs.
C     GPTU(N,L)    flux of L at us end of branch N
C     GPV(N,K)     volume of parcel K in branch N
C     GTRIB(L,I,N)   conc. of L in trib. at grid I of branch N
C-----(tribs can not occur at first or last grid of branch)
C     GVU(N,I)     volume of parcel u/s of grid I in branch N
C     HR      hour of the day
C     IDAY    days since model started
C     IENG    input units: 0=metric (except river miles), 1=english
C     INX     number of subreaches
C     IOUT(N,I)    flag (1 = output) for grid I in branch N
C     IPPR(N)      initial number of parcels per reach in branch N
C     IPX(K)     subreach in  which the u/s boundary of parcel K is located
C     IRC     code for reading data in FINK (1=read data, 0=no read)
C     ITDDS   tdds use (0=no, 1=flow only, 2=flow and boundary conditions)
C     JTIME       time step
C     JCD(M)     code for junction mixing (0=mixed, 2=not mixed)
C     JNCD(N)    d/s junction no. for branch N (number interior first)
C     JNCU(N)    u/s junction no. for branch N (number interior first)
C     JGO     number of time steps between output prints for grids
C     JPO     number of time steps between output prints for parces
C     JTS     number of time steps from midnight to start of model
C     K       parcel number
C     KAI(I)     parcel at grid I
C     L       constituent number
C     LABEL(10)   name of constituents (4 letters max)
C     LR(L)      index denoting that the decay of constituent L
C-----due to the presence of constituent LR(L) is tracked
C     NBC     number of boundary conditions,
C     NBRCH   number of branchs
C     NEQ     number of equations (constituents)
C     NHR     number of time steps to be modeled
C     NIPX(N,K)    subreach in which the upstream boundary of parcel N,K is
C     NJNCT   number of interior junctions
C     NKAI(N,I)    parcel at branch N, grid I
C     NS(N)      number of parcels in branch N
C     NXSEC(N)   number of grids in branch N
C     PDC(L,K)     change in initial concentration due to a specific reaction
C     PDF(L,K)     change in initial concentration due to dispersion
C     PH(K)      time parcel K entered reach in hours from day 0
C     PT(L,K)      conc. of constituent L in parcel K
C     PTD(L)     conc. of constituent L in parcel K, d/s, avg'd over time step
C     PTI(L,K)     initial concentration of constituent L in parcel K
C     PTR(L,K)     change in initial concentration due to tributary inflow
C     PTU(L)     conc. of constituent L in parcel K, u/s, avg'd over time step
C     PV(K)      volume of parcel K
C     PX(K)      location of upstream boundry of parcel in grid units
C     Q(I)   river flow at grid I (cu m/s or ft/s), trib inflow occurs just u/s of grid
C     QI     minimum flow of interest (flows<QI are considered zero).
C     QT(I)      tributary inflow at grid I (cu m/s or ft/s) enter just u/s of grid
C     RPT(L)     initial concentration of L in reach
C     TITLE(20)   title of program (80 characters)
C     TRIB(L,I)    concentration of constituent L in trib at grid I
C-----(tribs can not occur at first or last grid)
C     VJ      inflow volume to junction M
C     VU(I)      volume of parcel u/s of grid I
C     W(I)       average top width in subreach I (m or ft)
C     X(N,I)       distance of grid I from u/s of branch N (input in miles)
C     XFACT        conversion from miles to feet or meters, depending on IENG

C***  BEGIN DIMENSIONING DEFINITION

C     NOBR     Maximum number of branches allowed in model
C     MAX_NODES     Maximum number of internal junctions allowed in model
C     NOSC     Maximum number of cross sections (grids) allowed in branch
C     NOPR     Maximum number of parcels allowed in branch
C-----(NOPR should be at least 20 + 2 times NOSC)

      IMPLICIT NONE
      INCLUDE 'param.inc'
      INCLUDE '../input/fixed/common.f'
      INCLUDE '../input/time-varying/common_tide.f'
      INCLUDE 'bltm1.inc'
      INCLUDE 'bltm3.inc'
      INCLUDE 'bltm2.inc'

C     + + + LOCAL VARIABLES + + +C

      INTEGER I,I1,II,INX,
     &     JN,K,L,M, NN,
     &     IPPR(NOBR),IENG,KK
      REAL DX(NOBR,NOSC),
     &     RPT(NOSC,MAX_CONSTITUENT),XFACT

      LOGICAL RESTART           ! true if restart input filename given
      INTEGER LOB
      INTEGER XNO
      REAL XLENGTH
      if (print_level .ge. 2) then
         open (ihout,file='heat.out')
         open (irhout,file='resheat.out')
         open (idoout,file='do.out')
         open (irdoout,file='resdo.out')
         open (ialgout,file='alg.out')
         open (ibodout,file='bod.out')
         open (iralgout,file='resalg.out')

         write(ihout,113)
         write(irhout,113)
         write(idoout,116)
         write(irdoout,117)
         write(ialgout,118)
         write(iralgout,119)
         write(ibodout,111)


 113     format(//,40x,'heat components'/)
         
         write(ihout,114)
         write(irhout,115)
 114     format (' Date     Hour  Br     SWsolar  LWsolar   BackRad    Evap    Conduct     Net heat   Temp    E (in/mon)'/)
 115     format (' Date    Hour  ResNo    SWsolar  LWsolar   BackRad   Evap     Conduct    Net heat   Temp    E (in/mon)'/)
 116     format(/,'Date    Hour   Br     osf     oss    k2/day do-sat     DO    reaer   veloci   k2min   benth  kwind'/)
 117     format(/,'Date    Hour  ResNo   osf     oss    k2/day  do-sat    DO    reaer  velocity    k2min   benth, depth'/)
 118     format(/,'Date    Hour   ChNo    DO    algae   BOD  NH3    NO3    PO4     Temp     grw     solar   FL    FN    FP   mort'/)
 119     format(/,'Date    Hour   ResNo    DO     algae   BOD  NH3    NO3    PO4     Temp     grw     solar   FL    Depth   mort'/)
 111     format(/,'Date    Hour   ChNo    DO     BOD      xk      thetadjust   set'/)
      endif
C     + + + INPUT FORMATS + + +
 1000 FORMAT (L1,20A4)
 1010 FORMAT (10X,10I7)
 1020 FORMAT (10X,10F7.2)
 1030 FORMAT (10X,I7,3X,A5)
 1040 FORMAT (10X,I7,F7.2,3I7,F10.0)
 1050 FORMAT (10X,F7.3,I7,F7.3,10F6.3)
 1055 FORMAT (24X,F7.1,10F6.2)
 1100 FORMAT (15X,4E18.5)
 1110 FORMAT (3I5)
 1120 FORMAT (8f10.4)
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (20X,20A4,/)
 2040 FORMAT (' BOUNDARY CONDITIONS READ FROM TDDB')
 2050 FORMAT (' FLOW FIELD READ FROM BLTM.FLW')
 2060 FORMAT (' FLOW FIELD READ FROM TDDB')

C     zero arrays and preliminaries

      DO 5 N=1,MAX_NODES
         JCD(N)=0
 5    CONTINUE
      DO 60 N=1,NOBR
         IPPR(N)=1
         DO 30 I=1,NOSC
            GVU(N,I)=0.0
            NKAI(N,I)=0
            DO 10 M=1,4
               FLOW(N,M,I)=0.0
 10         CONTINUE
            IOUT(N,I)=0
            DX(N,I)=0.0
            X(N,I)=0.0
            DO 20 L=1,MAX_CONSTITUENT
               GTRIB(L,I,N)=0.0
 20         CONTINUE
 30      CONTINUE
         DO 50 K=1,NOPR
            NIPX(N,K)=0
            GPV(N,K)=0.0
            DO 40 L=1,MAX_CONSTITUENT
               GPT(L,K,N)=0.0
 40         CONTINUE
 50      CONTINUE
 60   CONTINUE

C     read common input

      NHR=(END_JULMIN-START_JULMIN)/TIME_STEP
      JTS=0
      JGO=400
      JPO=48
      IENG=1
      DQV=0.

      DT=FLOAT(TIME_STEP)/60.

      JHR=INT(1./DT)
      IF(ABS(JHR*DT - 1.) .GT. 1.E-4)THEN
         PRINT*,' DT=',DT
         PRINT*,' SELECT A DELTA T THAT WOULD BE DIVISIBLE WITH 1 HOUR'
         call exit(2)
      ENDIF

      IF (IO_FILES(QUAL,IO_RESTART,IO_READ).USE) THEN ! restart input file requested
         RESTART=.TRUE.
         call restart_file(IO_READ)
      ELSE                      ! no restart input file
         RESTART=.FALSE.
         DO I=1,NRES
            DO L=1,NEQ
               CRES(I,L)=INIT_CONC
            ENDDO
         ENDDO
      ENDIF

      DO 120 N=1,NBRCH
         NXSEC(N)=2
         IPPR(N)=8

C--------Fixme: Line Below assumes rectangular prismatic channel

         NN=INT2EXT(N)
         XLENGTH=CHAN_GEOM(NN).LENGTH
         XNO=CHAN_GEOM(NN).XSECT(1)
         B(N)=XSECT_GEOM(XNO).WIDTH

 991     FORMAT(I5,2X,2I5)
C--------Figure out the largest junction id#

         IF(NJUNC.LT.JNCU(N)) NJUNC=JNCU(N)
         IF(NJUNC.LT.JNCD(N)) NJUNC=JNCD(N)

         JUNCFLG(JNCU(N))=1
         JUNCFLG(JNCD(N))=1

         IF (IPPR(N) .LE. 0) IPPR(N)=1
         IF (IPPR(N)*(NXSEC(N)-1) .GT. (NOPR-2))
     &        IPPR(N)=(NOPR-2)/(NXSEC(N)-1)
         MAXPARCEL(N)=NOPR-2*(NXSEC(N)-1)
         I1=NXSEC(N)
         XFACT=5280.00

C--------XLENGTH=FLOAT(INT(XLENGTH/XFACT*1000.+0.5))/1000.
         XLENGTH=XLENGTH/XFACT  !NEEDS TO BE CHANGED LATER

C--------Modify number of parcels

         LOB=XLENGTH*XFACT/B(N)

         IF(LOB.GT.(NOPR-2))LOB=NOPR-2
         IF(LOB.LT.8)LOB=8
         IPPR(N)=LOB/(I1-1)

         DO I=1,I1
            IF(I.EQ.1)THEN
               X(N,I)=0.
            ELSEIF(I.EQ.2)THEN
               X(N,I)=XLENGTH
            ELSE
               write(unit_error,*) 'Software problem #1 in bltminit.f'
               call exit(2)
            ENDIF
            IF(I.NE.I1)THEN
               DO L=1,NEQ
                  IF(RESTART)THEN
                     RPT(I,L)=CSTART(N,L)
                  ELSE
                     RPT(I,L)=INIT_CONC
                  ENDIF
               ENDDO
            ELSE
               DO L=1,NEQ
                  RPT(I,L)=0.
               ENDDO
            ENDIF
            IF (I .LT. I1) THEN
               DO II=(I-1)*IPPR(N)+1,I*IPPR(N)
                  DO L=1,NEQ
                     GPT(L,II,N)=RPT(I,L)
                  ENDDO
               ENDDO
            END IF
         ENDDO
 120  CONTINUE

      DO N=1,NBRCH
         JN=JNCU(N)
         NUMUP(JN)=NUMUP(JN)+1
         LISTUP(JN,NUMUP(JN))=N

         JN=JNCD(N)
         NUMDOWN(JN)=NUMDOWN(JN)+1
         LISTDOWN(JN,NUMDOWN(JN))=N
      ENDDO

      JULMIN=START_JULMIN
      call read_mult_tide
      CALL INTERPX

C     make preliminary compt. and write init.conditions
      XFACT=5280.00
      DO 230 N=1,NBRCH
         INX=NXSEC(N)-1
         DO 210 I=1,INX
            DX(N,I)=(X(N,I+1)-X(N,I))*XFACT
            DO 205 KK=1,IPPR(N)
               K=(I-1)*IPPR(N)+KK
               NIPX(N,K)=I
               IF (KK .EQ. 1) NIPX(N,K)=I-1
               PX(K,N)=FLOAT(I)+FLOAT(KK-1)/FLOAT(IPPR(N))
               GPV(N,K) =Achan_Avg(N)*DX(N,I)/FLOAT(IPPR(N))
 205        CONTINUE
            if(N.le.5) write(*,*) ' N & Achan Average=', N, Achan_Avg(N)
            NKAI(N,I)=1+(I-1)*IPPR(N)
            GVU(N,I)=0.0
 210     CONTINUE
         NIPX(N,1)=1
         NS(N)=INX*IPPR(N)
         NKAI(N,NXSEC(N))=NS(N)
         GVU(N,NXSEC(N))=GPV(N,NS(N))
         IF(JPO.EQ.0)GO TO 230
         JTIME=0
 230  CONTINUE
      QI=1.

c-----Check for missing junction#'s
      CALL CHECKERROR
c-----create the lookup table for temperature factors
      if (no_of_nonconserve_constituent .gt. 0) then

         CALL TEMPFACTORS(-100.)
      end if
 
      RETURN
      END
