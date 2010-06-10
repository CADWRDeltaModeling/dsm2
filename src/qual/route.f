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

C     **************************  ROUTE  *************************************
   
      SUBROUTINE ROUTE
      Use IO_Units
      use common_qual
      use logging
      use runtime_data

C     + + + PURPOSE + + +

C     The purpose of this subroutine is to route through each branch.

C     NOSC     Maximum number of cross sections (grids) allowed in branch
C     NOPR     Maximum number of parcels allowed in branch
C     (NOPR should be at least 20 + 2 times NOSC)
C     MAX_CONSTITUENT     Maximum number of constituents allowed

      IMPLICIT NONE
      INCLUDE 'param.inc'
      INCLUDE 'bltm1.inc'
      INCLUDE 'bltm3.inc'
      INCLUDE 'bltm2.inc'
      include 'kinetic1.inc'

C     + + + DUMMY ARGUMENTS + + +

      real*8    DQMIN,VI,DQTINY

C     + + + ARGUMENT DEFINITIONS + + +
C     A(I)       average area in subreach I (sq m)
C     DQQ     dispersion factor D/(U*U*DT)
C     DQV     minimum dispersive velocity (m/s)
C     DT      time step size in hours
C     DVD     unknown volume of outflow at d/s end
C     DVU     unknown volume of outflow at u/s end
C     IPX(K)     subreach in which the u/s boundary of parcel K is located
C     IRC     code for reading data in FINK (1=read data, 0=no read)
C     Jtime       time step
C     JTS     number of time steps from midnight to start of model
C     KAI(I)     parcel at grid I
C     LR(L)      index denoting that the decay of constituent L due
C     to the presence of constituent LR(L) is tracked
C     NEQ     number of equations (constituents)
C     N     branch number
C     NS      number of parcels
C     NXSEC   number of Eulerian grids (subreaches in a branch)
C     PDC(L,K)     change in initial concentration due to a specific reaction
C     PDF(L,K)     change in initial concentration due to dispersion
C     PH(K)      time parcel K entered reach in hours from day 0
C     PT(L,K)      conc. of constituent L in parcel K
C     PTD(K)     conc. of constituent L in parcel K, d/s, avg'd over time step
C     PTI(L,K)     initial concentration of constituent L in parcel K
C     PTR(L,K)     change in initial concentration due to tributary inflow
C     PTU(L)     conc. of constituent L in parcel K, u/s, avg'd over time step
C     PV(K)      volume of parcel K
C     Q(I)   river flow at grid I (cu m/s),trib inflow occurs just u/s of grid
C     QI     minimum flow of interest (flows<QI are considered zero).
C     QT(I)      tributary inflow at grid I (cu m/s), enter just u/s of grid
C     TRIB(L,I)    concentration of constituent L in trib at grid I
C     (tribs can not occur at first or last grid)
C     VI   smallest volume of interest (VI=QI*DT)
C     VU(I)      volume of parcel upstream of grid I
C     W(I)       average top width in subreach I (m)

C     + + + LOCAL VARIABLES + + +
      INTEGER I,I1,IEXP,IFP,II,ILP,INX,K,K2,KK,KL,KS,KSML,L,
     &     ND(NOPR),NDMAX,NSL
      real*8    COF,COF1,DF(NOPR),DMF(NOPR),DQ(NOPR),
     &     PRDT(NOPR),PVT,RATIO,RDT,C(MAX_CONSTITUENT)
	real*8,parameter :: df_tol = 1.D-06
      real*8,parameter :: clog2=0.3010300

      real*8  RQ,CONCMIX,DTSEC,RNDMAX
      real*8  DIFFGPT(NOPR+1)
      !real*8  QPARCEL  Average of cross-section upstream and downstream of parcel
                      ! Hasn't been thought out for flow fields
                      ! with more than the NXSEC=2 assumption in DSM2
      integer KR, NN, NDD(NOPR+1)

      INTEGER JN,NSN,NXSECN
      LOGICAL NEWPARCEL(NOPR)


      real*8 objflow,massrate(max_constituent) ! object flow and massrates

C     + + + LOCAL VARIABLE DEFINITIONS + + +`
C     DF(K)  change in conc. of any constituent in parcel K due to dispersion
C     DMF(K)     the mass flux into parcel K at its u/s boundary
C     DQ(K)      flow volume between parcel K-1 and K due to velocity gradients
C     IEXP    exponent of 2 used to determine ND(K)
C     IFP     parcel at upstream boundary
C     ILP     parcel at d/s boundary
C     INX     number of subreaches
C     K       parcel number
C     KSML    parcel number of smallest parcel
C     L       constituent number
C     ND(K)      # sub-time steps needed at u/s bndry of parcel K for dispersion
C     NDMAX   maximum number of sub-time steps needed for dispersion
C     MX      subreach where parcel is located
C     NOPR    maximum number of parcels in net dimension statements
C     PRDT(K)    remaining time for decay in parcel K
C     RDT     time remaining for movement
C     X(I)       dist of grid I from u/s boundary (m), input as river mile

      DTSEC=DT*3600.
      DQTINY=0.00001*DTSEC
      VI=QI*DTSEC
      chan_res=1                ! pointer to channel for kinetic computations
      DO 490 N=1,NBRCH
C--------ROUTE BRANCHES
         I1=NXSEC(N)
         NXSECN=I1
         NSN=NS(N)
         DO K=1,NSN+2
            NEWPARCEL(K)=.FALSE.
         ENDDO
         JN=JNCU(N)
         DO 410 I=1,I1
            Q(I)=FLOW(N,1,I)
            A(I)=FLOW(N,2,I)
            W(I)=FLOW(N,3,I)
            QT(I)=FLOW(N,4,I)
 410     CONTINUE

C--------compute hydraulics statements

         INX=NXSECN-1
         NIPX(N,NSN+1)=NXSECN

C--------disperse constituents statements

C--------COMPUTE DQ'S

C--------No dispersion if only 1 parcel is left

         IF(NSN.LE.1)GOTO 150

         DO 20 K=2,NSN
            IF(GPV(N,K-1).GT.VI.AND.GPV(N,K).GT.VI)THEN
               MX=NIPX(N,K)
               !todo: this change to an average eliminates one-sidedness
               !(different answers depending on channel orientation
               !QPARCEL=(Q(MX)+Q(MX+1))/2.D0
               !DQ(K)=ABS(DQQ(N)*QPARCEL)
               DQ(K)=ABS(DQQ(N)*Q(MX))
               DQMIN=DQV*A(MX)*0.5
               IF(DQ(K).LT.DQMIN)DQ(K)=DQMIN
C--------------Changed from flow rate to volume
               DQ(K)=DQ(K)*DTSEC
            ELSE
               DQ(K)=0.0
            ENDIF
 20      CONTINUE
         DQ(1)=0.0
         DQ(NSN+1)=0.0

C--------determine number of subdivisions needed for each parcel

         NDMAX=0
         ND(1)=1
         ND(NSN+1)=1
         DO 30 K=2,NSN
            ND(K)=1
            IF(DQ(K).LT.DQTINY) GO TO 31
            IEXP=0
            RATIO=DQ(K)/MIN(GPV(N,K-1),GPV(N,K))
            IF(RATIO.GT.0.0)IEXP=INT(LOG10(RATIO/0.4)/CLOG2+1)
            IF(IEXP.GT.0)ND(K)=2**IEXP
 31         NDMAX=MAX(ND(K),NDMAX)
 30      CONTINUE
C--------Calculate DQ per sub-timestep. This makes thing go faster
         RNDMAX=dble(NDMAX)
         DO K=2,NSN+1
            DQ(K)=DQ(K)/RNDMAX
            NDD(K)=NDMAX/(ND(K))
         ENDDO

C--------compute new mass fluxes if needed and update all concentrations

         DMF(1)=0.0
         DO 120 L=1,NEQ
            DO K=1,NSN+1
               DF(K)=0.0
               DIFFGPT(K)=GPT(L,K,N)-GPT(L,K+1,N)
            ENDDO
            IF(NDMAX.EQ.1)THEN
               DO K=1,NSN
                  DMF(K+1)=DQ(K+1)*DIFFGPT(K)
                  IF(GPV(N,K).GT.VI) THEN
                     DF(K)=(DMF(K)-DMF(K+1))/GPV(N,K)
                  ELSE
                     DF(K)=0.0
                  END IF
                  IF (ABS(DF(K)).LT.1.0E-6) DF(K)=0.0
               ENDDO
            ELSE
               DO 110 NN=1,NDMAX
                  DO 100 K=1,NSN
                     IF (MOD(NN-1,NDD(K+1)).EQ.0) then
                        DMF(K+1)=DQ(K+1)*
     &                       (DIFFGPT(K)+DF(K)-DF(K+1))
                     ENDIF
                     IF(GPV(N,K).GT.VI) THEN
                        DF(K)=(DMF(K)-DMF(K+1))/GPV(N,K)+DF(K)
                     ELSE
                        DF(K)=0.0
                     END IF
                     IF (ABS(DF(K)).LT.1.0E-6) DF(K)=0.0
 100              CONTINUE
 110           CONTINUE
            ENDIF
            DO  K=1,NSN
               GPT(L,K,N)=GPT(L,K,N)+DF(K)
            ENDDO
 120     CONTINUE

C--------FINISHED DISPERSION

C--------set inflow boundary values statements

 150     IF (Q(1).LE.0.0) GO TO 240

C--------FLOW IN UPSTREAM BOUNDARY
         NEWPARCEL(1)=.TRUE.
         NS(N)=NS(N)+1
         NSN=NS(N)
         NSL=NSN-1
         DO 210 K2=1,NSL
            K=NSN+1-K2
            KK=K-1
            NIPX(N,K)=NIPX(N,KK)
            GPV(N,K)=GPV(N,KK)
            DO 200 L=1,NEQ
               GPT(L,K,N)=GPT(L,KK,N)
 200        CONTINUE
 210     CONTINUE
         DO 220 I=1,NXSECN
            NKAI(N,I)=NKAI(N,I)+1
 220     CONTINUE
         NIPX(N,1)=1
         GPV(N,1)=Q(1)*DTSEC
         GVU(N,1)=0.0
         NKAI(N,1)=1
         PRDT(1)=0.0
!         IF(node_geom(JN).qual_int .OR. NCONRES(JN).EQ.0)THEN
C-----------Upstream junction is not at the boundary
C-----------Or if it is, there are no reservoirs connected
            !todo: GPTU seems to always be zero here
            DO L=1,NEQ
               ! todo: analyze if this is really ever non-zero or necessary.
               !       same for downstream case. If you are reading this in
               !       2011 please go ahead and delete this assertion
               if(GPTU(L,N) .ne. 0.d0) then
                   print*,"GPTU != 0, please report to DSM2 maintanence team"
               end if
               GPT(L,1,N)=GPTU(L,N)
            ENDDO
!         ELSEIF( (.not. node_geom(JN).qual_int) .AND. NCONRES(JN).GE.1)THEN
!C-----------Upstream junction is a boundary node
!C-----------and a reservoir is connected
!            !todo: ext_node node_rate is wrong time (b4 node mixing loop)
!            call node_rate(jn,TO_OBJ,0,objflow,massrate)
!            DO L=1,NEQ
!               IF(objflow .NE. 0.)THEN
!                  CONCMIX=massrate(l) / objflow
!               ELSE
!                  CONCMIX=0.
!               ENDIF
!               GPT(L,1,N)=CONCMIX
!            ENDDO
!         ENDIF
         GO TO 250
C--------flow at upstream boundary is 0 or negative
 240     CONTINUE
         PRDT(1)=DT
 250     CONTINUE
         IF (Q(NXSECN).GE.0.0) GO TO 270
C--------flow into downstream boundary
         NS(N)=NS(N)+1
         NSN=NS(N)
         NEWPARCEL(NSN)=.TRUE.
         NIPX(N,NSN)=NXSECN-1
         GPV(N,NSN)=-Q(NXSECN)*DTSEC
         NKAI(N,NXSECN)=NSN
         PRDT(NSN)=0.0
         GVU(N,NXSECN)=GPV(N,NSN)
         DO 260 L=1,NEQ
            if(GPTU(L,N) .ne. 0.d0) then
               print*,"GPTD != 0, please report to DSM2 maintanence team"
            end if
            GPT(L,NSN,N)=GPTD(L,N)
 260     CONTINUE
         GO TO 280
 270     CONTINUE
         PRDT(NSN)=DT
 280     CONTINUE
         NSL=NSN-1
         DO 290 K=2,NSL
            PRDT(K)=DT
 290     CONTINUE

C--------move parcels statements

C--------move parcels passing donwstream or stationary

         iskip = 1

         DO 320 I=2,NXSECN
            RDT=DT
            DTSUB=RDT
            K=NKAI(N,I)
            MX=I-1
            RQ=Q(I)-QT(I)
            IF(Q(I).LT.0.0.AND.RQ.LE.0.0)GO TO 320
            IF(RQ.LE.0) GO TO 310
 300        CONTINUE
            DTSUB=GVU(N,I)/(RQ*3600.0)
            IF(DTSUB.LT.0.0)DTSUB=0.0
            K=NKAI(N,I)
            MX=I-1
            IF (DTSUB.GE.RDT) GO TO 310
C-----------parcel passed grid
            IF(K.EQ.1)THEN
               WRITE(UNIT_ERROR,*) ' ERROR... CHANNEL: ',
     &              chan_geom(N).chan_no,' DRIED UP!'
               WRITE(UNIT_ERROR,*) '          TIME: ',current_date
               call exit(2)
            ENDIF
            RDT=RDT-DTSUB
            IF(Q(I).LT.0.0)DTSUB=DTSUB*RQ/(RQ-Q(I))
            IF (PRDT(K).LT.DTSUB) DTSUB=PRDT(K)
            DO L = 1, NEQ
               C(L) = GPT(L,K,N)
            END DO
            IF (DTSUB.GT.0 .AND. no_of_nonconserve_constituent. gt. 0
     &           .and. .not.newparcel(k)) then
               call rate_chanres(n)
	           call kinetic(c)
            end if
            IF(MASS_TRACKING)THEN
               DO L = 1, NEQ
                  AMOUNTDECAYED(L)=AMOUNTDECAYED(L)+
     &                 (GPT(L,K,N)-C(L))*GPV(N,K)
               ENDDO
            ENDIF
            DO L = 1, NEQ
               GPT(L,K,N) = C(L)
            END DO
            PRDT(K)=PRDT(K)-DTSUB
            NIPX(N,K)=I

            NKAI(N,I)=K-1
            GVU(N,I)=GPV(N,K-1)
            GO TO 300
C-----------did not pass grid
 310        CONTINUE
            IF(Q(I).LT.0.0)DTSUB=RDT*RQ/(RQ-Q(I))
            IF(DTSUB.GT.RDT)DTSUB=RDT
            IF (PRDT(K).LT.DTSUB) DTSUB=PRDT(K)
            DO L = 1, NEQ
               C(L) = GPT(L,K,N)
            END DO
            IF (DTSUB.GT.0 .AND. no_of_nonconserve_constituent. gt. 0
     &           .and. .not.newparcel(k)) then
               call rate_chanres(n)
	           call kinetic(c)
            end if
            IF(MASS_TRACKING)THEN
               DO L = 1, NEQ
                  AMOUNTDECAYED(L)=AMOUNTDECAYED(L)+
     &                 (GPT(L,K,N)-C(L))*GPV(N,K)
               ENDDO
            ENDIF

            DO L = 1, NEQ
               GPT(L,K,N) = C(L)
            END DO
            PRDT(K)=PRDT(K)-DTSUB
            GVU(N,I)=GVU(N,I)-RQ*RDT*3600.
 320     CONTINUE
C--------move parcels going upstream
         DO 350 II=2,NXSECN
            I=NXSECN+1-II
            IF (Q(I).GE.0.0) GO TO 350
            RDT=DT
            MX=I
 330        CONTINUE
            K=NKAI(N,I)
            RQ=Q(I)-QT(I)
            DTSUB=(GVU(N,I)-GPV(N,K))/(Q(I)*3600.0)
            IF(DTSUB.LT.0.0)DTSUB=0.0
            IF (DTSUB.GE.RDT) GO TO 340
C-----------parcel passed grid
            RDT=RDT-DTSUB
            IF(RQ.GT.0.0)DTSUB=DTSUB*Q(I)/QT(I)
            IF (PRDT(K).LT.DTSUB) DTSUB=PRDT(K)
            DO L = 1, NEQ
               C(L) = GPT(L,K,N)
            END DO
            IF (DTSUB.GT.0 .AND. no_of_nonconserve_constituent. gt. 0
     &           .and. .not.newparcel(k)) then
               call rate_chanres(n)
	           call kinetic(c)
            end if
            IF(MASS_TRACKING)THEN
               DO L = 1, NEQ
                  AMOUNTDECAYED(L)=AMOUNTDECAYED(L)+
     &                 (GPT(L,K,N)-C(L))*GPV(N,K)
               ENDDO
            ENDIF
            DO L = 1, NEQ
               GPT(L,K,N) = C(L)
            END DO
            PRDT(K)=PRDT(K)-DTSUB
            NIPX(N,K+1)=I-1
            IF (I.EQ.1) NIPX(N,K+1)=1
            NKAI(N,I)=K+1
            GVU(N,I)=0.0
            GO TO 330
C-----------did not pass grid
 340        CONTINUE
            K=NKAI(N,I)
            DTSUB=RDT
            IF(RQ.GT.0.0)DTSUB=RDT*RQ/(RQ-Q(I))
            IF (PRDT(K).LT.DTSUB) DTSUB=PRDT(K)
            DO L = 1, NEQ
               C(L) = GPT(L,K,N)
            END DO
            IF (DTSUB.GT.0 .AND. no_of_nonconserve_constituent. gt. 0
     &           .and. .not.newparcel(k)) then
               call rate_chanres(n)
	           call kinetic(c)
            end if
            IF(MASS_TRACKING)THEN
               DO L = 1, NEQ
                  AMOUNTDECAYED(L)=AMOUNTDECAYED(L)+
     &                 (GPT(L,K,N)-C(L))*GPV(N,K)
               ENDDO
            ENDIF
            DO L = 1, NEQ
               GPT(L,K,N) = C(L)
            END DO
            PRDT(K)=0.0
            IF(RQ.LE.0.0) GVU(N,I)=GVU(N,I)-RQ*RDT*3600.0
 350     CONTINUE
C--------complete decay step
         DO 360 K=1,NSN
            IF (MX.GT.NXSECN-1) MX=NXSECN-1
            IF (MX.LT.1) MX=1
            DO L = 1, NEQ
               C(L) = GPT(L,K,N)
            END DO
            dtsub = prdt(k)
            IF (DTSUB.GT.0 .AND. no_of_nonconserve_constituent. gt. 0) then
               call rate_chanres(n)
               call kinetic(c)
            end if
            IF(MASS_TRACKING)THEN
               DO L = 1, NEQ
                  AMOUNTDECAYED(L)=AMOUNTDECAYED(L)+
     &                 (GPT(L,K,N)-C(L))*GPV(N,K)
               ENDDO
            ENDIF
            DO L = 1, NEQ
               GPT(L,K,N) = C(L)
            END DO
 360     CONTINUE

C--------compute outflow flux statements

         DO 400 L=1,NEQ
            GPTU(L,N)=0.0
            GPTD(L,N)=0.0
 400     CONTINUE

         IF (Q(1).GE.0.0) GO TO 430
         K=0
 411     CONTINUE
         K=K+1
         DVU(N)=GPV(N,K)
         IF (K.EQ.NKAI(N,1)) DVU(N)=GVU(N,1)
         DO 420 L=1,NEQ
            GPTU(L,N)=GPTU(L,N)+GPT(L,K,N)*DVU(N)
 420     CONTINUE
         IF (K.LT.NKAI(N,1)) GO TO 411

 430     CONTINUE
         IF (Q(NXSECN).GT.0.0) THEN
            DO K=NSN,NKAI(N,NXSECN),-1
               DVD(N)=GPV(N,K)
               IF (K.EQ.NKAI(N,NXSECN)) DVD(N)=GPV(N,K)-
     &              GVU(N,NXSECN)
               DO 450 L=1,NEQ
                  GPTD(L,N)=GPTD(L,N)+GPT(L,K,N)*DVD(N)
 450           CONTINUE
            ENDDO
         ENDIF
         IFP=NKAI(N,1)
         GPV(N,IFP)=GPV(N,IFP)-GVU(N,1)
         I=1
 470     CONTINUE
         I=I+1
         IF (NKAI(N,I).EQ.IFP) THEN
            GVU(N,I)=GVU(N,I)-GVU(N,1)
            GOTO 470
         ENDIF
         GVU(N,1)=0.0
         ILP=NKAI(N,NXSECN)
         GPV(N,ILP)=GVU(N,NXSECN)

C--------renumber parcels and combine statements

         NS(N)=ILP-IFP+1
         NSN=NS(N)
         IF (NSN.EQ.1) THEN
            IF(Q(NXSECN).GE.0.) DVU(N)=0.0
            IF(Q(1).LE.0.) DVD(N)=0.0
         ELSE
            DVU(N)=0.
            DVD(N)=0.
         ENDIF

         IF (IFP.EQ.1) GO TO 530
C--------RENUMBER PARCELS
         DO 510 K=1,NSN
            K2=K+IFP-1
            NIPX(N,K)=NIPX(N,K2)
            GPV(N,K)=GPV(N,K2)
            DO 500 L=1,NEQ
               GPT(L,K,N)=GPT(L,K2,N)
 500        CONTINUE
 510     CONTINUE
         DO 520 I=1,NXSECN
            NKAI(N,I)=NKAI(N,I)-(IFP-1)
 520     CONTINUE
 530     CONTINUE

C--------combine parcel statements

 600     CONTINUE

C--------Determine the smallest parcel
         KSML=1
         DO K=1,NSN
            IF (GPV(N,K).LE.GPV(N,KSML)) KSML=K
         ENDDO

         IF (NSN.LE.3.OR.GPV(N,KSML).GE.VI)THEN
            DO KR=2,NSN
               RATIO=GPV(N,KR)/GPV(N,KR-1)
               IF(RATIO.GE.25.0.OR.RATIO.LE.0.04)THEN
                  KSML=KR
                  IF(RATIO.GT.1.0)KSML=KR-1
                  GO TO 610
               ENDIF
            ENDDO
            IF (NSN.LT.MAXPARCEL(N)) GO TO 700
         ENDIF
 610     CONTINUE
         IF(MASS_TRACKING.AND.KSML.GT.NSN)THEN
            WRITE(UNIT_ERROR,*) ' PROBLEM KSML CHANNEL:',N,' KS=',KS,' KSML=',KSML
         ENDIF
         IF(KSML.EQ.1)THEN
            KS=1
         ELSEIF(KSML.EQ.NSN)THEN
            KS=NSN-1
         ELSE
            KS=KSML-1
            IF (GPV(N,KS).GT.GPV(N,KS+2)) KS=KSML
         ENDIF
         KL=KS+1
         I1=NIPX(N,KS)+1
         DO 650 I=I1,NXSECN
            IF (NIPX(N,KL).LT.I) THEN
               NKAI(N,I)=NKAI(N,I)-1
               IF (NKAI(N,I).EQ.KS) GVU(N,I)=GVU(N,I)+GPV(N,KS)
            ENDIF
 650     CONTINUE
         PVT=GPV(N,KS)+GPV(N,KL)
         IF (PVT.LE.0.0) PVT=VI
         COF=GPV(N,KS)/PVT
         COF1=1.0-COF
         GPV(N,KS)=GPV(N,KS)+GPV(N,KL)
         DO 660 L=1,NEQ
            GPT(L,KS,N)=GPT(L,KS,N)*COF+COF1*GPT(L,KL,N)
 660     CONTINUE
C--------renumber remaining parcels
         NS(N)=NS(N)-1
         NSN=NS(N)
         DO 680 K=KL,NSN
            NIPX(N,K)=NIPX(N,K+1)
            GPV(N,K)=GPV(N,K+1)
            DO 670 L=1,NEQ
               GPT(L,K,N)=GPT(L,K+1,N)
 670        CONTINUE
 680     CONTINUE
         IF (NSN.GE.MAXPARCEL(N)) GO TO 600
 700     CONTINUE
         iskip = 0

 490  CONTINUE
      RETURN
      END

