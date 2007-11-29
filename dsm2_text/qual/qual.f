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
   
      program watqual
  
c      USE DFLIB   !! <NT>

C-----************ MULTIPLE BRANCH ESTUARY TRANSPORT MODEL
c-----******************

C-----PROGRAM MBLTM

C-----+ + + PURPOSE + + +
C-----All boundry conditions represent the ave during the time step.
C-----The first BC represents cond. from time 0 to dt, for example.
C-----The Q,A,W are at the grid point with QT occuring just upstream
C-----of grid. Nxxx or Gxxx variables defined for all branches.

C-----+ + + LOCAL VARIABLE DEFINITIONS + + +
C-----A(I)       average area in each subreach I (sq m)
C-----CJ(L,M)      mass and conc. of L at junction M
C-----DQQ(N)     dispersion factor for branch N (D/U*U*DT)
C-----DQV       minimum dispersive velocity (m/s or ft/s)
C-----DT      time step size (hours)
C-----DVD(N)     unknown volume in outflow at d/s end
C-----DVU(N)     unknown volume in outflow at u/s end
C-----FLOW(N,M,I)    flow field at grid I averaged over time step
C-----M=1 for discharge (cu m/s or ft/s)
C-----M=2 for cross sectional area (sq m or ft)
C-----M=3 for top width (m or ft)
C-----M=4 for trib. flow (cu m/s or ft/s)
C-----DX(N,I)     length of subreach I in branch N
C-----GPDC(L,K,N)    increase of L in parcel N,K due to reaction LR(L)
C-----GPDF(L,K,N)    increase of L in parcel N,K due to dispersion
C-----GPH(N,K)     time in hours since parcel K entered branch N
C-----GPT(L,K,N)     concentration of constituent L in parcel N,K
C-----GPTD(L,N)    flux of L at ds end of branch N
C-----GPTI(L,K,N)    conc. of L as parcel K entered branch N
C-----GPTR(L,K,N)    increase of L in parcel N,K due to tribs.
C-----GPTU(L,N)    flux of L at us end of branch N
C-----GPV(N,K)     volume of parcel K in branch N
C-----GTRIB(L,I,N)   conc. of L in trib. at grid I of branch N
C-----(tribs can not occur at first or last grid of branch)
C-----GVU(N,I)     volume of parcel u/s of grid I in branch N
C-----HR      hour of the day
C-----IDAY    days since model started
C-----IENG    input units: 0=metric (except river miles), 1=english
C-----INX     number of subreaches
C-----IOUT(N,I)    flag (1 = output) for grid I in branch N
C-----IPPR(N)      initial number of parcels per reach in branch N
C-----IPX(K)     subreach in  which the u/s boundary of parcel K is located
C-----IRC     code for reading data in FINK (1=read data, 0=no read)
C-----ITDDS   tdds use (0=no, 1=flow only, 2=flow and boundary conditions)
C-----JTIME       time step
C-----JCD(M)     code for junction mixing (0=mixed, 2=not mixed)
C-----JNCD(N)    d/s junction no. for branch N (number interior first)
C-----JNCU(N)    u/s junction no. for branch N (number interior first)
C-----JGO     number of time steps between output prints for grids
C-----JPO     number of time steps between output prints for parces
C-----JTS     number of time steps from midnight to start of model
C-----K       parcel number
C-----KAI(I)     parcel at grid I
C-----L       constituent number
C-----LABEL(10)   name of constituents (4 letters max)
C-----LR(L)      index denoting that the decay of constituent L due to the presence of constituent LR(L) is tracked
C-----NBC     number of boundary conditions,
C-----NBRCH   number of branchs
C-----NEQ     number of equations (constituents)
C-----NHR     number of time steps to be modeled
C-----NIPX(N,K)    subreach in which the upstream boundary of parcel N,K is
C-----NJNCT   number of interior junctions
C-----NKAI(N,I)    parcel at branch N, grid I
C-----NS(N)      number of parcels in branch N
C-----NXSEC(N)   number of grids in branch N
C-----PDC(L,K)     change in initial concentration due to a specific reaction
C-----PDF(L,K)     change in initial concentration due to dispersion
C-----PH(K)      time parcel K entered reach in hours from day 0
C-----PT(L,K)      conc. of constituent L in parcel K
C-----PTD(L)     conc. of constituent L in parcel K, d/s, avg'd over time step
C-----PTI(L,K)     initial concentration of constituent L in parcel K
C-----PTR(L,K)     change in initial concentration due to tributary inflow
C-----PTU(L)     conc. of constituent L in parcel K, u/s, avg'd over time step
C-----PV(K)      volume of parcel K
C-----PX(K)      location of upstream boundry of parcel in grid units
C-----Q(I)   river flow at grid I (cu m/s or ft/s), trib inflow occurs just u/s of grid
C-----QI     minimum flow of interest (flows<QI are considered zero).
C-----QT(I)      tributary inflow at grid I (cu m/s or ft/s) enter just u/s of grid
C-----RPT(L)     initial concentration of L in reach
C-----TITLE(20)   title of program (80 characters)
C-----TRIB(L,I)    concentration of constituent L in trib at grid I (tribs can not occur at first or last grid)
C-----VJ      inflow volume to junction M
C-----VU(I)      volume of parcel u/s of grid I
C-----W(I)       average top width in subreach I (m or ft)
C-----X(N,I)       distance of grid I from u/s of branch N (input in miles)
C-----XFACT        conversion from miles to feet or meters, depending on IENG

C***--BEGIN DIMENSIONING DEFINITION

C-----NOBR     Maximum number of branches allowed in model
C-----NOIJ     Maximum number of internal junctions allowed in model
C-----NOSC     Maximum number of cross sections (grids) allowed in
c-----branch
C-----NOPR     Maximum number of parcels allowed in branch
C-----(NOPR should be at least 20 + 2 times NOSC)
      IMPLICIT NONE
      INCLUDE 'param.inc'
      INCLUDE '../input/fixed/common.f'
      INCLUDE '../hydro/network.inc'
      INCLUDE '../input/time-varying/common_tide.f'

      INCLUDE 'bltm1.inc'
      INCLUDE 'bltm3.inc'
      INCLUDE 'bltm2.inc'
      INCLUDE 'kinetic1.inc'
      LOGICAL AllJunctionsMixed 

c-----variables for mass tracking
c      real resconc, resflow

C-----+ + + LOCAL VARIABLES + + +C

      INTEGER I,INX,
     &     JN,K,CONS_NO,M,
     &     NN,KK
      integer res_num_clfct

      REAL    HR,SVOL,TIME,VJ,VOL
      !real * 8 DTT
c      REAL    ERZ
      REAL    TOTFLO
      REAL    C(MAX_CONSTITUENT)
     &     ,objflow,massrate(max_constituent) ! flow and massrate at object

      integer*4
     &     incr_intvl           ! increment julian minute by interval function
     &     ,next_output_flush   ! next time to flush output
     &     ,next_display        ! next time to display model time
     &     ,next_restart_output ! next time to write restart file
     &     ,next_binary_output  ! next time to write binary file

      integer
     &     istat                ! status of fixed input
     &     ,lnblnk              ! last nonblank function

      character
     &     init_input_file*130  ! initial input file on command line [optional]
     &     ,jmin2cdt*14         ! convert from julian minute to char date/time

      integer iprnt_mass
      common /mass_tracking_1/ iprnt_mass

      include '../input/time-varying/dss.inc'
      include '../input/time-varying/readdss.inc'
      include '../input/time-varying/writedss.inc'

      data init_input_file /' '/

C-----+ + + INPUT FORMATS + + +
 1010 FORMAT (10X,10I7)
 1050 FORMAT (24X,F7.3,10F6.3)
 1060 FORMAT (2X,I3,3X,I2,11F7.3)
 1110 FORMAT (3I5)
 1120 format (8f10.4)
c-----1120  FORMAT (8G10.3)

C-----+ + + OUTPUT FORMATS + + +
 2300 FORMAT (1H ,'  DAY',I4,'  HOUR',F4.1,6X,'VU',8X,'PV',8X,'PH',
     &     10(4X,A4))
C-----2310  FORMAT (1H ,' BR',I4,' GRID',I3,'  K=',I2,2G10.3,F7.2,10F8
c-----.3)
 2310 FORMAT (1H ,' BR',I4,' GRID',I3,'  K=',I2,2G10.3,7X,10F8.3)
 2320 FORMAT (1H ,38X,'PTI',7X,10F8.3)
 2330 FORMAT (1H ,38X,'PDF',7X,10F8.3)
 2340 FORMAT (1H ,38X,'PTR',7X,10F8.3)
 2350 FORMAT (1H ,38X,'PDC',7X,10F8.3)

c-----module, name and version
      include 'version.inc'

c      open(unit_screen, carriagecontrol='list')    !! <NT>
c      open(unit_error, carriagecontrol='list')     !! <NT>
c-----get optional starting input file from command line,
c-----then from environment variables,
c-----then default
      call getarg(1,init_input_file)
      if (lnblnk(init_input_file) .eq. 0) then ! no command line arg
         call getenv('QUALINPUT',init_input_file)
         if (init_input_file .eq. ' ') then
            call getenv('DSM2INPUT',init_input_file)
            if (init_input_file .eq. ' ') then
               init_input_file='dsm2.inp'
            endif
         endif
      endif

c-----read input file(s)
      call read_fixed(init_input_file,istat)
      if (istat .ne. 0) then
         write(unit_error, *)
     &        'Error in reading fixed data; run stopped.'
         call exit(1)
      endif

      call check_fixed(istat)
      if (istat .ne. 0) then
         write(unit_error, *)
     &        'Error in checking fixed data; run stopped.'
         call exit(1)
      endif

      call check_fixed_qual(istat)
      if (istat .ne. 0) then
         write(unit_error, *)
     &        'Error in checking fixed qual data; run stopped.'
         call exit(1)
      endif

      if (istat .ne. 0) then
         write(unit_error, *)
     &        'Error in checking fixed data; run stopped.'
         call exit(1)
      endif

      prev_julmin=0
      julmin=start_julmin
      current_dt=jmin2cdt(julmin)

      CALL BLTMINIT
      DTT=DT*3600.             ! hard coded, time step in minute
      CALL print_outqual(istat)

      write(unit_screen, 650)
     &     no_of_constituent-no_of_nonconserve_constituent,
     &     ' conservative constituents simulated.'
      write(unit_output, 650)
     &     no_of_constituent-no_of_nonconserve_constituent,
     &     ' conservative constituents simulated.'

      write(unit_screen, 650)
     &     no_of_nonconserve_constituent,
     &     ' non-conservative constituents simulated.'
      write(unit_output, 650)
     &     no_of_nonconserve_constituent,
     &     ' non-conservative constituents simulated.'

 650  format(i2,a)

      IF(MASS_TRACKING)THEN
         call read_input_data_for_masstracking
         ! the region is hard coded, it could be altered.
         ! it will count only the last region.
         call read_nodeflags_for_masstracking_region
         DO CONS_NO = 1, NEQ
            ! variables calculated in masstrack with Command=1
            TOTCONSTIT(CONS_NO) = 0.0
            TOTRESCONSTIT(CONS_NO) = 0.0
            TOTCHCONSTIT(CONS_NO) = 0.0
            do i=1,num_masstrack_regions
               totmass_in_region(i,CONS_NO) = 0.
            enddo

            ! variables updated in masstrack with command=2
            TOTCONSTITENTER(CONS_NO)=0.
            TOTCONSTITBND(CONS_NO)=0.
            TOTCONSTITCHDEP(CONS_NO)=0.

            AMOUNTDECAYED(CONS_NO)=0.
            totconstitpump(CONS_NO) = 0.
            totconstitexport(CONS_NO) = 0.
               
            do i=1,num_export_nodes
               mass_export(i,CONS_NO)=0.    ! export node no and export locations
            enddo
         ENDDO
         CALL MASSTRACK(1)      ! mass in storage
                 
         DO CONS_NO=1,NEQ
            TOTOLDCONSTIT(CONS_NO)=TOTCONSTIT(CONS_NO)
            call open_masstrack_output_files(CONS_NO)
         ENDDO
      ENDIF

      IRC=1

      next_display=incr_intvl(start_julmin,display_intvl,TO_BOUNDARY)
      next_output_flush=incr_intvl(start_julmin,flush_intvl,TO_BOUNDARY)
      if (io_files(qual,io_restart,io_write).use) then
         next_restart_output=incr_intvl(start_julmin,io_files(qual,
     &        io_restart,io_write).interval,TO_BOUNDARY)
      endif

c-----###################################################################
c-----To open qual binary output file
      if (io_files(qual,io_tide,io_write).use) then

         open(unit=io_files(qual,io_tide,io_write).unit,
     &        file=io_files(qual,io_tide,io_write).filename,
     &        form='unformatted',
c     &        convert='big_endian',                 !! <NT>
     &        status='unknown')

         call WriteQualBinaryDataHeader

         next_binary_output=incr_intvl(start_julmin,io_files(qual,
     &        io_tide,io_write).interval,TO_BOUNDARY)
      endif

c-----##################################################################
c      write(*,*) 'Next output flush', next_output_flush
c      write(*,*) 'Next restart output', next_restart_output
c      write(*,*) 'next binary output', next_binary_output
c      write(*,*) 'start julmin and io_file interval',start_julmin
c      write(*,*) io_files(qual,io_tide,io_write).interval
c      write(*,*) 'qual io_tide and io_write', qual,io_tide,io_write
c      write(*,*) 'Unit number'
c      write(*,*) io_files(qual,io_tide,io_write).unit
c      write(*,*) 'io_files(qual,io_tide,io_write).filename'
c      write(*,*) io_files(qual,io_tide,io_write).filename
c-----###################################################################

      if (check_input_data) then
c--------just check input data for bogus values; no simulation

 604     format('Checking data at time: ',a)
         write(unit_screen,604) current_dt

C--------start time loop for checking boundary data

         prev_julmin=julmin
         julmin=julmin+time_step
         current_dt=jmin2cdt(julmin)

         do while (julmin .le. end_julmin)

            if (julmin .ge. next_display) then
 611           format('Starting data-check for time: ',a)
               write(unit_screen,611) current_dt
               next_display=incr_intvl(next_display,display_intvl,
     &              TO_BOUNDARY)
            endif

            call read_boundary_values
            call read_mult_tide

            prev_julmin=julmin
C-----------Read the Hydro tidefile
            julmin=julmin+time_step
            current_dt=jmin2cdt(julmin)
         enddo
         go to 790
      endif

      call update_intervals

      call init_store_outpaths(istat)

 605  format('Starting run at time: ',a)
      write(unit_screen,605) current_dt
      call store_outpaths(.false.)

C-----start time loop

      prev_julmin=julmin
      julmin=julmin+time_step
      current_dt=jmin2cdt(julmin)

      do while (julmin .le. end_julmin)
         call update_intervals
         if (julmin .ge. next_display) then
            write(unit_screen,610) current_dt
 610        format('Starting computations for time: ',a)
            next_display=incr_intvl(next_display,display_intvl,
     &           TO_BOUNDARY)
         endif

         jtime=(julmin-start_julmin)/time_step
         if (mtemp .gt. 0 .or. malg .gt. 0) then
            call heat
         end if

C--------set boundary and junction values
         DO 320 N=1,NBRCH
            DVU(N)=0.0
            DVD(N)=0.0
            DO 310 CONS_NO=1,NEQ
               GPTU(CONS_NO,N)=0.0
               GPTD(CONS_NO,N)=0.0
 310        CONTINUE
 320     CONTINUE
         DO 330 N=1,NJNCT
            JCD(N)=2
 330     CONTINUE

         call read_boundary_values

C--------Read the Hydro tidefile
         call read_mult_tide
         CALL INTERPX
         
         DO 360 N=1,NBRCH
            JN=JNCU(N)
c-----------calculate total flow and mass into this node for each constituent
            call node_rate(jn,TO_OBJ,0,objflow,massrate)
            IF (JN.GT.NJNCT .AND. ! external node
     &           objflow .gt. 0.) THEN ! source at node
c--------------checkit: what if qext source at mtz (stage boundary)?
               QNODE(jn)=objflow
               DO CONS_NO=1,NEQ
                  GTRIB(CONS_NO,1,N)=massrate(CONS_NO)/objflow
               ENDDO
            ENDIF

            DO CONS_NO=1,NEQ
               GPTU(CONS_NO,N)=GTRIB(CONS_NO,1,N)
               GPTD(CONS_NO,N)=GTRIB(CONS_NO,NXSEC(N),N)
            ENDDO
 360     CONTINUE

C--------Initialize reservoir stuff
         DO I=1,NRES
            RESDEPTH = RESVOL(I)/ARES(I)
            IREV = I
            reschgvol(i)=0.0         ! for masstracking
            DO CONS_NO=1,NEQ
               C(CONS_NO) = CRES(I,CONS_NO)
               reschgconc(i,CONS_NO)=0.0   ! for masstracking
            ENDDO
            iskip = 1
            if (no_of_nonconserve_constituent .gt. 0) then
               chan_res = 2     ! pointer to reservoir for kinetic computations
               call rate_chanres(i)
               call kinetic(c)
            end if
            IF(MASS_TRACKING)THEN
               DO CONS_NO = 1, NEQ
                  AMOUNTDECAYED(CONS_NO)=AMOUNTDECAYED(CONS_NO)+
     &                 (CRES(I,CONS_NO)-C(CONS_NO))*RESVOL(I)
               ENDDO
            ENDIF
            DO CONS_NO = 1, NEQ
               CRES(I,CONS_NO) = C(CONS_NO)
            ENDDO
         ENDDO

         CALL ROUTE

C--------update junction concentrations and codes
C--------compute inflow flux at known junction

         DO M=1,NJNCT
            DO CONS_NO=1,NEQ
               cj_prev(CONS_NO,m)=cj(CONS_NO,m)
               cj(cons_no,m)=0.0     ! for masstracking
            ENDDO
         ENDDO

 400     CONTINUE
         AllJunctionsMixed=.false.
         do while (.not.AllJunctionsMixed) 

            DO 640 M=1,NJNCT
               JN=M
               TOTFLO=0.
               IF (JCD(JN).EQ.0) GO TO 640
               VJ=0.0
               DO KK=1,NUMUP(JN)
                  N=LISTUP(JN,KK)
                  TOTFLO=TOTFLO-FLOW(N,1,1)
                  IF (FLOW(N,1,1).LT.0.0) THEN
                     IF(abs(dvu(n)) .gt. 1.0 .AND.
     &                    JNCD(N) .LE. NJNCT) GO TO 640
                     VJ=VJ-FLOW(N,1,1)*DTT
                     DO 520 CONS_NO=1,NEQ
                        CJ(CONS_NO,M)=CJ(CONS_NO,M)+GPTU(CONS_NO,N)
 520                 CONTINUE
                  END IF
               ENDDO

               DO KK=1,NUMDOWN(JN)
                  N=LISTDOWN(JN,KK)
                  TOTFLO=TOTFLO+FLOW(N,1,NXSEC(N))
                  IF (FLOW(N,1,NXSEC(N)).GT.0.0) THEN
                     IF (abs(dvd(n)).gt. 1.0 .AND.JNCU(N).LE.NJNCT) GO TO 640
                     VJ=VJ+FLOW(N,1,NXSEC(N))*DTT
                     DO 540 CONS_NO=1,NEQ
                        CJ(CONS_NO,M)=CJ(CONS_NO,M)+GPTD(CONS_NO,N)
 540                 CONTINUE
                  END IF
               ENDDO

C--------------Now add the effects of external and internal flows, and reservoirs
               call node_rate(jn,TO_OBJ,0,objflow,massrate)
               VJ=VJ+objflow*DTT
               TOTFLO=TOTFLO+objflow
               
               DO CONS_NO=1,NEQ
                  CJ(CONS_NO,M)=CJ(CONS_NO,M)+massrate(CONS_NO)*DTT
               ENDDO
               call node_rate(jn,FROM_OBJ,0,objflow,massrate)
               TOTFLO=TOTFLO+objflow
               
               IF (VJ.GT.0.0) THEN
                  DO 560 CONS_NO=1,NEQ
                     CJ(CONS_NO,M)=CJ(CONS_NO,M)/VJ
                     cj_prev(CONS_NO,m)=cj(CONS_NO,m)
 560              CONTINUE
               ENDIF
               
               JCD(JN)=0
C--------------At this point all the flows entering the junction have
c--------------been mixed.
C--------------update GPTU,GPTD,DVU,DVD,PT,PTI,PTR
               DO KK=1,NUMUP(JN)
                  N=LISTUP(JN,KK)
                  
                  IF (FLOW(N,1,1).GT.0.0) THEN
                     VOL=GPV(N,1)+DVD(N)
                     DO 600 CONS_NO=1,NEQ
                        GPTU(CONS_NO,N)=CJ(CONS_NO,M)*DTT*FLOW(N,1,1)
                        GPT(CONS_NO,1,N)=(CJ(CONS_NO,M)*FLOW(N,1,1)*DTT+
     &                       GPT(CONS_NO,1,N)*VOL)/VOL
                        GPTD(CONS_NO,N)=GPTD(CONS_NO,N)+CJ(CONS_NO,M)*DVD(N)
 600                 CONTINUE
                  END IF
                  DVD(N)=0.0
               ENDDO
               
               DO KK=1,NUMDOWN(JN)
                  N=LISTDOWN(JN,KK)
                  IF (FLOW(N,1,NXSEC(N)).LT.0.0) THEN
                     VOL=GPV(N,NS(N))+DVU(N)
                     DO 620 CONS_NO=1,NEQ
                        GPTD(CONS_NO,N)=CJ(CONS_NO,M)*DTT*FLOW(N,1,NXSEC(N))
                        GPT(CONS_NO,NS(N),N)=(-CJ(CONS_NO,M)*FLOW(N,1,NXSEC(N))*DTT+
     &                       GPT(CONS_NO,NS(N),N)*VOL)/VOL
                        GPTU(CONS_NO,N)=GPTU(CONS_NO,N)+CJ(CONS_NO,M)*DVU(N)
 620                 CONTINUE
                  END IF
                  DVU(N)=0.0
               ENDDO
 640        CONTINUE
         
            AllJunctionsMixed=.true.
            DO M=1,NJNCT
               IF(JCD(M).NE.0) THEN
C-----------------This JUNCTION NOT MIXED YET. Have to go back
                  AllJunctionsMixed=.false.
                  DO CONS_NO=1,NEQ
                     CJ(CONS_NO,M)=0.0
                  ENDDO
               ENDIF
            enddo
         ENDDO
c--------IF(.NOT.AllJunctionsMixed) GOTO 400

C--------Now all junctions have been mixed
         if(mass_tracking) then
c-----------Update reservoir salinities
            call reservoir_salinity_update_formtk(res_num_clfct)
         endif

         call update_resvol_for_masstracking_region

         IF(MASS_TRACKING)THEN
            call print_results_for_masstracking
         ENDIF

C--------write results

         if (io_files(qual,io_restart,io_write).use .and.
     &        julmin .ge. next_restart_output) then ! restart file requested
            next_restart_output=incr_intvl(next_restart_output,
     &           io_files(qual,io_restart,io_write).interval,
     &           TO_BOUNDARY)
            call restart_file(IO_WRITE)
         ENDIF

c--------***************************************************************
c--------Added by Ganesh Pandey
c--------To print qual binary file outputs

c--------Conditions to be checked
c--------1. Binary file is requested
c--------2. julmin => next_binary_output
c--------3. mod(julmin,next_display)=0
         !write(*,*) 'Point 2'
         !write(*,*) 'io_files(qual,io_tide,io_write).use'
         !write(*,*)  io_files(qual,io_tide,io_write).use
         !write(*,*) 'io_files(qual,io_tide,io_write).interval'
         !write(*,*) io_files(qual,io_tide,io_write).interval
         !write(*,*) 'julmin ', julmin, julmin+1
         !write(*,*) 'next binary output',next_binary_output
         !write(*,*) next_binary_output+1
         !write(*,*) '*************************************'

c--------******************************************************************
         if (io_files(qual,io_tide,io_write).use .and.
     &        julmin .ge. next_binary_output .and.
     &        mod(next_binary_output,julmin).eq.0 ) then ! restart file requested
           next_binary_output=incr_intvl(next_binary_output,
     &           io_files(qual,io_tide,io_write).interval,
     &           TO_BOUNDARY)
            call WriteQualBinaryData

         endif
c--------******************************************************************

         DO 730 N=1,NBRCH
            PX(1,N)=1.0
            INX=NXSEC(N)-1
            DO 720 I=1,INX
               VOL=-GVU(N,I)
               K=NKAI(N,I)-1
 700           CONTINUE
               K=K+1
               VOL=VOL+GPV(N,K)
               IF (K.LT.NKAI(N,I+1)) GO TO 700
               VOL=VOL-GPV(N,K)+GVU(N,I+1)

               K=NKAI(N,I)
               SVOL=GPV(N,K)-GVU(N,I)
 710           CONTINUE
               K=K+1
               PX(K,N)=FLOAT(I)+1.0
               VJ=ABS(VOL)
               IF(VJ.GT.1.0E-6)PX(K,N)=FLOAT(I)+SVOL/VOL
               SVOL=SVOL+GPV(N,K)
               IF (K.LT.NKAI(N,I+1)) GO TO 710
 720        CONTINUE
            IF(JPO.EQ.0)GO TO 730
            IF(MOD(JTIME,JPO).NE.0)GO TO 730
 730     CONTINUE

         if (julmin .ge. next_output_flush) then
            next_output_flush=incr_intvl(next_output_flush,
     &           flush_intvl,TO_BOUNDARY)
            call store_outpaths(.true.)
         else
            call store_outpaths(.false.)
         endif
         
 735     TIME=FLOAT(JTIME+JTS)*DT
         IDAY=IFIX(TIME/24.0)+1
         HR=TIME-FLOAT(IDAY-1)*24.0

         IF (MOD(JTIME,JGO).NE.0)GO TO 760
         DO 750 N=1,NBRCH
            NN=NXSEC(N)
            DO 740 I=1,NN
               IF (IOUT(N,I).NE.1) GO TO 740
               K=NKAI(N,I)
 740        CONTINUE
 750     CONTINUE

 760     continue
         prev_julmin=julmin
         julmin=julmin+time_step
         current_dt=jmin2cdt(julmin)
      enddo                     ! do while julmin time loop

      if (julmin .gt. end_julmin) then
         julmin=prev_julmin
         prev_julmin=prev_julmin-time_step
         current_dt=jmin2cdt(julmin)
      endif

 790  continue

      if (.not. check_input_data) then

*--------Write time-series network results.
         call store_outpaths(.true.) ! flush temp files
         call wrt_outpaths
      endif
c-----Close the DSS input files
      i=1
      do while(i .le. max_dssinfiles .and.
     &     infilenames(i) .ne. ' ')
         call zclose (ifltab_in(1,i))
         i=i+1
      enddo

      WRITE(*,*) '   -----------------------------'
      WRITE(*,*) ' '
      WRITE(*,*) '   Normal program end.'
      WRITE(*,*) ' '
      WRITE(*,*) '   -----------------------------'

      call exit(0)
      END

      subroutine read_boundary_values

      implicit none

c-----read time-varying data arrays for Qual

c-----subroutine arguments

c-----common blocks

      include '../input/fixed/common.f'
      include 'param.inc'
      include 'bltm1.inc'
      include 'bltm2.inc'

      include '../input/time-varying/dss.inc'
      include '../input/time-varying/readdss.inc'
      include '../input/time-varying/writedss.inc'

      integer
     &     chan,grid,constituent_no ! array indices

      do chan=1,nobr
         do grid=1,nosc
            do constituent_no=1,max_constituent
               gtrib(constituent_no,grid,chan)=0.0
            enddo
         enddo
      enddo

      if (npthsin_min15 .gt. 0) then
         call readtvd(max_inp_min,mins15,npthsin_min15,ptin_min15,
     &        datain_15min)
      endif

      if (npthsin_hour1 .gt. 0) then
         call readtvd(max_inp_hour,hrs,npthsin_hour1, ptin_hour1,
     &        datain_1hour)
      endif

      if (npthsin_day1 .gt. 0) then
         call readtvd(max_inp_day,dys,npthsin_day1,ptin_day1,
     &        datain_1day)
      endif

      if (npthsin_month1 .gt. 0) then
         call readtvd(max_inp_month,mths,npthsin_month1,ptin_month1,
     &        datain_1month)
      endif

      if (npthsin_year1 .gt. 0) then
         call readtvd(max_inp_year,yrs,npthsin_year1,ptin_year1,
     &        datain_1year)
      endif

      if (npthsin_irr .gt. 0) then
         call readtvd(max_inp_irr,irrs,npthsin_irr,ptin_irr,
     &        datain_irr)
      endif

      call store_values

      return
      end

      subroutine store_values

      implicit none

c-----Fill time-varying data arrays for Qual

c-----common blocks

      include 'param.inc'
      include '../input/fixed/common.f'
      include 'bltm1.inc'
      include 'bltm2.inc'
      include 'bltm3.inc'

c-----local variables

      integer
     &     ptr,i,ic             ! array indices
     &     ,intchan,extchan     ! internal and external channel numbering
     &     ,extnode             ! internal and external node numbering
     &     ,constituent_no      ! constituent number

      do ptr=1,ninpaths

c--------don't use input paths which are only for replacement
c--------(priority 2 or higher)
         if (pathinput(ptr).priority .gt. 1) goto 100

         call get_inp_data(ptr) ! get input data from buffers

c--------meteorological values
         if (pathinput(ptr).c_part .eq. 'cloud') then
            cloud=pathinput(ptr).value
         else if (pathinput(ptr).c_part .eq. 'dryblb') then
            dryblb=pathinput(ptr).value
         else if (pathinput(ptr).c_part .eq. 'wetblb') then
            wetblb=pathinput(ptr).value
         else if (pathinput(ptr).c_part .eq. 'wind') then
            wind=pathinput(ptr).value
         else if (pathinput(ptr).c_part .eq. 'atmpr') then
            atmpr=pathinput(ptr).value
         else
c-----------water quality constituent
            if (pathinput(ptr).object .eq. obj_node) then
               extnode=pathinput(ptr).object_no
c--------------only the stage type boundary EC is used later from this section;
c--------------flow ECs are handled in node_rate and res_rate
               do i=1,nstgbnd
                  if (extnode .eq. stgbnd(i).node) then ! Downstream salinity boundary (may be Martinez)
                     extchan=node_geom(extnode).downstream(1)
                     intchan=ext2int(extchan)
                     do ic=1,pathinput(ptr).n_consts
                        constituent_no = pathinput(ptr).const_ndx(ic)
                        gtrib(constituent_no,nxsec(intchan),intchan)=
     &                       pathinput(ptr).value
                     enddo
                  endif
               enddo
            endif
         endif
 100     continue
      enddo

      return
      end

      subroutine restart_file(action)

c-----read or write a restart file for qual

      implicit none

c-----include files

      include '../input/fixed/common.f'

      include 'param.inc'
      include 'bltm1.inc'

c-----arguments and local variables

      integer
     &     action               ! whether to read or write the restart file
     &     ,i,k,l,n             ! loop indices
     &     ,unit_restart        ! output unit
     &     ,lnblnk              ! last non-blank intrinsic function
     &     ,n_all               ! number of incoming chemical constituents

      real
     &     salavg(max_constituent)
     &     ,vol

      character
     &     header*150           ! header line
     &     ,cchem*20(max_constituent) ! incoming chemical constituent names
     &     ,get_substring*200   ! get substring function

      if (action .eq. io_write) then
c--------write restart file
         unit_restart=io_files(qual,io_restart,io_write).unit
         open(unit=unit_restart,file=io_files(qual,io_restart
     &        ,io_write).filename,status='unknown',err=901)

         write(unit_restart,900) dsm2_version, current_dt
 900     format('Qual Version ',a
     &        /'The following data corresponds to   ',a14//)
         write(unit_restart,970)
     &        (constituents(all_source_ptr(l)).constituent
     &        (:lnblnk(constituents(all_source_ptr(l)).constituent)),
     &        l=1,no_all_source)
 970     format('Initial Channel Concentration'/'Channel',11(1x,a11))

c--------figure out the average concentration in each channel
c--------only do calcs for constituents from all sources
         do n=1,nbrch
            do l=1,no_all_source
               salavg(all_source_ptr(l))=0
            enddo
            vol=0
            do k=1,ns(n)
               vol=vol+gpv(n,k)
               do l=1,no_all_source
                  salavg(all_source_ptr(l))=salavg(all_source_ptr(l)) +
     &                 gpv(n,k)*gpt(all_source_ptr(l),k,n)
               enddo
            enddo
            do l=1,no_all_source
               salavg(all_source_ptr(l))=salavg(all_source_ptr(l))/vol
            enddo
            write(unit_restart,971) int2ext(n),
     &           (salavg(all_source_ptr(l)),l=1,no_all_source)
 971        format(i7,1p,11(1x,g11.3))
         enddo
         write(unit_restart,975) (constituents(all_source_ptr(l)).constituent
     &        (:lnblnk(constituents(all_source_ptr(l)).constituent)),
     &        l=1,no_all_source)
 975     format('Initial Reservoir Salinity'/
     &        'Reservoir',11(1x,a11))

         do i=1,nres
            write(unit_restart,976) i,
     &           (cres(i,all_source_ptr(l)),l=1,no_all_source)
 976        format(i9,1p,11(1x,g11.3))
         enddo
         close(unit_restart)
      else
c--------read restart file
         unit_restart=io_files(qual,io_restart,io_read).unit
         open(file=io_files(qual,io_restart,io_read).filename,
     &        unit=unit_restart,status='old',err=901)
c--------read header, then test to see if it's really the header (old restart file)
c--------or the restart file version (new file)
         restart_version=' '
         read(unit_restart,'(a)') header
         if (header(:12) .eq. 'Qual Version') then
            restart_version=header(13:)
            read(unit_restart,'(a)') header
         endif
c--------get chemical names
         read(unit_restart,910) header
 910     format(///a)
         i=1
         cchem(1)=get_substring(header,' ') ! first field is 'Channel', discard
         do while (header .ne. ' ' .and. i .le. max_constituent)
            cchem(i)=get_substring(header,' ')
            call locase(cchem(i))
            i=i+1
         enddo
         n_all=i-1
c--------for each different chemical read in, copy to possibly multiple
c--------constituent locations
         do n=1,nbrch
            read(unit_restart,*) k, (salavg(i), i=1,n_all)
            do l=1,neq
               cstart(ext2int(k),l)=0.0
               do i=1,n_all
                  if (constituents(l).constituent .eq. cchem(i)) then
                     cstart(ext2int(k),l)=salavg(i)
                  endif
               enddo
            enddo
         enddo
c--------skip reservoir headers, assume chemical names are in same order
c--------as for channels
         read(unit_restart,911) header
 911     format(/a)
         do n=1,nres
            read(unit_restart,*) k, (salavg(i), i=1,n_all)
            do l=1,neq
               cres(k,l)=0.0
               do i=1,n_all
                  if (constituents(l).constituent .eq. cchem(i)) then
                     cres(k,l)=salavg(i)
                  endif
               enddo
            enddo
         enddo
         close(unit_restart)
      endif

      return

 901  continue                  ! open error on restart file
      write(unit_error,605) io_files(qual,io_restart,action).filename
 605  format(/'Error opening restart file:',a)
      call exit(2)

      end

      subroutine node_rate(qualnode, direction, acct_ndx,
     &     objflow, massrate)

c-----Return flows and massrates, either into or out of, for qual node,
c-----for given accounting label index (if none given ignore accounting
c-----labels).

      implicit none

      include 'param.inc'
      include '../hydro/network.inc'
      include '../input/fixed/common.f'
      include '../input/time-varying/common_tide.f'
      include '../input/time-varying/tide.inc'
      include 'bltm1.inc'
      include 'bltm3.inc'
      include 'bltm2.inc'

c-----local variables
      logical accounting_label_ok ! true if flow's accounting label match input
     &     ,err_node(max_nodes) ! nodal error counter

      integer
     &     qualnode             ! qual node number [INPUT]
     &     ,direction           ! either FROM_OBJ or TO_OBJ [INPUT]
     &     ,acct_ndx            ! accounting index, if 0 ignore [INPUT]
     &     ,ic                  ! constituent index
     &     ,qndx                ! external and internal flow index
     &     ,pndx                ! pathname index
     &     ,dsmnode             ! dsm node number
     &     ,res                 ! reservoir number
     &     ,conndx              ! reservoir connection index
     &     ,i,j,k               ! loop indices
     &     ,lnblnk              ! intrinsic

      real
     &     objflow              ! total external and internal flow at node [OUTPUT]
     &     ,massrate(max_constituent) ! total external and internal massrate at node [OUTPUT]
     &     ,conc                ! flow concentration
     &     ,node_qual           ! node quality function

      record /from_to_s/ from,to ! from and to objects
      save err_node

      dsmnode=nodequal2dsm(qualnode)

      objflow=0.0
      do ic=1,max_constituent
         massrate(ic)=0.0
      enddo

c-----external flows
      i=1
      do while (node_geom(dsmnode).qext(i) .ne. 0)
         qndx=node_geom(dsmnode).qext(i)
         accounting_label_ok=
     &        acct_ndx .eq. ALL_FLOWS .or.
     &        acct_ndx .eq. NO_CONNECT .or.
     &        acct_ndx .eq. QEXT_FLOWS .or.
     &        acct_ndx .eq. 0 .or.
     &        qext(qndx).acct_ndx .eq. 0 .or.
     &        (acct_ndx .gt. 0 .and. acct_ndx .eq. qext(qndx).acct_ndx)

         if (accounting_label_ok) then
            if (direction .eq. TO_OBJ .and.
     &           qext(qndx).avg .gt. 0.0) then ! direction and flow to node
               objflow=objflow + qext(qndx).avg
               if (n_conqext(qndx) .eq. 0 .and. .not. err_node(dsmnode)) then
                  err_node(dsmnode)=.true.
                  write(unit_error,610)
     &                 obj_names(qext(qndx).attach.object)
     &                 (:lnblnk(obj_names(qext(qndx).attach.object)))
     &                 ,qext(qndx).attach.obj_name(:lnblnk(qext(qndx).attach.obj_name)),
     &                 qext(qndx).acct_name(:lnblnk(qext(qndx).acct_name))
 610              format(/'Warning; no input path constituent',
     &                 ' for 'a,' ',a,' type ',a,'; assumed zero.')
                  conc=0.0
               else
                  do k=1,n_conqext(qndx)
                     pndx=const_qext(qndx,k) ! input path index to constituents for this flow
                     conc=pathinput(pndx).value
                     do j=1,pathinput(pndx).n_consts
                        massrate(pathinput(pndx).const_ndx(j))=
     &                       massrate(pathinput(pndx).const_ndx(j))
     &                       + qext(qndx).avg * conc
                     enddo
                  enddo
               endif
            else if (direction .eq. FROM_OBJ .and.
     &              qext(qndx).avg .lt. 0.0) then ! direction and flow from node
               objflow=objflow + qext(qndx).avg
               do j=1,neq
                  conc=node_qual(dsmnode,j)
                  massrate(j)=massrate(j)
     &                 + qext(qndx).avg * conc * qext(qndx).mass_frac
               enddo
            endif
         endif
         i=i+1
      enddo

c-----internal flows
      i=1
      do while (node_geom(dsmnode).qint(i) .ne. 0)
         qndx=node_geom(dsmnode).qint(i)

         call obj2obj_direc(obj2obj(qndx).flow_avg,
     &        obj2obj(qndx), from, to)

         if (direction .eq. TO_OBJ) then ! flow to node wanted
c-----------is the object correct type and number?
            if (to.object .eq. obj_node .and.
     &           to.object_no .eq. dsmnode) then
c--------------does accounting label match?
               accounting_label_ok=
     &              acct_ndx .eq. ALL_FLOWS .or.
     &              acct_ndx .eq. NO_CONNECT .or.
     &              acct_ndx .eq. QINT_FLOWS .or.
     &              acct_ndx .eq. 0 .or.
     &              to.acct_ndx .eq. 0 .or.
     &              (acct_ndx .gt. 0 .and. acct_ndx .eq. to.acct_ndx)

               if (accounting_label_ok) then
                  objflow=objflow + obj2obj(qndx).flow_avg
                  do j=1,neq
c--------------------determine concentration of 'from' object
                     if (from.object .eq. obj_node) then
                        conc=node_qual(from.object_no,j)
                     else if (from.object .eq. obj_reservoir) then
                        conc=cres(from.object_no,j)
                     endif
                     massrate(j)= massrate(j)
     &                    + obj2obj(qndx).flow_avg * conc * from.mass_frac
                  enddo
               endif            ! accounting label ok
            endif
         else                   ! flow from node wanted
c-----------is the object correct type and number?
            if (from.object .eq. obj_node .and.
     &           from.object_no .eq. dsmnode) then
c--------------does accounting label match?
               accounting_label_ok=
     &              acct_ndx .eq. ALL_FLOWS .or.
     &              acct_ndx .eq. NO_CONNECT .or.
     &              acct_ndx .eq. QINT_FLOWS .or.
     &              acct_ndx .eq. 0 .or.
     &              to.acct_ndx .eq. 0 .or.
     &              (acct_ndx .gt. 0 .and. acct_ndx .eq. from.acct_ndx)

               if (accounting_label_ok) then
                  objflow=objflow - obj2obj(qndx).flow_avg
                  do j=1,neq
                     conc=node_qual(dsmnode,j)
                     massrate(j)=massrate(j)
     &                    + obj2obj(qndx).flow_avg * conc * from.mass_frac
                  enddo
               endif
            endif
         endif
         i=i+1
      enddo

c-----reservoir flows connected to node

      accounting_label_ok=
     &     acct_ndx .eq. ALL_FLOWS .or.
c     &     acct_ndx .eq. QEXT_FLOWS .or.
     &     acct_ndx .eq. QINT_FLOWS .or.
     &     acct_ndx .eq. 0

      if (accounting_label_ok) then ! no accounting
         do i=1,nconres(qualnode)
            res=lconres(qualnode,i,1)
            conndx=lconres(qualnode,i,2)
c-----------positive qres means flow from reservoir to node
            if (direction .eq. TO_OBJ .and.
     &           qres(res,conndx) .gt. 0.0) then ! flow to node
               objflow=objflow + qres(res,conndx)
               do j=1,neq
                  massrate(j)=massrate(j)
     &                 + qres(res,conndx) * cres(res,j)
               enddo
               if(dsmnode.eq.220) then
               endif
            else if (direction .eq. FROM_OBJ .and.
     &              qres(res,conndx) .lt. 0.0) then ! flow from node
               objflow=objflow + qres(res,conndx)
               do j=1,neq
                  conc=node_qual(dsmnode,j)
                  massrate(j)=massrate(j)
     &                 + qres(res,conndx) * conc
               enddo
            endif
         enddo
      endif

      return
      end

      subroutine res_rate(res, direction, acct_ndx,
     &     objflow, massrate)

c-----Return flows and massrates, either into or out of, for qual reservoir,
c-----for given accounting label index (if none given ignore accounting
c-----labels).

      implicit none

      include 'param.inc'
      include '../hydro/network.inc'
      include '../input/fixed/common.f'
      include '../input/time-varying/common_tide.f'
      include '../input/time-varying/tide.inc'
      include 'bltm1.inc'
      include 'bltm3.inc'
      include 'bltm2.inc'

c-----local variables
      logical accounting_label_ok ! true if flow's accounting label match input
     &     ,err_res(max_reservoirs) ! nodal error counter

      integer
     &     res                  ! qual reservoir number [INPUT]
     &     ,direction           ! either FROM_OBJ or TO_OBJ [INPUT]
     &     ,acct_ndx            ! accounting index, if 0 ignore [INPUT]
     &     ,ic                  ! constituent index
     &     ,qndx                ! external and internal flow index
     &     ,pndx                ! pathname index
     &     ,dsmnode             ! dsm node number
     &     ,i,j,k               ! loop indices
     &     ,lnblnk              ! intrinsic
     &     ,jn                  ! Sequence node number
     &     ,ich                 ! channel number

      real
     &     objflow              ! total external and internal flow at reservoir [OUTPUT]
     &     ,massrate(max_constituent) ! total external and internal massrate at reservoir [OUTPUT]
     &     ,conc                ! flow concentration
     &     ,node_qual           ! node quality function

      record /from_to_s/ from,to ! from and to objects
      save err_res

      objflow=0.0
      do ic=1,max_constituent
         massrate(ic)=0.0
      enddo

c-----external flows
      i=1
      do while (res_geom(res).qext(i) .ne. 0)
         qndx=res_geom(res).qext(i)

         accounting_label_ok=
     &        acct_ndx .eq. ALL_FLOWS .or.
     &        acct_ndx .eq. NO_CONNECT .or.
     &        acct_ndx .eq. QEXT_FLOWS .or.
     &        acct_ndx .eq. 0 .or.
     &        qext(qndx).acct_ndx .eq. 0 .or.
     &        (acct_ndx .gt. 0 .and. acct_ndx .eq. qext(qndx).acct_ndx)

         if (accounting_label_ok) then
            if (direction .eq. TO_OBJ .and.
     &           qext(qndx).avg .gt. 0.0) then ! direction and flow to reservoir
               objflow=objflow + qext(qndx).avg
               if (n_conqext(qndx) .eq. 0 .and. .not. err_res(res)) then
                  err_res(res)=.true.
                  write(unit_error,610)
     &                 obj_names(qext(qndx).attach.object)
     &                 (:lnblnk(obj_names(qext(qndx).attach.object)))
     &                 ,qext(qndx).attach.obj_name(:lnblnk(qext(qndx).attach.obj_name)),
     &                 qext(qndx).acct_name(:lnblnk(qext(qndx).acct_name))
 610              format(/'Warning; no input path constituent',
     &                 ' for 'a,' ',a,' type ',a,'; assumed zero.')
                  conc=0.0
               else
                  do k=1,n_conqext(qndx)
                     pndx=const_qext(qndx,k) ! input path index to constituents for this flow
                     conc=pathinput(pndx).value
                     do j=1,pathinput(pndx).n_consts
                        massrate(pathinput(pndx).const_ndx(j))=
     &                       massrate(pathinput(pndx).const_ndx(j))
     &                       + qext(qndx).avg * conc
                     enddo
                  enddo
               endif
            else if (direction .eq. FROM_OBJ .and.
     &              qext(qndx).avg .lt. 0.0) then ! direction and flow from reservoir
               objflow=objflow + qext(qndx).avg
               do j=1,neq
                  massrate(j)=massrate(j)
     &                 + qext(qndx).avg * cres(res,j)
     &                 * qext(qndx).mass_frac
               enddo
            endif
         endif
         i=i+1
      enddo

c-----internal flows
      i=1
      do while (res_geom(res).qint(i) .ne. 0)
         qndx=res_geom(res).qint(i)

         call obj2obj_direc(obj2obj(qndx).flow_avg,
     &        obj2obj(qndx), from, to)

         if (direction .eq. TO_OBJ) then ! flow to reservoir wanted
c-----------is the object correct type and number?
            if (to.object .eq. obj_reservoir .and.
     &           to.object_no .eq. res) then
c--------------does accounting label match?
               accounting_label_ok=
     &              acct_ndx .eq. ALL_FLOWS .or.
     &              acct_ndx .eq. NO_CONNECT .or.
     &              acct_ndx .eq. QINT_FLOWS .or.
     &              acct_ndx .eq. 0 .or.
     &              to.acct_ndx .eq. 0 .or.
     &              (acct_ndx .gt. 0 .and. acct_ndx .eq. to.acct_ndx)

               if (accounting_label_ok) then
                  objflow=objflow + obj2obj(qndx).flow_avg
                  do j=1,neq
c--------------------determine concentration of 'from' object
                     if (from.object .eq. obj_node) then
                        conc=node_qual(from.object_no,j)
                     else if (from.object .eq. obj_reservoir) then
                        conc=cres(from.object_no,j)
                     endif
                     massrate(j)= massrate(j)
     &                    + obj2obj(qndx).flow_avg * conc * from.mass_frac
                  enddo
               endif            ! accounting label ok
            endif
         else                   ! flow from reservoir wanted
c-----------is the object correct type and number?
            if (from.object .eq. obj_reservoir .and.
     &           from.object_no .eq. res) then
c--------------does accounting label match?
               accounting_label_ok=
     &              acct_ndx .eq. ALL_FLOWS .or.
     &              acct_ndx .eq. NO_CONNECT .or.
     &              acct_ndx .eq. QINT_FLOWS .or.
     &              acct_ndx .eq. 0 .or.
     &              to.acct_ndx .eq. 0 .or.
     &              (acct_ndx .gt. 0 .and. acct_ndx .eq. from.acct_ndx)

               if (accounting_label_ok) then
                  objflow=objflow - obj2obj(qndx).flow_avg
                  do j=1,neq
                     massrate(j)=massrate(j)
     &                    - obj2obj(qndx).flow_avg * cres(res,j)
     &                    * from.mass_frac
                  enddo
               endif
            endif
         endif
         i=i+1
      enddo

      accounting_label_ok=
     &     acct_ndx .eq. ALL_FLOWS .or.
     &     acct_ndx .eq. QEXT_FLOWS .or.
     &     acct_ndx .eq. QINT_FLOWS .or.
     &     acct_ndx .eq. 0

      if (accounting_label_ok) then ! no accounting
c--------reservoir flows connected to nodes

c--------positive qres means flow from reservoir to node
         do i=1,res_geom(res).nnodes
            if (direction .eq. TO_OBJ .and.
     &           qres(res,i) .lt. 0.0) then ! flow to reservoir
               objflow=objflow - qres(res,i)
               dsmnode=res_geom(res).node_no(i)
               jn=nodedsm2qual(dsmnode)
               do j=1,neq
                  if (jn .le. njnct) then
c--------------------internal node connected to a reservoir
                     conc=node_qual(dsmnode,j)
                     massrate(j)=massrate(j)
     &                    - qres(res,i) * conc
                  else
c--------------------external node connected to a reservoir
                     if (listup(jn,1).gt.0) then
c-----------------------reservoir connected at the upstream boundary
                        ich=listup(jn,1)
                        massrate(j)=massrate(j)+gptu(j,ich)/(dt*3600.)
                     else if (listdown(jn,1).gt.0) then
c-----------------------reservoir connected at the downstream boundary
                        ich=listdown(jn,1)
                        massrate(j)=massrate(j)+gptd(j,ich)/(dt*3600.)
                     else
                        write(unit_error,905)
 905                    format('Program Bug in res_rate')
                        call exit(2)
                     endif
                  endif
               enddo
            else if (direction .eq. FROM_OBJ .and.
     &              qres(res,i) .gt. 0.0) then ! flow from reservoir
               objflow=objflow - qres(res,i)
               do j=1,neq
                  massrate(j)=massrate(j)
     &                 - qres(res,i) * cres(res,j)
               enddo
            endif
         enddo
      endif

      return
      end

c------------------------------------------------WriteQualBinaryData
c-----subroutine to print the qual nodal constituents at 
c-----specified time steps in binary format.
c-----Ganesh Pandey 11/22/1999
      subroutine WriteQualBinaryData

      implicit none

      INCLUDE 'param.inc'
      include '../hydro/network.inc'
      INCLUDE '../input/fixed/common.f'
      include '../input/time-varying/common_tide.f'
      include '../input/time-varying/tide.inc'

      INCLUDE 'bltm1.inc'
      include 'bltm2.inc'
      include 'bltm3.inc'

      integer jj
      integer QualNodeNo,ConstNo
      real node_qual                     ! node quality function
      !real * 4 NodeQuality(Max_Nodes,Neq)
      real * 4 NodeQuality(Neq,Max_Nodes)

c-------- write binary data
      do QualNodeNo=1,numnode
         jj=nodequal2dsm(QualNodeNo)
         do ConstNo=1,neq
            !nodequality(QualNodeNo,ConstNo)=node_qual(jj,ConstNo)
            NodeQuality(ConstNo,QualNodeNo)=node_qual(jj,ConstNo)
         enddo
      enddo
      !write(io_files(qual,io_tide,io_write).unit) current_dt
      !write(io_files(qual,io_tide,io_write).unit)current_dt,((nodequality(QualNodeNo,ConstNo),QualNodeNo=1,numnode),ConstNo=1,neq)
      write(io_files(qual,io_tide,io_write).unit)current_dt,((nodequality(ConstNo,QualNodeNo),QualNodeNo=1,numnode),ConstNo=1,neq)
      return
      end

c------------------------------------------WriteQualBinaryDataHeader
c-----subroutine to print header for the qual nodal constituents 
c-----binary output file.
c-----Ganesh Pandey 11/22/1999
      subroutine WriteQualBinaryDataHeader

      implicit none

      INCLUDE 'param.inc'
      include '../hydro/network.inc'
      INCLUDE '../input/fixed/common.f'
      include '../input/time-varying/common_tide.f'
      include '../input/time-varying/tide.inc'

      include '../input/time-varying/dss.inc'
      !include '../input/time-varying/readdss.inc'      

      INCLUDE 'bltm1.inc'
      include 'bltm2.inc'
      include 'bltm3.inc'
      
      integer l
      character * 14 jmin2cdt, start_date

c-----Write header for binary file.

c      write(io_files(qual,io_tide,io_write).unit)'Start Julmin= ',
c     &        start_julmin
c      write(io_files(qual,io_tide,io_write).unit)'Time Step',
c     &        io_files(qual,io_tide,io_write).interval
c      write(io_files(qual,io_tide,io_write).unit)'Number of Nodes',
c     &        numnode
c      write(io_files(qual,io_tide,io_write).unit)'Number of Consitutents',
c     &        neq
 
      start_date=jmin2cdt(start_julmin)
      !write(*,*) 'Dsm2 version    ', dsm2_version
      !write(*,*) 'Start Date      ', start_date
      !write(*,*) 'Start Julmin    ', start_julmin
      !write(*,*) 'Time Step       ', io_files(qual,io_tide,io_write).interval
      !write(*,*) 'Number of Nodes ',       numnode
      !write(*,*) 'Number of Consitutents',        neq
      !write(*,*) 'Constituent modeled ', (constituents(l).constituent,l=1,neq)
      !write(*,*)  'Mapping function',(l,l=1,numnode) 
      !write(*,*) 'Qual Node ',(nodequal2dsm(l),l=1,numnode) 

      write(io_files(qual,io_tide,io_write).unit) dsm2_version
      write(io_files(qual,io_tide,io_write).unit) start_date
      write(io_files(qual,io_tide,io_write).unit) start_julmin
      write(io_files(qual,io_tide,io_write).unit) 
     &     io_files(qual,io_tide,io_write).interval
      write(io_files(qual,io_tide,io_write).unit) numnode
      write(io_files(qual,io_tide,io_write).unit) neq
c      write(io_files(qual,io_tide,io_write).unit) (l, nodequal2dsm(l),l=1,numnode) 
      write(io_files(qual,io_tide,io_write).unit) (l,l=1,numnode) 
      write(io_files(qual,io_tide,io_write).unit) (nodequal2dsm(l),l=1,numnode) 
c-----Write mapping function
c      do l=1,numnode
c         write(io_files(qual,io_tide,io_write).unit) l, nodequal2dsm(l)
c      enddo

      return
      end
 
