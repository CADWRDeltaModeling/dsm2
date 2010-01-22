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

      program watqual
      Use IO_Units
      use common_qual
      use common_tide
      use runtime_data
      use iopath_data

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
C-----GPTD(L,N)    flux of L at ds end of branch N (seems to just be concentration)
C-----GPTI(L,K,N)    conc. of L as parcel K entered branch N
C-----GPTR(L,K,N)    increase of L in parcel N,K due to tribs.
C-----GPTU(L,N)    flux of L at us end of branch N (seems to just be concentration)
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
      INCLUDE '../hydrolib/network.inc'

      INCLUDE 'bltm1.inc'
      INCLUDE 'bltm3.inc'
      INCLUDE 'bltm2.inc'
      INCLUDE 'kinetic1.inc'
      LOGICAL AllJunctionsMixed

c-----variables for mass tracking

C-----+ + + LOCAL VARIABLES + + +C

      integer I,INX,
     &     JN,K,CONS_NO,M,
     &     NN,KK
      integer res_num_clfct
      logical echo_only

      real*8    HR,SVOL,tTIME,VJ,VOL
      real*8    TOTFLO
      real*8    C(MAX_CONSTITUENT)
     &     ,objflow,massrate(max_constituent) ! flow and massrate at object

      integer*4
     &     incr_intvl           ! increment julian minute by interval function
     &     ,next_output_flush   ! next time to flush output
     &     ,next_display        ! next time to display model time
     &     ,next_restart_output ! next time to write restart file
     &     ,next_binary_output  ! next time to write binary file

      integer
     &     istat                ! status of fixed input

      character
     &     init_input_file*130  ! initial input file on command line [optional]
     &     ,jmin2cdt*14         ! convert from julian minute to char date/time

      integer iprnt_mass
      common /mass_tracking_1/ iprnt_mass

      include '../timevar/dss.inc'
      include '../timevar/readdss.inc'
      include '../timevar/writedss.inc'

	integer MIXED, NOT_MIXED, QNDX, i_node_flow, from_obj_type, from_obj_no
	parameter (MIXED=0,NOT_MIXED=2)

      data init_input_file /' '/

C-----+ + + INPUT FORMATS + + +
 1010 FORMAT (10X,10I7)
 1050 FORMAT (24X,F7.3,10F6.3)
 1060 FORMAT (2X,I3,3X,I2,11F7.3)
 1110 FORMAT (3I5)
 1120 FORMAT (8f10.4)
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
      dsm2_module =  qual
      dsm2_name   = 'Qual'


      open (
     &    unit_screen
     &    ,carriagecontrol='list'
     &    ,buffered='NO'
     &    ) !! <NT>
      open (
     &    unit_error
     &    ,carriagecontrol='list'
     &    ,buffered='NO'
     &    ) !! <NT>

c-----get optional starting input file from command line and
c-----simulation name for Database read
      call get_command_args(init_input_file, model_name,echo_only)

c-----dsm2 initialization
      call dsm2_init

c---- begin data reading

c---- read all text into buffers and process envvironmental variables
      if (init_input_file .ne. miss_val_c) then
         call input_text(init_input_file)  ! reads and echoes text
         call process_initial_text()       ! reads scalar and envvars from buffer and processes
         call buffer_input_tidefile()      ! todo: remove from buffer_input_common
         call read_grid_from_tidefile()    ! todo
         call buffer_input_grid()    ! processes grid
      end if
      
c------ process input that is in buffers
      call buffer_input_common()        ! process common items
      call buffer_input_qual()          ! process qual specialty items
      
      call write_input_buffers()
      if (echo_only) call exit(1)


c------ end of input reading and echo, start checking data

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
      current_date=jmin2cdt(julmin)

      call InitHDF5MemoryDims()


      CALL BLTMINIT
      DTT=DT*3600.              ! hard coded, time step in minute
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

      IF(MASS_TRACKING) THEN
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
               mass_export(i,CONS_NO)=0. ! export node no and export locations
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

      if (check_input_data) then
c--------just check input data for bogus values; no simulation

 604     format('Checking data at time: ',a)
         write(unit_screen,604) current_date

C--------start time loop for checking boundary data

         prev_julmin=julmin
         julmin=julmin+time_step
         current_date=jmin2cdt(julmin)

         do while (julmin .le. end_julmin)

            if (julmin .ge. next_display) then
 611           format('Starting data-check for time: ',a)
               write(unit_screen,611) current_date
               next_display=incr_intvl(next_display,display_intvl,
     &              TO_BOUNDARY)
            endif

            call read_boundary_values
C-----------Read the Hydro tidefile
            call read_mult_tide

            prev_julmin=julmin
            julmin=julmin+time_step
            current_date=jmin2cdt(julmin)
         enddo
         go to 790
      endif

      call update_intervals

      call init_store_outpaths(istat)

 605  format('Starting DSM2-Qual run at time: ',a)
      write(unit_screen,605) current_date
      call store_outpaths(.false.)

C-----start time loop

      prev_julmin=julmin
      julmin=julmin+time_step
      current_date=jmin2cdt(julmin)

      do while (julmin .le. end_julmin)
         call update_intervals
         if (julmin .ge. next_display) then
            write(unit_screen,610) current_date
 610        format('Starting Qual computations for time: ',a)
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
         DO 330 N=1,NNODES
            if (node_geom(n).qual_int) JCD(N)=NOT_MIXED
 330     CONTINUE

         call read_boundary_values

C--------Read the Hydro tidefile
         call read_mult_tide
         CALL INTERPX

         DO 360 N=1,NBRCH
            !upstream
            JN=JNCU(N)
c-----------calculate total flow and mass into this node for each constituent
            if ( .not. node_geom(JN).qual_int) then ! external boundary node
                if (node_geom(JN).boundary_type .NE. stage_boundary) then
                    call node_rate(jn,TO_OBJ,0,objflow,massrate) 
                    IF ( objflow .gt. 0.) THEN ! source at node
                        QNODE(jn)=objflow
                        DO CONS_NO=1,NEQ
                            GTRIB(CONS_NO,1,N)=massrate(CONS_NO)/objflow
                        ENDDO
                    ENDIF
                endif
            endif    
           
            !downstream 
            JN=JNCD(N)
            if ( .not. node_geom(JN).qual_int) then ! external boundary node
                if (node_geom(JN).boundary_type .NE. stage_boundary) then
                    call node_rate(jn,TO_OBJ,0,objflow,massrate) 
                    IF ( objflow .gt. 0.) THEN ! source at node
                        QNODE(jn)=objflow
                        DO CONS_NO=1,NEQ
                            GTRIB(CONS_NO,NXSEC(N),N)=massrate(CONS_NO)/objflow
                        ENDDO
                    ENDIF
                endif
            endif 

            
            
            DO CONS_NO=1,NEQ
               GPTU(CONS_NO,N)=GTRIB(CONS_NO,1,N)
               GPTD(CONS_NO,N)=GTRIB(CONS_NO,NXSEC(N),N)
            ENDDO
                        
            
 360     CONTINUE





C--------Initialize reservoir stuff
         DO I=1,nreser
            resDepth = ResVol(I)/ARes(I)
            IRev = I
            reschgvol(i)=0.0    ! for masstracking
            do cons_no=1,NEQ
               C(cons_no) = CRES(i,cons_no)
               reschgconc(i,cons_no)=0.0 ! for masstracking
            end do
            iskip = 1
            if (no_of_nonconserve_constituent .gt. 0) then
               chan_res = 2     ! pointer to reservoir for kinetic computations
               call rate_chanres(i)
               call kinetic(c)
            end if
            if(MASS_TRACKING)then
               do cons_no = 1, nEq
                  AmountDecayed(cons_no)=AmountDecayed(cons_no)+
     &                 (CRes(I,cons_no)-C(cons_no))*ResVol(i)
               end do
            end if
            DO cons_no = 1, nEq
               CRes(I,cons_no) = C(cons_no)
            ENDDO
         ENDDO

         CALL ROUTE

C--------update junction concentrations and codes
C--------compute inflow flux at known junction



         DO JN=1,NNODES
            if (node_geom(jn).qual_int) then
               DO CONS_NO=1,NEQ
                  cj_prev(CONS_NO,JN)=cj(CONS_NO,JN)
                  cj(cons_no,jn)=0.0 ! for masstracking
               ENDDO
            endif
         ENDDO

 400     CONTINUE
         AllJunctionsMixed=.false.
         do while (.not.AllJunctionsMixed)   ! line 627
            DO 640 JN=1,NNODES     ! line 611

               if (node_geom(jn).qual_int) then

                  TOTFLO=0.
                  IF (JCD(JN) .EQ. MIXED) GOTO 640
                  
                  
                  ! find JN of transfer upstream node and check if it's mixed
                  ! if not mixed then wait
                  i_node_flow =1
                  do while (node_geom(JN).qinternal(i_node_flow) .ne. 0)                
                      qndx = node_geom(JN).qinternal(i_node_flow)
                      if ( obj2obj(qndx).flow_avg >= 0 ) then 
                          from_obj_type = obj2obj(qndx).from_obj.obj_type
                          from_obj_no   = obj2obj(qndx).from_obj.obj_no
                      else
                          from_obj_type = obj2obj(qndx).to_obj.obj_type
                          from_obj_no   = obj2obj(qndx).to_obj.obj_no                     
                      endif 
                                               
                      if (from_obj_type .eq. obj_node) then
                      
                          ! if upstream transfer node is not itself
                          if  (from_obj_no .ne. JN)  then 
                              if (JCD(from_obj_no) .ne. mixed) then
                                  goto 640  !wait for next loop
                              endif
                          endif 
                      endif       
                  i_node_flow = i_node_flow + 1
                  end do
                  ! finished checking transfer upstream node mixing
                  
                  VJ=0.0
                  DO KK=1,NUMUP(JN)
                     N=LISTUP(JN,KK)
                     TOTFLO=TOTFLO-FLOW(N,1,1)
                     IF (FLOW(N,1,1).LT.0.0) THEN
                        IF (abs(dvu(n)) .gt. 1.0 .AND.
     &                       node_geom(JNCD(N)).qual_int) THEN
                           !print*,"upstream unknown flow gt 0"
                           !print*,"Channel: Internal: ",N," Ext: ", chan_geom(N).chan_no
                           !print*,"D Node: Internal: ",JNCD(N)," Ext: ", node_geom(JNCD(N)).node_id
                           !print*,"Also Up: ", JNCU(N),node_geom(JNCU(N)).node_id
                           GOTO 640
                        END IF
                        VJ=VJ-FLOW(N,1,1)*DTT
                        DO CONS_NO=1,NEQ
                           CJ(CONS_NO,JN)=CJ(CONS_NO,JN)+GPTU(CONS_NO,N)
                        ENDDO
                     END IF
                  ENDDO

                  DO KK=1,NUMDOWN(JN)
                     N=LISTDOWN(JN,KK)
                     TOTFLO=TOTFLO+FLOW(N,1,NXSEC(N))
                     IF (FLOW(N,1,NXSEC(N)).GT.0.0) THEN
                        IF (abs(dvd(n)).gt. 1.0 .AND.
     &                       node_geom(JNCU(N)).qual_int)  THEN
                           GOTO 640
                        END IF
                        VJ=VJ+FLOW(N,1,NXSEC(N))*DTT
                        DO CONS_NO=1,NEQ
                           CJ(CONS_NO,JN)=CJ(CONS_NO,JN)+GPTD(CONS_NO,N)
                        ENDDO
                     END IF
                  ENDDO

C-----------------Now add the effects of external and internal flows, and reservoirs
                  call node_rate(JN,TO_OBJ,0,objflow,massrate)

                  VJ=VJ+objflow*DTT
                  TOTFLO=TOTFLO+objflow

                  DO CONS_NO=1,NEQ
                     CJ(CONS_NO,JN)=CJ(CONS_NO,JN)+massrate(CONS_NO)*DTT
                  ENDDO
                  call node_rate(JN,FROM_OBJ,0,objflow,massrate)
                  TOTFLO=TOTFLO+objflow

                  IF (VJ.GT.0.0) THEN
                     DO CONS_NO=1,NEQ
                        CJ(CONS_NO,JN)=CJ(CONS_NO,JN)/VJ
                        cj_prev(CONS_NO,JN)=cj(CONS_NO,JN)
                     ENDDO
                  ENDIF

                  JCD(JN)=MIXED
C-----------------At this point all the flows entering the junction have
c-----------------been mixed.
C-----------------update GPTU,GPTD,DVU,DVD,PT,PTI,PTR
                  DO KK=1,NUMUP(JN)
                     N=LISTUP(JN,KK)

                     IF (FLOW(N,1,1).GT.0.0) THEN
                        VOL=GPV(N,1)+DVD(N)
                        DO CONS_NO=1,NEQ
                           GPTU(CONS_NO,N)=CJ(CONS_NO,JN)*DTT*FLOW(N,1,1)
                           GPT(CONS_NO,1,N)=(CJ(CONS_NO,JN)*FLOW(N,1,1)*DTT+
     &                          GPT(CONS_NO,1,N)*VOL)/VOL
                           GPTD(CONS_NO,N)=GPTD(CONS_NO,N)+CJ(CONS_NO,JN)*DVD(N)
                        ENDDO
                     END IF
                     DVD(N)=0.0
                  ENDDO

                  DO KK=1,NUMDOWN(JN)
                     N=LISTDOWN(JN,KK)
                     IF (FLOW(N,1,NXSEC(N)).LT.0.0) THEN

C                  ! @todo: NS(N) should never be zero. This condition was encountered by Jon.                        
	                  IF (NS(N).GT.0) THEN  
                          VOL=GPV(N,NS(N))+DVU(N)
                          DO CONS_NO=1,NEQ
                             GPTD(CONS_NO,N)=CJ(CONS_NO,JN)*DTT*FLOW(N,1,NXSEC(N))
                             GPT(CONS_NO,NS(N),N)=(-CJ(CONS_NO,JN)*FLOW(N,1,NXSEC(N))*DTT+
     &                            GPT(CONS_NO,NS(N),N)*VOL)/VOL
                             GPTU(CONS_NO,N)=GPTU(CONS_NO,N)+CJ(CONS_NO,JN)*DVU(N)
                          ENDDO
	                  ELSE
	                    write(unit_error, *) "0 parcels in channel",N
	                  END IF

c                        VOL=GPV(N,NS(N))+DVU(N)
c                        DO CONS_NO=1,NEQ
c                           GPTD(CONS_NO,N)=CJ(CONS_NO,JN)*DTT*FLOW(N,1,NXSEC(N))
c                           GPT(CONS_NO,NS(N),N)=(-CJ(CONS_NO,JN)*FLOW(N,1,NXSEC(N))*DTT+
c     &                          GPT(CONS_NO,NS(N),N)*VOL)/VOL
c                           GPTU(CONS_NO,N)=GPTU(CONS_NO,N)+CJ(CONS_NO,JN)*DVU(N)
c                        ENDDO

                     END IF
                     DVU(N)=0.0
                  ENDDO
               endif
 640     ENDDO !line 502    DO 640 JN=1,NNODES
            
         AllJunctionsMixed=.true.
         DO JN=1,NNODES
           if (node_geom(jn).qual_int) then
               IF(JCD(JN) .NE. MIXED) THEN
C--------------------This JUNCTION NOT MIXED YET. Have to go back
                  AllJunctionsMixed=.false.
                  DO CONS_NO=1,NEQ
                    CJ(CONS_NO,JN)=0.0
                  ENDDO
               ENDIF
            endif
         ENDDO
         
         
      ENDDO !line 501      do while (.not.AllJunctionsMixed)
      
      
      

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
               PX(K,N)=dble(I)+1.0
               VJ=ABS(VOL)
               IF(VJ.GT.1.0E-6)PX(K,N)=dble(I)+SVOL/VOL
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

 735     tTIME=dble(JTIME+JTS)*DT
         IDAY=INT(tTIME/24.D0)+1
         HR=tTIME-dble(IDAY-1)*24.0

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
         current_date=jmin2cdt(julmin)
      enddo                     ! do while julmin time loop

      if (julmin .gt. end_julmin) then
         julmin=prev_julmin
         prev_julmin=prev_julmin-time_step
         current_date=jmin2cdt(julmin)
      endif

 790  continue

      if (.not. check_input_data) then

*--------Write time-series network results.
         call store_outpaths(.true.) ! flush temp files
         if (need_tmp_outfiles .and.
     &        .not. binary_output) call wrt_outpaths
      endif
c-----close all DSS input files
      i=1
      do while(i .le. max_dssinfiles .and.
     &     infilenames(i) .ne. ' ')
         call zclose (ifltab_in(1,i))
         i=i+1
      enddo
      if (dss_direct) then
c--------close all DSS output files
         i=1
         do while(i .le. max_dssoutfiles .and.
     &        outfilenames(i) .ne. ' ')
            call zclose (ifltab_out(1,i))
            i=i+1
         enddo
      endif

900   WRITE(*,*) '   -----------------------------'
      WRITE(*,*) ' '
      WRITE(*,*) '   Normal program end.'
      WRITE(*,*) ' '
      WRITE(*,*) '   -----------------------------'

      call exit(0)
      END
c==================================================

      subroutine read_boundary_values
      use iopath_data
      use grid_data
      implicit none

c-----read time-varying data arrays for Qual

c-----subroutine arguments

c-----common blocks

      include 'param.inc'
      include 'bltm1.inc'
      include 'bltm2.inc'

      include '../timevar/dss.inc'
      include '../timevar/readdss.inc'
      include '../timevar/writedss.inc'

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
      use common_qual
      use iopath_data
      implicit none
c-----Fill time-varying data arrays for Qual

c-----common blocks

      include 'param.inc'
      include 'bltm1.inc'
      include 'bltm2.inc'
      include 'bltm3.inc'

c-----local variables

      integer
     &     ptr,i,ic             ! array indices
     &     ,intchan             ! internal channel numbering
     &     ,intnode             !  node numbering
     &     ,constituent_no      ! constituent number

      do ptr=1,ninpaths

c--------don't use input paths which are only for replacement
c--------(priority 2 or higher)

         call get_inp_data(ptr) ! get input data from buffers

c--------meteorological values
         if (pathinput(ptr).variable .eq. 'cloud') then
            cloud=pathinput(ptr).value
         else if (pathinput(ptr).variable .eq. 'dryblb') then
            dryblb=pathinput(ptr).value
         else if (pathinput(ptr).variable .eq. 'wetblb') then
            wetblb=pathinput(ptr).value
         else if (pathinput(ptr).variable .eq. 'wind') then
            wind=pathinput(ptr).value
         else if (pathinput(ptr).variable .eq. 'atmpr') then
            atmpr=pathinput(ptr).value
         else
c-----------water quality constituent
            if (pathinput(ptr).obj_type .eq. obj_node) then
               intnode=pathinput(ptr).obj_no
c--------------only the stage type boundary concentration is used later from this section;
c--------------flow BCs are handled in node_rate and res_rate
               do i=1,nstgbnd
                  if (intnode .eq. stgbnd(i).node) then
                     !downstream stage boundary node
                     if ((node_geom(intnode).Nup .eq. 0) .and. (node_geom(intnode).Ndown .eq. 1)) then
                         intchan=node_geom(intnode).downstream(1)                     
                         do ic=1,pathinput(ptr).n_consts
                             constituent_no = pathinput(ptr).const_ndx(ic)
                             gtrib(constituent_no,nxsec(intchan),intchan)=
     &                       pathinput(ptr).value
                         enddo
                     !upstream stage boundary node
                     elseif ((node_geom(intnode).Nup .eq. 1) .and. (node_geom(intnode).Ndown .eq. 0)) then
                         intchan=node_geom(intnode).upstream(1)
                         do ic=1,pathinput(ptr).n_consts
                             constituent_no = pathinput(ptr).const_ndx(ic)
                             gtrib(constituent_no,1,intchan)=
     &                       pathinput(ptr).value
                         enddo
                     endif                     
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
      use common_qual
      use runtime_data
      use iopath_data
      Use IO_Units
      implicit none

c-----include files

      include 'param.inc'
      include 'bltm1.inc'

c-----arguments and local variables

      integer
     &     action               ! whether to read or write the restart file
     &     ,i,k,l,n             ! loop indices
     &     ,unit_restart        ! output unit
     &     ,n_all               ! number of incoming chemical constituents
     &     ,ext2int

      external ext2int

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

         write(unit_restart,900) dsm2_version, current_date
 900     format('Qual Version ',a
     &        /'The following data corresponds to   ',a14//)
         write(unit_restart,970)
     &        (trim(constituents(all_source_ptr(l)).name),
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
         write(unit_restart,975) (trim(constituents(all_source_ptr(l)).name),
     &        l=1,no_all_source)
 975     format('Initial Reservoir Salinity'/
     &        'Reservoir',11(1x,a11))

         do i=1,nreser
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
                  if (constituents(l).name .eq. cchem(i)) then
                     cstart(ext2int(k),l)=salavg(i)
                  endif
               enddo
            enddo
         enddo
c--------skip reservoir headers, assume chemical names are in same order
c--------as for channels
         read(unit_restart,911) header
 911     format(/a)
         do n=1,nreser
            read(unit_restart,*) k, (salavg(i), i=1,n_all)
            do l=1,neq
               cres(k,l)=0.0
               do i=1,n_all
                  if (constituents(l).name .eq. cchem(i)) then
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

      end subroutine

