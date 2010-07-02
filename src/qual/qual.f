

      program dsm2qual
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
     &     JN,K,CONS_NO,
     &     NN,KK
      integer res_num_clfct
      logical echo_only, file_exists

      real*8    HR,SVOL,tTIME,VJ,VOL
      real*8    TOTFLO
      real*8    C(MAX_CONSTITUENT)
     &     ,objflow,massrate(max_constituent) ! flow and massrate at object

      integer*4
     &     incr_intvl           ! increment julian minute by interval function
     &     ,next_output_flush   ! next time to flush output
     &     ,next_display        ! next time to display model time
     &     ,next_restart_output ! next time to write restart file


      integer
     &     istat                ! status of fixed input        
     &     ,ibound
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
         inquire(file=init_input_file, exist=file_exists)      
         if (.not. file_exists)then
             write(unit_error,*)"Input file does not exist: ",init_input_file
             call exit(1)     
         end if
         call input_text(init_input_file)  ! reads and echoes text
         call process_initial_text()       ! reads scalar and envvars from buffer and processes
         call initialize_runtimes
         call buffer_input_tidefile()      ! process tidefile name(s)
         call read_grid_from_tidefile()
         call buffer_input_grid()    ! processes grid
      end if
c----- load header information from the first hydro tidefile
c      this assures that names of qext and stage boundaries are available
      call read_tide_head(tide_files(1).filename, .false.)
      ! Loop through number of stage boudnaries and set node_geom
      do ibound = 1,nstgbnd
          node_geom(stgbnd(ibound).node).boundary_type=stage_boundary
      end do      
      
      
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
      
      call read_boundary_values

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

C--------set boundary and junction values to zero
         DO N=1,NBRCH
            DVU(N)=0.0
            DVD(N)=0.0
            DO CONS_NO=1,NEQ
               GPTU(CONS_NO,N)=0.0
               GPTD(CONS_NO,N)=0.0
            ENDDO
         ENDDO

C--------set internal nodes and non-stage-boundary nodes to NOT_MIXED
         ! todo: how to deal with stage boundaries?         
         DO N=1,NNODES
                   
            if (node_geom(N).boundary_type .ne. stage_boundary) then 
                JCD(N)=NOT_MIXED             
            end if                
         ENDDO         
         
         call read_boundary_values

C--------Read the Hydro tidefile
         call read_mult_tide
         CALL INTERPX

c--------calculate total flow and mass into boundary nodes. This prepares GPTU and GPTD for ROUTE.     
         DO 360 N=1,NBRCH
            DO CONS_NO=1,NEQ   ! includes stage boudnary GTRIB from store_values
               GPTU(CONS_NO,N)=GTRIB(CONS_NO,1,N)
               GPTD(CONS_NO,N)=GTRIB(CONS_NO,NXSEC(N),N)
            ENDDO                                   
 360     ENDDO

C--------Initialize reservoir stuff
         DO I=1,nreser
            resDepth = ResVol(I)/ARes(I)
            IRev = I
            reschgvol(i)=0.0    ! for masstracking
            do cons_no=1,NEQ   ! copy to temp
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

         ! set cj(cons_no,jn) to zero for internal nodes and non-stage-boundary nodes
         DO JN=1,NNODES
            if (node_geom(JN).boundary_type .NE. stage_boundary) then
               DO CONS_NO=1,NEQ
                  cj_prev(CONS_NO,JN)=cj(CONS_NO,JN)
                  cj(cons_no,jn)=0.0 ! for masstracking
               ENDDO
            endif
         ENDDO

C--------update junction concentrations and codes
C--------compute inflow flux at known junction        
         AllJunctionsMixed=.false.
         do while (.not. AllJunctionsMixed)  
            DO 640 JN=1,NNODES     

               if (node_geom(JN).boundary_type .NE. stage_boundary) then
                  TOTFLO=0.
                  IF (JCD(JN) .EQ. MIXED) GOTO 640                 
                  
                  ! find JN of transfer upstream node and check if it's mixed
                  ! if not mixed then wait
                  i_node_flow =1
                  do while (node_geom(JN).qinternal(i_node_flow) .ne. 0)                
                      qndx = node_geom(JN).qinternal(i_node_flow)
                      if ( obj2obj(qndx).flow_avg > 0 ) then 
                          from_obj_type = obj2obj(qndx).from_obj.obj_type
                          from_obj_no   = obj2obj(qndx).from_obj.obj_no
                      else if ( obj2obj(qndx).flow_avg < 0 ) then 
                          from_obj_type = obj2obj(qndx).to_obj.obj_type
                          from_obj_no   = obj2obj(qndx).to_obj.obj_no
                      else  ! obj2obj(qndx).flow_avg == 0 
                          goto 747 ! next node_flow. don't wait if flow_avg is 0                   
                      endif 
                                               
                      if (from_obj_type .eq. obj_node) then
                      
                          ! if upstream transfer node is not itself
                          if  (from_obj_no .ne. JN)  then 
                              if (JCD(from_obj_no) .ne. mixed) then
                                  goto 640  !wait for next loop
                              endif
                          endif 
                      endif       
 747                  i_node_flow = i_node_flow + 1
                  end do
                  ! finished checking internal transfer upstream node mixing                           
                  
                  VJ=0.0
                  DO KK=1,NUMUP(JN)
                     N=LISTUP(JN,KK)
                     TOTFLO=TOTFLO-FLOW(N,1,1)
                     IF (FLOW(N,1,1).LT.0.0) THEN
                        IF (abs(dvu(n)) .gt. 1.0 ) THEN
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
                        IF (abs(dvd(n)).gt. 1.0 )  THEN
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

           endif ! if not stage-boundary node              
               
 640     ENDDO !  DO 640 JN=1,NNODES
            
         AllJunctionsMixed=.true.
         DO JN=1,NNODES
           if (node_geom(JN).boundary_type .ne. stage_boundary) then
               IF(JCD(JN) .NE. MIXED) THEN
C--------------------This JUNCTION NOT MIXED YET. Have to go back
                  AllJunctionsMixed=.false.
                  DO CONS_NO=1,NEQ
                    CJ(CONS_NO,JN)=0.0
                  ENDDO
               ENDIF
            endif
         ENDDO
         
         ENDDO ! do while (.not.AllJunctionsMixed)
  

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
            IF(JPO.EQ.0) GO TO 730
            IF(MOD(JTIME,JPO).NE.0) GO TO 730
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
