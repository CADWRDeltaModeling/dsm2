C!<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    DSM2 is free software: you can redistribute it and/or modify
C!    it under the terms of the GNU General Public !<license as published by
C!    the Free Software Foundation, either version 3 of the !<license, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public !<license for more details.

C!    You should have received a copy of the GNU General Public !<license
C!    along with DSM2.  If not, see <http://www.gnu.org/!<licenses/>.
C!</license>


*== Public  (FourPt) =================================================

      PROGRAM FourPt

	use groups, only: extractrange
      use io_units
      use dsm2_database
      use type_defs
      use constants
      use runtime_data
      use iopath_data
      use grid_data
      
      IMPLICIT NONE

*   Purpose:  Compute 1-dimensional streamflow in a network of open
*             channels, in terms of discharge and water-surface
*             elevation.
*
*             Optionally density is allowed to vary with time and
*             distance.  Currently density at both ends of each channel
*             must be read from a user-supplied file if this option is
*             used.
*
*             Optionally sinuosity is allowed to vary with depth of flow
*             and distance.  Sinuosity information is supplied through
*             channel-properties tables.

*   Version:  1993.01 (FORTRAN)

*                      Lew DeLong
*                      U.S.G.S., Water Resources Division
*                      Office of Surface Water
*                      Stennis Space Center, MS, 39529
*                      January 1992
*
*                      David Thompson, formerly with U.S.G.S., currently
*                      with Texas Tech University, is primarily responsible
*                      for adding file utilities, buffered output of
*                      time-series and space-series results, string
*                      utilities, the master-file look-up module for
*                      relating user-supplied file names and unit numbers
*                      with internal defaults.
*
*                      Janice Fulford contributed routines necessary
*                      to represent general 3-parameter ratings
*                      capable of representing hydraulic structures.
*
*                      Other persons involved in the coding or
*                      modification of code used by FourPt include
*                      Barry Wicktom, Jenifer Johnson, Victoria Israel.

      include '../hydrolib/network.inc'
      include '../hydrolib/solver.inc'
      include '../hydrolib/netcntrl.inc'
      include '../hydrolib/chnlcomp.inc'
      include '../hydrolib/chconnec.inc'
      include '../hydrolib/chstatus.inc'
      include '../timevar/dss.inc'
      include '../timevar/readdss.inc'
c-----include '../input/time-varying/writedss.inc'

*   Local variables:
      LOGICAL   OK, isopen

      integer*4
     &     incr_intvl           ! increment julian minute by interval function
     &     ,next_output_flush   ! next time to flush output
     &     ,next_display        ! next time to display model time
     &     ,next_restart_output ! next time to write restart file

      integer
     &     istat                ! status of fixed input
     &     ,i,j                 ! loop index

      character
     &     init_input_file*130  ! initial input file on command line [optional]
     &     ,jmin2cdt*14         ! convert from julian minute to char date/time

*   Routines by module:

***** Network control:
      INTEGER  NetworkTimeSteps, TotalNetworkIterations
      EXTERNAL NetworkTimeSteps, TotalNetworkIterations

      LOGICAL  InitializeNetworkControl
      EXTERNAL InitializeNetworkControl

      LOGICAL  IncrementNetworkTimeStep
      EXTERNAL IncrementNetworkTimeStep

      LOGICAL  SetBoundaryValuesFromData
      EXTERNAL SetBoundaryValuesFromData

***** Local:
      LOGICAL  DefineNetwork, UpdateNetwork,CloseSolver
      EXTERNAL DefineNetwork, UpdateNetwork,CloseSolver

***** Network volume and mass balance:

      LOGICAL  AverageFlow, WriteNetworkRestartFile
      EXTERNAL AverageFlow, WriteNetworkRestartFile

      LOGICAL  InitNetBalance, UpdateNetBalance, InitReservoirFlow
      EXTERNAL InitNetBalance, UpdateNetBalance, InitReservoirFlow
      LOGICAL  ReportNetBalance
      EXTERNAL ReportNetBalance

***** Channel status:

      INTEGER  NumberOfChannels,TotalStreamLocations
      EXTERNAL NumberOfChannels,TotalStreamLocations

      LOGICAL  WriteHydroToTidefile, Calculate_Chan_Net_Flow
      LOGICAL  InitializeChannelNetwork, InitializeSolver,InitOpRules

      LOGICAL Compute_ChArea
      EXTERNAL Compute_ChArea

      EXTERNAL WriteHydroToTidefile, Calculate_Chan_Net_Flow
      EXTERNAL InitializeChannelNetwork, InitializeSolver,InitOpRules
      LOGICAL CloseHDF5
      EXTERNAL CloseHDF5
      
      logical, external :: order_nodes

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:   Lew DeLong
*   Last modified: December 1992
*   Version 93.01, January, 1993
*   Last modified: October 1994 Parviz Nader DWR
*   Last modified: September 1996 Ralph Finch DWR

      data init_input_file /' '/

*-----Implementation -------------------------------------------------



c-----DSM2 module, name and version number
      include 'version.inc'

      open (
     &    unit_screen
     &    ,carriagecontrol='list'
     &    ,buffered='NO'
     &    ,iostat=istat
     &    )
      open (
     &    unit_error
     &    ,carriagecontrol='list'
     &    ,buffered='NO'
     &    ,iostat=istat
     &    ) !! <NT>

c-----get optional starting input file from command line and
c-----simulation name for Database read

      call get_command_args(init_input_file, model_name)

c-----dsm2 initialization
      call dsm2_init

      if(.Not. InitOpRules() ) Then
         write(unit_error,*)
     &        ' Initialization of Gate Ops failed...'
         call exit(1)
      end if

c---- begin data reading
      database_name=miss_val_c
c---- read all text into buffers and process envvironmental variables
      if (init_input_file .ne. ' ') then
         call input_text(init_input_file)  ! reads and echoes text
         call process_initial_text()       ! process scalar and envvars
         call buffer_input_grid()    ! processes grid
      end if

c---- possibly read from db, though it is hobbled now
      if ( database_name .ne. miss_val_c .and.
     &      model_name .ne. miss_val_c 
     &      .and. model_name .ne. 'none') then
         write(unit_screen,*) "Database name given: ",trim(database_name),","
         write(unit_screen,*) "Model name given: ",trim(model_name),","
         write(unit_screen,*) "Reading from database. If not desired,"
         write(unit_screen,*) "set model_name to 'none' in SCALARS or remove it"
         write(unit_screen,*) "to read only from text"

         call init_database(istat)
         if (istat .ne. 0) then
            write(unit_error, *) 'Error initializing database; run stopped.'
            call exit(1)
         endif
         call read_sql(istat)
         if (istat .ne. 0) then
            write(unit_error, *) 'Error in loading fixed data from RDMS; run stopped.'
            call exit(1)
         endif
      endif
      
c------ process input that is in buffers
      call buffer_input_common()
      call buffer_input_hydro()

      call process_text_gate_input()
      call process_text_oprule_ts_input()      
      call write_input_buffers()

c------ end of input reading and echo, start checking data
      

      call check_fixed(istat)
      if (istat .ne. 0) then
         write(unit_error, *)
     &        'Error in checking fixed data; run stopped.'
         call exit(1)
      endif

      call virtual_xsect        ! create virtual cross-section lookup table

      call check_fixed_hydro(istat)
      if (istat .ne. 0) then
         write(unit_error, *)
     &        'Error in checking fixed fourpt data; run stopped.'
         call exit(1)
      endif

      prev_julmin=0
      julmin=start_julmin
      current_date=jmin2cdt(julmin)

c-----calculate julian minute of end of each DSS interval
      call update_intervals

      if(.Not. InitializeChannelNetwork() ) Then
         write(unit_error,*)
     &        ' Initialization of Channel network failed...'
         call exit(1)
      end if

      call AssignGateCompPoints() ! Attach gates to hydro computational points

      if(.Not. InitReservoirFlow() ) Then
         write(unit_error,*)
     &        ' Initialization of Reservoir flow failed...'
         call exit(1)
      end if

      ! Oprules cannot be parsed until channel network is defined
      call process_text_oprule_input()
      call close_database()

      if ( .not. InitializeSolver() ) THEN
         write(unit_error,*)
     &        ' Initialization of SPARSE matrix solver failed...'
         call exit(1)
      end if

      OK = InitNetBalance()
      call init_store_outpaths(istat)

      if ( io_files(hydro,io_hdf5,io_write).use ) then ! hydro binary file output
         call DetermineFirstTidefileInterval()
         call InitHydroTidefile
	   OK = WriteHydroToTidefile()
      endif

 605  format('Starting DSM2-Hydro at time: ',a)
      write(unit_output,605) current_date
      write(unit_screen,605) current_date
      call store_outpaths(.false.)
      next_display=incr_intvl(start_julmin,display_intvl,TO_BOUNDARY)
      next_output_flush=incr_intvl(start_julmin,flush_intvl,TO_BOUNDARY)
      if (io_files(hydro,io_restart,io_write).use) then
         next_restart_output=incr_intvl(start_julmin,io_files(hydro,
     &        io_restart,io_write).interval,TO_BOUNDARY)
      endif

      prev_julmin=julmin
      julmin=julmin+time_step
      current_date=jmin2cdt(julmin)

      do while (julmin .le. end_julmin) ! normal time run
         

         DO I=1,TotalStreamLocations()
            QOld(I)=Q(I)
         ENDDO

         DO i=1,Nreser
            YResOld(i)=YRes(i)
            DO j=1,res_geom(i).nnodes
               QResOld(i,j)=QRes(i,j)
            ENDDO
         ENDDO

         OK=IncrementNetworkTimeStep()
c--------calculate julian minute of end of each DSS interval
         call update_intervals

         if (julmin .ge. next_display) then
 610        format('Starting Hydro computations for time: ',a)
            write(unit_output,610) current_date
            write(unit_screen,610) current_date
            next_display=incr_intvl(next_display,display_intvl,
     &           TO_BOUNDARY)
         endif

         if (check_input_data) then
c-----------just check input data for bogus values; no simulation
            OK = SetBoundaryValuesFromData()
         else                   ! full simulation
            IF (UpdateNetwork()) THEN
               OK = UpdateNetBalance()
            else
               write (unit_error,*)
     &              ' Network update failed at time ',current_date
               write (unit_error,*) ' Abnormal program end.'
               call exit(1)
            end if

            if (julmin .ge. next_output_flush) then
               next_output_flush=incr_intvl(next_output_flush,
     &              flush_intvl,TO_BOUNDARY)
               call store_outpaths(.true.)
            else
               call store_outpaths(.false.)
            endif

            if ( io_files(hydro,io_hdf5,io_write).use ) then
               OK=WriteHydroToTidefile()
            endif

            if (io_files(hydro,io_restart,io_write).use) then
               if (Restart_Write .and. julmin .ge. next_restart_output) then
C-----------------Write the hydrodynamic information at the end of
c-----------------every interval to ascii file in case of any
c-----------------interruptions to the model
                  next_restart_output=incr_intvl(next_restart_output,
     &                 io_files(hydro,io_restart,io_write).interval,
     &                 TO_BOUNDARY)
                  OK = WriteNetworkRestartFile()
               endif

               if (.not. restart_write) then
                  next_restart_output=incr_intvl(start_julmin,io_files(hydro,
     &                 io_restart,io_write).interval,TO_BOUNDARY)
                  restart_write=.true.
          !  fixme: what are these next 2 lines. Should it be restart???
                  io_files(hydro,io_tide,io_write).use=.true.
                  io_files(hydro,io_hdf5,io_write).use=.true.
               endif
            endif
         endif

         prev_julmin=julmin
         julmin=julmin+time_step
         current_date=jmin2cdt(julmin)
      enddo

      if (julmin .gt. end_julmin) then
         julmin=prev_julmin
         prev_julmin=prev_julmin-time_step
         current_date=jmin2cdt(julmin)
      endif

      if (.not. check_input_data) then
         if (io_files(hydro,io_restart,io_write).use) then
*-----------Write network restart file.
            OK = WriteNetworkRestartFile()
         endif


*--------Write time-series network results.
         call store_outpaths(.true.) ! flush temp files
         if (need_tmp_outfiles .and.
     &        .not. binary_output) call wrt_outpaths
      endif

c--------close HDF5
      if (io_files(hydro,io_hdf5,io_write).use) then
         OK = CloseHDF5()
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

*-----Compute and report final volume and mass balances.
c@@@         OK = ReportNetBalance()

      WRITE(unit_screen,*) '   -----------------------------'
      WRITE(unit_screen,*) ' '
      WRITE(unit_screen,*) ' ',
     &     TotalNetworkIterations(),' total network iterations...'

      WRITE(unit_screen,*) ' '
      WRITE(unit_screen,*) '   Normal program end.'
      WRITE(unit_screen,*) ' '
      WRITE(unit_screen,*) '   -----------------------------'



      WRITE(unit_output,*) '   -----------------------------'
      WRITE(unit_output,*) ' '
      WRITE(unit_output,*)
     &     TotalNetworkIterations(),'  total network iterations...'
      WRITE(unit_output,*) ' '
      WRITE(unit_output,*) ' Normal program end.'
      WRITE(unit_output,*)
     &     '   -----------------------------'

      

      OK = CloseSolver()

      inquire(unit_output,opened=isopen)
      if(isopen)close(unit_output, err=1222)
      inquire(unit_screen,opened=isopen)
      if(isopen)close(unit_output, err=1222)

1222  call exit(0)

      END

