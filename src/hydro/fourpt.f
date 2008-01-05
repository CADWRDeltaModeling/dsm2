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

      USE DFLIB                 !! <NT>
	use groups, only: extractrange
      use IO_Units
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
     &    ) !! <NT>
      open (
     &    unit_error
     &    ,carriagecontrol='list'
     &    ,buffered='NO'
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

      database_name='DSM2Input'

      if (init_input_file .ne. ' ') then
         call read_fixed(init_input_file,.true.,istat) !First pass is for envvars only
         if (istat .ne. 0) then
            write(unit_error, *)'Error in loading fixed data from text files; run stopped.'
            call exit(1)
         endif
         if (model_name .eq. miss_val_c)then
            write(unit_error, *)
     &           'Model name not loaded at command line or in text input file; run stopped.'
            call exit(1)
         end if
      end if

      call init_database(istat)
      if (istat .ne. 0) then
         write(unit_error, *) 'Error initializing database; run stopped.'
         call exit(1)
      endif

c-----read input for grid
      if (model_name .ne. miss_val_c) then
         call read_sql(istat)
         if (istat .ne. 0) then
            write(unit_error, *) 'Error in loading fixed data from RDMS; run stopped.'
            call exit(1)
         endif
      endif

      if (init_input_file .ne. ' ') then ! Second pass gives text input priority
         call read_fixed(init_input_file,.false.,istat)

         if (istat .ne. 0) then
            write(unit_error, *)'Error in loading fixed data from text files; run stopped.'
            call exit(1)
         endif
      end if

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

      call read_operating_rules(istat)
      if (istat .ne. 0) then
         write(unit_error, *)
     &        'Error in loading oprules data; run stopped.'
         call exit(1)
      endif
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

      
      inquire(unit_output,opened=isopen)
      if(isopen)close(unit_output, err=1222)
      OK = CloseSolver()

1222  write(unit_screen,*) 'Exit with code 0'
      call exit(0)


c--------close solver


      END

*== Public (UpdateNetwork) =============================================

      LOGICAL FUNCTION UpdateNetwork()
      Use Gates,only: NGate, GateArray

      use IO_Units
      use grid_data
      IMPLICIT NONE

*   Purpose:  Determine values of discharge and water surface elevation
*             within a network of open channels at a new time.

*   Arguments:

*   Argument Definitions:

*   Module data:


      INCLUDE '../hydrolib/network.inc'
      INCLUDE '../hydrolib/netcntrl.inc'
      INCLUDE '../hydrolib/chconnec.inc'
      INCLUDE '../hydrolib/solver.inc'

      INCLUDE '../hydrolib/chstatus.inc'

      INCLUDE '../hydrolib/netbnd.inc'
*   Local variables:
      INTEGER ChannelNumber
      LOGICAL OK
     &     ,ClosedIteration     ! indicator that iteration has been closed
*   Routines by module:

***** Channel schematic:
      INTEGER  NumberOfChannels
      EXTERNAL NumberOfChannels

      LOGICAL  OpenChannel, CloseChannel
      EXTERNAL OpenChannel, CloseChannel

***** Network control:
      LOGICAL  OpenNetworkIteration, CloseNetworkIteration
      LOGICAL  VariableStreamDensity
      EXTERNAL VariableStreamDensity
      EXTERNAL OpenNetworkIteration, CloseNetworkIteration

      LOGICAL  IncrementNetworkIteration
      EXTERNAL IncrementNetworkIteration

      LOGICAL  IncrementNetworkTimeStep, SetNetworkBoundaryEqValues
      EXTERNAL IncrementNetworkTimeStep, SetNetworkBoundaryEqValues

      LOGICAL  SetBoundaryValuesFromData,ApplyBoundaryValues
      EXTERNAL SetBoundaryValuesFromData,ApplyBoundaryValues

      INTEGER  NetworkIteration
      EXTERNAL NetworkIteration

      LOGICAL  CalculateReservoirFlow
      EXTERNAL CalculateReservoirFlow

      LOGICAL  CalculateGateFlow
      EXTERNAL CalculateGateFlow

      LOGICAL InitOpRules,AdvanceOpRules,TestOpRuleActivation
      EXTERNAL InitOpRules,AdvanceOpRules,TestOpRuleActivation

***** Solver:
      LOGICAL  SolveFourPt
      EXTERNAL SolveFourPt

***** Locals:
      LOGICAL  UpdateChannel, NetworkClosure
      EXTERNAL UpdateChannel, NetworkClosure

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:   Lew DeLong
*   Last modified: December 1992
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      UpdateNetwork = .FALSE.

c-----Load data from time series to boundary objects
      OK = SetBoundaryValuesFromData()

c-----Test gate operating rules, which may alter boundary values
c-----OK = TestGateOps()
      OK = AdvanceOpRules()
c-----Boundary values are final, apply to mdel    
      OK = ApplyBoundaryValues()

c-----Initialize (to zero) known part of right hand side vector and
c-----elapsed iterations 
      
      ClosedIteration=.false.
      Iteration=0
      Rescale=1.D0
      XOld=0.0
      do 100 while (.not. ClosedIteration)

*--------Begin iteration loop and clear matrix
         OK = IncrementNetworkIteration()

*--------Set iteration-specific part of 
*        right hand side to zero
         XAdj=0.

         if(NGate.GT.0)then
c-----------Calculate flow through all the gates
            OK = CalculateGateFlow()
         end if

         do 200 ChannelNumber = 1, NumberOfChannels()
            if( OpenChannel(ChannelNumber) ) then
               if( UpdateChannel() ) then
                  OK = CloseChannel()
               else
                  write(UNIT_ERROR,*) ' Update of channel',
     &                 chan_geom(ChannelNumber).chan_no,' failed...'
                  return
               end if
            else
               OK = CloseChannel()
            end if
	

 200     continue

         
         if(Nreser.GT.0)then
c-----------Calculate Reservoir flows
            OK = CalculateReservoirFlow()
         end if

*--------Solve for incremental change in dependent variables.

         if( SolveFourPt() ) then
         else
            write(UNIT_ERROR,*) ' *** Error (UpdateNetwork)'
            write(UNIT_ERROR,*) ' Failed to solve matrix...'
            return
         end if

*--------Update dependent variables and check for closure.
         closediteration=NetworkClosure()
 100  end do

*-----End iteration loop.
      OK = CloseNetworkIteration()

*-----Test operating rules for activation/finalization
      OK = TestOpRuleActivation()

      UpdateNetwork = .TRUE.

      return
      end

*== Public (UpdateChannel) =============================================

      LOGICAL FUNCTION UpdateChannel()
      use IO_Units
      IMPLICIT NONE

*   Purpose:  Update dependent variables defining a channel.  In this
*             implementation, dependent variables are flow and water-
*             surface elevation.

*   Arguments:

*   Argument definitions:

*   Module data:

*   Local variables:
      INTEGER Node, I

*   Routines by module:

***** Channel schematic:
      INTEGER  UpstreamPointer, DownstreamPointer
      EXTERNAL UpstreamPointer, DownstreamPointer

***** Flow equations:
      LOGICAL  DynamicWaveEq,DynamicWaveEqDS
      EXTERNAL DynamicWaveEq,DynamicWaveEqDS

***** Network control:
      LOGICAL  VariableStreamSinuosity
      EXTERNAL VariableStreamSinuosity

***** Locals:
      LOGICAL  SetConstraint
      EXTERNAL SetConstraint

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      UpdateChannel = .TRUE.

*-----Compute and store constraint-equation coefficients for
*      upstream end of channel.

      IF( SetConstraint(+1) ) THEN
      ELSE
         UpdateChannel = .FALSE.
         WRITE(UNIT_ERROR,*) ' *** Error (UpdateChannel)'
         WRITE(UNIT_ERROR,*) ' Attempt to set upstream constraint failed...'
      END IF

*-----Compute and store intermediate general-equation coeficients for ...

      Node = 0

      if( .NOT. VariableStreamSinuosity() ) then

*--------Constant sinuosity and water density.

         do 100 I=UpstreamPointer(),DownstreamPointer()-1
            Node = Node + 1
            if( .not. DynamicWaveEq(Node, Node+1) ) then
               UpdateChannel = .FALSE.
               write(UNIT_ERROR,*) ' *** Error (UpdateChannel)'
               write(UNIT_ERROR,*) ' Failed to update node...',Node
            end if
 100     continue

      else

*--------Variable sinuosity and water density.

         do 150 I=UpstreamPointer(),DownstreamPointer()-1
            Node = Node + 1
            if( DynamicWaveEqDS(Node, Node+1) )then
            else
               UpdateChannel = .FALSE.
               WRITE(UNIT_ERROR,*) ' *** Error (UpdateChannel)'
               WRITE(UNIT_ERROR,*) ' Failed to update node...',Node
            end if
 150     continue

      end if

*-----Compute and store constraint-equation coefficients for
*      downstream end of channel.

      IF( SetConstraint(-1) ) THEN
      ELSE
         UpdateChannel = .FALSE.
         WRITE(UNIT_ERROR,*) ' *** Error (UpdateChannel)'
         WRITE(UNIT_ERROR,*) ' Attempt to set downstream constraint failed...'
      END IF

      RETURN
      END

*== Public (SetConstraint) =====================================

      LOGICAL FUNCTION SetConstraint(Extremity)
      use IO_Units
      IMPLICIT NONE

*   Purpose:  Select and compute appropriate channel
*             constraint-equation
*             for one end of channel
*   Arguments:

*   Argument definitions:

*   Local Variables:
      INTEGER Code, Extremity, ConstraintCode
      Logical OK

*   Routines by module:

***** Channel constraint:
      LOGICAL   ForceStreamSurface, ForceStreamFlow
      EXTERNAL  ForceStreamSurface, ForceStreamFlow

      LOGICAL   ForceSumStreamFlow, ForceEqualStreamSurface
      EXTERNAL  ForceSumStreamFlow, ForceEqualStreamSurface

***** Channel schematic:
      INTEGER   UpstreamCode,DownStreamCode
      EXTERNAL  UpstreamCode,DownStreamCode

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:   Lew DeLong
*   Last modified: December 1992
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      SetConstraint = .FALSE.

*-----Boundary-condition code key
*     
*     Last Digit = 1 : Upstream
*                  2 : Downstream
*     
*     
*       water surface  * =implemented in DSM2
*          1,2*               explicitly known
*          4                  self-setting (downstream only)
*         11,12*              equal to another water surface
* ........31,32               3-parameter relation
* ........41,42               (reserved for four-parameter rating)
* ........51,52*              user-defined (e.g. gate)
*     
*         Note: User-defined codes can have four digits, last
*         two of which will be 51 or 52
*****-
*-----Set channel-extremity code to upstream.

      If(Extremity .EQ. 1)Then
         ConstraintCode = UpstreamCode()
      Else If(Extremity .EQ. -1)Then
         ConstraintCode = DownstreamCode()
      Else
         WRITE(UNIT_ERROR,*) ' *** Error (SetConstraint)'
         WRITE(UNIT_ERROR,*) ' Inappropriate extremity code (not +1/-1).'
         RETURN
      End If

*-----Get upstream boundary-condition code.
      Code = MOD(ConstraintCode, 100)
      OK = .FALSE.

*-----Set appropriate constraint.
      If (Code.EQ.1)Then
         IF( ForceStreamSurface(Extremity) ) THEN
         ELSE
            RETURN
         END IF
      Else If (Code.EQ.2)Then
         IF( ForceStreamFlow(Extremity) ) THEN
         ELSE
            RETURN
         END IF

      Else If (Code.EQ.11)Then
         IF( ForceEqualStreamSurface(Extremity) ) THEN
         ELSE
            RETURN
         END IF

      Else If (Code.EQ.12)Then
         IF( ForceSumStreamFlow(Extremity) ) THEN
         ELSE
            RETURN
         END IF

      Else If (Code.EQ.51 .OR.  Code.EQ.52)Then
         IF(IABS( MOD( ConstraintCode/100 , 100 ) ) .EQ.4 ) THEN
         ELSE
            RETURN
         END IF
      Else
         WRITE(UNIT_ERROR,*) ' *** Error (SetConstraint)'
         WRITE(UNIT_ERROR,*) ' Condition code ',Code,' not implemented.'
         RETURN

      End If

      SetConstraint = .True.

      RETURN
      END

*== Public (NetworkClosure) ============================================

      LOGICAL FUNCTION NetworkClosure()
      Use Gates, only: NGate, GateArray
      use grid_data
      IMPLICIT NONE

*   Purpose:  Check for closure of iteration on a set of equations
*             describing flow in a network of open channels, and
*             update dependent variables.

*   Arguments:

*   Argument definitions:

*   Module data:

*   Local Variables:
      INTEGER I, J
      INTEGER ChannelQ, NodeQ, ChannelZ, NodeZ
      INTEGER ChannelNumber, Node
      REAL*8    SmallQ
      PARAMETER (SmallQ = 1000.0)
      REAL*8    Change, CurrentValue, CurrentRatioQ, CurrentChangeZ
      REAL*8    MaxChangeZ, MaxRatioQ
      LOGICAL OK

*   Routines by module:
      INCLUDE '../hydrolib/network.inc'
      INCLUDE '../hydrolib/netcntrl.inc'
      INCLUDE '../hydrolib/chconnec.inc'
      INCLUDE '../hydrolib/solver.inc'

***** Channel flow status:
      LOGICAL  SetStreamSurfaceElevation, SetStreamFlow
      EXTERNAL SetStreamSurfaceElevation, SetStreamFlow

      REAL*8     StreamFlow, StreamSurfaceElevation
      EXTERNAL StreamFlow, StreamSurfaceElevation

      LOGICAL  StoreNetworkClosure
      EXTERNAL StoreNetworkClosure

***** Channel schematic:
      INTEGER  NumberOfStreamLocations
      EXTERNAL NumberOfStreamLocations

      INTEGER  NumberOfChannels, MaxNetworkIterations
      EXTERNAL NumberOfChannels, MaxNetworkIterations

      LOGICAL  OpenChannel, CloseChannel
      EXTERNAL OpenChannel, CloseChannel

***** Solver:
      REAL*8     IncrementalZ, IncrementalQ,IncrementalZRes
      EXTERNAL IncrementalZ, IncrementalQ,IncrementalZRes

***** Network control:
      LOGICAL  IntermediateNetworkResults
      EXTERNAL IntermediateNetworkResults

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:   Lew DeLong
*   Last modified: December 1992
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      MaxRatioQ  = 0.0
      MaxChangeZ = 0.0
      MaxChangeQ = 0.0

      DO 200 I=1,NumberOfChannels()

         ChannelNumber = I

         IF( OpenChannel(ChannelNumber) ) Then

            DO 100 J = 1,NumberOfStreamLocations()
               Node = J

*--------------Check for closure on flow.

               Change = IncrementalQ(Node)
               CurrentValue = StreamFlow(Node)

               IF( ABS(CurrentValue) .GT. SmallQ  ) THEN
*-----------------Flow is greater than a small flow.
                  CurrentRatioQ = ABS(Change/CurrentValue)
               ELSE
*-----------------Flow is smaller than a small flow.
                  CurrentRatioQ = ABS(Change/SmallQ)
               END IF

               IF(ABS(Change).GT. MaxChangeQ) THEN
                  MaxChangeQ=ABS(Change)
                  MaxChangeChannelQ=ChannelNumber
               ENDIF
               IF( CurrentRatioQ .GT. MaxRatioQ ) THEN
                  MaxRatioQ = CurrentRatioQ
                  NodeQ = Node
                  ChannelQ = ChannelNumber
               END IF

*--------------Update flow.
               OK = SetStreamFlow( Node, CurrentValue + Change )

*--------------Check for closure on water-surface elevation.

               Change = IncrementalZ(Node)
               CurrentChangeZ = ABS(Change)
               CurrentValue = StreamSurfaceElevation(Node)

               IF( CurrentChangeZ .GT. MaxChangeZ ) THEN
                  MaxChangeZ = CurrentChangeZ
                  NodeZ = Node
                  ChannelZ = ChannelNumber
               END IF

*--------------Update water-surface elevation.
               OK = SetStreamSurfaceElevation(Node, CurrentValue + Change)

 100        CONTINUE

            OK = CloseChannel()

         ELSE
         END IF

 200  CONTINUE

      do i = 1,Nreser
*--------Adjust reservoir height from iteration
*     channel Z is already adjusted
         Change=X(ResEqRow(i))
         Yres(i) = Yres(i) + Change
         do j=1,res_geom(i).nnodes
            QRes(i,j)=QRes(i,j) + X(ResEqRow(i)+j)
         end do
      end Do

*-----Adjust gate device flows from iteration
      do i=1,NGate
         do j=1,GateArray(i).nDevice
            Change=X(GateArray(i).devices(j).calcRow)
            GateArray(i).devices(j).flow=GateArray(i).devices(j).flow
     &           +Change        ! calcRow =device column in X
         end do
      end do

*-----Check for overall closure and store closure information.
      MaxRatioQ = MaxRatioQ/rescale
      MaxChangeZ = MaxChangeZ/rescale

      IF( StoreNetworkClosure(
     &     MaxRatioQ, ChannelQ, NodeQ,
     &     MaxChangeZ, ChannelZ, NodeZ
     &     )
     &     ) THEN
         NetworkClosure = .TRUE.
      ELSE
         NetworkClosure = .FALSE.
      END IF

*-----Write intermediate results.

      OK = IntermediateNetworkResults()

      RETURN
      END

*==== EOF fourpt =======================================================
