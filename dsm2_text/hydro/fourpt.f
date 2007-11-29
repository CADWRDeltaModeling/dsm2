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


*==== BOF fourpt =====================================================

*== Public  (FourPt) =================================================

      PROGRAM FourPt

c 
      USE DFLIB                 !! <NT> Comment this line for UNIX version of executable

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

*     Non-standard usage: Symbolic names in this module may be represented by
*                         as many as 31 characters in order to provide better
*                         definition directly in the code.  Standard FORTRAN
*                         allows only 6 characters, but this restriction is
*                         generally extended to 32 characters by most compilers.
*

      include 'network.inc'
      include 'solver.inc'
      include 'netcntrl.inc'
      include 'chnlcomp.inc'
      include 'chconnec.inc'
      include 'chstatus.inc'
      include '../input/fixed/common.f'
      include '../input/time-varying/dss.inc'
      include '../input/time-varying/readdss.inc'
      include '../input/time-varying/writedss.inc'

*   Local variables:
      LOGICAL   OK
     &     ,SteadyState         ! true if repeating tide achieved steady state
     &     ,FinalCycle          ! true if tide is final output cycle
     &     ,new_tide            ! true if new tide cycle just started

      integer*4
     &     incr_intvl           ! increment julian minute by interval function
     &     ,next_output_flush   ! next time to flush output
     &     ,next_display        ! next time to display model time
     &     ,next_restart_output ! next time to write restart file

      integer
     &     istat                ! status of fixed input
     &     ,i,j                 ! loop index
     &     ,lnblnk              ! intrinsic last-non-blank

      INTEGER Up, Down
      integer tide_count, old_tide_count

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

***** Local:
      LOGICAL  DefineNetwork, UpdateNetwork,CloseSolver
      EXTERNAL DefineNetwork, UpdateNetwork,CloseSolver

      INTEGER  GateNum

***** Network volume and mass balance:

      LOGICAL  SetUserNetBndValues
      EXTERNAL SetUserNetBndValues

      LOGICAL  AverageFlow, WriteNetworkRestartFile
      EXTERNAL AverageFlow, WriteNetworkRestartFile

      LOGICAL  InitNetBalance, UpdateNetBalance
      EXTERNAL InitNetBalance, UpdateNetBalance
      LOGICAL  ReportNetBalance
      EXTERNAL ReportNetBalance

***** Channel status:

      INTEGER  NumberOfChannels
      EXTERNAL NumberOfChannels

      LOGICAL  WriteHydroFile, Calculate_Chan_Net_Flow
      EXTERNAL WriteHydroFile, Calculate_Chan_Net_Flow

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:   Lew DeLong
*   Last modified: December 1992
*   Version 93.01, January, 1993
*   Last modified: October 1994 Parviz Nader DWR
*   Last modified: September 1996 Ralph Finch DWR

      data SteadyState /.false./, FinalCycle /.false./,
     &     tide_count /0/, init_input_file /' '/

*-----Implementation -------------------------------------------------

c-----DSM2 module, name and version number
      include 'version.inc'

c 
      open(unit_screen, carriagecontrol='list') !! <NT> Comment this line for UNIX version of executable
      open(unit_error, carriagecontrol='list') !! <NT> Comment this line for UNIX version of executable

c-----get optional starting input file from command line,
c-----then from environment variables,
c-----then default
      call getarg(1,init_input_file)
      if (lnblnk(init_input_file) .eq. 0) then ! no command line arg
         call getenv('HYDROINPUT',init_input_file)
         if (init_input_file .eq. ' ') then
            call getenv('DSM2INPUT',init_input_file)
            if (init_input_file .eq. ' ') then
               init_input_file='dsm2.inp'
            endif
         endif
      else                      ! command line arg
         if (init_input_file(:2) .eq. "-v" .or.
     &        init_input_file(:2) .eq. "-V" .or.
     &        init_input_file(:2) .eq. "-h" .or.
     &        init_input_file(:2) .eq. "-H") then ! print version, quit
            print *, 'DSM2-Hydro ',dsm2_version
            call exit(1)
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

      call virtual_xsect        ! create virtual cross-section lookup table

      call check_fixed_hydro(istat)
      if (istat .ne. 0) then
         write(unit_error, *)
     &        'Error in checking fixed fourpt data; run stopped.'
         call exit(1)
      endif

      prev_julmin=0
      julmin=start_julmin
      current_dt=jmin2cdt(julmin)



c-----calculate julian minute of end of each DSS interval
      call update_intervals

      IF( .not. DefineNetwork() ) THEN
         WRITE(UNIT_ERROR,*) ' System definition not complete,'
         WRITE(UNIT_ERROR,*) ' Abnormal program end.'
         CALL EXIT(1)
      ENDIF

      OK = InitNetBalance()
      call init_store_outpaths(istat)

      if (io_files(hydro,io_tide,io_write).use) then ! hydro binary file output
         OK = WriteHydroFile()
      endif

 605  format('Starting run at time: ',a)
      write(unit_output,605) current_dt
      write(unit_screen,605) current_dt
      call store_outpaths(.false.)
      next_display=incr_intvl(start_julmin,display_intvl,TO_BOUNDARY)
      next_output_flush=incr_intvl(start_julmin,flush_intvl,TO_BOUNDARY)
      if (io_files(hydro,io_restart,io_write).use) then
         next_restart_output=incr_intvl(start_julmin,io_files(hydro,
     &        io_restart,io_write).interval,TO_BOUNDARY)
      endif

      prev_julmin=julmin
      julmin=julmin+time_step
      current_dt=jmin2cdt(julmin)
      do while (
     &     (.not. repeating_tide .and. julmin .le. end_julmin) ! normal time run
     &     .or.
     &     (repeating_tide .and. ! repeating tide run, not final cycle
     &     .not. FinalCycle .and. .not. SteadyState)
     &     .or.
     &     (repeating_tide .and. ! repeating tide run, final cycle
     &     FinalCycle .and. julmin .le. end_julmin)
     &     )

         DO I=1,NumberofChannels()
            Up=UpCompPointer(I)
            Down=DownCompPointer(I)
            QOld(Up)=Q(Up)
            QOld(Down)=Q(Down)
         ENDDO

         DO i=1,Nres
            DO j=1,NconnectReservoir(i)
               QResOld(i,j)=QRes(i,j)
            ENDDO
         ENDDO

         OK=IncrementNetworkTimeStep()
c--------calculate julian minute of end of each DSS interval
         call update_intervals

         if (julmin .ge. next_display) then
 610        format('Starting computations for time: ',a)
            write(unit_output,610) current_dt
            write(unit_screen,610) current_dt
            next_display=incr_intvl(next_display,display_intvl,
     &           TO_BOUNDARY)
         endif

         if (check_input_data) then
c-----------just check input data for bogus values; no simulation
            OK = SetUserNetBndValues()
         else                   ! full simulation
            old_tide_count=tide_count

            IF (UpdateNetwork(SteadyState,tide_count)) THEN
               new_tide=old_tide_count .ne. tide_count

               OK = UpdateNetBalance()

            else
               write (unit_error,*)
     &              ' Network update failed at time ',current_dt
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

            if(repeating_tide)then
               OK=Calculate_Chan_Net_Flow()
            endif
            if (io_files(hydro,io_tide,io_write).use) then
               OK=AverageFlow()
            endif

            if (Restart_Write .and. julmin .ge. next_restart_output) then
C--------------Write the hydrodynamic information at the end of
c--------------every interval to ascii file in case of any
c--------------interruptions to the model
               next_restart_output=incr_intvl(next_restart_output,
     &              io_files(hydro,io_restart,io_write).interval,
     &              TO_BOUNDARY)
               OK = WriteNetworkRestartFile()
            endif

            if (.not. restart_write .and.
     &           repeating_tide .and.
     &           (SteadyState .or. tide_count .eq. max_tides)) then ! achieved dynamic steady-state
               next_restart_output=incr_intvl(start_julmin,io_files(hydro,
     &              io_restart,io_write).interval,TO_BOUNDARY)
               Restart_Write=.true.
               io_files(hydro,io_tide,io_write).use=.true.
c               end_julmin=start_julmin+tide_cycle_length_mins
            endif
         endif

         prev_julmin=julmin
         if (repeating_tide .and. new_tide .and. .not. FinalCycle) then
            FinalCycle=SteadyState .or. tide_count .eq. max_tides
            julmin=start_julmin+time_step
            next_display=julmin
            next_output_flush=julmin
            current_dt=jmin2cdt(julmin)
            if (PrintLevel .ge. 1) then
               write(unit_output,611) current_dt,tide_count
               write(unit_screen,611) current_dt,tide_count
 611           format('Recycling time to: ',a,' Tide count: ',i2)
            endif
            if (SteadyState .and. PrintLevel .ge. 1) then
               write(unit_output,612)
               write(unit_screen,612)
 612           format('Dynamic steady state tide achieved.')
            endif
            if (FinalCycle) then

            end_julmin=start_julmin+tide_cycle_length_mins
               OK=WriteHydroFile() ! initialize hydro tidefile
               if (PrintLevel .ge. 1) then
                  write(unit_output,613)
                  write(unit_screen,613)
 613              format('Final tide cycle.')
               endif
            endif
            next_display=incr_intvl(next_display,display_intvl,
     &           TO_BOUNDARY)
            next_output_flush=incr_intvl(next_output_flush,flush_intvl,
     &           TO_BOUNDARY)
            do i=1,NumGatesOperating
               GateNum=ListGateOperating(i)
               GateOperatingTime(GateNum)=start_julmin
            enddo
            DO i=1,NumSpecialGates
               GateNum=ListSpecialGates(i)
               GateOperatingTime(GateNum)=start_julmin
            enddo
         else
            julmin=julmin+time_step
            current_dt=jmin2cdt(julmin)
         endif
      enddo

      if (julmin .gt. end_julmin) then
         julmin=prev_julmin
         prev_julmin=prev_julmin-time_step
         current_dt=jmin2cdt(julmin)
      endif

      if (.not. check_input_data) then
         if (io_files(hydro,io_restart,io_write).use) then
*-----------Write network restart file.
            OK = WriteNetworkRestartFile()
         endif

         if (io_files(hydro,io_tide,io_write).use) then ! close the tidefile
            close(unit=io_files(hydro,io_tide,io_write).unit)
         endif

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

*-----Compute and report final volume and mass balances.
c@@@         OK = ReportNetBalance()

      WRITE(*,*) '   -----------------------------'
      WRITE(*,*) ' '
      WRITE(*,*) ' ',
     &     TotalNetworkIterations(),' total network iterations...'

      WRITE(Unit_Output,*) ' Normal program end.'

      WRITE(Unit_Output,*) ' '
      WRITE(Unit_Output,*)
     &     '   -----------------------------'
      WRITE(Unit_Output,*) ' '
      WRITE(Unit_Output,*)
     &     TotalNetworkIterations(),'  total network iterations...'
      WRITE(Unit_Output,*) ' '
      WRITE(Unit_Output,*)
     &     '   -----------------------------'

      WRITE(*,*) ' '
      WRITE(*,*) '   Normal program end.'
      WRITE(*,*) ' '
      WRITE(*,*) '   -----------------------------'

      close (unit_output)

      OK = CloseSolver()
      CALL EXIT(0)

      END

*== Public (DefineNetwork) =============================================

      LOGICAL FUNCTION DefineNetwork()

      IMPLICIT NONE

*   Purpose:  Define a network of channels in terms of downstream locations
*             within each channel and corresponding initial values of depth
*             of flow and flow.

*   Arguments:

*   Argument definitions:

*   Module data:

      include '../input/fixed/misc.f'

*   Routines by module:

***** Buffered output:
      LOGICAL  InitSpaceTimeSeries
      EXTERNAL InitSpaceTimeSeries

***** Channel schematic:
      LOGICAL  InitializeChannelNetwork, InitGates
      EXTERNAL InitializeChannelNetwork, InitGates

***** Solver:
      LOGICAL  InitializeSolver
      EXTERNAL InitializeSolver

***** Network control:
      INTEGER  NetworkPrintLevel
      EXTERNAL NetworkPrintLevel

***** Network boundary values:
      LOGICAL  InitializeNetworkBoundaryVal
      EXTERNAL InitializeNetworkBoundaryVal

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:   Lew DeLong
*   Last modified: December 1992
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      DefineNetwork = .FALSE.

      IF( InitializeChannelNetwork() ) THEN

         IF ( .NOT. InitializeSolver() ) THEN
            WRITE(UNIT_ERROR,*)
     &           ' Initialization of SPARSE matrix solver failed...'
            CALL EXIT(1)
         END IF

      ELSE

         WRITE(UNIT_ERROR,*) ' Initialization of space- and time-series',
     &        ' buffers failed...'
         RETURN

      END IF

      DefineNetwork = .TRUE.

      RETURN
      END

*== Public (UpdateNetwork) =============================================

      LOGICAL FUNCTION UpdateNetwork(SteadyState,tide_count)

      IMPLICIT NONE

*   Purpose:  Determine values of discharge and water surface elevation
*             within a network of open channels at a new time.

*   Arguments:

*   Argument Definitions:

*   Module data:

      include '../input/fixed/common.f'

      INCLUDE 'network.inc'
      INCLUDE 'netcntrl.inc'
      INCLUDE 'chconnec.inc'
      INCLUDE 'solver.inc'
      INCLUDE 'solver2.inc'

      INCLUDE 'netbnd.inc'
*   Local variables:
      INTEGER ChannelNumber
     &     ,tide_count          ! number of tide cycles simulated
      LOGICAL OK
     &     ,SteadyState         ! true if repeating tide is steady state

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

      LOGICAL  SetUserNetBndValues
      EXTERNAL SetUserNetBndValues

      INTEGER  NetworkIteration
      EXTERNAL NetworkIteration

      LOGICAL  GateSchedule, CalculateReservoirFlow,CalculateGateFlow
      EXTERNAL GateSchedule, CalculateReservoirFlow,CalculateGateFlow

      LOGICAL  AdjustReservoirFlow
      EXTERNAL AdjustReservoirFlow

      LOGICAL  CheckSteadyState
      EXTERNAL CheckSteadyState

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

      if (repeating_tide .and.
     &     mod(julmin, tide_cycle_length_mins) .eq.
     &     mod(start_julmin, tide_cycle_length_mins)) then
         tide_count=tide_count+1
         SteadyState=CheckSteadyState()
      endif

      OK = SetUserNetBndValues()


      IF( OpenNetworkIteration() ) THEN

 100     CONTINUE

*--------Begin iteration loop.
         OK = IncrementNetworkIteration()

         IF(NGate.GT.0)THEN
c-----------Update flow coefficients according to the gate schedules
            OK = GateSchedule()

c-----------Calculate flow through all the gates
            OK = CalculateGateFlow()
         ENDIF

         DO 200 ChannelNumber = 1, NumberOfChannels()

            IF( OpenChannel(ChannelNumber) ) THEN

               IF( UpdateChannel() ) THEN

                  OK = CloseChannel()

               ELSE
                  WRITE(UNIT_ERROR,*) ' Update of channel',
     &                 int2ext(ChannelNumber),' failed...'
                  RETURN
               END IF

            ELSE
               OK = CloseChannel()
            END IF

 200     CONTINUE

         IF(Nres.GT.0)THEN
c-----------Calculate Reservoir flows
            OK = CalculateReservoirFlow()
         ENDIF

*--------Solve for incremental change in dependent variables.

         IF( SolveFourPt() ) THEN
         ELSE
            WRITE(UNIT_ERROR,*) ' *** Error (UpdateNetwork)'
            WRITE(UNIT_ERROR,*) ' Failed to solve matrix...'
            RETURN
         END IF

*--------Update dependent variables and check for closure.

         IF( .Not. NetworkClosure() ) THEN

            GO TO 100

         END IF

*--------End iteration loop.

         IF(Nres.GT.0)THEN
c-----------Adjust Reservoir flows

            OK = AdjustReservoirFlow()
         ENDIF

         OK = CloseNetworkIteration()

      ELSE
      END IF

      UpdateNetwork = .TRUE.

      RETURN
      END

*== Public (UpdateChannel) =============================================

      LOGICAL FUNCTION UpdateChannel()

      IMPLICIT NONE

*   Purpose:  Update dependent variables defining a channel.  In this
*             implementation, dependent variables are flow and water-
*             surface elevation.

*   Arguments:

*   Argument definitions:

*   Module data:

      include '../input/fixed/misc.f'

*   Local variables:
      INTEGER Node, I, Terms

*   Routines by module:

***** Channel schematic:
      INTEGER  UpstreamPointer, DownstreamPointer
      EXTERNAL UpstreamPointer, DownstreamPointer

***** Flow equations:
      LOGICAL  DynamicWaveEq, DiffusionWaveEq, KinematicWaveEq
      LOGICAL  DynamicWaveEqDS, DiffusionWaveEqS
      EXTERNAL DynamicWaveEq, DiffusionWaveEq, KinematicWaveEq
      EXTERNAL DynamicWaveEqDS, DiffusionWaveEqS

***** Network control:
      INTEGER  Terms1D
      LOGICAL  VariableStreamSinuosity
      EXTERNAL Terms1D
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
      Terms = Terms1D()

      IF(Terms .EQ. 1 ) THEN

*--------Dynamic wave equation.

         IF( .NOT. VariableStreamSinuosity() ) THEN

*-----------Constant sinuosity and water density.

            DO 100 I=UpstreamPointer(),DownstreamPointer()-1
               Node = Node + 1

               IF( DynamicWaveEq(Node, Node+1) ) THEN
               ELSE
                  UpdateChannel = .FALSE.
                  WRITE(UNIT_ERROR,*) ' *** Error (UpdateChannel)'
                  WRITE(UNIT_ERROR,*) ' Failed to update node...',Node
               END IF

 100        CONTINUE

         ELSE

*-----------Variable sinuosity and water density.

            DO 150 I=UpstreamPointer(),DownstreamPointer()-1
               Node = Node + 1

               IF( DynamicWaveEqDS(Node, Node+1) ) THEN
               ELSE
                  UpdateChannel = .FALSE.
                  WRITE(UNIT_ERROR,*) ' *** Error (UpdateChannel)'
                  WRITE(UNIT_ERROR,*) ' Failed to update node...',Node
               END IF

 150        CONTINUE

         END IF

      ELSE IF(Terms .EQ. 2) THEN

*--------Diffusion-wave equation.

         IF( .NOT. VariableStreamSinuosity() ) THEN

*-----------Constant sinuosity and water density.

            DO 200 I=UpstreamPointer(),DownstreamPointer()-1
               Node = Node + 1

               IF( DiffusionWaveEq(Node, Node+1) ) THEN
               ELSE
                  UpdateChannel = .FALSE.
                  WRITE(UNIT_ERROR,*) ' *** Error (UpdateChannel)'
                  WRITE(UNIT_ERROR,*) ' Failed to update node...',Node
               END IF

 200        CONTINUE

         ELSE

*-----------Variable sinuosity but constant water density.

            DO 250 I=UpstreamPointer(),DownstreamPointer()-1
               Node = Node + 1

               IF( DiffusionWaveEqS(Node, Node+1) ) THEN
               ELSE
                  UpdateChannel = .FALSE.
                  WRITE(UNIT_ERROR,*) ' *** Error (UpdateChannel)'
                  WRITE(UNIT_ERROR,*) ' Failed to update node...',Node
               END IF

 250        CONTINUE

         END IF

      ELSE IF(Terms .EQ. 3) THEN

*--------Kinematic-wave equation, constant sinuosity and density.

         DO 300 I=UpstreamPointer(),DownstreamPointer()-1
            Node = Node + 1

            IF( KinematicWaveEq(Node, Node+1) ) THEN
            ELSE
               UpdateChannel = .FALSE.
               WRITE(UNIT_ERROR,*) ' *** Error (UpdateChannel)'
               WRITE(UNIT_ERROR,*) ' Failed to update node...',Node
            END IF

 300     CONTINUE

      ELSE
         UpdateChannel = .FALSE.
         WRITE(UNIT_ERROR,*) ' *** Error (UpdateChannel)'
         WRITE(UNIT_ERROR,*) ' Terms = ',Terms,'  not supported...'
      END IF

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

      IMPLICIT NONE
      include '../input/fixed/misc.f'

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
      INCLUDE 'network.inc'
      INCLUDE 'netcntrl.inc'
      INCLUDE 'chconnec.inc'
      INCLUDE 'solver.inc'
      INCLUDE 'solver2.inc'

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

      Do i = 1,Nres
*--------Adjust reservoir height from iteration
*     channel Z is already adjusted
         Change = IncrementalZRes(i)
         Yres(i) = Yres(i) + Change
      End Do

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

*==  Public (ReportNetworkStatus) ======================================

      LOGICAL FUNCTION ReportNetworkStatus()

      IMPLICIT NONE

*   Purpose:  Report current status of flow in a network of open channels.

*   Arguments:

*   Argument definitions:

*   Module data:

      include '../input/fixed/common.f'

*   Local Variables:
      INTEGER PrintUnit
      INTEGER I, J, Node, ChannelNumber, Channels
      LOGICAL OK

*   Routines by module:

***** Channel flow status:
      REAL*8     StreamFlow, StreamDepth, StreamDistance
      EXTERNAL StreamFlow, StreamDepth, StreamDistance

      REAL*8     StreamSurfaceElevation
      EXTERNAL StreamSurfaceElevation

***** Channel schematic:
      INTEGER  NumberOfChannels, CurrentChannel, NumberOfStreamLocations
      EXTERNAL NumberOfChannels, CurrentChannel, NumberOfStreamLocations

      LOGICAL  OpenChannel, CloseChannel
      EXTERNAL OpenChannel, CloseChannel

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:   Barry Wicktom (use of master file names added)
*   Last modified: Tues  01-07-1992
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      PrintUnit = unit_output

      ReportNetworkStatus = .FALSE.

      Channels = NumberOfChannels()

      DO 200 I=1,Channels

         ChannelNumber = I

         IF( Channels .GT. 1 ) THEN
            WRITE(PrintUnit,*) ' '
            WRITE(PrintUnit,*) '        Channel...',int2ext(ChannelNumber)
         END IF

         IF( OpenChannel( ChannelNumber ) ) THEN

            WRITE(PrintUnit,*) ' '
            WRITE(PrintUnit,*)
     &           'CompCxNo  Location   Discharge   WS_Elev     Depth'

            DO 100 J=1,NumberOfStreamLocations()
               Node = J

               WRITE(PrintUnit,'(I4,2X,4F11.2)')
     &              Node, StreamDistance(Node),
     &              StreamFlow(Node), StreamSurfaceElevation(Node),
     &              StreamDepth(Node)

 100        CONTINUE

         ELSE

            WRITE(UNIT_ERROR,*) ' ***Error (ReportSystemStatus)'
            WRITE(UNIT_ERROR,*) ' Failed to open channel ',int2ext(ChannelNumber)
            WRITE(unit_error,*) '***Error (ReportSystemStatus)'
            WRITE(unit_error,*) 'Failed to open channel ',int2ext(ChannelNumber)
            CALL EXIT(1)

         END IF

         OK = CloseChannel()

 200  CONTINUE

      ReportNetworkStatus = .TRUE.

      RETURN
      END

*==== EOF fourpt =======================================================
