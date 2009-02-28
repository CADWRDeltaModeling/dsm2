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

*==== BOF netcntr1 =====================================================




*== Public (NetworkTimeSteps) ==========================================

      INTEGER FUNCTION NetworkTimeSteps()

      IMPLICIT NONE

*   Purpose:  Return requested number of time steps for network.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netcntrl.inc'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      NetworkTimeSteps = MaxTimeSteps

      RETURN
      END


*== Public (MaxNetworkIterations) ======================================

      INTEGER FUNCTION MaxNetworkIterations()

      IMPLICIT NONE

*   Purpose: Return the maximum number of iterations allowed for each
*            time step executed by an open-channel network model.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netcntrl.inc'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      MaxNetworkIterations = MaxIterations

      RETURN
      END

*== Public (NetworkTimeIncrement) ======================================

      INTEGER FUNCTION NetworkTimeIncrement()

      IMPLICIT NONE

*   Purpose: Return the time increment used by an
*            open-channel network model.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netcntrl.inc'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      NetworkTimeIncrement = DT

      RETURN
      END

*== Public (NetworkTheta) ==============================================

      REAL*8 FUNCTION NetworkTheta()

      IMPLICIT NONE

*   Purpose:  Return the time-weighting factor used by an
*             open-channel network model.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netcntrl.inc'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      NetworkTheta = Theta

      RETURN
      END


*== Public (ToleranceStreamZ) ==========================================

      REAL*8 FUNCTION ToleranceStreamZ()

      IMPLICIT NONE

*   Purpose:  Return the closure tolerance for water-surface elevation.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netcntrl.inc'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      ToleranceStreamZ = ToleranceZ

      RETURN
      END

*== Public (ToleranceStreamQ) ==========================================

      REAL*8 FUNCTION ToleranceStreamQ()

      IMPLICIT NONE

*   Purpose:  Return the closure tolerance for stream flow.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netcntrl.inc'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      ToleranceStreamQ = ToleranceQ

      RETURN
      END

*== Public (NetworkPrintLevel) =========================================

      INTEGER FUNCTION NetworkPrintLevel()

      IMPLICIT NONE

*   Purpose:  Return level of printing activity.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netcntrl.inc'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      NetworkPrintLevel = PrintLevel

      RETURN
      END

*== Public (NetworkQuadPts) =========================================

      INTEGER FUNCTION NetworkQuadPts()

      IMPLICIT NONE

*   Purpose:  Return current number of quadrature points to be used
*             in numerical integration.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netcntrl.inc'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:   Lew DeLong
*   Last modified: May      1992
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      NetworkQuadPts = QuadPts

      RETURN
      END

*== Public (NetworkQuadPts) ============================================

      SUBROUTINE NetworkQuadPtWt(
     &     K,
     &     Pt, Wt
     &     )

      IMPLICIT NONE

*   Purpose:  Return location, in local coordinate ( 0 to 1 ), and weight
*             of quadrature-point K.

*   Arguments:
      INTEGER K
      REAL*8    Pt, Wt

*   Argument definitions:
*     K - quadrature-point sequence number.
*     Pt - location of quadrature point K,
*              in local coordinate ( 0 to 1 ).
*     Wt - weight, corresponding to quadrature point "K",
*              for numerical integration.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netcntrl.inc'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:   Lew DeLong
*   Last modified: May      1992
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      Pt = QuadPt(K)
      Wt = QuadWt(K)

      RETURN
      END

*== Public (NetworkTimeStep) ===========================================

      INTEGER FUNCTION NetworkTimeStep()

      IMPLICIT NONE

*   Purpose:  Return current time step number.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netcntrl.inc'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      NetworkTimeStep = TimeStep

      RETURN
      END

*== Public (NetworkSeconds) ============================================

      INTEGER FUNCTION NetworkSeconds()

      IMPLICIT NONE

*   Purpose:  Return current network elapse time in seconds.
*             Seconds = 0 at begining of current execution.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netcntrl.inc'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      NetworkSeconds = Seconds

      RETURN
      END

*== Public (IncrementNetworkTimeStep) ==================================

      LOGICAL FUNCTION IncrementNetworkTimeStep()

      IMPLICIT NONE

*   Purpose:  Increment current time step number.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netcntrl.inc'

*   Local Variables:

*   Routines by module:

*   Intrinsics:

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      TimeStep = TimeStep + 1

      Seconds = Seconds + DT

      IF( TimeStep .LE. MaxTimeSteps ) THEN
         IncrementNetworkTimeStep = .TRUE.
      ELSE
         IncrementNetworkTimeStep = .FALSE.
      END IF

      RETURN
      END

*== Public (IncrementNetworkIteration) =================================

      LOGICAL FUNCTION IncrementNetworkIteration()

      IMPLICIT NONE

*   Purpose:  Increment current iteration number.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netcntrl.inc'

*   Local Variables:
      LOGICAL OK

*   Routines by module:

***** Solver:
      LOGICAL  InitializeMatrix
      EXTERNAL InitializeMatrix

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      Iteration = Iteration + 1

      if( Iteration .LE. MaxIterations ) then
         OK = InitializeMatrix()
         IncrementNetworkIteration = .TRUE.
      else
         IncrementNetworkIteration = .FALSE.
      end if

      return
      end

*== Public (StoreNetworkClosure) =======================================

      LOGICAL FUNCTION StoreNetworkClosure(
     &     MaxRatioQ, ChannelQ, NodeQ,
     &     MaxChangeZ, ChannelZ, NodeZ
     &     )

      IMPLICIT NONE

*   Purpose:  Store current network closure information, and check if
*              current information satisfies criteria.

*   Arguments:
      INTEGER ChannelQ, ChannelZ, NodeQ, NodeZ
      REAL*8    MaxRatioQ, MaxChangeZ

*   Argument definitions:
*     MaxRatioQ - maximum ratio of change in flow over the last
*                  iteration to the current value of flow.
*     ChannelQ - channel number in which MaxRatioQ occurred.
*     NodeQ - local location number at which MaxRatioQ occurred.

*     MaxChangeZ - maximum change in water-surface elevation over
*                   the last iteration.
*     ChannelZ - channel number in which MaxChangeZ ocurred.
*     NodeZ - local location number at which MaxChangeZ occurred.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netcntrl.inc'
      include 'solver.inc' 
*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

*---- Store current values.
      RatioQ         = MaxRatioQ
      ChannelRatioQ  = ChannelQ
      NodeRatioQ     = NodeQ

      ChangeZ        = MaxChangeZ
      ChannelChangeZ = ChannelZ
      NodeChangeZ    = NodeZ

*---- Test for overall closure. 
      StoreNetworkClosure = .FALSE.

      IF( ChangeZ .GT. ToleranceZ ) THEN
         StoreNetworkClosure = .FALSE.
         CloseZ = .FALSE.
      ELSE
         CloseZ = .TRUE.
      END IF

      IF( RatioQ .GT. ToleranceQ ) THEN
         StoreNetworkClosure = .FALSE.
         CloseQ = .FALSE.
      ELSE
         CloseQ = .TRUE.
      END IF

      IF(NormClose)then ! CloseZ .AND. CloseQ ) THEN
         StoreNetworkClosure = .TRUE.
      ELSE
*--------Check for exceedence of maximum number of iterations.
         IF( Iteration .GE. MaxIterations ) THEN
            StoreNetworkClosure = .TRUE.
         END IF
      END IF

      EndIteration = StoreNetworkClosure
      RETURN
      END


*== Public (CloseNetworkIteration) =====================================

      LOGICAL FUNCTION CloseNetworkIteration()

      IMPLICIT NONE

*   Purpose:  Close out the current iteration.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netcntrl.inc'

*   Routines by module:

***** Buffered output:
      LOGICAL  WriteNetworkSpatialSeries
      EXTERNAL WriteNetworkSpatialSeries

***** Network control:
      LOGICAL  ReportNetworkIterationStatus
      EXTERNAL ReportNetworkIterationStatus

***** Network model:
      LOGICAL  ReportNetworkStatus
      EXTERNAL ReportNetworkStatus

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

*---- Increment output counters.

      PrintCount = PrintCount + 1

*---- Reset print counter.

      IF( PrintCount .GE. PrintInc )  THEN
         PrintCount = 0
      END IF

*---- Sum total iterations.

      TotalIterations = TotalIterations + Iteration

      CloseNetworkIteration = .TRUE.

      RETURN
      END

*== Public (TotalNetworkIterations) ====================================

      INTEGER FUNCTION TotalNetworkIterations()

      IMPLICIT NONE

*   Purpose:  Return level of printing activity.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netcntrl.inc'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      TotalNetworkIterations = TotalIterations

      RETURN
      END

*== Private (ReportNetworkIterationStatus) =============================

      LOGICAL FUNCTION ReportNetworkIterationStatus()
      use IO_Units
      IMPLICIT NONE

*   Purpose:  Write network iteration information to print file.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netcntrl.inc'


*   Local Variables:
      INTEGER PrintUnit
      CHARACTER*1 Star, Blank
      PARAMETER ( Star = '*', Blank = ' ' )

*   Routines by module:

      INTEGER  NetworkTimeStep
      EXTERNAL NetworkTimeStep

*   Programmed by: Lew DeLong
*   Date:          March 1991
*   Modified by:  Barry Wicktom (use of master file names added)
*   Last modified: 1/7/92
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      PrintUnit = unit_output

*   Output formats:

 2000 FORMAT(2I5,2I8,A1,4X,2I8,A1)

      TimeStep= NetworkTimeStep()
      IF( CloseQ ) THEN
         IF( CloseZ ) THEN

            WRITE(PrintUnit,2000) TimeStep, Iteration,
     &           ChannelRatioQ, NodeRatioQ, Blank,
     &           ChannelChangeZ, NodeChangeZ, Blank

         ELSE

            WRITE(PrintUnit,2000) TimeStep, Iteration,
     &           ChannelRatioQ, NodeRatioQ, Blank,
     &           ChannelChangeZ, NodeChangeZ, Star

         END IF

      ELSE IF( CloseZ ) THEN

         WRITE(PrintUnit,2000) TimeStep, Iteration,
     &        ChannelRatioQ, NodeRatioQ, Star,
     &        ChannelChangeZ, NodeChangeZ, Blank

      ELSE

         WRITE(PrintUnit,2000) TimeStep, Iteration,
     &        ChannelRatioQ, NodeRatioQ, Star,
     &        ChannelChangeZ, NodeChangeZ, Star

      END IF

      ReportNetworkIterationStatus = .TRUE.

      RETURN
      END

*== Private (ScreenNetworkStatus) ======================================

      LOGICAL FUNCTION ScreenNetworkStatus()
      use IO_Units
      use grid_data
      IMPLICIT NONE

*   Purpose:  Write network iteration/timestep information to screen.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netcntrl.inc'


*   Local Variables:
      CHARACTER*1 Star, Blank
      PARAMETER ( Star = '*', Blank = ' ' )

*   Routines by module:
      INTEGER  NetworkTimeStep
      EXTERNAL NetworkTimeStep

*   Programmed by: Lew DeLong
*   Date:          March 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

*   Output formats:

 2000 FORMAT(2I5,2I8,A1,4X,2I8,A1,3(1x,f12.6),1x,i4)

      TimeStep= NetworkTimeStep()
      IF( CloseQ ) THEN
         IF( CloseZ ) THEN

            WRITE(unit_screen,2000) TimeStep, Iteration,
     &           chan_geom(ChannelRatioQ).chan_no, NodeRatioQ, Blank,
     &           chan_geom(ChannelChangeZ).chan_no, NodeChangeZ, Blank,RatioQ,
     &           ChangeZ,MaxChangeQ,chan_geom(MaxChangeChannelQ).chan_no

         ELSE

            WRITE(unit_screen,2000) TimeStep, Iteration,
     &           chan_geom(ChannelRatioQ).chan_no, NodeRatioQ, Blank,
     &           chan_geom(ChannelChangeZ).chan_no, NodeChangeZ, Star,RatioQ,
     &           ChangeZ,MaxChangeQ,chan_geom(MaxChangeChannelQ).chan_no

         END IF

      ELSE IF( CloseZ ) THEN

         WRITE(*,2000) TimeStep, Iteration,
     &        chan_geom(ChannelRatioQ).chan_no, NodeRatioQ, Star,
     &        chan_geom(ChannelChangeZ).chan_no, NodeChangeZ, Blank,RatioQ,
     &        ChangeZ,MaxChangeQ,chan_geom(MaxChangeChannelQ).chan_no

      ELSE

         WRITE(*,2000) TimeStep, Iteration,
     &        chan_geom(ChannelRatioQ).chan_no, NodeRatioQ, Star,
     &        chan_geom(ChannelChangeZ).chan_no, NodeChangeZ, Star,RatioQ,
     &        ChangeZ,MaxChangeQ,chan_geom(MaxChangeChannelQ).chan_no

      END IF

      ScreenNetworkStatus = .TRUE.

      RETURN
      END

*== Public (IntermediateNetworkResults) ================================

      LOGICAL FUNCTION IntermediateNetworkResults()
      use IO_Units
      use runtime_data
      use grid_data
      IMPLICIT NONE

*   Purpose:  Report values of variables, current iteration.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netcntrl.inc'

 

*   Local Variables:
      INTEGER PrintUnit
      LOGICAL OK

*   Routines by module:

      INTEGER  NetworkTimeStep
      EXTERNAL NetworkTimeStep

***** Network control:
      INTEGER  NetworkPrintLevel
      LOGICAL  ReportNetworkIterationStatus
      EXTERNAL ReportNetworkIterationStatus, NetworkPrintLevel
      INTEGER  NetworkIteration, MaxNetworkIterations
      EXTERNAL NetworkIteration, MaxNetworkIterations

      LOGICAL  ScreenNetworkStatus
      EXTERNAL ScreenNetworkStatus

***** Network model:
      LOGICAL  ReportNetworkStatus
      EXTERNAL ReportNetworkStatus

*   Intrinsics:
      INTEGER MOD
      INTRINSIC MOD

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:   Barry Wicktom (use of master file names added)
*   Last modified: 1/7/92
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      PrintUnit = unit_output

*   Output formats:
 2001 FORMAT(////'  Time          ....Q....            ....Z....'/
     &     '  Step Iter. Channel  Section     Channel  Section')


*---- Write network status to print file if requested.

      IF( PrintLevel .EQ. 37 ) THEN
         IF( Iteration  .EQ. 1 ) WRITE(PrintUnit,2001) ! write header
         OK = ReportNetworkIterationStatus()
         OK = ReportNetworkStatus()

      END IF

*---- Write iteration-status header to screen.

      TimeStep= NetworkTimeStep()

      IF( NetworkPrintLevel() .GE. 4) THEN
         IF(
     &        EndIteration
     &        .AND.
     &        TimeStep .EQ. 1
     &        )      THEN

            WRITE(*,2001)

         END IF
      END IF

*---- Write iteration status to screen.
      IF( EndIteration ) THEN
         IF( NetworkPrintLevel() .GE. 4 ) THEN
            OK = ScreenNetworkStatus()
         END IF
         IF (NetworkIteration() .GE. MaxNetworkIterations()) THEN
            if (NetworkPrintLevel() .ge. 1) then
                write(unit_screen,610) current_date,NetworkIteration()
                write(unit_output,610) current_date,NetworkIteration()
 610        format('Note: at ', a, 
     &        ' network iterations at maximum (',i3,').')
	       endif
         END IF
      END IF

      IntermediateNetworkResults = .TRUE.

      RETURN
      END

*== Public (NetworkIteration) ==========================================

      INTEGER FUNCTION NetworkIteration()

      IMPLICIT NONE

*   Purpose: Return current iteration number.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netcntrl.inc'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      NetworkIteration = Iteration

      RETURN
      END

*== Public (ForwardElimInt) ==========================================

      INTEGER FUNCTION ForwardElimInt()

      IMPLICIT NONE

*   Purpose: Return interval at which complete forward eliminations
*            are to be performed on the solution matrix, in terms of
*            number of iterations during a time step.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netcntrl.inc'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          April 1992
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      ForwardElimInt = LuInc

      RETURN
      END

*== Public (NetworkRestart) ==========================================

      LOGICAL FUNCTION NetworkRestart()

      IMPLICIT NONE

*   Purpose: Return .TRUE. if network restart file has been requested,
*            otherwise return .FALSE..

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netcntrl.inc'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          December 1992
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      NetworkRestart = Restart_Write

      RETURN
      END


*== Public (VariableStreamDensity) ===================================================

      LOGICAL FUNCTION VariableStreamDensity()

      IMPLICIT NONE

*   Purpose: Return .TRUE. if density is allowed to vary with
*             time and distance, .FALSE. otherwise.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netcntrl.inc'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      VariableStreamDensity = VariableDensity

      RETURN
      END

*== Public (VariableStreamSinuosity) ===================================================

      LOGICAL FUNCTION VariableStreamSinuosity()

      IMPLICIT NONE

*   Purpose: Return .TRUE. if sinuosity is allowed to vary with
*             depth of flow and distance, .FALSE. otherwise.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netcntrl.inc'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      VariableStreamSinuosity = VariableSinuosity

      RETURN
      END

*==  Public (ReportNetworkStatus) ======================================

      LOGICAL FUNCTION ReportNetworkStatus()
      use IO_Units
      use grid_data
      IMPLICIT NONE

*   Purpose:  Report current status of flow in a network of open channels.

*   Arguments:

*   Argument definitions:

*   Module data:


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
            WRITE(PrintUnit,*) '        Channel...',chan_geom(ChannelNumber).chan_no
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
            WRITE(UNIT_ERROR,*) ' Failed to open channel ',chan_geom(ChannelNumber).chan_no
            WRITE(unit_error,*) '***Error (ReportSystemStatus)'
            WRITE(unit_error,*) 'Failed to open channel ',chan_geom(ChannelNumber).chan_no
            CALL EXIT(1)

         END IF

         OK = CloseChannel()

 200  CONTINUE

      ReportNetworkStatus = .TRUE.

      RETURN
      END

