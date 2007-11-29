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


*==== BOF netcntr1 =====================================================

*   Module data:
*
*    'network.inc'
*     MaxChannels - maximum number of channels.
*     NumCh - current number of channels.
*     Branch - current selected or active channel.
*     MaxLocations - maximum number of locations (computational or user).
*
*
*
*    'netcntrl.inc'
*     MaxQuadPts - maximum number of quadrature points allowed
*                  for numerical spatial integration between
*                  adjacent computational cross sections.
*     MaxBndEq - maximum number of equation-type boundaries.
*     MaxTS - Maximum number of locations at which timeseries
*             output may be accumulated.
*     Terms - index indicating form of 1-D flow equations.
*              [1] full dynamic-wave equation.
*              [2] diffusion-wave equation.
*              [3] kinematic-wave equation.
*
*     VariableSinuosity - .TRUE. if allowed to vary, .FALSE. otherwise.
*     VariableDensity - .TRUE. if allowed to vary, .FALSE. otherwise.
*     Perturbation - .TRUE. if perturbation of input is requested,
*                    otherwise .FALSE. .
*
*     Gravity - acceleration due to gravity.
*
*     RandSeed - seed for random-number generator.
*
*     MaxTimeSteps - maximum number of time steps allowed.
*     TimeStep - current time step.
*     MaxIterations - Maximum number of iterations per time step.
*     Iteration - current iteration number.
*     TotalIterations - total iterations.
*     LuInc - interval of complete forward eliminations, in terms of
*             number of iterations during a time step.
*
*     PrintLevel - level of printing activity, 0 to 9, activity
*                   increasing with increasing number.
*     PrintCount - counter for printed output.
*     PrintInc - increment, in time steps, for printed output.
*     TimeSeriesCount - counter for time-series output.
*     TimeSeriesInc - increment, in time steps, for time-series output.
*     SpatialSeriesCount - counter for spatial-series output.
*     SpatialSeriesInc - increment for spatial-series output.
*
*     TimeSeriesLocation(i) - global computational location sequence number
*                              for the "i"th time series.
*     TSChannel(i) - channel number coreesponding to TimeSeriesLocation(i).
*     TSX(i) - downstream distance corresponding to TimeSeriesLocation(i).
*
*     TimeSeries - total number of time series requested.
*
*
*
*                 Equation-type boundary values ...
*
*       value = EqBase + for each component [
*
*         Amplitude * cos( TwoPI* (Hours+PhaseAngle) / Period )
*
*                                           ]
*
*
*
*             ( time unit for following constants is hours... )
*
*     BoundaryEquations - number of user-supplied boundary equations.
*     EqNumber(k+MaxChannels+1) - equation number for channel k
*      (upstream end of channel k>0, downstream end of channel k<0).
*     EqChannelNumber(j) - channel number, + upsteam, - downstream, equation j.
*     EqComponents(j) - number of harmonic components in equation j.
*     MaxEqComponents - maximum harmonic components per equation.
*     EqBase(j) - base value, equation j.
*     Amplitude1(i,j) - amplitude of component i, equation j.
*     Period(i,j) - period, in hours, of component i, equation j.
*     PhaseAngle(i,j) - phase angle, in hours, of component i, equation j.
*     EqStart(j) - elapse time, in hours, at which equation becomes effective.
*     EqEnd(j) - elapse time, in hours, time at which equation is no longer effective.
*     j - sequential storage index.
*     StreamBndValue(k) - current value computed from equation, possibly
*                         one upstream and one downstream value,
*                         sequentially, odd "k" are upstream,
*                         even are downstream.
*
*
*
*          Standard deviations of normal distributions of errors, used to
*          perturb boundary-equation values, follow. Perturbation will be active
*          when the file "perturb.dat" exists and may be opened and read
*          successfully.  Standard deviations are initialized to zero.
*          Non-zero values are read from file "perturb.dat" if it exists.
*
*
*          group 1: applied each time step and held constant over a time step.
*
*     EqBaseStdDev(j) - standard deviation of a random normal distribution
*                          of errors in base value for equation j.
*     AmplitudeStdDev(j,i) - standard deviation of a random normal distribution
*                          of errors in amplitude for component i, equation j.
*     PeriodStdDev(j,i) - standard deviation of a random normal distribution
*                          of errors in period for component i, equation j.
*     PhaseAngleStdDev(j,i) - standard deviation of a random normal distribution
*                          of errors in phase angle for component i, equation j.
*
*          group 2: applied only once, at the beginning of a model run,
*                   and held constant through out the entire run.
*
*     EqBaseBias(j,i) - standard deviation of a random normal distribution
*                          of errors in base value for equation j.
*     AmplitudeBias(j,i) - standard deviation of a random normal distribution
*                          of errors in amplitude for component i, equation j.
*     PeriodBias(j,i) - standard deviation of a random normal distribution
*                          of errors in period for component i, equation j.
*     PhaseAngleBias(j,i) - standard deviation of a random normal distribution
*                          of errors in phase angle for component i, equation j.
*
*
*
*     DT - time increment, in seconds.
*     Theta - time-weighting factor.
*     ToleranceQ - closure tolerance for discharge.
*     ToleranceZ - closure tolerance for water-surface elevation.
*     RatioQ - current maximum ratio of change in Q to prior Q (flow).
*     ChangeZ - current maximum change in Z (water-surface elevation).
*     ChannelRatioQ - channel in which RatioQ occurs.
*     NodeRatioQ - location within ChannelRatioQ that RatioQ occurs.
*     ChannelChangeZ - channel in which ChangeZ occurs.
*     NodeChangeZ - location within ChannelChangeZ that ChangeZ occurs.
*
*     CloseQ - .TRUE. if closure criteria for discharge has been
*                determined to currently be satisfied, .FALSE. otherwise.
*     CloseZ - .TRUE. if closure criteria for ws_elev has been determined
*                to currently be satisfied, .FALSE. otherwise.
*     EndIteration - .TRUE. if it has determined that iterations are to
*                      stop, .FALSE. otherwise.
*
*
*     MaxQuadPts - Maximum number of quadrature points.
*     QuadPts - current number of quadrature points.
*     QuadPt(i) - location of quadrature point "i",
*                  in local coordinate ( 0 to 1 ).
*     QuadWt(i) - weight, corresponding to quadrature point "i",
*                  for numerical integration.
*
*     Seconds - elapse time, in seconds


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

*== Public (Terms1D) ===================================================

      INTEGER FUNCTION Terms1D()

      IMPLICIT NONE

*   Purpose: Return an index indicating which terms are included in the
*            unsteady 1-D open-channel flow equations.

*              [1] full dynamic-wave equation.
*              [2] diffusion-wave equation.
*              [3] kinematic-wave equation.

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

      Terms1D = Terms

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

*== Public (AccelerationGravity) ==============================================

      REAL*8 FUNCTION AccelerationGravity()

      IMPLICIT NONE

*   Purpose:  Return the value of the acceleration due to gravity.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netcntrl.inc'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          May   1992
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      AccelerationGravity = Gravity

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

      IF( Iteration .LE. MaxIterations ) THEN

         OK = InitializeMatrix()

         IncrementNetworkIteration = .TRUE.
      ELSE
         IncrementNetworkIteration = .FALSE.
      END IF

      RETURN
      END

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

      IF( CloseZ .AND. CloseQ ) THEN
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

*== Public (OpenNetworkIteration) ======================================

      LOGICAL FUNCTION OpenNetworkIteration()

      IMPLICIT NONE

*   Purpose:  Initialize for an iteration.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netcntrl.inc'

*   Local Variables:
      LOGICAL OK

*   Routines by module:

***** Solver:
      LOGICAL  InitializeRightSide
      EXTERNAL InitializeRightSide

*   Programmed by: Lew DeLong
*   Date:          January 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

*---- Set iteration counter to zero.

      Iteration = 0

*---- Set all rows of right-hand side load vector to zero.

      OK = InitializeRightSide()

      OpenNetworkIteration = .TRUE.

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

      IMPLICIT NONE

*   Purpose:  Write network iteration information to print file.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netcntrl.inc'

      include '../input/fixed/misc.f'

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

      IMPLICIT NONE

*   Purpose:  Write network iteration/timestep information to screen.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netcntrl.inc'
      INCLUDE '../input/fixed/common.f'

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
      ChannelRatioQ=int2ext(ChannelRatioQ)
      ChannelChangeZ=int2ext(ChannelChangeZ)
      MaxChangeChannelQ=int2ext(MaxChangeChannelQ)
      IF( CloseQ ) THEN
         IF( CloseZ ) THEN

            WRITE(unit_screen,2000) TimeStep, Iteration,
     &           ChannelRatioQ, NodeRatioQ, Blank,
     &           ChannelChangeZ, NodeChangeZ, Blank,RatioQ,ChangeZ,MaxChangeQ,MaxChangeChannelQ

         ELSE

            WRITE(unit_screen,2000) TimeStep, Iteration,
     &           ChannelRatioQ, NodeRatioQ, Blank,
     &           ChannelChangeZ, NodeChangeZ, Star,RatioQ,ChangeZ,MaxChangeQ,MaxChangeChannelQ

         END IF

      ELSE IF( CloseZ ) THEN

         WRITE(*,2000) TimeStep, Iteration,
     &        ChannelRatioQ, NodeRatioQ, Star,
     &        ChannelChangeZ, NodeChangeZ, Blank,RatioQ,ChangeZ,MaxChangeQ,MaxChangeChannelQ

      ELSE

         WRITE(*,2000) TimeStep, Iteration,
     &        ChannelRatioQ, NodeRatioQ, Star,
     &        ChannelChangeZ, NodeChangeZ, Star,RatioQ,ChangeZ,MaxChangeQ,MaxChangeChannelQ

      END IF

      ScreenNetworkStatus = .TRUE.

      RETURN
      END

*== Public (IntermediateNetworkResults) ================================

      LOGICAL FUNCTION IntermediateNetworkResults()

      IMPLICIT NONE

*   Purpose:  Report values of variables, current iteration.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netcntrl.inc'

      include '../input/fixed/common.f'

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

*---- Write iteration-status header to the print file.

      IF( PrintLevel .GE. 4
     &     .AND.
     &     PrintCount .EQ. 0
     &     .AND.
     &     Iteration  .EQ. 1 ) THEN

         WRITE(PrintUnit,2001)

      END IF

*---- Write network status to print file if requested.

      IF( PrintLevel .EQ. 37 ) THEN

         OK = ReportNetworkIterationStatus()
         OK = ReportNetworkStatus()

      END IF

*---- Write iteration-status header to screen.

      TimeStep= NetworkTimeStep()

      IF( NetworkPrintLevel() .GE. 4) THEN
         IF(
     &        EndIteration
     &        .AND.
     &        MOD( TimeStep, 20 ) .EQ. 1
     &        )      THEN

            WRITE(*,2001)
            WRITE(*,*) ' '

         END IF
      END IF

*---- Write iteration status to screen.
      IF( EndIteration ) THEN
         IF( NetworkPrintLevel() .GE. 4 ) THEN
            OK = ScreenNetworkStatus()
         END IF
         IF (NetworkIteration() .GE. MaxNetworkIterations()) THEN
            write(unit_error,610) current_dt,NetworkIteration()
 610        format('Warning: at ', a, ' network iterations at maximum (',
     &           i3,').')
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


