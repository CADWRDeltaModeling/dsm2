C!    Copyright (C) 1996, 1997, 1998 State of California,
C!    Department of Water Resources.
C!
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

*===== BOF netcntrl.inc ================================================
*   Version 93.01, January, 1993

*   Note: This include, if used, must appear after "Network.inc".

      INTEGER Terms, MaxTimeSteps, TimeStep, MaxIterations, LuInc
      INTEGER Iteration, TotalIterations, QuadPts
      INTEGER Seconds
      INTEGER MaxBndEq, MaxTS
      PARAMETER (MaxBndEq = 2*MaxChannels, MaxTS = 10 )
      INTEGER PrintLevel, PrintCount, PrintInc
      INTEGER TimeSeriesCount, TimeSeriesInc
      INTEGER SpatialSeriesCount, SpatialSeriesInc
      INTEGER TSChannel(MaxTS)
      INTEGER TimeSeriesLocation(MaxTS), TimeSeries
      INTEGER BoundaryEquations, EqChannelNumber(2*MaxChannels)
      INTEGER EqNumber(2*MaxChannels+1)
      INTEGER EqComponents(2*MaxChannels), MaxComponents
      PARAMETER (MaxComponents = 5)
      INTEGER ChannelRatioQ, NodeRatioQ, ChannelChangeZ, NodeChangeZ
      INTEGER DT, NetStartTime
      REAL*8    TSX(MaxTS)
      REAL*8    QuadPt(MaxQuadPts), QuadWt(MaxQuadPts)
      REAL*8    Theta, ToleranceQ, ToleranceZ

      INTEGER MaxChangeChannelQ
      REAL*8    ChangeZ, RatioQ, MaxChangeQ
      LOGICAL CloseQ, CloseZ, EndIteration
      LOGICAL VariableSinuosity, VariableDensity, Perturbation
      LOGICAL Restart_Write,Restart_Read

      COMMON  / NetCntrl_i /
     &     Terms, MaxTimeSteps, TimeStep, MaxIterations, Iteration,
     &     LuInc, TotalIterations, QuadPts, NetStartTime,
     &     PrintLevel, PrintCount, PrintInc,
     &     TimeSeriesCount, TimeSeriesInc,
     &     SpatialSeriesCount, SpatialSeriesInc,
     &     TimeSeriesLocation, TimeSeries,TSChannel,
     &     BoundaryEquations, EqChannelNumber, EqComponents, EqNumber,
     &     ChannelRatioQ, NodeRatioQ, ChannelChangeZ, NodeChangeZ,DT,
     &     Seconds, EndIteration,
     &     VariableSinuosity, VariableDensity, Perturbation,
     &     Restart_Write,Restart_Read
      COMMON  / NetCntrl_r /
     &     TSX,Theta, ToleranceQ, ToleranceZ,
     &     QuadPt, QuadWt,
     &     ChangeZ, RatioQ, CloseQ, CloseZ,
     &     MaxChangeQ, MaxChangeChannelQ

      SAVE / NetCntrl_i /
      SAVE / NetCntrl_r/

*   Definitions:

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
*     Restart_Write - .TRUE. if file of dependent and independent final
*                            values is to be written to a restart file, otherwise false.
*
*     Restart_Read - .TRUE. if file of dependent and independent final
*                            values is to be read from a restart file, otherwise false.
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
*     NetStartTime - elapse time at beginning of current model execution,
*                    in seconds.
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

*===== EOF netcntrl.inc ================================================
