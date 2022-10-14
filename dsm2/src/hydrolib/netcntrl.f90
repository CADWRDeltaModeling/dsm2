!!<license>
!!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
!!    Department of Water Resources.
!!    This file is part of DSM2.

!!    The Delta Simulation Model 2 (DSM2) is free software:
!!    you can redistribute it and/or modify
!!    it under the terms of the GNU General Public License as published by
!!    the Free Software Foundation, either version 3 of the License, or
!!    (at your option) any later version.

!!    DSM2 is distributed in the hope that it will be useful,
!!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!!    GNU General Public License for more details.

!!    You should have received a copy of the GNU General Public License
!!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
!!</license>

!===== BOF netcntrl.inc ================================================
!   Version 93.01, January, 1993

module netcntrl
    use netcntrl_common
    implicit none
    integer, save:: MaxTimeSteps
    integer, save:: TimeStep
    integer, save:: Iteration
    integer, save:: TotalIterations
    integer, save:: QuadPts
    integer, save:: Seconds
    integer, parameter:: MaxBndEq=2*MaxChannels
    integer, parameter:: MaxTS=10
    integer, save:: PrintLevel
    integer, save:: PrintCount
    integer, save:: PrintInc
    integer, save:: TimeSeriesCount
    integer, save:: TimeSeriesInc
    integer, save:: SpatialSeriesCount
    integer, save:: SpatialSeriesInc
    integer, save:: TSChannel(MaxTS)
    integer, save:: TimeSeriesLocation(MaxTS)
    integer, save:: TimeSeries
    integer, save:: BoundaryEquations
    integer, save:: EqChannelNumber(2*MaxChannels)
    integer, save:: EqNumber(2*MaxChannels+1)
    integer, save:: EqComponents(2*MaxChannels)
    integer, parameter:: MaxComponents =5
    integer, save:: ChannelRatioQ
    integer, save:: NodeRatioQ
    integer, save:: ChannelChangeZ
    integer, save:: NodeChangeZ
    integer, save:: DT
    integer, save:: NetStartTime
    real*8, save:: TSX(MaxTS)
    integer, save:: MaxChangeChannelQ
    real*8, save::    ChangeZ
    real*8, save:: RatioQ
    real*8, save:: MaxChangeQ
    logical, save:: CloseQ
    logical, save:: CloseZ
    logical, save:: EndIteration
    logical, save:: Perturbation
    logical, save:: Restart_Write
    logical, save::Restart_Read
    
!   Definitions:

!     MaxBndEq - maximum number of equation-type boundaries.
!     MaxTS - Maximum number of locations at which timeseries
!             output may be accumulated.

!     Terms - index indicating form of 1-D flow equations.
!              [1] full dynamic-wave equation.
!              [2] diffusion-wave equation.
!              [3] kinematic-wave equation.
!
!     VariableSinuosity - .TRUE. if allowed to vary, .FALSE. otherwise.
!     VariableDensity - .TRUE. if allowed to vary, .FALSE. otherwise.
!     Perturbation - .TRUE. if perturbation of input is requested,
!                    otherwise .FALSE. .
!     Restart_Write - .TRUE. if file of dependent and independent final
!                            values is to be written to a restart file, otherwise false.
!
!     Restart_Read - .TRUE. if file of dependent and independent final
!                            values is to be read from a restart file, otherwise false.
!
!     Gravity - acceleration due to gravity.
!
!     RandSeed - seed for random-number generator.
!
!     MaxTimeSteps - maximum number of time steps allowed.
!     TimeStep - current time step.
!     MaxIterations - Maximum number of iterations per time step.
!     Iteration - current iteration number.
!     TotalIterations - total iterations.
!     LuInc - interval of complete forward eliminations, in terms of
!             number of iterations during a time step.
!
!     PrintLevel - level of printing activity, 0 to 9, activity
!                   increasing with increasing number.
!     PrintCount - counter for printed output.
!     PrintInc - increment, in time steps, for printed output.
!     TimeSeriesCount - counter for time-series output.
!     TimeSeriesInc - increment, in time steps, for time-series output.
!     SpatialSeriesCount - counter for spatial-series output.
!     SpatialSeriesInc - increment for spatial-series output.
!
!     TimeSeriesLocation(i) - global computational location sequence number
!                              for the "i"th time series.
!     TSChannel(i) - channel number coreesponding to TimeSeriesLocation(i).
!     TSX(i) - downstream distance corresponding to TimeSeriesLocation(i).
!
!     TimeSeries - total number of time series requested.
!
!
!
!                 Equation-type boundary values ...
!
!       value = EqBase + for each component [
!
!         Amplitude * cos( TwoPI* (Hours+PhaseAngle) / Period )
!
!                                           ]
!
!
!
!             ( time unit for following constants is hours... )
!
!     BoundaryEquations - number of user-supplied boundary equations.
!     EqNumber(k+MaxChannels+1) - equation number for channel k
!      (upstream end of channel k>0, downstream end of channel k<0).
!     EqChannelNumber(j) - channel number, + upsteam, - downstream, equation j.
!     EqComponents(j) - number of harmonic components in equation j.
!     MaxEqComponents - maximum harmonic components per equation.
!     EqBase(j) - base value, equation j.
!     Amplitude1(i,j) - amplitude of component i, equation j.
!     Period(i,j) - period, in hours, of component i, equation j.
!     PhaseAngle(i,j) - phase angle, in hours, of component i, equation j.
!     EqStart(j) - elapse time, in hours, at which equation becomes effective.
!     EqEnd(j) - elapse time, in hours, time at which equation is no longer effective.
!     j - sequential storage index.
!
!
!
!          Standard deviations of normal distributions of errors, used to
!          perturb boundary-equation values, follow. Perturbation will be active
!          when the file "perturb.dat" exists and may be opened and read
!          successfully.  Standard deviations are initialized to zero.
!          Non-zero values are read from file "perturb.dat" if it exists.
!
!
!          group 1: applied each time step and held constant over a time step.
!
!     EqBaseStdDev(j) - standard deviation of a random normal distribution
!                          of errors in base value for equation j.
!     AmplitudeStdDev(j,i) - standard deviation of a random normal distribution
!                          of errors in amplitude for component i, equation j.
!     PeriodStdDev(j,i) - standard deviation of a random normal distribution
!                          of errors in period for component i, equation j.
!     PhaseAngleStdDev(j,i) - standard deviation of a random normal distribution
!                          of errors in phase angle for component i, equation j.
!
!          group 2: applied only once, at the beginning of a model run,
!                   and held constant through out the entire run.
!
!     EqBaseBias(j,i) - standard deviation of a random normal distribution
!                          of errors in base value for equation j.
!     AmplitudeBias(j,i) - standard deviation of a random normal distribution
!                          of errors in amplitude for component i, equation j.
!     PeriodBias(j,i) - standard deviation of a random normal distribution
!                          of errors in period for component i, equation j.
!     PhaseAngleBias(j,i) - standard deviation of a random normal distribution
!                          of errors in phase angle for component i, equation j.
!
!
!
!     NetStartTime - elapse time at beginning of current model execution,
!                    in seconds.
!     DT - time increment, in seconds.
!     Theta - time-weighting factor.
!     ToleranceQ - closure tolerance for discharge.
!     ToleranceZ - closure tolerance for water-surface elevation.
!     RatioQ - current maximum ratio of change in Q to prior Q (flow).
!     ChangeZ - current maximum change in Z (water-surface elevation).
!     ChannelRatioQ - channel in which RatioQ occurs.
!     NodeRatioQ - location within ChannelRatioQ that RatioQ occurs.
!     ChannelChangeZ - channel in which ChangeZ occurs.
!     NodeChangeZ - location within ChannelChangeZ that ChangeZ occurs.
!
!     CloseQ - .TRUE. if closure criteria for discharge has been
!                determined to currently be satisfied, .FALSE. otherwise.
!     CloseZ - .TRUE. if closure criteria for ws_elev has been determined
!                to currently be satisfied, .FALSE. otherwise.
!     EndIteration - .TRUE. if it has determined that iterations are to
!                      stop, .FALSE. otherwise.
!
!
!     MaxQuadPts - Maximum number of quadrature points.
!     QuadPts - current number of quadrature points.
!     QuadPt(i) - location of quadrature point "i",
!                  in local coordinate ( 0 to 1 ).
!     QuadWt(i) - weight, corresponding to quadrature point "i",
!                  for numerical integration.
!
!     Seconds - elapse time, in seconds

contains
    integer function NetworkTimeSteps()
        implicit none
        !   Purpose:  Return requested number of time steps for network.
        NetworkTimeSteps = MaxTimeSteps
        return
    end function NetworkTimeSteps

    integer function MaxNetworkIterations()
        implicit none
        !   Purpose: Return the maximum number of iterations allowed for each
        !            time step executed by an open-channel network model.
        MaxNetworkIterations = MaxIterations
        return
    end function MaxNetworkIterations

    integer function NetworkTimeIncrement()
        implicit none
        !   Purpose: Return the time increment used by an
        !            open-channel network model.
        NetworkTimeIncrement = DT
        return
    end function NetworkTimeIncrement

    real*8 function NetworkTheta()
        implicit none
        !   Purpose:  Return the time-weighting factor used by an
        !             open-channel network model.
        NetworkTheta = Theta
        return
    end function NetworkTheta

    real*8 function ToleranceStreamZ()
        implicit none
        !   Purpose:  Return the closure tolerance for water-surface elevation.
        ToleranceStreamZ = ToleranceZ
        return
    end function ToleranceStreamZ

    real*8 function ToleranceStreamQ()
        implicit none
        !   Purpose:  Return the closure tolerance for stream flow.
        ToleranceStreamQ = ToleranceQ
        return
    end function ToleranceStreamQ
    integer function NetworkPrintLevel()
        implicit none
        !   Purpose:  Return level of printing activity.
        NetworkPrintLevel = PrintLevel
        return
    end function NetworkPrintLevel

    integer function NetworkQuadPts()
        implicit none
        !   Purpose:  Return current number of quadrature points to be used
        !             in numerical integration.
        NetworkQuadPts = QuadPts
        return
    end function NetworkQuadPts

    subroutine NetworkQuadPtWt(&
        K,&
        Pt, Wt&
        )
        implicit none
        !   Purpose:  Return location, in local coordinate ( 0 to 1 ), and weight
        !             of quadrature-point K.
        !   Arguments:
        integer :: K ! quadrature-point sequence number.
        real*8 :: Pt ! location of quadrature point K, in local coordinate ( 0 to 1 ).
        real*8 :: Wt ! weight, corresponding to quadrature point "K",for numerical integration.
        Pt = QuadPt(K)
        Wt = QuadWt(K)
        return
    end subroutine NetworkQuadPtWt

    integer function NetworkTimeStep()
        implicit none
        !   Purpose:  Return current time step number.
        NetworkTimeStep = TimeStep
        return
    end function NetworkTimeStep

    integer function NetworkSeconds()
        implicit none
        !   Purpose:  Return current network elapse time in seconds.
        !             Seconds = 0 at begining of current execution.
        NetworkSeconds = Seconds
        return
    end function NetworkSeconds

    logical function IncrementNetworkTimeStep()
        implicit none
        !   Purpose:  Increment current time step number.
        TimeStep = TimeStep + 1
        Seconds = Seconds + DT
        if( TimeStep <= MaxTimeSteps ) then
            IncrementNetworkTimeStep = .true.
        else
            IncrementNetworkTimeStep = .false.
        end if
        return
    end function IncrementNetworkTimeStep

    logical function IncrementNetworkIteration()
        implicit none
        !   Purpose:  Increment current iteration number.
        Iteration = Iteration + 1
        if( Iteration <= MaxIterations ) then
            IncrementNetworkIteration = .true.
        else
            IncrementNetworkIteration = .false.
        end if
        return
    end function IncrementNetworkIteration

    logical function StoreNetworkClosure(&
        MaxRatioQ, ChannelQ, NodeQ,&
        MaxChangeZ, ChannelZ, NodeZ,&
        NormClose &
        )
        implicit none
        !   Purpose:  Store current network closure information, and check if
        !              current information satisfies criteria.

        !   Arguments:
        integer :: ChannelQ ! channel number in which MaxRatioQ occurred.
        integer :: ChannelZ ! channel number in which MaxChangeZ ocurred.
        integer :: NodeQ ! local location number at which MaxRatioQ occurred.
        integer :: NodeZ ! local location number at which MaxChangeZ occurred.
        real*8 :: MaxRatioQ ! maximum ratio of change in flow over the last iteration to the current value of flow.
        real*8 :: MaxChangeZ !maximum change in water-surface elevation over the last iteration.
        logical :: NormClose
        !---- Store current values.
        RatioQ         = MaxRatioQ
        ChannelRatioQ  = ChannelQ
        NodeRatioQ     = NodeQ

        ChangeZ        = MaxChangeZ
        ChannelChangeZ = ChannelZ
        NodeChangeZ    = NodeZ

        !---- Test for overall closure.
        StoreNetworkClosure = .false.

        if( ChangeZ > ToleranceZ ) then
            StoreNetworkClosure = .false.
            CloseZ = .false.
        else
            CloseZ = .true.
        end if

        if( RatioQ > ToleranceQ ) then
            StoreNetworkClosure = .false.
            CloseQ = .false.
        else
            CloseQ = .true.
        end if

        if(NormClose) then 
            StoreNetworkClosure = .true.
        else
            !--------Check for exceedence of maximum number of iterations.
            if( Iteration >= MaxIterations ) then
                StoreNetworkClosure = .true.
            end if
        end if

        EndIteration = StoreNetworkClosure
        return
    end function StoreNetworkClosure

    logical function CloseNetworkIteration()
        implicit none
        !   Purpose:  Close out the current iteration.
        !**** Buffered output:
        logical :: WriteNetworkSpatialSeries
        external WriteNetworkSpatialSeries
        !---- Increment output counters.

        PrintCount = PrintCount + 1

        !---- Reset print counter.

        if( PrintCount >= PrintInc )  then
            PrintCount = 0
        end if

        !---- Sum total iterations.

        TotalIterations = TotalIterations + Iteration

        CloseNetworkIteration = .true.

        return
    end function CloseNetworkIteration

    integer function TotalNetworkIterations()
        implicit none
        !   Purpose:  Return level of printing activity.
        TotalNetworkIterations = TotalIterations
        return
    end function TotalNetworkIterations

    logical function ScreenNetworkStatus()
        use IO_Units
        use grid_data
        implicit none
        !   Purpose:  Write network iteration/timestep information to screen.
        !   Local Variables:
        character(len=1)::Star
        character(len=1)::Blank
        parameter ( Star = '*', Blank = ' ' )

        !   Output formats:

2000    format(2I5,2I8,A1,4X,2I8,A1,3(1x,f12.6),1x,i4)

        TimeStep= NetworkTimeStep()
        if( CloseQ ) then
            if( CloseZ ) then

                write(unit_screen,2000) TimeStep, Iteration, &
                    chan_geom(ChannelRatioQ)%chan_no, NodeRatioQ, Blank, &
                    chan_geom(ChannelChangeZ)%chan_no, NodeChangeZ, Blank,RatioQ, &
                    ChangeZ,MaxChangeQ,chan_geom(MaxChangeChannelQ)%chan_no

            else

                write(unit_screen,2000) TimeStep, Iteration, &
                    chan_geom(ChannelRatioQ)%chan_no, NodeRatioQ, Blank, &
                    chan_geom(ChannelChangeZ)%chan_no, NodeChangeZ, Star,RatioQ, &
                    ChangeZ,MaxChangeQ,chan_geom(MaxChangeChannelQ)%chan_no

            end if

        else if( CloseZ ) then

            write(*,2000) TimeStep, Iteration, &
                chan_geom(ChannelRatioQ)%chan_no, NodeRatioQ, Star, &
                chan_geom(ChannelChangeZ)%chan_no, NodeChangeZ, Blank,RatioQ, &
                ChangeZ,MaxChangeQ,chan_geom(MaxChangeChannelQ)%chan_no

        else

            write(*,2000) TimeStep, Iteration, &
                chan_geom(ChannelRatioQ)%chan_no, NodeRatioQ, Star, &
                chan_geom(ChannelChangeZ)%chan_no, NodeChangeZ, Star,RatioQ, &
                ChangeZ,MaxChangeQ,chan_geom(MaxChangeChannelQ)%chan_no

        end if

        ScreenNetworkStatus = .true.

        return
    end function ScreenNetworkStatus

    integer function NetworkIteration()
        implicit none
        !   Purpose: Return current iteration number.
        NetworkIteration = Iteration
        return
    end function NetworkIteration

    integer function ForwardElimInt()
        implicit none

        !   Purpose: Return interval at which complete forward eliminations
        !            are to be performed on the solution matrix, in terms of
        !            number of iterations during a time step.
        ForwardElimInt = LuInc
        return
    end function ForwardElimInt

    logical function NetworkRestart()
        implicit none
        !   Purpose: Return .TRUE. if network restart file has been requested,
        !            otherwise return .FALSE..
        NetworkRestart = Restart_Write
        return
    end function NetworkRestart

    logical function VariableStreamDensity()
        implicit none
        !   Purpose: Return .TRUE. if density is allowed to vary with
        !             time and distance, .FALSE. otherwise.
        VariableStreamDensity = VariableDensity
        return
    end function VariableStreamDensity

    logical function VariableStreamSinuosity()
        implicit none
        !   Purpose: Return .TRUE. if sinuosity is allowed to vary with
        !             depth of flow and distance, .FALSE. otherwise.
        VariableStreamSinuosity = VariableSinuosity
        return
    end function VariableStreamSinuosity
end module netcntrl
