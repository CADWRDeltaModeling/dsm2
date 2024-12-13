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

module netdense
    use network
    implicit none
    integer, save:: FortranUnit
    integer, save:: StartSeconds, dT
    integer, save:: OldTime, NewTime, CurrentTime
    real*8, save, allocatable:: Old(:), New(:), Current(:)
    logical, save:: ReadBoundaryValues
!   Definitions:
!     FortranUnit - Fortran unit number for density data.
!     StartSeconds - starting elapse time, in seconds.
!     dT - boundary-value time-series time increment.
!     OldTime - currently the time corresponding to oldest set of
!               boundary values in memory.
!     NewTime - currently the time corresponding to newest set of
!               boundary values in memory.
!     CurrentTime - current Network time, in seconds.
!     Old(i) - boundary values at OldTime.
!     New(i) - boundary values at NewTime.
!     Current(i) - boundary values at CurrentTime.
!     ReadBoundaryValues - indicator,
!                          [.TRUE.] read values.
!                          [.FALSE.] don't read values.
!
contains
    logical function ReadNetworkDensity()
        use IO_Units
        use network
        implicit none

        !   Purpose:  Read density values at channel ends.

        !   Arguments:

        !   Argument definitions:

        !   Module data:



        !   Local Variables:
        integer      I

        !   Routines by module:

        !**** Network schmatic:
        integer  NumberOfChannels
        external NumberOfChannels

        !   Intrinsics:

        !   Programmed by: Lew DeLong
        !   Date:          October  1991
        !   Modified by:   Barry Wicktom (use of master file names added)
        !   Last modified: 1/8/92
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        ReadNetworkDensity = .false.

        OldTime = NewTime
        NewTime = NewTime + dT

        if(not(allocated(Old))) allocate(Old(2*Numch))
        if(not(allocated(New))) allocate(New(2*Numch))

        do 100 I=1,2*NumberOfChannels()
            Old(I) = New(I)
100     continue
        read(FortranUnit,*,end=200) (New(I),I=1,2*NumberOfChannels())
        go to 202
200 continue
    write(UNIT_ERROR,*) ' ####error(ReadNetworkDensity)'
    write(UNIT_ERROR,*) ' New values.',  (New(I),I=1,2*NumberOfChannels())
    write(UNIT_ERROR,*) ' Unexpected end of file.'
    write(UNIT_ERROR,*) ' Possibly channel...',(I+1)/2
    write(UNIT_ERROR,*) ' Abnormal program end.'
    call EXIT(1)
202 continue

    ReadNetworkDensity = .true.

    return
end function

!== Public (SetNewNetworkDensity) ================================================

logical function SetNewNetworkDensity()
    use IO_Units
    use network
    implicit none

    !   Purpose:  Set water density in a network of open channels.
    !             Density is assumed to vary linearly with time
    !             (over a density time increment) and with space
    !             (from one end of a channel to the other).

    !   Arguments:

    !   Argument definitions:

    !   Local Variables:
    integer I, Channel
    real*8 UpstreamDensity, DownstreamDensity
    real*8 Shape1, Shape2
    logical OK

    !   Routines by module:

    !**** Local:
    logical  ReadNetworkDensity
    external ReadNetworkDensity
    logical  SetNewLinearStreamDensity
    external SetNewLinearStreamDensity

    !**** Channel schematic:
    logical  OpenChannel, CloseChannel
    integer  NumberOfChannels
    external NumberOfChannels, OpenChannel, CloseChannel

    !**** Channel control:
    integer  NetworkSeconds
    external NetworkSeconds

    !   Intrinsics:

    !   Programmed by: Lew DeLong
    !   Date:          October  1991
    !   Modified by:
    !   Last modified:
    !   Version 93.01, January, 1993

    !-----Implementation -----------------------------------------------------

    SetNewNetworkDensity = .false.

    CurrentTime = NetworkSeconds()
    if(not(allocated(Current))) allocate(Current(2*NumCh))

100 continue

    !-----Compute current density values at channel ends.

    if( CurrentTime > OldTime ) then
        if( CurrentTime < NewTime ) then

            !-----------Interpolate current values.

            Shape1 = FLOAT(NewTime - CurrentTime)/ &
                FLOAT(NewTime - OldTime)
            Shape2 = 1.0 - Shape1

            
            do 200 I=1,2*NumberOfChannels()
                Current(I) = Shape1*Old(I) + Shape2*New(I)
            !--------------WRITE(*,*) I,Current(I)
200         continue

        else if( CurrentTime == NewTime ) then

            !-----------Assign current values.

            do 300 I=1,2*NumberOfChannels()
                Current(I) = New(I)
            !--------------WRITE(*,*) I,Current(I)
300         continue

        else

            !-----------Read new values.

            OK = ReadNetworkDensity()

            go to 100

        end if
    else if(CurrentTime == OldTime ) then

        !--------Read new values.

        OK = ReadNetworkDensity()

        go to 100

    else

        !--------Shouldn't ever get here!

        write(UNIT_ERROR,*) ' ####error(SetNewNetDensity)'
        write(UNIT_ERROR,*) ' Current time < available from boundary values.'
        write(UNIT_ERROR,*) ' CurrentTime...',CurrentTime
        write(UNIT_ERROR,*) ' OldTime.......',OldTime
        write(UNIT_ERROR,*) ' Abnormal program end.'
        call EXIT(1)
    end if

    do 400 I=1,NumberOfChannels()

        Channel = I
        OK = OpenChannel(Channel)

        UpstreamDensity = Current(2*I-1)
        DownstreamDensity = Current(2*I)

        if( SetNewLinearStreamDensity( &
            UpstreamDensity, DownstreamDensity) &
            ) then
        else

            write(UNIT_ERROR,*) ' ####Error(SetNetworkDensity)'
            write(UNIT_ERROR,*) ' Water density not set, channel...',Channel
            return

        end if

        OK = CloseChannel()

400 continue

    SetNewNetworkDensity = .true.

    return
end function

!== Private (SetNewLinearStreamDensity) ====================================

logical function SetNewLinearStreamDensity( &
    UpstreamDensity, DownstreamDensity &
    )
    use network
    use channel_schematic, only: UpstreamPointer, DownstreamPointer, &
        NumberOfStreamLocations, StreamDistance
    use chstatus, only: NewStreamDensity, SetOldStreamDensity, SetNewStreamDensity
    implicit none

    !   Purpose:  Set stream density to values linearly interpolated from
    !              values supplied at extremities of current channel, at
    !              the end of the current time step.

    !   Arguments:
    real*8 UpstreamDensity, DownstreamDensity

    !   Argument definitions:
    !     UpstreamDensity - density at the upstream end of current channel.
    !     DownstreamDensity - density at downstream end of current channel.

    !   Local Variables:
    integer I, N
    real*8    XUp, dX, Shape, Rho
    logical OK

    !   Routines by module:
    !   Intrinsics:

    !   Programmed by: Lew DeLong
    !   Date:          October  1991
    !   Modified by:
    !   Last modified:
    !   Version 93.01, January, 1993

    !-----Implementation -----------------------------------------------------

    XUp = StreamDistance(1)
    dX = StreamDistance( NumberOfStreamLocations() ) - XUp

    N = 0
    do 100 I=UpstreamPointer(),DownstreamPointer()
        N = N + 1

        OK = SetOldStreamDensity( N, NewStreamDensity(N) )

        Shape = ( StreamDistance(N) - XUp ) / dX
        Rho = (1.0 - Shape) * UpstreamDensity &
            + Shape * DownstreamDensity
        OK = SetNewStreamDensity( N, Rho )

100 continue

    SetNewLinearStreamDensity = .true.

    return
end function
end module
!===== EOF netdense =====================================================
