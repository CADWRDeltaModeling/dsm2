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

!==== BOF chschmt ======================================================

!****************************************************************************
!
!     This file is a FORTRAN module containing schematic data associated with
!     networks of 1-D channels.
!
!
!
!
!
!****************************************************************
module channel_schematic
    !! Module to manage channel schematic data
    !! Original procedures from FourPt.
    use network
    implicit none
contains
    !== Public (NumberOfChannels) ==========================================

    integer function NumberOfChannels()
        use IO_Units
        implicit none

        !   Purpose:  Return number of channels in current channel network.

        !   Arguments:

        !   Argument definitions:

        !   Local Variables:

        !   Routines by modules:

        !   Programmed by: Lew DeLong
        !   Date:          November 1990
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation ---------------------------------------------------

        if( NumCh > 0 ) then
            NumberOfChannels = NumCh
        else
            write(UNIT_ERROR,*) ' Network not initialized (NumberOfChannels)...'
            call EXIT(1)
        end if

        return
    end function

    !== Public (CurrentChannel) ============================================

    integer function CurrentChannel()
        use IO_Units
        use grid_data
        implicit none

        !   Purpose:  Return current channel number.

        !   Arguments:

        !   Argument definitions:



        !   Local Variables:

        !   Routines by module:

        !   Programmed by: Lew DeLong
        !   Date:          November 1990
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation ---------------------------------------------------

        if( Branch > 0 ) then
            CurrentChannel = Branch
        else
            write(UNIT_ERROR,*) ' CurrentChannel not set (CurrentChannel)...'
            call EXIT(1)
        end if

        return
    end function

    !== Public (OpenChannel) ===============================================

    logical function OpenChannel(ChannelNumber)
        use IO_Units
        use grid_data
        implicit none

        !   Purpose:  Set current channel number.

        !   Arguments:
        integer :: ChannelNumber

        !   Argument definitions:
        !      ChannelNumber - channel sequence number.



        !   Local Variables:

        !   Routines by module:

        !   Programmed by: Lew DeLong
        !   Date:          November 1990
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation ---------------------------------------------------

        if( Branch == 0 ) then

            if( ChannelNumber <= NumCh .and. ChannelNumber > 0 ) then
                OpenChannel = .true.
                Branch = ChannelNumber
            else
                OpenChannel = .false.
                write(UNIT_ERROR,*) ' Can not open channel', chan_geom(ChannelNumber)%chan_no
                write(UNIT_ERROR,*) ' NumberOfChannels = ', NumCh
            end if

        else
            write(UNIT_ERROR,*) ' *** Error (OpenChannel)'
            write(UNIT_ERROR,*) ' Channel ', Branch, ' still open ???'
            OpenChannel = .false.
        end if

        return
    end function

    !== Public (CloseChannel) ==============================================

    logical function CloseChannel()
        use network
        implicit none

        !   Purpose:  Close current channel, set CurrentChannel = 0.

        !   Arguments:

        !   Argument definitions:


        !   Local Variables:

        !   Routines by module:

        !   Programmed by: Lew DeLong
        !   Date:          November 1990
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation ---------------------------------------------------

        if( Branch > 0 ) then
            CloseChannel = .true.
            Branch = 0
        else
            CloseChannel = .false.
            Branch = 0
        end if

        return
    end function


    !== Public (SetCompLocations) ==========================================

    logical function SetCompLocations()
        use IO_Units
        use grid_data
        use chnluser
        use chnlcomp
        use chconnec
        use netcntrl ,only: NetworkPrintLevel
        use linear, only: xinsrt
        implicit none

        !   Purpose:

        !   Arguments:

        !   Argument definitions:
        !   Local Variables:
        integer :: I
        integer :: ChNum
        integer :: MaxCx
        integer :: ModelIndex
        logical :: OK
        logical :: Out

        !**** Local:
        real*8 :: XEnd(2)

        !   Programmed by: Lew DeLong
        !   Date:          November 1990
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        SetCompLocations = .false.
        if( NetworkPrintLevel() > 3) then
            write(*,*) ' Setting computational locations...'
        end if

        !-----Set output index.

        if( NetworkPrintLevel() >= 3 ) then
            Out = .true.
        else
            Out = .false.
        end if

        ModelIndex = 1
        MaxCx = MAX_LOCATIONS
        TotalCompLocations = 0
        
        if(not(allocated(NumberOfCompLocations)))allocate(NumberOfCompLocations(Numch))
        if(not(allocated(UpCompPointer)))allocate(UpCompPointer(Numch))
        if(not(allocated(DownCompPointer)))allocate(DownCompPointer(Numch))
        !-----Begin channel loop.
        do I = 1, NumCh
            ChNum = I

            if( OpenChannel( ChNum ) ) then


                !-----------Insert computational locations if needed.
                if( ChNum > 1 ) then
                    UpCompPointer(ChNum) = DownCompPointer(ChNum-1) + 1
                else
                    UpCompPointer(ChNum) = 1
                end if
                XEnd(1)=0.
                XEnd(2)=chan_geom(branch)%length
                if( XInsrt( &
                    MaxCx, &
                    2,XEnd, & !(user points are ignored for generating comp. points)
                    dX(ChNum), .false., ModelIndex, &
                    NumberOfCompLocations(ChNum), &
                    CompLocation( UpCompPointer(ChNum) ) &
                    )  )  then

                    if( Out ) then
                        write(*,*) '  Channel number ',chan_geom(ChNum)%chan_no,' # points:', &
                            NumberOfCompLocations(ChNum)
                    end if

                    TotalCompLocations = TotalCompLocations &
                        + NumberOfCompLocations(ChNum)

                else
                    write(UNIT_ERROR,*) ' ####error(SetCompLocations)'
                    write(UNIT_ERROR,*) ' Maximum number of computational ', &
                        'locations exceeded...'
                    write(UNIT_ERROR,*) ' Insertion of computational locations failed...'
                    write(UNIT_ERROR,*) ' Attempting channel...',chan_geom(ChNum)%chan_no
                    return
                end if

                DownCompPointer(ChNum) = UpCompPointer(ChNum) &
                    + NumberOfCompLocations(ChNum) - 1

                !-----------Reduce maximum number of remaining locations.
                MaxCx = MaxCx - NumberOfCompLocations(ChNum)
                if( MaxCx < 0 ) then
                    write(UNIT_ERROR,*) ' Max number of computational locations', &
                        ' exceeded...(SetCompLocations)'
                    write(UNIT_ERROR,*) ' Channel number...',chan_geom(ChNum)%chan_no
                end if

                OK = CloseChannel()

            else

                write(UNIT_ERROR,*) ' Attempt to open channel',chan_geom(ChNum)%chan_no,' failed...'
                write(UNIT_ERROR,*) ' (SetCompLocations)'
                return

            end if

        end do

        !-----Determine global, computational-cross-section number
        !       of locations at which output of time-series results
        !       is required.
        !      OK = SetNetworkTimeSeriesLocations()

        SetCompLocations = .true.

        return
    end function

    !== Public (NumberOfStreamLocations) ===================================

    integer function NumberOfStreamLocations()
        use chnlcomp
        implicit none

        !   Purpose:  Return the number of computational locations in the
        !             current channel.

        !   Arguments:

        !   Argument definitions:


        !   Local Variables:

        !   Routines by module:

        !   Programmed by: Lew DeLong
        !   Date:          November 1990
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        NumberOfStreamLocations = NumberOfCompLocations(Branch)

        return
    end function

    !== Public (TotalStreamLocations) ======================================

    integer function TotalStreamLocations()
        use chnlcomp
        implicit none

        !   Purpose:  Return total number of computational locations in network.

        !   Arguments:

        !   Argument definitions:


        !   Local Variables:

        !   Routines by module:

        !   Programmed by: Lew DeLong
        !   Date:          November 1990
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        TotalStreamLocations = TotalCompLocations

        return
    end function

    !== Public (StreamEndNode) =============================================

    integer function StreamEndNode( ChannelNumber )
        use chnlcomp
        implicit none

        !   Purpose:  Return a global location number delimiting the channel,
        !             ChannelNumber.  The location number will be for the
        !             upstream end of the channel if ChannelNumber is
        !             positive and downstream if negative.  The returned
        !             location number will carry the same sign as ChannelNumber.

        !   Arguments:
        integer :: ChannelNumber

        !   Argument definitions:
        !      ChannelNumber - channel sequence number.

        !   Local Variables:

        !   Routines by module:

        !   Programmed by: Lew DeLong
        !   Date:          November 1990
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        if( ChannelNumber > 0 ) then

            !--------Upstream end of channel.

            StreamEndNode = UpCompPointer( ChannelNumber )

        else if( ChannelNumber < 0 ) then

            !--------Downstream end of channel.

            StreamEndNode = - DownCompPointer(  - ChannelNumber )

        else

            !--------No channel number.

            StreamEndNode = 0

        end if

        return
    end function

    !== Public (UpstreamPointer) ===========================================

    integer function UpstreamPointer()
        use chnlcomp
        implicit none

        !   Purpose:  Return pointer to upstream computational location number
        !             for current channel.

        !   Arguments:

        !   Argument definitions:


        !   Local Variables:

        !   Routines by module:

        !   Programmed by: Lew DeLong
        !   Date:          November 1990
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        UpstreamPointer = UpCompPointer(Branch)

        return
    end function

    !== Public (DownstreamPointer) =========================================

    integer function DownstreamPointer()
        use chnlcomp
        implicit none

        !   Purpose:  Return pointer to downstream computational location number
        !             for current channel.

        !   Arguments:

        !   Argument definitions:


        !   Local Variables:

        !   Routines by module:

        !   Programmed by: Lew DeLong
        !   Date:          November 1990
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        DownstreamPointer = DownCompPointer(Branch)

        return
    end function

    !== Public (StreamDistance) ============================================

    real*8 function StreamDistance(LocationNumber)
        use IO_Units
        use chnlcomp
        implicit none

        !   Purpose:  Return downstream distance to the computational
        !             LocationNumber, within the current channel.

        !   Arguments:
        integer :: LocationNumber

        !   Argument definitions:
        !     LocationNumber - computational location number, begining with 1
        !                       at upstream end of current channel.

        !   Local Variables:

        !   Routines by module:


        !   Programmed by: Lew DeLong
        !   Date:          November 1990
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        if( CheckChannelCompLocationRange(LocationNumber) ) then

            StreamDistance = CompLocation( &
                UpCompPointer(Branch) + LocationNumber - 1 &
                )

        else

            write(UNIT_ERROR,*) ' Failed to determine correct stream distance...'
            write(UNIT_ERROR,*) ' Incorrectly returned last location in channel...'
            write(UNIT_ERROR,*) ' (StreamDistance)'
            StreamDistance = CompLocation( &
                DownCompPointer(Branch) &
                )

        end if

        return
    end function

    !== Public (GlobalStreamDistance) ======================================

    real*8 function GlobalStreamDistance(GlobalLocationNumber)
        use IO_Units
        use chnlcomp
        implicit none

        !   Purpose:  Return downstream distance to the GlobalLocationNumber.

        !   Arguments:
        integer :: GlobalLocationNumber

        !   Argument definitions:
        !     GlobalLocationNumber - global location number, sequential
        !                             with out regard to channel number.


        !   Local Variables:

        !   Routines by module:

        !   Programmed by: Lew DeLong
        !   Date:          March 1990
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        if( GlobalLocationNumber > 0 &
            .and. &
            GlobalLocationNumber <= MAX_LOCATIONS ) then
            GlobalStreamDistance = CompLocation( GlobalLocationNumber )
        else
            write(UNIT_ERROR,*) ' ####error( GlobalStreamDistance )'
            write(UNIT_ERROR,*) ' Global location number out of range...'
            GlobalStreamDistance = 0.0
        end if
        return
    end function

    !== Private (CheckChannelCompLocationRange) ============================

    logical function CheckChannelCompLocationRange(LocationNumber)
        use IO_Units
        use chnlcomp
        implicit none

        !   Purpose:  Check to see if LocationNumber is in the range of
        !             location numbers available to the current channel.

        !   Arguments:
        integer :: LocationNumber

        !   Argument definitions:
        !     LocationNumber - computational-location number (index) within
        !                      current channel.

        !   Local Variables:

        !   Routines by module:

        !   Programmed by: Lew DeLong
        !   Date:          November 1990
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        if( LocationNumber <= NumberOfCompLocations(Branch) &
            .and. &
            LocationNumber > 0 ) then

            CheckChannelCompLocationRange = .true.

        else

            write(UNIT_ERROR,*) ' Location number out of range...'
            write(UNIT_ERROR,*) ' Branch...',Branch
            write(UNIT_ERROR,*) ' Requested location number = ',LocationNumber
            write(UNIT_ERROR,*) ' Available range = 1', &
                ' to',NumberOfCompLocations(Branch)
            CheckChannelCompLocationRange = .false.

        end if

        return
    end function


    !== Public (GetUserStreamLocationIDs) ==================================

    subroutine GetUserStreamLocationIDs( &
        Num, &
        CurrentIDs &
        )
        use chnluser
        implicit none

        !   Purpose:  Return an array of current  user stream-location
        !             identifiers for the current channel.  Dimension of
        !             returned identifier array must be large enough and is
        !             not checked by this routine.

        !   Arguments:
        integer :: Num
        character*16 :: CurrentIDs(Num)

        !   Argument definitions:
        !     Num - dimension of identifier array.
        !     CurrentIDs(i) - array of identifiers.

        !   Local Variables:
        integer :: I
        integer :: J

        !   Routines by module:

        !   Programmed by: Lew DeLong
        !   Date:          January 1991
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        J = 0
        do I=UpUserPointer(Branch),DownUserPointer(Branch)
            J = J + 1
            CurrentIDs(J) = UserLocationID(I)
        end do

        return
    end subroutine

    !== Public (GetUserStreamFlow) =========================================

    subroutine GetUserStreamFlow( &
        Num, &
        Streamflow &
        )
        use chnluser
        implicit none

        !   Purpose:  Return an array of initial streamflow values at user
        !             locations within the current channel.  Dimension of
        !             returned array must be large enough and is not
        !             checked by this routine.

        !   Arguments:
        integer :: Num
        real*8 :: Streamflow(Num)

        !   Argument definitions:
        !     Num - dimension of identifier array.
        !     Streamflow(i) - array of streamflow values.


        !   Local Variables:
        integer :: I
        integer :: J

        !   Routines by module:

        !   Programmed by: Lew DeLong
        !   Date:          January 1991
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        J = 0
        do  I=UpUserPointer(Branch),DownUserPointer(Branch)
            J = J + 1
            Streamflow(J) = UserQ(I)
        end do

        return
    end subroutine

    !== Public (GetUserStreamSurfaceElevation) =============================

    subroutine GetUserStreamSurfaceElevation( &
        Num, &
        StreamElevation &
        )
        use chnluser
        implicit none

        !   Purpose:  Return an array of initial water surface elevations at
        !             user locations within the current channel.  Dimension of
        !             returned array must be large enough and is not
        !             checked by this routine.

        !   Arguments:
        integer :: Num
        real*8 :: StreamElevation(Num)

        !   Argument definitions:
        !     Num - dimension of identifier array.
        !     StreamElevation(i) - array of stream-surface-elevation values.


        !   Local Variables:
        integer :: I
        integer :: J

        !   Routines by module:

        !   Programmed by: Lew DeLong
        !   Date:          January 1991
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        J = 0
        do I=UpUserPointer(Branch),DownUserPointer(Branch)
            J = J + 1
            StreamElevation(J) = UserWS(I)
        end do

        return
    end subroutine

    !== Public (UpstreamCode) ==============================================

    integer function UpstreamCode()
        use chconnec
        implicit none

        !   Purpose:  Return upstream boundary condition code for the current channel.

        !   Arguments:

        !   Argument definitions:


        !   Local Variables:

        !   Routines by module:

        !   Programmed by: Lew DeLong
        !   Date:          February 1991
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        UpstreamCode = UpBoundaryCode(Branch)

        return
    end function

    !== Public (UpstreamConnections) =======================================

    integer function UpstreamConnections()
        use chconnec
        implicit none

        !   Purpose:  Return number of connections to upstream end
        !             of current channel.

        !   Arguments:

        !   Argument definitions:


        !   Local Variables:

        !   Routines by module:

        !   Programmed by: Lew DeLong
        !   Date:          February 1991
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        UpstreamConnections = UpNumberOfConnections(Branch)

        return
    end function

    !== Public (DownstreamCode) ============================================

    integer function DownstreamCode()
        use chconnec
        implicit none

        !   Purpose:  Return downstream boundary condition code
        !             for the current channel.

        !   Arguments:

        !   Argument definitions:


        !   Local Variables:

        !   Routines by module:

        !   Programmed by: Lew DeLong
        !   Date:          February 1991
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        DownstreamCode = DownBoundaryCode(Branch)

        return
    end function

    !== Public (DownstreamConnections) =====================================

    integer function DownstreamConnections()
        use chconnec
        implicit none

        !   Purpose:  Return number of connections to downstream end
        !             of current channel.

        !   Arguments:

        !   Argument definitions:

        !   Local Variables:

        !   Routines by module:

        !   Programmed by: Lew DeLong
        !   Date:          February 1991
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        DownstreamConnections = DownNumberOfConnections(Branch)

        return
    end function

    !== Public (UpstreamConnect) ==========================================

    integer function UpstreamConnect(I)
        use chconnec
        implicit none

        !   Purpose:  Return channel number of the Ith upstream connection to the
        !             current channel.  The number is positive if is the downstream
        !             end of the connecting channel that is connected, negative if
        !             it is the upstream end.

        !   Arguments:
        integer :: I

        !   Argument definitions:
        !     I - sequential number of connection.

        !   Local Variables:

        !   Routines by module:

        !   Programmed by: Lew DeLong
        !   Date:          February 1991
        !   Modified by:   Lew DeLong
        !   Last modified: August   1992
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        UpstreamConnect = UpConnection( &
            (Branch-1) * MaxConnectingChannels + I &
            )

        return
    end function

    !== Public (DownstreamConnect) ========================================

    integer function DownstreamConnect(I)
        use chconnec
        implicit none

        !   Purpose:  Return channel number of the Ith downstream connection to the
        !             current channel.  The number is positive if is the downstream
        !             end of the connecting channel that is connected, negative if
        !             it is the upstream end.

        !   Arguments:
        integer :: I

        !   Argument definitions:
        !     I - sequential number of connection.


        !   Local Variables:

        !   Routines by module:

        !   Programmed by: Lew DeLong
        !   Date:          February 1991
        !   Modified by:   Lew DeLong
        !   Last modified: August   1992
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        DownstreamConnect = DownConnection( &
            (Branch-1) * MaxConnectingChannels + I &
            )

        return
    end function


    !== Public (CompPointAt) ========================================

    subroutine CompPointAtDist(intchan, distance,iup,idown,wt_up,wt_down)

        !-----Purpose: Locate the global computation points bracketing a given
        !     channel distance
        use grid_data
        use chnlcomp
        implicit none

        !   Arguments:
        integer:: intchan  ! Channel where comp point is being requested
        real*8:: distance     ! Downstream distance along intchan
        integer:: iup      ! (Global) index of point at or immediately upst of dist
        integer:: idown    ! (Global) index of point at or immediately downst of dist
        real*8:: wt_down  ! fractional distance between iup and idown of dist
        real*8:: wt_up

        !   Locals
        real*8:: xup
        real*8::xdown
        integer:: mid     ! index for bisection

        iup=UpCompPointer(intchan)
        idown=DownCompPointer(intchan)
        !----- ends of channel are very likely special cases
        if (abs(distance)< 1.D-5) then
            idown=iup
            wt_down=0.D0
            wt_up=1.D0
            return
        else if (abs(distance - chan_geom(intchan)%length) < 1.D-5) then
            iup=idown
            wt_down=1.D0
            wt_up=0.D0
            return
        end if


        !---- variation on binary search to bracket requested distanceance by bisection
        !-----note that for most
        do while ((idown - iup) > 1)
            mid = (iup+idown)/2;            ! integer divide, rounds down
            if (distance < CompLocation(mid)) then
                if (distance == CompLocation(iup)) then
                    idown=iup
                    exit
                else
                    idown=mid
                end if
            else if (distance > CompLocation(mid)) then
                if (distance == CompLocation(idown)) then
                    iup=idown
                    exit
                else
                    iup = mid
                end if
            else            !(distance .eq. CompLocation(mid))
                iup=mid
                idown=mid
                exit
            end if
        end do
        xup=CompLocation(iup)
        xdown=CompLocation(idown)
        wt_down=(distance-xup)/(xdown-xup)
        wt_up=1.D0-wt_down
        return
    end subroutine


end module
!==== EOF chschmt =========================================================
