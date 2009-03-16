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

*==== BOF chschmt ======================================================

*****************************************************************************
*
*     This file is a FORTRAN module containing schematic data associated with
*     networks of 1-D channels.
*
*
*
*
*
*****************************************************************

*== Public (NumberOfChannels) ==========================================

      INTEGER FUNCTION NumberOfChannels()
      use IO_Units
      IMPLICIT NONE

*   Purpose:  Return number of channels in current channel network.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'


*   Local Variables:

*   Routines by modules:

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation ---------------------------------------------------

      IF( NumCh .GT. 0 ) THEN
         NumberOfChannels = NumCh
      ELSE
         WRITE(UNIT_ERROR,*) ' Network not initialized (NumberOfChannels)...'
         CALL EXIT(1)
      END IF

      RETURN
      END

*== Public (CurrentChannel) ============================================

      INTEGER FUNCTION CurrentChannel()
      use IO_Units
      use grid_data
      IMPLICIT NONE

*   Purpose:  Return current channel number.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'


*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation ---------------------------------------------------

      IF( Branch .GT. 0 ) THEN
         CurrentChannel = Branch
      ELSE
         WRITE(UNIT_ERROR,*) ' CurrentChannel not set (CurrentChannel)...'
         CALL EXIT(1)
      END IF

      RETURN
      END

*== Public (OpenChannel) ===============================================

      LOGICAL FUNCTION OpenChannel(ChannelNumber)
      use IO_Units
      use grid_data
      IMPLICIT NONE

*   Purpose:  Set current channel number.

*   Arguments:
      INTEGER ChannelNumber

*   Argument definitions:
*      ChannelNumber - channel sequence number.

*   Module data:
      INCLUDE 'network.inc'


*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation ---------------------------------------------------

      IF( Branch .EQ. 0 ) THEN

         IF( ChannelNumber .LE. NumCh .AND. ChannelNumber .GT. 0 ) THEN
            OpenChannel = .TRUE.
            Branch = ChannelNumber
         ELSE
            OpenChannel = .FALSE.
            WRITE(UNIT_ERROR,*) ' Can not open channel', chan_geom(ChannelNumber).chan_no
            WRITE(UNIT_ERROR,*) ' NumberOfChannels = ', NumCh
         END IF

      ELSE
         WRITE(UNIT_ERROR,*) ' *** Error (OpenChannel)'
         WRITE(UNIT_ERROR,*) ' Channel ', Branch, ' still open ???'
         OpenChannel = .FALSE.
      END IF

      RETURN
      END

*== Public (CloseChannel) ==============================================

      LOGICAL FUNCTION CloseChannel()

      IMPLICIT NONE

*   Purpose:  Close current channel, set CurrentChannel = 0.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation ---------------------------------------------------

      IF( Branch .GT. 0 ) THEN
         CloseChannel = .TRUE.
         Branch = 0
      ELSE
         CloseChannel = .FALSE.
         Branch = 0
      END IF

      RETURN
      END



*== Public (InitializeChannelNetwork) ==================================

      LOGICAL FUNCTION InitializeChannelNetwork()
      use IO_Units
      IMPLICIT NONE

*   Purpose:  Initialize a network of channels.

*   Arguments:

*   Argument definitions:

*   Module data:


*   Local Variables:
      LOGICAL OK

*   Routines by module:

***** Network control:
      LOGICAL  VariableStreamDensity, VariableStreamSinuosity
      EXTERNAL VariableStreamDensity, VariableStreamSinuosity

***** Channel flow status:
      LOGICAL  InitializeNetworkFlowValues, InitializeNetworkDensity
      LOGICAL  SetConstantStreamDensity
      EXTERNAL InitializeNetworkFlowValues, InitializeNetworkDensity
      EXTERNAL SetConstantStreamDensity

***** Channel properties:
      LOGICAL  InitializeChannelProperties
      EXTERNAL InitializeChannelProperties
      LOGICAL  WriteInterveningProperties
      EXTERNAL WriteInterveningProperties

***** Locals:
      LOGICAL  SetCompLocations
      EXTERNAL  SetCompLocations

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      InitializeChannelNetwork = .FALSE.

*-----Read network schematic data.
c      IF( .not. InitializeNetworkSchematic() ) THEN
c         WRITE(UNIT_ERROR,*) ' Attempt to initialize network schematic failed...'
c         RETURN
c      END IF

*-----Determine computational locations and set channel pointers.
      IF( .not. SetCompLocations() ) THEN
         WRITE(UNIT_ERROR,*)
     &        ' Attempt to set computational locations failed...'
         RETURN
      ENDIF

*-----Set initial channel flow values.
      IF( .not. InitializeNetworkFlowValues() ) THEN
         WRITE(UNIT_ERROR,*) ' Attempt to set initial flow channel',
     &        ' values failed...'
         RETURN
      END IF

*-----Set initial water density.

      IF( VariableStreamDensity() ) THEN
      ELSE IF(VariableStreamSinuosity() ) THEN
         OK = SetConstantStreamDensity()
      END IF

      InitializeChannelNetwork = .TRUE.

      RETURN
      END






*== Public (SetCompLocations) ==========================================

      LOGICAL FUNCTION SetCompLocations()
      use IO_Units
      use grid_data
      IMPLICIT NONE

*   Purpose:

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chnluser.inc'
      INCLUDE 'chnlcomp.inc'
      INCLUDE 'chconnec.inc'


*   Local Variables:
      INTEGER I, ChNum, MaxCx, ModelIndex
      LOGICAL OK, Out

*   Routines by module:

***** Channel properties:


***** Linear interpolation utilities:
      LOGICAL  XINSRT
      EXTERNAL XINSRT

***** Network control:
      LOGICAL  SetNetworkTimeSeriesLocations
      EXTERNAL SetNetworkTimeSeriesLocations

      INTEGER  NetworkPrintLevel
      EXTERNAL NetworkPrintLevel

***** Local:
	real*8 XEnd(2)
	
      LOGICAL  OpenChannel, CloseChannel
      EXTERNAL OpenChannel, CloseChannel

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      SetCompLocations = .FALSE.
      IF( NetworkPrintLevel() .GT. 3) THEN
         WRITE(*,*) ' Setting computational locations...'
      END IF

*-----Set output index.

      IF( NetworkPrintLevel() .GE. 3 ) THEN
         Out = .TRUE.
      ELSE
         Out = .FALSE.
      END IF

      ModelIndex = 1
      MaxCx = MaxLocations
      TotalCompLocations = 0

*-----Begin channel loop.
      DO 100 I = 1, NumCh
         ChNum = I

         IF( OpenChannel( ChNum ) ) THEN


*-----------Insert computational locations if needed.
            IF( ChNum .GT. 1 ) THEN
               UpCompPointer(ChNum) = DownCompPointer(ChNum-1) + 1
            ELSE
               UpCompPointer(ChNum) = 1
            END IF
            XEnd(1)=0.
		  XEnd(2)=chan_geom(branch).length
            IF( XInsrt(
     &           MaxCx, 
     &           2,XEnd, !(user points are ignored for generating comp. points)
     &           dX(ChNum), .false., ModelIndex,
     &           NumberOfCompLocations(ChNum),
     &           CompLocation( UpCompPointer(ChNum) )
     &           )  )  THEN

               IF( Out ) THEN
                  WRITE(*,*) '  Channel number ',chan_geom(ChNum).chan_no,' # points:',
     &                 NumberOfCompLocations(ChNum)
               END IF

               TotalCompLocations = TotalCompLocations
     &              + NumberOfCompLocations(ChNum)

            ELSE
               WRITE(UNIT_ERROR,*) ' ####error(SetCompLocations)'
               WRITE(UNIT_ERROR,*) ' Maximum number of computational ',
     &              'locations exceeded...'
               WRITE(UNIT_ERROR,*) ' Insertion of computational locations failed...'
               WRITE(UNIT_ERROR,*) ' Attempting channel...',chan_geom(ChNum).chan_no
               RETURN
            END IF

            DownCompPointer(ChNum) = UpCompPointer(ChNum)
     &           + NumberOfCompLocations(ChNum) - 1

*-----------Reduce maximum number of remaining locations.
            MaxCx = MaxCx - NumberOfCompLocations(ChNum)
            IF( MaxCx .LT. 0 ) THEN
               WRITE(UNIT_ERROR,*) ' Max number of computational locations',
     &              ' exceeded...(SetCompLocations)'
               WRITE(UNIT_ERROR,*) ' Channel number...',chan_geom(ChNum).chan_no
            END IF

            OK = CloseChannel()

         ELSE

            WRITE(UNIT_ERROR,*) ' Attempt to open channel',chan_geom(ChNum).chan_no,' failed...'
            WRITE(UNIT_ERROR,*) ' (SetCompLocations)'
            RETURN

         END IF

 100  CONTINUE

*-----Determine global, computational-cross-section number
*       of locations at which output of time-series results
*       is required.
c      OK = SetNetworkTimeSeriesLocations()

      SetCompLocations = .TRUE.

      RETURN
      END

*== Public (NumberOfStreamLocations) ===================================

      INTEGER FUNCTION NumberOfStreamLocations()

      IMPLICIT NONE

*   Purpose:  Return the number of computational locations in the
*             current channel.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chnlcomp.inc'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      NumberOfStreamLocations = NumberOfCompLocations(Branch)

      RETURN
      END

*== Public (TotalStreamLocations) ======================================

      INTEGER FUNCTION TotalStreamLocations()

      IMPLICIT NONE

*   Purpose:  Return total number of computational locations in network.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chnlcomp.inc'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      TotalStreamLocations = TotalCompLocations

      RETURN
      END

*== Public (StreamEndNode) =============================================

      INTEGER FUNCTION StreamEndNode( ChannelNumber )

      IMPLICIT NONE

*   Purpose:  Return a global location number delimiting the channel,
*             ChannelNumber.  The location number will be for the
*             upstream end of the channel if ChannelNumber is
*             positive and downstream if negative.  The returned
*             location number will carry the same sign as ChannelNumber.

*   Arguments:
      INTEGER ChannelNumber

*   Argument definitions:
*      ChannelNumber - channel sequence number.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chnlcomp.inc'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      IF( ChannelNumber .GT. 0 ) THEN

*--------Upstream end of channel.

         StreamEndNode = UpCompPointer( ChannelNumber )

      ELSE IF( ChannelNumber .LT. 0 ) THEN

*--------Downstream end of channel.

         StreamEndNode = - DownCompPointer(  - ChannelNumber )

      ELSE

*--------No channel number.

         StreamEndNode = 0

      END IF

      RETURN
      END

*== Public (UpstreamPointer) ===========================================

      INTEGER FUNCTION UpstreamPointer()

      IMPLICIT NONE

*   Purpose:  Return pointer to upstream computational location number
*             for current channel.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chnlcomp.inc'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      UpstreamPointer = UpCompPointer(Branch)

      RETURN
      END

*== Public (DownstreamPointer) =========================================

      INTEGER FUNCTION DownstreamPointer()

      IMPLICIT NONE

*   Purpose:  Return pointer to downstream computational location number
*             for current channel.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chnlcomp.inc'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      DownstreamPointer = DownCompPointer(Branch)

      RETURN
      END

*== Public (StreamDistance) ============================================

      REAL*8 FUNCTION StreamDistance(LocationNumber)
      use IO_Units
      IMPLICIT NONE

*   Purpose:  Return downstream distance to the computational
*             LocationNumber, within the current channel.

*   Arguments:
      INTEGER LocationNumber

*   Argument definitions:
*     LocationNumber - computational location number, begining with 1
*                       at upstream end of current channel.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chnlcomp.inc'


*   Local Variables:

*   Routines by module:

***** Locals:
      LOGICAL  CheckChannelCompLocationRange
      EXTERNAL CheckChannelCompLocationRange

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      IF( CheckChannelCompLocationRange(LocationNumber) ) THEN

         StreamDistance = CompLocation(
     &        UpCompPointer(Branch) + LocationNumber - 1
     &        )

      ELSE

         WRITE(UNIT_ERROR,*) ' Failed to determine correct stream distance...'
         WRITE(UNIT_ERROR,*) ' Incorrectly returned last location in channel...'
         WRITE(UNIT_ERROR,*) ' (StreamDistance)'
         StreamDistance = CompLocation(
     &        DownCompPointer(Branch)
     &        )

      END IF

      RETURN
      END

*== Public (GlobalStreamDistance) ======================================

      REAL*8 FUNCTION GlobalStreamDistance(GlobalLocationNumber)
      use IO_Units
      IMPLICIT NONE

*   Purpose:  Return downstream distance to the GlobalLocationNumber.

*   Arguments:
      INTEGER GlobalLocationNumber

*   Argument definitions:
*     GlobalLocationNumber - global location number, sequential
*                             with out regard to channel number.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chnlcomp.inc'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          March 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      IF( GlobalLocationNumber .GT. 0
     &     .AND.
     &     GlobalLocationNumber .LE. MaxLocations ) THEN
         GlobalStreamDistance = CompLocation( GlobalLocationNumber )
      ELSE
         WRITE(UNIT_ERROR,*) ' ####error( GlobalStreamDistance )'
         WRITE(UNIT_ERROR,*) ' Global location number out of range...'
         GlobalStreamDistance = 0.0
      END IF
      RETURN
      END

*== Private (CheckChannelCompLocationRange) ============================

      LOGICAL FUNCTION CheckChannelCompLocationRange(LocationNumber)
      use IO_Units
      IMPLICIT NONE

*   Purpose:  Check to see if LocationNumber is in the range of
*             location numbers available to the current channel.

*   Arguments:
      INTEGER LocationNumber

*   Argument definitions:
*     LocationNumber - computational-location number (index) within
*                      current channel.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chnlcomp.inc'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      IF( LocationNumber .LE. NumberOfCompLocations(Branch)
     &     .AND.
     &     LocationNumber .GT. 0 ) THEN

         CheckChannelCompLocationRange = .TRUE.

      ELSE

         WRITE(UNIT_ERROR,*) ' Location number out of range...'
         WRITE(UNIT_ERROR,*) ' Branch...',Branch
         WRITE(UNIT_ERROR,*) ' Requested location number = ',LocationNumber
         WRITE(UNIT_ERROR,*) ' Available range = 1',
     &        ' to',NumberOfCompLocations(Branch)
         CheckChannelCompLocationRange = .FALSE.

      END IF

      RETURN
      END


*== Public (GetUserStreamLocationIDs) ==================================

      SUBROUTINE GetUserStreamLocationIDs(
     &     Num,
     &     CurrentIDs
     &     )

      IMPLICIT NONE

*   Purpose:  Return an array of current  user stream-location
*             identifiers for the current channel.  Dimension of
*             returned identifier array must be large enough and is
*             not checked by this routine.

*   Arguments:
      INTEGER Num
      CHARACTER*16 CurrentIDs(Num)

*   Argument definitions:
*     Num - dimension of identifier array.
*     CurrentIDs(i) - array of identifiers.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chnluser.inc'

*   Local Variables:
      INTEGER I, J

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          January 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      J = 0
      DO 100 I=UpUserPointer(Branch),DownUserPointer(Branch)
         J = J + 1
         CurrentIDs(J) = UserLocationID(I)
 100  CONTINUE

      RETURN
      END

*== Public (GetUserStreamFlow) =========================================

      SUBROUTINE GetUserStreamFlow(
     &     Num,
     &     Streamflow
     &     )

      IMPLICIT NONE

*   Purpose:  Return an array of initial streamflow values at user
*             locations within the current channel.  Dimension of
*             returned array must be large enough and is not
*             checked by this routine.

*   Arguments:
      INTEGER Num
      REAL*8    Streamflow(Num)

*   Argument definitions:
*     Num - dimension of identifier array.
*     Streamflow(i) - array of streamflow values.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chnluser.inc'

*   Local Variables:
      INTEGER I, J

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          January 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      J = 0
      DO 100 I=UpUserPointer(Branch),DownUserPointer(Branch)
         J = J + 1
         Streamflow(J) = UserQ(I)
 100  CONTINUE

      RETURN
      END

*== Public (GetUserStreamSurfaceElevation) =============================

      SUBROUTINE GetUserStreamSurfaceElevation(
     &     Num,
     &     StreamElevation
     &     )

      IMPLICIT NONE

*   Purpose:  Return an array of initial water surface elevations at
*             user locations within the current channel.  Dimension of
*             returned array must be large enough and is not
*             checked by this routine.

*   Arguments:
      INTEGER Num
      REAL*8    StreamElevation(Num)

*   Argument definitions:
*     Num - dimension of identifier array.
*     StreamElevation(i) - array of stream-surface-elevation values.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chnluser.inc'

*   Local Variables:
      INTEGER I, J

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          January 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      J = 0
      DO 100 I=UpUserPointer(Branch),DownUserPointer(Branch)
         J = J + 1
         StreamElevation(J) = UserWS(I)
 100  CONTINUE

      RETURN
      END

*== Public (UpstreamCode) ==============================================

      INTEGER FUNCTION UpstreamCode()

      IMPLICIT NONE

*   Purpose:  Return upstream boundary condition code for the current channel.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chconnec.inc'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      UpstreamCode = UpBoundaryCode(Branch)

      RETURN
      END

*== Public (UpstreamConnections) =======================================

      INTEGER FUNCTION UpstreamConnections()

      IMPLICIT NONE

*   Purpose:  Return number of connections to upstream end
*             of current channel.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chconnec.inc'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      UpstreamConnections = UpNumberOfConnections(Branch)

      RETURN
      END

*== Public (DownstreamCode) ============================================

      INTEGER FUNCTION DownstreamCode()

      IMPLICIT NONE

*   Purpose:  Return downstream boundary condition code
*             for the current channel.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chconnec.inc'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      DownstreamCode = DownBoundaryCode(Branch)

      RETURN
      END

*== Public (DownstreamConnections) =====================================

      INTEGER FUNCTION DownstreamConnections()

      IMPLICIT NONE

*   Purpose:  Return number of connections to downstream end
*             of current channel.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chconnec.inc'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      DownstreamConnections = DownNumberOfConnections(Branch)

      RETURN
      END

*== Public (UpstreamConnect) ==========================================

      INTEGER FUNCTION UpstreamConnect(I)

      IMPLICIT NONE

*   Purpose:  Return channel number of the Ith upstream connection to the
*             current channel.  The number is positive if is the downstream
*             end of the connecting channel that is connected, negative if
*             it is the upstream end.

*   Arguments:
      INTEGER I

*   Argument definitions:
*     I - sequential number of connection.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chconnec.inc'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:   Lew DeLong
*   Last modified: August   1992
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      UpstreamConnect = UpConnection(
     &     (Branch-1) * MaxConnectingChannels + I
     &     )

      RETURN
      END

*== Public (DownstreamConnect) ========================================

      INTEGER FUNCTION DownstreamConnect(I)

      IMPLICIT NONE

*   Purpose:  Return channel number of the Ith downstream connection to the
*             current channel.  The number is positive if is the downstream
*             end of the connecting channel that is connected, negative if
*             it is the upstream end.

*   Arguments:
      INTEGER I

*   Argument definitions:
*     I - sequential number of connection.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chconnec.inc'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:   Lew DeLong
*   Last modified: August   1992
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      DownstreamConnect = DownConnection(
     &     (Branch-1) * MaxConnectingChannels + I
     &     )

      RETURN
      END


*== Public (CompPointAt) ========================================

      subroutine CompPointAtDist(intchan, distance,iup,idown,wt_up,wt_down)

*-----Purpose: Locate the global computation points bracketing a given
*     channel distance 
      use grid_data
      implicit none
	include 'network.inc'
      include 'chnlcomp.inc'

*   Arguments:
      integer :: intchan  ! Channel where comp point is being requested
	real*8  :: distance     ! Downstream distance along intchan
	integer :: iup      ! (Global) index of point at or immediately upst of dist
	integer :: idown    ! (Global) index of point at or immediately downst of dist
	real*8  :: wt_down  ! fractional distance between iup and idown of dist
      real*8  :: wt_up

*   Locals
      real*8 :: xup,xdown
	integer :: mid     ! index for bisection

	iup=UpCompPointer(intchan)
      idown=DownCompPointer(intchan)
c----- ends of channel are very likely special cases
	if (abs(distance).lt. 1.D-5) then
	   idown=iup
	   wt_down=0.D0
	   wt_up=1.D0
	   return
	else if (abs(distance - chan_geom(intchan).length) .lt. 1.D-5) then
         iup=idown
         wt_down=1.D0
	   wt_up=0.D0
	   return
	end if
	

c---- variation on binary search to bracket requested distanceance by bisection
c-----note that for most 
      do while ((idown - iup) .gt. 1)
         mid = (iup+idown)/2;	! integer divide, rounds down
         if (distance .lt. CompLocation(mid)) then
	      if (distance .eq. CompLocation(iup)) then
	          idown=iup
	          exit
	      else 
		      idown=mid
	      end if
         else if (distance .gt. CompLocation(mid)) then
	       if (distance .eq. CompLocation(idown)) then
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
	end



*==== EOF chschmt =========================================================
