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


*==== BOF chschmt ======================================================

*****************************************************************************
*
*     This file is a FORTRAN module containing schematic data associated with
*     networks of 1-D channels.
*
*
*
*     Module note: Only functions or subroutines marked "public" should be
*                   used outside of this module as those marked "private"
*                   may not be supported by future revisions of this module
*                   or replacement modules.  Likewise, no data or common
*                   blocks contained within this module should be accessed
*                   by routines outside of this module accept through the
*                   use of "public" functions or subroutines contained
*                   within this module.
*
*     Non-standard usage: Symbolic names in this module may be represented by
*                         as many as 31 characters in order to provide better
*                         definition directly in the code.  Standard FORTRAN
*                         allows only 6 characters, but this restriction is
*                         generally extended to 32 characters by most compilers.
*
*
*
*   Public functions:
*     INTEGER FUNCTION NumberOfChannels()
*            - return number of channels in the network.
*
*     INTEGER FUNCTION CurrentChannel()
*            - return the current channel number.
*
*     LOGICAL FUNCTION OpenChannel(ChannelNumber)
*            - open a channel making it the current channel.
*
*     LOGICAL FUNCTION CloseChannel()
*            - close the current channel.
*
*     INTEGER FUNCTION NumberOfUserStreamLocations()
*            - return number of user-supplied locations in current channel.
*
*     LOGICAL FUNCTION InitializeChannelNetwork()
*            - initialize a network of open channels.
*
*     INTEGER FUNCTION NumberOfStreamLocations()
*            - return number of computational locations in current channel.
*
*     INTEGER FUNCTION UpstreamPointer()
*            - return global sequence number for first computational
*              location in current channel.
*
*     INTEGER FUNCTION DownstreamPointer()
*            - return global sequence number for last computational
*              location in current channel.
*
*     REAL*8 FUNCTION StreamDistance(LocationNumber)
*            - return downstream distance of a computational location.
*
*     INTEGER FUNCTION UpstreamCode()
*            - return upstream boundary condition code.
*
*     INTEGER FUNCTION DownstreamCode()
*            - return downstream boundary condition code.
*
*     INTEGER FUNCTION UpstreamConnections()
*            - return number of upstream connections.
*
*     INTEGER FUNCTION DownstreamConnections()
*            - return number of downstream connections.
*
*     INTEGER FUNCTION UpstreamConnect1()
*            - return channel number of first upstream connection.
*
*     INTEGER FUNCTION UpstreamConnect2()
*            - return channel number of second upstream connection.
*
*     INTEGER FUNCTION UpstreamConnect3()
*            - return channel number of third upstream connection.
*
*     INTEGER FUNCTION DownstreamConnect1()
*            - return channel number of first downstream connection.
*
*     INTEGER FUNCTION DownstreamConnect2()
*            - return channel number of second downstream connection.
*
*     INTEGER FUNCTION DownstreamConnect3()
*            - return channel number of third downstream connection.
*
*   Public Subroutines:
*
*     SUBROUTINE GetUserStreamLocationIDs(in: Num, out: CurrentUserIDs)
*            - returns an array of user location IDs for current channel.
*
*     SUBROUTINE GetUserStreamFlow(in: Num, out: StreamFlow)
*            - return an array of streamflow values for user locations
*              in current channel.
*
*     SUBROUTINE GetUserStreamSurfaceElevation(in: Num, out: StreamElevation)
*            - return an array of watresurface elevations at user locations
*              in current channel.
*
*   Arguments:
*     ChannelNumber - channel sequence number.
*     LocationNumber - sequential location number within current channel,
*                      1 beginning at upstream most location.
*     Num - an integer count of the elements in an array.
*
*         corresponding to user-supplied locations, arrays of user-supplied ...
*     CurrentUserIDs - identifiers.
*     StreamFlow - discharges.
*     StreamElevation - watersurface elevations.
*
*
*   Module data:
*
*    'network.inc'
*     MaxChannels - maximum number of channels.
*     NumCh - current number of channels.
*     Branch - current selected or active channel.
*     MaxLocations - maximum number of locations (computational or user).
*
*    'chnluser.inc'
*     NumUserLoc - current total number of user locations.
*     UpUserPointer(i) - pointer to upstream most user-supplied
*                        location for channel "i".
*     DownUserPointer(i) - pointer to downstream most user-supplied
*                          location for channel "i".
*     NumberOfUserLocations(i) - number of user-supplied locations
*                              for channel "i".
*     UserWS(i) - water surface elevation at user location "i".
*     UserQ(i) - volumetric discharge at user location "i".
*     UserH(i) - depth of flow at user location "i".
*
*    'chconnec.inc'
*        for the current channel i ...
*     UpBoundaryCode(i) - upstream boundary-condition code.
*     UpNumberOfConnections(i) - number of channels connected to upstream end.
*     UpConnection((i-1)*J+k) - number of kth channel connected to
*                               upstream end,
*                               where j = MaxConnectingChannels.
*     DownBoundaryCode(i) - downstream boundary-condition code.
*                           (See codes below.)
*     DownNumberOfConnections(i) - number of channels connected to
*                                  downstream end.
*     DownConnection((i-1)*J+k) - number of kth channel connected to
*                                 downstream end,
*                                 where j = MaxConnectingChannels.
*     InitialApprox(i) - initial approximation index, indicating approximation
*                        of intial conditions from:
*              [0] user input (schematic data file),
*              [1] normal depth,
*              [2] steady state,
*              [3] normal depth w/adverse slopes removed,
*              [4] maximum ws elevation for branch,
*              [5] model dependent file, or
*              [6] no approximation attempted.
*
*      AdditionalVariables(i) - number of variables in addition to watersurface
*                               elevation and discharge (not currently used).
*      dX(i) - desired spacing of computational locations.
*      KeepAllCx(i) - index indicating,
*               [.TRUE.] - keep all user-supplied locations as computational locations.
*               [.FALSE.] - do not keep intermediate user-supplied locations as
*                           computational locations.
*
*
*     'chnlcomp.inc'
*           for the current channel, i,
*     NumberOfCompLocations(i) - number of computational locations.
*     UpCompPointer(i) - global sequence number of most upstream location.
*     DownCompPointer(i) - global sequence number of most downstream location.
*            for the global sequence number, j,
*     CompLocation(j) - downstream distance coordinate.
*     DummyArray(j) - a REAL value dependent upon context.
*     DummyArray2(j) - a REAL value dependent upon context.
*     DummyCharArray(j) - a CHARACTER value dependent upon context.
*
*
************************************************************************

*== Public (NumberOfChannels) ==========================================

      INTEGER FUNCTION NumberOfChannels()

      IMPLICIT NONE

*   Purpose:  Return number of channels in current channel network.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'

      include '../input/fixed/misc.f'

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

      IMPLICIT NONE

*   Purpose:  Return current channel number.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'

      include '../input/fixed/misc.f'

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

      IMPLICIT NONE

*   Purpose:  Set current channel number.

*   Arguments:
      INTEGER ChannelNumber

*   Argument definitions:
*      ChannelNumber - channel sequence number.

*   Module data:
      INCLUDE 'network.inc'

      include '../input/fixed/common.f'

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
            WRITE(UNIT_ERROR,*) ' Can not open channel', int2ext(ChannelNumber)
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

*== Public (NumberOfUserStreamLocations) ===============================

      INTEGER FUNCTION NumberOfUserStreamLocations()

      IMPLICIT NONE

*   Purpose:  Return number of user-supplied stream locations.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chconnec.inc'
      INCLUDE 'chnluser.inc'

      include '../input/fixed/misc.f'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      IF( Branch .GT. 0 ) THEN
         NumberOfUserStreamLocations = NumberOfUserLocations(Branch)
      ELSE
         WRITE(UNIT_ERROR,*)
     &        ' CurrentChannel not set (NumberOfUserStreamLocations)...'
         CALL EXIT(1)
      END IF

      RETURN
      END

*== Public (InitializeChannelNetwork) ==================================

      LOGICAL FUNCTION InitializeChannelNetwork()

      IMPLICIT NONE

*   Purpose:  Initialize a network of channels.

*   Arguments:

*   Argument definitions:

*   Module data:

      include '../input/fixed/misc.f'

*   Local Variables:
      LOGICAL OK

*   Routines by module:

***** Network control:
      LOGICAL  VariableStreamDensity, VariableStreamSinuosity
      INTEGER  Terms1D
      EXTERNAL VariableStreamDensity, VariableStreamSinuosity, Terms1D

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
      ELSE IF( Terms1D() .EQ. 1
     &        .AND.
     &        VariableStreamSinuosity() ) THEN
         OK = SetConstantStreamDensity()
      END IF

      InitializeChannelNetwork = .TRUE.

      RETURN
      END



*== Private (ReadChannelCxSequence) ====================================

      LOGICAL FUNCTION ReadChannelCxSequence(Unit)

      IMPLICIT NONE

*   Purpose:  Read user-supplied schematic data
*             and approximate initial conditions.

*   Arguments:
      INTEGER Unit

*   Argument definitions:
*     Unit - FORTRAN unit number for input.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chnluser.inc'
      INCLUDE 'chconnec.inc'

      include '../input/fixed/misc.f'

*   Local Variables:
      INTEGER BRN, M, N, I, CxNum
      INTEGER Keep, Approx
      LOGICAL Out

*   Routines by module:

***** Network control:
      INTEGER  NetworkPrintLevel
      EXTERNAL NetworkPrintLevel

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      ReadChannelCxSequence = .FALSE.

*-----Read channel number, number of user-defined cross sections,
*       desired spacing for computational cross sections,
*       insertion index, initial-condition approximation index, and
*       the number of initial-value types in addition to discharge and
*       water-surface elevation (perhaps density or constituent
*       concentrations to be simulated).

      N = Unit

*-----Set output index.

      IF( NetworkPrintLevel() .GT. 1 ) THEN
         Out = .TRUE.
      ELSE
         Out = .FALSE.
      END IF

*-----Check for out-of-range on number of channels.
      IF( NumCh .LT. MaxChannels ) THEN
         M = NumCh + 1
         Keep  = 1
         InitialApprox(M) = 0
         AdditionalVariables(M) = 0

*--------Read channel number, jump out if EOF.
         READ(N,*, END=200 ) BRN
         IF(BRN.NE.M) THEN
            WRITE(UNIT_ERROR,*) ' ****Channel-number-sequence error...'
            WRITE(UNIT_ERROR,*) BRN,' changed to ',M,'...'
         END IF
         READ(N,*) NumberOfUserLocations(M),dX(M),Keep,InitialApprox(M),
     &        AdditionalVariables(M)
         IF( Out ) THEN
            WRITE(UNIT_ERROR,*) ' '
            WRITE(UNIT_ERROR,*) ' Channel ',M,'...'
            WRITE(UNIT_ERROR,*) '  ',NumberOfUserLocations(M),
     &           ' stream locations...'
            WRITE(UNIT_ERROR,*) '   dX = ',dX(M)
         END IF
         IF(Keep.EQ.0) THEN
            KeepAllCx(M) = .FALSE.

            IF( Out ) THEN
               WRITE(UNIT_ERROR,*) '   Intermediate user locations will not be ',
     &              'included as computational locations...'
            END IF

         ELSE
            KeepAllCx(M) = .TRUE.

            IF( Out ) THEN
               WRITE(UNIT_ERROR,*) '   All user locations will be included as',
     &              ' computational locations...'
            END IF
         END IF

         Approx = InitialApprox(M)

         IF(Approx .EQ. 0) THEN
            IF( Out ) THEN
               WRITE(UNIT_ERROR,*) '   Initial conditions are to be approximated ',
     &              'from user input...'
            END IF
         ELSE IF(Approx .EQ. 1) THEN
            IF( Out ) THEN
               WRITE(UNIT_ERROR,*) '   Initial ws_elev is to be approximated',
     &              ' by normal depth...'
            END IF
         ELSE IF(Approx .EQ. 2) THEN
            IF( Out ) THEN
               WRITE(UNIT_ERROR,*) '   Initial ws_elev is to be computed',
     &              ' assuming steady state...'
               WRITE(UNIT_ERROR,*) '    (not implemented)...'
            END IF
         ELSE IF(Approx .EQ. 3) THEN
            IF( Out ) THEN
               WRITE(UNIT_ERROR,*) '  Initial ws_elev is to be approximated',
     &              ' by normal depth...'
               WRITE(UNIT_ERROR,*) '   Adverse slopes are to be flattened...'
            END IF
         ELSE IF(Approx .EQ. 4) THEN
            IF( Out ) THEN
               WRITE(UNIT_ERROR,*) '   Initial ws_elev is to be approximated',
     &              ' equal to maximum for channel...'
            END IF
         ELSE IF(Approx .EQ. 5) THEN
            IF( Out ) THEN
               WRITE(UNIT_ERROR,*) '   Initial conditions are to be approximated ',
     &              'from model-dependent file...'
            END IF
         ELSE IF(Approx .EQ. 6) THEN
            IF( Out ) THEN
               WRITE(UNIT_ERROR,*) '   Initial conditions are',
     &              'not to be approximated... '
            END IF

         ELSE IF( Approx .EQ. 7 ) THEN
            IF( Out ) THEN
               WRITE(UNIT_ERROR,*) '   Initial conditions are to be approximated',
     &              ' assuming steady state...'
            END IF
         ELSE
            IF( Out ) THEN
               WRITE(UNIT_ERROR,*) '   Initial-condition index ',Approx,
     &              ' not supported...'
            END IF
         END IF

*--------Begin cross-section loop, stop if EOF.
         UpUserPointer(M) = NumUserLoc + 1
         DO 100 I = 1, NumberOfUserLocations(M)
            NumUserLoc = NumUserLoc + 1
            CxNum = NumUserLoc

*-----------Read cross-section ID.
            READ(N,'(A16)', END=150) UserLocationID(CxNum)

*-----------Read user cross-section number, reference distance,
*         initial ws_elev, and initial flow.
            UserWS(CxNum) = 0.0
            UserQ(CxNum)  = 0.0
            UserH(CxNum)  = 1.0E-08
            READ(N,*, END=150)  UserWS(CxNum), UserQ(CxNum), UserH(CxNum)

 100     CONTINUE
         DownUserPointer(M) = CxNum

         NumCh = NumCh + 1
         Branch = NumCh

         ReadChannelCxSequence = .TRUE.

         RETURN

      ELSE
         WRITE(UNIT_ERROR,*) ' Too many channels for dimension...',
     &        '(ReadChannelCxSequence)'
         WRITE(UNIT_ERROR,*) ' Attempt to read ',NumCh+1
         WRITE(UNIT_ERROR,*) ' Dimensioned for',MaxChannels
         CALL EXIT(1)
      END IF

 150  CONTINUE
      WRITE(UNIT_ERROR,*) ' End of file encountered prematurely...',
     &     ' (ReadChannelCxSequence)'
      WRITE(UNIT_ERROR,*) ' Channel',M,'  location',I
      CALL EXIT(1)
 200  CONTINUE

      RETURN
      END

*== Private (ReadChannelConnections) ===================================

      LOGICAL FUNCTION ReadChannelConnections(Unit)

      IMPLICIT NONE

*   Purpose:  Read user-supplied schematic data
*             and approximate initial conditions.

*   Arguments:
      INTEGER Unit

*   Argument definitions:
*     Unit - FORTRAN unit number for input.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chnluser.inc'
      INCLUDE 'chconnec.inc'

      include '../input/fixed/misc.f'

*   Local Variables:
      INTEGER I, J, K, Connections
      LOGICAL Out

*   Routines by module:

***** Network control:
      INTEGER  NetworkPrintLevel
      EXTERNAL NetworkPrintLevel

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      ReadChannelConnections = .FALSE.

*-----Set output index.

      IF( NetworkPrintLevel() .GT. 1 ) THEN
         Out = .TRUE.
      ELSE
         Out = .FALSE.
      END IF

*-----Read connection and condition codes.
      I = Branch
      K = (I - 1) * MaxConnectingChannels

*-----Upstream...

      READ(Unit,*,END=999) UpBoundaryCode(I)
      READ(Unit,*,END=999) Connections
      IF( Out ) THEN
         WRITE(UNIT_ERROR,*) '   Upstream condition code = ',
     &        UpBoundaryCode(I),
     &        ' with',Connections,' connection(s) ...'
      END IF
      UpNumberOfConnections(I) = Connections
      IF(Connections .EQ. 0) THEN
      ELSE IF(Connections .LE. MaxConnectingChannels) THEN

         DO 100 J=1,Connections
            READ(Unit,*,END=999) UpConnection(K+J)
            IF( Out ) THEN
               WRITE(UNIT_ERROR,*) '    ...  ',UpConnection(K+J)
            END IF
 100     CONTINUE

      ELSE
         WRITE(UNIT_ERROR,*)
     &        ' Number of upstream connections (',Connections,'),'
         WRITE(UNIT_ERROR,*) ' for channel',I
         WRITE(UNIT_ERROR,*) ' are <0 or >',MaxConnectingChannels,' .'
         CALL EXIT(1)
      END IF

*-----Downstream...

      READ(Unit,*,END=999) DownBoundaryCode(I)
      READ(Unit,*,END=999) Connections
      IF( Out ) THEN
         WRITE(*,*) '   Downstream condition code = ',
     &        DownBoundaryCode(I),
     &        ' with',Connections,' connection(s) ...'
      END IF
      DownNumberOfConnections(I) = Connections
      IF(Connections .EQ. 0) THEN
      ELSE IF(Connections .LE. MaxConnectingChannels) THEN
         DO 150 J=1,Connections
            READ(Unit,*,END=999) DownConnection(K+J)
            IF( Out ) THEN
               WRITE(*,*) '    ...  ',DownConnection(K+J)
            END IF
 150     CONTINUE
      ELSE
         WRITE(UNIT_ERROR,*)
     &        ' Number of downstream connections (',Connections,'),'
         WRITE(UNIT_ERROR,*) ' for channel',I
         WRITE(UNIT_ERROR,*) ' are <0 or >',MaxConnectingChannels,' .'
         CALL EXIT(1)
      END IF
 220  CONTINUE

      IF( Out ) THEN
         WRITE(*,*) ' '
      END IF

      ReadChannelConnections = .TRUE.

      RETURN

 999  CONTINUE
      WRITE(UNIT_ERROR,*) ' Reached end of channel schematic data file too soon !'

      RETURN
      END

*== Public (SetCompLocations) ==========================================

      LOGICAL FUNCTION SetCompLocations()

      IMPLICIT NONE

*   Purpose:

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chnluser.inc'
      INCLUDE 'chnlcomp.inc'
      INCLUDE 'chconnec.inc'

      include '../input/fixed/misc.f'

*   Local Variables:
      INTEGER I, ChNum, MaxCx, ModelIndex
      LOGICAL OK, Out

*   Routines by module:

***** Channel properties:
      EXTERNAL UserStreamLocations

***** Linear interpolation utilities:
      LOGICAL  XINSRT
      EXTERNAL XINSRT

***** Network control:
      LOGICAL  SetNetworkTimeSeriesLocations
      EXTERNAL SetNetworkTimeSeriesLocations

      INTEGER  NetworkPrintLevel
      EXTERNAL NetworkPrintLevel

***** Local:
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

*-----------Load user downstream distances in DummyArray.
            CALL UserStreamLocations(
     &           MaxCx,
     &           DummyCharArray, DummyArray
     &           )

*-----------Insert computational locations if needed.
            IF( ChNum .GT. 1 ) THEN
               UpCompPointer(ChNum) = DownCompPointer(ChNum-1) + 1
            ELSE
               UpCompPointer(ChNum) = 1
            END IF
            IF( XINSRT(
     &           MaxCx, NumberOfUserLocations(ChNum),
     &           DummyArray,
     &           dX(ChNum), KeepAllCx(ChNum), ModelIndex,
     &           NumberOfCompLocations(ChNum),
     &           CompLocation( UpCompPointer(ChNum) )
     &           )  )  THEN

               IF( Out ) THEN
                  WRITE(*,*) '  Channel number ',ChNum,' ...',
     &                 NumberOfCompLocations(ChNum)
               END IF

               TotalCompLocations = TotalCompLocations
     &              + NumberOfCompLocations(ChNum)

            ELSE
               WRITE(UNIT_ERROR,*) ' ####error(SetCompLocations)'
               WRITE(UNIT_ERROR,*) ' Maximum number of computational',
     &              'locations eceeded...'
               WRITE(UNIT_ERROR,*) ' Insertion of computational locations failed...'
               WRITE(UNIT_ERROR,*) ' Attempting channel...',ChNum
               RETURN
            END IF

            DownCompPointer(ChNum) = UpCompPointer(ChNum)
     &           + NumberOfCompLocations(ChNum) - 1

*-----------Reduce maximum number of remaining locations.
            MaxCx = MaxCx - NumberOfCompLocations(ChNum)
            IF( MaxCx .LT. 0 ) THEN
               WRITE(UNIT_ERROR,*) ' Max number of computational locations',
     &              ' exceeded...(SetCompLocations)'
               WRITE(UNIT_ERROR,*) ' Channel number...',ChNum
            END IF

            OK = CloseChannel()

         ELSE

            WRITE(UNIT_ERROR,*) ' Attempt to open channel',ChNum,' failed...'
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

      include '../input/fixed/misc.f'

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

      include '../input/fixed/misc.f'

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

      include '../input/fixed/misc.f'

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

*== Public (KeepCx) ====================================================

      LOGICAL FUNCTION KeepCx()

      IMPLICIT NONE

*   Purpose: Get an index for including or not including user cross
*            sections intermediate to branch extremity cross sections
*            as computational cross sections:
*              [.TRUE.] include all cross sections.
*              [.FALSE.] include only branch extremity cross sections.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chconnec.inc'

*   Local variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          January 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      KeepCx = KeepAllCx(Branch)

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

*==== EOF chschmt =========================================================
