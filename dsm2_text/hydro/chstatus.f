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

*==== BOF chstatus ======================================================

************************************************************************
*
*    This file is a FORTRAN module containing 1-D channel flow values,
*    including density, for networks of open channels.
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
*   Public functions:
*
*     REAL FUNCTION StreamFlow(LocationNumber)
*            - returns volumetric discharge.
*
*     LOGICAL FUNCTION SetStreamFlow(LocationNumber, Value)
*            - store volumetric discharge.
*
*     REAL FUNCTION StreamDepth(LocationNumber)
*            - return depth of flow.
*
*     LOGICAL FUNCTION SetStreamDepth(LocationNumber, Value)
*            - store depth of flow.
*
*     REAL FUNCTION StreamSurfaceElevation(LocationNumber)
*            - return watersurface elevation.
*
*     LOGICAL FUNCTION SetStreamSurfaceElevation(LocationNumber, Value)
*            - store watersurface elevation.
*
*     LOGICAL FUNCTION InitializeNetworkFlowValues()
*            - set initial flow values.
*
*      REAL FUNCTION OldStreamDensity(LocationNumber)
*             - returns water density at LocationNumber,
*               at the beginning of the current time step.
*
*      REAL FUNCTION NewStreamDensity(LocationNumber)
*             - returns water density at LocationNumber,
*               at the end of the current time step.
*
*      REAL FUNCTION EstOldStreamDensity(DownStreamDistance)
*             - returns interpolated density at DownStreamDistance,
*               at the begining of the current time step.
*
*      REAL FUNCTION EstOldStreamDensity(DownStreamDistance)
*             - returns interpolated density at DownStreamDistance,
*               at the begining of the current time step.
*
*      LOGICAL FUNCTION SetNewLinearStreamDensity(
*                         UpstreamDensity, DownstreamDensity
*                                             )
*             - sets density at computational locations, at the
*               end of the current time step.
*
*      LOGICAL FUNCTION SetConstantStreamDensity()
*             - sets stream density to 1.0.
*
*      LOGICAL FUNCTION SetNewLinearStreamDensity(
*                         UpstreamDensity, DownstreamDensity
*                                             )
*             - sets density at computational locations, at the
*               end of the current time step.
*
*
*   Arguments:
*     LocationNumber - computational location index, begining with
*                      1 in the current channel.
*     Value          - value of the specific variable to be stored.
*     UpstreamDensity - density at the upstream end of current channel.
*     DownstreamDensity - density at downstream end of current channel.
*     DownstreamDistance - distance from upstream end of current channel.
*     LocationNumber - computational cross-section sequence number,
*                       begining with 1 at upstream end of current
*                       channel.
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
*    'chstatus.inc'
*   Definitions:
*     WS(i) - water surface elevation at computational location "i".
*     Q(i) - volumetric discharge at computational location "i".
*     H(i) - depth of flow at computational location "i".
*     Rho(i) - density at computational location "i".
*
*
************************************************************************

*== Public (WriteNetworkRestartFile) ===================================

      LOGICAL FUNCTION WriteNetworkRestartFile()

      IMPLICIT NONE

*   Purpose:  Write current values of dependent and independent
*             variables to a file that may be used as initial
*             conditions to restart the model.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chstatus.inc'
      INCLUDE 'chconnec.inc'

      include '../input/fixed/common.f'
      include '../input/time-varying/tide.inc'

*   Local Variables:
      INTEGER      fUnit, I, J
      CHARACTER*130 FileName
      LOGICAL      OK
      real*8 reservoir_source_sink
      external reservoir_source_sink

*   Routines by module:

      INTEGER  NetworkTimeIncrement
      EXTERNAL NetworkTimeIncrement

***** Network control:
      LOGICAL  NetworkRestart
      EXTERNAL NetworkRestart

***** File utilities:
      LOGICAL    OpenNewText
      EXTERNAL   OpenNewText

***** Channel schematic:
      INTEGER  NumberOfStreamLocations
      EXTERNAL NumberOfStreamLocations

      real*8     StreamDistance
      EXTERNAL StreamDistance

      real*8     StreamSurfaceElevation, StreamFlow
      EXTERNAL StreamSurfaceElevation, StreamFlow

      INTEGER  NumberOfChannels
      EXTERNAL NumberOfChannels

      LOGICAL  OpenChannel, CloseChannel
      EXTERNAL OpenChannel, CloseChannel

*   Intrinsics:

*   Programmed by: Lew DeLong
*   Date:          Dec   1992
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      WriteNetworkRestartFile = .FALSE.

*-----Open restart file.

      fUnit = io_files(hydro,io_restart,io_write).unit
      FileName = io_files(hydro,io_restart,io_write).filename
      IF (.NOT. OpenNewText( fUnit, FileName ))  THEN
         WRITE(UNIT_ERROR,*) ' ####Error(WriteNetworkRestartFile)'
         WRITE(UNIT_ERROR,*) ' Failed to open restart file: ',FileName
         call exit(2)
      END IF

      WRITE(funit,900)dsm2_version,current_dt
 900  FORMAT('Hydro Version ',a
     &     /'The following data corresponds to   ',a14//)
      WRITE(fUnit,*) NumberOfChannels(),'/Channels'

*-----Loop on channels.

      DO 200 I=1,NumberOfChannels()

         IF( OpenChannel(I) ) THEN

            WRITE(fUnit,*) int2ext(I), '/Channel' ! write external number
            WRITE(fUnit,*) NumberOfStreamLocations(),'/Locations'

            DO 100 J=1,NumberOfStreamLocations()

c--------------WRITE(fUnit,'(3(F16.3,1X),A20)')
               WRITE(fUnit,'(F16.3,1X,F16.7,1X,F16.3,1X,A20)')
     &              StreamDistance(J), StreamSurfaceElevation(J), StreamFlow(J),
     &              '/Distance, WSElev, Q'

 100        CONTINUE

            OK = CloseChannel()

         ELSE
            WRITE(UNIT_ERROR,*) ' ####Error(WriteNetworkRestartFile)'
            WRITE(UNIT_ERROR,*) ' Could not open channel ',I
            RETURN
         END IF

 200  CONTINUE

      WRITE(funit,901)Nres
 901  FORMAT(/i5,' /Number of Reservoir')
      DO I=1,Nres
C--------Reservoir Stage, nodal flows
         WRITE(funit,902)I,NconnectReservoir(I),Yres(I)
 902     FORMAT(I5,' /Reservoir Number'
     &        /I5,' /Connections'
     &        /5X,1P,E15.7,' /Yres')
         WRITE(funit,903)(J,QRes(I,J),J=1, NconnectReservoir(I))
 903     FORMAT(I5,1P,E15.7,' /Connection, Qres')
      ENDDO

      CLOSE( fUnit )

      WriteNetworkRestartFile = .TRUE.

      RETURN
      END

*== Private (ReadNetworkInitialConditions) ==============================

      LOGICAL FUNCTION ReadNetworkInitialConditions()

      IMPLICIT NONE

*   Purpose:  Read initial values of dependent variables.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chinitcd.inc'
      INCLUDE 'chconnec.inc'
      INCLUDE 'chnlcomp.inc'

      include '../input/fixed/common.f'

*   Local Variables:
      INTEGER      Channels,extchan
      INTEGER      fUnit, I, K, J, IRes, NConnect, IConnect
      CHARACTER*130 FileName

*   Routines by module:

***** Local:
      INTEGER  NresStart_File
      CHARACTER*80 Header

***** File utilities:
      LOGICAL    OpenOldText
      EXTERNAL   OpenOldText

***** Channel schematic:
      INTEGER  NumberOfChannels, NumberOfStreamLocations
      EXTERNAL NumberOfChannels, NumberOfStreamLocations

*   Intrinsics:

*   Programmed by: Lew DeLong
*   Date:          Dec   1992
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      ReadNetworkInitialConditions = .FALSE.

*-----Initialize channel-number cross-reference numbers.

      DO 50 I=1,MaxChannels
         InitialConditionIndex(I) = 0
 50   CONTINUE

*-----Open initial-condition file.

      fUnit = io_files(hydro,io_restart,io_read).unit
      FileName = io_files(hydro,io_restart,io_read).filename
      IF( OpenOldText( fUnit, FileName ) ) THEN
      ELSE
         WRITE(UNIT_ERROR,*) ' ####Error(ReadNetworkInitialConditions)'
         WRITE(UNIT_ERROR,*) ' Could not open file...',FileName
         RETURN
      END IF

*-----Begin reading of data.

c-----read header, then test to see if it's really the header (old restart file)
c-----or the restart file version (new file)
      restart_version=' '
      READ(funit,'(a)') header
      IF (header(:14) .eq. 'Hydro Version') then
         restart_version=header(15:)
         READ(funit,'(a)') header
      ENDIF
      READ(fUnit,*) Channels
      IF( Channels .NE. NumberOfChannels() ) THEN
         WRITE(UNIT_ERROR,*) ' ####Error(ReadNetworkInitialConditions)'
         WRITE(UNIT_ERROR,*) ' Number of channels in restart file'
         WRITE(UNIT_ERROR,*) ' not the same as number of configured channels...'
         WRITE(UNIT_ERROR,*) ' ', Channels,' <> ', NumberOfChannels()
         RETURN
      END IF

      K = 0
      DO 200 I=1,Channels

         READ(fUnit,*) extchan  ! external channel number
         ChannelNumber(I)=ext2int(extchan)

         IF( ChannelNumber(I) .LE. NumberOfChannels()
     &        .AND.
     &        ChannelNumber(I) .GT. 0 ) THEN

            READ(fUnit,*) Locations(I)
            if (Locations(i) .ne. NumberofCompLocations(i)) then
               write(unit_error,610) int2ext(i), locations(i),
     &              NumberofCompLocations(i)

 610           format(/'####Error(ReadNetworkInitialConditions)'
     &              'For channel ',i3,
     &              /'Number of restart file computational locations (',
     &              i2, ')'
     &              /'not equal to number of locations in run (',
     &              i2, ').'
     &              /'Probably caused by different DELTAX value in SCALAR input section.'/)
               return
            endif

            IF( ( K + Locations(I) ) .LE. MaxLocations ) THEN

               FirstLocation(I) = K + 1
               DO 100 J=1,Locations(I)

                  K = K + 1
                  READ(fUnit,*)
     &                 InitialX(K), InitialWS(K), InitialQ(K)

 100           CONTINUE

            ELSE
               WRITE(UNIT_ERROR,*) ' ####Error(ReadNetworkInitialConditions)'
               WRITE(UNIT_ERROR,*) ' Reading initial conditions for channel...',
     &              int2ext(ChannelNumber(I))
               WRITE(UNIT_ERROR,*) ' Maximum number of loactions exceeded.'
               WRITE(UNIT_ERROR,*) ' Attempted...', K + Locations(I)
               WRITE(UNIT_ERROR,*) ' Allowed.....', MaxLocations
               RETURN
            END IF

         ELSE
            WRITE(UNIT_ERROR,*) ' ####Error(ReadNetworkInitialConditions)'
            WRITE(UNIT_ERROR,*) ' Read channel number ... ',
     &           int2ext(ChannelNumber(I))
            WRITE(UNIT_ERROR,*) '  must fall on or between 1 and ',
     &           NumberOfChannels()
            RETURN
         END IF

         InitialConditionIndex( ChannelNumber(I) ) = I

 200  CONTINUE

      READ(funit,*)NresStart_File
      IF(NresStart_File.EQ.Nres)THEN
         IF (restart_version .ne. ' ') then
c-----------restart file version supports reservoir connection flows
            DO I=1,Nres
               READ(funit,*)IRes ! reservoir number
               READ(funit,*)NConnect   ! number of connections
               READ(funit,*)Yres(IRes) ! reservoir stage
               IF (NConnect .eq. NconnectReservoir(IRes)) THEN
                  DO K=1, NconnectReservoir(IRes)
                     READ(funit,*)IConnect,QRes(IRes,IConnect)
                  ENDDO
               ELSE
                  WRITE(UNIT_ERROR,901) IRes
 901              FORMAT('Error(ReadNetworkInitialConditions)'
     &                 /'Number of Reservoir Connections does not match with the Restart File:'
     &                 /'Reservoir ',I5)
                  RETURN
               ENDIF
            ENDDO
         ELSE                   ! no reservoir connection flows
            DO I=1,Nres
               READ(funit,*)IRes,Yres(IRes)
            ENDDO
         ENDIF
      ELSE
         WRITE(UNIT_ERROR,902)
 902     FORMAT('Error(ReadNetworkInitialConditions)'/
     &        'Number of Reservoirs does not match with the Restart File'/)
         RETURN
      ENDIF

      ReadNetworkInitialConditions = .TRUE.

      RETURN
      END

*== Private (ApproxReadInitialConditions) ==============================

      LOGICAL FUNCTION ApproxReadInitialConditions()

      IMPLICIT NONE

*   Purpose:  Approximate initial values of dependent variables
*             for current channel from values read from an
*             initial-condition file.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netcntrl.inc'
      INCLUDE 'chinitcd.inc'
      INCLUDE 'chstatus.inc'

      include '../input/fixed/misc.f'

*   Local Variables:
      INTEGER I, J, K
      real*8    CompLocation_lcl(MaxLocations)
      LOGICAL UpstreamFlag, DownstreamFlag

*   Routines by module:

***** Local:
      LOGICAL  ReadNetworkInitialConditions
      EXTERNAL ReadNetworkInitialConditions

***** Channel schematic:
      INTEGER  NumberOfStreamLocations
      EXTERNAL NumberOfStreamLocations

      real*8     StreamDistance
      EXTERNAL StreamDistance

      INTEGER  UpstreamPointer, DownstreamPointer
      EXTERNAL UpstreamPointer, DownstreamPointer

***** Network control:
      INTEGER  CurrentChannel
      EXTERNAL CurrentChannel

***** Channel properties:
      real*8     BtmElev
      EXTERNAL BtmElev

***** Linear interpolation utilities:
      EXTERNAL Linear1D

*   Intrinsics:
      INTEGER   INT
      INTRINSIC INT

*   Programmed by: Lew DeLong
*   Date:          Dec   1992
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      ApproxReadInitialConditions = .FALSE.

*-----Set sequence number for initial conditions, check validity.

      I = InitialConditionIndex( CurrentChannel() )

      IF( I .GT. 0 ) THEN
      ELSE
         WRITE(UNIT_ERROR,*) ' ####Error(ApproxReadInitialConditions)'
         WRITE(UNIT_ERROR,*) ' Initial conditions not available for channel',
     &        CurrentChannel()
         RETURN
      END IF

*-----Check that initial conditions exist for first and last
*     cross section of channel.

      IF( INT( 100.0 * StreamDistance(1) )
     &     .EQ.
     &     INT( 100.0 * InitialX( FirstLocation(I) ) ) )  THEN

         UpstreamFlag = .TRUE.

      ELSE
         UpstreamFlag = .FALSE.
      END IF

      IF( INT( 100.0 * StreamDistance(NumberOfStreamLocations() ) )
     &     .EQ.
     &     INT( 100.0 * InitialX( FirstLocation(I)+Locations(I)-1) ) )
     &     THEN

         DownstreamFlag = .TRUE.

      ELSE
         DownstreamFlag = .FALSE.
      END IF

*-----If bad match, report errors and RETURN.

      IF( .NOT. UpstreamFlag .OR. .NOT. DownstreamFlag ) THEN

         WRITE(UNIT_ERROR,*) ' ####Error(ApproxReadInitialConditions)'
         WRITE(UNIT_ERROR,*) ' Location of channel extremities do not match.'
         WRITE(UNIT_ERROR,*) ' Channel...', CurrentChannel()

         IF( .NOT. UpstreamFlag ) THEN
            WRITE(UNIT_ERROR,*) ' Upstream, expected...........',
     &           StreamDistance(1)
            WRITE(UNIT_ERROR,*) ' Initial-condition location...',
     &           InitialX( FirstLocation(I) )
         END IF

         IF( .NOT. DownstreamFlag ) THEN
            WRITE(UNIT_ERROR,*) ' Downstream, expected.........',
     &           StreamDistance(NumberOfStreamLocations())
            WRITE(UNIT_ERROR,*) ' Initial-condition location...',
     &           InitialX( FirstLocation(I)+Locations(I)-1 )
         END IF

         RETURN

      END IF

*-----Get computational stream locations.

      K = 0
      DO 50 J=UpstreamPointer(),DownstreamPointer()
         K = K + 1
         CompLocation_lcl(J) = StreamDistance(K)
 50   CONTINUE

*-----Approximate streamflow.

      CALL Linear1D(
     &     NumberOfStreamLocations(),
     &     CompLocation_lcl(UpstreamPointer()),
     &     Locations(I), InitialX( FirstLocation(I) ),
     &     InitialQ( FirstLocation(I) ),
     &     Q(UpstreamPointer())
     &     )

*-----Approximate watersurface elevation.

      CALL Linear1D(
     &     NumberOfStreamLocations(),
     &     CompLocation_lcl(UpstreamPointer()),
     &     Locations(I), InitialX( FirstLocation(I) ),
     &     InitialWS( FirstLocation(I) ),
     &     WS(UpstreamPointer())
     &     )

*-----Approximate depth of flow.

      DO 100 J=UpstreamPointer(),DownstreamPointer()

         H( J ) = WS( J ) - BtmElev( CompLocation_lcl( J ) )

 100  CONTINUE

      ApproxReadInitialConditions = .TRUE.

      RETURN
      END

*== Private (SetNetworkInitCndRead) ================================================

      LOGICAL FUNCTION SetNetworkInitCndRead( State )

      IMPLICIT NONE

*   Purpose:  Set state of network initial-condition reading.
*             [.TRUE. ] if read, or
*             [.FALSE.] if not read.

*   Arguments:
      LOGICAL State

*   Argument definitions:
*     State - .TRUE. if read, .FALSE. otherwise.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chinitcd.inc'

*   Local Variables:

*   Routines by module:

***** Local:

*   Intrinsics:

*   Programmed by: Lew DeLong
*   Date:          Dec   1992
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      InitCndInitialized = State
      SetNetworkInitCndRead = .TRUE.

      RETURN
      END

*== Public (StreamFlow) ================================================

      real*8 FUNCTION StreamFlow(LocationNumber)

      IMPLICIT NONE

*   Purpose:  Return current value of stream flow in the current channel
*             at a location corresponding to the index LocationNumber.

*   Arguments:
      INTEGER LocationNumber

*   Argument definitions:
*     LocationNumber - computational-location sequence number within
*                      current channel.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chstatus.inc'

      include '../input/fixed/misc.f'

*   Local Variables:
      INTEGER J

*   Routines by module:

***** Channel schematic:
      INTEGER  UpstreamPointer, CurrentChannel
      EXTERNAL UpstreamPointer, CurrentChannel

      LOGICAL  CheckChannelCompLocationRange
      EXTERNAL CheckChannelCompLocationRange

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      J = UpstreamPointer() + LocationNumber - 1
      IF( CheckChannelCompLocationRange( LocationNumber ) ) THEN

         StreamFlow = Q( J )

      ELSE

         WRITE(UNIT_ERROR,*) ' Range error...(StreamFlow)'
         WRITE(UNIT_ERROR,*) ' Channel ',CurrentChannel(),'...'
         WRITE(UNIT_ERROR,*) ' Abnormal program end.'
         CALL EXIT(1)

      END IF

      RETURN
      END

*== Public (GlobalStreamFlow) ==========================================

      real*8 FUNCTION GlobalStreamFlow(LocationNumber)

      IMPLICIT NONE

*   Purpose:  Return current value of stream flow
*             at a location corresponding to the
*             index LocationNumber.

*   Arguments:
      INTEGER LocationNumber

*   Argument definitions:
*     LocationNumber - computational-location sequence number within
*                      current channel.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chstatus.inc'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      GlobalStreamFlow = Q( LocationNumber )

      RETURN
      END

*== Public (SetStreamFlow) =============================================

      LOGICAL FUNCTION SetStreamFlow(LocationNumber, Value)

      IMPLICIT NONE

*   Purpose:  Set current value of stream flow in the current channel
*             at a location corresponding to the index LocationNumber.

*   Arguments:
      INTEGER LocationNumber
      real*8    Value

*   Argument definitions:
*     LocationNumber - computational-location sequence number within
*                      current channel.
*     Value  - value to be stored.
*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chstatus.inc'

      include '../input/fixed/misc.f'

*   Local Variables:
      INTEGER J

*   Routines by module:

***** Channel schematic:
      INTEGER  UpstreamPointer
      EXTERNAL UpstreamPointer

      LOGICAL  CheckChannelCompLocationRange
      EXTERNAL CheckChannelCompLocationRange

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      J = UpstreamPointer() + LocationNumber - 1
      IF( CheckChannelCompLocationRange( LocationNumber ) ) THEN

         Q( J ) = Value
         SetStreamFlow = .TRUE.

      ELSE

         WRITE(UNIT_ERROR,*) ' Range error...(SetStreamFlow)'
         SetStreamFlow = .False.

      END IF

      RETURN
      END

*== Public (StreamDepth) ===============================================

      real*8 FUNCTION StreamDepth(LocationNumber)

      IMPLICIT NONE

*   Purpose:  Return current value of depth of flow in the current
*             channel at a location corresponding to the index
*             LocationNumber.

*   Arguments:
      INTEGER LocationNumber

*   Argument definitions:
*     LocationNumber - computational-location number (index) within
*                      current channel.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chstatus.inc'

      include '../input/fixed/misc.f'

*   Local Variables:
      INTEGER J

*   Routines by module:

***** Channel schematic:
      INTEGER  UpstreamPointer
      EXTERNAL UpstreamPointer

      LOGICAL  CheckChannelCompLocationRange
      EXTERNAL CheckChannelCompLocationRange

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      J = UpstreamPointer() + LocationNumber - 1
      IF( CheckChannelCompLocationRange( LocationNumber ) ) THEN

         StreamDepth = H( J )

      ELSE

         WRITE(UNIT_ERROR,*) ' Range error...(StreamDepth)'
         WRITE(UNIT_ERROR,*) ' Abnormal program end.'
         CALL EXIT(1)

      END IF

      RETURN
      END

*== Public (GlobalStreamDepth) =========================================

      real*8 FUNCTION GlobalStreamDepth(LocationNumber)

      IMPLICIT NONE

*   Purpose:  Return current value of depth of flow
*             at a location corresponding to the index
*             LocationNumber.

*   Arguments:
      INTEGER LocationNumber

*   Argument definitions:
*     LocationNumber - computational-location number (index) within
*                      current channel.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chstatus.inc'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      GlobalStreamDepth = H( LocationNumber )

      RETURN
      END

*== Public (SetStreamDepth) ============================================

      LOGICAL FUNCTION SetStreamDepth(LocationNumber, Value)

      IMPLICIT NONE

*   Purpose:  Set current value of depth of flow in the current
*             channel at a location corresponding to the index
*             LocationNumber.

*   Arguments:
      INTEGER LocationNumber
      real*8    Value

*   Argument definitions:
*     LocationNumber - computational-location number (index) within
*                      current channel.
*     Value - current value to be set.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chstatus.inc'

      include '../input/fixed/misc.f'

*   Local Variables:
      INTEGER J

*   Routines by module:

***** Channel schematic:
      INTEGER  UpstreamPointer
      EXTERNAL UpstreamPointer

      LOGICAL  CheckChannelCompLocationRange
      EXTERNAL CheckChannelCompLocationRange

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      J = UpstreamPointer() + LocationNumber - 1
      IF( CheckChannelCompLocationRange( LocationNumber ) ) THEN

         H( J ) = Value

         SetStreamDepth = .TRUE.

      ELSE
         WRITE(UNIT_ERROR,*) ' Range error...(SetStreamDepth)'
         SetStreamDepth = .FALSE.
      END IF

      RETURN
      END

*== Public (StreamSurfaceElevation) ====================================

      real*8 FUNCTION StreamSurfaceElevation(LocationNumber)

      IMPLICIT NONE

*   Purpose:  Return current value of water-surface elevation in the
*             current channel at a location corresponding to the index
*             LocationNumber.

*   Arguments:
      INTEGER LocationNumber

*   Argument definitions:
*     LocationNumber - computational-location number (index) within
*                      current channel.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chstatus.inc'

      include '../input/fixed/misc.f'

*   Local Variables:
      INTEGER J

*   Routines by module:

***** Channel schematic:
      INTEGER  UpstreamPointer
      EXTERNAL UpstreamPointer

      LOGICAL  CheckChannelCompLocationRange
      EXTERNAL CheckChannelCompLocationRange

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      J = UpstreamPointer() + LocationNumber - 1

      IF( CheckChannelCompLocationRange( LocationNumber ) ) THEN

         StreamSurfaceElevation = WS( J )

      ELSE
         WRITE(UNIT_ERROR,*) ' Range error...(StreamSurfaceElevation)'
         WRITE(UNIT_ERROR,*) ' Abnormal program end.'
         CALL EXIT(1)
      END IF

      RETURN
      END

*== Public (GlobalStreamSurfaceElevation) ==============================

      real*8 FUNCTION GlobalStreamSurfaceElevation(LocationNumber)

      IMPLICIT NONE

*   Purpose:  Return current value of water-surface elevation
*             at a location corresponding to the index
*             LocationNumber.

*   Arguments:
      INTEGER LocationNumber

*   Argument definitions:
*     LocationNumber - computational-location number (index).

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chstatus.inc'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      GlobalStreamSurfaceElevation = WS( LocationNumber )

      RETURN
      END

*== Public (SetStreamSurfaceElevation) =================================

      LOGICAL FUNCTION SetStreamSurfaceElevation
     &     (LocationNumber, Value)

      IMPLICIT NONE

*   Purpose:  Set current value of water-surface elevation in the
*             current channel at a location corresponding to the index
*             LocationNumber.  Also, set corresponding depth of flow.

*   Arguments:
      INTEGER LocationNumber
      real*8    Value

*   Argument definitions:
*     LocationNumber - computational-location number (index) within
*                      current channel.
*     Value  - value to be stored.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chstatus.inc'

      include '../input/fixed/misc.f'

*   Local Variables:
      INTEGER J

*   Routines by module:

***** Channel properties:
      real*8     BtmElev
      EXTERNAL BtmElev

***** Channel schematic:
      INTEGER  UpstreamPointer
      EXTERNAL UpstreamPointer

      real*8     StreamDistance
      EXTERNAL StreamDistance

      LOGICAL  CheckChannelCompLocationRange
      EXTERNAL CheckChannelCompLocationRange

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      J = UpstreamPointer() + LocationNumber - 1
      IF( CheckChannelCompLocationRange( LocationNumber ) ) THEN

         WS( J ) = Value
         H( J ) = Value - BtmElev( StreamDistance( LocationNumber ) )

         SetStreamSurfaceElevation = .TRUE.

      ELSE
         WRITE(UNIT_ERROR,*) ' Range error...(SetStreamSurfaceElevation)'
         SetStreamSurfaceElevation = .FALSE.
      END IF

      RETURN
      END

*== Public (InitializeNetworkFlowValues) ===============================

      LOGICAL FUNCTION InitializeNetworkFlowValues()

      IMPLICIT NONE

*   Purpose:  Set initial values of water-surface elevation and flow
*             in a network of channels.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netcntrl.inc'
      INCLUDE 'chstatus.inc'
      INCLUDE 'chinitcd.inc'

      include '../input/fixed/common.f'

*   Local Variables:
      INTEGER I, J, K
      INTEGER Channelnumber_L, UserLocations
      real*8    Velocity, dX, CrNo, dtr, G, Slope, FrNo
      real*8    CompLocation_lcl(MaxLocations)
      real*8    WidthRatio, Width, WSSlope, WSSlopeRatio, WSSlopeChange
      real*8    DummyArray(MaxLocations), DummyArray2(MaxLocations)
      CHARACTER*16 DummyCharArray(MaxLocations)
      LOGICAL OK

*     CompLocation_lcl(j) - local downstream distance coordinate.
*     DummyArray(j) - a REAL value dependent upon context.
*     DummyArray2(j) - a REAL value dependent upon context.
*     DummyCharArray(j) - a CHARACTER value dependent upon context.

*   Routines by module:

***** Local:
      real*8     Norm, GlobalStreamSurfaceSlope
      EXTERNAL Norm, GlobalStreamSurfaceSlope

      LOGICAL  SetNetworkInitCndRead
      EXTERNAL SetNetworkInitCndRead

      LOGICAL  ApproxReadInitialConditions
      EXTERNAL ApproxReadInitialConditions

      LOGICAL  Bernie, SpForce
      EXTERNAL Bernie, SpForce

      LOGICAL  ReadNetworkInitialConditions
      EXTERNAL ReadNetworkInitialConditions

***** Channel properties:
      real*8     BtmElev, CxArea, BtmSlope, ChannelWidth
      EXTERNAL BtmElev, CxArea, BtmSlope, ChannelWidth

      real*8     Conveyance, dConveyance
      EXTERNAL Conveyance, dConveyance

      EXTERNAL UserStreamLocations

***** Channel schematic:
      INTEGER  NumberOfUserStreamLocations, NumberOfStreamLocations
      INTEGER  UpstreamPointer, DownstreamPointer
      real*8     StreamDistance
      EXTERNAL NumberOfUserStreamLocations, NumberOfStreamLocations
      EXTERNAL UpstreamPointer, DownstreamPointer
      EXTERNAL StreamDistance

      INTEGER  NumberOfChannels, ChApprox
      EXTERNAL NumberOfChannels, ChApprox

      LOGICAL  OpenChannel, CloseChannel
      EXTERNAL OpenChannel, CloseChannel

      EXTERNAL GetUserStreamflow, GetUserStreamSurfaceElevation

***** Linear interpolation utilities:
      EXTERNAL Linear1D

***** Network control:
      INTEGER  NetworkPrintLevel, NetworkTimeIncrement
      EXTERNAL NetworKPrintLevel, NetworkTimeIncrement
      real*8     AccelerationGravity
      EXTERNAL AccelerationGravity

*   Intrinsics:
      real*8      SQRT, ABS
      INTRINSIC SQRT, ABS

*   Programmed by: Lew DeLong
*   Date:          Nov   1990
*   Modified by:   Lew DeLong
*   Last modified: August   1993
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      InitializeNetworkFlowValues = .FALSE.

      dtr = DFLOAT( NetworkTimeIncrement() )
      G = AccelerationGravity()

*-----Set initial-condition read flag to "not initialized".

      OK = .FALSE.
      OK = SetNetworkInitCndRead( OK )

      IF (Restart_Read) THEN    ! Read initial conditions from restart file
         IF( ReadNetworkInitialConditions() ) THEN
            InitCndInitialized = .TRUE.
         ELSE
            WRITE(UNIT_ERROR,*) ' ####Error(ReadNetworkInitialConditions)'
            WRITE(UNIT_ERROR,*) ' Reading of initial conditions failed...'
            RETURN
         END IF
      END IF

*-----Loop on channels.

      DO 200 I = 1, NumberOfChannels()

         Channelnumber_L = I
         IF( OpenChannel( Channelnumber_L )  ) THEN

*-----------Get computational stream locations.

            K = 0
            DO 50 J=UpstreamPointer(),DownstreamPointer()
               K = K + 1
               CompLocation_lcl(J) = StreamDistance(K)
 50         CONTINUE

*-----------Initial conditions approximated from user input.

            UserLocations = NumberOfUserStreamLocations()

*-----------Get user cross-section IDs and stream distances,
*            store in DummyArray.

            CALL UserStreamLocations(
     &           UserLocations,
     &           DummyCharArray, DummyArray
     &           )

*-----------Approximate streamflow linearly from user input.

            CALL GetUserStreamFlow( UserLocations, DummyArray2 )

            CALL Linear1D(
     &           NumberOfStreamLocations(),
     &           CompLocation_lcl(UpstreamPointer()),
     &           UserLocations, DummyArray, DummyArray2,
     &           Q(UpstreamPointer())
     &           )

*-----------Approximate watersurface elevation linearly from user input.

            CALL GetUserStreamSurfaceElevation(
     &           UserLocations, DummyArray2
     &           )

            CALL Linear1D(
     &           NumberOfStreamLocations(),
     &           CompLocation_lcl(UpstreamPointer()),
     &           UserLocations, DummyArray, DummyArray2,
     &           WS(UpstreamPointer())
     &           )

*-----------Approximate depth of flow.

            DO 100 J=UpstreamPointer(),DownstreamPointer()

               H( J ) = WS( J ) - BtmElev( CompLocation_lcl( J ) )

 100        CONTINUE

            if (Restart_Read)
     &           OK = ApproxReadInitialConditions()

*-----------Upstream end of channel.

            K = 1
            J = UpstreamPointer()
            Velocity = Q(J) / CxArea( CompLocation_lcl(J), H(J) )
            dX = CompLocation_lcl(J+1) - CompLocation_lcl(J)
            WSSlope = ( WS(J+1) - WS(J) ) / dX
            FrNo = Velocity / SQRT( G * H(J) )
            CrNo = dtr * ( Velocity + SQRT( G * H(J) ) ) / dX
            Slope = BtmSlope( CompLocation_lcl(J) )
            Width = ChannelWidth( CompLocation_lcl(J), H(J) )

*-----------Intervening cross sections.

            IF( (DownstreamPointer() - UpstreamPointer()) .GT. 2) THEN
               DO 150 J=UpstreamPointer()+1,DownstreamPointer()-1
                  K = K + 1
                  Velocity = Q(J) / CxArea( CompLocation_lcl(J), H(J) )
                  dX = ( CompLocation_lcl(J+1) - CompLocation_lcl(J-1) )
                  WSSlope = ( WS(J+1) - WS(J-1) ) / dX
                  CrNo = dtr * ( Velocity + SQRT( G * H(J) ) ) / ( 0.5 * dX )
                  FrNo = Velocity / SQRT( G * H(J) )
                  dX   = CompLocation_lcl(J+1) - CompLocation_lcl(J)
                  Slope = BtmSlope( CompLocation_lcl(J) )
                  IF( ABS( ( WS(J) - WS(J-1) ) ) .GT. 1.0E-10 ) THEN
                     WSSlopeRatio = ( WS(J+1) - WS(J) ) / ( WS(J) - WS(J-1) )
     &                    * ( CompLocation_lcl(J) - CompLocation_lcl(J-1) )
     &                    / ( CompLocation_lcl(J+1) - CompLocation_lcl(J) )
                  ELSE
                     WSSlopeRatio = 0.0
                  END IF
                  WSSlopeChange = ( WS(J+1) - WS(J) )
     &                 / ( CompLocation_lcl(J+1) - CompLocation_lcl(J) )
     &                 - ( WS(J) - WS(J-1) )
     &                 / ( CompLocation_lcl(J) - CompLocation_lcl(J-1) )
                  Width = ChannelWidth( CompLocation_lcl(J), H(J) )
                  WidthRatio = ChannelWidth( CompLocation_lcl(J+1), H(J+1) )
     &                 / ChannelWidth( CompLocation_lcl(J-1), H(J-1) )
 150           CONTINUE
            END IF

*-----------Downstream end of channel.

            K = K + 1
            J = DownstreamPointer()
            Velocity = Q(J) / CxArea( CompLocation_lcl(J), H(J) )
            dX = CompLocation_lcl(J) - CompLocation_lcl(J-1)
            FrNo = Velocity / SQRT( G * H(J) )
            CrNo = dtr * ( Velocity + SQRT( G * H(J) ) ) / dX
            Slope = BtmSlope( CompLocation_lcl(J) )
            WSSlope = ( WS(J) - WS(J-1) ) / dX
            Width = ChannelWidth( CompLocation_lcl(J), H(J) )

*-----------Check friction / WS slope / dX relation.

            OK = CloseChannel()

         ELSE
            WRITE(UNIT_ERROR,*) ' Could not open channel...',
     &           int2ext(Channelnumber_L)
            WRITE(UNIT_ERROR,*) ' (InitializeNetworkFlowValues)'
            RETURN
         END IF

 200  CONTINUE

      InitializeNetworkFlowValues = .TRUE.

      RETURN
      END

*== Public (OldStreamDensity) ================================================

      real*8 FUNCTION OldStreamDensity(LocationNumber)

      IMPLICIT NONE

*   Purpose:  Return current value of stream density in the current
*             channel at a location corresponding to the index
*             LocationNumber, at the begining of the current time step.

*   Arguments:
      INTEGER LocationNumber

*   Argument definitions:
*     LocationNumber - computational-location sequence number within
*                      current channel.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chstatus.inc'

      include '../input/fixed/misc.f'

*   Local Variables:
      INTEGER J

*   Routines by module:

***** Channel schematic:
      INTEGER  UpstreamPointer, CurrentChannel
      EXTERNAL UpstreamPointer, CurrentChannel

      LOGICAL  CheckChannelCompLocationRange
      EXTERNAL CheckChannelCompLocationRange

*   Programmed by: Lew DeLong
*   Date:          October 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      J = UpstreamPointer() + LocationNumber - 1
      IF( CheckChannelCompLocationRange( LocationNumber ) ) THEN

         OldStreamDensity = Rho1( J )

      ELSE

         WRITE(UNIT_ERROR,*) ' Range error...(OldStreamDensity)'
         WRITE(UNIT_ERROR,*) ' Channel ',CurrentChannel(),'...'
         WRITE(UNIT_ERROR,*) ' Abnormal program end.'
         CALL EXIT(1)

      END IF

      RETURN
      END

*== Public (NewStreamDensity) ================================================

      real*8 FUNCTION NewStreamDensity(LocationNumber)

      IMPLICIT NONE

*   Purpose:  Return current value of stream density in the current
*             channel at a location corresponding to the index
*             LocationNumber, at the end of the current time step.

*   Arguments:
      INTEGER LocationNumber

*   Argument definitions:
*     LocationNumber - computational-location sequence number within
*                      current channel.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chstatus.inc'

      include '../input/fixed/misc.f'

*   Local Variables:
      INTEGER J

*   Routines by module:

***** Channel schematic:
      INTEGER  UpstreamPointer, CurrentChannel
      EXTERNAL UpstreamPointer, CurrentChannel

      LOGICAL  CheckChannelCompLocationRange
      EXTERNAL CheckChannelCompLocationRange

*   Programmed by: Lew DeLong
*   Date:          October 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      J = UpstreamPointer() + LocationNumber - 1
      IF( CheckChannelCompLocationRange( LocationNumber ) ) THEN

         NewStreamDensity = Rho2( J )

      ELSE

         WRITE(UNIT_ERROR,*) ' Range error...(NewStreamDensity)'
         WRITE(UNIT_ERROR,*) ' Channel ',CurrentChannel(),'...'
         WRITE(UNIT_ERROR,*) ' Abnormal program end.'
         CALL EXIT(1)

      END IF

      RETURN
      END

*== Public (SetOldStreamDensity) ================================================

      LOGICAL FUNCTION SetOldStreamDensity(LocationNumber, Value)

      IMPLICIT NONE

*   Purpose:  Set current value of stream density in the current
*             channel at a location corresponding to the index
*             LocationNumber, at the begining of the current time step.

*   Arguments:
      INTEGER LocationNumber
      real*8    Value

*   Argument definitions:
*     LocationNumber - computational-location sequence number within
*                      current channel.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chstatus.inc'

      include '../input/fixed/misc.f'

*   Local Variables:
      INTEGER J

*   Routines by module:

***** Channel schematic:
      INTEGER  UpstreamPointer, CurrentChannel
      EXTERNAL UpstreamPointer, CurrentChannel

      LOGICAL  CheckChannelCompLocationRange
      EXTERNAL CheckChannelCompLocationRange

*   Programmed by: Lew DeLong
*   Date:          October 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      J = UpstreamPointer() + LocationNumber - 1
      IF( CheckChannelCompLocationRange( LocationNumber ) ) THEN

         Rho1( J ) = Value

      ELSE

         WRITE(UNIT_ERROR,*) ' Range error...(SetOldStreamDensity)'
         WRITE(UNIT_ERROR,*) ' Channel ',CurrentChannel(),'...'
         WRITE(UNIT_ERROR,*) ' Abnormal program end.'
         CALL EXIT(1)

      END IF

      SetOldStreamDensity = .TRUE.

      RETURN
      END

*== Public (SetNewStreamDensity) ================================================

      LOGICAL FUNCTION SetNewStreamDensity(LocationNumber, Value)

      IMPLICIT NONE

*   Purpose:  Set current value of stream density in the current
*             channel at a location corresponding to the index
*             LocationNumber, at the end of the current time step.

*   Arguments:
      INTEGER LocationNumber
      real*8    Value

*   Argument definitions:
*     LocationNumber - computational-location sequence number within
*                      current channel.
*     Value - value to be set.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chstatus.inc'

      include '../input/fixed/misc.f'

*   Local Variables:
      INTEGER J

*   Routines by module:

***** Channel schematic:
      INTEGER  UpstreamPointer, CurrentChannel
      EXTERNAL UpstreamPointer, CurrentChannel

      LOGICAL  CheckChannelCompLocationRange
      EXTERNAL CheckChannelCompLocationRange

*   Programmed by: Lew DeLong
*   Date:          October 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      J = UpstreamPointer() + LocationNumber - 1
      IF( CheckChannelCompLocationRange( LocationNumber ) ) THEN

         Rho2( J ) = Value

      ELSE

         WRITE(UNIT_ERROR,*) ' Range error...(SetNewStreamDensity)'
         WRITE(UNIT_ERROR,*) ' Channel ',CurrentChannel(),'...'
         WRITE(UNIT_ERROR,*) ' Abnormal program end.'
         CALL EXIT(1)

      END IF

      SetNewStreamDensity = .TRUE.

      RETURN
      END

*== Public (EstOldStreamDensity) ================================================

      real*8 FUNCTION EstOldStreamDensity(DownstreamDistance)

      IMPLICIT NONE

*   Purpose:  Return current value of stream density in the current
*             channel at a  downstream distance of
*             DownStreamDistance, at the begining of the current
*             time step.

*   Arguments:
      real*8 DownstreamDistance

*   Argument definitions:
*     DownstreamDistance - downstream distance to location within
*                           current channel.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chstatus.inc'

      include '../input/fixed/misc.f'

*   Local Variables:
      INTEGER I, N
      real*8    XUp, dX, Shape

*   Routines by module:

***** Channel schematic:
      INTEGER  UpstreamPointer, DownstreamPointer, CurrentChannel
      real*8     StreamDistance
      EXTERNAL UpstreamPointer, DownstreamPointer, CurrentChannel
      EXTERNAL StreamDistance

*   Programmed by: Lew DeLong
*   Date:          October 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      N = 0
      DO 100 I=UpstreamPointer(),DownstreamPointer()
         N = N + 1
         IF(StreamDistance(N) .LE. DownstreamDistance ) THEN
         ELSE
            GO TO 102
         END IF
 100  CONTINUE
      WRITE(UNIT_ERROR,*) ' Range error...(EstOldStreamDensity)'
      WRITE(UNIT_ERROR,*) ' Channel ',CurrentChannel(),'...'
      WRITE(UNIT_ERROR,*) ' Downstream distance ...', DownstreamDistance
 102  CONTINUE

      XUp = StreamDistance(N-1)
      dX = StreamDistance(N) - XUp
      Shape = (DownstreamDistance - XUp) / dX
      EstOldStreamDensity = Rho1(N) * Shape + Rho1(N-1) * (1.0 - Shape)

      RETURN
      END

*== Public (EstNewStreamDensity) ================================================

      real*8 FUNCTION EstNewStreamDensity(DownstreamDistance)

      IMPLICIT NONE

*   Purpose:  Return current value of stream density in the current
*             channel at a location at a downstream distance of
*             DownStreamDistance, at the end of the current
*             time step.

*   Arguments:
      real*8 DownstreamDistance

*   Argument definitions:
*     DownstreamDistance - downstream distance to location within
*                           current channel.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chstatus.inc'

      include '../input/fixed/misc.f'

*   Local Variables:
      INTEGER I, N
      real*8    XUp, dX, Shape

*   Routines by module:

***** Channel schematic:
      INTEGER  UpstreamPointer, DownstreamPointer, CurrentChannel
      real*8     StreamDistance
      EXTERNAL UpstreamPointer, DownstreamPointer, CurrentChannel
      EXTERNAL StreamDistance

*   Programmed by: Lew DeLong
*   Date:          October 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      N = 0
      DO 100 I=UpstreamPointer(),DownstreamPointer()
         N = N + 1
         IF(StreamDistance(N) .LE. DownstreamDistance ) THEN
         ELSE
            GO TO 102
         END IF
 100  CONTINUE
      WRITE(UNIT_ERROR,*) ' Range error...(EstNewStreamDensity)'
      WRITE(UNIT_ERROR,*) ' Channel ',CurrentChannel(),'...'
      WRITE(UNIT_ERROR,*) ' Downstream distance ...', DownstreamDistance
 102  CONTINUE

      XUp = StreamDistance(N-1)
      dX = StreamDistance(N) - XUp
      Shape = (DownstreamDistance - XUp) / dX
      EstNewStreamDensity = Rho2(N) * Shape + Rho2(N-1) * (1.0 - Shape)

      RETURN
      END

*== Public (SetConstantStreamDensity) ====================================

      LOGICAL FUNCTION SetConstantStreamDensity()

      IMPLICIT NONE

*   Purpose:  Set stream density to 1.0 for all channels.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chstatus.inc'

      include '../input/fixed/misc.f'

*   Local Variables:
      INTEGER I, M, Channel
      LOGICAL  OK

*   Routines by module:

***** Channel schematic:
      INTEGER  UpstreamPointer, DownstreamPointer
      EXTERNAL UpstreamPointer, DownstreamPointer
      INTEGER  NumberOfChannels
      EXTERNAL NumberOfChannels
      LOGICAL  OpenChannel, CloseChannel
      EXTERNAL OpenChannel, CloseChannel

*   Intrinsics:

*   Programmed by: Lew DeLong
*   Date:          October  1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      DO 200 M=1,NumberOfChannels()

         Channel = M
         IF( OpenChannel( Channel ) ) THEN
            DO 100 I=UpstreamPointer(),DownstreamPointer()
               Rho1(I) = 1.0
               Rho2(I) = 1.0
 100        CONTINUE
         ELSE
            WRITE(UNIT_ERROR,*) ' ####error(SetConstantStreamDensity)'
            WRITE(UNIT_ERROR,*) ' could not open channel...',Channel
            WRITE(UNIT_ERROR,*) ' Abnormal program end.'
            CALL EXIT(1)
         END IF

         OK = CloseChannel()

 200  CONTINUE

      SetConstantStreamDensity = .TRUE.

      RETURN
      END

*== Public (Norm) ======================================================

      real*8 FUNCTION Norm(X, Q)

      IMPLICIT NONE

*   Purpose:  Estimate normal depth in the current channel,
*             at X downstream distance, for discharge Q.

*   Arguments:
      real*8 X, Q

*   Argument Definitions:
*     X      - downstream distance.
*     Q      - streamflow

      include '../input/fixed/misc.f'

*   Local variables:
      INTEGER I, Attempts
      INTEGER MaxAttempts
      PARAMETER( MaxAttempts = 50 )
      real*8    K, NewK,NewH,dH, LowH, HighH
      real*8    Slope, Tolerance
      PARAMETER( Tolerance = 0.01 )

*   Local Definitions:
*     Slope   - water-surface slope, assumed = bottom slope.

*   Routines by model:

***** Channel properties:
      real*8     Conveyance, dConveyance, BtmSlope
      EXTERNAL Conveyance, dConveyance, BtmSlope

***** Channel schematic:
      INTEGER  CurrentChannel
      EXTERNAL CurrentChannel

*   Intrinsics:
      real*8      ABS,SQRT
      INTRINSIC ABS,SQRT

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:   Lew DeLong
*   Last modified: July     1993
*   Version 93.01, January, 1993

*-----Implementation ------------------------------------------------------------

      Slope = ABS( BtmSlope( X ) )

*-----Return arbitrary depth if slope is flat.

      IF( Slope .LT. 0.00001 ) THEN
         Norm = 42.0
         WRITE(UNIT_ERROR,*) '  ***Error(Norm) flat slope...'
         WRITE(UNIT_ERROR,*) ' Channel = ',CurrentChannel(),' X = ',
     &        X,' assigned H = ',Norm
         WRITE(UNIT_ERROR,*) ' Q = ',Q,'  Slope = ',Slope
         RETURN
      END IF

      K = Q/SQRT(Slope)
      NewH = 1.0
      LowH = 0.0

*-----Bracket normal depth.

      Attempts = 0
 50   CONTINUE

      IF( Attempts .LT. MaxAttempts ) THEN
         Attempts = Attempts + 1
         NewK = Conveyance(X,NewH)
         IF( NewK .LT. K ) THEN
            NewH = 2.0 * NewH
            GO TO 50
         END IF
      ELSE
         WRITE(UNIT_ERROR,*) ' *** Error (Norm)'
         WRITE(UNIT_ERROR,*) ' Failed to bracket normal depth.'
         WRITE(UNIT_ERROR,*) ' Channel = ',CurrentChannel(),
     &        ' X = ',X,' Last H = ',NewH
         WRITE(UNIT_ERROR,*) ' Q = ',Q,'  Slope = ',Slope
         Norm = NewH
         RETURN
      END IF

*-----Locate normal depth between brackets.

      Attempts = 0
      HighH = NewH
 60   CONTINUE
      IF( Attempts .LT. MaxAttempts ) THEN

         NewH = 0.5 * (LowH + HighH)
         NewK = Conveyance(X,NewH)

         IF( NewK .GT. K ) THEN
            HighH = NewH
         ELSE
            LowH = NewH
         END IF

         IF( ABS( HighH - LowH ) .GT. Tolerance ) THEN
            GO TO 60
         ELSE
            Norm = NewH
            RETURN
         END IF

      ELSE
         WRITE(UNIT_ERROR,*) ' ***Warning(Norm)'
         WRITE(UNIT_ERROR,*) ' Failed to accurately locate normal depth'
         WRITE(UNIT_ERROR,*) ' by a bisection method..., continuing...'
         WRITE(UNIT_ERROR,*) ' Channel = ',CurrentChannel(),
     &        ' X = ',X,' Last H = ',NewH
         WRITE(UNIT_ERROR,*) ' Q = ',Q,'  Slope = ',Slope
      END IF

      DO 100 I=1,MaxAttempts

         dH = (K-Conveyance(X,NewH))/dConveyance(X,NewH)
         NewH  = NewH+dH
*       WRITE(*,*) I,'...',NewH
         IF(ABS(dH).LT.0.01) GO TO 102

 100  CONTINUE

      WRITE(UNIT_ERROR,*) '  ***Warning(Norm) did not close...'
      WRITE(UNIT_ERROR,*) ' Channel = ',CurrentChannel(),' X = ',X,' H = ',NewH
      WRITE(UNIT_ERROR,*) ' Q = ',Q,'  Slope = ',Slope

 102  CONTINUE

      Norm = NewH

      RETURN
      END

*== Public (Bernie) ================================================

      LOGICAL FUNCTION Bernie()

      IMPLICIT NONE

*   Purpose:  Roughly approximate steady flow within a 1-D channel,
*             using Bernoulli's equation (specific energy).  Lateral
*             flow and is ignored, and alpha is assumed equal to 1.

*   Arguments:

*   Argument definitions:

*   Module data:

      include '../input/fixed/misc.f'

*   Local Variables:
      INTEGER I, J, CxNo, Subdivisions, Channel
      INTEGER MaxIterations, Iterations
      real*8    X, Xd, Z, Zd, Q, e0, c, dZ
      real*8    Tolerance, dX, Change
      real*8    Xup, Xdn, Zup, Zdn, e0i, WSSlope
      real*8    SmallestdX
      LOGICAL Errors, OK, Print, CheckConvergence, PrintMore

*   Routines by module:

***** Local:
      real*8     StreamEnergy, dStreamEnergydZ, StreamResistance
      EXTERNAL StreamEnergy, dStreamEnergydZ, StreamResistance
      real*8     dStreamResistancedZu
      EXTERNAL dStreamResistancedZu
      LOGICAL  SetdXBernie
      EXTERNAL SetdXBernie

***** Channel status:
      real*8     StreamFlow, StreamSurfaceElevation
      EXTERNAL StreamFlow, StreamSurfaceElevation
      LOGICAL  SetStreamSurfaceElevation
      EXTERNAL SetStreamSurfaceElevation
      real*8     StreamDepth
      EXTERNAL StreamDepth

***** Channel schematic:
      INTEGER  NumberOfStreamLocations, CurrentChannel
      EXTERNAL NumberOfStreamLocations, CurrentChannel
      real*8     StreamDistance
      EXTERNAL StreamDistance

***** Network control:
      real*8     ToleranceStreamZ
      EXTERNAL ToleranceStreamZ
      INTEGER  NetworkPrintLevel, MaxNetworkIterations
      EXTERNAL NetworkPrintLevel, MaxNetworkIterations

*   Intrinsics:
      INTEGER   INT
      real*8      ABS
      INTRINSIC INT, ABS

*   Programmed by: Lew DeLong
*   Date:          Sept  1993
*   Modified by:
*   Last modified:

*-----Implementation -----------------------------------------------------

      Bernie = .FALSE.
      Errors = .FALSE.

      MaxIterations = MaxNetworkIterations()
      Tolerance = ToleranceStreamZ()
      Channel   = CurrentChannel()

      IF( NetworkPrintLevel() .GT. 1  ) THEN
         Print = .TRUE.
         PrintMore = .TRUE.
      ELSE IF( NetworkPrintLevel() .GT. 0  ) THEN
         PrintMore = .FALSE.
         Print = .TRUE.
      ELSE
         Print = .FALSE.
         PrintMore = .FALSE.
      END IF

      IF( Print ) THEN
         WRITE(*,*) ' '
         WRITE(*,*) ' Steady approximation, Channel...',Channel
      END IF

*-----Compute energy at downstream end of channel.

      CxNo = NumberOfStreamLocations()
      Xd = StreamDistance(CxNo)
      Q  = StreamFlow(CxNo)
      Zd = StreamSurfaceElevation(CxNo)

      IF( Q .GT. 0.0 ) THEN
         e0 = StreamEnergy(Xd,Q,Zd)
      ELSE
         WRITE(UNIT_ERROR,*) ' #### Warning(Bernie)...'
         WRITE(UNIT_ERROR,*) ' Channel...',Channel
         WRITE(UNIT_ERROR,*) ' Steady-state approximation not implemented for',
     &        ' negative flow.'
         RETURN
      END IF

      SmallestdX = Xd - StreamDistance(CxNo-1)

*-----Set initial dX.

      dX = SmallestdX

*-----Begin computational-location loop, at downstream end of channel.

      DO 300 I=2,NumberOfStreamLocations()

*--------Set reference distance and WSElev at adjacent upstream
*       computational location.

         CxNo = CxNo -1

         X = StreamDistance(CxNo)
         Z = StreamSurfaceElevation(CxNo)

*--------Begin spatial-convergence loop.

         CheckConvergence = .FALSE.

 50      CONTINUE

*--------Estimate initial water-surface slope.

         IF( I .GT. 2 ) THEN
            WSSlope = ( StreamSurfaceElevation(CxNo+2) - Zd )
     &           /( StreamDistance(CxNo+2) - Xd)

         ELSE
            WSSlope = (Zd - Z) / (Xd - X)
         END IF

*--------Determine number of subdivisions and corresponding dX.

         Subdivisions = INT( (Xd-X) / dX )
         IF( Subdivisions .LT. 1 ) THEN
            Subdivisions = 1
         END IF
         dX = (Xd-X) / DBLE(Subdivisions)

*--------Initialize values for subdivision loop.  Note, different
*       variables are used in the subdivision loop to simplify
*       restarting the loop from the same initial values when required.

         Xup = Xd
         Zup = Zd
         e0i = e0

*--------Begin subdivision loop.

         DO 200 J=1,Subdivisions

*-----------Initialize values for the iteration loop.

            Xdn = Xup
            Xup = Xup - dX
            Zdn = Zup
            Zup = Zdn - dX * WSSlope
            Iterations = 0

*-----------Begin iteration loop within the subdivision loop.

 100        CONTINUE
            Iterations = Iterations + 1

*-----------Begin computation of coefficients.

            c = dStreamEnergydZ(Xup,Q,Zup)
     &           - dStreamResistancedZu(Xup,Xdn,Zup,Zdn,Q)

*-----------Update estimate of Z at the upstream end
*         of current the subdivision.

            dZ = (
     &           e0i-StreamEnergy(Xup,Q,Zup)
     &           + StreamResistance(Xup,Xdn,Zup,Zdn,Q)
     &           )  / c

            Zup = Zup + dZ

*-----------Check for closure of iteration.

            IF( ABS( dZ ) .GT. Tolerance ) THEN
               IF( Iterations .LT. MaxIterations ) THEN
                  GO TO 100
               ELSE
                  Errors = .TRUE.

*-----------------Subdivide dX, if iteration hasn't closed.

                  dX = 0.5 * dX
                  IF( dX .GT. 1.0 ) THEN
                     GO TO 50
                  ELSE
                     WRITE(UNIT_ERROR,*) ' Giving up on channel...',Channel
                     WRITE(UNIT_ERROR,*) ' X...',Xup,'Z...',Zup,
     &                    'Iterations...',Iterations
                     IF( dX .LT. SmallestdX ) THEN
                        SmallestdX = dX
                     END IF
                     OK = SetdXBernie( -SmallestdX )
                     RETURN
                  END IF

               END IF
            END IF
*-----------End of iteration loop within subdivision loop.

*-----------Estimate new water-surface slope and total energy
*         for use in next subdivision.

            WSSlope = (Zdn - Zup) / (Xdn - Xup)
            e0i = e0i + StreamResistance(Xup,Xdn,Zup,Zdn,Q)

 200     CONTINUE
*--------End of subdivision loop.

*--------Assign resulting final WSElev from the subdivision loop
*       to the adjacent upstream computational location.

         Z = Zup

*--------Check for convergence.

         Change = ABS(StreamSurfaceElevation(CxNo)-Z)
         OK = SetStreamSurfaceElevation( CxNo, Z )

         IF( CheckConvergence ) THEN
         ELSE
            CheckConvergence = .TRUE.
            dX = 0.5 * dX
            GO TO 50
         END IF

         IF( Change .LE. Tolerance ) THEN

*-----------If this is a successful test for convergence, the preceding
*         dX was sufficient ... change dX back to the preceding value.

            IF( CheckConvergence ) THEN
               dX = 2.0 * dX
               IF( dX .LT. SmallestdX ) THEN
                  SmallestdX = dX
               END IF
               IF( PrintMore ) THEN
                  WRITE(*,*) ' X...',Xup,'  Z...',Zup,'  dX...',dX,
     &                 '  Iterations last step...',Iterations
               END IF
            END IF

         ELSE

*-----------Convergence has not been obtained, reduce dX.

            Errors = .TRUE.
            dX = 0.5 * dX
            IF( dX .GT. 1.0 ) THEN
               GO TO 50
            ELSE
               WRITE(UNIT_ERROR,*) ' X...', X
               WRITE(UNIT_ERROR,*) ' Steady solution failed to converge...'
               WRITE(UNIT_ERROR,*) ' change, last iteration, ...',Change
               WRITE(UNIT_ERROR,*) ' giving up on this channel...'
               WRITE(UNIT_ERROR,*) ' '
               OK = SetdXBernie( -dX )
               RETURN
            END IF

         END IF
*--------End of spatial-convergence loop.

*--------Reassign values for the next computational location.

         e0 = e0i
         Xd = X
         Zd = Z

 300  CONTINUE
*-----End of computational-location loop.

      Bernie = .TRUE.

      OK = SetdXBernie( SmallestdX )

      RETURN
      END

*== Public (StreamEnergy) ================================================

      real*8 FUNCTION StreamEnergy( X, Q, Z )

      IMPLICIT NONE

*   Purpose:  Approximate specific energy in the current channel at X,
*             given flow (Q) and water-surface elevation (Z).

*   Arguments:
      real*8 X, Q, Z

*   Argument definitions:
*     X - downstream distance in current channel.
*     Q - discharge.
*     Z - water-surface elevation.

*   Module data:

*   Local Variables:
      real*8 Depth, Velocity, G

*   Routines by module:

***** Local:

***** Network control:
      real*8     AccelerationGravity
      EXTERNAL AccelerationGravity

***** Channel properties:
      real*8     CxArea, BtmElev
      EXTERNAL CxArea, BtmElev

*   Intrinsics:

*   Programmed by: Lew DeLong
*   Date:          Sept  1993
*   Modified by:
*   Last modified:

*-----Implementation -----------------------------------------------------

      Depth = Z - BtmElev( X )
      IF( Depth .GT. 0.0 ) THEN
         G = AccelerationGravity()
         Velocity = Q / CxArea( X, Depth )
         StreamEnergy = Z + Velocity**2 / (2.0 * G)
      ELSE
         StreamEnergy = 0.0
      END IF

      RETURN
      END

*== Public (dStreamEnergydZ) ================================================

      real*8 FUNCTION dStreamEnergydZ( X, Q, Z )

      IMPLICIT NONE

*   Purpose:  Approximate the gradient of specific energy
*             with depth of flow.

      real*8 X, Q, Z

*   Argument definitions:
*     X - downstream distance in current channel.
*     Q - discharge.
*     Z - water-surface elevation.

*   Module data:

*   Local Variables:
      real*8 G, A, Depth, Velocity

*   Routines by module:

***** Local:

***** Network control:
      real*8     AccelerationGravity
      EXTERNAL AccelerationGravity

***** Channel properties:
      real*8     CxArea, ChannelWidth, BtmElev
      EXTERNAL CxArea, ChannelWidth, BtmElev

*   Intrinsics:

*   Programmed by: Lew DeLong
*   Date:          Sept  1993
*   Modified by:
*   Last modified:

*-----Implementation -----------------------------------------------------

      Depth = Z - BtmElev( X )
      IF( Depth .GT. 0.0 ) THEN
         G = AccelerationGravity()
         A = CxArea( X, Depth )
         Velocity = Q / A
         dStreamEnergydZ = 1.0
     &        - ChannelWidth( X,Depth ) * Velocity**2 /(A*G)
      ELSE
         dStreamEnergydZ = 0.0
      END IF

      RETURN
      END

*== Public (StreamResistance) ================================================

      real*8 FUNCTION StreamResistance( Xu, Xd, Zu, Zd, Q )

      IMPLICIT NONE

*   Purpose:  Approximate head loss due to flow resistance between
*             an upstream and downstream cross section.

*   Arguments:
      real*8 Xu, Xd, Zu, Zd, Q

*   Argument definitions:
*     Xu - down stream distance to upstream cross section.
*     Xd - down stream distance to downstream cross section.
*     Zu - upstream water-surface elevation.
*     Zd - downstream water-surface elevation.
*     Q  - discharge.

*   Module data:
      INCLUDE 'network.inc'

*   Local Variables:
      INTEGER K, QuadPts
      real*8 N( MaxQuadPts )
      real*8 Hd, Hu
      real*8 H, X, QuadWt, QuadPt, fric

*   Routines by module:

***** Local:

***** Channel properties:
      real*8     BtmElev, Conveyance
      EXTERNAL BtmElev, Conveyance

***** Network control:
      INTEGER  NetworkQuadPts
      EXTERNAL NetworkQuadPts, NetworkQuadPtWt

*   Intrinsics:

*   Programmed by: Lew DeLong
*   Date:          Sept  1993
*   Modified by:
*   Last modified:

*-----Implementation -----------------------------------------------------

      Hu = Zu - BtmElev( Xu )
      Hd = Zd - BtmElev( Xd )

      fric = 0.0
      QuadPts = NetworkQuadPts()

      DO 100 K=1,QuadPts

*--------Estimate quadrature-point values.

         CALL NetworkQuadPtWt( K, QuadPt, QuadWt )

*--------Interpolation functions.
         N(1) = 1.0 - QuadPt
         N(2) = QuadPt

*--------Location of quadrature point.
         X = N(1) * Xu + N(2) * Xd

*--------Dependent variables.
         H = N(1) * Hu + N(2) * Hd

         IF( H .GT. 0.0 ) THEN
            fric = fric + QuadWt / ( Conveyance(X,H) ** 2 )
         END IF

 100  CONTINUE

      StreamResistance = Q * ABS( Q ) * fric * ( Xd - Xu )

      RETURN
      END

*== Public (dStreamResistancedZu) ================================================

      real*8 FUNCTION dStreamResistancedZu( Xu, Xd, Zu, Zd, Q )

      IMPLICIT NONE

*   Purpose:  Approximate gradient, with respect to a change in upstream
*             water-surface elevation, of head loss due to flow
*             resistance between an upstream and downstream cross section.

*   Arguments:
      real*8 Xu, Xd, Zu, Zd, Q

*   Argument definitions:
*     Xu - down stream distance to upstream cross section.
*     Xd - down stream distance to downstream cross section.
*     Zu - upstream water-surface elevation.
*     Zd - downstream water-surface elevation.
*     Q  - discharge.

*   Module data:
      INCLUDE 'network.inc'

*   Local Variables:
      INTEGER K, QuadPts
      real*8 N( MaxQuadPts )
      real*8 Hd, Hu
      real*8 H, X, QuadWt, QuadPt, fric

*   Routines by module:

***** Local:

***** Channel properties:
      real*8     BtmElev, Conveyance, dConveyance
      EXTERNAL BtmElev, Conveyance, dConveyance

***** Network control:
      INTEGER  NetworkQuadPts
      EXTERNAL NetworkQuadPts, NetworkQuadPtWt

*   Intrinsics:

*   Programmed by: Lew DeLong
*   Date:          Sept  1993
*   Modified by:
*   Last modified:

*-----Implementation -----------------------------------------------------

      Hu = Zu - BtmElev( Xu )
      Hd = Zd - BtmElev( Xd )

      fric = 0.0
      QuadPts = NetworkQuadPts()

      DO 100 K=1,QuadPts

*--------Estimate quadrature-point values.

         CALL NetworkQuadPtWt( K, QuadPt, QuadWt )

*--------Interpolation functions.
         N(1) = 1.0 - QuadPt
         N(2) = QuadPt

*--------Location of quadrature point.
         X = N(1) * Xu + N(2) * Xd

*--------Dependent variables.
         H = N(1) * Hu + N(2) * Hd

*--------Integrate friction term.

         IF( H .GT. 0.0 ) THEN
            fric = fric - QuadWt * N(1) *  2.0 * dConveyance(X,H)
     &           / ( Conveyance(X,H) ** 3 )
         END IF

 100  CONTINUE

      dStreamResistancedZu = Q * ABS( Q ) * fric * ( Xd - Xu )

      RETURN
      END

*== Public (GlobalStreamSurfaceSlope) ====================================

      real*8 FUNCTION GlobalStreamSurfaceSlope(LocationNumber)

      IMPLICIT NONE

*   Purpose:  Return current value of water-surface slope
*             at global location corresponding to LocationNumbe.

*   Arguments:
      INTEGER LocationNumber

*   Argument definitions:
*     LocationNumber - global computational-location number.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chstatus.inc'

*   Local Variables:

*   Routines by module:

***** Local:
      real*8     GlobalStreamDistance
      EXTERNAL GlobalStreamDistance

***** Channel schematic:
      INTEGER  UpstreamPointer, DownstreamPointer
      EXTERNAL UpstreamPointer, DownstreamPointer

*   Programmed by: Lew DeLong
*   Date:          September 1993
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      IF( LocationNumber .EQ. UpstreamPointer() ) THEN

*--------Upstream end of channel.

         GlobalStreamSurfaceSlope =
     &        ( WS( LocationNumber + 1 )  - WS( LocationNumber ) )
     &        /
     &        (
     &        GlobalStreamDistance( LocationNumber + 1 )
     &        - GlobalStreamDistance( LocationNumber     )
     &        )

      ELSE IF( LocationNumber .EQ.  DownstreamPointer() ) THEN

*--------Downstream end of channel.

         GlobalStreamSurfaceSlope =
     &        (
     &        WS( LocationNumber     )
     &        - WS( LocationNumber - 1 )
     &        )  /  (
     &        GlobalStreamDistance( LocationNumber     )
     &        - GlobalStreamDistance( LocationNumber - 1 )
     &        )

      ELSE

*--------Intervening reaches of channel.

         GlobalStreamSurfaceSlope =
     &        (
     &        WS( LocationNumber + 1 )
     &        - WS( LocationNumber - 1 )
     &        )  /  (
     &        GlobalStreamDistance( LocationNumber + 1 )
     &        - GlobalStreamDistance( LocationNumber - 1 )
     &        )

      END IF

      RETURN
      END

*== Public (SetdXBernie) ============================================

      LOGICAL FUNCTION SetdXBernie( Value )

      IMPLICIT NONE

*   Purpose:  Record value of incremental stream distance required
*             for solution of Bernouilli's equation for steady flow
*             in the current channel.

*   Arguments:
      real*8    Value

*   Argument definitions:
*     Value - incremental stream distance required
*             for solution of Bernouilli's equation for steady flow
*             in the current channel.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chstatus.inc'

*   Local Variables:

*   Routines by module:

***** Channel schematic:
      INTEGER  CurrentChannel
      EXTERNAL CurrentChannel

*   Programmed by: Lew DeLong
*   Date:          Sept 1993
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      IF( Value .GT. 0.0 ) THEN
         dXBernie( CurrentChannel() ) = Value
         ConvergedSteady(CurrentChannel()) = .TRUE.
      ELSE
         dXBernie( CurrentChannel() ) = -Value
         ConvergedSteady(CurrentChannel()) = .FALSE.
      END IF

      SetdXBernie = .TRUE.

      RETURN
      END

*== Public (SetdXForce) ============================================

      LOGICAL FUNCTION SetdXForce( Value )

      IMPLICIT NONE

*   Purpose:  Record last value of incremental stream distance used
*             for solution of specific-force equation for steady flow
*             in the current channel.

*   Arguments:
      real*8    Value

*   Argument definitions:
*     Value - last incremental stream distance used
*             for solution of specific-force equation for steady flow
*             in the current channel.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chstatus.inc'

*   Local Variables:

*   Routines by module:

***** Channel schematic:
      INTEGER  CurrentChannel
      EXTERNAL CurrentChannel

*   Programmed by: Lew DeLong
*   Date:          Sept 1993
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      IF( Value .GT. 0.0 ) THEN
         dXForce( CurrentChannel() ) = Value
         ConvergedSteady(CurrentChannel()) = .TRUE.
      ELSE
         dXForce( CurrentChannel() ) = -Value
         ConvergedSteady(CurrentChannel()) = .FALSE.
      END IF

      SetdXForce = .TRUE.

      RETURN
      END

*== Public (SpForce) ================================================

      LOGICAL FUNCTION SpForce()

      IMPLICIT NONE

*   Purpose:  Roughly approximate steady flow within a 1-D channel,
*             using specific-force equation.  Lateral flow is
*             ignored.

*   Arguments:

*   Argument definitions:

*   Module data:

      include '../input/fixed/misc.f'

*   Local Variables:
      INTEGER I, J, CxNo, Subdivisions, Channel
      INTEGER MaxIterations, Iterations
      real*8    X, Xd, Z, Zd, Q, e0, c, dZ
      real*8    Tolerance, dX, Change
      real*8    Xup, Xdn, Zup, Zdn, e0i, WSSlope
      LOGICAL Errors, OK, Print, CheckConvergence

*   Routines by module:

***** Local:
      real*8     StreamMoFlux, dStreamMoFluxdZ, gAdZdX, gASf
      EXTERNAL StreamMoFlux, dStreamMoFluxdZ, gAdZdX, gASf
      real*8     dgAdZdXdZu, dgASfdZu
      EXTERNAL dgAdZdXdZu, dgASfdZu
      LOGICAL  SetdXForce
      EXTERNAL SetdXForce

***** Channel status:
      real*8     StreamFlow, StreamSurfaceElevation
      EXTERNAL StreamFlow, StreamSurfaceElevation
      LOGICAL  SetStreamSurfaceElevation
      EXTERNAL SetStreamSurfaceElevation
      real*8     StreamDepth
      EXTERNAL StreamDepth

***** Channel schematic:
      INTEGER  NumberOfStreamLocations, CurrentChannel
      EXTERNAL NumberOfStreamLocations, CurrentChannel
      real*8     StreamDistance
      EXTERNAL StreamDistance

***** Network control:
      real*8     ToleranceStreamZ
      EXTERNAL ToleranceStreamZ
      INTEGER  NetworkPrintLevel, MaxNetworkIterations
      EXTERNAL NetworkPrintLevel, MaxNetworkIterations

*   Intrinsics:
      INTEGER   INT
      real*8      ABS
      INTRINSIC INT, ABS

*   Programmed by: Lew DeLong
*   Date:          Sept  1993
*   Modified by:
*   Last modified:

*-----Implementation -----------------------------------------------------

      SpForce = .FALSE.
      Errors = .FALSE.

      MaxIterations = MaxNetworkIterations()
      Tolerance = ToleranceStreamZ()
      Channel   = CurrentChannel()

      IF( NetworkPrintLevel() .GT. 0  ) THEN
         Print = .TRUE.
      ELSE
         Print = .FALSE.
      END IF

      IF( Print ) THEN
         WRITE(*,*) ' '
         WRITE(*,*) ' Steady approximation, Channel...',Channel
      END IF

*-----Compute momentum flux at downstream end of channel.

      CxNo = NumberOfStreamLocations()
      Xd = StreamDistance(CxNo)
      Q  = StreamFlow(CxNo)
      Zd = StreamSurfaceElevation(CxNo)

      IF( Q .GT. 0.0 ) THEN
         e0 = StreamMoFlux(Xd,Q,Zd)
      ELSE
         WRITE(UNIT_ERROR,*) ' #### Warning(SpForce)...'
         WRITE(UNIT_ERROR,*) ' Channel...',Channel
         WRITE(UNIT_ERROR,*) ' Steady-state approximation not implemented for',
     &        ' negative flow.'
         RETURN
      END IF

*-----Compute initial dX.

      dX = Xd - StreamDistance(CxNo-1)

*-----Begin computational-location loop, at downstream end of channel.

      DO 300 I=2,NumberOfStreamLocations()

*--------Set reference distance and WSElev at adjacent upstream
*       computational location.

         CxNo = CxNo -1
         X = StreamDistance(CxNo)
         Z = StreamSurfaceElevation(CxNo)

*--------Begin spatial-convergence loop.

         CheckConvergence = .FALSE.

 50      CONTINUE

*--------Estimate initial water-surface slope.

         IF( I .GT. 2 ) THEN
            WSSlope = ( StreamSurfaceElevation(CxNo+2) - Zd )
     &           /( StreamDistance(CxNo+2) - Xd)

         ELSE
            WSSlope = (Zd - Z) / (Xd - X)
         END IF

*--------Determine number of subdivisions and corresponding dX.

         Subdivisions = INT( (Xd-X) / dX )
         IF( Subdivisions .LT. 1 ) THEN
            Subdivisions = 1
         END IF
         dX = (Xd-X) / DFLOAT(Subdivisions)

*--------Initialize values for subdivision loop.  Note, different
*       variables are used in the subdivision loop to simplify
*       restarting the loop from the same initial values when required.

         Xup = Xd
         Zup = Zd
         e0i = e0

*--------Begin subdivision loop.

         DO 200 J=1,Subdivisions

*-----------Initialize values for the iteration loop.

            Xdn = Xup
            Xup = Xup - dX
            Zdn = Zup
            Zup = Zdn - dX * WSSlope
            Iterations = 0

*-----------Begin iteration loop within the subdivision loop.

 100        CONTINUE
            Iterations = Iterations + 1

*-----------Begin computation of coefficients.

            c = dStreamMoFluxdZ(Xup,Q,Zup)
     &           - dgASfdZu(Xup,Xdn,Zup,Zdn,Q)
     &           - dgAdZdXdZu(Xup,Xdn,Zup,Zdn)

*-----------Update estimate of Z at the upstream end
*         of current the subdivision.

            dZ = (
     &           e0i-StreamMoFlux(Xup,Q,Zup)
     &           + gASf(Xup,Xdn,Zup,Zdn,Q)
     &           + gAdZdX(Xup,Xdn,Zup,Zdn)
     &           )  / c

            write(*,*) ' X, Q, Zup...',X, Q, Zup
            write(*,*) ' Xup, Xdn, Zdn...',Xup, Xdn, Zdn
            write(*,*) ' e0i...',e0i
            write(*,*) ' gASf...',gASf(Xup,Xdn,Zup,Zdn,Q)
            write(*,*) ' gAdZdX...',gAdZdX(Xup,Xdn,Zup,Zdn)
            write(*,*) ' StreamMoFlux...',StreamMoFlux(Xup,Q,Zup)
            write(*,*) ' c...',c

            Zup = Zup + dZ

*-----------Check for closure of iteration.

            IF( ABS( dZ ) .GT. Tolerance ) THEN
               IF( Iterations .LT. MaxIterations ) THEN
                  GO TO 100
               ELSE
                  Errors = .TRUE.

*-----------------Subdivide dX, if iteration hasn't closed.

                  dX = 0.5 * dX
                  IF( dX .GT. 1.0 ) THEN
                     GO TO 50
                  ELSE
                     WRITE(UNIT_ERROR,*) ' Giving up on channel...',Channel
                     OK = SetdXForce( -dX )
                     RETURN
                  END IF

               END IF
            END IF
*-----------End of iteration loop within subdivision loop.

*-----------Estimate new water-surface slope and momentum flux
*         for use in next subdivision.

            WSSlope = (Zdn - Zup) / (Xdn - Xup)
            e0i = e0i + gASf(Xup,Xdn,Zup,Zdn,Q)
     &           + gAdZdX(Xup,Xdn,Zup,Zdn)

 200     CONTINUE
*--------End of subdivision loop.

*--------Assign resulting final WSElev from the subdivision loop
*       to the adjacent upstream computational location.

         Z = Zup

*--------Check for convergence.

         Change = ABS(StreamSurfaceElevation(CxNo)-Z)
         OK = SetStreamSurfaceElevation( CxNo, Z )

         IF( CheckConvergence ) THEN
         ELSE
            CheckConvergence = .TRUE.
            dX = 0.5 * dX
            GO TO 50
         END IF

         IF( Change .LE. Tolerance ) THEN

*-----------If this is a successful test for convergence, the preceding
*         dX was sufficient ... change dX back to the preceding value.

            IF( CheckConvergence ) THEN
               dX = 2.0 * dX
            END IF

         ELSE

*-----------Convergence has not been obtained, reduce dX.

            Errors = .TRUE.
            dX = 0.5 * dX
            IF( dX .GT. 1.0 ) THEN
               GO TO 50
            ELSE
               WRITE(UNIT_ERROR,*) ' X...', X
               WRITE(UNIT_ERROR,*) ' Steady solution failed to converge...'
               WRITE(UNIT_ERROR,*) ' change, last iteration, ...',Change
               WRITE(UNIT_ERROR,*) ' giving up on this channel...'
               WRITE(UNIT_ERROR,*) ' '
               OK = SetdXForce( -dX )
               RETURN
            END IF

         END IF
*--------End of spatial-convergence loop.

*--------Reassign values for the next computational location.

         e0 = e0i
         Xd = X
         Zd = Z

 300  CONTINUE
*-----End of computational-location loop.

      IF( Errors ) THEN
         IF( Print ) THEN
            WRITE(UNIT_ERROR,*)
     &           ' For steady solution, initial dX reduced to ...',dX
         END IF
      END IF

      SpForce = .TRUE.

      OK = SetdXForce( dX )

      RETURN
      END

*== Public (StreamMoFlux) ================================================

      real*8 FUNCTION StreamMoFlux( X, Q, Z )

      IMPLICIT NONE

*   Purpose:  Approximate momentum flux in the current channel at X,
*             given flow (Q) and water-surface elevation (Z).

*   Arguments:
      real*8 X, Q, Z

*   Argument definitions:
*     X - downstream distance in current channel.
*     Q - discharge.
*     Z - water-surface elevation.

*   Module data:

*   Local Variables:
      real*8 H

*   Routines by module:

***** Local:

***** Channel properties:
      real*8     CxArea, BtmElev, Beta
      EXTERNAL CxArea, BtmElev, Beta

*   Intrinsics:

*   Programmed by: Lew DeLong
*   Date:          Sept  1993
*   Modified by:
*   Last modified:

*-----Implementation -----------------------------------------------------

      H = Z - BtmElev( X )
      IF( H .GT. 0.0 ) THEN
         StreamMoFlux = Beta(X,H) * Q ** 2 / CxArea(X,H)
      ELSE
         StreamMoFlux = 0.0
      END IF

      RETURN
      END

*== Public (dStreamMoFluxdZ) ================================================

      real*8 FUNCTION dStreamMoFluxdZ( X, Q, Z )

      IMPLICIT NONE

*   Purpose:  Approximate the gradient of momentum flux
*             with depth of flow.

      real*8 X, Q, Z

*   Argument definitions:
*     X - downstream distance in current channel.
*     Q - discharge.
*     Z - water-surface elevation.

*   Module data:

*   Local Variables:
      real*8 A, H

*   Routines by module:

***** Local:

***** Channel properties:
      real*8     CxArea, ChannelWidth, BtmElev, Beta, dBeta
      EXTERNAL CxArea, ChannelWidth, BtmElev, Beta, dBeta

*   Intrinsics:

*   Programmed by: Lew DeLong
*   Date:          Sept  1993
*   Modified by:
*   Last modified:

*-----Implementation -----------------------------------------------------

      H = Z - BtmElev( X )
      IF( H .GT. 0.0 ) THEN
         A = CxArea( X, H )
         dStreamMoFluxdZ = Q**2 / A * (
     &        - ChannelWidth( X,H ) * Beta(X,H) / A
     &        + dBeta(X,H)
     &        )
      ELSE
         dStreamMoFluxdZ = 0.0
      END IF

      RETURN
      END

*== Public (gASf) ================================================

      real*8 FUNCTION gASf( Xu, Xd, Zu, Zd, Q )

      IMPLICIT NONE

*   Purpose:  Approximate momentum loss due to flow resistance between
*             an upstream and downstream cross section.

*   Arguments:
      real*8 Xu, Xd, Zu, Zd, Q

*   Argument definitions:
*     Xu - down stream distance to upstream cross section.
*     Xd - down stream distance to downstream cross section.
*     Zu - upstream water-surface elevation.
*     Zd - downstream water-surface elevation.
*     Q  - discharge.

*   Module data:
      INCLUDE 'network.inc'

*   Local Variables:
      INTEGER K, QuadPts
      real*8 N( MaxQuadPts )
      real*8 Hd, Hu, G
      real*8 H, X, QuadWt, QuadPt, fric

*   Routines by module:

***** Local:

***** Channel properties:
      real*8     BtmElev, Conveyance, CxArea
      EXTERNAL BtmElev, Conveyance, CxArea

***** Network control:
      INTEGER  NetworkQuadPts
      EXTERNAL NetworkQuadPts, NetworkQuadPtWt

      real*8     AccelerationGravity
      EXTERNAL AccelerationGravity

*   Intrinsics:

*   Programmed by: Lew DeLong
*   Date:          Sept  1993
*   Modified by:
*   Last modified:

*-----Implementation -----------------------------------------------------

      Hu = Zu - BtmElev( Xu )
      Hd = Zd - BtmElev( Xd )

      G = AccelerationGravity()

      fric = 0.0
      QuadPts = NetworkQuadPts()

      DO 100 K=1,QuadPts

*--------Estimate quadrature-point values.

         CALL NetworkQuadPtWt( K, QuadPt, QuadWt )

*--------Interpolation functions.
         N(1) = 1.0 - QuadPt
         N(2) = QuadPt

*--------Location of quadrature point.
         X = N(1) * Xu + N(2) * Xd

*--------Dependent variables.
         H = N(1) * Hu + N(2) * Hd

         IF( H .GT. 0.0 ) THEN
            fric = fric + QuadWt * CxArea(X,H) / ( Conveyance(X,H) ** 2 )
         END IF

 100  CONTINUE

      gASf = G * Q * ABS( Q ) * fric * ( Xd - Xu )

      RETURN
      END

*== Public (dgASfdZu) ================================================

      real*8 FUNCTION dgASfdZu( Xu, Xd, Zu, Zd, Q )

      IMPLICIT NONE

*   Purpose:  Approximate gradient, with respect to a change in upstream
*             water-surface elevation, of momentum loss due to flow
*             resistance between an upstream and downstream cross section.

*   Arguments:
      real*8 Xu, Xd, Zu, Zd, Q

*   Argument definitions:
*     Xu - down stream distance to upstream cross section.
*     Xd - down stream distance to downstream cross section.
*     Zu - upstream water-surface elevation.
*     Zd - downstream water-surface elevation.
*     Q  - discharge.

*   Module data:
      INCLUDE 'network.inc'

*   Local Variables:
      INTEGER K, QuadPts
      real*8 N( MaxQuadPts )
      real*8 Hd, Hu, G
      real*8 H, X, QuadWt, QuadPt, fric
      real*8 Conv

*   Routines by module:

***** Local:

***** Channel properties:
      real*8     BtmElev, Conveyance, dConveyance
      EXTERNAL BtmElev, Conveyance, dConveyance

      real*8     CxArea, ChannelWidth
      EXTERNAL CxArea, ChannelWidth

***** Network control:
      INTEGER  NetworkQuadPts
      EXTERNAL NetworkQuadPts, NetworkQuadPtWt

      real*8     AccelerationGravity
      EXTERNAL AccelerationGravity

*   Intrinsics:

*   Programmed by: Lew DeLong
*   Date:          Sept  1993
*   Modified by:
*   Last modified:

*-----Implementation -----------------------------------------------------

      Hu = Zu - BtmElev( Xu )
      Hd = Zd - BtmElev( Xd )

      G = AccelerationGravity()

      fric = 0.0
      QuadPts = NetworkQuadPts()

      DO 100 K=1,QuadPts

*--------Estimate quadrature-point values.

         CALL NetworkQuadPtWt( K, QuadPt, QuadWt )

*--------Interpolation functions.
         N(1) = 1.0 - QuadPt
         N(2) = QuadPt

*--------Location of quadrature point.
         X = N(1) * Xu + N(2) * Xd

*--------Dependent variables.
         H = N(1) * Hu + N(2) * Hd

*--------Integrate friction term.

         IF( H .GT. 0.0 ) THEN
            Conv = Conveyance(X,H)
            fric = fric + QuadWt * N(1) *  (
     &           - 2.0 * dConveyance(X,H) * CxArea(X,H) / Conv
     &           + ChannelWidth(X,H)
     &           )
     &           / ( Conv ** 2 )

         END IF

 100  CONTINUE

      dgASfdZu = G * Q * ABS( Q ) * fric * ( Xd - Xu )

      RETURN
      END

*== Public (gAdZdX) ================================================

      real*8 FUNCTION gAdZdX( Xu, Xd, Zu, Zd )

      IMPLICIT NONE

*   Purpose:  Approximate momentum gain due to WS slope between
*             an upstream and downstream cross section.

*   Arguments:
      real*8 Xu, Xd, Zu, Zd

*   Argument definitions:
*     Xu - down stream distance to upstream cross section.
*     Xd - down stream distance to downstream cross section.
*     Zu - upstream water-surface elevation.
*     Zd - downstream water-surface elevation.

*   Module data:
      INCLUDE 'network.inc'

*   Local Variables:
      INTEGER K, QuadPts
      real*8 N( MaxQuadPts )
      real*8 dNdX( MaxQuadPts )
      real*8 Hd, Hu, G
      real*8 H, X, QuadWt, QuadPt, Slope, A

*   Routines by module:

***** Local:

***** Channel properties:
      real*8     BtmElev, CxArea
      EXTERNAL BtmElev, CxArea

***** Network control:
      INTEGER  NetworkQuadPts
      EXTERNAL NetworkQuadPts, NetworkQuadPtWt

      real*8     AccelerationGravity
      EXTERNAL AccelerationGravity

*   Intrinsics:

*   Programmed by: Lew DeLong
*   Date:          Sept  1993
*   Modified by:
*   Last modified:

*-----Implementation -----------------------------------------------------

      Hu = Zu - BtmElev( Xu )
      Hd = Zd - BtmElev( Xd )

      G = AccelerationGravity()

      A = 0.0
      QuadPts = NetworkQuadPts()

      DO 100 K=1,QuadPts

*--------Estimate quadrature-point values.

         CALL NetworkQuadPtWt( K, QuadPt, QuadWt )

*--------Interpolation functions.
         N(1) = 1.0 - QuadPt
         N(2) = QuadPt
         dNdX(1) = -1.0 / (Xu - Xd)
         dNdX(2) = - dNdX(1)

*--------Location of quadrature point.
         X = N(1) * Xu + N(2) * Xd

*--------Dependent variables.
         H = N(1) * Hu + N(2) * Hd

*--------WS Slope.

         Slope = dNdX(1) * Zu + dNdX(2) * Zd

         IF( H .GT. 0.0 ) THEN
            A = A + QuadWt * CxArea(X,H) * Slope
         END IF

 100  CONTINUE

      gAdZdX = G * A * (Xd - Xu)

      RETURN
      END

*== Public (dgAdZdXdZu) ================================================

      real*8 FUNCTION dgAdZdXdZu( Xu, Xd, Zu, Zd )

      IMPLICIT NONE

*   Purpose:  Approximate gradient, with respect to a change in upstream
*             water-surface elevation, of momentum gain due to WS
*             slope between an upstream and downstream cross section.

*   Arguments:
      real*8 Xu, Xd, Zu, Zd

*   Argument definitions:
*     Xu - down stream distance to upstream cross section.
*     Xd - down stream distance to downstream cross section.
*     Zu - upstream water-surface elevation.
*     Zd - downstream water-surface elevation.

*   Module data:
      INCLUDE 'network.inc'

*   Local Variables:
      INTEGER K, QuadPts
      real*8 N( MaxQuadPts )
      real*8 dNdX( MaxQuadPts )
      real*8 Hd, Hu, G
      real*8 H, X, QuadWt, QuadPt, A, Slope

*   Routines by module:

***** Local:

***** Channel properties:
      real*8     BtmElev
      EXTERNAL BtmElev

      real*8     CxArea, ChannelWidth
      EXTERNAL CxArea, ChannelWidth

***** Network control:
      INTEGER  NetworkQuadPts
      EXTERNAL NetworkQuadPts, NetworkQuadPtWt

      real*8     AccelerationGravity
      EXTERNAL AccelerationGravity

*   Intrinsics:

*   Programmed by: Lew DeLong
*   Date:          Sept  1993
*   Modified by:
*   Last modified:

*-----Implementation -----------------------------------------------------

      Hu = Zu - BtmElev( Xu )
      Hd = Zd - BtmElev( Xd )

      G = AccelerationGravity()

      QuadPts = NetworkQuadPts()

      A = 0.0
      DO 100 K=1,QuadPts

*--------Estimate quadrature-point values.

         CALL NetworkQuadPtWt( K, QuadPt, QuadWt )

*--------Interpolation functions.
         N(1) = 1.0 - QuadPt
         N(2) = QuadPt
         dNdX(1) = -1.0 / (Xd - Xu)
         dNdX(2) = - dNdX(1)

*--------Location of quadrature point.
         X = N(1) * Xu + N(2) * Xd

*--------Dependent variables.
         H = N(1) * Hu + N(2) * Hd

*--------WS Slope.

         Slope = dNdX(1) * Zu + dNdX(2) * Zd

*--------Integrate friction term.

         IF( H .GT. 0.0 ) THEN
            A = A + QuadWt * (
     &           N(1) * ChannelWidth(X,H) * Slope
     &           + dNdX(1) * CxArea(X,H)
     &           )

         END IF

 100  CONTINUE

      dgAdZdXdZu = G * A * (Xd - Xu)

      RETURN
      END

*==== EOF chstatus ======================================================
