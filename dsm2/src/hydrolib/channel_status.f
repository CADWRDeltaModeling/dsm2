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
      use IO_Units
      use runtime_data
      use iopath_data
      use grid_data
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

      WRITE(funit,900)dsm2_version,current_date
 900  FORMAT('Hydro Version ',a,
     &     /'The following data corresponds to   ',a14//)
      WRITE(fUnit,*) NumberOfChannels(),'/Channels'

*-----Loop on channels.

      DO 200 I=1,NumberOfChannels()

         IF( OpenChannel(I) ) THEN

            WRITE(fUnit,*) chan_geom(I).chan_no, '/Channel' ! write external number
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

      WRITE(funit,901)Nreser
 901  FORMAT(/i5,' /Number of Reservoir')
      DO I=1,Nreser
C--------Reservoir Stage, nodal flows
         WRITE(funit,902)i,res_geom(i).nnodes,Yres(i)
 902     FORMAT(I5,' /Reservoir Number'
     &        /I5,' /Connections'
     &        /5X,1P,E15.7,' /Yres')
         WRITE(funit,903)(J,QRes(I,J),J=1, res_geom(i).nnodes)
 903     FORMAT(I5,1P,E15.7,' /Connection, Qres')
      ENDDO

      CLOSE( fUnit )

      WriteNetworkRestartFile = .TRUE.

      RETURN
      END

*== Private (ReadNetworkInitialConditions) ==============================

      LOGICAL FUNCTION ReadNetworkInitialConditions()
      use IO_Units
      use runtime_data
      use grid_data
      use iopath_data
      IMPLICIT NONE

*   Purpose:  Read initial values of dependent variables.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chinitcd.inc'
      INCLUDE 'chconnec.inc'
      INCLUDE 'chnlcomp.inc'


*   Local Variables:
      INTEGER      Channels,extchan,intchan
      INTEGER      fUnit, I, K, J, IRes, NConnect, IConnect, nLoc
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
         intchan=i

         IF( ChannelNumber(I) .LE. NumberOfChannels()
     &        .AND.
     &        ChannelNumber(I) .GT. 0 ) THEN

            READ(fUnit,*) nLoc            ! Number of comp. points in channel, according to restart file
            if (nLoc .ne. NumberofCompLocations(intchan)) then
               write(unit_error,610) extchan, nLoc,
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
     &              extchan
               WRITE(UNIT_ERROR,*) ' Maximum number of loactions exceeded.'
               WRITE(UNIT_ERROR,*) ' Attempted...', K + Locations(I)
               WRITE(UNIT_ERROR,*) ' Allowed.....', MaxLocations
               RETURN
            END IF

         ELSE
            WRITE(UNIT_ERROR,*) ' ####Error(ReadNetworkInitialConditions)'
            WRITE(UNIT_ERROR,*) ' Read channel number ... ', extchan
            WRITE(UNIT_ERROR,*) '  must fall on or between 1 and ',
     &           NumberOfChannels()
            RETURN
         END IF

         InitialConditionIndex( intchan ) = I

 200  CONTINUE

      READ(funit,*)NresStart_File
      IF(NresStart_File.EQ.Nreser)THEN
         IF (restart_version .ne. ' ') then
c-----------restart file version supports reservoir connection flows
c-- fixme: this is a bad idea. Should include just the model state (Yres)
c          and then calculate derived variables.
            do I=1,Nreser
               read(funit,*)IRes ! reservoir number
               read(funit,*)NConnect   ! number of connections
               read(funit,*)Yres(IRes) ! reservoir stage
               if (NConnect .eq. res_geom(ires).nnodes) THEN
                  do K=1, res_geom(ires).nnodes
                     read(funit,*)IConnect,QRes(IRes,IConnect)
                  enddo
               else
                  write(UNIT_ERROR,901) IRes
 901              format('Error(ReadNetworkInitialConditions)'
     &                 /'Number of Reservoir Connections does not match with the Restart File:'
     &                 /'Reservoir ',I5)
                  return
               endif
            end do
         else                   ! no reservoir connection flows
            do I=1,Nreser
               read(funit,*)IRes,Yres(IRes)
            end do
         end if
      else
         write(UNIT_ERROR,902)
 902     format('Error(ReadNetworkInitialConditions)'/
     &        'Number of Reservoirs does not match with the Restart File'/)
         return
      endif

c          No interpolation is necessary, since the computational points are matched exactly	      

      ReadNetworkInitialConditions = .TRUE.

      RETURN
      END

*== Private (ApproxReadInitialConditions) ==============================

      LOGICAL FUNCTION ApproxReadInitialConditions()
      use IO_Units
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
      use IO_Units
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
      use IO_Units
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

         Q(J) = Value
         SetStreamFlow = .TRUE.

      ELSE

         WRITE(UNIT_ERROR,*) ' Range error...(SetStreamFlow)'
         SetStreamFlow = .False.

      END IF

      RETURN
      END

*== Public (StreamDepth) ===============================================

      real*8 FUNCTION StreamDepth(LocationNumber)
      use IO_Units
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
      use IO_Units
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
      use IO_Units
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
      use IO_Units
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
      Use PhysicalConstants, only: gravity
      use IO_Units
      use grid_data
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
      INCLUDE 'chnluser.inc'
      INCLUDE 'chconnec.inc'


*   Local Variables:
      INTEGER I, J, K
      INTEGER Channelnumber_L, UserLocations
      real*8    Velocity, delx, CrNo, dtr, G, FrNo
      real*8    CompLocation_lcl(MaxLocations)
      real*8    WidthRatio, Width, WSSlope, WSSlopeRatio, WSSlopeChange
      LOGICAL OK

*     CompLocation_lcl(j) - local downstream distance coordinate.
*     DummyArray(j) - a REAL value dependent upon context.
*     DummyArray2(j) - a REAL value dependent upon context.
*     DummyCharArray(j) - a CHARACTER value dependent upon context.

*   Routines by module:

***** Local:
      integer FirstLoc

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
      real*8   BtmElev, CxArea, ChannelWidth
      EXTERNAL BtmElev, CxArea, ChannelWidth

      real*8   Conveyance, dConveyance
      EXTERNAL Conveyance, dConveyance

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
      G = gravity

*-----Set initial-condition read flag to "not initialized".

      OK = .FALSE.
      OK = SetNetworkInitCndRead( OK )

      IF (Restart_Read) THEN    ! Read initial conditions from restart file
         IF( ReadNetworkInitialConditions() ) THEN
            InitCndInitialized = .TRUE.
            Q=InitialQ
            QOld=InitialQ
            WS=InitialWS
            YResOld=YRes
         ELSE
            WRITE(UNIT_ERROR,*) ' ####Error(ReadNetworkInitialConditions)'
            WRITE(UNIT_ERROR,*) ' Reading of initial conditions failed...'
            RETURN
         END IF
      ELSE                      ! Interpolate user points
         DO 100 I = 1, NumberOfChannels()
            Channelnumber_L = I
            IF( OpenChannel( Channelnumber_L )  ) THEN

*--------------Get computational stream locations.
               K = 0
               DO J=UpstreamPointer(),DownstreamPointer()
                  K = K + 1
                  CompLocation_lcl(J) = StreamDistance(K)
               END DO

*--------------Initial conditions approximated from user input.

               UserLocations = NUserInitLocations(branch)
               FirstLoc=FirstLocation(branch)

               if (FirstLoc .eq. 0) then ! no initial conditions at all
                  write(unit_error,*)'No default initial conditions or restart file'
                  return
               end if

*--------------Approximate streamflow and ws linearly from user input.

               CALL Linear1D(
     &              NumberOfStreamLocations(),
     &              CompLocation_lcl(UpstreamPointer()),
     &              UserLocations, InitialX(FirstLoc), InitialQ(FirstLoc),
     &              Q(UpstreamPointer())
     &              )

               CALL Linear1D(
     &              NumberOfStreamLocations(),
     &              CompLocation_lcl(UpstreamPointer()),
     &              UserLocations, InitialX(FirstLoc), InitialWS(FirstLoc),
     &              WS(UpstreamPointer())
     &              )
               OK = CloseChannel()
            END IF

 100     CONTINUE
      END IF

      do i=1,MaxLocations
         QOld(i)=Q(i)
      end do
      do i=1,NReser
         YResOld(i)=YRes(i)
      end do

      DO 200 I = 1, NumberOfChannels()

         Channelnumber_L = I
         
         IF( OpenChannel( Channelnumber_L )  ) THEN
*-----------Approximate depth of flow.

            DO  J=UpstreamPointer(),DownstreamPointer()
               H( J ) = WS( J ) - BtmElev( CompLocation_lcl( J ) )
            END DO


c-----------if (Restart_Read)
c-----------&           OK = ApproxReadInitialConditions()               WHY??????!!!!!!

*-----------Upstream end of channel.     WHY???????/

            K = 1
            J = UpstreamPointer()
            Velocity = Q(J) / CxArea( CompLocation_lcl(J), H(J) )
            delX = CompLocation_lcl(J+1) - CompLocation_lcl(J)
            WSSlope = ( WS(J+1) - WS(J) ) / delX

            FrNo = Velocity / SQRT( G * H(J) )
            CrNo = dtr * ( Velocity + SQRT( G * H(J) ) ) / delX
            Width = ChannelWidth( CompLocation_lcl(J), H(J) )

*-----------Intervening cross sections.

            IF( (DownstreamPointer() - UpstreamPointer()) .GT. 2) THEN
               DO 150 J=UpstreamPointer()+1,DownstreamPointer()-1
                  K = K + 1
                  Velocity = Q(J) / CxArea( CompLocation_lcl(J), H(J) )
                  delX = ( CompLocation_lcl(J+1) - CompLocation_lcl(J-1) )
                  WSSlope = ( WS(J+1) - WS(J-1) ) / delX
                  CrNo = dtr * ( Velocity + SQRT( G * H(J) ) ) / ( 0.5 * delX )
                  FrNo = Velocity / SQRT( G * H(J) )
                  delX   = CompLocation_lcl(J+1) - CompLocation_lcl(J)
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
            CrNo = dtr * ( Velocity + SQRT( G * H(J) ) ) / delX
            WSSlope = ( WS(J) - WS(J-1) ) / delX
            Width = ChannelWidth( CompLocation_lcl(J), H(J) )

*-----------Check friction / WS slope / dX relation.

            OK = CloseChannel()

         ELSE
            WRITE(UNIT_ERROR,*) ' Could not open channel...',
     &           chan_geom(Channelnumber_L).chan_no
            WRITE(UNIT_ERROR,*) ' (InitializeNetworkFlowValues)'
            RETURN
         END IF

 200  CONTINUE

      InitializeNetworkFlowValues = .TRUE.

      RETURN
      END

*== Public (OldStreamDensity) ================================================

      real*8 FUNCTION OldStreamDensity(LocationNumber)
      use IO_Units
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
      use IO_Units
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
      use IO_Units
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
      use IO_Units
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
      use IO_Units
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
      use IO_Units
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
      use IO_Units
      IMPLICIT NONE

*   Purpose:  Set stream density to 1.0 for all channels.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chstatus.inc'

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



*== Public (StreamEnergy) ================================================

      real*8 FUNCTION StreamEnergy( X, Q, Z )
      Use PhysicalConstants,only: gravity
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
      real*8 Depth, Velocity

*   Routines by module:

***** Local:

***** Network control:


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
         Velocity = Q / CxArea( X, Depth )
         StreamEnergy = Z + Velocity**2 / (2.D0 * gravity)
      ELSE
         StreamEnergy = 0.0
      END IF

      RETURN
      END

*== Public (dStreamEnergydZ) ================================================

      real*8 function dStreamEnergydZ( X, Q, Z )
      use PhysicalConstants,only: gravity
      implicit none
*   Purpose:  Approximate the gradient of specific energy
*             with depth of flow.

      real*8 X, Q, Z

*   Argument definitions:
*     X - downstream distance in current channel.
*     Q - discharge.
*     Z - water-surface elevation.

*   Module data:

*   Local Variables:
      real*8 A, Depth, Velocity

*   Routines by module:

***** Local:

***** Network control:

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
         A = CxArea( X, Depth )
         Velocity = Q / A
         dStreamEnergydZ = 1.0
     &        - ChannelWidth( X,Depth ) * Velocity**2 /(A*gravity)
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

      real*8 function gASf( Xu, Xd, Zu, Zd, Q )
      use PhysicalConstants, only: gravity
      implicit none

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
      include 'network.inc'

*   Local Variables:
      integer K, QuadPts
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


*   Intrinsics:

*   Programmed by: Lew DeLong
*   Date:          Sept  1993
*   Modified by:
*   Last modified:

*-----Implementation -----------------------------------------------------

      Hu = Zu - BtmElev( Xu )
      Hd = Zd - BtmElev( Xd )

      G = gravity

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

      real*8 function dgASfdZu( Xu, Xd, Zu, Zd, Q )
      use PhysicalConstants,only: gravity
      implicit none

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
      include 'network.inc'

*   Local Variables:
      integer K, QuadPts
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



*   Intrinsics:

*   Programmed by: Lew DeLong
*   Date:          Sept  1993
*   Modified by:
*   Last modified:

*-----Implementation -----------------------------------------------------

      Hu = Zu - BtmElev( Xu )
      Hd = Zd - BtmElev( Xd )

      G = gravity

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
      Use PhysicalConstants,only: gravity
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



*   Intrinsics:

*   Programmed by: Lew DeLong
*   Date:          Sept  1993
*   Modified by:
*   Last modified:

*-----Implementation -----------------------------------------------------

      Hu = Zu - BtmElev( Xu )
      Hd = Zd - BtmElev( Xd )

      G = gravity

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
      Use PhysicalConstants,only: gravity
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


*   Intrinsics:

*   Programmed by: Lew DeLong
*   Date:          Sept  1993
*   Modified by:
*   Last modified:

*-----Implementation -----------------------------------------------------

      Hu = Zu - BtmElev( Xu )
      Hd = Zd - BtmElev( Xd )

      G = gravity

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
