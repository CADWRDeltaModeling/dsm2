<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    DSM2 is free software: you can redistribute it and/or modify
C!    it under the terms of the GNU General Public License as published by
C!    the Free Software Foundation, either version 3 of the License, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public License for more details.

C!    You should have received a copy of the GNU General Public License
C!    along with DSM2.  If not, see <http://www.gnu.org/licenses/>.
</license>

*===== BOF netdense =====================================================

*== Private (ReadNetworkDensity) ===================================

      LOGICAL FUNCTION ReadNetworkDensity()
      use IO_Units
      IMPLICIT NONE

*   Purpose:  Read density values at channel ends.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netdense.inc'

*   Local Variables:
      INTEGER      I

*   Routines by module:

***** Network schmatic:
      INTEGER  NumberOfChannels
      EXTERNAL NumberOfChannels

*   Intrinsics:

*   Programmed by: Lew DeLong
*   Date:          October  1991
*   Modified by:   Barry Wicktom (use of master file names added)
*   Last modified: 1/8/92
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      ReadNetworkDensity = .FALSE.

      OldTime = NewTime
      NewTime = NewTime + dT
      DO 100 I=1,2*NumberOfChannels()
         Old(I) = New(I)
 100  CONTINUE

C-----WRITE(*,*) ' '
C-----WRITE(*,*) ' Reading density...'
C-----WRITE(*,*) ' OldTime....', OldTime
C-----WRITE(*,*) ' NewTime....', NewTime
C-----WRITE(*,*) ' Old values.', (Old(I),I=1,2*NumberOfChannels())

      READ(FortranUnit,*,END=200) (New(I),I=1,2*NumberOfChannels())
C-----WRITE(*,*) ' New values.',  (New(I),I=1,2*NumberOfChannels())

      GO TO 202
 200  CONTINUE
      WRITE(UNIT_ERROR,*) ' ####error(ReadNetworkDensity)'
      WRITE(UNIT_ERROR,*) ' New values.',  (New(I),I=1,2*NumberOfChannels())
      WRITE(UNIT_ERROR,*) ' Unexpected end of file.'
      WRITE(UNIT_ERROR,*) ' Possibly channel...',(I+1)/2
      WRITE(UNIT_ERROR,*) ' Abnormal program end.'
      CALL EXIT(1)
 202  CONTINUE

      ReadNetworkDensity = .TRUE.

      RETURN
      END

*== Public (SetNewNetworkDensity) ================================================

      LOGICAL FUNCTION SetNewNetworkDensity()
      use IO_Units
      IMPLICIT NONE

*   Purpose:  Set water density in a network of open channels.
*             Density is assumed to vary linearly with time
*             (over a density time increment) and with space
*             (from one end of a channel to the other).

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netdense.inc'


*   Local Variables:
      INTEGER I, Channel
      REAL*8 UpstreamDensity, DownstreamDensity
      REAL*8 Shape1, Shape2
      LOGICAL OK

*   Routines by module:

***** Local:
      LOGICAL  ReadNetworkDensity
      EXTERNAL ReadNetworkDensity
      LOGICAL  SetNewLinearStreamDensity
      EXTERNAL SetNewLinearStreamDensity

***** Channel schematic:
      LOGICAL  OpenChannel, CloseChannel
      INTEGER  NumberOfChannels
      EXTERNAL NumberOfChannels, OpenChannel, CloseChannel

***** Channel control:
      INTEGER  NetworkSeconds
      EXTERNAL NetworkSeconds

*   Intrinsics:

*   Programmed by: Lew DeLong
*   Date:          October  1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      SetNewNetworkDensity = .FALSE.

      CurrentTime = NetworkSeconds()

 100  CONTINUE

*-----Compute current density values at channel ends.

C     WRITE(*,*) ' '
C     WRITE(*,*) ' SetNewNetworkDensity...'
C     WRITE(*,*) ' OldTime.......',OldTime
C     WRITE(*,*) ' CurrentTime...',CurrentTime
C     WRITE(*,*) ' NewTime.......',NewTime
      IF( CurrentTime .GT. OldTime ) THEN
         IF( CurrentTime .LT. NewTime ) THEN

*-----------Interpolate current values.

            Shape1 = FLOAT(NewTime - CurrentTime)/
     &           FLOAT(NewTime - OldTime)
            Shape2 = 1.0 - Shape1

            DO 200 I=1,2*NumberOfChannels()
               Current(I) = Shape1*Old(I) + Shape2*New(I)
C--------------WRITE(*,*) I,Current(I)
 200        CONTINUE

         ELSE IF( CurrentTime .EQ. NewTime ) THEN

*-----------Assign current values.

            DO 300 I=1,2*NumberOfChannels()
               Current(I) = New(I)
C--------------WRITE(*,*) I,Current(I)
 300        CONTINUE

         ELSE

*-----------Read new values.

            OK = ReadNetworkDensity()

            GO TO 100

         END IF
      ELSE IF(CurrentTime .EQ. OldTime ) THEN

*--------Read new values.

         OK = ReadNetworkDensity()

         GO TO 100

      ELSE

*--------Shouldn't ever get here!

         WRITE(UNIT_ERROR,*) ' ####error(SetNewNetDensity)'
         WRITE(UNIT_ERROR,*) ' Current time < available from boundary values.'
         WRITE(UNIT_ERROR,*) ' CurrentTime...',CurrentTime
         WRITE(UNIT_ERROR,*) ' OldTime.......',OldTime
         WRITE(UNIT_ERROR,*) ' Abnormal program end.'
         CALL EXIT(1)
      END IF

      DO 400 I=1,NumberOfChannels()

         Channel = I
         OK = OpenChannel(Channel)

         UpstreamDensity = Current(2*I-1)
         DownstreamDensity = Current(2*I)

         IF( SetNewLinearStreamDensity(
     &        UpstreamDensity, DownstreamDensity)
     &        ) THEN
         ELSE

            WRITE(UNIT_ERROR,*) ' ####Error(SetNetworkDensity)'
            WRITE(UNIT_ERROR,*) ' Water density not set, channel...',Channel
            RETURN

         END IF

         OK = CloseChannel()

 400  CONTINUE

      SetNewNetworkDensity = .TRUE.

      RETURN
      END

*== Private (SetNewLinearStreamDensity) ====================================

      LOGICAL FUNCTION SetNewLinearStreamDensity(
     &     UpstreamDensity, DownstreamDensity
     &     )

      IMPLICIT NONE

*   Purpose:  Set stream density to values linearly interpolated from
*              values supplied at extremities of current channel, at
*              the end of the current time step.

*   Arguments:
      REAL*8 UpstreamDensity, DownstreamDensity

*   Argument definitions:
*     UpstreamDensity - density at the upstream end of current channel.
*     DownstreamDensity - density at downstream end of current channel.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netdense.inc'

*   Local Variables:
      INTEGER I, N
      REAL*8    XUp, dX, Shape, Rho
      LOGICAL OK

*   Routines by module:

***** Channel schematic:
      INTEGER  UpstreamPointer, DownstreamPointer
      INTEGER  NumberOfStreamLocations
      REAL*8     StreamDistance
      EXTERNAL UpstreamPointer, DownstreamPointer
      EXTERNAL NumberOfStreamLocations
      EXTERNAL StreamDistance

***** Channel status:
      REAL*8     NewStreamDensity
      LOGICAL  SetOldStreamDensity, SetNewStreamDensity
      EXTERNAL NewStreamDensity
      EXTERNAL SetOldStreamDensity, SetNewStreamDensity
*   Intrinsics:

*   Programmed by: Lew DeLong
*   Date:          October  1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      XUp = StreamDistance(1)
      dX = StreamDistance( NumberOfStreamLocations() ) - XUp

      N = 0
      DO 100 I=UpstreamPointer(),DownstreamPointer()
         N = N + 1

         OK = SetOldStreamDensity( N, NewStreamDensity(N) )

         Shape = ( StreamDistance(N) - XUp ) / dX
         Rho = (1.0 - Shape) * UpstreamDensity
     &        + Shape * DownstreamDensity
         OK = SetNewStreamDensity( N, Rho )

 100  CONTINUE

      SetNewLinearStreamDensity = .TRUE.

      RETURN
      END

*===== EOF netdense =====================================================
