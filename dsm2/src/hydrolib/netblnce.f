C!<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    The Delta Simulation Model 2 (DSM2) is free software: 
C!    you can redistribute it and/or modify
C!    it under the terms of the GNU General Public License as published by
C!    the Free Software Foundation, either version 3 of the License, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public License for more details.

C!    You should have received a copy of the GNU General Public License
C!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
C!</license>

*===== BOF netblnce ====================================================

*   Module data:

*    'network.inc'
*     MaxChannels - maximum number of channels.
*     NumCh - current number of channels.
*     Branch - current selected or active channel.
*     MaxLocations - maximum number of computational or user locations.

*    'netblnce.inc'
*     InitialVolume(i) - initial volume of fluid in channel i.
*     InitialMass(i) - initial mass of fluid in channel i.
*     Volume(i) - current volume of fluid in channel i.
*     Mass(i) - current mass of fluid in channel i.
*     JnctVolFlow(i) - net change of volume through ends of channel i.
*     JnctMassFlow(i) - net change of mass through ends of channel i.
*     LateralVolFlow(i) - net change of volume resulting from flows
*                         distributed along reaches of channel i.
*     LateralVolMass(i) - net change of mass resulting from flows
*                         distributed along reaches of channel i.

*== Public (InitNetBalance) ============================================

      LOGICAL FUNCTION InitNetBalance()

      IMPLICIT NONE

*   Purpose: Initialize  volume (and mass balance if variable density)
*            module for a network of open channels.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netblnce.inc'

*   Local Variables:
      INTEGER I, J, K, QuadPts, Locations
      REAL*8    dT, OneMinusTheta
      REAL*8    Pt, Wt
      REAL*8    X1, X2, X, dX
      REAL*8    Z1, Z2, Z
      LOGICAL OK

*   Routines by module:

***** Channel schematic data;
      INTEGER  NumberOfChannels, NumberOfStreamLocations
      REAL*8     StreamDistance
      LOGICAL  OpenChannel, CloseChannel
      EXTERNAL OpenChannel, CloseChannel
      EXTERNAL NumberOfChannels, StreamDistance
      EXTERNAL NumberOfStreamLocations

***** Network control:
      INTEGER  NetworkTimeIncrement
      REAL*8     NetworkTheta
      EXTERNAL NetworkTimeIncrement, NetworkTheta
      LOGICAL  VariableStreamDensity
      EXTERNAL VariableStreamDensity
      INTEGER  NetworkQuadPts
      EXTERNAL NetworkQuadPts, NetworkQuadPtWt

***** Channel properties:
      REAL*8     CxArea
      EXTERNAL CxArea

***** Channel status:
      REAL*8     StreamFlow
      EXTERNAL StreamFlow
      REAL*8     EstOldStreamDensity, OldStreamDensity
      EXTERNAL EstOldStreamDensity, OldStreamDensity
      real*8     StreamSurfaceElevation
      EXTERNAL StreamSurfaceElevation

***** Local:


*   Programmed by: Lew DeLong
*   Date:          May   1992
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

*-----Get number of quadrature points for numerical integration.

      QuadPts = NetworkQuadPts()
      dT = DFLOAT(NetworkTimeIncrement())
      OneMinusTheta = 1.0 - NetworkTheta()

      IF( VariableStreamDensity() ) THEN

*--------Density is variable.

         DO 400 I=1,NumberOfChannels()

            OK = OpenChannel(I)
            Locations = NumberOfStreamLocations()

            InitialVolume(I) = 0.0
            InitialMass(I)   = 0.0
            JnctVolFlow(I)   = OneMinusTheta * dT * (
     &           StreamFlow(1) - StreamFlow( Locations )
     &           )

            JnctMassFlow(I)  = OneMinusTheta * dT * (
     &           StreamFlow(1) * OldStreamDensity(1)
     &           - StreamFlow( Locations ) * OldStreamDensity(Locations)
     &           )

            X1 = StreamDistance(1)
!            H1 = StreamDepth(1)
            Z1 = StreamSurfaceElevation(1)

            DO 300 J=1,Locations-1

               X2 = StreamDistance(J+1)
!               H2 = StreamDepth(J+1)
               Z2 = StreamSurfaceElevation(J+1)

               dX = X2 - X1

               DO 200 K=1,QuadPts

                  CALL NetworkQuadPtWt( K, Pt, Wt )
                  X = (1.0-Pt)*X1 + Pt*X2

*-----------------Assume Z varies linearly with channel length.
                  Z = (1.0-Pt)*Z1 + Pt*Z2

                  InitialVolume(I) = InitialVolume(I)
     &                 + CxArea( X, Z ) * dX * Wt

                  InitialMass(I) = InitialMass(I)
     &                 + EstOldStreamDensity( X ) * dX * Wt

 200           CONTINUE

               X1 = X2
               Z1 = Z2

 300        CONTINUE

            OK = CloseChannel()

 400     CONTINUE

      ELSE

*--------Density is constant.

         DO 800 I=1,NumberOfChannels()

            OK = OpenChannel(I)
            Locations = NumberOfStreamLocations()

            InitialVolume(I) = 0.0
            JnctVolFlow(I)   = OneMinusTheta * dT * (
     &           StreamFlow(1) - StreamFlow( Locations )
     &           )

            X1 = StreamDistance(1)
!            H1 = StreamDepth(1)
            Z1 = StreamSurfaceElevation(1)            
		
            DO 700 J=1,NumberOfStreamLocations()-1

               X2 = StreamDistance(J+1)
!               H2 = StreamDepth(J+1)
               Z2 = StreamSurfaceElevation(J+1)

               dX = X2 - X1

               DO 600 K=1,QuadPts

                  CALL NetworkQuadPtWt( K, Pt, Wt )
                  X = (1.0-Pt)*X1 + Pt*X2

*-----------------Assume Z varies linearly with channel length.
                  Z = (1.0-Pt)*Z1 + Pt*Z2

                  InitialVolume(I) = InitialVolume(I)
     &                 + CxArea( X, Z ) * dX * Wt

 600           CONTINUE

               X1 = X2
               Z1 = Z2

 700        CONTINUE

            OK = CloseChannel()

 800     CONTINUE

      END IF

      InitNetBalance = .TRUE.

      RETURN
      END

*== Public (UpdateNetBalance) ================================================

      LOGICAL FUNCTION UpdateNetBalance()

      IMPLICIT NONE

*   Purpose:  Compute net inflow to a network of open channels.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netblnce.inc'

*   Local Variables:
      INTEGER I, Locations
      REAL*8    dT
      LOGICAL OK

*   Routines by module:

***** Channel schematic data;
      INTEGER  NumberOfChannels, NumberOfStreamLocations
      LOGICAL  OpenChannel, CloseChannel
      EXTERNAL OpenChannel, CloseChannel
      EXTERNAL NumberOfChannels
      EXTERNAL NumberOfStreamLocations

***** Network control:
      INTEGER  NetworkTimeIncrement
      LOGICAL  VariableStreamDensity
      EXTERNAL VariableStreamDensity, NetworkTimeIncrement

***** Channel status:
      REAL*8     StreamFlow, NewStreamDensity, OldStreamDensity
      EXTERNAL StreamFlow, NewStreamDensity, OldStreamDensity

***** Local:


*   Programmed by: Lew DeLong
*   Date:          May   1992
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      UpdateNetBalance = .TRUE.

      dT = DFLOAT(NetworkTimeIncrement())

      IF( VariableStreamDensity() ) THEN

         DO 100 I=1,NumberOfChannels()

            OK = OpenChannel(I)

            Locations = NumberOfStreamLocations()

            JnctMassFlow(I) = JnctMassFlow(I)
     &           + (
     &           StreamFlow(1) * NewStreamDensity(1)
     &           - StreamFlow(Locations) * NewStreamDensity(Locations)
     &           ) * dT

            JnctVolFlow(I) = JnctVolFlow(I)
     &           + (
     &           StreamFlow(1)
     &           - StreamFlow( Locations )
     &           ) * dT

            OK = CloseChannel()

 100     CONTINUE

      ELSE

         DO 300 I=1,NumberOfChannels()

            OK = OpenChannel(I)

            Locations = NumberOfStreamLocations()

            JnctVolFlow(I) = JnctVolFlow(I)
     &           + (
     &           StreamFlow(1)
     &           - StreamFlow( Locations )
     &           ) * dT

            OK = CloseChannel()

 300     CONTINUE

      END IF

      RETURN
      END

*== Public (ReportNetBalance) ================================================

      LOGICAL FUNCTION ReportNetBalance()
      use IO_Units
      IMPLICIT NONE

*   Purpose: Compute and report volume (and mass balance if variable density)
*            for a network of open channels.

*   Purpose: Initialize  volume (and mass balance if variable density)
*            module for a network of open channels.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netblnce.inc'

*   Local Variables:
      INTEGER I, J, K, QuadPts, Locations, U
      REAL*8    OneMinusTheta, dT
      REAL*8    Pt, Wt
      REAL*8    X1, X2, X, dX
      REAL*8    Z1, Z2, Z
      REAL*8    Difference, TotalInitialVolume, TotalVolume
      REAL*8    TotalJnctVolFlow, TotalLateralVolFlow
      REAL*8    TotalInitialMass, TotalMass
      REAL*8    TotalJnctMassFlow, TotalLateralMassFlow
      REAL*8    TotalDifference
      LOGICAL OK

*   Routines by module:

***** Channel schematic data;
      INTEGER  NumberOfChannels, NumberOfStreamLocations
      REAL*8     StreamDistance
      LOGICAL  OpenChannel, CloseChannel
      EXTERNAL OpenChannel, CloseChannel
      EXTERNAL NumberOfChannels, StreamDistance
      EXTERNAL NumberOfStreamLocations

***** Network control:
      INTEGER  NetworkTimeIncrement
      REAL*8     NetworkTheta
      EXTERNAL NetworkTimeIncrement, NetworkTheta
      LOGICAL  VariableStreamDensity
      EXTERNAL VariableStreamDensity
      INTEGER  NetworkQuadPts
      EXTERNAL NetworkQuadPts, NetworkQuadPtWt

***** Channel properties:
      REAL*8     CxArea
      EXTERNAL CxArea

***** Channel status:
      REAL*8     StreamFlow
      EXTERNAL StreamFlow
      REAL*8     EstNewStreamDensity, NewStreamDensity
      EXTERNAL EstNewStreamDensity, NewStreamDensity
      real*8     StreamSurfaceElevation
      EXTERNAL StreamSurfaceElevation

***** Local:


*   Programmed by: Lew DeLong
*   Date:          May   1992
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      U = unit_output

*-----Get number of quadrature points for numerical integration.

      QuadPts = NetworkQuadPts()
      OneMinusTheta = 1.0 - NetworkTheta()
      dT =  DFLOAT(NetworkTimeIncrement())

      IF( VariableStreamDensity() ) THEN

*--------Density is variable.

         DO 400 I=1,NumberOfChannels()

            Volume(I) = 0.0
            Mass(I)   = 0.0

            OK = OpenChannel(I)
            Locations = NumberOfStreamLocations()

            JnctVolFlow(I)   = JnctVolFlow(I)
     &           - OneMinusTheta * dT * (
     &           StreamFlow(1) - StreamFlow( Locations )
     &           )

            JnctMassFlow(I)  = OneMinusTheta * dT * (
     &           StreamFlow(1) * NewStreamDensity(1)
     &           - StreamFlow( Locations ) * NewStreamDensity(Locations)
     &           )
            X1 = StreamDistance(1)
!            H1 = StreamDepth(1)
            Z1 = StreamSurfaceElevation(1)

            DO 300 J=1,Locations-1

               X2 = StreamDistance(J+1)
!               H2 = StreamDepth(J+1)
               Z2 = StreamSurfaceElevation(J+1)

               dX = X2 - X1

               DO 200 K=1,QuadPts

                  CALL NetworkQuadPtWt( K, Pt, Wt )
                  X = (1.0-Pt)*X1 + Pt*X2

*-----------------Assume Z varies linearly with channel length.
                  Z = (1.0-Pt)*Z1 + Pt*Z2

                  Volume(I) = Volume(I)
     &                 + CxArea( X, Z ) * dX * Wt

                  Mass(I) = Mass(I)
     &                 + EstNewStreamDensity( X ) * dX * Wt

 200           CONTINUE

               X1 = X2
               Z1 = Z2

 300        CONTINUE

            OK = CloseChannel()

 400     CONTINUE

      ELSE

*--------Density is constant.

         DO 800 I=1,NumberOfChannels()

            Volume(I) = 0.0

            OK = OpenChannel(I)
            Locations = NumberOfStreamLocations()

            JnctVolFlow(I)   = JnctVolFlow(I)
     &           - OneMinusTheta * dT * (
     &           StreamFlow(1) - StreamFlow( Locations )
     &           )

            X1 = StreamDistance(1)
!            H1 = StreamDepth(1)
            Z1 = StreamSurfaceElevation(1)

            DO 700 J=1,NumberOfStreamLocations()-1

               X2 = StreamDistance(J+1)
!               H2 = StreamDepth(J+1)
               Z2 = StreamSurfaceElevation(J+1)

               dX = X2 - X1

               DO 600 K=1,QuadPts

                  CALL NetworkQuadPtWt( K, Pt, Wt )
                  X = (1.0-Pt)*X1 + Pt*X2

*-----------------Assume Z varies linearly with channel length.
                  Z = (1.0-Pt)*Z1 + Pt*Z2

                  Volume(I) = Volume(I)
     &                 + CxArea( X, Z ) * dX * Wt

 600           CONTINUE

               X1 = X2
               Z1 = Z2

 700        CONTINUE

            OK = CloseChannel()

 800     CONTINUE

      END IF

*-----Write results to screen and print file.

      IF(VariableStreamDensity() ) THEN

         WRITE(U,*) ' '
         WRITE(U,*) ' '
         WRITE(U,*) '                                 ___Mass Balance___'
         WRITE(U,*) ' '
         WRITE(U,*)
     &        ' Channel   Initial mass.   Final mass.    Junction',
     &        '       Lateral    Difference'
         WRITE(U,*) ' '

         TotalDifference      = 0.0
         TotalInitialMass     = 0.0
         TotalMass            = 0.0
         TotalJnctMassFlow    = 0.0
         TotalLateralMassFlow = 0.0

         DO 900 I=1,NumberOfChannels()

            Difference = Mass(I) - InitialMass(I) - JnctMassFlow(I)
     &           - LateralMassFlow(I)

            TotalDifference = TotalDifference + Difference
            TotalInitialMass = TotalInitialMass + InitialMass(I)
            TotalMass = TotalMass + Mass(I)
            TotalJnctMassFlow = TotalJnctMassFlow + JnctMassFlow(I)
            TotalLateralMassFlow = TotalLateralMassFlow
     &           + LateralMassFlow(I)

            IF( NumberOfChannels() .GT. 1 ) THEN
               WRITE(U,'(1X,I7,5F14.2)')  I,
     &              InitialMass(I), Mass(I), JnctMassFlow(I),
     &              LateralMassFlow(I),  Difference
            END IF

 900     CONTINUE

         IF( NumberOfChannels() .GT. 1 ) THEN
            WRITE(U,*)
     &           ' -------------------------------------------------------------'
     &           ,'--------------------'
         END IF

         WRITE(U,'(8X,5F14.2)')
     &        TotalInitialMass, TotalMass, TotalJnctMassFlow,
     &        TotalLateralMassFlow, TotalDifference

      END IF

      WRITE(U,*) ' '
      WRITE(U,*) ' '
      WRITE(U,*) '                               ___Volume Balance___'
      WRITE(U,*) ' '
      WRITE(U,*)
     &     ' Channel   Initial vol.    Final vol.     Junction',
     &     '       Lateral    Difference'
      WRITE(U,*) ' '

      TotalDifference     = 0.0
      TotalInitialVolume  = 0.0
      TotalVolume         = 0.0
      TotalJnctVolFlow    = 0.0
      TotalLateralVolFlow = 0.0

      DO 1000 I=1,NumberOfChannels()

         Difference = Volume(I) - InitialVolume(I) - JnctVolFlow(I)
     &        - LateralVolFlow(I)

         TotalDifference = TotalDifference + Difference
         TotalInitialVolume = TotalInitialVolume + InitialVolume(I)
         TotalVolume = TotalVolume + Volume(I)
         TotalJnctVolFlow = TotalJnctVolFlow + JnctVolFlow(I)
         TotalLateralVolFlow = TotalLateralVolFlow
     &        + LateralVolFlow(I)

         IF( NumberOfChannels() .GT. 1 ) THEN
            WRITE(U,'(1X,I7,5F14.2)') I,
     &           InitialVolume(I), Volume(I), JnctVolFlow(I),
     &           LateralVolFlow(I),  Difference
         END IF

 1000 CONTINUE

      IF( NumberOfChannels() .GT. 1 ) THEN
         WRITE(U,*)
     &        ' -------------------------------------------------------------'
     &        ,'--------------------'
      END IF

      WRITE(U,'(8X,5F14.2)')
     &     TotalInitialVolume, TotalVolume, TotalJnctVolFlow,
     &     TotalLateralVolFlow, TotalDifference
      WRITE(U,*) ' '

      ReportNetBalance = .TRUE.

      RETURN
      END

*===== EOF netblnce ====================================================
