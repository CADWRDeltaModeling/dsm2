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
      REAL*8    H1, H2, H
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
      REAL*8     StreamDepth, EstOldStreamDensity, OldStreamDensity
      EXTERNAL StreamDepth, EstOldStreamDensity, OldStreamDensity

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
            H1 = StreamDepth(1)

            DO 300 J=1,Locations-1

               X2 = StreamDistance(J+1)
               H2 = StreamDepth(J+1)

               dX = X2 - X1

               DO 200 K=1,QuadPts

                  CALL NetworkQuadPtWt( K, Pt, Wt )
                  X = (1.0-Pt)*X1 + Pt*X2

*-----------------Assume H varies linearly with channel length.
                  H = (1.0-Pt)*H1 + Pt*H2

                  InitialVolume(I) = InitialVolume(I)
     &                 + CxArea( X, H ) * dX * Wt

                  InitialMass(I) = InitialMass(I)
     &                 + EstOldStreamDensity( X ) * dX * Wt

 200           CONTINUE

               X1 = X2
               H1 = H2

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
            H1 = StreamDepth(1)
		
            DO 700 J=1,NumberOfStreamLocations()-1

               X2 = StreamDistance(J+1)
               H2 = StreamDepth(J+1)

               dX = X2 - X1

               DO 600 K=1,QuadPts

                  CALL NetworkQuadPtWt( K, Pt, Wt )
                  X = (1.0-Pt)*X1 + Pt*X2

*-----------------Assume H varies linearly with channel length.
                  H = (1.0-Pt)*H1 + Pt*H2

                  InitialVolume(I) = InitialVolume(I)
     &                 + CxArea( X, H ) * dX * Wt

 600           CONTINUE

               X1 = X2
               H1 = H2

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
      REAL*8    H1, H2, H
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
      REAL*8     StreamDepth, EstNewStreamDensity, NewStreamDensity
      EXTERNAL StreamDepth, EstNewStreamDensity, NewStreamDensity

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
            H1 = StreamDepth(1)

            DO 300 J=1,Locations-1

               X2 = StreamDistance(J+1)
               H2 = StreamDepth(J+1)

               dX = X2 - X1

               DO 200 K=1,QuadPts

                  CALL NetworkQuadPtWt( K, Pt, Wt )
                  X = (1.0-Pt)*X1 + Pt*X2

*-----------------Assume H varies linearly with channel length.
                  H = (1.0-Pt)*H1 + Pt*H2

                  Volume(I) = Volume(I)
     &                 + CxArea( X, H ) * dX * Wt

                  Mass(I) = Mass(I)
     &                 + EstNewStreamDensity( X ) * dX * Wt

 200           CONTINUE

               X1 = X2
               H1 = H2

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
            H1 = StreamDepth(1)

            DO 700 J=1,NumberOfStreamLocations()-1

               X2 = StreamDistance(J+1)
               H2 = StreamDepth(J+1)

               dX = X2 - X1

               DO 600 K=1,QuadPts

                  CALL NetworkQuadPtWt( K, Pt, Wt )
                  X = (1.0-Pt)*X1 + Pt*X2

*-----------------Assume H varies linearly with channel length.
                  H = (1.0-Pt)*H1 + Pt*H2

                  Volume(I) = Volume(I)
     &                 + CxArea( X, H ) * dX * Wt

 600           CONTINUE

               X1 = X2
               H1 = H2

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
