C!    Copyright (C) 1996, 1997 State of California, Department of Water
C!    Resources.
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

*==== BOF floweq1d =====================================================

*== Public (DynamicWaveEq) =============================================

      LOGICAL FUNCTION DynamicWaveEq(Up, Down)

      IMPLICIT NONE

*   Purpose:  Compute and store coefficients of an integral form
*             of the dynamic-wave equation (for computing
*             1-D unsteady flow in open channels).

*   Program notes:
*     Variables are integrated over time using an average weighted
*     by Theta.  Equal weight would be given to new and old time steps
*     when Theta = 0.5.  A Newton-Raphson technique is used in the
*     computation of equation coefficients with the exception of the
*     momentum coefficient Beta which is estimated by successive
*     substitution.  Integration in space is accomplished by numerical
*     quadrature.  Maximum number of quadrature points is currently
*     set by the parameter MaxQuadPts.

*     This routine allows solution with constant coefficient matrix
*     when Forward is .FALSE..

*   Arguments:
      INTEGER Up, Down

*   Argument definitions:
*     Up - upstream location number.
*     Down - downstream location number.

*   Module data:
      Include 'network.inc'
      Include 'solver.inc'

*   Local Variables:

      INTEGER J, J2, K
      INTEGER Nodes, Iteration,QuadPts
      PARAMETER (Nodes = 2)
      real*8 G
      real*8 N(Nodes), DNDX(Nodes)
      real*8 QuadWt, QuadPt
      real*8 X1, X2, Q1, Q2, Z1, Z2, H1, H2
      real*8 Width1, Width2, Area1, Area2
      real*8 Velocity1, Velocity2
      real*8 BetaVelocity1, BetaVelocity2
      REAL*8 Coef(8)
      real*8 DX, DT, Theta, OneMinusTheta
      real*8 CoefMassKnown, CoefMassAdjust
      real*8 CoefDynmKnown, CoefDynmAdjust
      real*8 Xdist, Q, H, Z, DZDX
      real*8 Width, Area, Conv, ConvSq
      real*8 DXxDT, AbsQxAreaByConvSq, AbsQxQbyConvSq, DConvDZ
      LOGICAL OK, Forward

*   Routines by module:

***** Channel flow status:
      real*8     StreamFlow, StreamDepth
      EXTERNAL StreamFlow, StreamDepth

      real*8     StreamSurfaceElevation
      EXTERNAL StreamSurfaceElevation

***** Channel properties:
      real*8     ChannelWidth, CxArea, Beta, Conveyance, dConveyance
      EXTERNAL ChannelWidth, CxArea, Beta, Conveyance, dConveyance

***** Channel schematic:
      real*8     StreamDistance
      EXTERNAL StreamDistance

***** Network control:
      INTEGER  NetworkIteration
      INTEGER  NetworkQuadPts
      real*8     AccelerationGravity
      EXTERNAL NetworkQuadPts, NetworkQuadPtWt
      EXTERNAL NetworkIteration, AccelerationGravity

      INTEGER  NetworkTimeIncrement
      real*8     NetworkTheta
      EXTERNAL NetworkTheta, NetworkTimeIncrement

***** Solver:
      LOGICAL  StoreMassEq, StoreMassKnown, StoreMassAdjust
      EXTERNAL StoreMassEq, StoreMassKnown, StoreMassAdjust

      LOGICAL  StoreDynamicEq, StoreDynmKnown, StoreDynmAdjust
      EXTERNAL StoreDynamicEq, StoreDynmKnown, StoreDynmAdjust

      LOGICAL  ForwardElim
      EXTERNAL ForwardElim

*   Intrinsics:
      real*8      ABS
      INTRINSIC ABS

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:   Lew DeLong
*   Last modified: April 1992
*   Modified for Sparse solution by: Eli Ateljevich July98
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      Iteration = NetworkIteration()
      Forward = ForwardElim()

      G = AccelerationGravity()

*-----Current flow variables.

*          upstream face
      Q1 = StreamFlow(Up)
      Z1 = StreamSurfaceElevation(Up)
      H1 = StreamDepth(Up)

*          downstream face
      Q2 = StreamFlow(Down)
      Z2 = StreamSurfaceElevation(Down)
      H2 = StreamDepth(Down)

*-----Current channel properties.

*          upstream face
      X1 = StreamDistance(Up)
      Width1 = ChannelWidth(X1,H1)
      Area1 = CxArea(X1,H1)
      Velocity1 = Q1 / Area1
      BetaVelocity1 = Beta(X1,H1) * Velocity1

*          downstream face
      X2 = StreamDistance(Down)
      Width2 = ChannelWidth(X2,H2)
      Area2 = CxArea(X2,H2)
      Velocity2 = Q2 / Area2
      BetaVelocity2 = Beta(X2,H2) * Velocity2

*-----Time increment and weighting.
      DT = DFLOAT( NetworkTimeIncrement() )
      Theta = NetworkTheta()
      OneMinusTheta = 1.0 - Theta

*-----Spatial increment.
      DX = X2 - X1

*-----Water-surface slope, assumed constant from X1 to X2.
      DZDX = (Z2 - Z1) / DX




      IF( Forward ) THEN

*--------Mass equation.

*  Coef(1)*Q1 + Coef(2)*Z1 + Coef(3)*Q2 + Coef(4)*Z2 = MassConst + MassAdjust

*--------Dynamic equation.

*  Coef(5)*Q1 + Coef(6)*Z1 + Coef(7)*Q2 + Coef(8)*Z2 = DynmConst + DynmAdjust

*--------Contribution from upstream and downstream faces,
*        integrated in time.

*--------mass-equation coefficients for change in Q1, Z1, Q2, & Z2.
         Coef(1) = - Theta * DT
         Coef(2) =   0.0
         Coef(3) =   Theta * DT
         Coef(4) =   0.0

*--------dynamic equation coefficients for change in Q1, Z1, Q2, & Z2.
         Coef(5) = - Theta * 2.0                * BetaVelocity1 * DT
         Coef(6) =   Theta * Velocity1 * Width1 * BetaVelocity1 * DT
         Coef(7) =   Theta * 2.0                * BetaVelocity2 * DT
         Coef(8) = - Theta * Velocity2 * Width2 * BetaVelocity2 * DT

      END IF

      IF( Iteration .GT. 1 ) THEN
      ELSE

         CoefMassKnown = OneMinusTheta * ( Q1 - Q2 ) * DT

         CoefDynmKnown = OneMinusTheta * (
     &        Q1 * BetaVelocity1
     &        - Q2 * BetaVelocity2
     &        ) * DT

      END IF

      CoefMassAdjust = Theta * ( Q1 - Q2 ) * DT

      CoefDynmAdjust = Theta * (
     &     Q1 * BetaVelocity1
     &     - Q2 * BetaVelocity2
     &     ) * DT


*-----Contribution from spatial dimension,
*        integrated in space or time and space.

*-----Derivative of interpolation functions with respect
*            to space, constant across subdomain.
      DNDX(1) = -1.0 / DX
      DNDX(2) = -DNDX(1)

      QuadPts = NetworkQuadPts()

      DO 200 K=1,QuadPts

*--------Estimate quadrature-point values.

         CALL NetworkQuadPtWt( K, QuadPt, QuadWt )

*--------Interpolation functions.
         N(1) = 1.0 - QuadPt
         N(2) = QuadPt

*--------Location of quadrature point.
         Xdist = N(1) * X1 + N(2) * X2

*--------Dependent variables.
         Q = N(1) * Q1 + N(2) * Q2
         H = N(1) * H1 + N(2) * H2
         Z = N(1) * Z1 + N(2) * Z2

*--------Channel variables.
         Width = ChannelWidth( Xdist, H )
         Area = CxArea( Xdist, H )

*--------Friction-slope terms.
         Conv = Conveyance(Xdist,H)
         ConvSq = Conv * Conv
         DConvDZ = dConveyance(Xdist,H)

         AbsQxQbyConvSq =    ABS(Q) / ConvSq
         AbsQxAreaByConvSq = AbsQxQbyConvSq * Area
         AbsQxQbyConvSq =    AbsQxQbyConvSq * Q

*--------Product of space and time increments.
         DXxDT = DX * DT

         IF( Forward ) THEN
            DO 100 J=1,2

               J2 = J*2

*--------------mass equation, coefficients for change in Z,
*              coefficient numbers 2 & 4.

               Coef(J2) = Coef(J2) + QuadWt
     &              * DX * Width * N(J)

*--------------dynamic equation, coefficients for change in Q,
*              coefficient numbers 5 and 7.

               Coef(J2+3) = Coef(J2+3) + QuadWt *    (
     &              DX
     &              + Theta * DXxDT * G * 2.0 * AbsQxAreaByConvSq
     &              ) * N(J)

*--------------dynamic equation, coefficients for change in Z
*              coefficient numbers 6 and 8.

               Coef(J2+4) = Coef(J2+4)
     &              + QuadWt * Theta * G * DXxDT *    (

     &              Width * DZDX * N(J)
     &              + Area   *    DNDX(J)

     &              + (
     &              Q * AbsQxAreaByConvSq * (
     &              Width / Area - 2.0 * DConvDZ / Conv
     &              )
     &              )            * N(J)

     &              )

 100        CONTINUE
         END IF

         IF( Iteration .NE. 1 ) THEN
         ELSE

            CoefMassKnown = CoefMassKnown + QuadWt * Area * DX

            CoefDynmKnown = CoefDynmKnown + QuadWt *    (
     &           Q * DX
     &           - OneMinusTheta * DXxDT * G * Area * (
     &           DZDX + AbsQxQbyConvSq
     &           )
     &           )

         END IF

         CoefMassAdjust = CoefMassAdjust - QuadWt * Area * DX

         CoefDynmAdjust = CoefDynmAdjust + QuadWt *    (
     &        - Q * DX
     &        - Theta * DXxDT * G * Area * (
     &        DZDX + AbsQxQbyConvSq
     &        )
     &        )

 200  CONTINUE

*-----Store computed coefficients.

      IF( Forward ) THEN
         Call sfAdd4Equation( MASSEQ(1,Up + EqPointer(Branch) - 1), Coef(1) )
         Call sfAdd4Equation( DYNMEQ(1,Up + EqPointer(Branch) - 1), Coef(5) )
      END IF

      OK = StoreMassAdjust( Up, CoefMassAdjust )
      OK = StoreDynmAdjust( Up, CoefDynmAdjust )

      IF( Iteration .GT. 1 ) THEN
      ELSE

         OK = StoreMassKnown( Up, CoefMassKnown )
         OK = StoreDynmKnown( Up, CoefDynmKnown )

      END IF

      DynamicWaveEq = .TRUE.

      RETURN
      END

*== Public (DiffusionWaveEq) ===========================================

      LOGICAL FUNCTION DiffusionWaveEq(Up, Down)

      IMPLICIT NONE

*   Purpose:  Compute and store coefficients of an integral form
*             of the diffusion-wave equation (for computing
*             1-D unsteady flow in open channels).

*   Program notes:
*     Variables are integrated over time using an average weighted
*     by Theta.  Equal weight would be given to new and old time steps
*     when Theta = 0.5.  A Newton-Raphson technique is used in the
*     computation of equation coefficients with the exception of the
*     momentum coefficient Beta which is estimated by successive
*     substitution.  Integration in space is accomplished by numerical
*     quadrature.  Maximum number of quadrature points is currently
*     set by the parameter MaxQuadPts.

*     This routine allows solution with constant coefficient matrix
*     when Forward is .FALSE..

*   Arguments:
      INTEGER Up, Down

*   Argument definitions:
*     Up - upstream location number.
*     Down - downstream location number.

*   Module data:
      Include 'network.inc'
      Include 'solver.inc'

*   Local Variables:
      INTEGER J, J2, K
      INTEGER Nodes, Iteration, QuadPts
      PARAMETER (Nodes = 2)
      real*8 G
      real*8 N(Nodes), DNDX(Nodes)
      real*8 QuadWt, QuadPt
      real*8 X1, X2, Q1, Q2, Z1, Z2, H1, H2
      real*8 Coef(8)
      real*8 DX, DT, Theta, OneMinusTheta
      real*8 CoefMassKnown, CoefMassAdjust
      real*8 CoefDynmKnown, CoefDynmAdjust
      real*8 Xdist, Q, H, Z, DZDX
      real*8 Width, Area, Conv, ConvSq
      real*8 DXxDT, AbsQxAreaByConvSq, AbsQxQbyConvSq, DConvDZ
      LOGICAL OK, Forward

*  Routines by module:

***** Channel flow status:
      real*8     StreamFlow, StreamDepth
      EXTERNAL StreamFlow, StreamDepth

      real*8     StreamSurfaceElevation
      EXTERNAL StreamSurfaceElevation

***** Channel properties:
      real*8     ChannelWidth, CxArea, Beta, Conveyance, dConveyance
      EXTERNAL ChannelWidth, CxArea, Beta, Conveyance, dConveyance

***** Channel schematic:
      real*8     StreamDistance
      EXTERNAL StreamDistance

***** Network control:
      INTEGER  NetworkIteration
      real*8     AccelerationGravity
      EXTERNAL NetworkIteration, AccelerationGravity

      INTEGER  NetworkTimeIncrement
      real*8     NetworkTheta
      EXTERNAL NetworkTheta, NetworkTimeIncrement

      INTEGER  NetworkQuadPts
      EXTERNAL NetworkQuadPts, NetworkQuadPtWt

***** Solver:
      LOGICAL  StoreMassEq, StoreMassKnown, StoreMassAdjust
      EXTERNAL StoreMassEq, StoreMassKnown, StoreMassAdjust

      LOGICAL  StoreDynamicEq, StoreDynmKnown, StoreDynmAdjust
      EXTERNAL StoreDynamicEq, StoreDynmKnown, StoreDynmAdjust

      LOGICAL  ForwardElim
      EXTERNAL ForwardElim

*   Intrinsics:
      real*8      ABS
      INTRINSIC ABS

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      Iteration = NetworkIteration()
      Forward = ForwardElim()

      G = AccelerationGravity()

*-----Current channel properties.

*          upstream face
      X1 = StreamDistance(Up)

*          downstream face
      X2 = StreamDistance(Down)

*-----Current flow variables.

*          upstream face
      Q1 = StreamFlow(Up)
      Z1 = StreamSurfaceElevation(Up)
      H1 = StreamDepth(Up)

*          downstream face
      Q2 = StreamFlow(Down)
      Z2 = StreamSurfaceElevation(Down)
      H2 = StreamDepth(Down)

*-----Time increment and weighting.
      DT = DFLOAT( NetworkTimeIncrement() )
      Theta = NetworkTheta()
      OneMinusTheta = 1.0 - Theta

*-----Spatial increment.
      DX = X2 - X1

*-----Water-surface slope, assumed constant from X1 to X2.
      DZDX = (Z2 - Z1) / DX

      IF( Forward ) THEN

*--------Mass equation.

*  Coef(1)*Q1 + Coef(2)*Z1 + Coef(3)*Q2 + Coef(4)*Z2 = MassConst + MassAdjust

*--------Dynamic equation.

*  Coef(5)*Q1 + Coef(6)*Z1 + Coef(7)*Q2 + Coef(8)*Z2 = DynmConst + DynmAdjust

*--------Contribution from upstream and downstream faces,
*        integrated in time.

*--------mass-equation coefficients for change in Q1, Z1, Q2, & Z2.
         Coef(1) = - Theta * DT
         Coef(2) =   0.0
         Coef(3) =   Theta * DT
         Coef(4) =   0.0

*--------dynamic equation coefficients for change in Q1, Z1, Q2, & Z2,
*         flux terms dropped in diffusion-wave equation.
         Coef(5) = 0.0
         Coef(6) = 0.0
         Coef(7) = 0.0
         Coef(8) = 0.0

      END IF

      IF( Iteration .GT. 1 ) THEN
      ELSE

         CoefMassKnown = OneMinusTheta * ( Q1 - Q2 ) * DT

         CoefDynmKnown = 0.0

      END IF

      CoefMassAdjust = Theta * ( Q1 - Q2 ) * DT

      CoefDynmAdjust = 0.0

*-----Contribution from spatial dimension,
*        integrated in space or time and space.

*-----Derivative of interpolation functions with respect
*            to space, constant across subdomain.
      DNDX(1) = -1.0 / DX
      DNDX(2) = -DNDX(1)

      QuadPts = NetworkQuadPts()

      DO 200 K=1,QuadPts

*--------Estimate quadrature-point values.

         CALL NetworkQuadPtWt( K, QuadPt, QuadWt )

*--------Interpolation functions.
         N(1) = 1.0 - QuadPt
         N(2) = QuadPt

*--------Location of quadrature point.
         Xdist = N(1) * X1 + N(2) * X2

*--------Dependent variables.
         Q = N(1) * Q1 + N(2) * Q2
         H = N(1) * H1 + N(2) * H2
         Z = N(1) * Z1 + N(2) * Z2

*--------Channel variables.
         Width = ChannelWidth( Xdist, H )
         Area = CxArea( Xdist, H )

*--------Friction-slope terms.
         Conv = Conveyance(Xdist,H)
         ConvSq = Conv * Conv
         DConvDZ = dConveyance(Xdist,H)

         AbsQxQbyConvSq =    ABS(Q) / ConvSq
         AbsQxAreaByConvSq = AbsQxQbyConvSq * Area
         AbsQxQbyConvSq =    AbsQxQbyConvSq * Q

*--------Product of space and time increments.
         DXxDT = DX * DT

         IF( Forward ) THEN
            DO 100 J=1,2

               J2 = J*2

*--------------mass equation, coefficients for change in Z,
*              coefficient numbers 2 & 4.

               Coef(J2) = Coef(J2) + QuadWt
     &              * DX * Width * N(J)

*--------------dynamic equation, coefficients for change in Q,
*              coefficient numbers 5 and 7.

               Coef(J2+3) = Coef(J2+3) + QuadWt *    (

     &              + Theta * DXxDT * G * 2.0 * AbsQxAreaByConvSq
     &              ) * N(J)

*--------------dynamic equation, coefficients for change in Z
*              coefficient numbers 6 and 8.

               Coef(J2+4) = Coef(J2+4)
     &              + QuadWt * Theta * G * DXxDT *    (

     &              Width * DZDX * N(J)
     &              + Area   *    DNDX(J)

     &              + (
     &              Q * AbsQxAreaByConvSq * (
     &              Width / Area - 2.0 * DConvDZ / Conv
     &              )
     &              )            * N(J)

     &              )

 100        CONTINUE
         END IF

         IF( Iteration .NE. 1 ) THEN
         ELSE

            CoefMassKnown = CoefMassKnown + QuadWt * Area * DX

            CoefDynmKnown = CoefDynmKnown + QuadWt *    (

     &           - OneMinusTheta * DXxDT * G * Area * (
     &           DZDX + AbsQxQbyConvSq
     &           )
     &           )

         END IF

         CoefMassAdjust = CoefMassAdjust - QuadWt * Area * DX

         CoefDynmAdjust = CoefDynmAdjust + QuadWt *    (

     &        - Theta * DXxDT * G * Area * (
     &        DZDX + AbsQxQbyConvSq
     &        )
     &        )

 200  CONTINUE

*-----Store computed coefficients.

      IF( Forward ) THEN
         Call sfAdd4Equation( MASSEQ(1,Up + EqPointer(Branch) - 1), Coef(1) )
         Call sfAdd4Equation( DYNMEQ(1,Up + EqPointer(Branch) - 1), Coef(5) )
      END IF

      OK = StoreMassAdjust( Up, CoefMassAdjust )
      OK = StoreDynmAdjust( Up, CoefDynmAdjust )

      IF( Iteration .GT. 1 ) THEN
      ELSE

         OK = StoreMassKnown( Up, CoefMassKnown )
         OK = StoreDynmKnown( Up, CoefDynmKnown )

      END IF

      DiffusionWaveEq = .TRUE.

      RETURN
      END

*== Public (KinematicWaveEq) ===========================================

      LOGICAL FUNCTION KinematicWaveEq(Up, Down)

      IMPLICIT NONE

*   Purpose:  Compute and store coefficients of an integral form
*             of the kinematic-wave equation (for computing
*             1-D unsteady flow in open channels).

*   Program notes:
*     Variables are integrated over time using an average weighted
*     by Theta.  Equal weight would be given to new and old time steps
*     when Theta = 0.5.  A Newton-Raphson technique is used in the
*     computation of equation coefficients with the exception of the
*     momentum coefficient Beta which is estimated by successive
*     substitution.  Integration in space is accomplished by numerical
*     quadrature.  Maximum number of quadrature points is currently
*     set by the parameter MaxQuadPts.

*   Arguments:
      INTEGER Up, Down

*   Argument definitions:
*     Up - upstream location number.
*     Down - downstream location number.

*   Module data:
      Include 'network.inc'
      Include 'solver.inc'

*   Local Variables:
      INTEGER J, J2, K
      INTEGER Nodes, Iteration, QuadPts
      PARAMETER (Nodes = 2)
      real*8 G
      real*8 N(Nodes), DNDX(Nodes)
      real*8 QuadWt, QuadPt
      real*8 X1, X2, Q1, Q2, Z1, Z2, H1, H2
      REAL*8 Coef(8)
      real*8 DX, DT, Theta, OneMinusTheta
      real*8 CoefMassKnown, CoefMassAdjust
      real*8 CoefDynmKnown, CoefDynmAdjust
      real*8 Xdist, Q, H, Z, DZDX
      real*8 Width, Area, Conv, ConvSq
      real*8 DXxDT, AbsQxAreaByConvSq, AbsQxQbyConvSq, DConvDZ
      LOGICAL OK

*   Routines by module:

***** Channel flow status:
      real*8     StreamFlow, StreamDepth
      EXTERNAL StreamFlow, StreamDepth

      EXTERNAL StreamSurfaceElevation
      real*8     StreamSurfaceElevation

***** Channel properties:
      real*8     BtmSlope
      EXTERNAL BtmSlope

      real*8     ChannelWidth, CxArea, Beta, Conveyance, dConveyance
      EXTERNAL ChannelWidth, CxArea, Beta, Conveyance, dConveyance

***** Channel schematic:
      real*8     StreamDistance
      EXTERNAL StreamDistance

***** Network control:
      INTEGER  NetworkIteration
      real*8     AccelerationGravity
      EXTERNAL NetworkIteration, AccelerationGravity

      INTEGER  NetworkTimeIncrement
      real*8     NetworkTheta
      EXTERNAL NetworkTheta, NetworkTimeIncrement

      INTEGER  NetworkQuadPts
      EXTERNAL NetworkQuadPts, NetworkQuadPtWt

***** Solver:
      LOGICAL  StoreMassEq, StoreMassKnown, StoreMassAdjust
      EXTERNAL StoreMassEq, StoreMassKnown, StoreMassAdjust

      LOGICAL  StoreDynamicEq, StoreDynmKnown, StoreDynmAdjust
      EXTERNAL StoreDynamicEq, StoreDynmKnown, StoreDynmAdjust

*   Intrinsics:
      real*8      ABS
      INTRINSIC ABS

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      Iteration = NetworkIteration()

      G = AccelerationGravity()

*-----Current channel properties.

*          upstream face
      X1 = StreamDistance(Up)

*          downstream face
      X2 = StreamDistance(Down)

*-----Current flow variables.

*          upstream face
      Q1 = StreamFlow(Up)
      Z1 = StreamSurfaceElevation(Up)
      H1 = StreamDepth(Up)

*          downstream face
      Q2 = StreamFlow(Down)
      Z2 = StreamSurfaceElevation(Down)
      H2 = StreamDepth(Down)

*-----Time increment and weighting.
      DT = DFLOAT( NetworkTimeIncrement() )
      Theta = NetworkTheta()
      OneMinusTheta = 1.0 - Theta

*-----Spatial increment.
      DX = X2 - X1

*-----Mass equation.

*  Coef(1)*Q1 + Coef(2)*Z1 + Coef(3)*Q2 + Coef(4)*Z2 = MassConst + MassAdjust

*-----Dynamic equation.

*  Coef(5)*Q1 + Coef(6)*Z1 + Coef(7)*Q2 + Coef(8)*Z2 = DynmConst + DynmAdjust

*-----Contribution from upstream and downstream faces,
*        integrated in time.

*-----mass-equation coefficients for change in Q1, Z1, Q2, & Z2.
      Coef(1) = - Theta * DT
      Coef(2) =   0.0
      Coef(3) =   Theta * DT
      Coef(4) =   0.0

*-----dynamic equation coefficients for change in Q1, Z1, Q2, & Z2,
*         flux terms dropped in kinematic-wave equation.
      Coef(5) = 0.0
      Coef(6) = 0.0
      Coef(7) = 0.0
      Coef(8) = 0.0

      IF( Iteration .GT. 1 ) THEN
      ELSE

         CoefMassKnown = OneMinusTheta * ( Q1 - Q2 ) * DT

         CoefDynmKnown = 0.0

      END IF

      CoefMassAdjust = Theta * ( Q1 - Q2 ) * DT

      CoefDynmAdjust = 0.0

*-----Contribution from spatial dimension,
*        integrated in space or time and space.

*-----Derivative of interpolation functions with respect
*            to space, constant across subdomain.
      DNDX(1) = -1.0 / DX
      DNDX(2) = -DNDX(1)

      QuadPts = NetworkQuadPts()

      DO 200 K=1,QuadPts

*--------Estimate quadrature-point values.

         CALL NetworkQuadPtWt( K, QuadPt, QuadWt )

*--------Interpolation functions.
         N(1) = 1.0 - QuadPt
         N(2) = QuadPt

*--------Location of quadrature point.
         Xdist = N(1) * X1 + N(2) * X2

*--------Dependent variables.
         Q = N(1) * Q1 + N(2) * Q2
         H = N(1) * H1 + N(2) * H2
         Z = N(1) * Z1 + N(2) * Z2

*--------Channel variables.
         Width = ChannelWidth( Xdist, H )
         Area = CxArea( Xdist, H )

*--------Kinematic approximation, assume water-surface slope
*             is same as bottom slope.
         DZDX = BtmSlope(Xdist)

*--------Friction-slope terms.
         Conv = Conveyance(Xdist,H)
         ConvSq = Conv * Conv
         DConvDZ = dConveyance(Xdist,H)

         AbsQxQbyConvSq =    ABS(Q) / ConvSq
         AbsQxAreaByConvSq = AbsQxQbyConvSq * Area
         AbsQxQbyConvSq =    AbsQxQbyConvSq * Q

*--------Product of space and time increments.
         DXxDT = DX * DT

         DO 100 J=1,2

            J2 = J*2

*-----------mass equation, coefficients for change in Z,
*              coefficient numbers 2 & 4.

            Coef(J2) = Coef(J2) + QuadWt
     &           * DX * Width * N(J)

*-----------dynamic equation, coefficients for change in Q,
*              coefficient numbers 5 and 7.

            Coef(J2+3) = Coef(J2+3) + QuadWt *    (

     &           + Theta * DXxDT * G * 2.0 * AbsQxAreaByConvSq
     &           ) * N(J)

*-----------dynamic equation, coefficients for change in Z
*              coefficient numbers 6 and 8.

            Coef(J2+4) = Coef(J2+4)
     &           + QuadWt * Theta * G * DXxDT *    (

     &           Width * DZDX * N(J)
     &           + Area   *    DNDX(J)

     &           + (
     &           Q * AbsQxAreaByConvSq * (
     &           Width / Area - 2.0 * DConvDZ / Conv
     &           )
     &           )            * N(J)

     &           )

 100     CONTINUE

         IF( Iteration .NE. 1 ) THEN
         ELSE

            CoefMassKnown = CoefMassKnown + QuadWt * Area * DX

            CoefDynmKnown = CoefDynmKnown + QuadWt *    (

     &           - OneMinusTheta * DXxDT * G * Area * (
     &           DZDX + AbsQxQbyConvSq
     &           )
     &           )

         END IF

         CoefMassAdjust = CoefMassAdjust - QuadWt * Area * DX

         CoefDynmAdjust = CoefDynmAdjust + QuadWt *    (

     &        - Theta * DXxDT * G * Area * (
     &        DZDX + AbsQxQbyConvSq
     &        )
     &        )

 200  CONTINUE

*-----Store computed coefficients.

      Call sfAdd4Equation( MASSEQ(1,Up + EqPointer(Branch) - 1), Coef(1) )
      Call sfAdd4Equation( DYNMEQ(1,Up + EqPointer(Branch) - 1), Coef(5) )

      OK = StoreMassAdjust( Up, CoefMassAdjust )
      OK = StoreDynmAdjust( Up, CoefDynmAdjust )

      IF( Iteration .GT. 1 ) THEN
      ELSE

         OK = StoreMassKnown( Up, CoefMassKnown )
         OK = StoreDynmKnown( Up, CoefDynmKnown )

      END IF

      KinematicWaveEq = .TRUE.

      RETURN
      END

*== Public (DynamicWaveEqDS) =============================================

      LOGICAL FUNCTION DynamicWaveEqDS(Up, Down)

      IMPLICIT NONE

*   Purpose:  Compute and store coefficients of an integral form
*             of the dynamic-wave equation (for computing
*             1-D unsteady flow in open channels).

*   Program notes:
*     Variables are integrated over time using an average weighted
*     by Theta.  Equal weight would be given to new and old time steps
*     when Theta = 0.5.  A Newton-Raphson technique is used in the
*     computation of equation coefficients with the exception of the
*     momentum coefficient Beta which is estimated by successive
*     substitution.  Integration in space is accomplished by numerical
*     quadrature.  Maximum number of quadrature points is currently
*     set by the parameter MaxQuadPts.  Water density is allowed to vary
*     with time and distance.  Channel sinuosity is allowed to vary
*     with depth of flow and distance.

*     This routine allows solution with constant coefficient matrix
*     when Forward is .FALSE..

*   Arguments:
      INTEGER Up, Down

*   Argument definitions:
*     Up - upstream location number.
*     Down - downstream location number.

*   Module data:
      Include 'network.inc'
      Include 'solver.inc'

*   Local Variables:

      INTEGER J, J2, K
      INTEGER Nodes, Iteration, QuadPts
      PARAMETER (Nodes = 2)
      real*8 G
      real*8 N(Nodes), DNDX(Nodes)
      real*8 QuadWt, QuadPt
      real*8 X1, X2, Q1, Q2, Z1, Z2, H1, H2
      real*8 Width1, Width2, Area1, Area2
      real*8 Velocity1, Velocity2
      real*8 BetaVelocity1, BetaVelocity2
      real*8 OldRho1, NewRho1, OldRho2, NewRho2, OldRho, NewRho
      real*8 OlddRhodX, NewdRhodX
      real*8 ZBar, dZBardX
      REAL*8 Coef(8)
      real*8 DX, DT, Theta, OneMinusTheta
      real*8 CoefMassKnown, CoefMassAdjust
      real*8 CoefDynmKnown, CoefDynmAdjust
      real*8 Xdist, Q, H, Z, DZDX
      real*8 Ma, dMadH, Mq, dMqdH
      real*8 Width, Area, Conv, ConvSq
      real*8 DXxDT, AbsQxAreaByConvSq, AbsQxQbyConvSq, DConvDZ
      LOGICAL OK, Forward

*   Routines by module:

***** Channel flow status:
      real*8     StreamFlow, StreamDepth
      real*8     OldStreamDensity, NewStreamDensity
      EXTERNAL StreamFlow, StreamDepth
      EXTERNAL OldStreamDensity, NewStreamDensity

      real*8     StreamSurfaceElevation
      EXTERNAL StreamSurfaceElevation

***** Channel properties:
      real*8     ChannelWidth, CxArea, Beta, Conveyance, dConveyance
      real*8     AreaWtSinuosity,  QWtSinuosity
      real*8     dAreaWtSinuosity, dQWtSinuosity
      real*8     CxCentroid, dCxCentroid
      EXTERNAL CxCentroid, dCxCentroid
      EXTERNAL ChannelWidth, CxArea, Beta, Conveyance, dConveyance
      EXTERNAL AreaWtSinuosity,  QWtSinuosity
      EXTERNAL dAreaWtSinuosity, dQWtSinuosity

***** Channel schematic:
      real*8     StreamDistance
      EXTERNAL StreamDistance

***** Network control:
      INTEGER  NetworkIteration
      real*8     AccelerationGravity
      EXTERNAL NetworkIteration, AccelerationGravity

      INTEGER  NetworkTimeIncrement
      real*8     NetworkTheta
      EXTERNAL NetworkTheta, NetworkTimeIncrement

      INTEGER  NetworkQuadPts
      EXTERNAL NetworkQuadPts, NetworkQuadPtWt

***** Solver:
      LOGICAL  StoreMassEq, StoreMassKnown, StoreMassAdjust
      EXTERNAL StoreMassEq, StoreMassKnown, StoreMassAdjust

      LOGICAL  StoreDynamicEq, StoreDynmKnown, StoreDynmAdjust
      EXTERNAL StoreDynamicEq, StoreDynmKnown, StoreDynmAdjust

      LOGICAL  ForwardElim
      EXTERNAL ForwardElim

*   Intrinsics:
      real*8      ABS
      INTRINSIC ABS

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      Iteration = NetworkIteration()

      G = AccelerationGravity()

      Forward = ForwardElim()

*-----Current flow variables.

*          upstream face
      Q1 = StreamFlow(Up)
      Z1 = StreamSurfaceElevation(Up)
      H1 = StreamDepth(Up)
      NewRho1 = NewStreamDensity(Up)

*          downstream face
      Q2 = StreamFlow(Down)
      Z2 = StreamSurfaceElevation(Down)
      H2 = StreamDepth(Down)
      NewRho2 = NewStreamDensity(Down)

*-----Current channel properties.

*          upstream face
      X1 = StreamDistance(Up)
      Width1 = ChannelWidth(X1,H1)
      Area1 = CxArea(X1,H1)
      Velocity1 = Q1 / Area1
      BetaVelocity1 = Beta(X1,H1) * Velocity1

*          downstream face
      X2 = StreamDistance(Down)
      Width2 = ChannelWidth(X2,H2)
      Area2 = CxArea(X2,H2)
      Velocity2 = Q2 / Area2
      BetaVelocity2 = Beta(X2,H2) * Velocity2

*-----Time increment and weighting.
      DT = DFLOAT( NetworkTimeIncrement() )
      Theta = NetworkTheta()
      OneMinusTheta = 1.0 - Theta

*-----Spatial increment.
      DX = X2 - X1

*-----Water-surface slope, assumed constant from X1 to X2.
      DZDX = (Z2 - Z1) / DX

      IF( Forward ) THEN

*--------Mass equation.

*  Coef(1)*Q1 + Coef(2)*Z1 + Coef(3)*Q2 + Coef(4)*Z2 = MassConst + MassAdjust

*--------Dynamic equation.

*  Coef(5)*Q1 + Coef(6)*Z1 + Coef(7)*Q2 + Coef(8)*Z2 = DynmConst + DynmAdjust

*--------Contribution from upstream and downstream faces,
*        integrated in time.

*--------mass-equation coefficients for change in Q1, Z1, Q2, & Z2.
         Coef(1) = - Theta * NewRho1 * DT
         Coef(2) =   0.0
         Coef(3) =   Theta * NewRho2 * DT
         Coef(4) =   0.0

*--------dynamic equation coefficients for change in Q1, Z1, Q2, & Z2.
         Coef(5) =-Theta * 2.0 *     NewRho1       * BetaVelocity1 * DT
         Coef(6) = Theta *Velocity1* NewRho1* Width1*BetaVelocity1 * DT
         Coef(7) = Theta * 2.0 *     NewRho2       * BetaVelocity2 * DT
         Coef(8) =-Theta *Velocity2* NewRho2* Width2*BetaVelocity2 * DT

      END IF

      IF( Iteration .GT. 1 ) THEN
      ELSE

         OldRho1 = OldStreamDensity(Up)
         OldRho2 = OldStreamDensity(Down)

         CoefMassKnown = OneMinusTheta* (OldRho1*Q1 - OldRho2*Q2) * DT

         CoefDynmKnown = OneMinusTheta * (
     &        OldRho1 * Q1 * BetaVelocity1
     &        - OldRho2 * Q2 * BetaVelocity2
     &        ) * DT

      END IF

      CoefMassAdjust = Theta * (NewRho1*Q1 - NewRho2*Q2 ) * DT

      CoefDynmAdjust = Theta * (
     &     NewRho1 * Q1 * BetaVelocity1
     &     - NewRho2 * Q2 * BetaVelocity2
     &     ) * DT

*-----Contribution from spatial dimension,
*        integrated in space or time and space.

*-----Derivative of interpolation functions with respect
*            to space, constant across subdomain.
      DNDX(1) = -1.0 / DX
      DNDX(2) = -DNDX(1)

      QuadPts = NetworkQuadPts()

      DO 200 K=1,QuadPts

*--------Estimate quadrature-point values.

         CALL NetworkQuadPtWt( K, QuadPt, QuadWt )

*--------Interpolation functions.
         N(1) = 1.0 - QuadPt
         N(2) = QuadPt

*--------Location of quadrature point.
         Xdist = N(1) * X1 + N(2) * X2

*--------Dependent variables.
         Q = N(1) * Q1 + N(2) * Q2
         H = N(1) * H1 + N(2) * H2
         Z = N(1) * Z1 + N(2) * Z2

*--------Channel geometry variables.
         Width = ChannelWidth( Xdist, H )
         Area = CxArea( Xdist, H )

*--------Density and density gradient.
         NewRho = N(1) * NewRho1 + N(2) * NewRho2
         NewdRhodX = DNDX(1) * NewRho1 + DNDX(2) * NewRho2

*--------Distance from centroid to water surface.
         ZBar = CxCentroid(Xdist,H)
         dZBardX = dCxCentroid(Xdist,H)

*--------Friction-slope terms.
         Conv = Conveyance(Xdist,H)
         ConvSq = Conv * Conv
         DConvDZ = dConveyance(Xdist,H)

         AbsQxQbyConvSq =    ABS(Q) / ConvSq
         AbsQxAreaByConvSq = AbsQxQbyConvSq * Area
         AbsQxQbyConvSq =    AbsQxQbyConvSq * Q

*--------Sinuosity.
         Ma = AreaWtSinuosity(Xdist,H)
         dMadH = dAreaWtSinuosity(Xdist,H)
         Mq = QWtSinuosity(Xdist,H)
         dMqdH = dQWtSinuosity(Xdist,H)

*--------Product of space and time increments.
         DXxDT = DX * DT

         IF( Forward ) THEN
            DO 100 J=1,2

               J2 = J*2

*--------------mass equation, coefficients for change in Z,
*              coefficient numbers 2 & 4.

               Coef(J2) = Coef(J2) + QuadWt *    (
     &              Ma  * Width + Area * dMadH
     &              ) * NewRho * DX * N(J)

*--------------dynamic equation, coefficients for change in Q,
*              coefficient numbers 5 and 7.

               Coef(J2+3) = Coef(J2+3) + QuadWt *    (
     &              NewRho * Mq * DX
     &              + Theta * NewRho * DXxDT * G * 2.0 * AbsQxAreaByConvSq
     &              ) * N(J)

*--------------dynamic equation, coefficients for change in Z
*              coefficient numbers 6 and 8.

               Coef(J2+4) = Coef(J2+4) + QuadWt *            (
     &              NewRho * Q * dMqdH
     &              +    Theta * G * DXxDT * (

     &              NewRho * (
     &              Width * DZDX * N(J)
     &              + Area   *    DNDX(J)
     &              )

     &              + (
     &              NewdRhodX * (
     &              ZBar * Width + Area * dZBardX
     &              )

     &              + (
     &              NewRho * Q * AbsQxAreaByConvSq * (
     &              Width / Area - 2.0 * DConvDZ / Conv
     &              )
     &              )
     &              )  * N(J)

     &              )
     &              )

 100        CONTINUE
         END IF

         IF( Iteration .NE. 1 ) THEN
         ELSE

            OldRho = N(1) * OldRho1 + N(2) * OldRho2
            OlddRhodX = DNDX(1) * OldRho1 + DNDX(2) * OldRho2

            CoefMassKnown = CoefMassKnown + QuadWt *
     &           OldRho * Area * Ma * DX

            CoefDynmKnown = CoefDynmKnown + QuadWt *    (
     &           OldRho * Q * Mq * DX
     &           - OneMinusTheta * DXxDT * G * Area * (
     &           OldRho * (DZDX + AbsQxQbyConvSq)
     &           + ZBar * OlddRhodX
     &           )

     &
     &           )

         END IF

         CoefMassAdjust = CoefMassAdjust - QuadWt *
     &        NewRho * Area * Ma * DX

         CoefDynmAdjust = CoefDynmAdjust + QuadWt *    (
     &        - NewRho * Q * Mq * DX
     &        - Theta * DXxDT * G * Area * (
     &        NewRho * (DZDX + AbsQxQbyConvSq)
     &        + ZBar * NewdRhodX
     &
     &        )
     &        )

 200  CONTINUE

*-----Store computed coefficients.

      IF( Forward ) THEN
         Call sfAdd4Equation( MASSEQ(1,Up + EqPointer(Branch) - 1), Coef(1) )
         Call sfAdd4Equation( DYNMEQ(1,Up + EqPointer(Branch) - 1), Coef(5) )
      END IF

      OK = StoreMassAdjust( Up, CoefMassAdjust )
      OK = StoreDynmAdjust( Up, CoefDynmAdjust )

      IF( Iteration .GT. 1 ) THEN
      ELSE

         OK = StoreMassKnown( Up, CoefMassKnown )
         OK = StoreDynmKnown( Up, CoefDynmKnown )

      END IF

      DynamicWaveEqDS = .TRUE.

      RETURN
      END

*== Public (DiffusionWaveEqS) ===========================================

      LOGICAL FUNCTION DiffusionWaveEqS(Up, Down)

      IMPLICIT NONE

*   Purpose:  Compute and store coefficients of an integral form
*             of the diffusion-wave equation (for computing
*             1-D unsteady flow in open channels).  Sinuosity is
*             allowed to vary with flow depth.

*   Program notes:
*     Variables are integrated over time using an average weighted
*     by Theta.  Equal weight would be given to new and old time steps
*     when Theta = 0.5.  A Newton-Raphson technique is used in the
*     computation of equation coefficients with the exception of the
*     momentum coefficient Beta which is estimated by successive
*     substitution.  Integration in space is accomplished by numerical
*     quadrature.  Maximum number of quadrature points is currently
*     set by the parameter MaxQuadPts.

*     This routine allows solution with constant coefficient matrix
*     when Forward is .FALSE..

*   Arguments:
      INTEGER Up, Down

*   Argument definitions:
*     Up - upstream location number.
*     Down - downstream location number.

*   Module data:
      Include 'network.inc'
      Include 'solver.inc'

*   Local Variables:
      INTEGER J, J2, K
      INTEGER Nodes, Iteration, QuadPts
      PARAMETER (Nodes = 2)
      real*8 G
      real*8 N(Nodes), DNDX(Nodes)
      real*8 QuadWt, QuadPt
      real*8 X1, X2, Q1, Q2, Z1, Z2, H1, H2
      REAL*8 Coef(8)
      real*8 DX, DT, Theta, OneMinusTheta
      real*8 CoefMassKnown, CoefMassAdjust
      real*8 CoefDynmKnown, CoefDynmAdjust
      real*8 Xdist, Q, H, Z, DZDX
      real*8 Width, Area, Conv, ConvSq
      real*8 Ma, dMadH
      real*8 DXxDT, AbsQxAreaByConvSq, AbsQxQbyConvSq, DConvDZ
      LOGICAL OK, Forward

*  Routines by module:

***** Channel flow status:
      real*8     StreamFlow, StreamDepth
      EXTERNAL StreamFlow, StreamDepth

      real*8     StreamSurfaceElevation
      EXTERNAL StreamSurfaceElevation

***** Channel properties:
      real*8     ChannelWidth, CxArea, Beta, Conveyance, dConveyance
      real*8     AreaWtSinuosity, dAreaWtSinuosity
      EXTERNAL ChannelWidth, CxArea, Beta, Conveyance, dConveyance
      EXTERNAL AreaWtSinuosity, dAreaWtSinuosity

***** Channel schematic:
      real*8     StreamDistance
      EXTERNAL StreamDistance

***** Network control:
      INTEGER  NetworkIteration
      real*8     AccelerationGravity
      EXTERNAL NetworkIteration, AccelerationGravity

      INTEGER  NetworkTimeIncrement
      real*8     NetworkTheta
      EXTERNAL NetworkTheta, NetworkTimeIncrement

      INTEGER  NetworkQuadPts
      EXTERNAL NetworkQuadPts, NetworkQuadPtWt

***** Solver:
      LOGICAL  StoreMassEq, StoreMassKnown, StoreMassAdjust
      EXTERNAL StoreMassEq, StoreMassKnown, StoreMassAdjust

      LOGICAL  StoreDynamicEq, StoreDynmKnown, StoreDynmAdjust
      EXTERNAL StoreDynamicEq, StoreDynmKnown, StoreDynmAdjust

      LOGICAL  ForwardElim
      EXTERNAL ForwardElim

*   Intrinsics:
      real*8      ABS
      INTRINSIC ABS

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      Iteration = NetworkIteration()
      Forward = ForwardElim()

      G = AccelerationGravity()

*-----Current channel properties.

*          upstream face
      X1 = StreamDistance(Up)

*          downstream face
      X2 = StreamDistance(Down)

*-----Current flow variables.

*          upstream face
      Q1 = StreamFlow(Up)
      Z1 = StreamSurfaceElevation(Up)
      H1 = StreamDepth(Up)

*          downstream face
      Q2 = StreamFlow(Down)
      Z2 = StreamSurfaceElevation(Down)
      H2 = StreamDepth(Down)

*-----Time increment and weighting.
      DT = DFLOAT( NetworkTimeIncrement() )
      Theta = NetworkTheta()
      OneMinusTheta = 1.0 - Theta

*-----Spatial increment.
      DX = X2 - X1

*-----Water-surface slope, assumed constant from X1 to X2.
      DZDX = (Z2 - Z1) / DX

      IF( Forward ) THEN

*--------Mass equation.

*  Coef(1)*Q1 + Coef(2)*Z1 + Coef(3)*Q2 + Coef(4)*Z2 = MassConst + MassAdjust

*--------Dynamic equation.

*  Coef(5)*Q1 + Coef(6)*Z1 + Coef(7)*Q2 + Coef(8)*Z2 = DynmConst + DynmAdjust

*--------Contribution from upstream and downstream faces,
*        integrated in time.

*--------mass-equation coefficients for change in Q1, Z1, Q2, & Z2.
         Coef(1) = - Theta * DT
         Coef(2) =   0.0
         Coef(3) =   Theta * DT
         Coef(4) =   0.0

*--------dynamic equation coefficients for change in Q1, Z1, Q2, & Z2,
*         flux terms dropped in diffusion-wave equation.
         Coef(5) = 0.0
         Coef(6) = 0.0
         Coef(7) = 0.0
         Coef(8) = 0.0

      END IF

      IF( Iteration .GT. 1 ) THEN
      ELSE

         CoefMassKnown = OneMinusTheta * ( Q1 - Q2 ) * DT

         CoefDynmKnown = 0.0

      END IF

      CoefMassAdjust = Theta * ( Q1 - Q2 ) * DT

      CoefDynmAdjust = 0.0

*-----Contribution from spatial dimension,
*        integrated in space or time and space.

*-----Derivative of interpolation functions with respect
*            to space, constant across subdomain.
      DNDX(1) = -1.0 / DX
      DNDX(2) = -DNDX(1)

      QuadPts = NetworkQuadPts()

      DO 200 K=1,QuadPts

*--------Estimate quadrature-point values.

         CALL NetworkQuadPtWt( K, QuadPt, QuadWt )

*--------Interpolation functions.
         N(1) = 1.0 - QuadPt
         N(2) = QuadPt

*--------Location of quadrature point.
         Xdist = N(1) * X1 + N(2) * X2

*--------Dependent variables.
         Q = N(1) * Q1 + N(2) * Q2
         H = N(1) * H1 + N(2) * H2
         Z = N(1) * Z1 + N(2) * Z2

*--------Channel variables.
         Width = ChannelWidth( Xdist, H )
         Area = CxArea( Xdist, H )

*--------Friction-slope terms.
         Conv = Conveyance(Xdist,H)
         ConvSq = Conv * Conv
         DConvDZ = dConveyance(Xdist,H)

         AbsQxQbyConvSq =    ABS(Q) / ConvSq
         AbsQxAreaByConvSq = AbsQxQbyConvSq * Area
         AbsQxQbyConvSq =    AbsQxQbyConvSq * Q

*--------Sinuosity.
         Ma = AreaWtSinuosity(Xdist,H)
         dMadH = dAreaWtSinuosity(Xdist,H)

*--------Product of space and time increments.
         DXxDT = DX * DT

         IF( Forward ) THEN
            DO 100 J=1,2

               J2 = J*2

*--------------mass equation, coefficients for change in Z,
*              coefficient numbers 2 & 4.

               Coef(J2) = Coef(J2) + QuadWt *    (
     &              Ma  * Width + Area * dMadH
     &              ) * DX * N(J)

*--------------dynamic equation, coefficients for change in Q,
*              coefficient numbers 5 and 7.

               Coef(J2+3) = Coef(J2+3) + QuadWt *    (

     &              + Theta * DXxDT * G * 2.0 * AbsQxAreaByConvSq
     &              ) * N(J)

*--------------dynamic equation, coefficients for change in Z
*              coefficient numbers 6 and 8.

               Coef(J2+4) = Coef(J2+4)
     &              + QuadWt * Theta * G * DXxDT *    (

     &              Width * DZDX * N(J)
     &              + Area   *    DNDX(J)

     &              + (
     &              Q * AbsQxAreaByConvSq * (
     &              Width / Area - 2.0 * DConvDZ / Conv
     &              )
     &              )            * N(J)

     &              )

 100        CONTINUE
         END IF

         IF( Iteration .NE. 1 ) THEN
         ELSE

            CoefMassKnown = CoefMassKnown + QuadWt * Area * Ma * DX

            CoefDynmKnown = CoefDynmKnown + QuadWt *    (

     &           - OneMinusTheta * DXxDT * G * Area * (
     &           DZDX + AbsQxQbyConvSq
     &           )
     &           )

         END IF

         CoefMassAdjust = CoefMassAdjust - QuadWt * Area * DX

         CoefDynmAdjust = CoefDynmAdjust + QuadWt *    (

     &        - Theta * DXxDT * G * Area * (
     &        DZDX + AbsQxQbyConvSq
     &        )
     &        )

 200  CONTINUE

*-----Store computed coefficients.

      IF( Forward ) THEN
         Call sfAdd4Equation( MASSEQ(1,Up + EqPointer(Branch) - 1), Coef(1) )
         Call sfAdd4Equation( DYNMEQ(1,Up + EqPointer(Branch) - 1), Coef(5) )
      END IF

      OK = StoreMassAdjust( Up, CoefMassAdjust )
      OK = StoreDynmAdjust( Up, CoefDynmAdjust )

      IF( Iteration .GT. 1 ) THEN
      ELSE

         OK = StoreMassKnown( Up, CoefMassKnown )
         OK = StoreDynmKnown( Up, CoefDynmKnown )

      END IF

      DiffusionWaveEqS = .TRUE.

      RETURN
      END

*==== EOF floweq1d =====================================================
