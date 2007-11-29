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

C!
C!    For information about the solver routines, contact:
C!    Eli Ateljevich
C!    (510) 843-1569
C!
*****-SPARSE COPYRIGHT *************
*  Revision and copyright information.
*
*  Copyright (c) 1985,86,87,88
*  by Kenneth S. Kundert and the University of California.
*
*  Permission to use, copy, modify, and distribute this software and
*  its documentation for any purpose and without fee is hereby granted,
*  provided that the copyright notices appear in all copies and
*  supporting documentation and that the authors and the University of
*  California are properly credited.  The authors and the University of
*  California make no representations as to the suitability of this
*  software for any purpose.  It is provided `as is', without express
*  or implied warranty.
*
************************************************************************

*== Public (InitializeSolver) ==========================================

      LOGICAL FUNCTION InitializeSolver()

      IMPLICIT NONE

*   Purpose: Initialize the matrix-solution module skymod.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'solver.inc'
      INCLUDE 'solver2.inc'
      include '../input/fixed/misc.f'
      include 'chconnec.inc'

*   Local Variables:
      INTEGER Error,Zero
      Parameter (zero = 0)
      INTEGER I

*   Routines by module:

*   Solver

***** Channel schematic:
      INTEGER  TotalStreamLocations
      EXTERNAL TotalStreamLocations

      INTEGER sfcreate
      EXTERNAL sfcreate

***** Local:
      LOGICAL  ReserveMatrix
      EXTERNAL ReserveMatrix

*   Programmed by:   Lew DeLong
*   Date:            July 1990
*   Modified by:     Eli Ateljevich
*   Date:			   July 1998
*

*-----Implementation -----------------------------------------------------

      InitializeSolver = .FALSE.

      TotalNonResRows = 2*TotalStreamLocations()
      Equations = TotalNonResRows + Nres

      Matrix = sfCreate(Equations,Zero,Error)

*-----Initialize Matrix Scale Factors
      Do i=1,Equations
         If (Mod(i,2) .eq. 0) Then
            ColumnScale(i)=ZscaleFactor
         Else
            ColumnScale(i)=1.0
         End If

         If (i.gt.TotalNonResRows) Then
            RowScale(i)=ResScaleFactor
            ColumnScale(i)=ZscaleFactor
         Else
            RowScale(i)=1.
         End If

      End Do

*-----Set up matrix-shape indicies.
      IF( ReserveMatrix() ) THEN
      ELSE
         WRITE(UNIT_ERROR,*) ' ReserveMatrix failed...'
         RETURN
      END IF

*-----Initialize 'first iteration' index.
      FirstTime=.True.
      InitializeSolver = .TRUE.

      RETURN
      END

*== Public (CloseSolver) ==========================================

      LOGICAL FUNCTION CloseSolver()

      IMPLICIT NONE

*   Purpose: Close and deallocate
*   the matrix solver in SPARSE.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'solver.inc'
      INCLUDE 'solver2.inc'

*   Programmed by:   Lew DeLong
*   Date:            July 1990
*   Modified by:     Eli Ateljevich
*   Date:			   July 1998
*

*-----Implementation -----------------------------------------------------

      CloseSolver = .FALSE.
      Call sfDestroy(Matrix)
      CloseSolver = .TRUE.

      RETURN
      END

*== Public (InitializeMatrix) ==========================================

      LOGICAL FUNCTION InitializeMatrix()

      IMPLICIT NONE

*   Purpose:  Fourpoint allows for two levels of solution.. In the first,
*-----both the coefficient matrix and the RHS are updated and
*             solved. In the second, only the RHS is updated.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'solver.inc'

*   Local Variables:
c-----INTEGER I

*   Routines by module:

***** Local:
      LOGICAL  ForwardElim
      EXTERNAL ForwardElim

*   Programmed by: Lew DeLong
*   Date:          January 1991
*   Modified by:   Lew DeLong
*   Last modified: April 1992
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      InitializeMatrix = .FALSE.

      IF( ForwardElim() ) THEN
         call sfClear(Matrix)
 100     CONTINUE
      END IF

      InitializeMatrix = .TRUE.

      RETURN
      END

*== Public (StoreAtLocation) ==========================================

      LOGICAL FUNCTION StoreAtLocation(Location, ZZZ)

      IMPLICIT NONE

*   Purpose:  Store a value in the coeficient matrix at a location
*             corresponding to an integer pointer (Location)

*             This Function has no counterpart in SPARSE
*             Functionality is largely redundant with AddAtLocation
*-----It is kept here to allow control using ForwElim()
*             Values are stored only if complete forward elimination
*             is to be performed on the coefficient matrix, indicated
*             by ForwardElim() = .TRUE..

*   Arguments:
      INTEGER Location
      DOUBLE PRECISION Value
      REAL*8 ZZZ
*   Argument definitions:
*     Location  - a pointer to the desired location in the
*-----coef matrix

*     Value  - value to be stored. Incoming number may not be
*     Double precision, but must be stored as double precision
*     for SPARSE solver.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'solver.inc'

*   Local Variables:
c-----INTEGER N

*   Routines by module:

      LOGICAL  ForwardElim
      EXTERNAL ForwardElim

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:   Eli Ateljevich
*   Last modified: July 1998
*   Version 98.01, July, 1998

*-----Implementation -----------------------------------------------------

      Value=0
      IF( ForwardElim() ) THEN
         Value=zzz

         Call sfAdd1Real(Location,Value)
      END IF

      StoreAtLocation = .TRUE.

      RETURN
      END

*== Public (AddAtLocation) ============================================

      LOGICAL FUNCTION AddAtLocation(Location, ZZZ)

      IMPLICIT NONE

*   Purpose:  Add a value to the coeficient matrix at a location
*             corresponding to Row and Offset from the diagonal in
*             the virtual matrix.

*   Arguments:
      INTEGER Location
      REAL*8 ZZZ
      DOUBLE PRECISION VALUE
      LOGICAL ForwardElim
      External ForwardElim

*   Argument definitions:
*     Location  - a pointer to the desired location in the
*-----coef matrix

*     Value  - value to be stored. Incoming number may not be
*     Double precision, but must be stored as double precision
*     for SPARSE solver.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'solver.inc'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:   Eli Ateljevich
*   Last modified: July, 1998
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      If (ForwardElim())Then
         Value = ZZZ

         Call sfAdd1Real(Location,Value)

      End If
      AddAtLocation = .TRUE.

      RETURN
      END

*== Public (SolveFourPt) ============================================

      LOGICAL FUNCTION SolveFourPt()

      IMPLICIT NONE

      INCLUDE 'network.inc'
      INCLUDE 'solver.inc'
      INCLUDE 'solver2.inc'
      INCLUDE '../input/fixed/misc.f'

*   Purpose:  Interface to SPARSE solver for solution
*             of incremental change in flow/stage

*   Arguments:
      INTEGER I,IterSinceOrder
      SAVE IterSinceOrder
      LOGICAL Scaled

*   Local Variables
      INTEGER Error

      LOGICAL ForwardElim
      EXTERNAL ForwardElim

      INTEGER NetworkIteration,NetworkTimeStep
      EXTERNAL NetworkIteration,NetworkTimeStep

      INTEGER sfOrderAndFactor,sfFactor,sfFileMatrix,sfFileVector
      EXTERNAL sfOrderAndFactor,sfFactor,sfFileMatrix,sfFileVector

*   Argument definitions:
*     Location  - a pointer to the desired location in the
*-----coef matrix

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:   Eli Ateljevich
*   Last modified: July, 1998
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      SolveFourPt = .FALSE.

*-----Create RHS vector of proper size and precision
      If ( FirstTime .OR. (Mod(IterSinceOrder,2000).EQ.0))Then
         Scaled = .True.
         FirstTime = .False.
      Else
         If ( ForwardElim() )Scaled = .False.
      End If



      Do  100 I=1,Equations
         XX(I) = XOld(I) + XAdj(I)
         If ( Scaled ) 	XX(I) = XX(I)*RowScale(i)
 100  Continue

c was 298 not 1

c	If ((NetworkTimeStep().eq.1 .or. NetworkTimeStep().eq. 2)
c	 .or. NetworkTimestep() .eq.324)
c     &	.and. NetworkIteration() .eq. 1)Then
c	Error = SfFileVector(Matrix,XX)
c	print*,Equations
c	End If

      If( ForwardElim() ) Then
         If( Scaled ) Then
            Call sfScale(Matrix,RowScale,ColumnScale)
            Error=sfOrderAndFactor(Matrix,XX,1.D-7,1.D-3,1)
         Else
            Error=sfFactor(Matrix)
         End If
      End If
      Call sfSolve(Matrix,XX,X)
      IterSinceOrder = IterSinceOrder+1

      If (NetworkIteration().GT.7) Then
         Rescale = .9*Rescale
C--------(Sometimes too large)
         If (Rescale.LT.0.01)Rescale = 0.01
         Do  I=1,Equations
            X(I) = X(I) *Rescale
         End Do
      Else
         Rescale = 1.
      End If

      If(Scaled)Then
*--------Need to unscale result
         Do  200 I=1,Equations
            X(I) = X(I) * ColumnScale(I)
 200     Continue
      End If

      SolveFourPt=.True.
      RETURN
      END
