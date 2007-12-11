C!    Copyright (C) 1996-1999 State of California,
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
C!    Note that the routines below which contain part of an interface to
C!    the SPARSE matrix library were modified by Eli Ateljevich.
C!    
C!    The SPARSE matrix library was created by Kenneth S. Kundert and
C!    the University of California for which copyright information is
C!    given below.

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


*== Public (StoreMassKnown) ============================================

      LOGICAL FUNCTION StoreMassKnown(EqNum, Value)

      IMPLICIT NONE

*   Purpose:  Store known part of load vector, unchanging over
*             iteration, contributed by mass equation.

*   Arguments:
      INTEGER EqNum
      REAL*8    Value

*   Argument definitions:
*     EqNum - mass-equation sequence number, beginning in the network
*              adjacent to the lowest cross-section number and
*              progressing in the direction of increasing cross-section
*              numbers.
*              This number is local to the current channel and the sequence
*              does not include boundary equations.
*     Value - value to be stored.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'solver.inc'

*   Local Variables:
      INTEGER K

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      K = MASSEQROW(EqPointer(Branch) + EqNum - 1)
      XOld(K) = Value
      StoreMassKnown = .TRUE.

      RETURN
      END

*== Public (StoreMassAdjust) ===========================================

      LOGICAL FUNCTION StoreMassAdjust(EqNum, Value)

      IMPLICIT NONE

*   Purpose:  Store adjustment part of load vector, changing over
*             iteration, contributed by mass equation.

*   Arguments:
      INTEGER EqNum
      REAL*8    Value

*   Argument definitions:
*     EqNum - mass-equation sequence number, beginning in the network
*             adjacent to the lowest cross-section number and
*             progressing in the direction of increasing cross-section
*             numbers.
*              This number is local to the current channel and the sequence
*              does not include boundary equations.
*     Value - value to be stored.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'solver.inc'

*   Local Variables:
      INTEGER K

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:   Eli Ateljevich
*   Last modified: July, 1998
*   Version 98.01, January, 1998

*-----Implementation -----------------------------------------------------

      K = MASSEQROW(EqPointer(Branch) + EqNum - 1)
      XAdj(K) = Value
      StoreMassAdjust = .TRUE.

      RETURN
      END

*== Public (StoreDynmKnown) ============================================

      LOGICAL FUNCTION StoreDynmKnown(EqNum, Value)

      IMPLICIT NONE

*   Purpose:  Store known part of load vector, unchanging over
*             iteration, contributed by dynamic equation.

*   Arguments:
      INTEGER EqNum
      REAL*8    Value

*   Argument definitions:
*     EqNum - dynamic-equation sequence number, beginning in the network
*              adjacent to the lowest cross-section number and
*              progressing in the direction of increasing cross-section
*              numbers.
*     Value - value to be stored.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'solver.inc'

*   Local Variables:
      INTEGER K

*   Routines by module:

***** Local:

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:   Eli Ateljevich
*   Last modified: July, 1998
*   Version 98.01, July, 1998

*-----Implementation -----------------------------------------------------

      K = DYNMEQROW(EqPointer(Branch) + EqNum - 1)
      XOld(K) = Value
      StoreDynmKnown = .TRUE.

      RETURN
      END

*== Public (StoreDynmAdjust) ===========================================

      LOGICAL FUNCTION StoreDynmAdjust(EqNum, Value)

      IMPLICIT NONE

*   Purpose:  Store adjustment part of load vector, changing over
*             iteration, contributed by dynamic equation.

*   Arguments:
      INTEGER EqNum
      REAL*8    Value

*   Argument definitions:
*     EqNum - dynamic-equation sequence number, beginning in the network
*             adjacent to the lowest cross-section number and
*             progressing in the direction of increasing cross-section
*             numbers.
*     Value - value to be stored.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'solver.inc'

*   Local Variables:
      INTEGER K

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:   Eli Ateljevich
*   Last modified: July, 1998
*   Version 98.01, July, 1998

*-----Implementation -----------------------------------------------------

      K = DYNMEQROW(EqPointer(Branch) + EqNum - 1)
      XAdj(K) = Value

      StoreDynmAdjust = .TRUE.

      RETURN
      END

*== Public (IncrementalQ) ==============================================

      REAL*8 FUNCTION IncrementalQ(LocationNumber)

      IMPLICIT NONE

*   Purpose:  Return the incremental change in flow over the last
*             iteration.

*   Arguments:
      INTEGER LocationNumber

*   Argument definitions:
*     LocationNumber - sequential location number within the current
*                       channel.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'solver.inc'

*   Local Variables:
      INTEGER J

*   Routines by module:

***** Channel schematic:
      INTEGER  UpstreamPointer
      EXTERNAL UpstreamPointer

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      J = UpstreamPointer() + LocationNumber - 1
      IncrementalQ = X( 2 * J - 1 )

      RETURN
      END

*== Public (IncrementalZ) ==============================================

      REAL*8 FUNCTION IncrementalZ(LocationNumber)

      IMPLICIT NONE

*   Purpose:  Return the incremental change in stream-surface elevation
*             over the last iteration.

*   Arguments:
      INTEGER LocationNumber

*   Argument definitions:
*     LocationNumber - sequential location number within the current
*                      channel.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'solver.inc'

*   Local Variables:
      INTEGER J

*   Routines by module:

***** Channel schematic:
      INTEGER  UpstreamPointer
      EXTERNAL UpstreamPointer

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      J = UpstreamPointer() + LocationNumber - 1
      IncrementalZ = X( 2 * J )

      RETURN
      END


*== Public (ForwardElim) ================================================

      LOGICAL FUNCTION ForwardElim()

      IMPLICIT NONE

*   Purpose:  Return .TRUE. if changes will be
*             performed on the coefficient matrix in the curren iteration,
*             otherwise .FALSE..

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'

*   Local Variables:
      INTEGER Iteration, IterBack

*   Routines by module:

***** Network control:
      INTEGER  NetworkIteration
      INTEGER  ForwardElimInt
      EXTERNAL NetworkIteration
      EXTERNAL ForwardElimInt

***** Local:

*   Intrinsics:
      INTEGER   MOD
      INTRINSIC MOD

*   Programmed by: Lew DeLong
*   Date:          April 1992
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      IterBack = ForwardElimInt()

      IF( IterBack .GT. 1  ) THEN

         Iteration = NetworkIteration()

         IF( Iteration .GT. 1) THEN

            IF( MOD(Iteration-1,IterBack) .EQ. 0 ) THEN
               ForwardElim = .TRUE.
            ELSE
               ForwardElim = .FALSE.
            END IF

         ELSE
            ForwardElim = .TRUE.
         END IF

      ELSE
         ForwardElim = .TRUE.
      END IF

      RETURN
      END

*== Public (StoreAtRow) ================================================

      LOGICAL FUNCTION StoreAtRow(Row, Value)

      IMPLICIT NONE

*   Purpose:  Store a value in the load vector at a location
*             corresponding to Row.

*   Arguments:
      INTEGER Row
      REAL*8    Value

*   Argument definitions:
*     Row   - row number in the load vector.
*     Value - value to be stored.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'solver.inc'

      INTEGER CURRENTCHANNEL
      EXTERNAL CURRENTCHANNEL
*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      XAdj(Row) = Value
      XOld(Row) = 0.0

      StoreAtRow = .TRUE.

      RETURN
      END

*== Public (AddAtRow) ==================================================

      LOGICAL FUNCTION AddAtRow(Row, Value)

      IMPLICIT NONE

*   Purpose:  Add a value to the load vector at a location
*             corresponding to Row.

*   Arguments:
      INTEGER Row
      REAL*8    Value

*   Argument definitions:
*     Row   - row number in the load vector.
*     Value - value to be stored.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'solver.inc'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      XAdj(Row) = XAdj(Row) + Value

      AddAtRow = .TRUE.

      RETURN
      END

*== Public (UpstreamConstraintRow) =====================================

      INTEGER FUNCTION UpstreamConstraintRow()

      IMPLICIT NONE

*   Purpose:  Return row number for the upstream constraint equation
*             for the current channel.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'solver.inc'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      UpstreamConstraintRow = UpConstraintEq(Branch)

      RETURN
      END

*== Public (DownstreamConstraintRow) ===================================

      INTEGER FUNCTION DownstreamConstraintRow()

      IMPLICIT NONE

*   Purpose:  Return row number for the downstream constraint equation
*             for the current channel.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'solver.inc'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      DownstreamConstraintRow = DownConstraintEq(Branch)

      RETURN
      END
