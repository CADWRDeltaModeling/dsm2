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
C!
C!    For information about the solver routines, contact: 
C!    Eli Ateljevich
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
