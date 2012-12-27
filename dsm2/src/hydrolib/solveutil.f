!!<license>
!!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
!!    Department of Water Resources.
!!    This file is part of DSM2.

!!    The Delta Simulation Model 2 (DSM2) is free software:
!!    you can redistribute it and/or modify
!!    it under the terms of the GNU General Public License as published by
!!    the Free Software Foundation, either version 3 of the License, or
!!    (at your option) any later version.

!!    DSM2 is distributed in the hope that it will be useful,
!!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!!    GNU General Public License for more details.

!!    You should have received a copy of the GNU General Public License
!!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
!!</license>
!!
!!    For information about the solver routines, contact:
!!    Eli Ateljevich
!!
!****-SPARSE COPYRIGHT *************
!  Revision and copyright information.
!
!  Copyright (c) 1985,86,87,88
!  by Kenneth S. Kundert and the University of California.
!
!  Permission to use, copy, modify, and distribute this software and
!  its documentation for any purpose and without fee is hereby granted,
!  provided that the copyright notices appear in all copies and
!  supporting documentation and that the authors and the University of
!  California are properly credited.  The authors and the University of
!  California make no representations as to the suitability of this
!  software for any purpose.  It is provided `as is', without express
!  or implied warranty.
!
!***********************************************************************


!== Public (StoreMassKnown) ============================================
module solveutil
    use solver
    implicit none
contains
    logical function StoreMassKnown(EqNum, Value)

        use network
        implicit none

        !   Purpose:  Store known part of load vector, unchanging over
        !             iteration, contributed by mass equation.

        !   Arguments:
        integer EqNum
        real*8    Value

        !   Argument definitions:
        !     EqNum - mass-equation sequence number, beginning in the network
        !              adjacent to the lowest cross-section number and
        !              progressing in the direction of increasing cross-section
        !              numbers.
        !              This number is local to the current channel and the sequence
        !              does not include boundary equations.
        !     Value - value to be stored.

        !   Module data:
        !   Local Variables:
        integer K

        !   Programmed by: Lew DeLong
        !   Date:          February 1991
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        K = MASSEQROW(EqPointer(Branch) + EqNum - 1)
        XOld(K) = Value
        StoreMassKnown = .true.

        return
    end function

    !== Public (StoreMassAdjust) ===========================================

    logical function StoreMassAdjust(EqNum, Value)

        use network
        implicit none

        !   Purpose:  Store adjustment part of load vector, changing over
        !             iteration, contributed by mass equation.

        !   Arguments:
        integer EqNum
        real*8    Value

        !   Argument definitions:
        !     EqNum - mass-equation sequence number, beginning in the network
        !             adjacent to the lowest cross-section number and
        !             progressing in the direction of increasing cross-section
        !             numbers.
        !              This number is local to the current channel and the sequence
        !              does not include boundary equations.
        !     Value - value to be stored.

        !   Module data:

        !   Local Variables:
        integer K

        !   Programmed by: Lew DeLong
        !   Date:          February 1991
        !   Modified by:   Eli Ateljevich
        !   Last modified: July, 1998
        !   Version 98.01, January, 1998

        !-----Implementation -----------------------------------------------------

        K = MASSEQROW(EqPointer(Branch) + EqNum - 1)
        XAdj(K) = Value
        StoreMassAdjust = .true.

        return
    end function

    !== Public (StoreDynmKnown) ============================================

    logical function StoreDynmKnown(EqNum, Value)

        use network
        implicit none

        !   Purpose:  Store known part of load vector, unchanging over
        !             iteration, contributed by dynamic equation.

        !   Arguments:
        integer EqNum
        real*8    Value

        !   Argument definitions:
        !     EqNum - dynamic-equation sequence number, beginning in the network
        !              adjacent to the lowest cross-section number and
        !              progressing in the direction of increasing cross-section
        !              numbers.
        !     Value - value to be stored.


        !   Local Variables:
        integer K

        !   Routines by module:

        !**** Local:

        !   Programmed by: Lew DeLong
        !   Date:          February 1991
        !   Modified by:   Eli Ateljevich
        !   Last modified: July, 1998
        !   Version 98.01, July, 1998

        !-----Implementation -----------------------------------------------------

        K = DYNMEQROW(EqPointer(Branch) + EqNum - 1)
        XOld(K) = Value
        StoreDynmKnown = .true.

        return
    end function

    !== Public (StoreDynmAdjust) ===========================================

    logical function StoreDynmAdjust(EqNum, Value)

        use network

        implicit none

        !   Purpose:  Store adjustment part of load vector, changing over
        !             iteration, contributed by dynamic equation.

        !   Arguments:
        integer EqNum
        real*8    Value

        !   Argument definitions:
        !     EqNum - dynamic-equation sequence number, beginning in the network
        !             adjacent to the lowest cross-section number and
        !             progressing in the direction of increasing cross-section
        !             numbers.
        !     Value - value to be stored.


        !   Local Variables:
        integer K

        !   Routines by module:

        !   Programmed by: Lew DeLong
        !   Date:          February 1991
        !   Modified by:   Eli Ateljevich
        !   Last modified: July, 1998
        !   Version 98.01, July, 1998

        !-----Implementation -----------------------------------------------------

        K = DYNMEQROW(EqPointer(Branch) + EqNum - 1)
        XAdj(K) = Value

        StoreDynmAdjust = .true.

        return
    end function

    !== Public (IncrementalQ) ==============================================

    real*8 function IncrementalQ(LocationNumber)

        use network
        use channel_schematic, only: UpstreamPointer
        implicit none

        !   Purpose:  Return the incremental change in flow over the last
        !             iteration.

        !   Arguments:
        integer LocationNumber

        !   Argument definitions:
        !     LocationNumber - sequential location number within the current
        !                       channel.

        !   Local Variables:
        integer J



        !   Programmed by: Lew DeLong
        !   Date:          February 1991
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        J = UpstreamPointer() + LocationNumber - 1
        IncrementalQ = X( 2 * J - 1 )

        return
    end function

    !== Public (IncrementalZ) ==============================================

    real*8 function IncrementalZ(LocationNumber)

        use network
        use channel_schematic, only: UpstreamPointer
        implicit none

        !   Purpose:  Return the incremental change in stream-surface elevation
        !             over the last iteration.

        !   Arguments:
        integer LocationNumber

        !   Argument definitions:
        !     LocationNumber - sequential location number within the current
        !                      channel.

        !   Local Variables:
        integer J



        !   Programmed by: Lew DeLong
        !   Date:          February 1991
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        J = UpstreamPointer() + LocationNumber - 1
        IncrementalZ = X( 2 * J )

        return
    end function


    !== Public (StoreAtRow) ================================================

    logical function StoreAtRow(Row, Value)

        use network
        implicit none

        !   Purpose:  Store a value in the load vector at a location
        !             corresponding to Row.

        !   Arguments:
        integer Row
        real*8    Value

        !   Argument definitions:
        !     Row   - row number in the load vector.
        !     Value - value to be stored.


        integer CURRENTCHANNEL
        external CURRENTCHANNEL
        !   Local Variables:

        !   Routines by module:

        !   Programmed by: Lew DeLong
        !   Date:          February 1991
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        XAdj(Row) = Value
        XOld(Row) = 0.0

        StoreAtRow = .true.

        return
    end function

    !== Public (AddAtRow) ==================================================

    logical function AddAtRow(Row, Value)

        use network
        use solver
        implicit none

        !   Purpose:  Add a value to the load vector at a location
        !             corresponding to Row.

        !   Arguments:
        integer Row
        real*8    Value

        !   Argument definitions:
        !     Row   - row number in the load vector.
        !     Value - value to be stored.


        !   Local Variables:

        !   Routines by module:

        !   Programmed by: Lew DeLong
        !   Date:          February 1991
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        XAdj(Row) = XAdj(Row) + Value

        AddAtRow = .true.

        return
    end function

    !== Public (UpstreamConstraintRow) =====================================

    integer function UpstreamConstraintRow()

        use network
        use solver
        implicit none

        !   Purpose:  Return row number for the upstream constraint equation
        !             for the current channel.

        !   Arguments:

        !   Argument definitions:


        !   Local Variables:

        !   Routines by module:

        !   Programmed by: Lew DeLong
        !   Date:          February 1991
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        UpstreamConstraintRow = UpConstraintEq(Branch)

        return
    end function

    !== Public (DownstreamConstraintRow) ===================================

    integer function DownstreamConstraintRow()

        use network
        use solver
        implicit none

        !   Purpose:  Return row number for the downstream constraint equation
        !             for the current channel.

        !   Arguments:

        !   Argument definitions:

        !   Local Variables:

        !   Routines by module:

        !   Programmed by: Lew DeLong
        !   Date:          February 1991
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        DownstreamConstraintRow = DownConstraintEq(Branch)

        return
    end function
end module
