!!<license>
!!    Copyright (C) 1996, 1997, 1998, 2001, 2007 State of California,
!!    Department of Water Resources.
!!    This file is part of DSM2.

!!    DSM2 is free software: you can redistribute it and/or modify
!!    it under the terms of the GNU General Public !<license as published by
!!    the Free Software Foundation, either version 3 of the !<license, or
!!    (at your option) any later version.

!!    DSM2 is distributed in the hope that it will be useful,
!!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!!    GNU General Public !<license for more details.

!!    You should have received a copy of the GNU General Public !<license
!!    along with DSM2.  If not, see <http://www.gnu.org/!<licenses/>.
!!</license>

!===== BOF Solver.inc ================================================
!   Version 98.01, July, 1998

!   Note: This include, when used, must follow after "Network.inc".

module solver
    use network
    implicit none
    logical, save:: resetSolver=.true.
    integer,parameter :: MaxGatePtr=MaxNGate*5*4+ MaxNGate*5*4+MaxNgate*5
    integer, parameter :: MAX_EQUATION=(2*MaxLocations+MaxNRes*(1+MaxResConnectChannel))
    real*8, parameter :: ZscaleFactor=1.D0
	real*8, parameter :: MassScaleFactor=1.D0/(32.D0*1024.D0)
	real*8, parameter :: DynScaleFactor=1.D0/(1024.D0)
	real*8, parameter :: ResScaleFactor=1.D0
	real*8, parameter :: ResConnectScaleFactor=128.D0
    integer, save::  Matrix &
        ,MassEq(4,MaxLocations) &
        ,DynmEq(4,MaxLocations) &
        ,MassEqRow(MaxLocations) &
        ,DynmEqRow(MaxLocations) &
        ,TotalChanRows & ! Total number of rows (equations) dedicated to channels. &
        ,TotalChanResRows &  ! Total number of rows (equations) dedicated to channels and reservoirs
        !-------fixme: make the following limits better
        ,ConstraintPointers(7*MaxChannels) &
        ,ResEqPointer(6*MaxResConnectChannel*MaxNRes + MaxNRes) &
        ,Obj2objEqPointer(100) & !   based on reservoir gate with 5 pipes and 5 weirs per gate &
        ,GateEqPointer(MaxGatePtr) &
        ,ResEqIndex(MaxNRes),ResEqRow(MaxNRes), obj2objEqIndex(20) &
        ,GateEqIndex(MaxNGate),GateEqRow(MaxNGate),GateNodeRow(MaxNGate)
    integer, save::   UpConstraintEq(MaxChannels), DownConstraintEq(MaxChannels)
    integer, save::   UpConstraintIndex(MaxChannels), DownConstraintIndex(MaxChannels), EqPointer(MaxChannels)

    logical*4,save:: NormClose
    real*8,save :: LInfNorm,LastLInfNorm,L2Norm,LastL2Norm
    !-------Don't change the size of these variables!!!!
    real*8, save ::  XX(2*MaxLocations+MaxNRes*(1+MaxResConnectChannel)) &
        , X(MAX_EQUATION) &
        , XOld(MAX_EQUATION) &
        , XAdj(MAX_EQUATION) &
        , ColumnScale(MAX_EQUATION),RowScale(MAX_EQUATION) &
        , Rescale

    integer, save:: Equations, ErrUnit
      
 
contains
    !****************-SPARSE COPYRIGHT *************
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
    !== Public (CloseSolver) ==========================================

    logical function CloseSolver()
        use klu
      
        implicit none

        !   Purpose: Close and deallocate
        !   the matrix solver in SPARSE.

        !   Arguments:

        !   Argument definitions:

        !   Programmed by:   Lew DeLong
        !   Date:            July 1990
        !   Modified by:     Eli Ateljevich
        !   Date:			   July 1998
        !

        !-----Implementation -----------------------------------------------------

        CloseSolver = .false.
        if (use_klu) then
            call close_solver()
        else
            call sfDestroy(Matrix)
        end if
        CloseSolver = .true.

        return
    end function

    !== Public (InitializeMatrix) ==========================================

    logical function InitializeMatrix()
      
        use klu
        implicit none

        !   Purpose:  Fourpoint allows for two levels of solution.. In the first,
        !-----both the coefficient matrix and the RHS are updated and
        !             solved. In the second, only the RHS is updated.

        !   Arguments:

        !   Argument definitions:

        !   Local Variables:

        !   Routines by module:

        !    Programmed by: Lew DeLong
        !    Date:          January 1991
        !    Modified by:   Lew DeLong
        !    Last modified: April 1992
        !    Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        InitializeMatrix = .false.

        if( ForwardElim() ) then
            if (use_klu) then
                call clear_matrix()
            else
                call sfClear(Matrix)
            end if
100     continue
    end if

    InitializeMatrix = .true.

    return
end function

!== Public (StoreAtLocation) ==========================================

logical function StoreAtLocation(Location, Val)
      
    use klu
    implicit none
    !   Purpose:  Store a value in the coeficient matrix at a location
    !             corresponding to an integer pointer (Location)

    !             This Function has no counterpart in SPARSE
    !             Functionality is largely redundant with AddAtLocation
    !-----It is kept here to allow control using ForwElim()
    !             Values are stored only if complete forward elimination
    !             is to be performed on the coefficient matrix, indicated
    !             by ForwardElim() = .TRUE..

    !   Arguments:
    integer Location
    double precision Value
    real*8 Val
    !   Argument definitions:
    !     Location  - a pointer to the desired location in the
    !-----coef matrix

    !     Value  - value to be stored. Incoming number may not be
    !     Double precision, but must be stored as double precision
    !     for SPARSE solver.

    !   Local Variables:

    !     Programmed by: Lew DeLong
    !     Date:          February 1991
    !     Modified by:   Eli Ateljevich

    !     -----Implementation -----------------------------------------------------

    Value=0
    if( ForwardElim() ) then
        Value=Val
        if (use_klu) then
            call add_to_matrix(Location,Val)
        else
            call sfAdd1Real(Location,Value)
        end if
    end if

    StoreAtLocation = .true.

    return
end function

!== Public (AddAtLocation) ============================================

logical function AddAtLocation(Location, Val)
      
    use klu
    implicit none
    !   Purpose:  Add a value to the coeficient matrix at a location
    !             corresponding to Row and Offset from the diagonal in
    !             the virtual matrix.

    !   Arguments:
    integer Location
    real*8 Val
    double precision VALUE


    !   Argument definitions:
    !     Location  - a pointer to the desired location in the
    !-----coef matrix

    !     Value  - value to be stored. Incoming number may not be
    !     Double precision, but must be stored as double precision
    !     for SPARSE solver.

    !   Local Variables:

    !   Routines by module:

    !      Programmed by: Lew DeLong
    !      Date:          February 1991
    !      Modified by:   Eli Ateljevich
    !      Last modified: July, 1998
    !      Version 93.01, January, 1993

    !-----Implementation -----------------------------------------------------

    if (ForwardElim()) then
        Value = Val
        if (use_klu) then
            call add_to_matrix(Location,Val)
        else
            call sfAdd1Real(Location,Value)
        end if
    end if
    AddAtLocation = .true.

    return
end function


!== Public (SolveFourPt) ============================================

logical function SolveFourPt()
    use IO_Units
    use runtime_data
    use klu
    use chstatus
    use netcntrl
    implicit none

    !   Purpose:  Interface to SPARSE or KLU solver for solution
    !             of incremental change in flow/stage

    !   Arguments:
    integer IterSinceOrder
    logical,save :: FirstTime =.true.
    logical,save :: FirstBackTrack
    logical,save :: LastBackTrack
    integer MaxNormLoc
    real*8,save :: prevBackLInfNorm
    !      character*14, save:: last_date
    !      integer, save:: last_max_iteration
    save IterSinceOrder
    logical Scaled
    real*8, parameter :: CLOSE_L2=1.D0
    real*8, parameter :: MIN_RESCALE=(1.D0/16.D0)
    
    !   Local Variables
    logical lasttime
    integer Error

    integer sfOrderAndFactor,sfFactor,sfFileMatrix,sfFileVector,sfPrint
    external sfOrderAndFactor,sfFactor,sfFileMatrix,sfFileVector, sfPrint
      
    integer netIteration

    !      Argument definitions:
    !      Location  - a pointer to the desired location in the
    !      coef matrix

    !      Programmed by: Eli Ateljevich, Nicky Sandhu (added KLU solver)

    !-----Implementation -----------------------------------------------------

    SolveFourPt = .false.
      
    !-----Create RHS vector of proper size and precision
    if ( FirstTime ) then ! .OR. (Mod(IterSinceOrder,2000) .le. 1)) then
        if (use_klu) then
           !-- done only once as non-zeros pattern do not change after this point
            k_symbolic = klu_fortran_analyze(matrix_size, ica, jca, k_common)
        end if
        Scaled = .true.
        FirstTime = .false.
    else
        Scaled = .true.
    end if

    netIteration=NetworkIteration()
      
    if (netIteration==1) then
        firstbacktrack=.true.
        lastbacktrack=.false.
        LastLInfNorm=Huge(1.D0)
        LastL2Norm=Huge(1.D0)
        Rescale=1.
    end if
    XX(1:equations)=(XOld(1:equations)+XAdj(1:equations))*RowScale(1:equations)
    if (use_klu) then
        b(1:equations)=xx(1:equations)
    end if
    L2Norm=DOT_PRODUCT(XX,XX)
    maxNormLoc=maxloc(abs(XX),1)
    LInfNorm=abs(XX(maxNormLoc))
    NormClose=(L2Norm < CLOSE_L2) ! .and. LInfNorm .lt. 5.D-1)
    lasttime=(netIteration == MaxNetworkIterations())
    if ( (L2Norm < LastL2Norm .or. L2Norm < CLOSE_L2)  &
        .or. Rescale <= MIN_RESCALE  .or. lasttime  &
        ) then

        if( ForwardElim() ) then
            if( Scaled ) then
                if (use_klu) then
                    call scale_coo(RowScale, ColumnScale)
                    if ( resetSolver ) then !refactorize for new pattern
                        call klu_fortran_free_numeric(k_numeric, k_common)
                        k_numeric = klu_fortran_factor(ica, jca, coo, k_symbolic, k_common)
                        resetSolver=.false.
                    else
                        call klu_fortran_refactor(ica, jca, coo, k_symbolic, k_numeric, k_common)
                    end if
                    error=0
                else
                    call sfScale(Matrix,RowScale,ColumnScale)
                    Error=sfOrderAndFactor(Matrix,XX,1.D-7,0.D0,1)
                end if
            else
                if (use_klu) then
                    Error=sfFactor(Matrix)
                end if
            end if
            if(error /= 0) then
                write(unit_error, &
                    "('Error in linear algebra. SPARSE returned error code: ',i5)") &
                    error
                call exit(3)
            end if
        end if
        if (use_klu) then
            call klu_fortran_solve(k_symbolic, k_numeric, matrix_size, 1, b, k_common)
            x(1:equations)=b(1:equations)
        else
            call sfSolve(Matrix,XX,X)
        end if

        X=X*Rescale
        Rescale=min(1.,Rescale*2)
        if (lasttime) rescale=1.
        IterSinceOrder = IterSinceOrder+1
        !-----Need to unscale result
        if(Scaled) then
            X(1:equations)=X(1:equations)*ColumnScale(1:equations)
        end if
        FirstBacktrack=.true.
        LastLInfNorm=LInfNorm
        PrevBackLInfNorm=LInfNorm
        LastL2Norm=L2Norm
        lastbacktrack=.false.
    else
         
        lastbacktrack=.true.
        Rescale=0.5*Rescale
        X=0.5*X
        if(firstbacktrack) then
            X=-X
            firstbacktrack=.false.
        end if
        PrevBackLInfNorm=LInfNorm
    end if
    SolveFourPt=.true.
    return
end function

!== Public (ForwardElim) ================================================

logical function ForwardElim()

    use network
    use netcntrl, only: NetworkIteration, ForwardElimInt

    implicit none

    !   Purpose:  Return .TRUE. if changes will be
    !             performed on the coefficient matrix in the curren iteration,
    !             otherwise .FALSE..

    !   Arguments:

    !   Argument definitions:


    !   Local Variables:
    integer Iteration, IterBack

    !   Routines by module:

    !**** Local:

    !   Intrinsics:
    integer   MOD
    intrinsic MOD

    !   Programmed by: Lew DeLong
    !   Date:          April 1992
    !   Modified by:
    !   Last modified:
    !   Version 93.01, January, 1993

    !-----Implementation -----------------------------------------------------

    IterBack = ForwardElimInt()

    if( IterBack > 1  ) then

        Iteration = NetworkIteration()

        if( Iteration > 1) then

            if( MOD(Iteration-1,IterBack) == 0 ) then
                ForwardElim = .true.
            else
                ForwardElim = .false.
            end if

        else
            ForwardElim = .true.
        end if

    else
        ForwardElim = .true.
    end if

    return
end function

     

end module
