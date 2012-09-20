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

*== Public (InitializeSolver) ==========================================

      logical function InitializeSolver()
      use IO_Units
      use Gates, only: NGate,GateArray
      use grid_data
      use klu
      implicit none

*   Purpose: Initialize the solution matrix

*   Arguments:

*   Argument definitions:

*   Module data:
      include 'network.inc'
        
      include 'solver.inc'
      include 'chconnec.inc'

*   Local Variables:
      integer,parameter :: Zero=0
      integer error             ! error indicator for matrix allocation routine
      integer rowcounter        ! counter for reservoir rows and gate rows
      integer i                 ! loop index 

*   Routines:

      integer, external ::  TotalStreamLocations
      integer, external :: sfcreate
      logical,external ::  ReserveMatrix

*-----Implementation -----------------------------------------------------

      InitializeSolver = .FALSE.

      TotalChanRows = 2*TotalStreamLocations()
      rowcounter=0
      do i=1,nreser
         rowcounter=rowcounter+res_geom(i).nnodes+1   
      end do
      TotalChanResRows=TotalChanRows + rowcounter
      rowcounter=0
      do i=1,ngate
         rowcounter=rowcounter+GateArray(i).nDevice
      end do

      Equations = TotalChanResRows + rowcounter
      if (use_klu) then
            k_common=klu_fortran_init()
      else
            Matrix = sfCreate(Equations,Zero,Error)
      endif
      RowScale=1.D0

*-----Initialize Matrix Scale Factors
      do i=1,Equations
         if (Mod(i,2) .eq. 0 .and. i .le. TotalChanRows ) Then
            ColumnScale(i)=ZscaleFactor
         else
            ColumnScale(i)=1.0
         end If
      end do

*-----Set up matrix-shape indicies.
      if( ReserveMatrix() ) then
      else
         write(UNIT_ERROR,*) ' ReserveMatrix failed...'
         return
      end if

*-----Initialize 'first iteration' index.

      InitializeSolver = .TRUE.
      return
      end

*== Public (CloseSolver) ==========================================

      LOGICAL FUNCTION CloseSolver()
      use klu
      IMPLICIT NONE

*   Purpose: Close and deallocate
*   the matrix solver in SPARSE.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'solver.inc'

*   Programmed by:   Lew DeLong
*   Date:            July 1990
*   Modified by:     Eli Ateljevich
*   Date:			   July 1998
*     

*-----Implementation -----------------------------------------------------

      CloseSolver = .FALSE.
      if (use_klu) then
        call close_solver()
      else
        Call sfDestroy(Matrix)
      end if
      CloseSolver = .TRUE.

      RETURN
      END

*== Public (InitializeMatrix) ==========================================

      LOGICAL FUNCTION InitializeMatrix()
      use klu
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

*   Routines by module:

***** Local:
      LOGICAL  ForwardElim
      EXTERNAL ForwardElim

!    Programmed by: Lew DeLong
!    Date:          January 1991
!    Modified by:   Lew DeLong
!    Last modified: April 1992
!    Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      InitializeMatrix = .FALSE.

      IF( ForwardElim() ) THEN
        if (use_klu) then
              call clear_matrix()
        else
              call sfClear(Matrix)
        end if
 100     CONTINUE
      END IF

      InitializeMatrix = .TRUE.

      RETURN
      END

*== Public (StoreAtLocation) ==========================================

      LOGICAL FUNCTION StoreAtLocation(Location, Val)

      use klu
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
      REAL*8 Val
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

      LOGICAL  ForwardElim
      EXTERNAL ForwardElim

!     Programmed by: Lew DeLong
!     Date:          February 1991
!     Modified by:   Eli Ateljevich

!     -----Implementation -----------------------------------------------------

      Value=0
      IF( ForwardElim() ) THEN
         Value=Val
         if (use_klu) then
               call add_to_matrix(Location,Val)
         else
            Call sfAdd1Real(Location,Value)
         end if
      END IF

      StoreAtLocation = .TRUE.

      RETURN
      END

*== Public (AddAtLocation) ============================================

      LOGICAL FUNCTION AddAtLocation(Location, Val)
      use klu
      IMPLICIT NONE
*   Purpose:  Add a value to the coeficient matrix at a location
*             corresponding to Row and Offset from the diagonal in
*             the virtual matrix.

*   Arguments:
      INTEGER Location
      REAL*8 Val
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

*      Programmed by: Lew DeLong
*      Date:          February 1991
*      Modified by:   Eli Ateljevich
!      Last modified: July, 1998
!      Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      If (ForwardElim()) then
         Value = Val
         if (use_klu) then 
               call add_to_matrix(Location,Val)
         else
               Call sfAdd1Real(Location,Value)
         end if
      End If
      AddAtLocation = .TRUE.

      RETURN
      END

*== Public (SolveFourPt) ============================================

      LOGICAL FUNCTION SolveFourPt()
      Use IO_Units
      USE runtime_data
      use klu
      IMPLICIT NONE

      INCLUDE 'network.inc'
      INCLUDE 'solver.inc'

      INCLUDE 'chstatus.inc'

*   Purpose:  Interface to SPARSE or KLU solver for solution
*             of incremental change in flow/stage

*   Arguments:
      INTEGER I,IterSinceOrder
      logical,save :: FirstTime =.true.
      logical,save :: FirstBackTrack
      logical,save :: LastBackTrack
      Integer MaxNormLoc
      real*8,save :: prevBackLInfNorm


c      character*14, save:: last_date
c      integer, save:: last_max_iteration
      SAVE IterSinceOrder
      LOGICAL Scaled
      real*8, parameter :: CLOSE_L2=1.D0
      real*8, parameter :: MIN_RESCALE=(1.D0/16.D0)
*   Local Variables
      logical lasttime, OK
      INTEGER Error

      LOGICAL ForwardElim
      EXTERNAL ForwardElim

      INTEGER NetworkIteration,NetworkTimeStep,MaxNetworkIterations
      EXTERNAL NetworkIteration,NetworkTimeStep,MaxNetworkIterations

      INTEGER sfOrderAndFactor,sfFactor,sfFileMatrix,sfFileVector,sfPrint
      EXTERNAL sfOrderAndFactor,sfFactor,sfFileMatrix,sfFileVector, sfPrint
      
      INTEGER netIteration
      

*      Argument definitions:
*      Location  - a pointer to the desired location in the
*      coef matrix

*      Programmed by: Eli Ateljevich, Nicky Sandhu (added KLU solver)

*-----Implementation -----------------------------------------------------

      SolveFourPt = .FALSE.
      
*-----Create RHS vector of proper size and precision
      If ( FirstTime ) then ! .OR. (Mod(IterSinceOrder,2000) .le. 1)) then
          if (use_klu) then
              !-- done only once as non-zeros pattern do not change after this point
              k_symbolic = klu_fortran_analyze(matrix_size, ica, jca, k_common)
          end if
         Scaled = .True.
         FirstTime = .False.
      Else
         Scaled = .true.
      End If

      netIteration=NetworkIteration()
      
      If (netIteration.eq.1) then
         if (use_klu .and. IterSinceOrder .lt. 100) then !refactorize for new pattern
             call klu_fortran_free_numeric(k_numeric, k_common)
             k_numeric = klu_fortran_factor(ica, jca, coo, k_symbolic, k_common)      
         end if
         !if (use_klu) then
         !   print *, "First Iteration Condition number (estimated) :",klu_fortran_condest(ica, coo, k_symbolic, k_numeric, k_common)
         !end if
         firstbacktrack=.true.
         lastbacktrack=.false.
         LastLInfNorm=Huge(1.D0)
         LastL2Norm=Huge(1.D0)
         Rescale=1.
      End If
      XX(1:equations)=(XOld(1:equations)+XAdj(1:equations))*RowScale(1:equations)
      if (use_klu) then
         b(1:equations)=xx(1:equations)
      end if
      L2Norm=DOT_PRODUCT(XX,XX)
      maxNormLoc=maxloc(abs(XX),1)
      LInfNorm=abs(XX(maxNormLoc))
      NormClose=(L2Norm .LT. CLOSE_L2) ! .and. LInfNorm .lt. 5.D-1)
      lasttime=(netIteration .eq. MaxNetworkIterations())

      If ( (L2Norm .lt. LastL2Norm .or. L2Norm .lt. CLOSE_L2) 
     &     .or. Rescale .le. MIN_RESCALE  .or. lasttime 
     &     ) then

      If( ForwardElim() ) Then
         If( Scaled ) Then
            if (use_klu) then
            call scale_coo(RowScale, ColumnScale)
            call klu_fortran_refactor(ica, jca, coo, k_symbolic, k_numeric, k_common)
            error=0
            else
            Call sfScale(Matrix,RowScale,ColumnScale)
            Error=sfOrderAndFactor(Matrix,XX,1.D-7,0.D0,1)
            end if
         Else
            if (use_klu) then
                  Error=sfFactor(Matrix)
            end if
         End If
         if(error .ne. 0) then
            write(unit_error,
     &           "('Error in linear algebra. SPARSE returned error code: ',i5)")
     &           error
            call exit(3)
         end if
      End If
      if (use_klu) then
            call klu_fortran_solve(k_symbolic, k_numeric, matrix_size, 1, b, k_common)
            x(1:equations)=b(1:equations)
      else
        Call sfSolve(Matrix,XX,X)
      end if

      X=X*Rescale
      Rescale=min(1.,Rescale*2)
      if (lasttime) rescale=1.
      IterSinceOrder = IterSinceOrder+1

*-----Need to unscale result
      If(Scaled) then
         X(1:equations)=X(1:equations)*ColumnScale(1:equations)
      End If

      FirstBacktrack=.true.
      LastLInfNorm=LInfNorm
      PrevBackLInfNorm=LInfNorm
      LastL2Norm=L2Norm
      lastbacktrack=.false.
      Else
         
         lastbacktrack=.true.
         Rescale=0.5*Rescale
         X=0.5*X
         if(firstbacktrack) then
            X=-X
            firstbacktrack=.false.
         end if
         PrevBackLInfNorm=LInfNorm
      End If
      SolveFourPt=.True.
      RETURN
      END
