!<license>
!    Copyright (C) 2015 State of California,
!    Department of Water Resources.
!    This file is part of DSM2-GTM.
!
!    The Delta Simulation Model 2 (DSM2) - General Transport Model (GTM) 
!    is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    DSM2 is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
!</license>

!> Matrix solver module for the system of linear equations.
!> The solvers include:
!>   - Tri-diagonal solver using todo:? Thomas algorithm
!>   - Penta-diagonal solver using todo:? algorithm
!>   - Sparse matrix solver 
!>@ingroup transport
module matrix_solver

  use gtm_precision

  contains

  !> Solves a tridiagonal system. 
  !> This sub-routine was taken from the Numerical Recipes in Fortran, page 43 (Edition of 1992)
  !> 
  !>             [B1,C1,00,00,00]  
  !> [X1,X2,...] [A2,B2,C2,00,00] = [D1,D2,....]
  !>             [00,A3,B3,C3,00]  
  !>             [00,00,A4,B4,C4]
  !>             [00,00,00,A5,B5]    
  !>
  !> Variables:
  !>
  !> A(i): Values of the coefficients below diagonal in matrix
  !> B(i): Values of the coefficients at the diagonal in matrix
  !> C(i): Values of the coefficients above diagonal in matrix
  !> D(i): Values of the right hand side vector 
  !> X(i): Values of the computed solution
  pure subroutine tridi_solver(center_diag,          &
                               up_diag,              &     
                               down_diag,            &
                               right_hand_side,      &
                               conc,                 &
                               ncell)
      ! --- args
      integer,intent (in) :: ncell                          !< Number of volumes 
      real(gtm_real),intent (out) :: conc(ncell)            !< Values of the computed solution
      real(gtm_real),intent (in)  :: down_diag(ncell)       !< Values of the coefficients below diagonal in matrix
      real(gtm_real),intent (in)  :: center_diag(ncell)     !< Values of the coefficients at the diagonal in matrix
      real(gtm_real),intent (in)  :: up_diag(ncell)         !< Values of the coefficients above the diagonal in matrix
      real(gtm_real),intent (in)  :: right_hand_side(ncell) !< Values of the right hand side vector
    
      !--- Local 
      integer :: ivar
      real(gtm_real) :: gam(ncell)
      real(gtm_real) :: bet

      ! todo: what is this?
      if(center_diag(1) == zero)then
          !call gtm_fail("Error in tridiagonal system of equations")
      end if

      bet = center_diag(1)
      conc(1) = right_hand_side(1) / bet

      do ivar = 2, ncell
          gam(ivar) = up_diag(ivar - 1) / bet
          bet = center_diag(ivar) - down_diag(ivar) * gam(ivar)
          if(bet == 0)then     
              !call stm_fail("Error in tridiagonal solution")
          end if
          conc(ivar) = (right_hand_side(ivar) - down_diag(ivar) * conc(ivar - 1)) / bet 
      end do

      do ivar= ncell-1, 1, -1
          conc(ivar) = conc(ivar) - gam(ivar + 1) * conc(ivar + 1)
      end do

      return
  end subroutine 
    
end module  
