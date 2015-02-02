!<license>
!    Copyright (C) 2013 State of California,
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

!> This module is to solve sparse matrix by using KLU Sparse Solver which is part of SuiteSparse package.
!>@ingroup gtm_core
module klu
    use gtm_precision
    implicit none
    logical, parameter :: use_klu = .true.
    logical :: first_time_klu = .true. 
    integer :: k_common, k_symbolic, k_numeric
    integer :: previous_non_zero = LARGEINT
    real(gtm_real), allocatable :: coo(:), b(:)
    integer, allocatable:: jca(:), ica(:)
    integer :: num_non_zeros ! counter increments as elements are added to coo array
    integer :: matrix_size
    
    interface
        integer function klu_fortran_init()
        !DEC$ ATTRIBUTES C :: _klu_fortran_init
        end function
        integer function klu_fortran_analyze(n, ap, ai, k_common)
            !DEC$ ATTRIBUTES C :: _klu_fortran_analyze
            integer, intent(in) :: n,k_common
            integer, intent(in) :: ap(*), ai(*)
        end function
        integer function klu_fortran_factor(ap, ai, ax, k_symbolic, k_common)
            !DEC$ ATTRIBUTES C :: _klu_fortran_factor
            integer, intent(in) :: k_common,k_symbolic
            integer, intent(in) :: ap(*), ai(*)
            real*8, intent(in) :: ax(*)
        end function
        subroutine klu_fortran_refactor(ap, ai, ax, k_symbolic, k_numeric, k_common)
            !DEC$ ATTRIBUTES C :: _klu_fortran_refactor
            integer, intent(in) :: k_symbolic, k_common
            integer, intent(inout) :: k_numeric
            integer, intent(in) :: ap(*), ai(*)
            real*8, intent(in) :: ax(*)
        end subroutine
        subroutine klu_fortran_solve(k_symbolic, k_numeric, n, nrhs, b, k_common)
            !DEC$ ATTRIBUTES C :: _klu_fortran_solve
            integer, intent(in) :: k_symbolic, k_numeric, k_common
            integer, intent(in) :: n, nrhs
            real*8, intent(inout) :: b(*)
        end subroutine
        subroutine klu_fortran_free_numeric(k_numeric, k_common)
            !DEC$ ATTRIBUTES C :: _klu_fortran_free_numeric
            integer, intent(in) :: k_numeric, k_common
        end subroutine
        subroutine klu_fortran_free(k_symbolic, k_numeric, k_common)
            !DEC$ ATTRIBUTES C :: _klu_fortran_free
            integer, intent(in) :: k_symbolic, k_numeric, k_common
        end subroutine
        real*8 function klu_fortran_condest(ap, ax, k_symbolic, k_numeric, k_common)
            integer, intent(in) :: k_symbolic, k_numeric, k_common
            integer, intent(in) :: ap(*)
            real*8, intent(in) :: ax(*)
        end function
    end interface

    contains
      
    subroutine clear_matrix()
        implicit none
        coo = 0
    end subroutine
      
    subroutine close_klu_solver()
        implicit none
        call klu_fortran_free(k_numeric, k_symbolic, k_common)
        deallocate(coo, b, ica, jca)
    end subroutine

    !> write the values from a tri-diagonal matrix to the structure of a sparse matrix
    subroutine tri2sparse(ap,           &
                          ai,           &
                          ax,           &
                          up_diag,      &
                          center_diag,  & 
                          down_diag,    &
                          ncell)
        use gtm_precision
        implicit none
        integer, intent(in) :: ncell
        real(gtm_real), intent(in) :: up_diag(ncell)
        real(gtm_real), intent(in) :: down_diag(ncell)
        real(gtm_real), intent(in) :: center_diag(ncell)
        integer, intent(out) :: ap(ncell+1)
        integer, intent(out) :: ai(4+3*(ncell-2))
        real(gtm_real), intent(out) :: ax(4+3*(ncell-2))
        integer :: i      
        ax = zero  
        ap(1:2) = (/0, 2/)
        ai(1:5) = (/0, 1, 0, 1, 2/)
        ax(1:5) = (/center_diag(1), down_diag(2), up_diag(1), center_diag(2), down_diag(3)/)
        do i = 3, ncell-1
            ap(i) = ap(i-1) + 3
            ai(ap(i)+1) = (i-3) + 1
            ai(ap(i)+2) = (i-3) + 2
            ai(ap(i)+3) = (i-3) + 3
            ax(ap(i)+1) = up_diag(i-3+2)
            ax(ap(i)+2) = center_diag(i-3+3)
            ax(ap(i)+3) = down_diag(i-3+4)
        end do
        ap(ncell) = ap(ncell-1) + 3
        ap(ncell+1) = ap(ncell) + 2
        ai(ap(ncell-1)+1) = ncell-3
        ai(ap(ncell-1)+2) = ncell-2
        ai(ap(ncell-1)+3) = ncell-1
        ai(ap(ncell)+1) = ncell - 2
        ai(ap(ncell)+2) = ncell - 1
        ax(ap(ncell-1)+1) = up_diag(ncell-2)
        ax(ap(ncell-1)+2) = center_diag(ncell-1)
        ax(ap(ncell-1)+3) = down_diag(ncell)
        ax(ap(ncell)+1) = up_diag(ncell-1)
        ax(ap(ncell)+2) = center_diag(ncell)             
        return
    end subroutine    
    
end module
