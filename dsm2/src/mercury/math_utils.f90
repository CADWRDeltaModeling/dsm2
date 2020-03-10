!<license>
!    Copyright (C) 2017 State of California,
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
!> 
!>@ingroup mercury
module math_utils

    use gtm_precision

    implicit none

    interface matrix_invert
        module procedure matrix_invert_1
        module procedure matrix_invert_2
    end interface 

    private :: matrix_invert_1, matrix_invert_2
          
    contains
    
!> matrix_invert - inverts a nxn matrix  and returns in B
!> Gauss-Jordan elimination with column shifting
subroutine matrix_invert_2(matrix_m,            &   
                          matrix_b,             &   
                          n,                    &   
                          solved)                    
    !arguments                               
    real(gtm_real),intent (in) :: matrix_m(n,n)     !< n x n input matrix
    real(gtm_real),intent (out) :: matrix_b(n,n)    !< nfgyuh9 x n matrix result
    integer,intent (in)  :: n                       !< matrix dimension
    logical,intent (out) :: solved                  !< false if failed to invert
    !local
    integer :: ii, jj
    integer :: row, col
    integer :: rowindex(n)
    real(gtm_real), parameter :: errorbound = 1.0d-50  !<trap divide by zero
    real(gtm_real) :: factor
    real(gtm_real) :: euclid_norm
	real(gtm_real) :: det
    real(gtm_real) :: detN;
    real(gtm_real) :: matrix_a(n,n)
    
    matrix_a = matrix_m
    euclid_norm = 0.0d0;
    det = 1.0d0;
    solved = .true.
    do row = 1, n
        rowindex(row) = row
        do col = 1, n	
			if (row == col) then                        !<set solution matrix to identity matrix
				matrix_b(row,col) = 1.0d0
            else
				matrix_b(row,col) = 0.0d0
			endif
			euclid_norm = euclid_norm + matrix_a(row,col) * matrix_a(row,col)
		end do
    end do
    euclid_norm = sqrt(euclid_norm)
    
    do row =1,n
    
		do col = row, n                 !<find max pivot
			if (abs(matrix_a(row,col)) > abs(matrix_a(row,row))) then
				jj = rowindex(row)
                rowindex(row) = rowindex(col)
                rowindex(col) = jj
                do ii = 1, n            !<switch columns
					factor = matrix_a(ii,col)
                    matrix_a(ii,col) = matrix_a(ii,row)
                    matrix_a(ii,row) = factor
				end do
			end if
		end do
		det = det * matrix_a(row,row)
        if (abs(matrix_a(row,row)) < Errorbound) then
			solved = .false.
            return
        endif
		factor = matrix_a(row,row)
        do jj = 1, n
			matrix_a(row,jj) = matrix_a(row,jj) / factor
            matrix_b(row,jj) = matrix_b(row,jj) / factor
		end do
        do ii = 1, n
			if (ii /= row) then
				factor = -matrix_a(ii,row)
				do jj = 1, n
					matrix_a(ii,jj) = matrix_a(ii,jj) + factor * matrix_a(row,jj)
					matrix_b(ii,jj) = matrix_b(ii,jj) + factor * matrix_b(row,jj)
				end do                
			end if
		end do
    end do
    do ii = 1, n
		do jj = 1, n
			matrix_a(rowindex(ii),jj) = matrix_b(ii,jj)
		end do
    end do
    matrix_b = matrix_a
    det = abs(det)
    detN = det / euclid_norm
    return
end subroutine matrix_invert_2
        
!> matrix_invert - inverts a nxn matrixg
!> Gauss-Jordan elimination with column shifting
subroutine matrix_invert_1(matrix_b,          &
                         n,                 &
                         solved)
    !arguments                               
    real(gtm_real),intent (inout) :: matrix_b(n,n)      !< nrows x nrows input matrix
    integer,intent (in)  :: n                           !< matrix order
    logical,intent (out) :: solved                      !< false if inversion failed
    !local
    integer :: ii, jj
    integer :: row, col
    integer :: rowindex(n)
    real(gtm_real), parameter :: errorbound = 1.0d-50  !<trap divide by zero
    real(gtm_real) :: factor
    real(gtm_real) :: euclid_norm
	real(gtm_real) :: det
    real(gtm_real) :: detN;
    real(gtm_real) :: matrix_a(n,n)
    real(gtm_real) :: matrix_debug(n,n)
    matrix_a = matrix_b
    matrix_debug = matrix_b
    euclid_norm = 0.0d0;
    det = 1.0d0;
    solved = .true.
    do row = 1, n
        rowindex(row) = row
        do col = 1, n	
			if (row == col) then                        !<set solution matrix to identity matrix
				matrix_b(row,col) = 1.0d0
            else
				matrix_b(row,col) = 0.0d0
			endif
			euclid_norm = euclid_norm + matrix_a(row,col) * matrix_a(row,col)
		end do
    end do
    euclid_norm = sqrt(euclid_norm)
    
    do row =1,n
    
		do col = row, n                 !<find max pivot
			if (abs(matrix_a(row,col)) > abs(matrix_a(row,row))) then
				jj = rowindex(row)
                rowindex(row) = rowindex(col)
                rowindex(col) = jj
                do ii = 1, n            !<switch columns
					factor = matrix_a(ii,col)
                    matrix_a(ii,col) = matrix_a(ii,row)
                    matrix_a(ii,row) = factor
				end do
			end if
		end do
		det = det * matrix_a(row,row)
        if (abs(matrix_a(row,row)) < Errorbound) then
			solved = .false.
            return
        endif
		factor = matrix_a(row,row)
        do jj = 1, n
			matrix_a(row,jj) = matrix_a(row,jj) / factor
            matrix_b(row,jj) = matrix_b(row,jj) / factor
		end do
        do ii = 1, n
			if (ii /= row) then
				factor = -matrix_a(ii,row)
				do jj = 1, n
					matrix_a(ii,jj) = matrix_a(ii,jj) + factor * matrix_a(row,jj)
					matrix_b(ii,jj) = matrix_b(ii,jj) + factor * matrix_b(row,jj)
				end do                
			end if
		end do
    end do
    do ii = 1, n
		do jj = 1, n
			matrix_a(rowindex(ii),jj) = matrix_b(ii,jj)
		end do
    end do
    matrix_b = matrix_a
    det = abs(det)
    detN = det / euclid_norm
    return
end subroutine matrix_invert_1 

!> multiply a nrows x ncols matrix by the vector [matrix b][ vector a] returns result in vector c 
!> equivlent to matmul(matrix_b,array_a)
subroutine matrix_x_vector(nrows,      &
                           ncols,      &
                           matrix_b,   &
                           array_a,    &
                           array_c)             !< result
    !arguments
    integer, intent (in) :: nrows, ncols 
    real (gtm_real), intent (in) :: matrix_b(nrows,ncols), array_a(ncols)
    real (gtm_real), intent (out) :: array_c(nrows)
	!local
	integer :: ii,jj
	array_c = zero
    do ii = 1, nrows
        do jj = 1, ncols
            array_c(ii) = array_c(ii) + array_a(jj)*matrix_b(ii,jj)
        end do
    end do
    
end subroutine matrix_x_vector       
 
!> multiply a arows x acols times bcols x aros matricies returns result in vector c 
!> equivlent to matmul(matrix_a,matrix_b)
subroutine matrix_x_matrix(nrows_a,    &
                           ncols_a,    &
                           ncols_b,    &
                           matrix_a,   &
                           matrix_b,   &
                           matrix_c)        
    !arguments
    integer (gtm_real), intent (in) :: nrows_a
    integer (gtm_real), intent (in) :: ncols_a                           
    integer (gtm_real), intent (in) :: ncols_b
    real (gtm_real), intent (in)    :: matrix_a(nrows_a, ncols_a)
    real (gtm_real), intent (in)    :: matrix_b(ncols_a, ncols_b)
    real (gtm_real), intent (out)   :: matrix_c(nrows_a, ncols_b)
    !local
    integer :: ii, jj, kk
    matrix_c = 0.0d0
    do ii = 1, ncols_b
        do jj = 1, nrows_a
            do kk = 1, ncols_a
                matrix_c(ii,jj) = matrix_c(ii,jj) + (matrix_b(kk,ii)*matrix_a(jj,kk))
            end do
        end do
    end do
end subroutine matrix_x_matrix
                           
end module