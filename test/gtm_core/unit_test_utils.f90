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
!>@ingroup test_gtm_core
module ut_utils

    use fruit 
    
    contains
    
    !> Test QsortC function
    subroutine test_qsort
        use utils
        use gtm_precision, only: weakest_eps
        implicit none
        integer :: A(3,5), B(5)
        A(1,:) = (/41, 32, 34, 3, -1/)
        A(2,:) = (/4, -32, 34, 34, 10/)
        A(3,:) = (/1, 3, -5, 18, 16/)
        B(1:5) = (/1, 2, 3, 4, 5/)
        call QsortCI(A(1,:),B)
        call assertEquals(dble(A(1,1)), dble(-1), weakest_eps, "problem in test_qsort")
        call assertEquals(dble(B(1)), dble(5), weakest_eps, "problem in test_qsort")
        call assertEquals(dble(A(1,3)), dble(32), weakest_eps, "problem in test_qsort")
        call assertEquals(dble(B(3)), dble(2), weakest_eps, "problem in test_qsort")
        call QsortC(A(3,:))
        call assertEquals(dble(A(3,4)), dble(16), weakest_eps, "problem in test_qsort")
        return
    end subroutine    


    !> Test rowcol2apai function
    !       [   4.0  -1.5  -2.5   0.0  3.0 ]
    !       [  -1.5   8.0   0.0   0.0  0.0 ] 
    !   A = [   1.1   0.0  16.0  -1.0  0.0 ]
    !       [   0.0   0.0  -3.0  11.0  0.0 ]
    !       [   0.0   0.0   0.0   0.0  1.0 ]    
    ! ap = /0, 3, 5, 8, 10, 12/
    ! ai = /0,1,2,0,1,0,2,3,2,3,0,4/
    subroutine test_rowcol2apai
        use utils
        use gtm_precision
        implicit none
        real(gtm_real) :: val(12), ax(12)
        integer :: row(12), col(12), rci(12)
        integer :: ap(6), ai(12)
        integer :: nrci(12)
        integer :: i
        rci(1:12) = (/1,2,3,4,5,6,7,8,9,10,11,12/)
        row = (/1,1,1,1,2,2,3,3,3,4,4,5/)
        col = (/1,2,3,5,1,2,1,3,4,3,4,5/)          
        call rowcol2apai(ap, ai, row, col, rci, 12, 5)
        call assertEquals(dble(ap(3)), dble(5), weakest_eps, "problem in rowcol2apai")
        call assertEquals(dble(ap(5)), dble(10), weakest_eps, "problem in rowcol2apai")
        call assertEquals(dble(ai(7)), dble(2), weakest_eps, "problem in rowcol2apai")
        call assertEquals(dble(ai(10)), dble(3), weakest_eps, "problem in rowcol2apai")
        ! re-arrange the array and see if that changes the results
        rci(1:12) = (/1,2,3,4,5,6,7,8,9,10,11,12/)
        row = (/1,1,1,1,3,3,3,2,2,4,4,5/)
        col = (/3,5,1,2,4,1,3,2,1,4,3,5/)          
        call rowcol2apai(ap, ai, row, col, rci, 12, 5)
        call assertEquals(dble(ap(3)), dble(5), weakest_eps, "problem in rowcol2apai")
        call assertEquals(dble(ap(5)), dble(10), weakest_eps, "problem in rowcol2apai")
        call assertEquals(dble(ai(7)), dble(2), weakest_eps, "problem in rowcol2apai")
        call assertEquals(dble(ai(10)), dble(3), weakest_eps, "problem in rowcol2apai")        
        rci(1:12) = (/1,2,3,4,5,6,7,8,9,10,11,12/)
        row = (/1,1,1,1,2,2,3,3,3,4,4,5/)
        col = (/1,2,3,5,1,2,1,3,4,3,4,5/)          
        val = (/4.0, -1.5, -2.5, 3.0, -1.5, 8.0, 1.1, 16.0, -1.0, -3.0, 11.0, 1.0/)
        call rowcol2apaiax(ap, ai, ax, row, col, val, rci, 12, 5)
        call assertEquals(dble(ax(3)), dble(1.1), weakest_eps, "problem in rowcol2apaiax")
        call assertEquals(dble(ax(5)), dble(8.0), weakest_eps, "problem in rowcol2apaiax")
        call assertEquals(dble(ax(8)), dble(-3.0), weakest_eps, "problem in rowcol2apaiax")
        rci(1:12) = (/1,2,3,4,5,6,7,8,9,10,11,12/)
        row = (/1,1,1,1,3,3,3,2,2,4,4,5/)
        col = (/3,5,1,2,4,1,3,2,1,4,3,5/)         
        val = (/-2.5, 3.0, 4.0, -1.5, -1.0, 1.1, 16.0, 8.0, -1.5, 11.0, -3.0, 1.0/)
        call rowcol2apaiax(ap, ai, ax, row, col, val, rci, 12, 5)
        call assertEquals(dble(ax(3)), dble(1.1), weakest_eps, "problem in rowcol2apaiax")
        call assertEquals(dble(ax(5)), dble(8.0), weakest_eps, "problem in rowcol2apaiax")
        call assertEquals(dble(ax(8)), dble(-3.0), weakest_eps, "problem in rowcol2apaiax")       
           
        return
    end subroutine
        
end module    
    
