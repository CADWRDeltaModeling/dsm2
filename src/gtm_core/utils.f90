

! Recursive Fortran 95 quicksort routine
! sorts real numbers into ascending numerical order
! Author: Juli Rew, SCD Consulting (juliana@ucar.edu), 9/03
! modified by ehsu for integer array sorting and associated index
module utils

    implicit none
    public :: QsortC
    private :: Partition

    contains

    recursive subroutine QsortC(A)
        integer, intent(inout), dimension(:) :: A
        integer :: iq

        if(size(A) > 1) then
            call Partition(A, iq)
            call QsortC(A(:iq-1))
            call QsortC(A(iq:))
        endif
    end subroutine QsortC

    subroutine Partition(A, marker)
        integer, intent(in out), dimension(:) :: A
        integer, intent(out) :: marker
        integer :: i, j
        integer :: temp
        integer :: x      ! pivot point
        x = A(1)
        i = 0
        j= size(A) + 1
        do
            j = j-1
            do
                if (A(j) <= x) exit
                j = j-1
            end do
            i = i+1
            do
                if (A(i) >= x) exit
                i = i+1
            end do
            if (i < j) then
                ! exchange A(i) and A(j)
                temp = A(i)
                A(i) = A(j)
                A(j) = temp
            elseif (i == j) then
                marker = i+1
                return
            else
                marker = i
                return
            endif
        end do
    end subroutine Partition
    
    !> Sort array A and store its original index in B
    recursive subroutine QsortCI(A, B)
        integer, intent(inout), dimension(:) :: A
        integer, intent(inout), dimension(:) :: B
        integer :: iq

        if(size(A) > 1) then
            call PartitionI(A, B, iq)
            call QsortCI(A(:iq-1), B(:iq-1))
            call QsortCI(A(iq:), B(iq:))
        endif
    end subroutine QsortCI

    subroutine PartitionI(A, B, marker)
        integer, intent(inout), dimension(:) :: A
        integer, intent(inout), dimension(:) :: B
        integer, intent(out) :: marker
        integer :: i, j
        integer :: temp
        integer :: x      ! pivot point
        x = A(1)
        i = 0
        j= size(A) + 1
        do
            j = j-1
            do
                if (A(j) <= x) exit
                j = j-1
            end do
            i = i+1
            do
                if (A(i) >= x) exit
                i = i+1
            end do
            if (i < j) then
                ! exchange A(i) and A(j)
                temp = A(i)
                A(i) = A(j)
                A(j) = temp
                temp = B(i)
                B(i) = B(j)
                B(j) = temp
            elseif (i == j) then
                marker = i+1
                return
            else
                marker = i
                return
            endif
        end do
    end subroutine PartitionI

    !> Row Column to Ap, Ai representation
    subroutine rowcol2apai(ap,         & 
                           ai,         &
                           row,        & 
                           col,        &
                           rcindex,    &
                           n_nonzero,  &
                           n_matrix)
        use gtm_precision
        implicit none
        integer, intent(in) :: n_nonzero
        integer, intent(in) :: n_matrix
        integer, intent(in) :: row(n_nonzero)
        integer, intent(in) :: col(n_nonzero)
        integer, intent(in) :: rcindex(n_nonzero)
        integer, intent(out) :: ai(n_nonzero)
        integer, intent(out) :: ap(n_matrix+1)
        integer :: ro(n_nonzero), rci(n_nonzero)
        integer :: i, j, k 
        
        ro = row
        rci = rcindex
        call QsortCI(ro, rci)
        ai = LARGEINT
        ap(1) = 0
        k = 0
        do i = 1, n_matrix
            ap(i+1) = ap(i)
            do j = 1, n_nonzero
                if (col(j).eq.i) then
                    k = k + 1
                    ap(i+1) = ap(i+1) + 1
                    ai(k) = row(rci(j))-1                
                end if
            end do
        end do                
        return
    end subroutine    

    !> Row Column to Ap, Ai and Ax representation
    subroutine rowcol2apaiax(ap,         & 
                             ai,         &
                             ax,         &
                             row,        & 
                             col,        &
                             val,        &
                             rcindex,    &
                             n_nonzero,  &
                             n_matrix)
        use gtm_precision
        implicit none
        integer, intent(in) :: n_nonzero
        integer, intent(in) :: n_matrix
        integer, intent(in) :: row(n_nonzero)
        integer, intent(in) :: col(n_nonzero)        
        integer, intent(in) :: rcindex(n_nonzero)
        real(gtm_real), intent(in) :: val(n_nonzero)
        real(gtm_real), intent(out) :: ax(n_nonzero)
        integer, intent(out) :: ai(n_nonzero)
        integer, intent(out) :: ap(n_matrix+1)
        integer :: ro(n_nonzero), rci(n_nonzero)
        integer :: i, j, k 
        
        ro = row
        rci = rcindex
        call QsortCI(ro, rci)
        ai = LARGEINT
        ax = LARGEREAL
        ap(1) = 0
        k = 0
        do i = 1, n_matrix
            ap(i+1) = ap(i)
            do j = 1, n_nonzero
                if (col(rci(j)).eq.i) then
                    k = k + 1
                    ap(i+1) = ap(i+1) + 1
                    ai(k) = row(rci(j))-1
                    ax(k) = val(rci(j))
                end if
            end do
        end do                
        return
    end subroutine    

    
end module