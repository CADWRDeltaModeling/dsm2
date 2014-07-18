!<license>
!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
!    Department of Water Resources.
!    This file is part of DSM2.
!
!    The Delta Simulation Model 2 (DSM2) is free software: 
!    you can redistribute it and/or modify
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

!> Module containing routines for calculating differences and limiters
!>@ingroup transport
module gradient
    contains

    !> Calculate the divided lo, hi, and centered differences
    subroutine difference(grad_lo,     & 
                          grad_hi,     &
                          grad_center, &
                          vals,        &
                          dx,          &
                          ncell,       &
                          nvar)
        use gtm_precision
        implicit none

        !---- args
        integer, intent(in) :: ncell                          !< Number of cells
        integer, intent(in) :: nvar                           !< Number of variables
        real(gtm_real), intent(in) :: vals(ncell,nvar)        !< Data to be differenced
        real(gtm_real), intent(in) :: dx(ncell)               !< Cell length
        real(gtm_real), intent(out):: grad_lo(ncell,nvar)     !< Difference on lo side, LARGEREAL in first index
        real(gtm_real), intent(out):: grad_hi(ncell,nvar)     !< Difference on hi side (n+1) minus (n) LARGEREAL for last index
        real(gtm_real), intent(out):: grad_center(ncell,nvar) !< Centered diff, LARGEREAL for undefined boundary cells
        !----local
        integer :: ivar

        do ivar = 1, nvar
            grad_center(2:(ncell-1),ivar) = (vals(3:ncell,ivar) - vals(1:(ncell-2),ivar))/    &
                                          (half*dx(3:ncell) + dx(2:ncell-1) + half*dx(1:ncell-2))
            grad_center(1,ivar)=LARGEREAL
            grad_center(ncell,ivar)=LARGEREAL            
            grad_hi(1:(ncell-1),ivar) = (vals(2:ncell,ivar) - vals(1:(ncell-1),ivar))/        &
                                        (half*dx(2:ncell) + half*dx(1:ncell-1))
            grad_hi(ncell,ivar)=LARGEREAL
            grad_lo(2:ncell,ivar)=grad_hi(1:(ncell-1),ivar)
            grad_lo(1,ivar)=LARGEREAL              
        end do

        return
    end subroutine

    !> Apply a flux limiter (van Leer) given one-sided and centered differences
    subroutine limiter(grad_lim,    &
                       grad_lo,     &
                       grad_hi,     &
                       grad_center, &
                       ncell,       &
                       nvar)
        
        use gtm_precision
        implicit none

        !--- args
        integer,intent(in)  :: ncell                         !< Number of cells
        integer,intent(in)  :: nvar                          !< Number of variables
        real(gtm_real),intent(in) :: grad_lo(ncell,nvar)     !< Difference on lo side, LARGEREAL in first index
        real(gtm_real),intent(in) :: grad_hi(ncell,nvar)     !< Difference on hi side (n+1) minus (n) LARGEREAL for last index
        real(gtm_real),intent(in) :: grad_center(ncell,nvar) !< Centered difference, LARGEREAL for undefined boundary cells 
        real(gtm_real),intent(out) :: grad_lim(ncell,nvar)   !< Limited difference
        
        !---locals
        real(gtm_real) :: delta_limit(ncell,nvar) ! Intermediate quantity
        real(gtm_real) :: sign                           
        integer        :: ivar, icell             ! Counting variables

        do ivar = 1,nvar
            do icell = 1,ncell
                delta_limit(icell,ivar) = two*min(abs(grad_lo(icell,ivar)), &
                                                  abs(grad_hi(icell,ivar)) )                              
                if (grad_center(icell,ivar) < zero)then
                    sign = minus
                else
                    sign = one
                end if
                grad_lim(icell,ivar) = min(abs(grad_center(icell,ivar)), &
                                           abs(delta_limit(icell,ivar)))*sign
            end do
        end do
        where (grad_lo*grad_hi < zero)
            grad_lim = zero
        end where

        return
    end subroutine


    !> Adjust differences to account for special cases (boundaries, structures, junctions, flow reversals)
    subroutine adjust_differences(grad,         &
                                  grad_lo,      &  
                                  grad_hi,      &
                                  grad_center,  &
                                  vals,         &
                                  dx,           &
                                  ncell,        &
                                  nvar,         &
                                  use_limiter)
        use gtm_precision
        use common_variables, only : n_node, dsm2_node
        implicit none
        !--- args
        real(gtm_real), intent(out) :: grad(ncell,nvar)          !< Cell centered difference adjusted for boundaries and hydraulic devices
        real(gtm_real), intent(inout) :: grad_lo(ncell,nvar)     !< Difference on lo side, LARGEREAL in first index
        real(gtm_real), intent(inout) :: grad_hi(ncell,nvar)     !< Difference on hi side (n+1) minus (n) LARGEREAL for last index
        real(gtm_real), intent(inout) :: grad_center(ncell,nvar) !< Dentered diff, LARGEREAL for undefined boundary cells
        real(gtm_real), intent(in) :: vals(ncell,nvar)           !< Data to be differenced          
        real(gtm_real), intent(in) :: dx(ncell)                  !< Cell length
        integer,intent(in)  :: ncell                             !< Number of cells
        integer,intent(in)  :: nvar                              !< Number of variables
        logical,intent(in), optional :: use_limiter              !< whether to use slope limiter
        !--- local variabls
        real(gtm_real) :: upval(nvar), downval(nvar)             ! sum of connected up/down-stream vals
        real(gtm_real) :: up_length, down_length                 ! sum of connected up/down-stream length
        real(gtm_real) :: up_split_ratio, down_split_ratio       ! ratio to apply splitting to up/down-stream        
        integer :: n_up_cell, n_down_cell                        ! num of connected up/down-stream cells        
        integer :: icell, i, j                                   ! local variables
        logical :: limit_slope                                   ! whether slope limiter is used         
        integer :: min_cell_no
        
        if (present(use_limiter))then
            limit_slope = use_limiter
        else
            limit_slope = .true.
        end if        
         
        if (limit_slope)then    ! Applies flux-limeter on high resolution gradient 
            call limiter(grad, grad_lo, grad_hi ,grad_center, ncell, nvar)
        else    
            grad = grad_center
        end if    
        grad(1,:)     = grad_hi(1,:)             ! in case cell_no=1 does not locate at actual boundary, w/t this line will cause error. 
        grad(ncell,:) = grad_lo(ncell,:)         ! in case cell_no=ncell does not locate at actual boundary, w/t this line will cause error.          
 
        do i = 1, n_node
            ! adjust boundaries
            if (dsm2_node(i)%boundary_no .ne. 0) then
                icell = dsm2_node(i)%cell_no(1)
                if (dsm2_node(i)%up_down(1) .eq. 1) then  ! upstream boundary
                    grad(icell,:) = grad_hi(icell,:)
                else                               ! downstream boundary
                    grad(icell,:) = grad_lo(icell,:)
                end if              
            elseif (dsm2_node(i)%junction_no .ne. 0) then
                ! assign gradient for cells around junction to be zero--> first order accuracy
                ! but this may run into issue of smoothing two close signals (delta uniform flow case)
                do j = 1, dsm2_node(i)%n_conn_cell
                   icell = dsm2_node(i)%cell_no(j)
                   grad(icell,:) = zero
                end do
            elseif (dsm2_node(i)%nonsequential==1) then
                ! assign gradient for nonsequential adjucent cells to be zero--> first order accuracy
                do j = 1, 2
                    icell = dsm2_node(i)%cell_no(j)
                    if (dsm2_node(i)%up_down(j)==0) then   !cell at upstream of link
                        grad(icell,:) = grad_lo(icell,:)
                    else
                        grad(icell,:) = grad_hi(icell,:)
                    end if    
                end do                        
            end if                  
        end do         
 
        return
    end subroutine
    
end module


