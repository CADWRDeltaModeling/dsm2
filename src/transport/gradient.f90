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

    !> Calculate the undivided lo, hi, and centered differences
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
        real(gtm_real), intent(out):: grad_center(ncell,nvar) !< Dentered diff, LARGEREAL for undefined boundary cells
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
                                  bound_val,    &
                                  ncell,        &
                                  nvar,         &
                                  use_limiter)
        use gtm_precision
        use common_variables
        implicit none
        !--- args
        real(gtm_real), intent(out) :: grad(ncell,nvar)       !< Cell centered difference adjusted for boundaries and hydraulic devices
        real(gtm_real), intent(inout) :: grad_lo(ncell,nvar)     !< Difference on lo side, LARGEREAL in first index
        real(gtm_real), intent(inout) :: grad_hi(ncell,nvar)     !< Difference on hi side (n+1) minus (n) LARGEREAL for last index
        real(gtm_real), intent(inout) :: grad_center(ncell,nvar) !< Dentered diff, LARGEREAL for undefined boundary cells
        real(gtm_real), intent(in) :: vals(ncell,nvar)        !< Data to be differenced        
        real(gtm_real), intent(in) :: bound_val(n_boun,nvar)  !< Boundary conc values  
        real(gtm_real), intent(in) :: dx(ncell)               !< Cell length
        integer,intent(in)  :: ncell                          !< Number of cells
        integer,intent(in)  :: nvar                           !< Number of variables
        logical,intent(in),optional :: use_limiter            !< whether to use slope limiter
        !--- local variabls
        real(gtm_real) :: upval(nvar), downval(nvar)          ! sum of connected up/down-stream vals
        real(gtm_real) :: up_length, down_length              ! sum of connected up/down-stream length
        real(gtm_real) :: up_split_ratio, down_split_ratio    ! ratio to apply splitting to up/down-stream        
        integer :: n_up_cell, n_down_cell                     ! num of connected up/down-stream cells        
        integer :: icell, i, j       
        logical :: limit_slope                                ! whether slope limiter is used         
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
          
        !------ adjust boundaries ------
        if (n_boun > 0) then 
            do i = 1, n_boun
                icell = bound(i)%cell_no
                if (bound(i)%up_down .eq. 1) then  ! upstream boundary
                    grad(icell,:) = grad_hi(icell,:)
                else                               ! downstream boundary
                    grad(icell,:) = grad_lo(icell,:)
                end if                           
            end do                 
        end if
 
        !------ adjust junctions ------
        if (n_junc > 0) then
            ! assign gradient for cells around junction to be zero--> first order accuracy
            do i = 1, n_junc           
                do j = 1, junc(i)%n_conn_cells
                   icell = junc(i)%cell_no(j)
                   grad(icell,:) = zero
                end do
            end do          
            ! assign gradient for cells around junction equal to upstream adjent cell
            !do i = 1, n_junc           
            !    min_cell_no = 10000   ! a dummy number helps to detect minimum cell no
            !    do j = 1, junc(i)%n_conn_cells
            !       icell = junc(i)%cell_no(j)                
            !       if ((junc(i)%up_down(j)==0) .and. (icell<min_cell_no)) then
            !           min_cell_no = icell
            !       end if
            !    end do
            !    do j = 1, junc(i)%n_conn_cells
            !        icell = junc(i)%cell_no(j)
            !        grad(icell,:) = grad_hi(min_cell_no,:)
            !    end do
            !end do  
        end if
 
        return
    end subroutine


!==============================================================================
!======================todo:: BELOW CAN BE DELETED AFTER TESTING===============
!==============================================================================
    
    !> Calculate the undivided lo, hi, and centered differences
    subroutine difference_single_channel(grad_lo,grad_hi,grad_center,vals,ncell,nvar)

        use gtm_precision
        implicit none

        !---- args
        integer,intent(in)  :: ncell                          !< Number of cells
        integer,intent(in)  :: nvar                           !< Number of variables
        real(gtm_real),intent(in)  :: vals(ncell,nvar)        !< Data to be differenced
        real(gtm_real),intent(out) :: grad_lo(ncell,nvar)     !< Difference on lo side, LARGEREAL in first index
        real(gtm_real),intent(out) :: grad_hi(ncell,nvar)     !< Difference on hi side (n+1) minus (n) LARGEREAL for last index
        real(gtm_real),intent(out) :: grad_center(ncell,nvar) !< Dentered diff, LARGEREAL for undefined boundary cells
        
        !----local
        integer :: ivar

        do ivar = 1, nvar
            grad_center(2:(ncell-1),ivar) = (vals(3:ncell,ivar) - vals(1:(ncell-2),ivar))/two
            grad_center(1,ivar)=LARGEREAL
            grad_center(ncell,ivar)=LARGEREAL
            grad_hi(1:(ncell-1),ivar) = (vals(2:ncell,ivar) - vals(1:(ncell-1),ivar))
            grad_hi(ncell,ivar)=LARGEREAL
            grad_lo(2:ncell,ivar)=grad_hi(1:(ncell-1),ivar)
            grad_lo(1,ivar)=LARGEREAL
        end do

        return
    end subroutine


    !> Apply a flux limiter (van Leer) given one-sided and centered differences
    subroutine limiter_single_channel(grad_lim,grad_lo,grad_hi,grad_center,ncell,nvar)

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

        ! Boundary values are not defined
        do ivar = 1,nvar
            grad_lim(1,ivar)    = LARGEREAL   !todo: is this really what we want? 
            grad_lim(ncell,ivar)= LARGEREAL   !todo: is this really what we want? 
        end do
        
        return
    end subroutine

    !> Adjust differences to account for special cases (boundaries, structures, junctions, flow reversals)
    !> Currently implementation only accounts for two boundaries at ends of channel
    subroutine adjust_differences_single_channel(grad,     &
                                  grad_lim, &
                                  grad_lo,  &
                                  grad_hi,  &
                                  ncell,    &
                                  nvar)
        use gtm_precision
        implicit none
        !--- args
        integer,intent(in)  :: ncell                       !< Number of cells
        integer,intent(in)  :: nvar                        !< Number of variables

        real(gtm_real),intent(in)  :: grad_lo(ncell,nvar)  !< Difference based on lo side difference
        real(gtm_real),intent(in)  :: grad_hi(ncell,nvar)  !< Difference based on hi side difference
        real(gtm_real),intent(in)  :: grad_lim(ncell,nvar) !< Limited cell centered difference
        real(gtm_real),intent(out) :: grad(ncell,nvar)     !< Cell centered difference adjusted for boundaries and hydraulic devices
        !---------
        grad          = grad_lim
        grad(1,:)     = grad_hi(1,:)
        grad(ncell,:) = grad_lo(ncell,:)
        return
    end subroutine
    
end module


