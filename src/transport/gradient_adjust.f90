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
 
!> adjust gradient differences for network interface to be fulfilled by driver or application
!>@ingroup transport
module gradient_adjust
  !> Adjust gradient difference
  interface
    !> Generic interface for adjusting gradient differences routine that should be fulfilled by
    !> client programs
    subroutine adjust_gradient_if(grad,          &
                                  grad_lo,       &  
                                  grad_hi,       &
                                  grad_center,   &
                                  vals,          &
                                  dx,            &
                                  ncell,         & 
                                  nvar,          &
                                  use_limiter)
        use gtm_precision
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

    end subroutine adjust_gradient_if
  end interface

  !> This pointer should be set by the driver or client code to specify the 
  !> treatment of gradient for the network
  procedure(adjust_gradient_if), pointer :: adjust_gradient => null()

    contains
  
    !> Adjust differences to account for special cases (boundaries, structures, junctions, flow reversals)
    subroutine adjust_differences_single_channel(grad,         &
                                                 grad_lo,      &  
                                                 grad_hi,      &
                                                 grad_center,  &
                                                 vals,         &
                                                 dx,           &
                                                 ncell,        &
                                                 nvar,         &
                                                 use_limiter)
        use gtm_precision
        use gradient, only : limiter
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
        logical :: limit_slope                                   ! whether slope limiter is used         
        
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
 
        return
    end subroutine


    !> Adjust differences to account for special cases (boundaries, structures, junctions, flow reversals)
    !> This routine needs to use back door information: dsm2_network
    subroutine adjust_differences_network(grad,         &
                                          grad_lo,      &  
                                          grad_hi,      &
                                          grad_center,  &
                                          vals,         &
                                          dx,           &
                                          ncell,        &
                                          nvar,         &
                                          use_limiter)
        use gtm_precision
        use gradient, only : limiter
        use common_variables, only : n_node, dsm2_network
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
            if (dsm2_network(i)%boundary_no .ne. 0) then
                icell = dsm2_network(i)%cell_no(1)
                if (dsm2_network(i)%up_down(1) .eq. 1) then  ! upstream boundary
                    grad(icell,:) = grad_hi(icell,:)
                else                               ! downstream boundary
                    grad(icell,:) = grad_lo(icell,:)
                end if              
            elseif (dsm2_network(i)%junction_no .ne. 0) then
                ! assign gradient for cells around junction to be zero--> first order accuracy
                ! but this may run into issue of smoothing two close signals (delta uniform flow case)
                do j = 1, dsm2_network(i)%n_conn_cell
                   icell = dsm2_network(i)%cell_no(j)
                   grad(icell,:) = zero
                end do
            elseif (dsm2_network(i)%nonsequential==1) then
                ! assign gradient for nonsequential adjucent cells to be zero--> first order accuracy
                do j = 1, 2
                    icell = dsm2_network(i)%cell_no(j)
                    if (dsm2_network(i)%up_down(j)==0) then   !cell at upstream of link
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
