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
        use constants
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
        use constants
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

end module
