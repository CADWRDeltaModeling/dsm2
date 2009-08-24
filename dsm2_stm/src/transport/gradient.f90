!> Module containing routines for calculating differences and limiters
!>@ingroup transport
module gradient
contains

!///////////////////////////////////////////////////////////////////////


!> Calculate the undivided lo, hi and centered differences
subroutine difference(grad_lo,grad_hi,grad_center,vals,ncell,nvar)
use stm_precision
implicit none

!---- args
integer,intent(in)  :: ncell  !< Number of cells
integer,intent(in)  :: nvar   !< Number of variables
real(STM_REAL),intent(in) :: vals(ncell,nvar)         !< data to be differenced
real(STM_REAL),intent(out) :: grad_lo(ncell,nvar)     !< gradient on lo side, LARGEREAL in first index
real(STM_REAL),intent(out) :: grad_hi(ncell,nvar)     !< hi side (n+1) minus (n) LARGEREAL for last index
real(STM_REAL),intent(out) :: grad_center(ncell,nvar) !< centered diff, LARGEREAL for undefined boundary cells

!----
integer :: ivar

!----------------------

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

!///////////////////////////////////////////////////////////////////////


!> Apply a flux limiter (van Leer) given one-sided and centered gradients
subroutine limiter(grad_lim,grad_lo,grad_hi,grad_center,ncell,nvar)

use stm_precision
implicit none

!--- args
integer,intent(in)  :: ncell  !< Number of cells
integer,intent(in)  :: nvar   !< Number of variables
real(STM_REAL),intent(in) :: grad_lo(ncell,nvar) !< gradient on lo side, LARGEREAL in first index
real(STM_REAL),intent(in) :: grad_hi(ncell,nvar) !< hi side (n+1) minus (n) LARGEREAL for last index
real(STM_REAL),intent(in) :: grad_center(ncell,nvar) !< centered diff, LARGEREAL for undefined boundary cells 
real(STM_REAL),intent(out) :: grad_lim(ncell,nvar) !< limited gradient

!----

real(STM_REAL) :: delta_limit(ncell,nvar) ! intermediate quantity
real(STM_REAL) :: sign
integer        :: ivar, icell             ! counting variables

!----------------------

do ivar = 1,nvar
    ! todo: Kaveh -- make sure you understand fortran ordering/strategy
    do icell = 1,ncell
        sign = one
        delta_limit(icell,ivar) = two*min(abs(grad_lo(icell,ivar)), &
                                          abs(grad_hi(icell,ivar))  &
                                          )
        sign = one
        if (grad_center(icell,ivar) < zero) sign = minus
        grad_lim(icell,ivar) = min(abs(grad_center(icell,ivar)), &
                                 abs(delta_limit(icell,ivar)))*sign
    end do
end do
where (grad_lo*grad_hi < zero)
    grad_lim = zero
end where

do ivar = 1,nvar
  grad_lim(1,ivar)    = LARGEREAL   !todo: is this really what we want? 
  grad_lim(ncell,ivar)= LARGEREAL   !todo: is this really what we want? 
end do

return
end subroutine


end module

