
!> Testing of the extrapolate step.
!>@ingroup test
module test_extrapolate
use fruit

contains
!///////////////////////////////////////

!> Test the extrapolation part of the predictor step of the advection algorithm
!> There are three cells and two constituents to test symmetry in space across constituents
subroutine test_extrapolation
use stm_precision
use gradient
use advection
implicit none
  integer,parameter :: nx = 3       !interior and two ends
  integer,parameter :: nconst = 2
 
  real(STM_REAL) :: grad(nx,nconst)
  real(STM_REAL) :: conc(nx,nconst)
  real(STM_REAL) :: conc_hi(nx,nconst)
  real(STM_REAL) :: conc_lo(nx,nconst)
  real(STM_REAL) :: source(nx,nconst)

  real(STM_REAL) :: flow(nx)
  real(STM_REAL) :: area(nx)  
  real(STM_REAL) :: dx
  real(STM_REAL) :: dt
  real(STM_REAL) :: time
  integer        :: ix,ic
  
  conc(1,1)=62.D0
  conc(2,1)=64.D0
  conc(3,1)=65.D0
  conc(1,2)=65.D0
  conc(2,2)=64.D0
  conc(3,2)=62.D0

  grad(1,1)=2.D0
  grad(2,1)=1.5D0
  grad(3,1)=1.D0
  grad(1,2)=-1.D0
  grad(2,2)=-1.5D0
  grad(3,2)=-2.D0
  
  source = 0.D0
  flow(1) = 8.D0
  flow(2) = zero
  flow(3) = -8.D0
  area = 16.D0
  dx = 32.D0
  dt = 48.D0
  time = zero

  call extrapolate(conc_lo,  &
                   conc_hi,  &
                   conc,     &
                   grad,     &
                   source,   &
                   flow,     &  
                   area,     &
                   nx,       &
                   nconst,   &
                   time,     &
                   dt,       &
                   dx)

  call assertEquals(conc_lo(1,1),60.25D0,"extrapolate, lo(1,1)")
  call assertEquals(conc_lo(2,1),63.25D0,"extrapolate, lo(2,1)")
  call assertEquals(conc_lo(3,1),64.875D0,"extrapolate, lo(3,1)")
  call assertEquals(conc_lo(1,1),conc_hi(3,2),"extrapolate, lo-hi(1,1)")
  call assertEquals(conc_lo(2,1),conc_hi(2,2),"extrapolate, lo-hi(2,1)")
  call assertEquals(conc_lo(3,1),conc_hi(1,2),"extrapolate, lo-hi(3,1)")

  call assertEquals(conc_hi(1,1),62.25D0,"extrapolate, hi(1,1)")
  call assertEquals(conc_hi(2,1),64.75D0,"extrapolate, hi(2,1)")
  call assertEquals(conc_hi(3,1),65.875D0,"extrapolate, hi(3,1)")
  call assertEquals(conc_hi(1,1),conc_lo(3,2),"extrapolate, hi-lo(3,2)")
  call assertEquals(conc_hi(2,1),conc_lo(2,2),"extrapolate, hi-lo(2,2)")
  call assertEquals(conc_hi(3,1),conc_lo(1,2),"extrapolate, hi-lo(1,2)")

!print*,":::"
!do ic = 1,2
!do ix = 1,3
!print*,ix,ic,conc_lo(ix,ic),conc_hi(ix,ic)
!end do
!end do
return
end subroutine

end module



