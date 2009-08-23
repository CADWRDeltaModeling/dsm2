!> Test module for gradients and limiters
!>@ingroup test
module test_gradient

use fruit
use stm_precision
use state_variables 

contains

!> Tesitng routine for undivided differences
subroutine test_gradient_calc
  use gradient
  implicit none
  integer,parameter :: nx = 8
  integer,parameter :: nconst = 2
  real(STM_REAL) :: grad_lo(nx,nconst)
  real(STM_REAL) :: grad_hi(nx,nconst)
  real(STM_REAL) :: grad_c(nx,nconst)
  integer        :: ix,ic
  
  call allocate_state(nx,nconst)
  conc(1,1)=0.D0
  conc(2,1)=2.D0
  conc(3,1)=3.D0  !sign reversal
  conc(4,1)=2.D0
  conc(5,1)=1.D0
  conc(6,1)=-12.D0  !abrupt drop
  conc(7,1)=-13.D0
  conc(8,1)=-13.5D0
  conc(1,2)=2.D0
  conc(2,2)=2.D0
  conc(3,2)=1.D0  !sign reversal
  conc(4,2)=2.D0
  conc(5,2)=3.D0
  conc(6,2)=12.D0  !abrupt rise
  conc(7,2)=13.0
  conc(8,2)=13.5D0
  
  call difference(grad_lo,grad_hi,grad_c,conc,nx,nconst)
  call assertEquals(grad_lo(1,1),LARGEREAL,"Gradient (1,1)")
  call assertEquals(grad_lo(2,1),two,"Gradient (2,1)")
  call assertEquals(grad_lo(8,1),minus*half,"3: ")
  call assertEquals(grad_lo(4,1),minus*one,"4: ")
  call assertEquals(grad_lo(1,2),LARGEREAL,"5: ")
  call assertEquals(grad_lo(2,2),zero,"6: ")
  
  call assertEquals(grad_hi(1,2),zero)
  call assertEquals(grad_hi(7,2),half)
  call assertEquals(grad_hi(8,2),LARGEREAL)
  call assertEquals(grad_c(1,1),LARGEREAL)
  call assertEquals(grad_c(8,1),LARGEREAL)
  call assertEquals(grad_c(2,1),1.5D0)
  call assertEquals(grad_c(7,1),-7.5D-1)  
  call assertEquals(grad_c(1,2),LARGEREAL)
  call assertEquals(grad_c(8,2),LARGEREAL)
  call assertEquals(grad_c(2,2),minus*half)
  call assertEquals(grad_c(7,2),7.5D-1)
  call assertEquals(grad_c(8,2),LARGEREAL)
  call deallocate_state
  return
end subroutine

!> Tesitng routine for van Leer limiter
subroutine test_limiter
  use gradient
  implicit none
  integer,parameter :: nx = 8
  integer,parameter :: nconst = 2
  real(STM_REAL) :: grad_lo(nx,nconst)
  real(STM_REAL) :: grad_hi(nx,nconst)
  real(STM_REAL) :: grad_c(nx,nconst)
  real(STM_REAL) :: grad_lim(nx,nconst)
  integer        :: ix,ic
  
  call allocate_state(nx,nconst)
  conc(1,1)=0.D0
  conc(2,1)=2.D0
  conc(3,1)=3.D0  !sign reversal
  conc(4,1)=2.D0
  conc(5,1)=1.D0
  conc(6,1)=-12.D0  !abrupt drop
  conc(7,1)=-13.D0
  conc(8,1)=-13.5D0
  conc(1,2)=2.D0
  conc(2,2)=2.D0
  conc(3,2)=1.D0  !sign reversal
  conc(4,2)=2.5D0
  conc(5,2)=3.D0
  conc(6,2)=12.D0  !abrupt rise
  conc(7,2)=13.0
  conc(8,2)=13.5D0
  
  call difference(grad_lo,grad_hi,grad_c,conc,nx,nconst)
  call limiter(grad_lim,grad_lo,grad_hi,grad_c,nx,nconst)

  ! Special cases due to end of array
  call assertEquals(grad_lim(1,1),LARGEREAL,"Variable 1 lo end")
  call assertEquals(grad_lim(1,2),LARGEREAL,"Variable 2 lo end")
  call assertEquals(grad_lim(8,1),LARGEREAL,"Variable 1 hi end")
  call assertEquals(grad_lim(8,2),LARGEREAL,"Variable 2 hi end")
   
  ! Typical case where centered diff is used
  call assertEquals(grad_lim(4,2),grad_c(4,2),"typical centered diff")

  ! Special cases where limiter is in effect
  call assertEquals(grad_lim(3,1),zero,"sign reversal at peak")
  call assertEquals(grad_lim(5,1),minus*two,"big drop hi side")
  call assertEquals(grad_lim(6,1),minus*two,"big drop lo side")
  call assertEquals(grad_lim(3,2),zero,"sign reversal valley")
  call assertEquals(grad_lim(5,2),one,"big rise lo side")
  call assertEquals(grad_lim(6,2),two,"big rise hi side")
  
  call deallocate_state
  return
end subroutine

end module