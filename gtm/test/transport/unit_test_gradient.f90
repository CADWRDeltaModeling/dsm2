!<!license>
!    Copyright (C) 2017 State of California,
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

!> Test module for gradients and limiters
!>@ingroup test_transport
module test_gradient

use fruit
use gtm_precision
use state_variables 

contains

!> Test the gradient algorithm
!> Tesitng routine for undivided differences
subroutine test_gradient_calc
  use gradient
  implicit none
  integer,parameter :: nx = 8            !< Number of cells
  integer,parameter :: nconst = 2        !< Number of constituents 
  real(gtm_real) :: grad_lo(nx,nconst)   !< Gradient on the left side of the cell 
  real(gtm_real) :: grad_hi(nx,nconst)   !< Gradient on the right side of the cell   
  real(gtm_real) :: grad_c(nx,nconst)    !< Centeral value gradient of the cell 
  integer        :: ix                   !< Counter on the
  real(gtm_real) :: dx(nx) = one
  
  call allocate_state(nx,nconst)
  conc(1,1)=0.d0
  conc(2,1)=2.d0
  conc(3,1)=3.d0  !sign reversal
  conc(4,1)=2.d0
  conc(5,1)=1.d0
  conc(6,1)=-12.d0  !abrupt drop
  conc(7,1)=-13.d0
  conc(8,1)=-13.5d0
  conc(1,2)=2.d0
  conc(2,2)=2.d0
  conc(3,2)=1.d0  !sign reversal
  conc(4,2)=2.d0
  conc(5,2)=3.d0
  conc(6,2)=12.d0  !abrupt rise
  conc(7,2)=13.0
  conc(8,2)=13.5d0
  
  call difference(grad_lo,grad_hi,grad_c,conc,dx,nx,nconst)
  call assertEquals(grad_lo(1,1),LARGEREAL,"Gradient (1,1)")
  call assertEquals(grad_lo(2,1),two,"Gradient (2,1)")
  call assertEquals(grad_lo(8,1),minus*half,"3: ")
  call assertEquals(grad_lo(4,1),minus*one,"4: ")
  call assertEquals(grad_lo(1,2),LARGEREAL,"5: ")
  call assertEquals(grad_lo(2,2),zero,"6: ")
  
  !todo: need names on the following tests
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
  integer,parameter :: nx = 8            !< Number of cells
  integer,parameter :: nconst = 2        !< Number of constituents 
  real(gtm_real) :: grad_lo(nx,nconst)   !< Gradient on the left side of the cell 
  real(gtm_real) :: grad_hi(nx,nconst)   !< Gradient on the right side of the cell
  real(gtm_real) :: grad_c(nx,nconst)    !< Centeral value gradient of the cell 
  real(gtm_real) :: grad_lim(nx,nconst)  !< Counter on the
  integer        :: ic
  real(gtm_real) :: dx(nx) = one
  
  call allocate_state(nx,nconst)
  conc(1,1)=0.d0
  conc(2,1)=2.d0
  conc(3,1)=3.d0  !sign reversal
  conc(4,1)=2.d0
  conc(5,1)=1.d0
  conc(6,1)=-12.d0  !abrupt drop
  conc(7,1)=-13.d0
  conc(8,1)=-13.5d0
  conc(1,2)=2.d0
  conc(2,2)=2.d0
  conc(3,2)=1.d0  !sign reversal
  conc(4,2)=2.5d0
  conc(5,2)=3.d0
  conc(6,2)=12.d0  !abrupt rise
  conc(7,2)=13.d0
  conc(8,2)=13.5d0
  
  call difference(grad_lo,grad_hi,grad_c,conc,dx,nx,nconst)
  call limiter(grad_lim,grad_lo,grad_hi,grad_c,nx,nconst)
   
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