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

!> Tests for matrix solver
!>@ingroup test_transport
module test_matrix_solver

use matrix_solver
use fruit
use gtm_precision

contains
!> Tests the tri_diagonal matrix solver subroutine versus pre-calculated values
subroutine test_tridi_solver


implicit none
  
  integer,parameter :: ncell = 11                 !< Number of volumes 
  integer,parameter :: nvar = 2                   !< Number of constituent 
      
    real(gtm_real)  :: down_diag(ncell,nvar)       !< Values of the coefficients below diagonal in matrix
    real(gtm_real)  :: center_diag(ncell,nvar)     !< Values of the coefficients at the diagonal in matrix
    real(gtm_real)  :: up_diag(ncell,nvar)         !< Values of the coefficients above the diagonal in matrix
    real(gtm_real)  :: right_hand_side(ncell,nvar) !< Values of the right hand side vector
    real(gtm_real)  :: conc(ncell,nvar)            !< Values of the computed solution
    
!---- local
integer :: ivar    

   
 !--- Small numbers on center diag large numbers on up and down diag
 ! todo: remove the doxygen comment
 !> The length of center_diagonal array is N, but up and down diagonal arrays are N-1, but the solver 
 !> works with the three inputs with the same length (all N) so the first elemenmt of down_diag and the last element of 
 !> up_diag were substituted by LARGEREAL = 12345689   
 center_diag (:,2) = [0.17D0,0.18D0,0.19D0,0.2D0,0.21D0,0.22D0,0.23D0,0.24D0,0.25D0,0.26D0,0.27D0]
 down_diag(:,2)  = [LARGEREAL,26.D0,31.D0,36.D0,41.D0,46.D0,51.D0,56.D0,61.D0,66.D0,71.D0]
 up_diag (:,2) = [36.D0,37.D0,38.D0,39.D0,40.D0,41.D0,42.D0,43.D0,44.D0,45.D0,LARGEREAL]
 right_hand_side(:,2)  = [0.01D0,0.02D0,0.03D0,0.04D0,500.0D0,400.0D0,0.07D0,0.08D0,0.09D0,0.1D0,0.11D0]
 ! todo: remove the doxygen comment
 !> Two constituents were added to check the solver 
do ivar = 1,nvar
  call tridi_solver (center_diag(:,ivar),up_diag(:,ivar),down_diag(:,ivar),right_hand_side(:,ivar),conc(:,ivar),ncell)
end do

  call assertEquals (conc(1,2),613.689083199382D0,weak_eps,"problem in solving cell num.1")
  call assertEquals (conc(4,2),4.520833065491D0,weak_eps,"problem in solving cell num.4")
  call assertEquals (conc(11,2),-834.471026100105D0,weak_eps,"problem in solving cell num.11")
  
!--- Large numbers on center diag small numbers on up and down diag

    center_diag(:,2) = [17D0,18D0,19D0,20D0,21D0,22D0,23D0,24D0,25D0,26D0,27D0]
    down_diag(:,2) = [LARGEREAL,0.05d0,0.1d0,0.15d0,0.2d0,0.25d0,0.3d0,0.35d0,0.4d0,0.45d0,0.5d0]
    up_diag(:,2) = [0.1d0,0.2d0,0.3d0,0.4d0,0.5d0,0.6d0,0.7d0,0.8d0,0.9d0,1D0,LARGEREAL]
    right_hand_side(:,2) = [0.01D0,0.02D0,0.03D0,0.04D0,0.05d0,0.06D0,0.07D0,0.08D0,0.09D0,0.1D0,0.11D0]

call tridi_solver (center_diag(:,2),up_diag(:,2),down_diag(:,2),right_hand_side(:,2),conc(:,2),ncell)


  call assertEquals (conc(1,2),0.000581809672D0,weak_eps,"problem in solving cell num.1")
  call assertEquals (conc(4,2),0.001942430407D0,weak_eps,"problem in solving cell num.4")
  call assertEquals (conc(11,2),0.004006798484D0,weak_eps,"problem in solving cell num.11")

   

return
end subroutine test_tridi_solver

end module