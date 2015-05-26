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

!> Test for interior lines of diffusion coefficient matrix
!>@ingroup test_transport
module test_interior_coef_matrix
contains
!> Tests the interior lines of coefficient matrix K (KU=F) 
subroutine test_interior_coef_matrix_sub

use diffusion
use fruit
use gtm_precision

  implicit none
  
integer, parameter:: ncell = 21                                !< Number of cells
integer, parameter:: nvar = 1                                  !< Number of variables
! todo: change it to 2

real(gtm_real)  :: down_diag(ncell,nvar)                       !< Values of the coefficients below diagonal in matrix
real(gtm_real)  :: center_diag(ncell,nvar)                     !< Values of the coefficients at the diagonal in matrix
real(gtm_real)  :: up_diag(ncell,nvar)                         !< Values of the coefficients above the diagonal in matrix
real(gtm_real)  :: area (ncell)                                !< Cell centered area at new time 
real(gtm_real)  :: area_lo(ncell)                              !< Low side area at new time
real(gtm_real)  :: area_hi(ncell)                              !< High side area at new time 
real(gtm_real)  :: disp_coef_lo (ncell,nvar)                   !< Low side constituent dispersion coef. at new time
real(gtm_real)  :: disp_coef_hi (ncell,nvar)                   !< High side constituent dispersion coef. at new time
real(gtm_real)  :: time                                        !< Current time
real(gtm_real)  :: theta_gtm                                   !< Explicitness coefficient; 0 is explicit, 0.5 Crank-Nicolson, 1 full implicit  
real(gtm_real)  :: dx(ncell)                                   !< Spatial step  
real(gtm_real)  :: dt                                          !< Time step                                   
               
area (:) = 1d0
area_hi(:) = 1d0
area_lo(:) = 1d0
disp_coef_lo =0.05d0
disp_coef_hi =0.05d0
theta_gtm = 0.9d0
dx = 0.045d0
time =LARGEREAL 
dt = 0.001d0

 !---check for theta = 0.9
 
call construct_diffusion_matrix(center_diag,      &
                                up_diag,          &     
                                down_diag,        &
                                area,             &
                                area_lo,          &
                                area_hi,          &
                                disp_coef_lo,     &
                                disp_coef_hi,     &
                                theta_gtm,        &
                                ncell,            &
                                time,             & 
                                nvar,             & 
                                dx,               &
                                dt)
                                            
      !--- centeral                                       
    call assertEquals (center_diag(2,1),1.044444444d0,weak_eps,"BigK(2,2) theta=0.9 Error!")
    call assertEquals (center_diag(7,1),1.044444444d0,weak_eps,"BigK(7,7) theta=0.9 Error!")
    call assertEquals (center_diag(18,1),1.044444444d0,weak_eps,"BigK(18,18) theta=0.9 Error!")
      !----up
    call assertEquals (up_diag(2,1),-0.022222222d0,weak_eps,"BigK(2,3) theta=0.9 Error!")
    call assertEquals (up_diag(7,1),-0.022222222d0,weak_eps,"BigK(7,8) theta=0.9 Error!")
    call assertEquals (up_diag(18,1),-0.022222222d0,weak_eps,"BigK(18,19) theta=0.9 Error!")  
      !----down
    call assertEquals (down_diag(2,1),-0.022222222d0,weak_eps,"BigK(2,1) theta=0.9 Error!")
    call assertEquals (down_diag(7,1),-0.022222222d0,weak_eps,"BigK(7,6) theta=0.9 Error!")
    call assertEquals (down_diag(18,1),-0.022222222d0,weak_eps,"BigK(18,17) theta=0.9 Error!") 
    
  
  !---check for theta = 0.5
    
     theta_gtm = 0.5d0

call construct_diffusion_matrix(center_diag ,     &
                                up_diag,          &     
                                down_diag,        &
                                area,             &
                                area_lo,          &
                                area_hi,          &
                                disp_coef_lo,     &
                                disp_coef_hi,     &
                                theta_gtm,        &
                                ncell,            &
                                time,             & 
                                nvar,             & 
                                dx,               &
                                dt)
                                            
      !--- centeral                                       
    call assertEquals (center_diag(2,1),1.02469135802469d0,weak_eps,"BigK(2,2) theta=0.5 Error!")
    call assertEquals (center_diag(7,1),1.02469135802469d0,weak_eps,"BigK(7,7) theta=0.5 Error!")
    call assertEquals (center_diag(18,1),1.02469135802469d0,weak_eps,"BigK(18,18) theta=0.5 Error!")
      !----up
    call assertEquals (up_diag(2,1),-0.0123456790123457d0,weak_eps,"BigK(2,3) theta=0.5 Error!")
    call assertEquals (up_diag(7,1),-0.0123456790123457d0,weak_eps,"BigK(7,8) theta=0.5 Error!")
    call assertEquals (up_diag(18,1),-0.0123456790123457d0,weak_eps,"BigK(18,19) theta=0.5 Error!")  
      !----down
    call assertEquals (down_diag(2,1),-0.0123456790123457d0,weak_eps,"BigK(2,1) theta=0.5 Error!")
    call assertEquals (down_diag(7,1),-0.0123456790123457d0,weak_eps,"BigK(7,6) theta=0.5 Error!")
    call assertEquals (down_diag(18,1),-0.0123456790123457d0,weak_eps,"BigK(18,17) theta=0.5 Error!") 
 

!---check for theta = 0.1
    
     theta_gtm = 0.1d0

call construct_diffusion_matrix(center_diag,      &
                                up_diag,          &     
                                down_diag,        &
                                area,             &
                                area_lo,          &
                                area_hi,          &
                                disp_coef_lo,     &
                                disp_coef_hi,     &
                                theta_gtm,        &
                                ncell,            &
                                time,             & 
                                nvar,             & 
                                dx,               &
                                dt)
                                            
      !--- centeral                                       
    call assertEquals (center_diag(2,1),1.004938272d0,weak_eps,"BigK(2,2) theta=0.1 Error!")
    call assertEquals (center_diag(7,1),1.004938272d0,weak_eps,"BigK(7,7) theta=0.1 Error!")
    call assertEquals (center_diag(18,1),1.004938272d0,weak_eps,"BigK(18,18) theta=0.1 Error!")
      !----up
    call assertEquals (up_diag(2,1),-0.00246913580246914d0,weak_eps,"BigK(2,3) theta=0.1 Error!")
    call assertEquals (up_diag(7,1),-0.00246913580246914d0,weak_eps,"BigK(7,8) theta=0.1 Error!")
    call assertEquals (up_diag(18,1),-0.00246913580246914d0,weak_eps,"BigK(18,19) theta=0.1 Error!")  
      !----down
    call assertEquals (down_diag(2,1),-0.00246913580246914d0,weak_eps,"BigK(2,1) theta=0.1 Error!")
    call assertEquals (down_diag(7,1),-0.00246913580246914d0,weak_eps,"BigK(7,6) theta=0.1 Error!")
    call assertEquals (down_diag(18,1),-0.00246913580246914d0,weak_eps,"BigK(18,17) theta=0.1 Error!") 
    

return
end subroutine

end module