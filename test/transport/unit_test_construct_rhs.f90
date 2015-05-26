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

!> todo: write tests for construct right hand side subroutine before imposing Boundary Conditions
!> Tests the interior elemets of right hand side F (KU=F)
!>@ingroup test_transport
module test_construct_r_h_s

contains

!> Tests the interior elemets of right hand side
subroutine test_construct_elemnts_rhs 
  
use diffusion 
use fruit
use gtm_precision
 
  implicit none
  
integer,parameter :: ncell = 6                                 !< Number of cells
integer,parameter :: nvar = 1                                  !< Number of variables

real(gtm_real) :: right_hand_side(ncell,nvar)                  !< The right hand side vector
real(gtm_real)  :: explicit_diffuse_op (ncell,nvar)            !< Explicit diffusion operator
real(gtm_real)  :: area_prev (ncell)                           !< Cell centered area at old time 
real(gtm_real)  :: conc_prev(ncell,nvar)                       !< Concentration at old time
real(gtm_real)  :: area_lo_prev (ncell)                        !< Low side area at old time
real(gtm_real)  :: area_hi_prev (ncell)                        !< High side area at old time 
real(gtm_real)  :: disp_coef_lo_prev (ncell,nvar)              !< Low side constituent dispersion coef. at old time
real(gtm_real)  :: disp_coef_hi_prev (ncell,nvar)              !< High side constituent dispersion coef. at old time
real(gtm_real)  :: time                                        !< Current time
real(gtm_real)  :: theta_gtm                                   !< Explicitness coefficient; 0 is explicit, 0.5 Crank-Nicolson, 1 full implicit  
real(gtm_real)  :: dx(ncell)                                   !< Spatial step  
real(gtm_real)  :: dt                                          !< Time step                                   
real(gtm_real)  :: diffusive_flux_boundary_lo (nvar)           !< Diffusive flux operator at low side 
real(gtm_real)  :: diffusive_flux_boundary_hi (nvar)           !< Diffusive flux operator at high side 


conc_prev(:,1) = [300.0d0,305.0d0,320.0d0,330.0d0,340.0d0,350.0d0]
explicit_diffuse_op(:,1) = [112.7d0,225.7d0,-112.8d0,-0.2d0,-0.4d0,-225.d0]
area_lo_prev(:) = [100.0d0,98.0d0, 96.0d0,94.0d0,92.0d0,90.0d0]
area_hi_prev(:) = [98.0d0, 96.0d0,94.0d0,92.0d0,90.0d0,88.0d0]
area_prev (:) = [99d0,97d0,95d0,93d0,91d0,89d0]
diffusive_flux_boundary_lo(:) = zero
diffusive_flux_boundary_hi(:) = zero
dx=2.0d0
dt=1.0d0
theta_gtm = 1.0d0
disp_coef_lo_prev(:,1) = [0.9d0,0.92d0,0.94d0,0.96d0,0.98d0,1.d0]
disp_coef_hi_prev(:,1) = [0.92d0,0.94d0,0.96d0,0.98d0,1.d0,1.02d0]
time =LARGEREAL 

   !--theta =1 
   
call construct_right_hand_side(right_hand_side,       & 
                               explicit_diffuse_op,   & 
                               area_prev,             &
                               area_lo_prev,          &
                               area_hi_prev,          &
                               disp_coef_lo_prev,     &
                               disp_coef_hi_prev,     &
                               conc_prev,             &
                               theta_gtm,             &
                               ncell,                 &
                               time,                  &
                               nvar,                  &  
                               dx,                    &
                               dt)
                                  
                                  
  call assertEquals (right_hand_side(2,1),29585d0,weak_eps,"Error in r_h_s vector 2 ,theta=1")
  call assertEquals (right_hand_side(5,1),30940d0,weak_eps,"Error in r_h_s vector 5 ,theta=1")
   
     !--theta =0.6 
     theta_gtm = 0.6d0 
   
call construct_right_hand_side(right_hand_side,       & 
                               explicit_diffuse_op,   & 
                               area_prev,             &
                               area_lo_prev,          &
                               area_hi_prev,          &
                               disp_coef_lo_prev,     &
                               disp_coef_hi_prev,     &
                               conc_prev,             &
                               theta_gtm,             &
                               ncell,                 &
                               time,                  &
                               nvar,                  &  
                               dx,                    &
                               dt)
                                  
                                  
  call assertEquals (right_hand_side(2,1),29494.72d0,weak_eps,"Error in r_h_s vector 2 ,theta = 0.6")
  call assertEquals (right_hand_side(6,1),31240d0,weak_eps, "Error in r_h_s vector 6 ,theta = 0.6")
 
  !--theta =0.1 
     theta_gtm = 0.1d0 
   
call construct_right_hand_side(right_hand_side,       & 
                               explicit_diffuse_op,   & 
                               area_prev,             &
                               area_lo_prev,          &
                               area_hi_prev,          &
                               disp_coef_lo_prev,     &
                               disp_coef_hi_prev,     &
                               conc_prev,             &
                               theta_gtm,             &
                               ncell,                 &
                               time,                  &
                               nvar,                  &  
                               dx,                    &
                               dt)
                                  
                                  
  call assertEquals (right_hand_side(1,1),29598.57d0,weak_eps,"Error in r_h_s vector 1,theta = 0.1")
  call assertEquals (right_hand_side(5,1),30940.36d0,weak_eps,"Error in r_h_s vector 5 ,theta = 0.1")


return
end subroutine 

end module