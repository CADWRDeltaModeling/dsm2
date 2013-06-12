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

!> Unit test for explicit diffusion operator subroutine
!>@ingroup test_transport
module test_explicit_diffusion_operator

use diffusion 
use fruit
use gtm_precision

contains
!> Test explicit diffusion operator versus pre-calculated values 
subroutine test_explicit_interior_diffusion_op
  use diffusion 
  implicit none
  
integer, parameter :: ncell = 6 !< Number of cells
integer, parameter :: nvar =1   !< Number of variables

real(gtm_real) :: explicit_diffuse_op(ncell,nvar)             !< Explicit diffusion operator
real(gtm_real) :: conc_prev(ncell,nvar)                       !< Concentration at old time
real(gtm_real) :: area_lo_prev (ncell)                        !< Low side area at old time
real(gtm_real) :: area_hi_prev (ncell)                        !< High side area at old time 
real(gtm_real) :: disp_coef_lo_prev (ncell,nvar)              !< Low side constituent dispersion coef. at old time
real(gtm_real) :: disp_coef_hi_prev (ncell,nvar)              !< High side constituent dispersion coef. at old time
real(gtm_real) :: time                                        !< Current time
real(gtm_real) :: dx(ncell)                                   !< Spacial step  
real(gtm_real) :: dt                                          !< Time step  

!-- set the values and the known answer
conc_prev(:,1)  = [300d0,305d0,320d0,330d0,340d0,350d0]
area_lo_prev(:) = [100d0,98d0,96d0,94d0,92d0,90d0]
area_hi_prev(:) = [98d0,96d0,94d0,92d0,90d0,88d0]
disp_coef_lo_prev(:,1) = [0.9d0,0.92d0,0.94d0,0.96d0,0.98d0,1d0]
disp_coef_hi_prev(:,1) = [0.92d0,0.94d0,0.96d0,0.98d0,1d0,1.02d0]
dx = 2.0d0 
time =LARGEREAL 
                                            
call explicit_diffusion_operator (explicit_diffuse_op,&
                                  conc_prev,          &
                                  area_lo_prev,       &
                                  area_hi_prev,       &
                                  disp_coef_lo_prev,  &  
                                  disp_coef_hi_prev,  &
                                  ncell,              &
                                  nvar,               &
                                  time,               &
                                  dx,                 &
                                  dt)                                                                     
  
  call assertEquals(explicit_diffuse_op(2,nvar),-225.7d0,weak_eps,"Error in explicit diffusive flux operator 2")
  call assertEquals(explicit_diffuse_op(3,nvar),112.8d0,weak_eps,"Error in explicit diffusive flux operator 3")
  call assertEquals(explicit_diffuse_op(4,nvar),0.2d0,weak_eps,"Error in explicit diffusive flux operator 4")
  call assertEquals(explicit_diffuse_op(5,nvar),0.4d0,weak_eps,"Error in explicit diffusive flux operator 5")
 
return
end subroutine

end module