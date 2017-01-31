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

!> todo: write tests for boundary diffusive flux subroutine
!> Tests boundary diffusive flux pointer 
!>@ingroup test_transport
module test_boundary_diffusion

contains
!> Tests boundary diffusive flux pointer
!> Test for trivial neumann no flux and also a time dependent boundary condition 
subroutine test_boundary_diffusion_flux

use fruit
use diffusion 
use boundary_diffusion
use gtm_precision

implicit none

integer,parameter :: nvar = 1                          !< Number of variables
integer,parameter :: ncell = 10                        !< Number of cells
  
real(gtm_real) :: diffusive_flux_lo(ncell,nvar)        !< Explicit diffusive boundary flux low side old time
real(gtm_real) :: diffusive_flux_hi(ncell,nvar)        !< Explicit diffusive boundary flux high side old time
real(gtm_real) :: conc(ncell,nvar)                     !< Explicit diffusive boundary flux low side new time
real(gtm_real) :: area_lo         (ncell)              !< Low side area centered at old time
real(gtm_real) :: area_hi         (ncell)              !< High side area centered at old time
real(gtm_real) :: disp_coef_lo (ncell,nvar)            !< Low side constituent dispersion coef.
real(gtm_real) :: disp_coef_hi (ncell,nvar)            !< High side constituent dispersion coef.
real(gtm_real) :: dt = 0.25d0                          !< dt
real(gtm_real) :: time = zero                          !< Time 
real(gtm_real) :: dx(ncell)                            !< dx
!---local
integer        :: istep                                !< Counter on time
integer, parameter :: nstep  = 10                      !< Number of time increment
real(gtm_real) :: reference_lo                         !< Reference value to compare with at low side of a cell
real(gtm_real) :: reference_hi                         !< Reference value to compare with at low side of a cell

dx = zero
  
boundary_diffusion_flux => neumann_zero_diffusive_flux

call boundary_diffusion_flux(diffusive_flux_lo, &
                             diffusive_flux_hi, &
                             conc,              &
                             area_lo,           &
                             area_hi,           &
                             disp_coef_lo,      &  
                             disp_coef_hi,      &
                             ncell,             &
                             nvar,              &
                             time,              &
                             dx,                &
                             dt)
                                                           
 call assertEquals (diffusive_flux_lo(1,nvar),zero,eps,"Error in diffusive boundary flux low at new time")
 call assertEquals (diffusive_flux_hi(ncell,nvar),zero,eps,"Error in diffusive boundary flux high at new time") 

boundary_diffusion_flux => neumann_sin_diffusive_flux

do istep =1,nstep

    call boundary_diffusion_flux(diffusive_flux_lo, &
                                 diffusive_flux_hi, &
                                 conc,              &
                                 area_lo,           &
                                 area_hi,           &
                                 disp_coef_lo,      &  
                                 disp_coef_hi,      &
                                 ncell,             &
                                 nvar,              &
                                 time,              &
                                 dx,                &
                                 dt)
                                
            
    reference_lo = two * dcos(pi* time / three)               !Just for test 
    reference_hi = five * dsin (pi * time / seven)                         
    
    time = time + dt 
    
    call assertEquals (diffusive_flux_lo(1,nvar),reference_lo,eps,"Error in diffusive boundary flux high at new time")                                                         
    call assertEquals (diffusive_flux_hi(ncell,nvar),reference_hi,eps,"Error in diffusive boundary flux high at new time")
end do

return
end subroutine

end module