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

!> Write tests for interior diffusive flux sobroutine
!>@ingroup test_transport
module test_diffusive_flux

contains

!> Tests the diffusion flux subroutine inside the diffusion routine
subroutine test_make_dif_flux_sub
  
use diffusion
use fruit
use gtm_precision
  
  implicit none
  
integer,parameter :: ncell = 6                       !< Number of cells
integer,parameter  :: nvar = 1                       !< Number of variables

real(gtm_real) :: diffusive_flux_hi(ncell,nvar)      !< Explicit diffusive flux high side
real(gtm_real) :: diffusive_flux_lo(ncell,nvar)      !< Explicit diffusive flux low side
real(gtm_real) :: conc_prev(ncell,nvar)              !< Concentration at old time
real(gtm_real) :: area_lo_prev (ncell)               !< Low side area at old time
real(gtm_real) :: area_hi_prev (ncell)               !< High side area at old time 
real(gtm_real) :: disp_coef_lo_prev (ncell,nvar)     !< Low side constituent dispersion coef. at old time
real(gtm_real) :: disp_coef_hi_prev (ncell,nvar)     !< High side constituent dispersion coef. at old time
real(gtm_real) :: time                               !< Current time
real(gtm_real) :: dx(ncell)                          !< Spatial step   
real(gtm_real) :: dt

conc_prev(:,1)  = [300d0,305d0,320d0,330d0,340d0,350d0]
area_lo_prev(:) = [100d0,98d0,96d0,94d0,92d0,90d0]
area_hi_prev(:) = [98d0,96d0,94d0,92d0,90d0,88d0]
disp_coef_lo_prev(:,1) = [0.9d0,0.92d0,0.94d0,0.96d0,0.98d0,1d0]
disp_coef_hi_prev(:,1) = [0.92d0,0.94d0,0.96d0,0.98d0,1d0,1.02d0]
dx = 2.0d0 
time =LARGEREAL 
 
call diffusive_flux(diffusive_flux_lo, &
                         diffusive_flux_hi, &
                          conc_prev,        &
                          area_lo_prev,     &
                          area_hi_prev,     &
                          disp_coef_lo_prev,&  
                          disp_coef_hi_prev,&
                          ncell,            &
                          nvar,             &
                          time,             &
                          dx,               &
                          dt)
                    
                                            
!----diffusive_flux_interior_lo

  call assertEquals (diffusive_flux_lo(2,1),-225.4d0 ,weak_eps,"Error in diffusive_flux_interior_lo cell 2")
  call assertEquals (diffusive_flux_lo(3,1),-676.8d0 ,weak_eps,"Error in diffusive_flux_interior_lo cell 3")
  call assertEquals (diffusive_flux_lo(4,1),-451.2d0 ,weak_eps,"Error in diffusive_flux_interior_lo cell 4")
  call assertEquals (diffusive_flux_lo(5,1),-450.8d0 ,weak_eps,"Error in diffusive_flux_interior_lo cell 5")
  call assertEquals (diffusive_flux_lo(6,1),-450d0 ,weak_eps,"Error in diffusive_flux_interior_lo cell 6")
  
!----diffusive_flux_interior_hi

  
  call assertEquals (diffusive_flux_hi(1,1),-225.4d0 ,weak_eps,"Error in diffusive_flux_interior_lo cell 1")
  call assertEquals (diffusive_flux_hi(2,1),-676.8d0 ,weak_eps,"Error in diffusive_flux_interior_lo cell 2")
  call assertEquals (diffusive_flux_hi(3,1),-451.2d0 ,weak_eps,"Error in diffusive_flux_interior_lo cell 3")
  call assertEquals (diffusive_flux_hi(4,1),-450.8d0 ,weak_eps,"Error in diffusive_flux_interior_lo cell 4")
  call assertEquals (diffusive_flux_hi(5,1),-450.0d0 ,weak_eps,"Error in diffusive_flux_interior_lo cell 5")
  

return
end subroutine 

end module