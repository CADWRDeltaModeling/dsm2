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

!> boundary diffusive matrix interface to be fulfilled by driver or application
!>@ingroup transport
module boundary_diffusion_matrix_module
 !> Calculate boundary diffusion matrix
 interface
       !> Generic interface for calculating BC of matrix that should be fulfilled by
       !> client programs
       subroutine boundary_diffusive_matrix_if( center_diag ,      &
                                                      up_diag,          &     
                                                      down_diag,        &
                                                      area,             &
                                                      area_lo,          &
                                                      area_hi,          &          
                                                      disp_coef_lo,     &
                                                      disp_coef_hi,     &
                                                      theta_stm,        &
                                                      ncell,            &
                                                      time,             & 
                                                      nvar,             & 
                                                      dx,               &
                                                      dt)
                                                      
                                                      
        
         use gtm_precision
         implicit none
         !--- args
                                       
        integer, intent (in) :: ncell                                               !< Number of cells
        integer, intent (in) :: nvar                                                !< Number of variables

        real(gtm_real),intent (inout):: down_diag(ncell,nvar)                       !< Values of the coefficients below diagonal in matrix
        real(gtm_real),intent (inout):: center_diag(ncell,nvar)                     !< Values of the coefficients at the diagonal in matrix
        real(gtm_real),intent (inout):: up_diag(ncell,nvar)                         !< Values of the coefficients above the diagonal in matrix
        real(gtm_real), intent (in)  :: area (ncell)                                !< Cell centered area at new time 
        real(gtm_real), intent (in)  :: area_lo(ncell)                              !< Low side area at new time
        real(gtm_real), intent (in)  :: area_hi(ncell)                              !< High side area at new time 
        real(gtm_real), intent (in)  :: disp_coef_lo (ncell,nvar)                   !< Low side constituent dispersion coef. at new time
        real(gtm_real), intent (in)  :: disp_coef_hi (ncell,nvar)                   !< High side constituent dispersion coef. at new time
        real(gtm_real), intent (in)  :: time                                        !< Current time
        real(gtm_real), intent (in)  :: theta_stm                                   !< Explicitness coefficient; 0 is explicit, 0.5 Crank-Nicolson, 1 full implicit  
        real(gtm_real), intent (in)  :: dx                                          !< Spatial step  
        real(gtm_real), intent (in)  :: dt                                          !< Time step     
      
      
       end subroutine boundary_diffusive_matrix_if
 end interface

 !> This pointer should be set by the driver or client code to specify the 
 !> treatment at the first and last row of coefficient matrix
 ! todo: check the "boundary_diffusion_matrix"
 procedure(boundary_diffusive_matrix_if),pointer :: boundary_diffusion_matrix  => null()


 contains
 
 !> Example matrix that prints an error and bails
 subroutine uninitialized_diffusive_bc_matrix( center_diag ,      &
                                                      up_diag,          &     
                                                      down_diag,        &
                                                      area,             &
                                                      area_lo,          &
                                                      area_hi,          &          
                                                      disp_coef_lo,     &
                                                      disp_coef_hi,     &
                                                      theta_stm,        &
                                                      ncell,            &
                                                      time,             & 
                                                      nvar,             & 
                                                      dx,               &
                                                      dt)
                                         
     use gtm_precision 
     use error_handling
     
     implicit none
         !--- args
                                       
        integer, intent (in) :: ncell                                               !< Number of cells
        integer, intent (in) :: nvar                                                !< Number of variables

        real(gtm_real),intent (inout):: down_diag(ncell,nvar)                       !< Values of the coefficients below diagonal in matrix
        real(gtm_real),intent (inout):: center_diag(ncell,nvar)                     !< Values of the coefficients at the diagonal in matrix
        real(gtm_real),intent (inout):: up_diag(ncell,nvar)                         !< Values of the coefficients above the diagonal in matrix
        real(gtm_real), intent (in)  :: area (ncell)                                !< Cell centered area at new time 
        real(gtm_real), intent (in)  :: area_lo(ncell)                              !< Low side area at new time
        real(gtm_real), intent (in)  :: area_hi(ncell)                              !< High side area at new time 
        real(gtm_real), intent (in)  :: disp_coef_lo (ncell,nvar)                   !< Low side constituent dispersion coef. at new time
        real(gtm_real), intent (in)  :: disp_coef_hi (ncell,nvar)                   !< High side constituent dispersion coef. at new time
        real(gtm_real), intent (in)  :: time                                        !< Current time
        real(gtm_real), intent (in)  :: theta_stm                                   !< Explicitness coefficient; 0 is explicit, 0.5 Crank-Nicolson, 1 full implicit  
        real(gtm_real), intent (in)  :: dx                                          !< Spatial step  
        real(gtm_real), intent (in)  :: dt                                          !< Time step     
                                               !< Time step     
      
     call gtm_fatal("boundary not implemented!")
     
     return
 end subroutine 
 
 !> Example diffusive flux that imposes Neumann boundaries with zero flux at
 !> both ends of the channel.
 subroutine neumann_no_flow_matrix(center_diag ,      &
                                                      up_diag,          &     
                                                      down_diag,        &
                                                      area,             &
                                                      area_lo,          &
                                                      area_hi,          &          
                                                      disp_coef_lo,     &
                                                      disp_coef_hi,     &
                                                      theta_stm,        &
                                                      ncell,            &
                                                      time,             & 
                                                      nvar,             & 
                                                      dx,               &
                                                      dt)
     use gtm_precision
     implicit none
         !--- args
                                       
        integer, intent (in) :: ncell                                               !< Number of cells
        integer, intent (in) :: nvar                                                !< Number of variables

        real(gtm_real),intent (inout):: down_diag(ncell,nvar)                       !< Values of the coefficients below diagonal in matrix
        real(gtm_real),intent (inout):: center_diag(ncell,nvar)                     !< Values of the coefficients at the diagonal in matrix
        real(gtm_real),intent (inout):: up_diag(ncell,nvar)                         !< Values of the coefficients above the diagonal in matrix
        real(gtm_real), intent (in)  :: area (ncell)                                !< Cell centered area at new time 
        real(gtm_real), intent (in)  :: area_lo(ncell)                              !< Low side area at new time
        real(gtm_real), intent (in)  :: area_hi(ncell)                              !< High side area at new time 
        real(gtm_real), intent (in)  :: disp_coef_lo (ncell,nvar)                   !< Low side constituent dispersion coef. at new time
        real(gtm_real), intent (in)  :: disp_coef_hi (ncell,nvar)                   !< High side constituent dispersion coef. at new time
        real(gtm_real), intent (in)  :: time                                        !< Current time
        real(gtm_real), intent (in)  :: theta_stm                                   !< Explicitness coefficient; 0 is explicit, 0.5 Crank-Nicolson, 1 full implicit  
        real(gtm_real), intent (in)  :: dx                                          !< Spatial step  
        real(gtm_real), intent (in)  :: dt                                          !< Time step     
      
        !---local
        real(gtm_real) :: d_star 
        d_star = dt/(dx*dx)  
      
     ! todo: add types of other BC 
          
     center_diag(1,nvar)=area(1) + theta_stm*d_star*(area_hi(1)*disp_coef_hi(1,nvar) + two*area_lo(1)*disp_coef_lo(1,nvar))
     center_diag(ncell,nvar)= area(ncell) + theta_stm*d_star*(two*area_hi(ncell)*disp_coef_hi(ncell,nvar) + area_lo(ncell)*disp_coef_lo(ncell,nvar))
     
     ! todo: implement and test
     return
 end subroutine
 
end module
