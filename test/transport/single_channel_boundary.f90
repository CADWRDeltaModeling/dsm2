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

!> Boundary conditions for a single channel
!>@ingroup test_transport
module single_channel_boundary

use boundary_diffusion
use boundary_advection

!> Advective fluxes on lo and hi side of single channel.
!> Each is expected to operate on only one end
procedure(boundary_advective_flux_if), pointer :: advective_flux_lo  => null()
procedure(boundary_advective_flux_if), pointer :: advective_flux_hi  => null()


!> Diffusive fluxes on lo and hi side of single channel.
!> Each is expected to operate on only one end
procedure(boundary_diffusive_flux_if), pointer :: diffusion_flux_lo  => null()
procedure(boundary_diffusive_flux_if), pointer :: diffusion_flux_hi  => null()


!> Diffusive matrix on lo and hi side of single channel.
!> Each is expected to operate on only one end
procedure(boundary_diffusive_matrix_if), pointer :: diffusion_matrix_lo  => null()
procedure(boundary_diffusive_matrix_if), pointer :: diffusion_matrix_hi  => null()
 
!> Provide data for boundary
abstract interface
!> Generic interface for boundary diffusion that should be fulfilled by client programs
subroutine boundary_data_if(bc_data,           &
                            xloc,              &
                            conc,              &
                            ncell,             &
                            nvar,              &
                            origin,            &
                            time,              &
                            dx,                &
                            dt)
    use gtm_precision
    implicit none
    !--- args
    integer, intent(in)  :: ncell                                    !< Number of cells
    integer, intent(in)  :: nvar                                     !< Number of variables
    real(gtm_real), intent (out)  :: bc_data(nvar)                   !< concentration or gradient data
    real(gtm_real), intent (in)   :: xloc                            !< location where data is requested
    real(gtm_real), intent (in)   :: time                            !< Time
    real(gtm_real), intent (in)   :: origin                          !< Space origin
    real(gtm_real), intent (in)   :: conc(ncell,nvar)                !< Concentration 
    real(gtm_real), intent (in)   :: dt                              !< Time step    
    real(gtm_real), intent (in)   :: dx(ncell)                       !< Spatial step
    
    end subroutine 
 end interface

!> User settable function that provides the data (concentration or derivative) used for the BC
procedure(boundary_data_if),pointer :: advection_data_lo  => null()
procedure(boundary_data_if),pointer :: advection_data_hi  => null()
procedure(boundary_data_if),pointer :: diffusion_data_lo  => null()
procedure(boundary_data_if),pointer :: diffusion_data_hi  => null()

contains

subroutine set_single_channel_boundary(advect_bc_lo, advect_data_lo,   &
                                       advect_bc_hi, advect_data_hi,   &
                                       diffuse_bc_lo, diffuse_data_lo, &
                                       diffuse_bc_hi, diffuse_data_hi)
   use error_handling
   implicit none

    procedure(boundary_advective_flux_if), pointer, intent(in) :: advect_bc_lo
    procedure(boundary_advective_flux_if), pointer, intent(in) :: advect_bc_hi
    
    procedure(boundary_diffusive_flux_if), pointer, intent(in) :: diffuse_bc_lo
    procedure(boundary_diffusive_flux_if), pointer, intent(in) :: diffuse_bc_hi

    procedure(boundary_data_if), pointer, intent(in) :: advect_data_lo
    procedure(boundary_data_if), pointer, intent(in) :: advect_data_hi
    procedure(boundary_data_if), pointer, intent(in) :: diffuse_data_lo
    procedure(boundary_data_if), pointer, intent(in) :: diffuse_data_hi

    advective_flux_lo => advect_bc_lo
    advective_flux_hi => advect_bc_hi
    diffusion_flux_lo => diffuse_bc_lo
    diffusion_flux_hi => diffuse_bc_hi

    advection_data_lo => advect_data_lo
    advection_data_hi => advect_data_hi
    diffusion_data_lo => diffuse_data_lo
    diffusion_data_hi => diffuse_data_hi

    if(associated(diffusion_flux_lo,dirichlet_diffusive_flux_lo))then
       diffusion_matrix_lo => dirichlet_diffusive_matrix_lo
    elseif(associated(diffusion_flux_lo,neumann_diffusive_flux_lo))then
       diffusion_matrix_lo => neumann_diffusive_matrix_lo
    else
       call gtm_fatal("Unable to infer diffusion lo-side boundary condition (for matrix)")
    end if
    if(associated(diffusion_flux_hi,dirichlet_diffusive_flux_hi))then
       diffusion_matrix_hi => dirichlet_diffusive_matrix_hi
    elseif(associated(diffusion_flux_hi,neumann_diffusive_flux_hi))then
       diffusion_matrix_hi => neumann_diffusive_matrix_hi
    else
       call gtm_fatal("Unable to infer diffusion hi-side boundary condition (for matrix)")
    end if


return
end subroutine

!> Advective flux boundary condition for a single channel, delagates to an implementation at each end
subroutine single_channel_boundary_advective_flux(flux_lo,    &
                                                  flux_hi,    &
                                                  conc_lo,    &
                                                  conc_hi,    &
                                                  flow_lo,    &
                                                  flow_hi,    &
                                                  ncell,      &
                                                  nvar,       &
                                                  time,       &
                                                  dt,         &
                                                  dx)
 
 use gtm_precision
  
 implicit none
  !--- args          
 integer,intent(in)  :: ncell                            !< Number of cells
 integer,intent(in)  :: nvar                             !< Number of variables
 real(gtm_real),intent(inout) :: flux_lo(ncell,nvar)     !< flux on lo side of cell, time centered
 real(gtm_real),intent(inout) :: flux_hi(ncell,nvar)     !< flux on hi side of cell, time centered
 real(gtm_real),intent(in)    :: flow_lo(ncell)          !< flow on lo side of cells centered in time
 real(gtm_real),intent(in)    :: flow_hi(ncell)          !< flow on hi side of cells centered in time
 real(gtm_real),intent(in)    :: conc_lo(ncell,nvar)     !< concentration extrapolated to lo face
 real(gtm_real),intent(in)    :: conc_hi(ncell,nvar)     !< concentration extrapolated to hi face
 real(gtm_real),intent(in)    :: time                    !< Current time
 real(gtm_real),intent(in)    :: dx(ncell)               !< Spatial step  
 real(gtm_real),intent(in)    :: dt                      !< Time step     

 call advective_flux_lo(flux_lo,    &
                        flux_hi,    &
                        conc_lo,    &
                        conc_hi,    &
                        flow_lo,    &
                        flow_hi,    &
                        ncell,      &
                        nvar,       &
                        time,       &
                        dt,         &
                        dx)

 call advective_flux_hi(flux_lo,    &
                        flux_hi,    &
                        conc_lo,    &
                        conc_hi,    &
                        flow_lo,    &
                        flow_hi,    &
                        ncell,      &
                        nvar,       &
                        time,       &
                        dt,         &
                        dx)                        

return
end subroutine 


!> Diffusive flux boundary condition for a single channel, delagates to an implementation at each end
subroutine single_channel_boundary_diffusive_flux(diffusive_flux_lo, &
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
       
    use gtm_precision
    implicit none
    !--- args
    integer, intent(in)  :: ncell                                    !< Number of cells
    integer, intent(in)  :: nvar                                     !< Number of variables
    real(gtm_real), intent (inout):: diffusive_flux_lo(ncell,nvar)   !< Face flux, lo side
    real(gtm_real), intent (inout):: diffusive_flux_hi(ncell,nvar)   !< Face flux, hi side
    real(gtm_real), intent (in)   :: area_lo(ncell)                  !< Low side area centered at time
    real(gtm_real), intent (in)   :: area_hi(ncell)                  !< High side area centered at time
    real(gtm_real), intent (in)   :: time                            !< Time
    real(gtm_real), intent (in)   :: conc(ncell,nvar)                !< Concentration 
    real(gtm_real), intent (in)   :: disp_coef_lo(ncell)             !< Low side constituent dispersion coef.
    real(gtm_real), intent (in)   :: disp_coef_hi(ncell)             !< High side constituent dispersion coef.
    real(gtm_real), intent (in)   :: dt                              !< Spatial step
    real(gtm_real), intent (in)   :: dx(ncell)                       !< Time step   
   
    call diffusion_flux_lo(diffusive_flux_lo, &
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

    call diffusion_flux_hi(diffusive_flux_lo, &
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

return
end subroutine

!> Matrix boundary condition, delagates to an implementation at each end
subroutine single_channel_boundary_diffusive_matrix(center_diag ,        &
                          up_diag,             &     
                          down_diag,           &
                          right_hand_side,     &
                          explicit_diffuse_op, & 
                          conc_prev,           &
                          mass_prev,           &
                          area_lo_prev,        &
                          area_hi_prev,        &
                          disp_coef_lo_prev,   &
                          disp_coef_hi_prev,   & 
                          conc,                &
                          flow_lo,             &
                          flow_hi,             & 
                          area,                &
                          area_lo,             &
                          area_hi,             &          
                          disp_coef_lo,        &
                          disp_coef_hi,        &
                          theta_gtm,           &
                          ncell,               &
                          time,                & 
                          nvar,                & 
                          dx,                  &
                          dt)
                                              
 use gtm_precision
 implicit none
 !--- args
                               
 integer, intent (in) :: ncell                                    !< Number of cells
 integer, intent (in) :: nvar                                     !< Number of variables
 real(gtm_real),intent (inout):: down_diag(ncell)                 !< Values of the coefficients below diagonal in matrix
 real(gtm_real),intent (inout):: center_diag(ncell)               !< Values of the coefficients at the diagonal in matrix
 real(gtm_real),intent (inout):: up_diag(ncell)                   !< Values of the coefficients above the diagonal in matrix
 real(gtm_real),intent (inout):: right_hand_side(ncell,nvar)      !< Values of the coefficients of right  hand side vector
 real(gtm_real), intent (in)  :: conc(ncell,nvar)                 !< Concentration
 real(gtm_real), intent (in)  :: explicit_diffuse_op(ncell,nvar)  !< Explicit diffusive operator 
 real(gtm_real), intent(in) :: conc_prev(ncell,nvar)             !< Cell centered concentration at old time
 real(gtm_real), intent(in) :: mass_prev(ncell,nvar)             !< Mass from old time and previous two steps
 real(gtm_real), intent(in) :: area_lo_prev(ncell)               !< Low side area at old time
 real(gtm_real), intent(in) :: area_hi_prev(ncell)               !< High side area at old time
 real(gtm_real), intent(in) :: disp_coef_lo_prev(ncell)          !< Low side constituent dispersion coef. at old time
 real(gtm_real), intent(in) :: disp_coef_hi_prev(ncell)          !< High side constituent dispersion coef. at old time
 real(gtm_real), intent(in) :: flow_lo(ncell)                    !< Low side flow at new time
 real(gtm_real), intent(in) :: flow_hi(ncell)                    !< High side flow at new time                                                      
 real(gtm_real), intent (in)  :: area (ncell)                     !< Cell centered area at new time 
 real(gtm_real), intent (in)  :: area_lo(ncell)                   !< Low side area at new time
 real(gtm_real), intent (in)  :: area_hi(ncell)                   !< High side area at new time 
 real(gtm_real), intent (in)  :: disp_coef_lo (ncell)             !< Low side constituent dispersion coef. at new time
 real(gtm_real), intent (in)  :: disp_coef_hi (ncell)             !< High side constituent dispersion coef. at new time
 real(gtm_real), intent (in)  :: time                             !< Current time
 real(gtm_real), intent (in)  :: theta_gtm                        !< Explicitness coefficient; 0 is explicit, 0.5 Crank-Nicolson, 1 full implicit  
 real(gtm_real), intent (in)  :: dx(ncell)                        !< Spatial step  
 real(gtm_real), intent (in)  :: dt                               !< Time step     

 call diffusion_matrix_lo(center_diag ,        &
                          up_diag,             &     
                          down_diag,           &
                          right_hand_side,     &
                          explicit_diffuse_op, & 
                          conc_prev,           &
                          mass_prev,           &
                          area_lo_prev,        &
                          area_hi_prev,        &
                          disp_coef_lo_prev,   &
                          disp_coef_hi_prev,   & 
                          conc,                &
                          flow_lo,             &
                          flow_hi,             & 
                          area,                &
                          area_lo,             &
                          area_hi,             &          
                          disp_coef_lo,        &
                          disp_coef_hi,        &
                          theta_gtm,           &
                          ncell,               &
                          time,                & 
                          nvar,                & 
                          dx,                  &
                          dt)
                          
 call diffusion_matrix_hi(center_diag ,        &
                          up_diag,             &     
                          down_diag,           &
                          right_hand_side,     &
                          explicit_diffuse_op, & 
                          conc_prev,           &
                          mass_prev,           &
                          area_lo_prev,        &
                          area_hi_prev,        &
                          disp_coef_lo_prev,   &
                          disp_coef_hi_prev,   & 
                          conc,                &
                          flow_lo,             &
                          flow_hi,             & 
                          area,                &
                          area_lo,             &
                          area_hi,             &          
                          disp_coef_lo,        &
                          disp_coef_hi,        &
                          theta_gtm,           &
                          ncell,               &
                          time,                & 
                          nvar,                & 
                          dx,                  &
                          dt)
return
end subroutine 


!> Example advective flux that imposes dirichlet boundaries on lo side
subroutine dirichlet_advective_flux_lo(flux_lo,    &
                                       flux_hi,    &
                                       conc_lo,    &
                                       conc_hi,    &
                                       flow_lo,    &
                                       flow_hi,    &
                                       ncell,      &
                                       nvar,       &
                                       time,       &
                                       dt,         &
                                       dx)
     
     use gtm_precision
     use source_sink
     implicit none
      !--- args          
     integer,intent(in)  :: ncell                            !< Number of cells
     integer,intent(in)  :: nvar                             !< Number of variables
     real(gtm_real),intent(inout) :: flux_lo(ncell,nvar)     !< flux on lo side of cell at time 
     real(gtm_real),intent(inout) :: flux_hi(ncell,nvar)     !< flux on hi side of cell at time
     real(gtm_real), intent(in)   :: flow_lo(ncell)          !< flow on lo side of cells centered at time
     real(gtm_real), intent(in)   :: flow_hi(ncell)          !< flow on hi side of cells centered at time
     real(gtm_real), intent(in)   :: conc_lo(ncell,nvar)     !< concentration extrapolated to lo face
     real(gtm_real), intent(in)   :: conc_hi(ncell,nvar)     !< concentration extrapolated to hi face
     real(gtm_real), intent(in)   :: time                    !< Current time
     real(gtm_real), intent(in)   :: dx(ncell)               !< Spatial step  
     real(gtm_real), intent(in)   :: dt                      !< Time step     
    !---local
    real(gtm_real) :: bc_data(nvar)
    real(gtm_real) :: xloc
    real(gtm_real) :: origin = zero !todo: HARDWIRE
    real(gtm_real) :: multiplier(nvar)
    xloc = origin
    call advection_data_lo(bc_data,           &
                           xloc,              &
                           conc_lo,           &
                           ncell,             &
                           nvar,              &
                           origin,            &
                           time,              &
                           dx,                &
                           dt)
   if (flow_lo(1) .ge. zero)then
        multiplier = one
        if(allocated(linear_decay)) multiplier = exp(linear_decay*half*dt)
       flux_lo(1,:)=bc_data*flow_lo(1)*multiplier
   end if
   return
 end subroutine

!> Example advective flux that imposes dirichlet boundaries on high side
subroutine dirichlet_advective_flux_hi(flux_lo,    &
                                       flux_hi,    &
                                       conc_lo,    &
                                       conc_hi,    &
                                       flow_lo,    &
                                       flow_hi,    &
                                       ncell,      &
                                       nvar,       &
                                       time,       &
                                       dt,         &
                                       dx)
     
    use gtm_precision
    implicit none
   !--- args          
     integer,intent(in)  :: ncell                            !< Number of cells
     integer,intent(in)  :: nvar                             !< Number of variables
     real(gtm_real),intent(inout) :: flux_lo(ncell,nvar)     !< flux on lo side of cell at time 
     real(gtm_real),intent(inout) :: flux_hi(ncell,nvar)     !< flux on hi side of cell at time
     real(gtm_real), intent(in)   :: flow_lo(ncell)          !< flow on lo side of cells centered at time
     real(gtm_real), intent(in)   :: flow_hi(ncell)          !< flow on hi side of cells centered at time
     real(gtm_real), intent(in)   :: conc_lo(ncell,nvar)     !< concentration extrapolated to lo face
     real(gtm_real), intent(in)   :: conc_hi(ncell,nvar)     !< concentration extrapolated to hi face
     real(gtm_real), intent(in)   :: time                    !< Current time
     real(gtm_real), intent(in)   :: dx(ncell)               !< Spatial step  
     real(gtm_real), intent(in)   :: dt                      !< Time step     
    !---local
    real(gtm_real) :: bc_data(nvar)
    real(gtm_real) :: xloc
    real(gtm_real) :: origin = zero !todo: HARDWIRE

    xloc = origin + sum(dx)
    call advection_data_hi(bc_data,           &
                           xloc,              &
                           conc_hi,           &
                           ncell,             &
                           nvar,              &
                           origin,            &
                           time,              &
                           dx,                &
                           dt)
    if (flow_hi(ncell) .le. zero) then
        flux_hi(ncell,:) = bc_data*flow_hi(ncell)
    end if
    return
 end subroutine


!> dirichlet boundary condition that sets only the low side boundary
subroutine dirichlet_diffusive_flux_lo(diffusive_flux_lo, &
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
       
    use gtm_precision
    implicit none
    !--- args
    integer, intent(in)  :: ncell                                    !< Number of cells
    integer, intent(in)  :: nvar                                     !< Number of variables
    real(gtm_real), intent (inout):: diffusive_flux_lo(ncell,nvar)   !< Face flux, lo side
    real(gtm_real), intent (inout):: diffusive_flux_hi(ncell,nvar)   !< Face flux, hi side
    real(gtm_real), intent (in)   :: area_lo(ncell)                  !< Low side area centered at time
    real(gtm_real), intent (in)   :: area_hi(ncell)                  !< High side area centered at time
    real(gtm_real), intent (in)   :: time                            !< Time
    real(gtm_real), intent (in)   :: conc(ncell,nvar)                !< Concentration 
    real(gtm_real), intent (in)   :: disp_coef_lo (ncell)            !< Low side constituent dispersion coef.
    real(gtm_real), intent (in)   :: disp_coef_hi (ncell)            !< High side constituent dispersion coef.
    real(gtm_real), intent (in)   :: dt                              !< Time step
    real(gtm_real), intent (in)   :: dx(ncell)                       !< Spatial step   
    !---local
    real(gtm_real) :: bc_data(nvar)
    real(gtm_real) :: xloc
    real(gtm_real) :: origin = zero !todo: HARDWIRE

    xloc = origin
    call diffusion_data_lo(bc_data,           &
                           xloc,              &
                           conc,              &
                           ncell,             &
                           nvar,              &
                           origin,            &
                           time,              &
                           dx,                &
                           dt)
    
   diffusive_flux_lo(1,:)=-two*area_lo(1)*disp_coef_lo(1)*(conc(1,:)-bc_data(:))/dx(1)
   
return
end subroutine

!> dirichlet boundary condition that sets only the hi side boundary
subroutine dirichlet_diffusive_flux_hi(diffusive_flux_lo, &
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
       
    use gtm_precision
    implicit none
    !--- args
    integer, intent(in)  :: ncell                                    !< Number of cells
    integer, intent(in)  :: nvar                                     !< Number of variables
    real(gtm_real), intent (inout):: diffusive_flux_lo(ncell,nvar)   !< Face flux, lo side
    real(gtm_real), intent (inout):: diffusive_flux_hi(ncell,nvar)   !< Face flux, hi side
    real(gtm_real), intent (in)   :: area_lo(ncell)                  !< Low side area
    real(gtm_real), intent (in)   :: area_hi(ncell)                  !< High side area
    real(gtm_real), intent (in)   :: time                            !< Time
    real(gtm_real), intent (in)   :: conc(ncell,nvar)                !< Concentration 
    real(gtm_real), intent (in)   :: disp_coef_lo(ncell)             !< Low side constituent dispersion coef.
    real(gtm_real), intent (in)   :: disp_coef_hi(ncell)             !< High side constituent dispersion coef.
    real(gtm_real), intent (in)   :: dt                              !< Time step
    real(gtm_real), intent (in)   :: dx(ncell)                       !< Spatial step   
    ! ---local
    real(gtm_real) :: bc_data(nvar)                                  !< The value of the constituent on the Boundary
    real(gtm_real) :: xloc                                
    real(gtm_real) :: origin = zero !todo: HARDWIRE

    xloc = origin + sum(dx)
    call diffusion_data_hi(bc_data,           &
                           xloc,              &
                           conc,              &
                           ncell,             &
                           nvar,              &
                           origin,            &
                           time,              &
                           dx,                &
                           dt)
    
   diffusive_flux_hi(ncell,:)=-two*area_hi(ncell)*disp_coef_hi(ncell)*(bc_data(:)-conc(ncell,:))/dx(ncell)

return
end subroutine



!> Matrix boundary condition for dirichlet, only operates on lo end
subroutine dirichlet_diffusive_matrix_lo(center_diag ,        &
                                         up_diag,             &     
                                         down_diag,           &
                                         right_hand_side,     &
                                         explicit_diffuse_op, & 
                                         conc_prev,           &
                                         mass_prev,           &
                                         area_lo_prev,        &
                                         area_hi_prev,        &
                                         disp_coef_lo_prev,   &
                                         disp_coef_hi_prev,   & 
                                         conc,                &
                                         flow_lo,             &
                                         flow_hi,             & 
                                         area,                &
                                         area_lo,             &
                                         area_hi,             &          
                                         disp_coef_lo,        &
                                         disp_coef_hi,        &
                                         theta_gtm,           &
                                         ncell,               &
                                         time,                & 
                                         nvar,                & 
                                         dx,                  &
                                         dt)
                                              
 use gtm_precision
 implicit none
 !--- args
                               
 integer, intent (in) :: ncell                                   !< Number of cells
 integer, intent (in) :: nvar                                    !< Number of variables
 real(gtm_real),intent (inout):: down_diag(ncell)                !< Values of the coefficients below diagonal in matrix
 real(gtm_real),intent (inout):: center_diag(ncell)              !< Values of the coefficients at the diagonal in matrix
 real(gtm_real),intent (inout):: up_diag(ncell)                  !< Values of the coefficients above the diagonal in matrix
 real(gtm_real),intent (inout):: right_hand_side(ncell,nvar)     !< Values of the coefficients of right  hand side vector
 real(gtm_real), intent (in)  :: conc(ncell,nvar)                !< Concentration
 real(gtm_real), intent (in)  :: explicit_diffuse_op(ncell,nvar) !< Explicit diffusive operator 
 real(gtm_real), intent(in) :: conc_prev(ncell,nvar)             !< Cell centered concentration at old time
 real(gtm_real), intent(in) :: mass_prev(ncell,nvar)             !< Mass from old time and previous two steps
 real(gtm_real), intent(in) :: area_lo_prev(ncell)               !< Low side area at old time
 real(gtm_real), intent(in) :: area_hi_prev(ncell)               !< High side area at old time
 real(gtm_real), intent(in) :: disp_coef_lo_prev(ncell)          !< Low side constituent dispersion coef. at old time
 real(gtm_real), intent(in) :: disp_coef_hi_prev(ncell)          !< High side constituent dispersion coef. at old time
 real(gtm_real), intent(in) :: flow_lo(ncell)                    !< Low side flow at new time
 real(gtm_real), intent(in) :: flow_hi(ncell)                    !< High side flow at new time                                                     
 real(gtm_real), intent (in)  :: area (ncell)                    !< Cell centered area at new time 
 real(gtm_real), intent (in)  :: area_lo(ncell)                  !< Low side area at new time
 real(gtm_real), intent (in)  :: area_hi(ncell)                  !< High side area at new time 
 real(gtm_real), intent (in)  :: disp_coef_lo(ncell)             !< Low side constituent dispersion coef. at new time
 real(gtm_real), intent (in)  :: disp_coef_hi(ncell)             !< High side constituent dispersion coef. at new time
 real(gtm_real), intent (in)  :: time                            !< Current time
 real(gtm_real), intent (in)  :: theta_gtm                       !< Explicitness coefficient; 0 is explicit, 0.5 Crank-Nicolson, 1 full implicit  
 real(gtm_real), intent (in)  :: dx(ncell)                       !< Spatial step  
 real(gtm_real), intent (in)  :: dt                              !< Time step    
  !---local
 real(gtm_real) :: dt_by_dxsq(ncell)
 real(gtm_real) :: xloc
 real(gtm_real) :: bc_data(nvar)
 real(gtm_real) :: origin = zero !todo: HARDWIRE

 xloc = origin
 call diffusion_data_lo(bc_data,           &
                        xloc,              &
                        conc,              &
                        ncell,             &
                        nvar,              &
                        origin,            &
                        time,              &
                        dx,                &
                        dt)
 
 
 dt_by_dxsq = dt/(dx*dx)
! todo: one part of center diag is based on old time and other part new time
!       is this really true?
 center_diag(1)= center_diag(1)+theta_gtm*dt_by_dxsq(1)*(area_lo(1)*disp_coef_lo(1))                  
 right_hand_side(1,:) = right_hand_side(1,:)&
             + two*theta_gtm*dt_by_dxsq(1)*(area_lo(1)*disp_coef_lo(1))*bc_data



return
end subroutine

!> Matrix boundary condition for dirichlet, only operates on high end
subroutine dirichlet_diffusive_matrix_hi(center_diag ,        &
                                         up_diag,             &     
                                         down_diag,           &
                                         right_hand_side,     &
                                         explicit_diffuse_op, & 
                                         conc_prev,           &
                                         mass_prev,           &
                                         area_lo_prev,        &
                                         area_hi_prev,        &
                                         disp_coef_lo_prev,   &
                                         disp_coef_hi_prev,   & 
                                         conc,                &
                                         flow_lo,             &
                                         flow_hi,             & 
                                         area,                &
                                         area_lo,             &
                                         area_hi,             &          
                                         disp_coef_lo,        &
                                         disp_coef_hi,        &
                                         theta_gtm,           &
                                         ncell,               &
                                         time,                & 
                                         nvar,                & 
                                         dx,                  &
                                         dt)
                                              
 use gtm_precision
 implicit none
 !--- args
                               
 integer, intent (in) :: ncell                                   !< Number of cells
 integer, intent (in) :: nvar                                    !< Number of variables
 real(gtm_real),intent (inout):: down_diag(ncell)                !< Values of the coefficients below diagonal in matrix
 real(gtm_real),intent (inout):: center_diag(ncell)              !< Values of the coefficients at the diagonal in matrix
 real(gtm_real),intent (inout):: up_diag(ncell)                  !< Values of the coefficients above the diagonal in matrix
 real(gtm_real),intent (inout):: right_hand_side(ncell,nvar)     !< Values of the coefficients of right  hand side vector
 real(gtm_real), intent (in)  :: conc(ncell,nvar)                !< Concentration
 real(gtm_real), intent (in)  :: explicit_diffuse_op(ncell,nvar) !< Explicit diffusive operator
 real(gtm_real), intent(in) :: conc_prev(ncell,nvar)             !< Cell centered concentration at old time
 real(gtm_real), intent(in) :: mass_prev(ncell,nvar)             !< Mass from old time and previous two steps
 real(gtm_real), intent(in) :: area_lo_prev(ncell)               !< Low side area at old time
 real(gtm_real), intent(in) :: area_hi_prev(ncell)               !< High side area at old time
 real(gtm_real), intent(in) :: disp_coef_lo_prev(ncell)          !< Low side constituent dispersion coef. at old time
 real(gtm_real), intent(in) :: disp_coef_hi_prev(ncell)          !< High side constituent dispersion coef. at old time
 real(gtm_real), intent(in) :: flow_lo(ncell)                    !< Low side flow at new time
 real(gtm_real), intent(in) :: flow_hi(ncell)                    !< High side flow at new time   
 real(gtm_real), intent (in)  :: area (ncell)                    !< Cell centered area at new time 
 real(gtm_real), intent (in)  :: area_lo(ncell)                  !< Low side area at new time
 real(gtm_real), intent (in)  :: area_hi(ncell)                  !< High side area at new time 
 real(gtm_real), intent (in)  :: disp_coef_lo(ncell)             !< Low side constituent dispersion coef. at new time
 real(gtm_real), intent (in)  :: disp_coef_hi(ncell)             !< High side constituent dispersion coef. at new time
 real(gtm_real), intent (in)  :: time                            !< Current time
 real(gtm_real), intent (in)  :: theta_gtm                       !< Explicitness coefficient; 0 is explicit, 0.5 Crank-Nicolson, 1 full implicit  
 real(gtm_real), intent (in)  :: dx(ncell)                       !< Spatial step  
 real(gtm_real), intent (in)  :: dt                              !< Time step    
  !---local
 real(gtm_real) :: dt_by_dxsq(ncell)
 real(gtm_real) :: xloc
 real(gtm_real) :: bc_data(nvar)
 real(gtm_real) :: origin = zero !todo: HARDWIRE

 dt_by_dxsq = dt/(dx*dx)
 xloc = origin + sum(dx)
 call diffusion_data_hi(bc_data,           &
                        xloc,              &
                        conc,              &
                        ncell,             &
                        nvar,              &
                        origin,            &
                        time,              &
                        dx,                &
                        dt)
 
 ! todo: one part of center diag is based on old time and other part new time?
 ! todo: is this really true?
 center_diag(ncell)= center_diag(ncell)&
                    +  theta_gtm*dt_by_dxsq(ncell)*(area_hi(ncell)*disp_coef_hi(ncell))
 right_hand_side(ncell,:) = right_hand_side(ncell,:)&
                    + two*theta_gtm*dt_by_dxsq(ncell)*(area_hi(ncell)*disp_coef_hi(ncell))*bc_data

return
end subroutine


 !> Example advective flux that imposes Neumann boundaries on lo side of channel
 subroutine neumann_advective_flux_lo(flux_lo,    &
                                      flux_hi,    &
                                      conc_lo,    &
                                      conc_hi,    &
                                      flow_lo,    &
                                      flow_hi,    &
                                      ncell,      &
                                      nvar,       &
                                      time,       &
                                      dt,         &
                                      dx)
     
   use gtm_precision
   use error_handling
   implicit none
   !--- args          
   integer,intent(in)  :: ncell                            !< Number of cells
   integer,intent(in)  :: nvar                             !< Number of variables
   real(gtm_real),intent(inout) :: flux_lo(ncell,nvar)     !< Flux on lo side of cell, time centered
   real(gtm_real),intent(inout) :: flux_hi(ncell,nvar)     !< Flux on hi side of cell, time centered
   real(gtm_real), intent(in)   :: flow_lo(ncell)          !< Flow on lo side of cells centered in time
   real(gtm_real), intent(in)   :: flow_hi(ncell)          !< Flow on hi side of cells centered in time
   real(gtm_real), intent(in)   :: conc_lo(ncell,nvar)     !< Concentration extrapolated to lo face
   real(gtm_real), intent(in)   :: conc_hi(ncell,nvar)     !< Concentration extrapolated to hi face
   real(gtm_real), intent (in)  :: time                    !< Current time
   real(gtm_real), intent (in)  :: dx(ncell)               !< Spatial step  
   real(gtm_real), intent (in)  :: dt                      !< Time step     
   call gtm_fatal("Single channel advection neumann boundary not implemented")
   
   return
 end subroutine

 !> Example advective flux that imposes Neumann boundaries on hi side of channel
 subroutine neumann_advective_flux_hi(flux_lo,    &
                                      flux_hi,    &
                                      conc_lo,    &
                                      conc_hi,    &
                                      flow_lo,    &
                                      flow_hi,    &
                                      ncell,      &
                                      nvar,       &
                                      time,       &
                                      dt,         &
                                      dx)
     
   use gtm_precision
   use error_handling

   implicit none
  !--- args          
   integer,intent(in)  :: ncell                            !< Number of cells
   integer,intent(in)  :: nvar                             !< Number of variables
   real(gtm_real),intent(inout) :: flux_lo(ncell,nvar)     !< Flux on lo side of cell, time centered
   real(gtm_real),intent(inout) :: flux_hi(ncell,nvar)     !< Flux on hi side of cell, time centered
   real(gtm_real), intent(in)   :: flow_lo(ncell)          !< Flow on lo side of cells centered in time
   real(gtm_real), intent(in)   :: flow_hi(ncell)          !< Flow on hi side of cells centered in time
   real(gtm_real), intent(in)   :: conc_lo(ncell,nvar)     !< Concentration extrapolated to lo face
   real(gtm_real), intent(in)   :: conc_hi(ncell,nvar)     !< Concentration extrapolated to hi face
   real(gtm_real), intent (in)  :: time                    !< Current time
   real(gtm_real), intent (in)  :: dx(ncell)               !< Spatial step  
   real(gtm_real), intent (in)  :: dt                      !< Time step     

   call gtm_fatal("Single channel advection neumann boundary not implemented")
      
   return
 end subroutine


!> neumann boundary condition that sets only the lo side boundary
subroutine neumann_diffusive_flux_lo(diffusive_flux_lo, &
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
       
    use gtm_precision
    implicit none
    !--- args
    integer, intent(in)  :: ncell                                    !< Number of cells
    integer, intent(in)  :: nvar                                     !< Number of variables
    real(gtm_real), intent (inout):: diffusive_flux_lo(ncell,nvar)   !< Face flux, lo side
    real(gtm_real), intent (inout):: diffusive_flux_hi(ncell,nvar)   !< Face flux, hi side
    real(gtm_real), intent (in)   :: area_lo(ncell)                  !< Low side area centered at time
    real(gtm_real), intent (in)   :: area_hi(ncell)                  !< High side area centered at time
    real(gtm_real), intent (in)   :: time                            !< Time
    real(gtm_real), intent (in)   :: conc(ncell,nvar)                !< Concentration 
    real(gtm_real), intent (in)   :: disp_coef_lo(ncell)             !< Low side constituent dispersion coef.
    real(gtm_real), intent (in)   :: disp_coef_hi(ncell)             !< High side constituent dispersion coef.
    real(gtm_real), intent (in)   :: dt                              !< Spatial step           
    real(gtm_real), intent (in)   :: dx(ncell)                       !< Time step     
    !---local
    real(gtm_real) :: dt_by_dxsq(ncell)
    real(gtm_real) :: xloc
    real(gtm_real) :: bc_data(nvar)
    real(gtm_real) :: origin = zero !todo: HARDWIRE

    xloc = origin
    call diffusion_data_lo(bc_data,           &
                           xloc,              &
                           conc,              &
                           ncell,             &
                           nvar,              &
                           origin,            &
                           time,              &
                           dx,                &
                           dt)    
    
        
    diffusive_flux_lo(1,:) = -area_lo(1)*disp_coef_lo(1)*bc_data
    
return
end subroutine

!> neumann boundary condition that sets only the hi side boundary
subroutine neumann_diffusive_flux_hi(diffusive_flux_lo, &
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
       
    use gtm_precision
    implicit none
    !--- args
    integer, intent(in)  :: ncell                                    !< Number of cells
    integer, intent(in)  :: nvar                                     !< Number of variables
    real(gtm_real), intent (inout):: diffusive_flux_lo(ncell,nvar)   !< Face flux, lo side
    real(gtm_real), intent (inout):: diffusive_flux_hi(ncell,nvar)   !< Face flux, hi side
    real(gtm_real), intent (in)   :: area_lo(ncell)                  !< Low side area centered at time
    real(gtm_real), intent (in)   :: area_hi(ncell)                  !< High side area centered at time
    real(gtm_real), intent (in)   :: time                            !< Time
    real(gtm_real), intent (in)   :: conc(ncell,nvar)                !< Concentration 
    real(gtm_real), intent (in)   :: disp_coef_lo (ncell)            !< Low side constituent dispersion coef.
    real(gtm_real), intent (in)   :: disp_coef_hi (ncell)            !< High side constituent dispersion coef.
    real(gtm_real), intent (in)   :: dt                              !< Spatial step  
    real(gtm_real), intent (in)   :: dx(ncell)                       !< Time step     

    !---local
    real(gtm_real) :: dt_by_dxsq(ncell)
    real(gtm_real) :: xloc
    real(gtm_real) :: bc_data(nvar)
    real(gtm_real) :: origin = zero !todo: HARDWIRE

    dt_by_dxsq = dt/(dx*dx)
    xloc = origin + sum(dx)
 
    call diffusion_data_hi(bc_data,           &
                           xloc,              &
                           conc,              &
                           ncell,             &
                           nvar,              &
                           origin,            &
                           time,              &
                           dx,                &
                           dt)
    
     diffusive_flux_hi(ncell,:) = -area_hi(ncell)*disp_coef_hi(ncell)*bc_data

     return
end subroutine


!> Matrix boundary condition for neumann, only operates on lo end
subroutine neumann_diffusive_matrix_lo(center_diag ,        &
                                         up_diag,             &     
                                         down_diag,           &
                                         right_hand_side,     &
                                         explicit_diffuse_op, & 
                                         conc_prev,           &
                                         mass_prev,           &
                                         area_lo_prev,        &
                                         area_hi_prev,        &
                                         disp_coef_lo_prev,   &
                                         disp_coef_hi_prev,   & 
                                         conc,                &
                                         flow_lo,             &
                                         flow_hi,             & 
                                         area,                &
                                         area_lo,             &
                                         area_hi,             &          
                                         disp_coef_lo,        &
                                         disp_coef_hi,        &
                                         theta_gtm,           &
                                         ncell,               &
                                         time,                & 
                                         nvar,                & 
                                         dx,                  &
                                         dt)
                                              
 use gtm_precision
 implicit none
 !--- args
                               
 integer, intent (in) :: ncell                                   !< Number of cells
 integer, intent (in) :: nvar                                    !< Number of variables
 real(gtm_real),intent (inout):: down_diag(ncell)                !< Values of the coefficients below diagonal in matrix
 real(gtm_real),intent (inout):: center_diag(ncell)              !< Values of the coefficients at the diagonal in matrix
 real(gtm_real),intent (inout):: up_diag(ncell)                  !< Values of the coefficients above the diagonal in matrix
 real(gtm_real),intent (inout):: right_hand_side(ncell,nvar)     !< Values of the coefficients of right  hand side vector
 real(gtm_real), intent (in)  :: conc(ncell,nvar)                !< Concentration
 real(gtm_real), intent (in)  :: explicit_diffuse_op(ncell,nvar) !< Explicit diffusive operator 
 real(gtm_real), intent(in) :: conc_prev(ncell,nvar)             !< Cell centered concentration at old time
 real(gtm_real), intent(in) :: mass_prev(ncell,nvar)             !< Mass from old time and previous two steps
 real(gtm_real), intent(in) :: area_lo_prev(ncell)               !< Low side area at old time
 real(gtm_real), intent(in) :: area_hi_prev(ncell)               !< High side area at old time
 real(gtm_real), intent(in) :: disp_coef_lo_prev(ncell)          !< Low side constituent dispersion coef. at old time
 real(gtm_real), intent(in) :: disp_coef_hi_prev(ncell)          !< High side constituent dispersion coef. at old time
 real(gtm_real), intent(in) :: flow_lo(ncell)                    !< Low side flow at new time
 real(gtm_real), intent(in) :: flow_hi(ncell)                    !< High side flow at new time    
 real(gtm_real), intent (in)  :: area (ncell)                    !< Cell centered area at new time 
 real(gtm_real), intent (in)  :: area_lo(ncell)                  !< Low side area at new time
 real(gtm_real), intent (in)  :: area_hi(ncell)                  !< High side area at new time 
 real(gtm_real), intent (in)  :: disp_coef_lo(ncell)             !< Low side constituent dispersion coef. at new time
 real(gtm_real), intent (in)  :: disp_coef_hi(ncell)             !< High side constituent dispersion coef. at new time
 real(gtm_real), intent (in)  :: time                            !< Current time
 real(gtm_real), intent (in)  :: theta_gtm                       !< Explicitness coefficient; 0 is explicit, 0.5 Crank-Nicolson, 1 full implicit  
 real(gtm_real), intent (in)  :: dx(ncell)                       !< Spatial step  
 real(gtm_real), intent (in)  :: dt                              !< Time step      

  !---local
 real(gtm_real) :: dt_by_dxsq(ncell)
 real(gtm_real) :: xloc
 real(gtm_real) :: bc_data(nvar)
 real(gtm_real) :: origin = zero !todo: HARDWIRE
 real(gtm_real) :: flux(nvar)
 dt_by_dxsq = dt/(dx*dx) 
 xloc = origin
 call diffusion_data_lo(bc_data,           &
                        xloc,              &
                        conc,              &
                        ncell,             &
                        nvar,              &
                        origin,            &
                        time,              &
                        dx,                &
                        dt)
   ! todo: there may be an issue of area vs area previous    
    
    flux(:) = -area_lo(1)*disp_coef_lo(1)*bc_data
    
    center_diag(1)= area(1)+ theta_gtm*dt_by_dxsq(1)* area_hi(1)*disp_coef_hi(1)  
    right_hand_side(1,:) = right_hand_side(1,:)+ theta_gtm*(dt/dx(1))*flux(:)
     
    return
end subroutine


!> Matrix boundary condition for neumann, only operates on high end
subroutine neumann_diffusive_matrix_hi(center_diag ,        &
                                         up_diag,             &     
                                         down_diag,           &
                                         right_hand_side,     &
                                         explicit_diffuse_op, & 
                                         conc_prev,           &
                                         mass_prev,           &
                                         area_lo_prev,        &
                                         area_hi_prev,        &
                                         disp_coef_lo_prev,   &
                                         disp_coef_hi_prev,   & 
                                         conc,                &
                                         flow_lo,             &
                                         flow_hi,             & 
                                         area,                &
                                         area_lo,             &
                                         area_hi,             &          
                                         disp_coef_lo,        &
                                         disp_coef_hi,        &
                                         theta_gtm,           &
                                         ncell,               &
                                         time,                & 
                                         nvar,                & 
                                         dx,                  &
                                         dt)
                                              
 use gtm_precision
 implicit none
 !--- args
                               
 integer, intent (in) :: ncell                                    !< Number of cells
 integer, intent (in) :: nvar                                     !< Number of variables
 real(gtm_real),intent (inout):: down_diag(ncell)                 !< Values of the coefficients below diagonal in matrix
 real(gtm_real),intent (inout):: center_diag(ncell)               !< Values of the coefficients at the diagonal in matrix
 real(gtm_real),intent (inout):: up_diag(ncell)                   !< Values of the coefficients above the diagonal in matrix
 real(gtm_real),intent (inout):: right_hand_side(ncell,nvar)      !< Values of the coefficients of right  hand side vector
 real(gtm_real), intent (in)  :: conc(ncell,nvar)                 !< Concentration
 real(gtm_real), intent (in)  :: explicit_diffuse_op(ncell,nvar)  !< Explicit diffusive operator 
 real(gtm_real), intent(in) :: conc_prev(ncell,nvar)             !< Cell centered concentration at old time
 real(gtm_real), intent(in) :: mass_prev(ncell,nvar)             !< Mass from old time and previous two steps
 real(gtm_real), intent(in) :: area_lo_prev(ncell)               !< Low side area at old time
 real(gtm_real), intent(in) :: area_hi_prev(ncell)               !< High side area at old time
 real(gtm_real), intent(in) :: disp_coef_lo_prev(ncell)          !< Low side constituent dispersion coef. at old time
 real(gtm_real), intent(in) :: disp_coef_hi_prev(ncell)          !< High side constituent dispersion coef. at old time
 real(gtm_real), intent(in) :: flow_lo(ncell)                    !< Low side flow at new time
 real(gtm_real), intent(in) :: flow_hi(ncell)                    !< High side flow at new time    
 real(gtm_real), intent (in)  :: area (ncell)                     !< Cell centered area at new time 
 real(gtm_real), intent (in)  :: area_lo(ncell)                   !< Low side area at new time
 real(gtm_real), intent (in)  :: area_hi(ncell)                   !< High side area at new time 
 real(gtm_real), intent (in)  :: disp_coef_lo(ncell)              !< Low side constituent dispersion coef. at new time
 real(gtm_real), intent (in)  :: disp_coef_hi(ncell)              !< High side constituent dispersion coef. at new time
 real(gtm_real), intent (in)  :: time                             !< Current time
 real(gtm_real), intent (in)  :: theta_gtm                        !< Explicitness coefficient; 0 is explicit, 0.5 Crank-Nicolson, 1 full implicit  
 real(gtm_real), intent (in)  :: dx(ncell)                        !< Spatial step  
 real(gtm_real), intent (in)  :: dt                               !< Time step    

  !---local
 real(gtm_real) :: dt_by_dxsq(ncell)
 real(gtm_real) :: xloc
 real(gtm_real) :: bc_data(nvar)
 real(gtm_real) :: origin = zero !todo: HARDWIRE
 real(gtm_real) :: flux(nvar)
 dt_by_dxsq = dt/(dx*dx) 
 xloc = origin + sum(dx)
 call diffusion_data_hi(bc_data,           &
                        xloc,              &
                        conc,              &
                        ncell,             &
                        nvar,              &
                        origin,            &
                        time,              &
                        dx,                &
                        dt)
 
     ! todo: there may be an issue of area vs area previous    
    flux(:) = -area_hi(ncell)*disp_coef_hi(ncell)*bc_data
       
    center_diag(ncell)= area(ncell)+ theta_gtm*dt_by_dxsq(ncell)* area_lo(ncell)*disp_coef_lo(1)
    right_hand_side(ncell,:)= right_hand_side(ncell,:) - theta_gtm*(dt/dx(ncell))*flux(:)

    return
end subroutine

end module