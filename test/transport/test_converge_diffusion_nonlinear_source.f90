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

!> todo: write tests for diffusion subroutine in case of Neumann BC at the left and Dirichlet BC at the right side 
!> Test of transport convergence diffusion subjected to cubic decay for a single channel
!>@ingroup test_transport
module test_diffusion_nonlinear_decay

contains

!> Subroutine that checks the error convergence ratio for diffusion reaction routines 

subroutine test_diffusion_cubic_decay(verbose)
use test_convergence_transport
use gtm_precision
use state_variables
use primitive_variable_conversion
use boundary_diffusion
use boundary_advection
use hydro_uniform_flow
use gradient_adjust
use dispersion_coefficient
use gaussian_init_boundary_condition
use diffusion
use hydro_data
use source_sink
use test_utility
use error_handling
use fruit
use gtm_logging

implicit none

!--- Problem variables

integer, parameter  :: nstep_base = 64                 !< Number of time steps in the finest grid
integer, parameter  :: nx_base = 64                    !< Number of cells in the finest grid 
integer :: icoarse = 0
integer :: nstep
integer :: nx
integer, parameter  :: nconc = 2                       !< Number of constituents
real(gtm_real), parameter :: domain_length =one        !< Domain length
real(gtm_real), parameter :: origin = zero             !< Left side of channel
real(gtm_real), parameter :: total_time  = one         !< Total time  (must be one)
real(gtm_real), parameter :: disp_coef   = half*half   !< Dispersion coefficent (m2/sec)
!! **** Note: Lambda is hardwiered in diffusion_cubic_decay_source  subroutine
real(gtm_real), parameter :: lambda = three/ten/ten    !< Constant coefficient of cubic decay
real(gtm_real)             :: theta = half             !< Explicitness coefficient; 0 is explicit, 0.5 Crank-Nicolson, 1 full implicit  
real(gtm_real),allocatable :: disp_coef_lo(:)          !< Low side constituent dispersion coef. at new time
real(gtm_real),allocatable :: disp_coef_hi(:)          !< High side constituent dispersion coef. at new time
real(gtm_real),allocatable :: disp_coef_lo_prev(:)     !< Low side constituent dispersion coef. at old time
real(gtm_real),allocatable :: disp_coef_hi_prev(:)     !< High side constituent dispersion coef. at old time

logical, optional :: verbose                           !< Detailed printout flag

real(gtm_real) :: fine_initial_condition(nx_base,nconc)!< initial condition f concentration at finest resolution
real(gtm_real) :: fine_solution(nx_base,nconc)         !< reference solution at finest resolution
real(gtm_real) :: dt                                            !< Time step    
real(gtm_real) :: dx(nx_base)                                   !< Spacial step
real(gtm_real), parameter :: constant_area = three              !< Constant Area
real(gtm_real), parameter :: start_time = zero                  !< Start time 
real(gtm_real), parameter :: end_time = start_time + total_time !< End time
real(gtm_real) :: time                                          !< Current time
procedure(hydro_data_if), pointer :: uniform_hydro => null()

integer :: itime                                                !< Counter on time
integer :: icell ! debug only -- remove later
!------
integer, parameter :: coarsen_factor = 2      ! coarsening factor used for convergence test
integer :: coarsening
integer, parameter :: nrefine = 3
integer :: which_cell(nrefine)
! the cell in which the worst error occures
real(gtm_real),allocatable :: reference(:)
real(gtm_real) :: norm_error(3,nrefine)
real(gtm_real) :: acceptance_ratio(3)                        !< Acceptance ratio
 
character(LEN=64):: label = 'test_diffusion_cubic_decay'

acceptance_ratio = [three, three, three]      ! relax the standard for diffusion cubic decay

call set_uniform_flow_area(zero,constant_area)
uniform_hydro => uniform_flow_area
adjust_gradient => adjust_differences_single_channel

boundary_diffusion_matrix  => dirichlet_diffusion_nonlinear_matrix
boundary_diffusion_flux    => dirichlet_diffusive_nonlinear_flux
advection_boundary_flux    => zero_advective_flux
compute_source             => diffusion_cubic_decay_source
call set_constant_dispersion(disp_coef)

dx = domain_length/dble(nx_base)

call initial_final_solution(fine_initial_condition,&
                            fine_solution,         &
                            start_time,            &
                            disp_coef,             &
                            lambda,                &
                            total_time,            &
                            origin,                &
                            domain_length,         &
                            nx_base,               &
                            nconc)
                                
!=====Dirichlet
label = 'test_diffusion_cubic_decay'
call test_convergence(label,                             &
                      uniform_hydro,                     &
                      adjust_differences_single_channel, &
                      zero_advective_flux,               &
                      dirichlet_test_diffusive_flux,     &
                      dirichlet_test_diffusion_matrix ,  &
                      diffusion_cubic_decay_source,      &
                      domain_length,                     &
                      total_time,                        &
                      start_time,                        &
                      fine_initial_condition,            &
                      fine_solution,                     &            
                      nstep_base,                        &
                      nx_base,                           &
                      nconc,                             &
                      dx,                                &
                      verbose,.true.,                    &
                      acceptance_ratio)

return
end subroutine


!> produce fine initial condition and reference solution 
subroutine initial_final_solution(fine_initial_conc,     &
                                  fine_solution,         &
                                  init_time,             &
                                  dispersion_coef,       &
                                  lambda,                &
                                  total_time,            &
                                  origin,                &
                                  domain_length,         &
                                  nx_base,               &
                                  nconc)
                                  
use gaussian_init_boundary_condition

implicit none
integer, intent(in) :: nx_base                                   !< Number of cells in the finest grid 
integer, intent(in) :: nconc                                     !< Number of constituents
real(gtm_real),intent(out) :: fine_initial_conc(nx_base,nconc)   !< Initial condition at finest resolution
real(gtm_real),intent(out) :: fine_solution(nx_base,nconc)       !< Reference solution at finest resolution
real(gtm_real),intent(in)  :: init_time                          !< Initial time
real(gtm_real),intent(in)  :: dispersion_coef                    !< Dispersion coefficient (m2/s)
real(gtm_real),intent(in)  :: lambda                             !< Constant of cubic decay rate  
real(gtm_real),intent(in)  :: total_time                         !< Total time
real(gtm_real),intent(in)  :: origin                             !< Origin
real(gtm_real),intent(in)  :: domain_length                      !< Domain length

!--local
integer :: ivar
integer :: icell

real(gtm_real) :: dx
real(gtm_real) :: xposition(nx_base)
real(gtm_real) :: current_time

dx = domain_length/dble(nx_base)

do icell = 1,nx_base
  xposition(icell) = dx*(dble(icell)-half)+ origin
end do

current_time = init_time

fine_initial_conc(:,1) = dsqrt(two*dispersion_coef/lambda)*two*xposition(:)/(one+six*dispersion_coef*current_time+xposition(:)*xposition(:))
fine_initial_conc(:,2) = fine_initial_conc(:,1)

current_time = init_time + total_time
fine_solution(:,1) = dsqrt(two*dispersion_coef/lambda)*two*xposition(:)/(one+six*dispersion_coef*current_time+xposition(:)*xposition(:))
fine_solution(:,2) = fine_solution(:,1)

return
end subroutine

subroutine diffusion_cubic_decay_source(source, & 
                                        conc,   &
                                        area,   &
                                        flow,   &
                                        ncell,  &
                                        nvar,   &
                                        time)
                                         
 use gtm_precision 
 use error_handling
           
implicit none
 !--- args
 integer,intent(in)  :: ncell                        !< Number of cells
 integer,intent(in)  :: nvar                         !< Number of variables
 real(gtm_real),intent(inout) :: source(ncell,nvar)  !< cell centered source 
 real(gtm_real),intent(in)    :: conc(ncell,nvar)    !< Concentration
 real(gtm_real),intent(in)    :: area(ncell)         !< area at source     
 real(gtm_real),intent(in)    :: flow(ncell)         !< flow at source location
 real(gtm_real),intent(in)    :: time                !< time
 !--- local
 integer :: ivar       
 
 !! **** Note: Lambda is hardwiered in diffusion_cubic_decay_source  subroutine
real(gtm_real), parameter :: lambda = three/ten/ten    !< Constant coefficient of cubic decay
 
 do ivar = 1,nvar
  source(:,ivar) = -lambda*conc(:,ivar)*conc(:,ivar)*conc(:,ivar)
end do
     
return
end subroutine

!> Diffusion matrix values for nonlinear decay test imposes Dirichlet boundaries at
!> both ends of the channel. 
subroutine dirichlet_diffusion_nonlinear_matrix(center_diag ,       &
                                                up_diag,            &     
                                                down_diag,          &
                                                right_hand_side,    &
                                                conc,               & 
                                                explicit_diffuse_op,&
                                                area,               &
                                                area_lo,            &
                                                area_hi,            &          
                                                disp_coef_lo,       &
                                                disp_coef_hi,       &
                                                theta,              &
                                                ncell,              &
                                                time,               & 
                                                nvar,               & 
                                                dx,                 &
                                                dt)
use gtm_precision
implicit none
 !--- args
                               
integer, intent (in) :: ncell                                               !< Number of cells
integer, intent (in) :: nvar                                                !< Number of variables
real(gtm_real),intent (inout):: down_diag(ncell,nvar)                       !< Values of the coefficients below diagonal in matrix
real(gtm_real),intent (inout):: center_diag(ncell,nvar)                     !< Values of the coefficients at the diagonal in matrix
real(gtm_real),intent (inout):: up_diag(ncell,nvar)                         !< Values of the coefficients above the diagonal in matrix
real(gtm_real),intent (inout):: right_hand_side(ncell,nvar)                 !< Values of the coefficients of right hand side vector
real(gtm_real), intent (in)  :: conc(ncell,nvar)                            !< Concentration 
real(gtm_real), intent (in)  :: explicit_diffuse_op(ncell,nvar)             !< Explicit diffusive operator
real(gtm_real), intent (in)  :: area (ncell)                                !< Cell centered area at new time 
real(gtm_real), intent (in)  :: area_lo(ncell)                              !< Low side area at new time
real(gtm_real), intent (in)  :: area_hi(ncell)                              !< High side area at new time 
real(gtm_real), intent (in)  :: disp_coef_lo(ncell)                         !< Low side constituent dispersion coef. at new time
real(gtm_real), intent (in)  :: disp_coef_hi(ncell)                         !< High side constituent dispersion coef. at new time
real(gtm_real), intent (in)  :: time                                        !< Current time
real(gtm_real), intent (in)  :: theta                                       !< Explicitness coefficient; 0 is explicit, 0.5 Crank-Nicolson, 1 full implicit  
real(gtm_real), intent (in)  :: dx(ncell)                                   !< Spatial step  
real(gtm_real), intent (in)  :: dt                                          !< Time step     

!---local
real(gtm_real) :: dt_by_dxsq(ncell)
real(gtm_real) :: xstart
real(gtm_real) :: xend  
real(gtm_real) :: conc_start(nvar)
real(gtm_real) :: conc_end(nvar)
real(gtm_real) :: disp_coef
!! **** Note: Lambda is hardwiered in diffusion_cubic_decay_source  subroutine
real(gtm_real), parameter :: lambda = three/ten/ten    !< Constant coefficient of cubic decay

!NOTE: disp coef and Lambda are hardwiered here

disp_coef = half*(disp_coef_lo(ncell)+disp_coef_hi(ncell))

dt_by_dxsq = dt/(dx*dx)

! here time is new time and area and Ks for updating rhs are for time stap n+1
conc_end = dsqrt(two*disp_coef/lambda)/(one+three*disp_coef*time)
conc_start = zero
! todo: one part of center diag is based on old time and other part new time
center_diag(1,:)=  center_diag(1,:) &
                      + theta*dt_by_dxsq(1)*(area_lo(1)*disp_coef_lo(1))                  
right_hand_side(1,:) = right_hand_side(1,:)&
            + two * theta*dt_by_dxsq(1)*(area_lo(1)*disp_coef_lo(1))*conc_start
  
center_diag(ncell,:)= center_diag(ncell,:)&
                       +  theta*dt_by_dxsq(ncell)*(area_hi(ncell)*disp_coef_hi(ncell))
right_hand_side(ncell,:) = right_hand_side(ncell,:)&
           + two * theta*dt_by_dxsq(ncell)*(area_hi(ncell)*disp_coef_hi(ncell))*conc_end


   return
 end subroutine
 
!> Diffusive flux that imposes Dirichlet boundaries at
!> both ends of the channel for nonlinear decay test.  
subroutine dirichlet_diffusive_nonlinear_flux(diffusive_flux_lo, &
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
    integer, intent(in)  :: ncell                                   !< Number of cells
    integer, intent(in)  :: nvar                                    !< Number of variables
    real(gtm_real), intent (inout):: diffusive_flux_lo(ncell,nvar)  !< Face flux, lo side
    real(gtm_real), intent (inout):: diffusive_flux_hi(ncell,nvar)  !< Face flux, hi side
    real(gtm_real), intent (in)   :: area_lo(ncell)                 !< Low side area centered at time
    real(gtm_real), intent (in)   :: area_hi(ncell)                 !< High side area centered at time
    real(gtm_real), intent (in)   :: time                           !< Time
    real(gtm_real), intent (in)   :: conc(ncell,nvar)               !< Concentration 
    real(gtm_real), intent (in)   :: disp_coef_lo (ncell)           !< Low side constituent dispersion coef.
    real(gtm_real), intent (in)   :: disp_coef_hi (ncell)           !< High side constituent dispersion coef.
    real(gtm_real), intent (in)   :: dt                             !< Time step  
    real(gtm_real), intent (in)   :: dx(ncell)                      !< Spatial step
    !--local
    real(gtm_real) :: conc_start(nvar)
    real(gtm_real) :: conc_end(nvar) 
    real(gtm_real) :: xstart = zero
    real(gtm_real) :: disp_coef

    !! **** Note: Lambda is hardwiered in diffusion_cubic_decay_source  subroutine
    real(gtm_real), parameter :: lambda = three/ten/ten    !< Constant coefficient of cubic decay

    !NOTE: disp coef and Lambda are hardwiered here

    disp_coef = half*(disp_coef_lo(ncell)+disp_coef_hi(ncell))

    conc_end = dsqrt(two*disp_coef/lambda)/(one+three*disp_coef*time)
    conc_start = zero

    ! todo: check convergence for second order boundary fitting 
    ! todo: this area also must be area_prev  
    diffusive_flux_lo(1,:)=-two*area_lo(1)*disp_coef_lo(1)*(conc(1,:)-conc_start(:))/dx
 
    diffusive_flux_hi(ncell,:)=-two*area_hi(ncell)*disp_coef_hi(ncell)*(conc_end(:)-conc(ncell,:))/dx
        
    return
 end subroutine

end module