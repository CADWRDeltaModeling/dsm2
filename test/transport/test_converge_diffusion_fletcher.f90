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
!> Test of transport diffusion convergence test for a single channel
!>@ingroup test_transport
module test_diffusion_fletcher

contains

!> Subroutine that checks the error convergence ratio for diffusion routine 
! todo: the printout of the result of this test needs to shift 0.1 forward 
! (now the printout starts at x=0.0 but it should starts at x=0.1) 
subroutine test_diffusion_convergence_fletcher(verbose)
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
use common_variables, only : dsm2_network_t

implicit none

!--- Problem variables

integer, parameter  :: nstep_base = 64                 !< Number of time steps in the finest grid
integer, parameter  :: nx_base = 64                    !< Number of cells in the finest grid 

integer :: icoarse = 0
integer :: nstep
integer :: nx
integer, parameter  :: nconc = 2                       !< Number of constituents
real(gtm_real), parameter :: domain_length = 0.9d0     !< Domain length
real(gtm_real), parameter :: origin = 0.1d0            !< Left side of channel
real(gtm_real), parameter :: total_time  = one         !< Total time 
real(gtm_real), parameter :: disp_coef   = 2.1d0       !< Dispersion coefficent (m2/sec)
real(gtm_real) :: theta = half                         !< Explicitness coefficient; 0 is explicit, 0.5 Crank-Nicolson, 1 full implicit  
real(gtm_real),allocatable :: disp_coef_lo(:)          !< Low side constituent dispersion coef. at new time
real(gtm_real),allocatable :: disp_coef_hi(:)          !< High side constituent dispersion coef. at new time
real(gtm_real),allocatable :: disp_coef_lo_prev(:)     !< Low side constituent dispersion coef. at old time
real(gtm_real),allocatable :: disp_coef_hi_prev(:)     !< High side constituent dispersion coef. at old time
logical, optional :: verbose                           !< Detailed printout flag

real(gtm_real) :: fine_initial_condition(nx_base,nconc)!< initial condition f concentration at finest resolution
real(gtm_real) :: fine_solution(nx_base,nconc)         !< reference solution at finest resolution

real(gtm_real) :: dt                                            !< Time step    
real(gtm_real) :: dx(nx_base)                                   !< Spacial step
real(gtm_real), parameter :: constant_area = 100.0d0            !< Constant Area
real(gtm_real), parameter :: start_time = zero                  !< Start time 
!NOTE: Initial condition depends on this
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

integer, parameter :: n_dsm2_node = 2
type(dsm2_network_t) :: dsm2_network_type(2)
real(gtm_real) :: node_conc_val(n_dsm2_node,nconc)

character(LEN=64):: label = 'test_diffusion_fletcher_dirichlet'

acceptance_ratio = [four, four, four]

call set_single_channel(dsm2_network_type, nx_base)
node_conc_val = one

dx = domain_length/dble(nx_base)

call set_uniform_flow_area(zero,constant_area)
uniform_hydro => uniform_flow_area
adjust_gradient => adjust_differences_single_channel
boundary_diffusion_matrix  => dirichlet_test_diffusion_matrix
boundary_diffusion_flux    => dirichlet_test_diffusive_flux
advection_boundary_flux    => zero_advective_flux
compute_source             => no_source
call set_constant_dispersion(disp_coef)


call initial_final_solution(fine_initial_condition,&
                            fine_solution,         &
                            start_time,            &
                            disp_coef,             &
                            total_time,            &
                            origin,                &
                            domain_length,         &
                            nx_base,               &
                            nconc)
                                
!=====Dirichlet
label = 'test_diffusion_fletcher_dirichlet'
call test_convergence(label,                             &
                      uniform_hydro,                     &
                      adjust_differences_single_channel, &
                      zero_advective_flux,               &
                      dirichlet_test_diffusive_flux,     &
                      dirichlet_test_diffusion_matrix ,  &
                      no_source,                         &
                      domain_length,                     &
                      total_time,                        &
                      start_time,                        &
                      fine_initial_condition,            &
                      fine_solution,                     &            
                      nstep_base,                        &
                      nx_base,                           &
                      nconc,                             &      
                      dx,                                &
                      n_dsm2_node,                       &
                      dsm2_network_type,                 &
                      node_conc_val,                     &
                      verbose,.true.,                    &
                      acceptance_ratio)


!=====Neumann 
label = 'test_diffusion_fletcher_neumann'
boundary_diffusion_matrix  => n_d_test_diffusion_matrix
boundary_diffusion_flux    => n_d_test_diffusive_flux

call initial_final_solution(fine_initial_condition,&
                            fine_solution,         &
                            start_time,            &
                            disp_coef,             &
                            total_time,            &
                            origin,                &
                            domain_length,         &
                            nx_base,               &
                            nconc)

call test_convergence(label,                             &
                      uniform_hydro,                     &
                      adjust_differences_single_channel, &
                      zero_advective_flux,               &
                      n_d_test_diffusive_flux,           &
                      n_d_test_diffusion_matrix ,        &
                      no_source,                         &
                      domain_length,                     &
                      total_time,                        &
                      start_time,                        &
                      fine_initial_condition,            &
                      fine_solution,                     &            
                      nstep_base,                        &
                      nx_base,                           &
                      nconc,                             &
                      dx,                                &
                      n_dsm2_node,                       &
                      dsm2_network_type,                 &
                      node_conc_val,                     &
                      verbose,.true.,                    &
                      acceptance_ratio)

return
end subroutine



!> produce fine initial condition and reference solution 
subroutine initial_final_solution(fine_initial_conc,     &
                                  fine_solution,         &
                                  init_time,             &
                                  dispersion_coef,       &
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

fine_initial_conc(:,1) = two*xposition(:) + four*dcos(half*pi*xposition(:))*dexp(-dispersion_coef*current_time*pi*pi/four )
fine_initial_conc(:,2) = fine_initial_conc(:,1)

current_time = init_time + total_time
fine_solution(:,1) = two*xposition(:) + four*dcos(half*pi*xposition(:))*dexp(-dispersion_coef*current_time*pi*pi/four )
fine_solution(:,2) = fine_solution(:,1)

return
end subroutine


end module