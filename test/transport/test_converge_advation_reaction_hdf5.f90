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

!> Testing advection of mass which is subjected to hydro boundary
!>@ingroup test_transport

module test_advection_reaction_hdf5

    use gtm_precision
    use common_variables
    integer, parameter  :: nconc = 2                                !< Number of constituents
    integer, parameter  :: nstep_base = 3                       !< Number of time steps in finer discritization
    integer, parameter  :: nx_base    = 4                      !< Number of spatial discritization in finer mesh 
    real(gtm_real),parameter :: origin = zero                     !< Left hand side of the channel
    real(gtm_real),parameter :: domain_length = 204800.d0/two     !< Domain Length in meter
    real(gtm_real),parameter :: amplitude = half                  !< Tidal amplitude in meter    
    real(gtm_real),parameter :: gravity = 9.80d0                  !< Gravitational acceleration in m/s^2
    real(gtm_real),parameter :: depth = 16.0d0                    !< Channel depth in meter
    real(gtm_real),parameter :: sec_per_hr = 60.d0*60.d0          !< Convert factor of hour to second 
    real(gtm_real),parameter :: m2_period = 12.4d0*sec_per_hr     !< M2 tidal period 
    real(gtm_real),parameter :: freq=two*pi/m2_period             !< Frequency of tidal oscillation
    real(gtm_real),parameter :: dye_length = domain_length/six    !< Length of cosine distribution of mass
    real(gtm_real),parameter :: dye_center = domain_length/two    !< Center of cosine distribution of mass
    real(gtm_real),parameter :: ic_gaussian_sd = domain_length/32.d0   !< Standard deviation of initial values 
    real(gtm_real),parameter :: ic_center = domain_length/two     !< Center of initial condition
    real(gtm_real),parameter :: total_time = m2_period            !< Total time of the test
    real(gtm_real),parameter :: start_time = zero                 !< Starts at zero
    real(gtm_real),parameter :: const_tidal_decay_rate = 1.552749d-5 !< Decay rate (set in the way it reduces half of the mass after one tidal period)

    contains

!todo: this test could be much less weird-looking if the tidal excursion moved toward the domain
!      rather than into the domain.
!      the tidal range is very short, so the discretization of the plume is fairly coarse despite the apparently
!      coarse discretization
!todo: to have an approximation in mind the tide hight is about one meter in Antioch where the water depth is ?? 


!> Tests the convergence of error rate in advection of mass which is exposed a tidal boundary 
subroutine test_hydro_advection_reaction(verbose)

use hydro_data
use gtm_network
use boundary_advection
use boundary_diffusion
use gaussian_init_boundary_condition
use source_sink
use test_convergence_transport
use diffusion
use dispersion_coefficient
implicit none
procedure(hydro_data_if), pointer :: gtm_hydro            !< The pointer points to DSM2 hydro flow data
logical :: verbose                                        !< Falg for showing the detail on the screen
logical :: detail_printout=.true.                         !< Flag for printing out the details
real(gtm_real) :: fine_initial_condition(nx_base*n_segm,nconc)   !< initial condition at finest resolution
real(gtm_real) :: fine_solution(nx_base*n_segm,nconc)            !< reference solution at finest resolution
real(gtm_real) :: solution_center = ic_center             !< Center of final solution 
real(gtm_real) :: solution_gaussian_sd = ic_gaussian_sd   !< Standard deviation of final values
real(gtm_real) :: tidal_ar_decay_rate                     !< Tidal decay rate
character(LEN=64) :: label                                !< Test name label
real(gtm_real) :: acceptance_ratio(3)                     !< Acceptance ratio
integer :: ncell 
acceptance_ratio = [four, four, four]

call allocate_network_tmp()
call interp_network(nx_base, nstep_base, 2) 
ncell = nx_base*n_segm
gtm_hydro => gtm_flow_area
! do not remove it 
!tidal_hydro=> tidal_flow_cell_average ! this flow generator is NOT mass conservative but it is cell averaged

advection_boundary_flux => zero_advective_flux !todo: move this so it isn't hardwired
boundary_diffusion_flux => no_diffusion_flux
boundary_diffusion_matrix => no_diffusion_matrix

call set_constant_dispersion(zero)

tidal_ar_decay_rate = zero
compute_source => no_source

label = 'advection_gtm_gaussian' 

! load the initial values and reference final values to feed the test routine
call initial_fine_solution_constant(fine_initial_condition, &
                                    fine_solution,          &
                                    ncell,                &
                                    nconc,                  &
                                    one)
!call initial_fine_solution_tidal_gaussian(fine_initial_condition, &
!                                          fine_solution,          &
!                                          ncell, & !nx_base,                &
!                                          nconc,                  &
!                                          origin,                 &
!                                          domain_length,          &
!                                          ic_gaussian_sd,         &
!                                          solution_gaussian_sd,    &
!                                          ic_center,              &
!                                          solution_center,        &        
!                                          tidal_ar_decay_rate)

! The general subroutine which gets the fine initial and reference values from the privious subroutine and 
! compute the norms, after each step coarsen the values and repeat computation.
! at the end  calculates the ratio of the norms and prints a log 
call test_convergence(label,                  &
                      gtm_hydro,              &
                      zero_advective_flux,    &
                      no_diffusion_flux,      &
                      no_diffusion_matrix,    &
                      no_source,              &
                      domain_length,          &
                      total_time,             &
                      start_time,             &
                      fine_initial_condition, &
                      fine_solution,          &            
                      nstep_base,             &
                      ncell,                  & !nx_base,                &
                      nconc,                  &
                      verbose,                &
                      .true.,                 &
                      acceptance_ratio)
                      
tidal_ar_decay_rate = const_tidal_decay_rate
compute_source => tidal_reaction_source


label = 'advection_reaction_gtm_gaussian' 

! load the initial values and reference final values to feed the test routine
!call initial_fine_solution_tidal_gaussian(fine_initial_condition, &
!                                          fine_solution,          &
!                                          nx_base,                &
!                                          nconc,                  &
!                                          origin,                 &
!                                          domain_length,          &
!                                          ic_gaussian_sd,         &
!                                          solution_gaussian_sd,    &
!                                          ic_center,              &
!                                          solution_center,        &        
!                                          tidal_ar_decay_rate)

! The general subroutine which gets the fine initial and reference values from the privious subroutine and 
! compute the norms, after each step coarsen the values and repeat computation.
! at the end  calculates the ratio of the norms and prints a log 
!call test_convergence(label,                  &
!                      gtm_hydro ,           &
!                      zero_advective_flux,    &
!                      no_diffusion_flux,      &
!                      no_diffusion_matrix,    &
!                      no_source,              &
!                      domain_length,          &
!                      total_time,             &
!                      start_time,             &
!                      fine_initial_condition, &
!                      fine_solution,          &            
!                      nstep_base,             &
!                      nx_base,                &
!                      nconc,                  &
!                      verbose,                &
!                      .true.,                 &
!                      acceptance_ratio)
end subroutine

!> Generates a fine initial and final solution of constant
subroutine initial_fine_solution_constant(fine_initial_condition, &
                                          fine_solution,          &
                                          nx_base,                &
                                          nconc,                  &
                                          constant)
    use gaussian_init_boundary_condition
    use gtm_precision
    implicit none
    integer,intent(in) :: nconc 
    integer,intent(in) :: nx_base
    real(gtm_real),intent(out) :: fine_initial_condition(nx_base,nconc) !< initial condition at finest resolution
    real(gtm_real),intent(out) :: fine_solution(nx_base,nconc)          !< reference solution at finest resolution
    real(gtm_real), intent(in) :: constant
    integer :: icell, i
    !do i = 1, nconc
    !    fine_initial_condition(:,i)=constant
    !    fine_solution(:,i)=constant
    !enddo
    fine_initial_condition = constant
    fine_solution = constant
  return
end subroutine
 

!> Subroutine provides source term (constant decay) to the tidal test
subroutine tidal_reaction_source(source,             & 
                                 conc,               &
                                 area,               &
                                 flow,               &
                                 ncell,              &
                                 nvar,               &
                                 time)
                               
                                     

 use  primitive_variable_conversion
 implicit none
 
 !--- args
integer,intent(in)  :: ncell                      !< Number of cells
integer,intent(in)  :: nvar                       !< Number of variables
real(gtm_real),intent(inout):: source(ncell,nvar) !< cell centered source 
real(gtm_real),intent(in)   :: conc(ncell,nvar)   !< Concentration
real(gtm_real),intent(in)   :: area(ncell)        !< area at source     
real(gtm_real),intent(in)   :: flow(ncell)        !< flow at source location
real(gtm_real),intent(in)   :: time               !< time 

! source must be in primitive variable 
source = -const_tidal_decay_rate*conc
  
return
end subroutine 

end module


