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

!> Test advection diffusion and reaction in a single channel with uniform flow
!>@ingroup test_transport
module test_convergence_transport_uniform

    use gtm_precision

    ! These variables are needed at the module level for things like boundary conditions
    real(gtm_real) :: const_disp_coef = one                       !< Constant dispersion coefficient
    real(gtm_real), parameter :: origin =zero                     !< Origin
    real(gtm_real), parameter :: base_domain_length = 25600.d0    !< Domain length for start (todo: is it correct)
    real(gtm_real) :: domain_length = base_domain_length          !< Domain length
    real(gtm_real) :: ic_center = LARGEREAL                       ! -------initialization to a number which trigers whenit is not initiated   
    real(gtm_real), parameter :: ic_peak = one                    !< Gaussian hight
    real(gtm_real) :: const_velocity                              !< Constant velocity 
    real(gtm_real) :: diffuse_start_time                          !< Diffusion start time
    real(gtm_real) :: diffuse_end_time                            !< Diffusion end time

    contains

    !todo: Have to parameterize this for a, ar, ad, adr
    !todo: Diffusion BC is wrong
    !todo: Diffusion is still not in predictor step
    !todo: Discovery about source term
    !> parameterized test driver for uniform flow
    subroutine test_converge_transport_uniform(verbose)

        implicit none

        logical, intent(in) :: verbose                         !< Flag to show the details 
        logical, parameter :: remote = .true.                  !< Flag to switch remote boundray on 
        logical, parameter :: do_detail = .true.               !< Flag to printout the details 
        real(gtm_real), parameter :: constant_flow  = 600.d0
        real(gtm_real), parameter :: constant_decay = 5.d-5 
        real(gtm_real), parameter :: constant_diffuse = sixteen ! todo: not smaller than 1 
        real(gtm_real) :: flow                                  !< Flag to switch on the advection test routine
        real(gtm_real) :: diffuse                               !< Flag to switch on the diffusion test routine
        real(gtm_real) :: decay                                 !< Flag to switch on the decay test routine

        flow   = constant_flow
        diffuse= zero
        decay  = zero
        call converge_transport_uniform(verbose,"uniform_advect",flow,diffuse,decay)
        call converge_transport_uniform(verbose,"uniform_advect_remote_bc",flow,diffuse,decay,boundary_remote=remote,detail_result=do_detail)

        flow   = zero
        diffuse= constant_diffuse
        decay  = zero
        call converge_transport_uniform(verbose,"uniform_diffuse",flow,diffuse,decay)
        call converge_transport_uniform(verbose,"uniform_diffuse_remote_bc",flow,diffuse,decay,boundary_remote=remote)

        flow   = zero
        diffuse= zero
        decay  = constant_decay
        call converge_transport_uniform(verbose,"uniform_react",flow,diffuse,decay)
        call converge_transport_uniform(verbose,"uniform_react_remote_bc",flow,diffuse,decay,boundary_remote=remote)

        flow   = constant_flow
        diffuse= constant_diffuse
        decay  = zero
        call converge_transport_uniform(verbose,"uniform_advect_diffuse",flow,diffuse,decay,detail_result=do_detail)
        call converge_transport_uniform(verbose,"uniform_advect_diffuse_remote_bc",flow,diffuse,decay,boundary_remote=remote)

        flow   = constant_flow
        diffuse= zero
        decay  = constant_decay
        call converge_transport_uniform(verbose,"uniform_advect_react",flow,diffuse,decay)
        call converge_transport_uniform(verbose,"uniform_advect_react_remote_bc",flow,diffuse,decay,boundary_remote=remote)

        flow   = constant_flow
        diffuse= constant_diffuse
        decay  = constant_decay
        call converge_transport_uniform(verbose,"uniform_advect_diffuse_react",flow,diffuse,decay)
        call converge_transport_uniform(verbose,"uniform_advect_diffuse_react_remote_bc",flow,diffuse,decay,boundary_remote=remote)

        return
    end subroutine


    !> Subroutine that tests error convergence of advection diffusion reaction 
    subroutine converge_transport_uniform(verbose,         &
                                          label,           &
                                          test_flow,       &
                                          test_diffuse,    &
                                          test_decay,      &
                                          boundary_remote, &
                                          detail_result)
        use test_convergence_transport
        use fruit
        use advection
        use diffusion
        use boundary_advection
        use boundary_diffusion
        use single_channel_boundary
        use primitive_variable_conversion
        use hydro_data
        use source_sink
        use test_utility
        use state_variables
        use gtm_logging
        use hydro_uniform_flow
        use dispersion_coefficient

        implicit none

        logical, intent(in) :: verbose                   !< Switch for detailed show of the results
        real(gtm_real), intent(in) :: test_flow          !< Flag for testing advection process 
        real(gtm_real), intent(in) :: test_diffuse       !< Flag for testing diffusion process
        real(gtm_real), intent(in) :: test_decay         !< Flag for testing decay process
        character(LEN=*),intent(in) :: label             !< Test's name  
        logical, intent(in), optional :: detail_result   !< Switch for detailed print of the results
        logical, intent(in), optional :: boundary_remote !< Switch for active boundary value testing vs zero (remote BC)
        real(gtm_real) :: acceptance_ratio(3)            !< Acceptance ratio

        integer, parameter  :: nx_base_standard = 256
        integer :: nx_base = nx_base_standard
        integer, parameter  :: nstep_base = 256
        real(gtm_real), parameter :: total_time = 38400.d0
        real(gtm_real), parameter :: start_time = zero 
        real(gtm_real), parameter :: constant_area = 1000.d0
        ! todo: what about diffusion included test?
        real(gtm_real), parameter :: reverse_time = total_time/two             !< Time the flow dirction switches back
        real(gtm_real), parameter :: ic_gaussian_sd = base_domain_length/32.d0 !< Initial center for the Gaussian hump of mass
        !real(gtm_real) :: solution_gaussian_sd = ic_gaussian_sd

        integer, parameter :: nconc = 2                                             !< Number of variables
        real(gtm_real) :: decay_rate = zero                                         !< Decay Rate
        real(gtm_real), dimension(nconc) :: rates                                   !< todo: Norm of teh errors rate 
        real(gtm_real),allocatable :: fine_initial_conc(:,:)                        !< Initial condition at finest resolution
        real(gtm_real),allocatable :: fine_solution(:,:)                            !< Reference solution at finest resolution
        procedure(hydro_data_if),               pointer :: uniform_hydro   => null()!< Hydrodynamic data pointer
        procedure(source_if),                   pointer :: test_source     => null()!< Source term data pointer
        procedure(boundary_advective_flux_if),  pointer :: bc_advect_flux  => null()!< Boundary fluxes of advection pointer
        procedure(boundary_diffusive_flux_if),  pointer :: bc_diff_flux    => null()!< Boundary fluxes of diffusion pointer
        procedure(boundary_diffusive_matrix_if),pointer :: bc_diff_matrix  => null()!< Boundary values of diffusion matrix ponter
        procedure(diffusion_coef_if),           pointer :: diff_coef       => null()!< Dispersion coefficient values pointer

        logical :: details = .false.                                                !< Flag switch todo: ?
        logical :: remote  = .false.                                                !< Flag Switch todo: ?

        integer, parameter :: n_bound = 2
        real(gtm_real) :: bound_val(n_bound)

        bound_val = one

        acceptance_ratio = [three, three, three]    ! relax the standard for uniform flow transport 

        if (present(detail_result))then
            details = detail_result
        else 
            details = .false.
        end if

        if (present(boundary_remote))then
            remote  = boundary_remote
        else 
            remote = .false.
        end if

        if (remote)then
            ic_center = origin + base_domain_length/three
            domain_length = base_domain_length*two
            nx_base = nx_base_standard*2
            call set_uniform_flow_area(test_flow,constant_area)
        else
            ic_center = origin + base_domain_length/sixteen
            domain_length = base_domain_length
            nx_base = nx_base_standard
            call set_uniform_flow_area(test_flow,constant_area)
        end if

        call set_uniform_flow_area(test_flow,constant_area)
        uniform_hydro=> uniform_flow_area
        const_velocity = test_flow/constant_area

        ! source
        decay_rate = test_decay
        rates = decay_rate
        call set_linear_decay(rates,2)
        if (test_decay .ne. zero)then
            rates = decay_rate
            call set_linear_decay(rates,2)
            test_source    => linear_decay_source
            compute_source => linear_decay_source !todo: because of compiler problem
        else
            test_source => no_source
            compute_source => no_source
        end if

        if (test_diffuse .eq. zero) then    
            const_disp_coef = one !for production of initial and final solution
            call set_constant_dispersion(zero)
            call set_single_channel_boundary(dirichlet_advective_flux_lo, gaussian_data,                &
                                             dirichlet_advective_flux_hi, gaussian_data,                &
                                             dirichlet_diffusive_flux_lo, extrapolate_hi_boundary_data, &
                                             dirichlet_diffusive_flux_hi, extrapolate_hi_boundary_data ) !todo: are these intentionally set here as out flow?
            boundary_diffusion_flux   => no_diffusion_flux         ! todo: improve set_single_channel_boundary to avoid this
            boundary_diffusion_matrix => no_diffusion_matrix
        else
            const_disp_coef =  test_diffuse
            call set_constant_dispersion(const_disp_coef)
            call set_single_channel_boundary(dirichlet_advective_flux_lo, gaussian_data, &
                                             dirichlet_advective_flux_hi, gaussian_data, &
                                             dirichlet_diffusive_flux_lo, gaussian_data, &
                                             dirichlet_diffusive_flux_hi, extrapolate_hi_boundary_data )

            boundary_diffusion_flux => single_channel_boundary_diffusive_flux
            boundary_diffusion_matrix => single_channel_boundary_diffusive_matrix
        end if
        const_dispersion = const_disp_coef
        diffuse_start_time  = ic_gaussian_sd**two/(const_disp_coef*two)
        advection_boundary_flux => single_channel_boundary_advective_flux

        allocate(fine_initial_conc(nx_base,nconc),fine_solution(nx_base,nconc))
        ! Subroutine which generates fine initial values and reference values to compare with 
        ! and feed the covvergence test subroutine.
        call initial_final_solution_uniform(fine_initial_conc,     &
                                            fine_solution,         &
                                            ic_center,             &
                                            ic_peak,               &
                                            const_velocity,        &
                                            decay_rate,            &
                                            total_time,            &
                                            origin,                &
                                            domain_length,         &
                                            nx_base,               &
                                            nconc)

        call test_convergence(label,                                     &
                              uniform_hydro,                             &
                              single_channel_boundary_advective_flux,    &
                              bc_diff_flux,                              &
                              bc_diff_matrix,                            &
                              test_source,                               &
                              domain_length,                             &
                              total_time,                                &
                              start_time,                                &
                              fine_initial_conc,                         &
                              fine_solution,                             &            
                              nstep_base,                                &
                              nx_base,                                   &
                              nconc,                                     &
                              n_bound,                                   &
                              bound_val,                                 &
                              verbose,                                   &
                              details,                                   &
                              acceptance_ratio)
                      
        deallocate(fine_initial_conc,fine_solution)
        return
    end subroutine

    !===========
    !> produce fine initial condition and reference solution 
    subroutine initial_final_solution_uniform(fine_initial_conc,     &
                                              fine_solution_conc,    &
                                              ic_center,             &
                                              ic_peak,               &
                                              const_velocity,        &
                                              decay_rate,            &
                                              total_time,            &
                                              origin,                &
                                              domain_length,         &
                                              nx_base,               &
                                              nconc)                                  
        use gaussian_init_boundary_condition
        use diffusion
        implicit none
        integer, intent(in) :: nx_base
        integer, intent(in) :: nconc
        real(gtm_real),intent(out) :: fine_initial_conc(nx_base,nconc)     !< Initial condition at finest resolution
        real(gtm_real),intent(out) :: fine_solution_conc(nx_base,nconc)    !< Reference solution at finest resolution
        real(gtm_real),intent(in)  :: ic_center                            !< Initial condition center of mass location
        real(gtm_real),intent(in)  :: ic_peak                              !< Peak hight of initial condition
        real(gtm_real),intent(in)  :: const_velocity                       !< Constant velocity of flow
        real(gtm_real),intent(in)  :: decay_rate                           !< Decay rate 
        real(gtm_real),intent(in)  :: total_time                           !< Total time of test  
        real(gtm_real),intent(in)  :: origin                               !< Origin location
        real(gtm_real),intent(in)  :: domain_length                        !< Domain length
        !--local
        integer :: ivar                                                    !< Counter on constituent     
        real(gtm_real) :: dx                                               !< Spacial step
        real(gtm_real) :: diffuse_end_time = LARGEREAL                     !< End time of diffusion process initialized to LARGEREAL 
        real(gtm_real) :: final_center                                     !< Solution center
        dx = domain_length/nx_base

        final_center = ic_center  + const_velocity * total_time
        if (use_diffusion())then
            diffuse_end_time = diffuse_start_time + total_time
        else
            diffuse_end_time = diffuse_start_time
        end if

        do ivar = 1, nconc
            call fill_gaussian(fine_initial_conc(:,ivar),nx_base,origin,dx, &
                               ic_center,dsqrt(two*const_disp_coef*diffuse_start_time),ic_peak)
            call fill_gaussian(fine_solution_conc(:,ivar),nx_base,origin,dx, & 
                               final_center,dsqrt(two*const_disp_coef*diffuse_end_time), &
                               ic_peak*dsqrt(diffuse_start_time/diffuse_end_time))
        end do

        fine_solution_conc = fine_solution_conc * dexp(-decay_rate*total_time)

        return
    end subroutine

    !> Gaussian data
    subroutine gaussian_data(bc_data,           &
                             xloc,              &
                             conc,              &
                             ncell,             &
                             nvar,              &
                             origin,            &
                             time,              &
                             dx,                &
                             dt)
        use gtm_precision
        use gaussian_init_boundary_condition
        use diffusion
        implicit none
        !--- args
        integer, intent(in)  :: ncell                                    !< Number of cells
        integer, intent(in)  :: nvar                                     !< Number of variables
        real(gtm_real), intent(out)   :: bc_data(nvar)                   !< concentration or gradient data
        real(gtm_real), intent(in)    :: xloc                            !< location where data is requested
        real(gtm_real), intent (in)   :: time                            !< Time
        real(gtm_real), intent (in)   :: origin                          !< Space origin
        real(gtm_real), intent (in)   :: conc(ncell,nvar)                !< Concentration 
        real(gtm_real), intent (in)   :: dt                              !< Time step
        real(gtm_real), intent (in)   :: dx(ncell)                       !< Spacial step
    
        !--- local
        real(gtm_real) :: val
        real(gtm_real) :: current_center
        real(gtm_real) :: diffuse_time

        current_center = ic_center + const_velocity*time
        if (use_diffusion())then
            diffuse_time = diffuse_start_time + time
        else
            diffuse_time = diffuse_start_time
        end if
        call gaussian(val,xloc,current_center,dsqrt(two*const_disp_coef*diffuse_time), &
                      ic_peak*dsqrt(diffuse_start_time/diffuse_time))
        bc_data = val
        return
    end subroutine

    !> Gaussian gradient data
    subroutine gaussian_gradient_data(bc_data,           &
                                      xloc,              &
                                      conc,              &
                                      ncell,             &
                                      nvar,              &
                                      origin,            &
                                      time,              &
                                      dx,                &
                                      dt)
        use gtm_precision
        use gaussian_init_boundary_condition
        use diffusion
        implicit none
        !--- args
        integer, intent(in)  :: ncell                                    !< Number of cells
        integer, intent(in)  :: nvar                                     !< Number of variables
        real(gtm_real), intent(out)   :: bc_data(nvar)                   !< concentration or gradient data
        real(gtm_real), intent(in)    :: xloc                            !< location where data is requested
        real(gtm_real), intent (in)   :: time                            !< Time
        real(gtm_real), intent (in)   :: origin                          !< Space origin
        real(gtm_real), intent (in)   :: conc(ncell,nvar)                !< Concentration 
        real(gtm_real), intent (in)   :: dt                              !< Time step
        real(gtm_real), intent (in)   :: dx(ncell)                       !< Spacial step
    
        !--- local
        real(gtm_real) :: val
        real(gtm_real) :: current_center
        real(gtm_real) :: diffuse_time

        current_center = ic_center + const_velocity*time
        if (use_diffusion())then
            diffuse_time = diffuse_start_time + time
        else
            diffuse_time = diffuse_start_time
        end if
  
        call derivative_gaussian(val,xloc,current_center,dsqrt(two*const_disp_coef*diffuse_time), &
                                 ic_peak*dsqrt(diffuse_start_time/diffuse_time))
        bc_data = val
        return
    end subroutine

    !> Extrapolate high side boundary data 
    subroutine extrapolate_hi_boundary_data(bc_data,           &
                                            xloc,              &
                                            conc,              &
                                            ncell,             &
                                            nvar,              &
                                            origin,            &
                                            time,              &
                                            dx,                &
                                            dt)
        use gtm_precision
        use gaussian_init_boundary_condition
        use diffusion
        implicit none
        !--- args
        integer, intent(in)  :: ncell                                    !< Number of cells
        integer, intent(in)  :: nvar                                     !< Number of variables
        real(gtm_real), intent(out) :: bc_data(nvar)                     !< concentration or gradient data
        real(gtm_real), intent(in)  :: xloc                              !< location where data is requested
        real(gtm_real), intent(in)  :: time                              !< Time
        real(gtm_real), intent(in)  :: origin                            !< Space origin
        real(gtm_real), intent(in)  :: conc(ncell,nvar)                  !< Concentration 
        real(gtm_real), intent(in)  :: dt                                !< Time step
        real(gtm_real), intent(in)  :: dx(ncell)                         !< Spacial step
    
        ! zero order approximation
         bc_data = conc(ncell,:)
        ! first order approximation
         bc_data = conc(ncell,:) + (conc(ncell,:) - conc(ncell-1,:))/two
    
         return
    end subroutine

end module