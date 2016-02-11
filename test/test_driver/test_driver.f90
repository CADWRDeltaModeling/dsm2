!<license>
!    Copyright (C) 2015 State of California,
!    Department of Water Resources.
!    This file is part of DSM2-GTM.
!
!    The Delta Simulation Model 2 (DSM2) - General Transport Model (GTM) 
!    is free software: you can redistribute it and/or modify
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

!> Main progran unit to test for all GTM components. 
!> FRUIT (FORTRAN Unit Test Framework) is utilized for result assertion. 
!> Files required for testing: 
!> dsm2_hydro_ut.h5 and dsm2_hydro_fine_ut.h5
!>@ingroup driver
program test_driver
  
    use fruit
    use gtm_logging 
    use klu

    !----- modules used in project test_common ------
    use ut_hydro_data_tide
    use ut_hydro_data_interp
    use ut_interpolation
    use ut_gtm_network
    
    !----- modules used in project test_process_io -----
    use ut_dsm2_time_utils
    use ut_process_gtm_input
    use ut_time_utilities
    use ut_gtm_dss_readdss
    use ut_gtm_dss_readtvd
    use ut_gtm_dss_main
    use ut_gtm_hdf_ts_wrt
    use ut_gtm_hdf_write
    use ut_boundary
    use ut_create_restart
    use ut_read_init_file
    
    !----- modules used in project test_transport -----  
    use test_extrapolate
    use test_prim_cons_conversion
    use test_prim_increment_to_cons
    use test_gradient    
    use test_converge_bidirectional_uniform
    use test_convergence_transport_uniform_vary_dx
    use test_advection_reaction_tidal_vary_dx
    use test_converge_hydro_interpolation
    use test_matrix_solver
    use test_boundary_diffusion
    use test_diffusive_flux
    use test_explicit_diffusion_operator
    use test_interior_coef_matrix
    use test_construct_r_h_s
    use source_sink
    use test_hydro
    use test_advection_reaction_tidal    
    use test_coarsening
    use test_converge_bidirectional_uniform
    use test_diffusion_fletcher
    use test_diffusion_nonlinear_decay

    use test_convergence_transport_uniform
    use test_zoppou_advection_dispersion
    use test_time_dependent_advection_dispersion
    use test_mms_advection_dispersion

    use ut_do_source    
    use ut_klu
    use ut_utils
    
    !----- modules used to test sediment module ----- 
    use test_non_cohesive
    use unit_test_suspend_sed_utility
    
    implicit none
    logical :: verbose = .true.
    character(len=255) :: parent_dir

    open(141,file="temp1.txt")
    open(142,file="temp2.txt")
    open(143,file="temp3.txt")
    open(144,file="temp4.txt")
    
    call init_fruit
    
    call get_parent_working_dir(parent_dir) 
    
    k_common = klu_fortran_init()  
    call klu_fortran_free_numeric(k_numeric, k_common)
   
    !----- function calls to test units in project process_io API ---
    call change_working_dir(parent_dir, "/gtm_core_unit_test_io")
    call test_dsm2_time_utils
    call test_time_util
    call test_readdss  
    call test_readtvd
    call test_dss_main
    call test_input_storage 
    call test_hdf_ts_wrt
    call test_gtm_hdf_write
    call test_find_bound_index
    call test_read_init_file
    
    !----- function calls to test units in project common -----
    call change_working_dir(parent_dir, "/gtm_core_unit_test_io")
    open(debug_unit, file = "debug_unit.txt")            !< output text file
    call test_hdf_util                                   ! test hdf_util()
    call test_interpolation                              ! test interpolation schemes
    call test_gtm_network                                ! test creating GTM network   
          
    !----- function calls to test units in project transport ----- 
    call change_working_dir(parent_dir, "/transport_unit_test_out")
    
    !----- Advection unit tests
    call test_gradient_calc
    call test_limiter
    call test_prim_cons_convert
    call test_prim_increment2cons
    call test_extrapolation
    call test_tidal_hydro
    call print_out_tidal_hydro

    !----- Advection-diffusion-reaction convergence in uniform flow,
    !      operators are layered in successively
    call test_converge_transport_uniform(verbose)
    call test_converge_transport_uniform_vary_dx(verbose)
    call test_converge_uniform_bidirectional(verbose)
        !----- Diffusion unit tests
    call test_tridi_solver
    call test_boundary_diffusion_flux
    call test_make_dif_flux_sub
    call test_explicit_interior_diffusion_op
    call test_interior_coef_matrix_sub
    call test_construct_elemnts_rhs 
    call test_coarsen
    call test_detect_wiggle
    call test_mass_comparison
    
    !----- Advection - reaction problems
    call test_tidal_advection_reaction(verbose)
    call test_tidal_advection_reaction_vary_dx(verbose)
    call test_advection_reaction_hydro_interp(verbose)
    
    !----- Advection-Diffusion tests
    call test_zoppou_flow() 
    call test_advection_diffusion_zoppou(verbose)
    !call test_advection_diffusion_t_dependent(verbose)
    !call test_advection_diffusion_time_dependent(verbose)

    !----- Advection-diffusion-reaction
    !call test_advection_diffusion_mms(verbose)
    
    !----- Test KLU Solver
    call klu_fortran_free(k_symbolic, k_numeric, k_common)    
    call test_klu_exmamples
    call test_qsort
    call test_rowcol2apai
    
    !----- Test sediment module
    call test_first_einstein_integral
    call test_es_garcia_parker
    call test_settling_velocity
    call test_submerged_specific_gravity
    call test_explicit_particle_reynolds_number
    call test_particle_reynolds_number
    call test_dimless_particle_diameter
    call test_critical_shields_parameter    
    call test_shear_velocity
    call test_rouse_number
    call test_allocation_ratio
    call test_source_non_cohesive
    
    !----- function calls to test units in project sediment -----
    
    call fruit_summary

    close(debug_unit)
    close(141)
    close(142)
    close(143)
    close(144)
    pause    
end program test_driver


!> Get parent working directory
subroutine get_parent_working_dir(parent_dir)
    implicit none
    integer :: getcwd, status
    character(len=255), intent(out) :: parent_dir
    status = getcwd(parent_dir)
    if (status .ne. 0) stop 'getcwd: error'
    return
end subroutine    


!> Change working directory
subroutine change_working_dir(parent_dir, subfolder)
    implicit none
    integer :: getcwd, chdir, status
    character(len=255) :: parent_dir
    character*(*) :: subfolder
    status = chdir(trim(parent_dir)//trim(subfolder))
    if (status .ne. 0) stop 'chdir: error'
    return
end subroutine