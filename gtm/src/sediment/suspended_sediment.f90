!<license>
!    Copyright (C) 2017 State of California,
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

!> Defines the common variables for the sediment transport sources.
!> The module defines the basic parameters for the sediment transport sources based
!>@ingroup sediment
module suspended_sediment

    use gtm_precision
    use common_variables, only: ero_coeff, vel_dep, vel_ero, size_dep, size_ero
    real(gtm_real) :: default_dep_size                      !< unit m
    real(gtm_real) :: default_ero_size                      !< unit m
    real(gtm_real) :: velocity_for_erosion                  !< unit m/s
    real(gtm_real) :: velocity_for_deposition               !< unit m/s
    real(gtm_real) :: param_M                               !< unit kg/(m^2s) 

    real(gtm_real), allocatable :: diameter(:,:)
    real(gtm_real), allocatable :: fall_velocity(:,:)
    real(gtm_real), allocatable :: critical_shear(:,:)
    real(gtm_real), allocatable :: particle_reynolds(:,:)
  
    contains

    !> Implementation of compute_source_if for 
    !> suspended sediment source
    subroutine suspended_sediment_source(source,          & 
                                         erosion_flux,    &
                                         deposition_flux, &
                                         conc,            & 
                                         flow,            & 
                                         area,            & 
                                         depth,           &
                                         wet_p,           & 
                                         dx,              & 
                                         dt,              & 
                                         ncell,           & 
                                         available_bed,   &
                                         isediment)      
        use common_variables, only: mann_arr 
        use sediment_variables
        use suspended_utility
        use cohesive_source
        use non_cohesive_source
        
        implicit none
        !--- args
        integer, intent(in) :: ncell                                !< number of cells      
        integer, intent(in) :: isediment                            !< sediment index
        real(gtm_real), intent(out) :: source(ncell)                !< sediment source/sink
        real(gtm_real), intent(out) :: erosion_flux(ncell)          !< sediment erosion flux
        real(gtm_real), intent(out) :: deposition_flux(ncell)       !< sediment deposition flux
        real(gtm_real), intent(in) :: conc(ncell)                   !< concentration
        real(gtm_real), intent(in) :: flow(ncell)                   !< flow
        real(gtm_real), intent(in) :: area(ncell)                   !< area
        real(gtm_real), intent(in) :: depth(ncell)                  !< depth        
        real(gtm_real), intent(in) :: wet_p(ncell)                  !< wetted perimeter
        real(gtm_real), intent(in) :: dx(ncell)                     !< dx
        real(gtm_real), intent(in) :: dt                            !< dt
        real(gtm_real), intent(in) :: available_bed(ncell)          !< available bed sediment flux
        !--- local      
        real(gtm_real) :: hydro_radius(ncell)                       !< hydraulic radius
        real(gtm_real) :: diameterp(ncell), fall_vel(ncell),critical_shear_strs(ncell)
        real(gtm_real) :: exp_re_p(ncell)
        real(gtm_real) :: capital_r, fall_vel_tmp, critical_shear_tmp, exp_re_p_tmp
        real(gtm_real) :: vertical_flux(ncell)
        real(gtm_real) :: conc_si(ncell), flow_si(ncell), area_si(ncell), dx_si(ncell)
        real(gtm_real) :: hydro_radius_si(ncell), depth_si(ncell), diameter_si(ncell)
        real(gtm_real) :: velocity(ncell)
        real(gtm_real) :: bottom_shear_stress(ncell)
        real(gtm_real) :: shear_vel(ncell)
        real(gtm_real) :: near_bed_c(ncell)
        real(gtm_real) :: si_to_english = 1000.d0                !< kg/m3-->mg/L
        integer :: icell
        logical ::  function_van_rijn 
   
        param_M = ero_coeff                        ! unit kg/(m^2s)    

        function_van_rijn = .false. !use Dietrich formula        
        hydro_radius = area/wet_p
        diameterp = diameter(:,isediment)
        fall_vel = fall_velocity(:,isediment)
        critical_shear_strs = critical_shear(:,isediment)
        exp_re_p = particle_reynolds(:,isediment)
                
        call si_unit(conc_si, flow_si, area_si,hydro_radius_si, depth_si, dx_si, diameter_si, &
                     conc, flow, area, hydro_radius, depth, dx, diameterp, ncell)    
                                                                        
        do icell = 1, ncell        
            velocity(icell) = abs(flow_si(icell)/area_si(icell))     
            call bed_shear_stress(bottom_shear_stress(icell),          &
                                  velocity(icell),                     &
                                  mann_arr(icell),                     &
                                  hydro_radius_si(icell))          
            call shear_velocity_calculator(shear_vel(icell),           &
                                           velocity(icell),            &
                                           mann_arr(icell),            &
                                           gravity,                    &
                                           hydro_radius_si(icell))  
            call near_bed_concentration(near_bed_c(icell),             &
                                        water_density,                 &
                                        sediment_density,              &
                                        gravity,                       &
                                        kinematic_viscosity,           &
                                        diameter_si(icell),            &
                                        depth_si(icell),               &
                                        bottom_shear_stress(icell),    &
                                        conc_si(icell))
            if (diameter_si(icell).gt.0.0000625d0) then
                ! Non-cohesive sediment  
                call non_cohesive_erosion(erosion_flux(icell),         &
                                          shear_vel(icell),            &
                                          exp_re_p(icell),             &
                                          fall_vel(icell))                
                !call non_cohesive_deposition(deposition_flux(icell),   &
                !                             shear_vel(icell),         &
                !                             fall_vel(icell),          &
                !                             near_bed_c(icell))               
                call cohesive_deposition(deposition_flux(icell),       &
                                         fall_vel(icell),              &
                                         near_bed_c(icell),            &
                                         critical_shear_strs(icell),   &
                                         bottom_shear_stress(icell))                 
            else
                ! Cohesive sediment
                call cohesive_erosion(erosion_flux(icell),             &
                                      critical_shear_strs(icell),      &
                                      bottom_shear_stress(icell),      &
                                      param_M)                                                                  
                call cohesive_deposition(deposition_flux(icell),       &
                                         fall_vel(icell),              &
                                         near_bed_c(icell),            &
                                         critical_shear_strs(icell),   &
                                         bottom_shear_stress(icell))                  
            end if

            if (erosion_flux(icell) .gt. available_bed(icell)) then
                erosion_flux(icell) = available_bed(icell)
            end if
            if (depth_si(icell) .eq. zero) then                
                erosion_flux(icell) = zero
                deposition_flux(icell) = zero
            end if
            if (conc(icell).le.zero) then    
                deposition_flux(icell) = zero
            end if
            vertical_flux(icell) = erosion_flux(icell) - deposition_flux(icell)
            source(icell) = vertical_flux(icell)/depth_si(icell)*si_to_english
         end do          
         return 
    end subroutine

   


    !> Implementation of compute_source_if for 
    !> suspended sediment source
    subroutine suspended_sediment_source_atche(source,          & 
                                         erosion_flux,    &
                                         deposition_flux, &
                                         conc,            & 
                                         flow,            & 
                                         area,            & 
                                         depth,           &
                                         wet_p,           & 
                                         dx,              & 
                                         dt,              & 
                                         ncell,           & 
                                         available_bed,   &
                                         isediment)      
        use common_variables, only: mann_arr 
        use sediment_variables
        use suspended_utility
        use cohesive_source
        use non_cohesive_source
        
        implicit none
        !--- args
        integer, intent(in) :: ncell                                !< number of cells      
        integer, intent(in) :: isediment                            !< sediment index
        real(gtm_real), intent(out) :: source(ncell)                !< sediment source/sink
        real(gtm_real), intent(out) :: erosion_flux(ncell)          !< sediment erosion flux
        real(gtm_real), intent(out) :: deposition_flux(ncell)       !< sediment deposition flux
        real(gtm_real), intent(in) :: conc(ncell)                   !< concentration
        real(gtm_real), intent(in) :: flow(ncell)                   !< flow
        real(gtm_real), intent(in) :: area(ncell)                   !< area
        real(gtm_real), intent(in) :: depth(ncell)                  !< depth        
        real(gtm_real), intent(in) :: wet_p(ncell)                  !< wetted perimeter
        real(gtm_real), intent(in) :: dx(ncell)                     !< dx
        real(gtm_real), intent(in) :: dt                            !< dt
        real(gtm_real), intent(in) :: available_bed(ncell)          !< available bed sediment flux
        !--- local      
        real(gtm_real) :: hydro_radius(ncell)                       !< hydraulic radius
        real(gtm_real) :: diameterp(ncell), fall_vel,critical_shear_strs
        real(gtm_real) :: exp_re_p(ncell)
        real(gtm_real) :: capital_r, fall_vel_tmp, critical_shear_tmp, exp_re_p_tmp
        real(gtm_real) :: vertical_flux(ncell)
        real(gtm_real) :: conc_si(ncell), flow_si(ncell), area_si(ncell), dx_si(ncell)
        real(gtm_real) :: hydro_radius_si(ncell), depth_si(ncell), diameter_si(ncell)
        real(gtm_real) :: velocity(ncell)
        real(gtm_real) :: bottom_shear_stress(ncell)
        real(gtm_real) :: shear_vel(ncell)
        real(gtm_real) :: near_bed_c(ncell)
        real(gtm_real) :: si_to_english = 1000.d0                !< kg/m3-->mg/L
        integer :: icell
        logical ::  function_van_rijn 
  
        param_M = 0.00000001d0                        ! unit kg/(m^2s)    
        critical_shear_strs = 0.25d0
        fall_vel = 0.00025d0

        function_van_rijn = .false. !use Dietrich formula        
        hydro_radius = area/wet_p
        diameterp = diameter(:,isediment)
        exp_re_p = particle_reynolds(:,isediment)
                
        call si_unit(conc_si, flow_si, area_si,hydro_radius_si, depth_si, dx_si, diameter_si, &
                     conc, flow, area, hydro_radius, depth, dx, diameterp, ncell)    
                                                                        
        do icell = 1, ncell        
            velocity(icell) = abs(flow_si(icell)/area_si(icell))     
            call bed_shear_stress(bottom_shear_stress(icell),          &
                                  velocity(icell),                     &
                                  mann_arr(icell),                     &
                                  hydro_radius_si(icell))          
            call shear_velocity_calculator(shear_vel(icell),           &
                                           velocity(icell),            &
                                           mann_arr(icell),            &
                                           gravity,                    &
                                           hydro_radius_si(icell))  
            call near_bed_concentration(near_bed_c(icell),           &
                                      water_density,           &
                                      sediment_density,        &
                                      gravity,          &
                                      kinematic_viscosity,     &
                                      diameter_si(icell), &
                                      depth_si(icell), &
                                      bottom_shear_stress(icell), &
                                      conc_si(icell))
            call cohesive_erosion(erosion_flux(icell),             &
                                  critical_shear_strs,      &
                                  bottom_shear_stress(icell),      &
                                  param_M)                                                                  
            call cohesive_deposition(deposition_flux(icell),       &
                                     fall_vel,              &
                                     near_bed_c(icell),               &
                                     critical_shear_strs,   &
                                     bottom_shear_stress(icell))                  

            if (erosion_flux(icell) .gt. available_bed(icell)) then
                erosion_flux(icell) = available_bed(icell)
            end if
            if (depth_si(icell) .eq. zero) then                
                erosion_flux(icell) = zero
                deposition_flux(icell) = zero
            end if
            if (conc(icell).le.zero) then    
                deposition_flux(icell) = zero
            end if
            vertical_flux(icell) = erosion_flux(icell) - deposition_flux(icell)
            source(icell) = vertical_flux(icell)/depth_si(icell)*si_to_english
         end do          
         return 
    end subroutine

   
    !> Implementation of compute_source_if for 
    !> suspended sediment source
    subroutine suspended_sediment_source_old(source,          & 
                                         erosion_flux,    &
                                         deposition_flux, &
                                         conc,            & 
                                         flow,            & 
                                         area,            & 
                                         depth,           &
                                         wet_p,           & 
                                         dx,              & 
                                         dt,              & 
                                         ncell,           & 
                                         available_bed,   &
                                         isediment)      
        use common_variables, only: mann_arr 
        use sediment_variables
        use suspended_utility
        use cohesive_source
        use non_cohesive_source
        
        implicit none
        !--- args
        integer, intent(in) :: ncell                                !< number of cells      
        integer, intent(in) :: isediment                            !< sediment index
        real(gtm_real), intent(out) :: source(ncell)                !< sediment source/sink
        real(gtm_real), intent(out) :: erosion_flux(ncell)          !< sediment erosion flux
        real(gtm_real), intent(out) :: deposition_flux(ncell)       !< sediment deposition flux
        real(gtm_real), intent(in) :: conc(ncell)                   !< concentration
        real(gtm_real), intent(in) :: flow(ncell)                   !< flow
        real(gtm_real), intent(in) :: area(ncell)                   !< area
        real(gtm_real), intent(in) :: depth(ncell)                  !< depth        
        real(gtm_real), intent(in) :: wet_p(ncell)                  !< wetted perimeter
        real(gtm_real), intent(in) :: dx(ncell)                     !< dx
        real(gtm_real), intent(in) :: dt                            !< dt
        real(gtm_real), intent(in) :: available_bed(ncell)          !< available bed sediment flux
        !--- local      
        real(gtm_real) :: hydro_radius(ncell)                       !< hydraulic radius
        real(gtm_real) :: diameterp(ncell), fall_vel(ncell),critical_shear_strs(ncell)
        real(gtm_real) :: exp_re_p(ncell)
        real(gtm_real) :: capital_r, fall_vel_tmp, critical_shear_tmp, exp_re_p_tmp
        real(gtm_real) :: vertical_flux(ncell)
        real(gtm_real) :: conc_si(ncell), flow_si(ncell), area_si(ncell), dx_si(ncell)
        real(gtm_real) :: hydro_radius_si(ncell), depth_si(ncell), diameter_si(ncell)
        real(gtm_real) :: velocity(ncell)
        real(gtm_real) :: bottom_shear_stress(ncell)
        real(gtm_real) :: shear_vel(ncell)
        real(gtm_real) :: si_to_english = 1000.d0                !< kg/m3-->mg/L
        integer :: icell
        logical ::  function_van_rijn 
   
        default_dep_size = size_dep*0.001d0        ! unit mm-->m
        default_ero_size = size_ero*0.001d0        ! unit mm-->m
        velocity_for_erosion = vel_ero*0.3048d0    ! unit ft/s-->m/s
        velocity_for_deposition = vel_dep*0.3048d0 ! unit ft/s-->m/s
        param_M = ero_coeff                        ! unit kg/(m^2s)    

        function_van_rijn = .false. !use Dietrich formula        
        hydro_radius = area/wet_p
        diameterp = diameter(:,isediment)
        fall_vel = fall_velocity(:,isediment)
        critical_shear_strs = critical_shear(:,isediment)
        exp_re_p = particle_reynolds(:,isediment)
                
        call si_unit(conc_si, flow_si, area_si,hydro_radius_si, depth_si, dx_si, diameter_si, &
                     conc, flow, area, hydro_radius, depth, dx, diameterp, ncell)    
                                                                        
        do icell = 1, ncell        
            velocity(icell) = abs(flow_si(icell)/area_si(icell))     
            call bed_shear_stress(bottom_shear_stress(icell),          &
                                  velocity(icell),                     &
                                  mann_arr(icell),                     &
                                  hydro_radius_si(icell))          
            call shear_velocity_calculator(shear_vel(icell),           &
                                           velocity(icell),            &
                                           mann_arr(icell),            &
                                           gravity,                    &
                                           hydro_radius_si(icell))  
            if (diameter_si(icell).gt.0.0000625d0 .and. diameter_si(icell).lt.0.001d0) then
                ! Non-cohesive sediment  
                call non_cohesive_erosion(erosion_flux(icell),         &
                                          shear_vel(icell),            &
                                          exp_re_p(icell),             &
                                          fall_vel(icell))                
                call cohesive_deposition(deposition_flux(icell),       &
                                         fall_vel(icell),              &
                                         conc_si(icell),               &
                                         critical_shear_strs(icell),   &
                                         bottom_shear_stress(icell))              
            else
                ! Cohesive sediment
                call cohesive_erosion(erosion_flux(icell),             &
                                      critical_shear_strs(icell),      &
                                      bottom_shear_stress(icell),      &
                                      param_M)                                                                  
                call cohesive_deposition(deposition_flux(icell),       &
                                         fall_vel(icell),              &
                                         conc_si(icell),               &
                                         critical_shear_strs(icell),   &
                                         bottom_shear_stress(icell))                  
            end if
            
            ! For special cases
            ! case 1: vertical flux=0 for cell i
            if ((diameter_si(icell).ge.0.001d0) .and. (diameter_si(icell).lt.0.002d0)) then                
                erosion_flux(icell) = deposition_flux(icell)
            ! case 2: trigger deposition when flow velocity is less than vel_dep
            elseif ((diameter_si(icell).ge.0.002d0) .and. (diameter_si(icell).lt.0.003d0)) then 
                if (velocity(icell).gt.velocity_for_deposition) erosion_flux(icell) = deposition_flux(icell)
            ! case 3: trigger erosion when flow velocity is greater than vel_ero
            elseif ((diameter_si(icell).ge.0.003d0) .and. (diameter_si(icell).lt.0.004d0)) then 
                if ((velocity(icell).gt.velocity_for_deposition).and.(velocity(icell).le.velocity_for_erosion)) then
                    erosion_flux(icell) = deposition_flux(icell)
                elseif (velocity(icell).gt.velocity_for_erosion) then
                    call settling_velocity(fall_vel_tmp, kinematic_viscosity, specific_gravity, default_ero_size, gravity, function_van_rijn)    
                    call critical_shear_stress(critical_shear_tmp, water_density, sediment_density, gravity, kinematic_viscosity, default_ero_size)                
                    call cohesive_erosion(erosion_flux(icell),             &
                                          critical_shear_tmp,              &
                                          bottom_shear_stress(icell),      &
                                          param_M)                                                                  
                    call cohesive_deposition(deposition_flux(icell),       &
                                             fall_vel_tmp,                 &
                                             conc_si(icell),               &
                                             critical_shear_tmp,           &
                                             bottom_shear_stress(icell))                
                end if                                          
            end if

            if (erosion_flux(icell) .gt. available_bed(icell)) then
                erosion_flux(icell) = available_bed(icell)
            end if
            if (depth_si(icell) .eq. zero) then                
                erosion_flux(icell) = zero
                deposition_flux(icell) = zero
            end if
            if (conc(icell).le.zero) then    
                deposition_flux(icell) = zero
            end if
            vertical_flux(icell) = erosion_flux(icell) - deposition_flux(icell)
            source(icell) = vertical_flux(icell)/depth_si(icell)*si_to_english
         end do          
         return 
    end subroutine


    !> allocate sediment variables
    subroutine allocate_sediment_variables(ncell,     &
                                           nsediment)
        implicit none     
        integer, intent(in) :: ncell
        integer, intent(in) :: nsediment
        allocate(diameter(ncell,nsediment))  
        allocate(fall_velocity(ncell,nsediment)) 
        allocate(particle_reynolds(ncell,nsediment))
        allocate(critical_shear(ncell,nsediment)) 
    end subroutine
 
    !> deallocate sediment variables
    subroutine dealloacte_sediment_variables()
        implicit none
        deallocate(diameter)
        deallocate(fall_velocity)
        deallocate(particle_reynolds)
        deallocate(critical_shear)
    end subroutine

    
    !> Convert all variables from English unit to SI unit
    subroutine si_unit(conc_si,        &
                       flow_si,        &
                       area_si,        &
                       hyd_radius_si,  &
                       depth_si,       &
                       dx_si,          &                       
                       diameter_si,    &
                       conc,           &
                       flow,           &
                       area,           &
                       hyd_radius,     &
                       depth,          &
                       dx,             &
                       diameter,       &
                       ncell) 
        implicit none
        integer, intent(in) :: ncell
        real(gtm_real), intent(out) :: conc_si(ncell)
        real(gtm_real), intent(out) :: flow_si(ncell)
        real(gtm_real), intent(out) :: area_si(ncell)
        real(gtm_real), intent(out) :: hyd_radius_si(ncell)
        real(gtm_real), intent(out) :: depth_si(ncell)
        real(gtm_real), intent(out) :: dx_si(ncell)
        real(gtm_real), intent(out) :: diameter_si(ncell)
        real(gtm_real), intent(in) :: conc(ncell)
        real(gtm_real), intent(in) :: flow(ncell)
        real(gtm_real), intent(in) :: area(ncell)
        real(gtm_real), intent(in) :: hyd_radius(ncell)
        real(gtm_real), intent(in) :: depth(ncell)
        real(gtm_real), intent(in) :: dx(ncell)
        real(gtm_real), intent(in) :: diameter(ncell)
        real(gtm_real), parameter :: L = 0.3048d0
        
        conc_si = conc*0.001d0         ! mg/L-->kg/m3
        flow_si = flow*L*L*L           ! cfs-->m3/s
        area_si = area*L*L             ! ft^2-->m^2
        hyd_radius_si = hyd_radius*L   ! ft-->m
        depth_si = depth*L             ! ft-->m
        dx_si = dx*L                   ! ft-->m
        diameter_si = diameter*0.001d0 ! mm-->m        
        return
    end subroutine    
    
    
    !> get sediment properties of explicit particle Reynolds number
    !> and critical shear stress.
    subroutine get_sediment_properties(fall_velocity,             &
                                       exp_re_p,                  &
                                       critical_shear_strs,       &
                                       diameterp,                 &
                                       ncell)
        use sediment_variables
        use suspended_utility
        implicit none
        integer, intent(in) :: ncell
        real(gtm_real), intent(in) :: diameterp(ncell)           !< mm
        real(gtm_real), intent(out) :: fall_velocity(ncell)
        real(gtm_real), intent(out) :: exp_re_p(ncell)
        real(gtm_real), intent(out) :: critical_shear_strs(ncell)
        real(gtm_real), parameter :: default_fines_size = 0.00002d0    
        real(gtm_real) :: capital_r
        real(gtm_real) :: diameter_si(ncell)
        logical :: function_van_rijn
        integer :: icell
        
        function_van_rijn = .false. !use Dietrich formula
        diameter_si = diameterp*0.001d0 ! mm-->m
         
        default_dep_size = size_dep*0.001d0       ! unit mm-->m

        where (diameter_si.ge.0.00100d0) diameter_si = default_dep_size
        
        call submerged_specific_gravity(capital_r,                     &
                                        water_density,                 &
                                        sediment_density)
        
        do icell = 1, ncell                                
            call explicit_particle_reynolds_number(exp_re_p(icell),     &
                                                   diameter_si(icell),  &
                                                   capital_r,           &
                                                   gravity,             &
                                                   kinematic_viscosity)
    
            call settling_velocity(fall_velocity(icell),                &
                                   kinematic_viscosity,                 &
                                   specific_gravity,                    &
                                   diameter_si(icell),                  &
                                   gravity,                             &
                                   function_van_rijn)       
    
            call critical_shear_stress(critical_shear_strs(icell),      &
                                       water_density,                   &
                                       sediment_density,                &
                                       gravity,                         &
                                       kinematic_viscosity,             &
                                       diameter_si(icell))
        end do         
        return
    end subroutine    
end module    