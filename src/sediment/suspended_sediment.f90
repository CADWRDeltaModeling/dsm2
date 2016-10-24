!<license>
!    Copyright (C) 2016 State of California,
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
 
    contains

    subroutine suspended_sediment_flux(source,            & 
                                       erosion_flux,      &
                                       deposition_flux,   &
                                       conc,              & 
                                       flow,              & 
                                       area,              & 
                                       width,             & 
                                       hydro_radius,      & 
                                       manning_n,         & 
                                       diameter,          &
                                       fall_vel,          &
                                       critical_shear,    &
                                       exp_re_p,          &                             
                                       dx,                & 
                                       dt,                & 
                                       ncell,             & 
                                       available_bed)
        use sediment_variables
        use suspended_utility
        use cohesive_source
        use non_cohesive_source
        implicit none
        !--- args
        integer, intent(in) :: ncell                                !< number of cells      
        real(gtm_real), intent(out) :: source(ncell)                !< sediment source/sink
        real(gtm_real), intent(out) :: erosion_flux(ncell)          !< sediment erosion flux
        real(gtm_real), intent(out) :: deposition_flux(ncell)       !< sediment deposition flux
        real(gtm_real), intent(in) :: conc(ncell)                   !< concentration
        real(gtm_real), intent(in) :: flow(ncell)                   !< flow
        real(gtm_real), intent(in) :: area(ncell)                   !< area
        real(gtm_real), intent(in) :: width(ncell)                  !< channel width
        real(gtm_real), intent(in) :: hydro_radius(ncell)           !< hydraulic radius
        real(gtm_real), intent(in) :: manning_n(ncell)              !< Manning's n
        real(gtm_real), intent(in) :: diameter(ncell)               !< diameter
        real(gtm_real), intent(in) :: fall_vel(ncell)               !< settling velocity
        real(gtm_real), intent(in) :: critical_shear(ncell)         !< critical shear stress
        real(gtm_real), intent(in) :: exp_re_p(ncell)               !< explicit particle Reynolds number
        real(gtm_real), intent(in) :: dx(ncell)                     !< dx
        real(gtm_real), intent(in) :: dt                            !< dt
        real(gtm_real), intent(in) :: available_bed(ncell)          !< available bed sediment flux
        !--- local
        real(gtm_real), parameter :: default_fines_size = 0.00002d0    !< unit m
        real(gtm_real), parameter :: default_sand_size =  0.00005d0    !< unit m
        real(gtm_real), parameter :: velocity_for_erosion = 1.0d0      !< unit m/s
        real(gtm_real), parameter :: param_M = 1.325d-6             ! kg/(m^2s)        
        real(gtm_real) :: vertical_flux(ncell)
        real(gtm_real) :: erosion_flux_nc, deposition_flux_nc
        real(gtm_real) :: conc_si(ncell), flow_si(ncell), area_si(ncell), dx_si(ncell)
        real(gtm_real) :: width_si(ncell), hydro_radius_si(ncell), diameter_si(ncell)
        real(gtm_real) :: diameter_tmp(ncell)
        real(gtm_real) :: velocity(ncell)
        real(gtm_real) :: bottom_shear_stress(ncell)
        real(gtm_real) :: shear_vel(ncell)
        real(gtm_real) :: Es
        real(gtm_real) :: c_b
        real(gtm_real) :: capital_r
        real(gtm_real) :: exp_re_p_tmp, fall_vel_tmp, critical_shear_tmp
        integer :: icell
        logical :: function_van_rijn  

        function_van_rijn = .false. !use Dietrich formula        
        
        call si_unit(conc_si, flow_si, area_si, width_si, hydro_radius_si, dx_si, diameter_si, &
                     conc, flow, area, width, hydro_radius, dx, diameter, ncell)    
                                                                        
        do icell = 1, ncell        
            velocity(icell) = abs(flow_si(icell)/area_si(icell))     
            call bed_shear_stress(bottom_shear_stress(icell),          &
                                  velocity(icell),                       &
                                  manning_n(icell),                     &
                                  hydro_radius_si(icell))          
            call shear_velocity_calculator(shear_vel(icell),           &
                                           velocity(icell),            &
                                           manning_n(icell),           &
                                           gravity,             &
                                           hydro_radius_si(icell))         
            if (((diameter_si(icell).ge.0.00200d0) .and. (diameter_si(icell).lt.0.00201d0)) &
                 .and.(velocity(icell).gt.velocity_for_erosion)) then        
                ! Non-cohesive sediment
                capital_r = sediment_density/water_density  - one
                call explicit_particle_reynolds_number(exp_re_p_tmp,       &
                                                       default_sand_size,  &
                                                       capital_r,          &
                                                       gravity,            &
                                                       kinematic_viscosity)
    
                call settling_velocity(fall_vel_tmp,                       &
                                       kinematic_viscosity,                &
                                       specific_gravity,                   &
                                       default_sand_size,                  &
                                       gravity,                            &
                                       function_van_rijn)       

                call critical_shear_stress(critical_shear_tmp,             &
                                           water_density,                  &
                                           sediment_density,               &
                                           gravity,                        &
                                           kinematic_viscosity,            &
                                           default_sand_size)            
                  
                call es_garcia_parker(Es,                                  &
                                      shear_vel(icell),                    &
                                      exp_re_p_tmp,                        &
                                      fall_vel_tmp)
                              
                call teeter(c_b,                                           &
                            shear_vel(icell),                              &            
                            fall_vel_tmp,                                  &
                            conc_si(icell))                 
                erosion_flux_nc = Es * fall_vel_tmp
                deposition_flux_nc = c_b * fall_vel_tmp                 
                if (erosion_flux_nc .gt. available_bed(icell))             &
                    erosion_flux_nc = available_bed(icell)
                erosion_flux(icell) = erosion_flux_nc
                deposition_flux(icell) = deposition_flux_nc
                vertical_flux(icell) = erosion_flux_nc - deposition_flux_nc               
            else                         
                ! Cohesive sediment
                call cohesive_erosion(erosion_flux(icell),                  &
                                      critical_shear(icell),                &
                                      bottom_shear_stress(icell),           &
                                      param_M)                                                                  
                call cohesive_deposition(deposition_flux(icell),            &
                                         fall_vel(icell),                   &
                                         conc_si(icell))               
                if (erosion_flux(icell) .gt. available_bed(icell))          &
                    erosion_flux(icell) = available_bed(icell)
                vertical_flux(icell) = erosion_flux(icell) - deposition_flux(icell)
                if (((diameter_si(icell).ge.0.00100d0).and.(erosion_flux(icell).ne.zero)) .or. &
                     (diameter_si(icell).ge.0.00300d0) ) then
                    vertical_flux(icell) = zero
                    erosion_flux(icell) = deposition_flux(icell)
                end if
            end if
            source(icell) = vertical_flux(icell)*dx_si(icell)*width_si(icell)/area_si(icell)
            if (area_si(icell) .eq. zero) source(icell) = zero           
         end do   
       
         return 
    end subroutine

    
    !> Convert all variables from English unit to SI unit
    subroutine si_unit(conc_si,        &
                       flow_si,        &
                       area_si,        &
                       width_si,       &
                       hyd_radius_si,  &
                       dx_si,          &                       
                       diameter_si,    &
                       conc,           &
                       flow,           &
                       area,           &
                       width,          &
                       hyd_radius,     &
                       dx,             &
                       diameter,       &
                       ncell) 
        implicit none
        integer, intent(in) :: ncell
        real(gtm_real), intent(out) :: conc_si(ncell)
        real(gtm_real), intent(out) :: flow_si(ncell)
        real(gtm_real), intent(out) :: area_si(ncell)
        real(gtm_real), intent(out) :: width_si(ncell)
        real(gtm_real), intent(out) :: hyd_radius_si(ncell)
        real(gtm_real), intent(out) :: dx_si(ncell)
        real(gtm_real), intent(out) :: diameter_si(ncell)
        real(gtm_real), intent(in) :: conc(ncell)
        real(gtm_real), intent(in) :: flow(ncell)
        real(gtm_real), intent(in) :: area(ncell)
        real(gtm_real), intent(in) :: width(ncell)
        real(gtm_real), intent(in) :: hyd_radius(ncell)
        real(gtm_real), intent(in) :: dx(ncell)
        real(gtm_real), intent(in) :: diameter(ncell)
        real(gtm_real), parameter :: L = 0.3048d0
        
        conc_si = conc*0.001d0         ! mg/L-->kg/m3
        flow_si = flow*L*L*L           ! cfs-->m3/s
        area_si = area*L*L             ! ft^2-->m^2
        width_si = width*L             ! ft-->m
        hyd_radius_si = hyd_radius*L   ! ft-->m
        dx_si = dx*L                   ! ft-->m
        diameter_si = diameter*0.001d0 ! mm-->m        
        return
    end subroutine    
    
    
    !> get sediment properties of explicit particle Reynolds number
    !> and critical shear stress.
    subroutine get_sediment_properties(fall_velocity,             &
                                       exp_re_p,                  &
                                       critical_shear,            &
                                       diameter,                  &
                                       ncell)
        use sediment_variables
        use suspended_utility
        implicit none
        integer, intent(in) :: ncell
        real(gtm_real), intent(in) :: diameter(ncell)
        real(gtm_real), intent(out) :: fall_velocity(ncell)
        real(gtm_real), intent(out) :: exp_re_p(ncell)
        real(gtm_real), intent(out) :: critical_shear(ncell)
        real(gtm_real), parameter :: default_fines_size = 0.00002d0    
        real(gtm_real) :: capital_r
        real(gtm_real) :: diameter_si(ncell)
        logical :: function_van_rijn
        integer :: icell
        
        function_van_rijn = .false. !use Dietrich formula
        diameter_si = diameter*0.001d0 ! mm-->m

        where (diameter_si.ge.0.00100d0) diameter_si = default_fines_size
        
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
    
            call critical_shear_stress(critical_shear(icell),           &
                                       water_density,                   &
                                       sediment_density,                &
                                       gravity,                         &
                                       kinematic_viscosity,             &
                                       diameter_si(icell))
        end do         
        return
    end subroutine    
end module    