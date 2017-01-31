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
module cohesive_source

    use gtm_precision

    contains 
    
    !> Calculate vertical net flux for cohesive sediment
    subroutine source_cohesive(vertical_flux,    & !< net vertical sediment flux
                               erosion_flux,     & !< erosion flux
                               deposition_flux,  & !< deposition flux
                               conc,             & !< sediment concentration
                               flow,             & !< flow
                               area,             & !< area
                               width,            & !< channel width
                               hydro_radius,     & !< hydraulic radius
                               manning,          & !< Manning's n
                               diameter,         & !< sediment particle diameter
                               ncell,            & !< number of model cells
                               available_bed)      !< available bed sediment flux
        use sediment_variables
        use suspended_utility

        implicit none
        real(gtm_real), intent(out):: vertical_flux(ncell)     !< vertical sediment net flux into the water column
        real(gtm_real), intent(out):: erosion_flux(ncell)      !< entrainment for resuspension
        real(gtm_real), intent(out):: deposition_flux(ncell)   !< deposition
        real(gtm_real), intent(in) :: conc(ncell)              !< concentration at new time
        real(gtm_real), intent(in) :: flow(ncell)              !< flow
        real(gtm_real), intent(in) :: area(ncell)              !< area
        real(gtm_real), intent(in) :: width(ncell)             !< channel width
        real(gtm_real), intent(in) :: hydro_radius(ncell)      !< hydraulic radius
        real(gtm_real), intent(in) :: manning(ncell)           !< Manning's n
        real(gtm_real), intent(in) :: diameter(ncell)          !< diameter in meter
        integer, intent(in) :: ncell                           !< number of cells
        real(gtm_real), intent(in) :: available_bed(ncell)     !< available bed sediment flux
        
        !--local variables
        real(gtm_real), parameter :: param_M = 1.325d-6        ! kg/(m^2s)
        real(gtm_real) :: critical_shear(ncell)
        real(gtm_real) :: fall_vel(ncell) 
        real(gtm_real) :: velocity(ncell)                      ! flow velocity   
        real(gtm_real) :: bottom_shear_stress(ncell)
        logical   :: function_van_rijn      
        integer :: icell
        
        function_van_rijn = .false. !use Dietrich formula                
        velocity = abs(flow/area)
        
        do icell = 1, ncell
            call settling_velocity(fall_vel(icell),           &
                                   kinematic_viscosity,       &
                                   specific_gravity,          &
                                   diameter(icell),           &
                                   gravity,                   &
                                   function_van_rijn)   

            call critical_shear_stress(critical_shear(icell), &
                                       water_density,         &
                                       sediment_density,      &
                                       gravity,               &
                                       kinematic_viscosity,   &
                                       diameter(icell))
                                     
            call bed_shear_stress(bottom_shear_stress(icell), &
                                  velocity(icell),            &
                                  manning(icell),             &
                                  hydro_radius(icell))
                                
            call cohesive_erosion(erosion_flux(icell),        &
                                  critical_shear(icell),      &
                                  bottom_shear_stress(icell), &
                                  param_M)                               
                                   
            call cohesive_deposition(deposition_flux(icell),  &
                                     fall_vel(icell),         &
                                     conc(icell))       
        
            if (erosion_flux(icell) .gt. available_bed(icell)) &
                 erosion_flux(icell) = available_bed(icell)
                              
            vertical_flux(icell) = erosion_flux(icell) - deposition_flux(icell)
        end do

        return
    end subroutine 


    !> Deposition flux calculated by Krone(1962)
    subroutine cohesive_deposition(deposition_flux,   &
                                   settling_velocity, &
                                   conc)
        implicit none
        real(gtm_real), intent(in) :: conc
        real(gtm_real), intent(in) :: settling_velocity
        real(gtm_real), intent(out) :: deposition_flux
        
        deposition_flux = settling_velocity * conc
        
        return
    end subroutine


    !> Erosion flux calculated by Krone(1962)
    subroutine cohesive_erosion(erosion_rate,           &
                                critical_shear_stress,  &
                                bottom_shear_stress,    &
                                param_M)
        implicit none
        real(gtm_real), intent(in) :: bottom_shear_stress
        real(gtm_real), intent(in) :: critical_shear_stress
        real(gtm_real), intent(in) :: param_M
        real(gtm_real), intent(out) :: erosion_rate
                
        erosion_rate = param_M * (bottom_shear_stress/critical_shear_stress-one)      
        if (bottom_shear_stress .le. critical_shear_stress) erosion_rate = zero 
       
        return
    end subroutine

    !> Bed shear stress
    subroutine bed_shear_stress(bed_shear,     &
                                velocity,      &
                                manning,       &
                                hydro_radius)
        use sediment_variables, only: water_density, gravity
        implicit none
        real(gtm_real), intent(in) :: velocity
        real(gtm_real), intent(in) :: manning
        real(gtm_real), intent(in) :: hydro_radius
        real(gtm_real), intent(out) :: bed_shear
        
        bed_shear = water_density*velocity**two*(manning**two)*gravity/(hydro_radius**(one/three))        
        
        return
    end subroutine    
    
end module