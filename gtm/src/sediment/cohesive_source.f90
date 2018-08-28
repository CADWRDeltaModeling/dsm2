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
!> Routines provide the calculation for cohesive suspended sediment erosion and deposition functions.
!>@ingroup sediment 
module cohesive_source

    use gtm_precision
    

    contains 
    

    !> Deposition flux calculated by Krone(1962)
    subroutine cohesive_deposition(deposition_flux,        &
                                   settling_velocity,      &
                                   conc,                   &
                                   critical_shear_stress,  &
                                   bottom_shear_stress)
        implicit none
        real(gtm_real), intent(in) :: conc
        real(gtm_real), intent(in) :: settling_velocity
        real(gtm_real), intent(in) :: critical_shear_stress
        real(gtm_real), intent(in) :: bottom_shear_stress        
        real(gtm_real), intent(out) :: deposition_flux

        if (bottom_shear_stress.gt.critical_shear_stress) then
            deposition_flux = zero
        else
            deposition_flux = settling_velocity * conc * (one-bottom_shear_stress/critical_shear_stress)        
        end if
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