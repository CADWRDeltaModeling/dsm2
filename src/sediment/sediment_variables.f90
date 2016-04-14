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
module sediment_variables

    use gtm_precision
 
    !> Sediment constants   
    real(gtm_real), save :: gravity                      !< Acceleration of gravity; it must be in SI units (constant)
    real(gtm_real), save :: water_density                !< Water density
    real(gtm_real), save :: sediment_density             !< Sediment density
    real(gtm_real), save :: kappa                        !< von Karman's constant
    real(gtm_real), save :: kinematic_viscosity          !< Kinematic viscosity of water    
    real(gtm_real), save :: specific_gravity             !< specific gravity
    real(gtm_real), save :: capital_r                    !< submerged_specific_gravity
    real(gtm_real), save :: delta_b                      !< Bed leyer relative tickness 
    real(gtm_real), save :: floc_density                 !< Floc density
    real(gtm_real), save :: cohesive_diameter            !< Representative cohesive mixture diameter     
    real(gtm_real), save :: crit_stress_full_dep         !< Critical shear stress for full deposition    
    real(gtm_real), save :: density_wet_bulk             !< Wet bulk density of the deposit
    real(gtm_real), save :: crit_stress_partial_dep      !< Critical shear stress for partial deposition
    real(gtm_real), save :: crit_stress_surf_erosion     !< Critical shear stress for surface erosion 
    real(gtm_real), save :: density_dry_bulk             !< Dry bulk density 
    real(gtm_real), save :: ta_floc                      !< Floc strength 
    
    contains

    !> Allocate spatial sediment parameters 
    subroutine allocate_sediment_variables()
        implicit none        
        gravity                   = LARGEREAL
        water_density             = LARGEREAL
        sediment_density          = LARGEREAL
        kappa                     = LARGEREAL
        kinematic_viscosity       = LARGEREAL
        specific_gravity          = LARGEREAL
        capital_r                 = LARGEREAL
        delta_b                   = LARGEREAL
        floc_density              = LARGEREAL
        cohesive_diameter         = LARGEREAL
        crit_stress_full_dep      = LARGEREAL
        density_wet_bulk          = LARGEREAL
        crit_stress_partial_dep   = LARGEREAL
        crit_stress_surf_erosion  = LARGEREAL
        density_dry_bulk          = LARGEREAL
        ta_floc                   = LARGEREAL    
        return
    end subroutine
 
 
    !> Set sediment constants
    subroutine set_sediment_constants()                 
        implicit none
        gravity                   = 9.81d0
        water_density             = 1000d0
        sediment_density          = 2600d0
        kappa                     = 0.41d0
        kinematic_viscosity       = 1.307d-6                                ! @ 10 deg C
        specific_gravity          = sediment_density/water_density
        capital_r                 = sediment_density/water_density  - one  
        delta_b                   = 0.05d0
        floc_density              = LARGEREAL
        cohesive_diameter         = LARGEREAL
        crit_stress_full_dep      = LARGEREAL
        density_wet_bulk          = LARGEREAL
        crit_stress_partial_dep   = LARGEREAL
        crit_stress_surf_erosion  = LARGEREAL
        density_dry_bulk          = LARGEREAL
        ta_floc                   = LARGEREAL      
        return
    end subroutine
     

    !> Deallocate the sediment static variable
    !> and reset ncell and nvar to zero.
    subroutine deallocate_sediment_variables()
        implicit none       
        gravity                   = LARGEREAL
        water_density             = LARGEREAL
        sediment_density          = LARGEREAL
        kappa                     = LARGEREAL
        kinematic_viscosity       = LARGEREAL
        specific_gravity          = LARGEREAL
        capital_r                 = LARGEREAL
        delta_b                   = LARGEREAL
        floc_density              = LARGEREAL
        cohesive_diameter         = LARGEREAL
        crit_stress_full_dep      = LARGEREAL
        density_wet_bulk          = LARGEREAL
        crit_stress_partial_dep   = LARGEREAL
        crit_stress_surf_erosion  = LARGEREAL
        density_dry_bulk          = LARGEREAL
        ta_floc                   = LARGEREAL                          
        return
    end subroutine


 
end module
