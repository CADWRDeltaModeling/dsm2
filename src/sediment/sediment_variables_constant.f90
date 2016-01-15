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

!> Defines the common variables for the sediment transport sources.
!> The module defines the basic parameters for the sediment transport sources based
!>@ingroup sediment
module sediment_variables

    use gtm_precision
 
    !> Sediment constants   
    real(gtm_real), save :: gravity                  !< Acceleration of gravity; it must be in SI units (constant)
    real(gtm_real), save :: water_density            !< Water density
    real(gtm_real), save :: sediment_density         !< Sediment density
    real(gtm_real), save :: kappa                    !< von Karman's constant
    real(gtm_real), save :: kinematic_viscosity      !< Kinematic viscosity of water    
    real(gtm_real), save :: floc_density             !< Floc density
    real(gtm_real), save :: cohesive_diameter        !< Representative cohesive mixture diameter     
    real(gtm_real), save :: crit_stress_full_dep     !< Critical shear stress for full deposition    
    real(gtm_real), save :: density_wet_bulk         !< Wet bulk density of the deposit
    real(gtm_real), save :: crit_stress_partial_dep  !< Critical shear stress for partial deposition
    real(gtm_real), save :: crit_stress_surf_erosion !< Critical shear stress for surface erosion 
    real(gtm_real), save :: density_dry_bulk         !< Dry bulk density 
    real(gtm_real), save :: ta_floc                  !< Floc strength 
    
    !> Spatial sediment parameters
    real(gtm_real), save, allocatable :: manning(:)  !< Manning's n 
    real(gtm_real), save, allocatable :: width(:)    !< Channel width 
    real(gtm_real), save, allocatable :: diameter(:) !< Particle diameter    
    
    contains
      
    !> Set sediment constants 
    subroutine set_sediment_constants
        use error_handling
        implicit none
    
        real(gtm_real) :: gravity                     !< Gravitational acceleration in SI unit
        real(gtm_real) :: water_density               !< Water density
        real(gtm_real) :: sediment_density            !< Sediment density
        real(gtm_real) :: kappa                       !< von Karman's constant
        real(gtm_real) :: kinematic_viscosity         !< Kinematic viscosity of water    
        real(gtm_real) :: floc_density                !< Floc density
        real(gtm_real) :: cohesive_diameter           !< Representative cohesive mixture diameter       
        real(gtm_real) :: crit_stress_full_dep        !< Critical shear stress for full deposition    
        real(gtm_real) :: density_wet_bulk            !< Wet bulk density of the deposit
        real(gtm_real) :: crit_stress_partial_dep     !< Critical shear stress for partial deposition
        real(gtm_real) :: crit_stress_surf_erosion    !< Critical shear stress for surface erosion 
        real(gtm_real) :: density_dry_bulk            !< Dry bulk density 
        real(gtm_real) :: ta_floc                     !< Floc strength 
 
        gravity                   = LARGEREAL
        water_density             = LARGEREAL
        sediment_density          = LARGEREAL
        kappa                     = LARGEREAL
        kinematic_viscosity       = LARGEREAL
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
    
    
    !> Allocate spatial sediment parameters 
    subroutine allocate_sediment_parameters(a_ncell,a_nclass)
        use error_handling
        implicit none        
        integer,intent(in):: a_ncell      !< Number of cells
        integer,intent(in):: a_nclass     !< Number of sediment classes
        integer :: istat = 0
        character(len=128) :: message        
        allocate(manning(a_ncell), stat = istat)
        allocate(width(a_ncell), stat = istat)
        allocate(diameter(a_nclass), stat = istat)       
         if (istat .ne. 0 )then
            call gtm_fatal(message)
         end if        
        manning   = LARGEREAL
        width     = LARGEREAL
        diameter  = LARGEREAL
        return
    end subroutine
 
 
    !> Deallocate the sediment static variable
    !> and reset ncell and nvar to zero.
    subroutine deallocate_sediment_static()
        implicit none       
        deallocate(manning)
        deallocate(width)
        deallocate(diameter)
        return
    end subroutine
 
    !> Set sediment values
    subroutine set_sediment_values(gravity,                  & 
                                   water_density,            &
                                   sediment_density,         &        
                                   kappa,                    &                   
                                   kinematic_viscosity,      &     
                                   floc_density,             &            
                                   cohesive_diameter,        &       
                                   crit_stress_full_dep,     &    
                                   density_wet_bulk,         &        
                                   crit_stress_partial_dep,  & 
                                   crit_stress_surf_erosion, &
                                   density_dry_bulk,         &        
                                   ta_floc)                 
        implicit none
        real(gtm_real),intent(inout):: gravity                     !< Gravitational acceleration in SI unit
        real(gtm_real),intent(inout):: water_density               !< Water density
        real(gtm_real),intent(inout):: sediment_density            !< Sediment density
        real(gtm_real),intent(inout):: kappa                       !< von Karman's constant
        real(gtm_real),intent(inout):: kinematic_viscosity         !< Kinematic viscosity of water    
        real(gtm_real),intent(inout):: floc_density                !< Floc density
        real(gtm_real),intent(inout):: cohesive_diameter           !< Representative cohesive mixture diameter       
        real(gtm_real),intent(inout):: crit_stress_full_dep        !< Critical shear stress for full deposition    
        real(gtm_real),intent(inout):: density_wet_bulk            !< Wet bulk density of the deposit
        real(gtm_real),intent(inout):: crit_stress_partial_dep     !< Critical shear stress for partial deposition
        real(gtm_real),intent(inout):: crit_stress_surf_erosion    !< Critical shear stress for surface erosion 
        real(gtm_real),intent(inout):: density_dry_bulk            !< Dry bulk density 
        real(gtm_real),intent(inout):: ta_floc                     !< Floc strength 
 
        gravity                   = 9.81d0
        water_density             = 1000d0
        sediment_density          = 2600d0
        kappa                     = 0.41d0
        kinematic_viscosity       = 1.307d6    ! 10 deg C
        floc_density              = LARGEREAL
        cohesive_diameter         = LARGEREAL
        crit_stress_full_dep      = LARGEREAL
        density_wet_bulk          = LARGEREAL
        crit_stress_partial_dep   = LARGEREAL
        crit_stress_surf_erosion  = LARGEREAL
        density_dry_bulk          = LARGEREAL
        ta_floc                   = LARGEREAL      
 
    end subroutine
 
end module
