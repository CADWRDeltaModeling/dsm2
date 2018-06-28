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

!> Tests the suspended cohesive sediment subroutines
!>@ingroup test_sediment
module test_cohesive

    use fruit
    use gtm_precision
    use sediment_variables
    use suspended_utility
    use cohesive_source
    
    contains

    !> Test suite for cohesive subroutines
    subroutine test_all_cohesive()
        implicit none
        call test_bed_shear_stress
        call test_deposition
        call test_erosion
        return
    end subroutine

    !> test bed shear stress    
    subroutine test_bed_shear_stress()
        implicit none
        real(gtm_real) :: manning
        real(gtm_real) :: hydro_radius
        real(gtm_real) :: velocity
        real(gtm_real) :: bed_shear

        velocity = 0.3d0
        manning = 0.022d0
        hydro_radius = 2.d0
        
        call bed_shear_stress(bed_shear,     &
                              velocity,      &
                              manning,       &
                              hydro_radius)  
                                   
        call assertEquals(0.339051144542464d0,bed_shear,weak_eps,"Error in test_bed_shear_stress")
        
        return
    end subroutine    

    !> test deposition
    subroutine test_deposition()
        implicit none
        real(gtm_real) :: deposition_flux
        real(gtm_real) :: settling_velocity
        real(gtm_real) :: critical_shear
        real(gtm_real) :: bed_shear
        real(gtm_real) :: conc

        settling_velocity = 0.02d0
        conc = 0.3d0
        critical_shear = 0.02d0
        bed_shear = 0.02d0
        call cohesive_deposition(deposition_flux,   &
                                 settling_velocity, &
                                 conc, &
                                 critical_shear, &
                                 bed_shear)        
        call assertEquals(0.006d0,deposition_flux,weak_eps,"Error in test_deposition")
        
        return
    end subroutine    

    !> test erosion
    subroutine test_erosion()
        implicit none
        real(gtm_real), parameter :: param_M = 1.0d-4               ! kg/(m^2s)
        real(gtm_real), parameter :: critical_shear_stress = 0.25d0 ! Pa
        real(gtm_real) :: bottom_shear_stress
        real(gtm_real) :: erosion_rate
                
        bottom_shear_stress = 0.5d0       
        call cohesive_erosion(erosion_rate,           &
                              critical_shear_stress,  &
                              bottom_shear_stress,    &
                              param_M)  
        call assertEquals(1.0d-4,erosion_rate,weak_eps,"Error in test_erosion")

        bottom_shear_stress = 0.15d0       
        call cohesive_erosion(erosion_rate,           &
                              critical_shear_stress,  &
                              bottom_shear_stress,    &
                              param_M)  
        call assertEquals(zero,erosion_rate,weak_eps,"Error in test_erosion")
                          
        return
    end subroutine    


end module

