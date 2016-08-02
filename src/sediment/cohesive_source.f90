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
                               ncell)              !< number of model cells
        use sediment_variables, only : kinematic_viscosity, specific_gravity, gravity
        use suspended_utility
  use non_cohesive_source

        implicit none
        real(gtm_real), intent(out):: vertical_flux(ncell)  !< vertical sediment net flux into the water column
        real(gtm_real), intent(out):: erosion_flux(ncell)   !< entrainment for resuspension
        real(gtm_real), intent(out):: deposition_flux(ncell)!< deposition
        real(gtm_real), intent(in) :: conc(ncell)           !< concentration at new time
        real(gtm_real), intent(in) :: flow(ncell)           !< flow
        real(gtm_real), intent(in) :: area(ncell)           !< area
        real(gtm_real), intent(in) :: width(ncell)          !< channel width
        real(gtm_real), intent(in) :: hydro_radius(ncell)   !< hydraulic radius
        real(gtm_real), intent(in) :: manning(ncell)        !< Manning's n
        real(gtm_real), intent(in) :: diameter(ncell)       !< diameter in meter
        integer, intent(in) :: ncell                        !< number of cells 
        
        !--local variables
        real(gtm_real), parameter :: param_M = 1.325d-6 !1.325d-5             ! kg/(m^2s)
        real(gtm_real) :: critical_shear(ncell)
        real(gtm_real) :: fall_vel(ncell) 
        real(gtm_real) :: velocity(ncell)                           ! flow velocity   
        real(gtm_real) :: bottom_shear_stress(ncell)
        real(gtm_real) :: diameter_tmp(ncell)
        logical   :: function_van_rijn      
        integer :: icell 
real(gtm_real) :: vertical_flux_nc(ncell), Es(ncell), c_b(ncell)

        
        diameter_tmp = diameter
        where (diameter.ge.0.00040d0) diameter_tmp = 0.00002d0
        
        function_van_rijn = .false. !use Dietrich formula                
        velocity = abs(flow/area)
        critical_shear = 0.179813408d0 !0.25d0 ! Pa
        
        call settling_velocity(fall_vel,            &
                               kinematic_viscosity, &
                               specific_gravity,    &
                               diameter_tmp,            &
                               gravity,             &
                               ncell,               &
                               function_van_rijn)   

        call bed_shear_stress(bottom_shear_stress,  &
                              velocity,             &
                              manning,              &
                              hydro_radius,         &
                              ncell)
                                
        call erosion(erosion_flux,           &
                     critical_shear,         &
                     bottom_shear_stress,    &
                     param_M,                &
                     ncell)                               

        call deposition0(deposition_flux,       &
                         critical_shear,        &
                         bottom_shear_stress,   &    
                         fall_vel,              &
                         conc,                  &
                         ncell)
                         

        diameter_tmp = 0.00005d0
        call source_non_cohesive(vertical_flux_nc,    &
                                 Es,               &
                                 c_b,              &
                                 conc,             &
                                 flow,             &
                                 area,             &
                                 width,            &
                                 hydro_radius,     &
                                 manning,          &
                                 diameter_tmp,         &
                                 ncell)


                         
        !call deposition(deposition_flux,   &
        !                fall_vel, &
        !                conc,              &
        !                ncell)       
                           
        vertical_flux = erosion_flux - deposition_flux
        do icell = 1, ncell
            if ((diameter(icell).ge.0.00040d0 .and. velocity(icell).gt.0.1d0).or.(diameter(icell).ge.0.00050d0)) then
                vertical_flux(icell) = zero
            end if    
            if ((diameter(icell).ge.0.00045d0 .and. diameter(icell).lt.0.00046d0).and.(velocity(icell).gt.1.0d0)) then
                vertical_flux(icell) = vertical_flux_nc(icell)
            end if 
        end do
        !where (diameter.ge.0.00020d0) vertical_flux = zero
        !write(107,'(5f15.10)') fall_vel(743), bottom_shear_stress(743), conc(743), erosion_flux(743),deposition_flux(743)
         
        return
    end subroutine 
    
    !> Deposition flux calculated by Krone(1962)
    subroutine deposition(deposition_flux,   &
                          settling_velocity, &
                          conc,              &
                          ncell)
        implicit none
        integer, intent(in) :: ncell
        real(gtm_real), intent(in) :: conc(ncell)
        real(gtm_real), intent(in) :: settling_velocity(ncell)
        real(gtm_real), intent(out) :: deposition_flux(ncell)
        
        deposition_flux = settling_velocity * conc
        
        return
    end subroutine


    !> Deposition flux calculated by Krone(1962)
    subroutine deposition0(deposition_flux,       &
                           critical_shear_stress, &
                           bottom_shear_stress,   &    
                           settling_velocity,     &
                           conc,                  &
                           ncell)
        implicit none
        integer, intent(in) :: ncell
        real(gtm_real), intent(in) :: bottom_shear_stress(ncell)
        real(gtm_real), intent(in) :: critical_shear_stress(ncell)        
        real(gtm_real), intent(in) :: conc(ncell)
        real(gtm_real), intent(in) :: settling_velocity(ncell)
        real(gtm_real), intent(out) :: deposition_flux(ncell)
        
        deposition_flux = settling_velocity * conc
        where (bottom_shear_stress < critical_shear_stress) &
             deposition_flux = settling_velocity * conc * (one - bottom_shear_stress/critical_shear_stress)
        return
    end subroutine

    !> Erosion flux calculated by Krone(1962)
    subroutine erosion(erosion_rate,           &
                       critical_shear_stress,  &
                       bottom_shear_stress,    &
                       param_M,                &
                       ncell)
        implicit none
        integer, intent(in) :: ncell
        real(gtm_real), intent(in) :: bottom_shear_stress(ncell)
        real(gtm_real), intent(in) :: critical_shear_stress(ncell)
        real(gtm_real), intent(in) :: param_M
        real(gtm_real), intent(out) :: erosion_rate(ncell)
                
        erosion_rate = param_M * (bottom_shear_stress/critical_shear_stress-one)      
        where (bottom_shear_stress .le. critical_shear_stress) erosion_rate = zero
        
        return
    end subroutine

    !> Bed shear stress
    subroutine bed_shear_stress(bed_shear,     &
                                velocity,      &
                                manning,       &
                                hydro_radius,  &
                                ncell)
        use sediment_variables, only: water_density, gravity
        implicit none
        integer, intent(in) :: ncell
        real(gtm_real), intent(in) :: velocity(ncell)
        real(gtm_real), intent(in) :: manning(ncell)
        real(gtm_real), intent(in) :: hydro_radius(ncell)
        real(gtm_real), intent(out) :: bed_shear(ncell)
        
        bed_shear = water_density*velocity**two*(manning**two)*gravity/(hydro_radius**(one/three))
        
        return
    end subroutine    


end module