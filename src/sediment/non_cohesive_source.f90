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

!> Routines provide the calculation for non-cohesive suspended sediment erosion and deposition functions.
!>@ingroup sediment 

module non_cohesive_source

    use gtm_precision 
    
    contains 
    
    subroutine source_non_cohesive(vertical_flux,    &
                                   Es,               &
                                   c_b,              &
                                   conc,             &
                                   flow,             &
                                   area,             &
                                   width,            &
                                   hydro_radius,     &
                                   manning,          &
                                   diameter,         &
                                   ncell)
        use sediment_variables, only : kinematic_viscosity, specific_gravity, gravity
        use suspended_utility

        implicit none
        real(gtm_real), intent(out):: vertical_flux(ncell)  !< vertical sediment net flux into the water column
        real(gtm_real), intent(out):: Es(ncell)             !< entrainment for resuspension
        real(gtm_real), intent(out):: c_b(ncell)            !< deposition
        real(gtm_real), intent(in) :: conc(ncell)           !< concentration at new time
        real(gtm_real), intent(in) :: flow(ncell)           !< flow
        real(gtm_real), intent(in) :: area(ncell)           !< area
        real(gtm_real), intent(in) :: width(ncell)          !< channel width
        real(gtm_real), intent(in) :: hydro_radius(ncell)   !< hydraulic radius
        real(gtm_real), intent(in) :: manning(ncell)        !< Manning's n
        real(gtm_real), intent(in) :: diameter              !< diameter
        integer, intent(in) :: ncell                        !< number of cells 

        !---local
        real(gtm_real) :: velocity(ncell)                   !< flow velocity
        real(gtm_real) :: c_bar_bed(ncell)                  !< near bed vaule of mean volumetric sediment concentration
        real(gtm_real) :: fall_vel                          !< settling velocity         
        real(gtm_real) :: big_e_sub_s(ncell)                !< dimenssionless rate of entrainment of bed sediment into suspension  
        real(gtm_real) :: shear_vel(ncell)                  !< shear velocity   
        real(gtm_real) :: exp_re_p                          !< explicit particle reynolds number  
        integer :: iclass                                   !< counter on grain class  
        integer :: i  
        logical   :: function_van_rijn  
        real(gtm_real) :: capital_r

        function_van_rijn = .false. !use Dietrich formula
        velocity = abs(flow/area)        
        capital_r = specific_gravity - one
        
        call settling_velocity(fall_vel,           &
                               kinematic_viscosity,&
                               specific_gravity,   &
                               diameter,           &
                               gravity,            &
                               function_van_rijn)
                       
        call explicit_particle_reynolds_number(exp_re_p,           &
                                               diameter,           &
                                               capital_r,          &
                                               gravity,            &
                                               kinematic_viscosity)

        call shear_velocity_calculator(shear_vel,           &
                                       velocity,            &
                                       manning,             &
                                       gravity,             &
                                       hydro_radius,        &
                                       ncell)     
                                                                                  
        call es_garcia_parker(Es,                &
                              shear_vel,         &
                              exp_re_p,          &
                              fall_vel,          & 
                              ncell)
                              
        call teeter(c_b,          &
                    shear_vel,    &                                   
                    fall_vel,     &
                    conc,         &
                    ncell)
                    
        vertical_flux = (Es-c_b)*fall_vel  
                             
        return
    end subroutine 


    !> Entrainment by Garcia Parker (1991)
    subroutine es_garcia_parker(big_e_sub_s,       &
                                shear_v,           &
                                exp_re_p,          &
                                settling_v,        & 
                                ncell)
        implicit none
        !-- arg
        integer, intent(in):: ncell                      !< Number of computational volumes in a channel
        real(gtm_real), intent(out):: big_e_sub_s(ncell) !< Dimenssionless rate of entrainment of bed sediment into suspension (i.e., vol entrained sediment/unit bed area/time)                                       
        real(gtm_real), intent(in) :: shear_v(ncell)     !< Shear Velocity
        real(gtm_real), intent(in) :: exp_re_p           !< Explicit particle Reynolds number
        real(gtm_real), intent(in) :: settling_v         !< Settling velocity
        !---local
        real(gtm_real) :: z_u(ncell)                     !< Captial z sub u a measure for strength of shear stress but it also takes into account the particle size in Garcia notation
        real(gtm_real), parameter :: cap_a = 1.3d-7      ! Constant value (see ASCE sediment manual no. 110 page 118)
        integer :: icell

        if (exp_re_p < 3.5d0) then
            z_u = 0.708d0*shear_v*(exp_re_p**0.6d0)/settling_v
        else
            z_u = shear_v*(exp_re_p**0.6d0)/settling_v
        end if
        big_e_sub_s  = cap_a*(z_u**five)/(one + (z_u**five)*cap_a/0.3d0)                                  
        return                             
    end subroutine
    
    !> Deposition by Parker(1982) estimated from the Rouse profile for rivers 
    subroutine parker_rouse_profile(c_b,          &
                                    shear_v,      &                                   
                                    settling_v,   &
                                    conc,         &
                                    ncell)
        implicit none
        !-- arg
        integer, intent(in):: ncell                    !< Number of computational volumes in a channel
        real(gtm_real), intent(out):: c_b(ncell)       !< Sediment into deposition
        real(gtm_real), intent(in) :: shear_v(ncell)   !< Shear Velocity
        real(gtm_real), intent(in) :: settling_v       !< Settling velocity
        real(gtm_real), intent(in) :: conc(ncell)      !< Sediment concentration
        !---local
        real(gtm_real) :: ro(ncell)
        integer :: icell

        ro = one + 31.5d0*(shear_v/settling_v)**(-1.46d0)
        c_b = ro * conc
          
        return
    end subroutine


    !> Deposition by Teeter(1988) 
    subroutine teeter(c_b,          &
                      shear_v,      &                                   
                      settling_v,   &
                      conc,         &
                      ncell)
        use sediment_variables, only : kappa
        implicit none
        !-- arg
        integer, intent(in):: ncell                  !< Number of computational volumes in a channel
        real(gtm_real), intent(out):: c_b(ncell)     !< Sediment into deposition
        real(gtm_real), intent(in) :: shear_v(ncell) !< Shear Velocity
        real(gtm_real), intent(in) :: settling_v     !< Settling velocity
        real(gtm_real), intent(in) :: conc(ncell)    !< Sediment concentration
        !---local
        real(gtm_real) :: Pe(ncell)                  !< Peclet number
        real(gtm_real) :: ro(ncell)
        integer :: icell
        
        Pe = six*settling_v/kappa/shear_v
        ro = one + Pe/1.25d0
        c_b = ro * conc
       
        return
    end subroutine            
    

end module 