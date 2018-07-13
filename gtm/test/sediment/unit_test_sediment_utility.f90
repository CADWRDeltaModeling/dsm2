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
!> Tests the suspended sediment utility subroutine
!>@ingroup test_sediment
module test_sediment_utility

    use fruit
    use suspended_utility
    use gtm_precision
    
    contains

    !> Test suite for suspended sediment utility
    subroutine test_all_sediment_utility
        implicit none 
        call test_settling_velocity
        call test_submerged_specific_gravity
        call test_explicit_particle_reynolds_number
        call test_particle_reynolds_number
        call test_dimless_particle_diameter
        call test_shear_velocity
        call test_rouse_number
        call test_critical_shear
        return
    end subroutine
   

    !> Tests the coarsening subroutine
    subroutine test_settling_velocity
        implicit none
        !---arg
        integer,parameter :: nclas = 5
        real(gtm_real) :: w_s                     !< Settling velocity
        real(gtm_real), parameter :: nu =1.307d-6          !< Kinematic viscosity 
        real(gtm_real), parameter :: specific_g = 2.65d0 !< Specific gravity of particle (~2.65)
        real(gtm_real) :: diameter(nclas)                !< Particle diameter in meter
        real(gtm_real), parameter :: g_accel = 9.80665d0 !< Gravitational acceleration
        real(gtm_real) :: hand_calc_value(nclas)         !< Value of the function which is known
        integer :: iclas
        ! Small value
        diameter = [0.8d-7,zero,5.0d-4,5.0d-5,2.0d-3]
        hand_calc_value = [-LARGEREAL,-LARGEREAL,0.067528489d0,0.0017194776526d0,0.197883687d0] 
        ! van Rijn     
        do iclas = 1, nclas
        call settling_velocity(w_s,             &
                               nu,              &
                               specific_g,      &
                               diameter(iclas),        &
                               g_accel,         &            
                               .true.)       
                                                                    
            call assertEquals(w_s,hand_calc_value(iclas),weak_eps,"Error in settling velocity, van Rijn, no optional input!")
        end do
        !Dietrich 
        diameter = [0.001d0,0.0001d0,0.0005d0,0.00005d0,0.000005d0]
        hand_calc_value = [1.898700795d0,0.007135499d0,0.475002507d0,0.000773401d0,0.000017195d0]
        do iclas=1,nclas  
            call settling_velocity(w_s,              &
                                   nu,               &
                                   specific_g,       &
                                   diameter(iclas),  &
                                   g_accel,          &
                                  .false.)                                                                                                 
            call assertEquals(w_s,hand_calc_value(iclas),weak_eps,"Error in settling velocity, Dietrich optional input=.false.!")
        end do

        return
    end subroutine 

    !> test submerged specific gravity
    subroutine test_submerged_specific_gravity
        implicit none
        !---args
        real(gtm_real) :: big_r
        real(gtm_real) :: rho_w
        real(gtm_real) :: rho_s
        real(gtm_real) :: hand_calc_value

        rho_w = 1000d0
        rho_s = 2650d0
        hand_calc_value = 1.65d0

        call submerged_specific_gravity(big_r,       &
                                        rho_w,       &
                                        rho_s)                                
        call assertEquals(big_r,hand_calc_value,weak_eps,"Error in submerged_specific_gravity subroutine!")

        return
    end subroutine


    !> test explicit particle reynolds number
    subroutine test_explicit_particle_reynolds_number
        implicit none
        !---args
        integer,parameter  :: nclas = 3            !< Number of sediment diameter classes
        real(gtm_real)  :: exp_re_p         !< Explicit particle reynolds number
        real(gtm_real)  :: diameter(nclas)         !< Particle diameter
        real(gtm_real)  :: capital_r               !< Submerged specific gravity of sediment particles  
        real(gtm_real)  :: g_acceleration          !< Gravitational acceleration 
        real(gtm_real)  :: kinematic_viscosity     !< Kinematic viscosity (m2/sec)
        real(gtm_real)  :: hand_calc_value(nclas)
        integer :: i

        diameter =  [1d-2,2d-2,1d-3]
        g_acceleration = 9.81d0
        capital_r = 1.65d0
        kinematic_viscosity = 1.0d-6 

        hand_calc_value =  [4023.2449589852d0,11379.455171492d0,127.22617655184d0]
        do i = 1, nclas  
            call explicit_particle_reynolds_number(exp_re_p,            &
                                                   diameter(i),         &
                                                   capital_r,           &
                                                   g_acceleration,      &
                                                   kinematic_viscosity)          
                                                                                     
            call assertEquals(hand_calc_value(i),exp_re_p,weak_eps,"Error in subroutine explicit_particle_reynolds_number!")
        end do
        return
    end subroutine


    !> test particle reynolds number
    subroutine test_particle_reynolds_number
        implicit none
        !---args
        integer, parameter :: nclas = 3       !< Number of sediment diameter classes
        real(gtm_real):: re_p          !< Particle Reynolds number
        real(gtm_real):: settling_v(nclas)    !< Settling velocity
        real(gtm_real):: diameter(nclas)      !< Particle diameter
        real(gtm_real):: kinematic_viscosity  !< Kinematic viscosity (m2/sec)
        real(gtm_real):: hand_calc_value(nclas)
        integer :: i

        diameter =  [2d-3,0.25d-3,0.031d-3] ! coarse silt medium sand and sand
        kinematic_viscosity = 1.0d-6 
        settling_v = [162d-3,25.7d-3,0.49d-3]
        hand_calc_value =  [324.0d0,6.425d0,0.01519d0]
        do i = 1, nclas 
            call particle_reynolds_number(re_p,                   &
                                          settling_v(i),          &
                                          diameter(i),            &
                                          kinematic_viscosity)                       
                                  
            call assertEquals(hand_calc_value(i),re_p,weak_eps,"Error in subroutine particle_reynolds_number!")
        end do                                         
        return
    end subroutine


    !> test dimless particle diameter
    subroutine test_dimless_particle_diameter
        implicit none
        integer, parameter :: nclas = 2        !< Number of cells
        real(gtm_real):: d_star          
        real(gtm_real):: capital_r             !< Submerged specific gravity of sediment particles  
        real(gtm_real):: g_accel               !< Gravitational acceleration 
        real(gtm_real):: diameter(nclas)       !< Particle diameter
        real(gtm_real):: kinematic_viscosity   !< Kinematic viscosity (m2/sec)
        real(gtm_real):: hand_calc_value(nclas)
        integer :: i

        diameter =  [2d-3,0.25d-3] ! coarse silt and medium sand 
        kinematic_viscosity = 1.0d-6
        g_accel = 9.81d0
        capital_r = 1.65d0

        hand_calc_value =  [50.591898800422d0,6.3239873500d0]
        do i = 1, nclas  
            call dimless_particle_diameter(d_star,              &
                                           g_accel,             &
                                           diameter(i),         &
                                           kinematic_viscosity, &
                                           capital_r)  
                                                                       
            call assertEquals(hand_calc_value(i),d_star,weak_eps,"Error in subroutine dimensionless particle number!")
        end do
        return 
    end subroutine


    !> test critical shields parameter
    subroutine test_critical_shields_parameter
        implicit none
        integer, parameter :: ncell = 6           !< Number of cells
        real(gtm_real):: d_star(ncell)          
        real(gtm_real):: cr_shields_prmtr  !< Critical Shields parameter                                      
        real(gtm_real):: hand_calc_value(ncell)
        integer :: icell

        d_star =  [160d0,21d0,15d0,10d0,2d0,1d0] ! coarse silt and medium sand 

        hand_calc_value =  [0.055d0,               &
                            0.031433080718165d0,   &
                            0.030510608231307d0,   &
                            0.03177312938897126d0, &
                            0.12d0,                &
                            0.24d0]
                            
        do icell=1,ncell
            call critical_shields_parameter_Yalin(cr_shields_prmtr, &
                                                  d_star(icell))
                                 
            call assertEquals(hand_calc_value(icell),cr_shields_prmtr,weak_eps,"Error in subroutine critical_shields_parameter!")             
        end do
        return 
    end subroutine

    !> test critical shear stress
    subroutine test_critical_shear
        implicit none
        integer, parameter :: ncell = 3               !< Number of cell
        real(gtm_real):: crtical_shear                                            
        real(gtm_real):: hand_calc_value(ncell) 
        real(gtm_real), parameter :: water_density = 1000.d0
        real(gtm_real), parameter :: sediment_density = 2650.d0
        real(gtm_real), parameter :: g_acceleration = 9.80665d0
        real(gtm_real), parameter :: kinematic_viscosity =  1.307d-6
        real(gtm_real) :: diameter(ncell)
        integer :: icell
        
        diameter = [0.00001d0, 0.0001d0, 0.001d0]
        
        hand_calc_value = [0.144042705d0, 0.181339027d0, 0.22829363d0]
        do icell = 1, ncell
            call critical_shear_stress(crtical_shear,           &
                                       water_density,           &
                                       sediment_density,        &
                                       g_acceleration,          &
                                       kinematic_viscosity,     &
                                       diameter(icell))
        
            call assertEquals(hand_calc_value(icell),crtical_shear,weak_eps,"Error in subroutine critical_shear!")
        end do 
        
        return
    end subroutine    


    !> test shear velocity
    subroutine test_shear_velocity
        implicit none
        integer, parameter :: ncell = 3          !< Number cells 
        real(gtm_real):: vel(ncell)              !< Velocity          
        real(gtm_real):: manning_n(ncell)        !< Manning's n                                     
        real(gtm_real):: hand_calc_value(ncell)  !< The sought output 
        real(gtm_real):: big_r(ncell)            !< Hydraulic radius 
        real(gtm_real):: gravity                 !< Gravity
        real(gtm_real):: shear_v          !< Shear velocity 
        integer :: iclas

        vel =  [1.1d0,.7d0,-1.5d0]    ! values for a river
        manning_n = [0.02d0,0.03d0,0.045d0]
        gravity = 9.8d0
        big_r = [three,five,seven]

        hand_calc_value =  [ 0.057347634619921d0,   0.050273292832295d0,   0.152780222207618d0]
        do iclas=1,ncell
            call shear_velocity_calculator(shear_v,          &
                                           vel(iclas),       &
                                           manning_n(iclas), &
                                           gravity,          &
                                           big_r(iclas))                      
         
            call assertEquals(hand_calc_value(iclas),shear_v,weak_eps,"Error in subroutine Shear Velocity!")
        end do

        return 
    end subroutine


    !> test rouse number
    subroutine test_rouse_number()
        implicit none
        integer, parameter :: nclas   = 2
        integer, parameter :: nvolume = 3
        real(gtm_real) :: rouse_num   !< Rouse dimensionless number  
        real(gtm_real) :: fall_vel(nclas)            !< Settling velocity
        real(gtm_real) :: shear_vel(nvolume)         !< Shear velocity 
        real(gtm_real) :: hand_value(nvolume,nclas)  !< Calculated values 
        !---local
        integer:: iclas, icell

        fall_vel  = [0.001d0, 0.1d0]
        shear_vel = [one,two,five]/ten

        hand_value = reshape ([0.024390244d0,	0.012195122d0,	0.004878049d0, &
                               2.439024390d0,	1.219512195d0,	0.487804878d0 ],[3,2])
                               
        do iclas=1,nclas
            do icell =1, nvolume
                call rouse_dimensionless_number(rouse_num,              &
                                                fall_vel(iclas),        &
                                                shear_vel(icell))
                call assertEquals(hand_value(icell,iclas),rouse_num,weak_eps,"Error in subroutine Rouse number!")
            end do
        end do
                                
        return
    end subroutine

end module
