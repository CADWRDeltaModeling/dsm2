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

!> Routines provide the general calculation for suspended sediment sinks/sources routines.
!> All the constant based drived variables are here
!>@ingroup sediment !todo: test or test sediment

module suspended_utility

    contains

    !> Calculates particle's settling velocity. NOTE: the subroutine works with SI units.
    !> Settling velocity formula based on Leo van Rijn (1984b).
    !> The subroutine does not consider particles smaller than 10 microns (fine clay).
    !> The smaller particles are assumed to be either part of wash load or pertain to cohesive sediment. 
    !> The subroutine is for non-cohesive particles.
    subroutine settling_velocity(settling_v,         &
                                 kinematic_viscosity,&
                                 specific_gravity,   &
                                 diameter,           &
                                 g_acceleration,     &
                                 nclass,             &
                                 function_van_rijn)                
        use gtm_precision
        implicit none
        !--- arg
        integer,intent(in)         :: nclass              !< Number of sediment diameter classes
        real(gtm_real),intent(out) :: settling_v(nclass)  !< Settling velocity (m/s)
        real(gtm_real),intent(in)  :: kinematic_viscosity !< Kinematic viscosity (m2/sec)
        real(gtm_real),intent(in)  :: specific_gravity    !< Specific gravity of particle (~2.65)
        real(gtm_real),intent(in)  :: diameter(nclass)    !< Particle diameter in meter
        real(gtm_real),intent(in)  :: g_acceleration      !< Gravitational acceleration (m/sec2)
        logical, optional          :: function_van_rijn   !< Flag for using van Rijn (1984) formula o/ Dietrich (1982). the default is van Rijn
        !--local
        logical :: van_rijn_flag
        integer :: iclass
        ! I checked the following values with the ebook on the website of Parker (UIUC)
        real(gtm_real) :: b_1 = 2.891394d0
        real(gtm_real) :: b_2 = 0.95296d0
        real(gtm_real) :: b_3 = 0.056835d0
        real(gtm_real) :: b_4 = 0.002892d0
        real(gtm_real) :: b_5 = 0.000245d0
        real(gtm_real) :: dimless_fall_velocity(nclass)  ! todo: should we consider the local varibles in 
        real(gtm_real) :: exp_re_p(nclass)               !< Explicit Reynols particle number 
        real(gtm_real) :: capital_r                      !< Submerged specific gravity of sediment particles 
 
        van_rijn_flag = .true.
        if (present(function_van_rijn)) then
            van_rijn_flag = function_van_rijn
        end if
 
        select case (van_rijn_flag)
 
            case (.true.)
            ! van Rijn formula
            do iclass=1,nclass
                if (diameter(iclass) > 1.0d-3)     then
                    settling_v(iclass) = 1.1d0*dsqrt((specific_gravity - one)*g_acceleration*diameter(iclass))
                elseif (diameter(iclass) > 1.0d-4) then
                    settling_v(iclass) = (ten*kinematic_viscosity/diameter(iclass))*(dsqrt(one + (0.01d0*(specific_gravity - one) &
                                         *g_acceleration*diameter(iclass)**three)/kinematic_viscosity**two)- one)
                elseif (diameter(iclass) > 0.9d-7) then
                    ! Stokes law here
                    settling_v(iclass) = ((specific_gravity - one)*g_acceleration*diameter(iclass)**two)/(18.0d0*kinematic_viscosity)
                else
                    settling_v(iclass) = minus * LARGEREAL
                    ! todo: the gtm_fatal can not be called here because settling velocity is a pure subroutine
                end if 
            end do   
            
            case(.false.)
            ! Dietrich formula
            capital_r = specific_gravity - one
            do iclass=1,nclass
                ! Stokes fall velocity
                if ( diameter(iclass) < 1.0d-5 ) then
                    settling_v(iclass) = (capital_r*g_acceleration*diameter(iclass)**two)/(18.0d0*kinematic_viscosity)
                else
                    call explicit_particle_reynolds_number(exp_re_p,            &
                                                           diameter,            &
                                                           capital_r,           &
                                                           g_acceleration,      &
                                                           kinematic_viscosity, &
                                                           nclass)
            
                    dimless_fall_velocity(iclass) = dexp(minus*b_1 + b_2*dlog(exp_re_p(iclass)) - b_3*(dlog(exp_re_p(iclass)))**two &
                                                   - b_4*(dlog(exp_re_p(iclass)))**three + b_5*(dlog(exp_re_p(iclass)))**four)
                                               
                    settling_v(iclass) = dimless_fall_velocity(iclass) * dsqrt(capital_r*g_acceleration*diameter(iclass))                 
                end if   
            end do 
            end select 

        return
    end subroutine


    !> Calculates the submerged specific gravity
    pure subroutine submerged_specific_gravity(capital_r,            &
                                               water_density,        &
                                               sediment_density)
        use gtm_precision
        implicit none
        !-- arguments
        real(gtm_real),intent(out) :: capital_r        !< Submerged specific gravity of sediment particles     
        real(gtm_real),intent(in)  :: water_density    !< Water density  
        real(gtm_real),intent(in)  :: sediment_density !< Solid particle density

        capital_r = sediment_density/water_density  - one                                     
        
        return 
    end subroutine


    !> Calculates the explicit particle Reynolds number
    pure subroutine explicit_particle_reynolds_number(exp_re_p,           &
                                                      diameter,           &
                                                      capital_r,          &
                                                      g_acceleration,     &
                                                      kinematic_viscosity,&
                                                      nclass)
        use gtm_precision
        implicit none
        !--- arguments 
        integer, intent(in) :: nclass                         !< Number of sediment diameter classes
        real(gtm_real),intent(out) :: exp_re_p(nclass)        !< Explicit particle reynolds number
        real(gtm_real),intent(in)  :: diameter(nclass)        !< Particle diameter
        real(gtm_real),intent(in)  :: capital_r               !< Submerged specific gravity of sediment particles  
        real(gtm_real),intent(in)  :: g_acceleration          !< Gravitational acceleration 
        real(gtm_real),intent(in)  :: kinematic_viscosity     !< Kinematic viscosity (m2/sec)

        exp_re_p = diameter*dsqrt(g_acceleration*capital_r*diameter)/kinematic_viscosity

        return
    end subroutine


    !> Calculates particle Reynolds number
    pure subroutine particle_reynolds_number(re_p,                &
                                             settling_v,          &
                                             diameter,            &
                                             kinematic_viscosity, &
                                             nclass)

        use gtm_precision
        implicit none
        !--- arguments 
        integer, intent(in) :: nclass                     !< Number of sediment diameter classes
        real(gtm_real),intent(out) :: re_p(nclass)        !< Particle Reynolds number
        real(gtm_real),intent(in)  :: settling_v(nclass)  !< Settling velocity
        ! todo: Eli; do we need settling_velocity and diameter as settling_velocity(nvar)?
        real(gtm_real),intent(in)  :: diameter(nclass)    !< Particle diameter
        real(gtm_real),intent(in)  :: kinematic_viscosity !< Kinematic viscosity (m2/sec)                            
 
        re_p = settling_v*diameter/kinematic_viscosity
 
        return
    end subroutine


    !> Calculates dimensionless particle diameter
    pure subroutine dimless_particle_diameter(d_star,                 &
                                              g_acceleration,         &
                                              diameter,               &
                                              kinematic_viscosity,    &
                                              capital_r,              &
                                              nclass)
        use gtm_precision
        implicit none
        !--- arguments 
        integer, intent(in) :: nclass                     !< Number of sediment diameter classes
        real(gtm_real),intent(out) :: d_star(nclass)      !< Dimensionless particle diameter
        real(gtm_real),intent(in)  :: g_acceleration      !< Gravitational acceleration 
        real(gtm_real),intent(in)  :: diameter(nclass)    !< Particle diameter
        real(gtm_real),intent(in)  :: kinematic_viscosity !< Kinematic viscosity (m2/sec)                            
        real(gtm_real),intent(in)  :: capital_r           !< Submerged specific gravity of sediment particles     

        d_star = diameter*(capital_r*g_acceleration/(kinematic_viscosity**two))**third
 
        return
    end subroutine


    !> Calculates critical shields parameter based on Yalin (1972) formula
    !> See van Rijn book equation (4.1.11)
    ! todo: add Parker formula here
    pure subroutine critical_shields_parameter(cr_shields_prmtr,   &
                                               d_star,             &
                                               nclass)                                           
        use gtm_precision
        implicit none
        !--- arguments  
        integer, intent(in) :: nclass                         !< Number of sediment diameter classes
        real(gtm_real),intent(out):: cr_shields_prmtr(nclass) !< Critical Shields parameter                                      
        real(gtm_real),intent(in) :: d_star(nclass)           !< Dimensionless particle diameter
        !--local
        integer :: iclass

        do iclass =1,nclass
            if (d_star(iclass) > 150.0d0) then
                cr_shields_prmtr(iclass) = 0.055d0   
            elseif (d_star(iclass) > 20.0d0) then
                cr_shields_prmtr(iclass) = 0.013d0*d_star(iclass)**0.29d0 
            elseif (d_star(iclass) > 10.0d0) then
                cr_shields_prmtr(iclass) = 0.04d0*d_star(iclass)**(-0.1d0)
            elseif (d_star(iclass) > 4.0d0)  then
                cr_shields_prmtr(iclass) = 0.14d0*d_star(iclass)**(-0.64d0)
            elseif (d_star(iclass) > one)    then
                cr_shields_prmtr(iclass) = 0.24d0/d_star(iclass)
            else
                cr_shields_prmtr(iclass) = LARGEREAL
                ! the number set here to prevent bad input (gtm_fatal can not be called)
            end if    
        end do                                     
        return
    end subroutine


    ! todo: should we assume depth = Rh? it is larger than 1/10 in the Delta 
    !> Shear velocity calculator
    subroutine shear_velocity_calculator(shear_velocity,      &
                                         velocity,            &
                                         manning,             &
                                         gravity,             &
                                         hydr_radius,         &
                                         ncell,               &
                                         si_flag)                                     
        use gtm_precision
        implicit none

        integer, intent(in) :: ncell                       !< Number of cells
        real(gtm_real),intent(in) :: velocity(ncell)       !< Flow velocity  
        real(gtm_real),intent(in) :: manning(ncell)        !< Manning's n 
        real(gtm_real),intent(in) :: hydr_radius(ncell)    !< Hydraulic radius 
        real(gtm_real),intent(in) :: gravity               !< Gravity
        real(gtm_real),intent(out):: shear_velocity(ncell) !< Shear velocity 
        logical, optional         :: si_flag               !< SI and British unit switch
        ! todo: is it safe to have si_falg optional?
        !---local
        real(gtm_real) :: phi
        logical :: flag

        flag = .true.

        if (present(si_flag)) then
            flag = si_flag
        end if

        if (flag)then
            phi = one
        else  
            phi = 1.486d0
        end if

        ! the ABS used due to the nature of shear velocity 
        shear_velocity = abs(velocity)*manning*dsqrt(gravity)/(hydr_radius**(one/six))/phi

    end subroutine


    !> Calculate Rouse number from given shear velocity 
    !> Ro # < 0.8 wash load and does not consider in sed transport
    !> Ro # (0.8~1.2) 100% suspended load
    !> Ro # (1.2~2.5) 50% suspended load
    !> Ro # (2.5~ 7 ) bedload
    !> Ro # > 7 does not move at all
    subroutine rouse_dimensionless_number(rouse_num,   &
                                          fall_vel,    &
                                          shear_vel,   &
                                          ncell,       &
                                          nclass,      &
                                          von_karman)                                 
        use gtm_precision
        implicit none
        integer,intent(in) :: nclass                         !< Number of sediment classes 
        integer,intent(in) :: ncell                          !< Number of cells
        real(gtm_real),intent(out):: rouse_num(ncell,nclass) !< Rouse dimensionless number  
        real(gtm_real),intent(in) :: fall_vel(nclass)        !< Settling velocity
        real(gtm_real),intent(in) :: shear_vel(ncell)        !< Shear velocity 
        real(gtm_real),intent(in),optional :: von_karman     !< Von karman constant, Kappa = 0.41
        !----local
        real(gtm_real) :: kappa
        integer        :: icell

        kappa = 0.41d0

        if (present(von_karman)) then
            kappa = von_karman
        end if

        do icell=1,ncell
            rouse_num(icell,:)= fall_vel/shear_vel(icell)/kappa
        end do

        return
    end subroutine


    ! The formula here is adopted from B. Greimann, Y. Lai and J. Huang, 2008
    !> subroutine to calculate the percentage in suspension
    ! todo: what should we do with large Rouse numbers? and exclude them from bedload? 
    subroutine allocation_ratio(susp_percent,    &
                                bed_percent,     &
                                rouse_num,       &
                                nclass,          &
                                ncell)                            
        use gtm_precision

        implicit none
        integer,intent(in) :: nclass                             !< Number of sediment classes 
        integer,intent(in) :: ncell                              !< Number of cells
        real(gtm_real),intent(in) :: rouse_num(ncell,nclass)     !< Rouse dimensionless number  
        real(gtm_real),intent(out):: susp_percent(ncell,nclass)  !< Percentage in suspension  
        real(gtm_real),intent(out):: bed_percent(ncell,nclass)   !< Percentage in bedload

        susp_percent = min (one,(2.5d0*dexp(-rouse_num)))
        bed_percent  = one - susp_percent
     
        return
    end subroutine 

end module
