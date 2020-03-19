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

!> Routines provide the calculation for non-cohesive suspended sediment erosion and deposition functions.
!>@ingroup sediment

module non_cohesive_source

    use gtm_precision

    contains

    !> Non-cohesive erosion flux
    subroutine non_cohesive_erosion(erosion_flux,      &
                                    shear_vel,         &
                                    exp_re_p,          &
                                    fall_vel,          &
                                    bed_frac)
        implicit none
        real(gtm_real), intent(out) :: erosion_flux
        real(gtm_real), intent(in) :: shear_vel
        real(gtm_real), intent(in) :: exp_re_p
        real(gtm_real), intent(in) :: fall_vel
        real(gtm_real) :: Es
        real(gtm_real), intent(in) :: bed_frac
        call es_garcia_parker(Es,                       &
                              shear_vel,                &
                              exp_re_p,                 &
                              fall_vel)
        erosion_flux = Es * fall_vel * bed_frac
        return
    end subroutine


    subroutine non_cohesive_deposition_(deposition_flux,        &
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

        if (bottom_shear_stress.lt.critical_shear_stress) then
            deposition_flux = settling_velocity * conc *(one-bottom_shear_stress/critical_shear_stress)
        else
            deposition_flux = settling_velocity * conc
        end if

        return
    end subroutine

    !> Non-cohesive deposition flux
    subroutine non_cohesive_deposition(deposition_flux,       &
                                       shear_vel,             &
                                       fall_vel,              &
                                       conc,                  &
                                       critical_shear_stress, &
                                       bottom_shear_stress)
        use sediment_variables, only : kappa, water_density
        implicit none
        real(gtm_real), intent(out) :: deposition_flux
        real(gtm_real), intent(in) :: shear_vel
        real(gtm_real), intent(in) :: fall_vel
        real(gtm_real), intent(in) :: conc
        real(gtm_real), intent(in) :: critical_shear_stress
        real(gtm_real), intent(in) :: bottom_shear_stress
        !---local
        real(gtm_real) :: c_b          !< Sediment into deposition
        real(gtm_real) :: Pe           !< Peclet number
        real(gtm_real) :: Pd           !< probability for deposition
        real(gtm_real) :: ro

        ! Deposition by Teeter(1988)
        Pe = 15.d0*fall_vel/(bottom_shear_stress/water_density)**0.5d0
        if (bottom_shear_stress.lt.critical_shear_stress) then
            Pd = (one - bottom_shear_stress/critical_shear_stress)
            ro = one + Pe/(1.25d0+4.75d0*Pd**2.50d0)
        else
            Pd = zero
        end if
        ro = one + Pe/(1.25d0+4.75d0*Pd**2)
        c_b = ro * conc
        deposition_flux = c_b * fall_vel

        return
    end subroutine

    !> Deposition by Parker(1982) estimated from the Rouse profile for rivers
    subroutine parker_rouse_profile(c_b,          &
                                    shear_v,      &
                                    settling_v,   &
                                    conc)
        implicit none
        !-- arg
        real(gtm_real), intent(out):: c_b        !< Sediment into deposition
        real(gtm_real), intent(in) :: shear_v    !< Shear Velocity
        real(gtm_real), intent(in) :: settling_v !< Settling velocity
        real(gtm_real), intent(in) :: conc       !< Sediment concentration
        !---local
        real(gtm_real) :: ro

        ro = one + 31.5d0*(shear_v/settling_v)**(-1.46d0)
        c_b = ro * conc

        return
    end subroutine

    !> Entrainment by Garcia Parker (1991)
    subroutine es_garcia_parker(big_e_sub_s,       &
                                shear_v,           &
                                exp_re_p,          &
                                settling_v)
        implicit none
        !-- arg
        real(gtm_real), intent(out):: big_e_sub_s !< Dimenssionless rate of entrainment of bed sediment into suspension (i.e., vol entrained sediment/unit bed area/time)
        real(gtm_real), intent(in) :: shear_v     !< Shear Velocity
        real(gtm_real), intent(in) :: exp_re_p    !< Explicit particle Reynolds number
        real(gtm_real), intent(in) :: settling_v  !< Settling velocity
        !---local
        real(gtm_real) :: z_u                     !< Captial z sub u a measure for strength of shear stress but it also takes into account the particle size in Garcia notation
        real(gtm_real), parameter :: cap_a = 1.3d-7  ! Constant value (see ASCE sediment manual no. 110 page 118)

        if (exp_re_p < 3.5d0) then
            z_u = 0.708d0*shear_v*(exp_re_p**0.6d0)/settling_v
        else
            z_u = shear_v*(exp_re_p**0.6d0)/settling_v
        end if
        big_e_sub_s  = cap_a*(z_u**five)/(one + (z_u**five)*cap_a/0.3d0)
        return
    end subroutine

end module