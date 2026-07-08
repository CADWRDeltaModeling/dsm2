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

!> Module orchestrating the advection scheme. The main
!> routine in the module is advection().
!>@ingroup transport
module mass_balance

    contains
    !> Perform mass balance to calculate "predicted" diffusive flux at cell level
    !> predicted diffusive flux = actual mass change  - advective flux

    subroutine expected_net_diffusive_flux(mass,                 &
                                           mass_prev,            &
                                           ncell,                &
                                           nvar,                 &
                                           dt,                   &
                                           dx,                   &
                                           advective_div_flux,   &
                                           mass_adv_adjust,      &
                                           calc_net_diffusive_flux)

        use constants
        use IO_Units
        use primitive_variable_conversion
        use gradient
        use advection
        use source_sink
        use boundary_advection
        use boundary_concentration
        use gradient_adjust
        use boundary_concentration
        use common_vars, only: n_node, n_qext

        implicit none

        !--- args
        real(gtm_real),intent(out) :: calc_net_diffusive_flux(ncell,nvar)       !< net diffusive flux at new time
        integer, intent(in) :: ncell                         !< Number of cells
        integer, intent(in) :: nvar                          !< Number of variables
        real(gtm_real),intent(in)  :: mass(ncell,nvar)       !< mass at new time
        real(gtm_real),intent(in)  :: mass_prev(ncell,nvar)  !< mass at old time
        real(gtm_real),intent(in)  :: advective_div_flux(ncell,nvar)  !< advective flux divergence
        real(gtm_real),intent(in)  :: dt                              !< current time step from old time to new time
        real(gtm_real),intent(in)  :: dx(ncell)                       !< spatial step
        real(gtm_real),intent(in)  :: mass_adv_adjust(ncell,nvar)  !< mass adjustment at the end of the advection step
        !-----locals
        real(gtm_real):: mass_change(ncell,nvar)  !< actual mass change calculated from conc*area change between new and old time step
        integer :: icell, ivar

        mass_change = mass(:,:) - mass_prev(:,:)
        do icell = 1,ncell
            do ivar = 1,nvar
                calc_net_diffusive_flux(icell,ivar) = - mass_change(icell,ivar)/(dt/dx(icell)) - advective_div_flux(icell,ivar) + mass_adv_adjust(icell,ivar)/(dt/dx(icell))
            end do
        end do
        return
    end subroutine

end module