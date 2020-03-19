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

!> Defines the common variables for the sediment transport sources.
!> The module defines the basic parameters for the sediment transport sources based
!>@ingroup sediment
module turbidity

    use gtm_precision

    ! Variables for turbidity module
    real(gtm_real), allocatable :: turbidity_decay(:)
    real(gtm_real), allocatable :: turbidity_settle(:)

    contains

    !> Allocate turbidity variables
    subroutine allocate_turbidity_variables(ncell)
        implicit none
        integer, intent(in) :: ncell  !< number of cells
        allocate(turbidity_decay(ncell),turbidity_settle(ncell))
        return
    end subroutine


    !> Deallocate turbidity variables
    subroutine deallocate_turbidity_variables()
        implicit none
        deallocate(turbidity_decay,turbidity_settle)
        return
    end subroutine


    !> turbidity decay and settling
    subroutine turbidity_source(source,        &
                                conc,          &
                                dt,            &
                                ncell,         &
                                constraint)
        use gtm_precision
        implicit none
        !--- args
        integer, intent(in) :: ncell                         !< Number of cells
        real(gtm_real), intent(inout) :: source(ncell)       !< cell centered source
        real(gtm_real), intent(in) :: conc(ncell)            !< Concentration
        real(gtm_real), intent(in) :: dt                      !< delta t
        real(gtm_real), intent(in) :: constraint(ncell)   !< constraint

        ! source must be in primitive variable
        source = - turbidity_decay*conc/dt - turbidity_settle*conc/dt

        return
    end subroutine


 end module