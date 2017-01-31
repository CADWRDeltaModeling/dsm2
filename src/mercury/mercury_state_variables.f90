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
!> 
!>@ingroup mercury

module mercury_state_variables

    use gtm_precision
   
    real(gtm_real), allocatable :: temperature(:)

    real(gtm_real) :: dgm_ratio    !< list of static input parameters
    real(gtm_real) :: rct_interface
    real(gtm_real) :: rct_water
    real(gtm_real) :: solid_in
    real(gtm_real) :: vol_frac                                             

    contains

    subroutine allocate_mercury(ncell,nlayers) 
        implicit none
        integer, intent(in) :: ncell
        integer, intent(in) :: nlayers
        allocate(temperature(ncell))
    
        return
    end subroutine
    
    subroutine deallocate_mercury()
        implicit none
        deallocate(temperature)
    
        return
    end subroutine 
 
 
end module