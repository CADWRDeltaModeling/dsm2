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
    
    subroutine deposition_flux(flow, &
                               area, &
                               ncell)
        implicit none
        integer, intent(in) :: ncell
        real(gtm_real), intent(in) :: flow(ncell)
        real(gtm_real), intent(in) :: area(ncell)
        
        !bed_shear_velocity = 0.5d0*water_density*friction_factor*velocity
        return
    end subroutine

end module