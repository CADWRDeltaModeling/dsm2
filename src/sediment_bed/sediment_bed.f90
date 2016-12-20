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
!> ================================================
!> The Sediment Bed Module is developed by:
!> Dave Hutchinson and Reed Harris
!> Reed Harris Environmental Ltd.
!> ================================================
!> 
!>@ingroup sediment_bed
module sediment_bed

    use gtm_precision
    
    contains
     
    subroutine sediment_bed_main(available_solid,   &
                                 erosion_flux,      &
                                 deposition_flux,   &                                 
                                 ncell,             &
                                 nsolids)
        implicit none
        integer, intent(in) :: ncell
        integer, intent(in) :: nsolids
        real(gtm_real), intent(in) :: erosion_flux(ncell,nsolids)
        real(gtm_real), intent(in) :: deposition_flux(ncell,nsolids)
        real(gtm_real), intent(out) :: available_solid(ncell,nsolids)
        
        available_solid = 9999.d0
        return
    end subroutine    

end module