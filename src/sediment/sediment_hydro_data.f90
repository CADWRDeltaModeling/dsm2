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

!> Hydrodynamics interface to provide depth and flow velocity to be fed by user
!>@ingroup sediment
module suspended_sediment_variable
      !> Generic interface for fetching hydrodynamic data (
    abstract  interface
       !> Get hydrodynamic data for sediment module.
       !> This data might be calculated from a function or provided by another module
        subroutine sediment_hydro_if(velocity,  &
                                     depth,     &
                                     ncell,     &
                                     time,      &
                                     dx,        &
                                     dt)
        use gtm_precision
        implicit none
        integer, intent(in) :: ncell                    !< Number of cells (in)
        real(gtm_real), intent(in)  :: time             !< Time of request (in)
        real(gtm_real), intent(in)  :: dx               !< Spatial step (in)
        real(gtm_real), intent(in)  :: dt               !< Time step  (in)
        real(gtm_real), intent(out) :: velocity(ncell)  !< Cell and time centered velocity (out)
        real(gtm_real), intent(out) :: depth(ncell)     !< Cell center depth (out)
       ! todo: the signature of this interface may be subjected to change 
        
        end subroutine
      end interface
      
 !> This pointer should be set by the driver or client code to specify the 
 !> depth and velocity for the sediment source sink routine
 procedure(sediment_hydro_if),pointer :: fill_spatiotemporal_sediment_data  => null()
 
  
end module