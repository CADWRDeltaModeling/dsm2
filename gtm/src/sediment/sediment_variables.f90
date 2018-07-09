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
module sediment_variables

    use gtm_precision
 
    !> Sediment constants   
    real(gtm_real), parameter :: gravity = 9.80665d0                  !< Acceleration of gravity; it must be in SI units (constant)
    real(gtm_real), parameter :: water_density = 1000.d0              !< Water density
    real(gtm_real), parameter :: sediment_density = 2650.d0           !< Sediment density
    real(gtm_real), parameter :: kappa = 0.41d0                       !< von Karman's constant
    real(gtm_real), parameter :: kinematic_viscosity = 1.307d-6       !< Kinematic viscosity (m2/sec) of water @ 10 deg C   
    real(gtm_real), parameter :: delta_b = 0.05d0                     !< Bed leyer relative tickness     
    real(gtm_real), parameter :: specific_gravity = sediment_density/water_density   !< specific gravity
     
end module
