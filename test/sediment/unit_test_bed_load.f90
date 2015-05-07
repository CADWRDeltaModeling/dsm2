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

!> Tests for the bed-load formula related subroutines
!>@ingroup test_sediment
module test_bed_load

contains 

subroutine test_volumetric_bedload_transport_rate

use fruit
use bed_load
use gtm_precision

implicit none
integer,parameter:: nsed  = 2                     !< Number of sediment classes 
integer,parameter:: ncell = 3                     !< Number of computational cell
real(gtm_real):: gravity = 9.8d0                  !< Gravity
real(gtm_real):: capital_r                        !< Submerged specific gravity of sediment particles  
real(gtm_real):: diameter(nsed)                   !< Particle diameter
real(gtm_real):: einstein_bedload_num(ncell,nsed) !< Dimensionless belload transport rate 
real(gtm_real):: q_sub_b(ncell,nsed)              !< Volumetric bedload transport rate
!----------------
real(gtm_real):: hand_calculation(ncell,nsed)
integer :: iclas 
integer :: ivol

capital_r = 1.65d0
diameter = [0.1d-1, 0.2d-2]
einstein_bedload_num(1,:) =[seven, three]
einstein_bedload_num(2,:) =[ten, two]
einstein_bedload_num(3,:) =[five , four]


call volumetric_bedload_transport_rate(q_sub_b,                &
                                       einstein_bedload_num,   &
                                       diameter,               &
                                       capital_r,              &
                                       gravity,                &
                                       nsed,                   &
                                       ncell)
                                       
hand_calculation(1,:) =[0.028148357d0, 0.001079d0]
hand_calculation(2,:) =[0.028148357d0, 0.001079d0]
hand_calculation(3,:) =[0.028148357d0, 0.001079d0]

do ivol=1,ncell
    do iclas=1,nsed                                                                   
        call assertEquals(hand_calculation(ivol,iclas),q_sub_b(ivol,iclas),weak_eps,"Error in volumetric_bedload_transport_rate"//char(ivol)//' & '//char(iclas))
    end do
end do


return
end subroutine



end module  
