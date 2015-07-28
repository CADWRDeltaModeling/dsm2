!<license>
!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
!    Department of Water Resources.
!    This file is part of DSM2.
!
!    The Delta Simulation Model 2 (DSM2) is free software: 
!    you can redistribute it and/or modify
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

!> Testing advection of mass which is subjected to a tidal boundary
!>@ingroup test_transport

!todo: this test fixes test_advection_covergence_tidal by having a mass
! conserving hydro interface. We need to do housekeeping and also compare them carefullly
! to make sure the fix is correct (things like dx and other parameters)


module test_tidal_synthetic

use gtm_precision
integer, parameter  :: nconc = 2                              !< Number of constituents
integer, parameter  :: nstep_base = 64                        !< Number of time steps in finer discritization
integer, parameter  :: nx_base    = 128                       !< Number of spatial discritization in finer mesh 
real(gtm_real),parameter :: origin = zero                     !< Left hand side of the channel
real(gtm_real),parameter :: domain_length = 204800.d0/two     !< Domain Length in meter
real(gtm_real),parameter :: amplitude = half                  !< Tidal amplitude in meter    
real(gtm_real),parameter :: gravity = 9.80d0                  !< Gravitational acceleration in m/s^2
real(gtm_real),parameter :: depth = 16.0d0                    !< Channel depth in meter
real(gtm_real),parameter :: sec_per_hr = 60.d0*60.d0          !< Convert factor of hour to second 
real(gtm_real),parameter :: m2_period = 12.4d0*sec_per_hr     !< M2 tidal period 
real(gtm_real),parameter :: freq=two*pi/m2_period             !< Frequency of tidal oscillation
real(gtm_real),parameter :: dye_length = domain_length/six    !< Length of cosine distribution of mass
real(gtm_real),parameter :: dye_center = domain_length/two    !< Center of cosine distribution of mass
real(gtm_real),parameter :: ic_gaussian_sd = domain_length/32.d0   !< Standard deviation of initial values 
real(gtm_real),parameter :: ic_center = domain_length/two     !< Center of initial condition
real(gtm_real),parameter :: total_time = m2_period         !< Total time of the test
real(gtm_real),parameter :: start_time = zero                 !< Starts at zero
real(gtm_real),parameter :: const_tidal_decay_rate = 1.552749d-5 !< Decay rate (set in the way it reduces half of the mass after one tidal period)
contains

!> Tidal flow for a rectangular basin with periodic forcing in the cell centerd 
!> the area here retrived from dQ/dx+dA/dt=0 --> A =width*(depth+int(-dQ/dx,t)) 
subroutine tidal_flow_synthetic(flow,    &
                                flow_lo, &
                                flow_hi, &
                                area,    &
                                area_lo, &
                                area_hi, &
                                ncell,   &
                                time,    &
                                dx,      &
                                dt)
                      
    implicit none
    integer, intent(in) :: ncell                   !< number of cells
    real(gtm_real), intent(in)  :: time            !< time of request
    real(gtm_real), intent(in)  :: dx(ncell)       !< spatial step 
    real(gtm_real), intent(in)  :: dt              !< time step 
    real(gtm_real), intent(out) :: flow(ncell)     !< cell centered flow
    real(gtm_real), intent(out) :: flow_lo(ncell)  !< lo face flow
    real(gtm_real), intent(out) :: flow_hi(ncell)  !< hi face flow
    real(gtm_real), intent(out) :: area(ncell)     !< cell center area
    real(gtm_real), intent(out) :: area_lo(ncell)  !< area lo face
    real(gtm_real), intent(out) :: area_hi(ncell)  !< area hi face

!--- local
real(gtm_real) :: half_time
real(gtm_real) :: old_time
real(gtm_real) :: big_b 
real(gtm_real) :: big_a 
real(gtm_real) :: vel_lo
real(gtm_real) :: vel_hi
real(gtm_real) :: vel
real(gtm_real) :: xpos_lo
real(gtm_real) :: xpos_hi
real(gtm_real) :: xpos
real(gtm_real) :: flow_term1
real(gtm_real) :: flow_term2

integer :: icell

half_time = time - half*dt
old_time = time - dt
big_b = freq/dsqrt(gravity*depth)
big_a = amplitude* dsqrt(gravity*depth)/(depth*dcos(big_b*domain_length))

! width is assumed to be equal to 1 meter 
do icell = 1,ncell  
  
  ! this is L-x
  xpos_lo = domain_length- dble(icell-1)*dx(icell)
  xpos_hi = domain_length- dble(icell)*dx(icell)
  xpos    = domain_length-(dble(icell)-half)*dx(icell)
  
   area(icell) = (big_a/(freq*dx(icell)))*(depth*dcos(freq*time)*(dsin(big_b*xpos_hi)-dsin(big_b*xpos_lo))+&
                                  amplitude*dcos(two*freq*time)*(dsin(two*big_b*xpos_hi)-dsin(two*big_b*xpos_lo))/(eight*dcos(big_b*domain_length)))
 ! todo: is it a correction factor for constant in integeration?
 ! check this
   area(icell) = depth + area(icell)
   area_lo(icell) = depth + amplitude * dcos(big_b*xpos_lo)/dcos(big_b*domain_length)*dcos(freq*half_time)  

   area_lo(icell) = big_a*big_b*(-depth*dcos(big_b*xpos_lo)*dcos(freq*half_time)/freq - &
                                amplitude*dcos(two*freq*half_time)*dcos(two*big_b*xpos_lo)&
                               /(four*freq*dcos(big_b*domain_length)))
  ! todo:
   ! i just match them base on old values ???                             
  area_lo(icell) = depth + area_lo(icell)
 
  area_hi(icell) = depth + amplitude * dcos(big_b*xpos_hi)/dcos(big_b*domain_length)*dcos(freq*half_time) 
   
  area_hi(icell) = big_a*big_b*(-depth*dcos(big_b*xpos_hi)*dcos(freq*half_time)/freq - &
                                amplitude*dcos(two*freq*half_time)*dcos(two*big_b*xpos_hi)&
                               /(four*freq*dcos(big_b*domain_length)))
   
   area_hi(icell) = depth + area_hi(icell)
                         
   flow(icell)    = big_a*depth*dsin(freq*time)*(dcos(big_b*xpos_hi)-dcos(big_b*xpos_lo))/(dx(icell)*big_b) &
                   + big_a*amplitude*dsin(two*freq*time)*(one/(four*dx(icell)*big_b*dcos(big_b*domain_length))) * &
                   (dcos(big_b*xpos_hi)**two - dcos(big_b*xpos_lo)**two)
  
   flow_term1 = - big_a*depth*dsin(big_b*xpos_lo)*(dcos(freq*time)-dcos(freq*old_time))/(dt*freq)
   flow_term2 = - big_a*amplitude*dsin(two*big_b*xpos_lo)*(dcos(freq*time)**two-dcos(freq*old_time)**two)&
                   /(four*freq*dcos(big_b*domain_length)*dt)
   flow_lo(icell) = flow_term1+flow_term2
          
   flow_term1 = - big_a*depth*dsin(big_b*xpos_hi)*(dcos(freq*time)-dcos(freq*old_time))/(dt*freq)
   flow_term2 = - big_a*amplitude*dsin(two*big_b*xpos_hi)*(dcos(freq*time)**two-dcos(freq*old_time)**two)&
                   /(four*freq*dcos(big_b*domain_length)*dt)
   flow_hi(icell) = flow_term1+flow_term2
   
end do  

return
end subroutine 

end module


