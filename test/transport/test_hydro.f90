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

!> Testing the subroutine which provides the hydrodynamics of the tidal flow
!>@ingroup test_transport
module test_hydro

contains

!> Generic test for checking mass continuity of fluid flow 
!> Checks dA/dt + dQ/dx = 0 in integral form over the input time interval
!> The test will pass if the mid-time face flows (face flow should representing face average over time)
!> equal the cell centered change in area*dx     (cell area should represent storage integrated over the cell)
subroutine test_flow_continuity(ncell,        &
                                nstep,        &
                                start_time,   &
                                total_time,   &
                                domain_length,&
                                hydrodynamics,&
                                label)
use test_utility
use gtm_precision
use hydro_data
use fruit

implicit none

!> todo: this must be coordinated with test_advection_tidal, better if it were automatic
integer,intent(in) :: ncell                                    !< Number of cells
integer,intent(in) :: nstep                                    !< Number of time 
real(gtm_real),intent(in):: total_time                         !< Total time
real(gtm_real),intent(in):: start_time                         !< Start time
real(gtm_real),intent(in):: domain_length                      !< Domain_length
character(LEN=*),intent(in) :: label                           !< Label for test
procedure(hydro_data_if), pointer, intent(in) :: hydrodynamics !< Hydrodynamics interface to be tested
!--- local
integer :: itime                         !< Time counter
integer :: icell                         !< Cell counter 
integer :: which_cell                    !< Cell number with worst error
real(gtm_real):: time                    !< Time of request
real(gtm_real):: flow(ncell)             !< Cell centered flow
real(gtm_real):: flow_lo(ncell)          !< Low face flow
real(gtm_real):: flow_hi(ncell)          !< High face flow
real(gtm_real):: area(ncell)             !< Cell center area
real(gtm_real):: area_lo(ncell)          !< Area lo face
real(gtm_real):: area_hi(ncell)          !< Area hi face
real(gtm_real):: flow_new(ncell)         !< Cell centered flow at time n+1
real(gtm_real):: flow_hi_half(ncell)     !< High side flow at time n+1/2
real(gtm_real):: flow_lo_half(ncell)     !< Low side flow at time n+1/2
real(gtm_real):: area_new(ncell)         !< Cell centered area at time n+1
real(gtm_real):: area_old(ncell)         !< Cell centered area at time n
real(gtm_real):: mass_difference(ncell)  !< Mass diffrence between results and analytical solution
real(gtm_real):: max_mass_diff(nstep)    !< Maximum of mass difference
real(gtm_real):: l1_mass_diff(nstep)     !< L1 norm for mass error
real(gtm_real):: l2_mass_diff(nstep)     !< L2 norm for mass error 
real(gtm_real):: all_zero(ncell)         !< All zero vector
real(gtm_real):: dt                      !< Time step size
real(gtm_real):: dx                      !< Spacial step size  

time = start_time
dt = total_time/dble(nstep)
dx = domain_length/dble(ncell)

all_zero = zero

call hydrodynamics(flow_new,     &
                   flow_lo_half, &
                   flow_hi_half, &
                   area_old,     &
                   area_lo,      &
                   area_hi,      &
                   ncell,        &
                   time,         &
                   dx,           &                  
                   dt)

do itime=1,nstep
  time = time + dt        
  call hydrodynamics(flow_new,      &
                     flow_lo_half,  &
                     flow_hi_half,  &
                     area_new,      &
                     area_lo,       &
                     area_hi,       &
                     ncell,         &
                     time,          &
                     dx,            &
                     dt)
   mass_difference = (flow_hi_half-flow_lo_half)*dt  +  &
                          (area_new-area_old)*dx
  
   call error_norm(l1_mass_diff(itime),   &
                   l2_mass_diff(itime),   &
                   max_mass_diff(itime),  &
                   which_cell,            &
                   mass_difference,       &
                   all_zero,              &
                   ncell,                 &
                   dx)  
  
   area_old = area_new
end do

call assert_true (maxval(max_mass_diff) < weak_eps ,'Water mass balance error in flow generator'//label)

return
end subroutine

!> Test the continuity of mass for water for tidal example flow
!>todo: coordinate this with test_advection_tidal
!>todo: a lot of this is redundant with the general continuity test. 
subroutine test_tidal_hydro
use gtm_precision
use hydro_data
use test_advection_reaction_tidal   

implicit none
integer :: ncell = 32                             !< Number of cells
integer :: nstep = 64                             !< Number of time steps
procedure(hydro_data_if),pointer :: tidal_hydro   !< The pointer points to tidal flow data
character (LEN=64) :: hydro_label                 !< Name of the hydrodynamic data generator

hydro_label = 'tidal_flow'
tidal_hydro=> tidal_flow_modified

call test_flow_continuity(ncell,              &
                          nstep,              &
                          start_time,         &
                          total_time,         &
                          domain_length,      &
                          tidal_hydro,        &
                          hydro_label)


return
end subroutine

!> Tests mass continuity of fluid flow for the Zoppou test
subroutine test_zoppou_flow()
use gtm_precision
use hydro_data

use test_zoppou_advection_dispersion
implicit none
integer :: ncell                                            !< Number of cells
integer :: nstep                                            !< Number of time steps
real(gtm_real) :: start_t                                   !< Start time
real(gtm_real) :: total_time                                !< Total time
real(gtm_real) :: domain_length                             !< Domain length
procedure(hydro_data_if),pointer :: hydrodynamics => null() !< Pointer of hydrodynamic values set to Zoppou hydro
character (LEN=64) :: hydro_label                           !< Name of the hydrodynamic data generator  

!todo: these numbers are hardwired 
ncell = 256
nstep = 128
start_t= zero
total_time = 2000.d0
domain_length = 2048.0d0
hydro_label = 'zoppou_flow'

hydrodynamics => zoppou_flow

call test_flow_continuity(ncell,        &
                          nstep,        &
                          start_t,      &
                          total_time,   &
                          domain_length,&
                          hydrodynamics,&
                          hydro_label)


return
end subroutine


end module