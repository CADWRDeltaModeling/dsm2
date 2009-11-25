
!> Test of transport in uniform flow
!>@ingroup test
module test_uniform_flow

contains

!> Subroutine that runs a small advective simulation
subroutine test_uniform_flow_advection()
use stm_precision
use state_variables
use primitive_variable_conversion
use advection
use example_initial_conditions
use example_hydro_data
use example_sources
implicit none

!--- Problem variables

integer, parameter  :: nstep = 40
integer, parameter  :: nx = 128
integer, parameter  :: nconc = 2

real(STM_REAL), parameter :: origin = zero   ! meters
real(STM_REAL), parameter :: domain_length = 51200
real(STM_REAL), parameter :: cfl = 0.8
real(STM_REAL) :: dt              ! seconds
real(STM_REAL) :: dx              ! meters
real(STM_REAL), parameter :: ic_center      = three*fourth*domain_length
real(STM_REAL), parameter :: ic_gaussian_sd = domain_length/sixteen
real(STM_REAL), parameter :: constant_flow = 1.D2
real(STM_REAL), parameter :: constant_area = 1.D2
real(STM_REAL) :: vel
real(STM_REAL) :: time
integer :: itime = 0
integer :: icell ! debug only -- remove later
!------



call allocate_state(nx,nconc)
area = constant_area
area_prev = area
area_lo = area     ! todo: used?
area_hi = area     ! todo: used? remove from advect?

flow = constant_flow
flow_hi = flow
flow_lo = flow
vel = constant_area/constant_flow
dx = domain_length/dble(nx)
print*,"dx=",dx
dt = cfl*dx/vel
print*,"dt=",dt

call fill_gaussian(conc(:,1),nx,origin,dx,three*fourth*domain_length,ic_gaussian_sd)
call fill_gaussian(conc(:,2),nx,origin,dx,one*fourth*domain_length,ic_gaussian_sd)
call prim2cons( mass_prev,conc,area,nx,nconc)
mass = mass_prev


time = zero

! forwards
do itime = 1,nstep
  time = time + dt
  print*,itime,time
  ! call transport using no_source as the callback 
  call advect(mass,     &
              mass_prev,&  
              flow,     &
              flow_lo,  &
              flow_hi,  &
              area,     &
              area_prev,&
              area_lo,  &
              area_hi,  &
              ncell,    &
              nvar,     &
              time,     &
              dt,       &
              dx)

  mass_prev = mass
  call cons2prim(conc,mass,area,nx,nconc) 
  ! assertions (tests)
  ! store latest time step
end do
call printout(conc(:,2),ncell)

call deallocate_state
return
end subroutine



end module