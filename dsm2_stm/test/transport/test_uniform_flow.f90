
!> Test of transport in uniform flow
!>@ingroup test
module test_uniform_flow

contains

!> Subroutine that runs a small advective simulation
subroutine test_uniform_flow_advection()
use stm_precision
use state_variables
use advection
use example_initial_conditions
use example_hydro_data
use example_sources
implicit none

!--- Problem variables

integer, parameter  :: nstep = 100
integer, parameter  :: nx = 100
integer, parameter  :: nconc = 1
real(STM_REAL) :: dx = 1000.0D0   ! meters
real(STM_REAL) :: dt =  300.0D0   ! five minutes

integer :: itime = 0

!------
call allocate_state(nx,nconc)

!call gaussian_ic
!call uniform_flow(flow, ....)

do itime = 1,nstep
  ! time = ...
  ! call transport using no_source as the callback 
  !   (callback means using a subroutine as input to a function for use inside the function)
  ! assertions (tests)
  ! store latest time step
end do

end subroutine
end module