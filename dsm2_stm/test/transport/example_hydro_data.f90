!> Sample flow fields for tests
!>@ingroup test
module example_hydro_data

contains

!> Constant uniform flow
!> todo: needs to satisfy the hydro_data_if interface, has to change
subroutine constant_uniform(flow,flow_lo,flow_hi,ncell,time,q_const)
use stm_precision
implicit none
!--- args
integer,intent(in)  :: ncell  !< Number of cells

real(STM_REAL),intent(out) :: flow(ncell)    !< cell-centered flow at time
real(STM_REAL),intent(out) :: flow_lo(ncell) !< flow on lo side of cells at time
real(STM_REAL),intent(out) :: flow_hi(ncell) !< flow on hi side at time
real(STM_REAL), intent(in) :: time           !< time
real(STM_REAL),intent(in) :: q_const         !< constant flow to be used


!-------
flow = q_const
flow_lo=flow
flow_hi=flow

return
end subroutine

end module