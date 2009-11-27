
!> Routines containing error metrics for assessing convergence or accuracy
!>@ingroup test
module error_metric

contains

!> Calculate the L1, L2 and Linf error between calculated values and a reference
subroutine error_norm(norm_1,norm_2,norm_inf,vals,reference,ncell,dx)
use stm_precision
implicit none

integer, intent(in) :: ncell
real(STM_REAL), intent(out) :: norm_1            !< L-1   error
real(STM_REAL), intent(out) :: norm_2            !< L-2   error
real(STM_REAL), intent(out) :: norm_inf          !< L-inf error

real(STM_REAL), intent(in) :: vals(ncell)        !< Calculated values
real(STM_REAL), intent(in) :: reference(ncell)   !< Reference or 'other' values
real(STM_REAL), intent(in) :: dx                 !< Spatial step !todo: do we use this????

! locals
integer :: icell
integer :: which_cell
real(STM_REAL) :: err
real(STM_REAL) :: sq_error
real(STM_REAL) :: abs_error

norm_1=zero
norm_2=zero
norm_inf=zero
do icell=1,ncell
   err = vals(icell) - reference(icell)
   abs_error = abs(err)
   sq_error = err*err
   if (abs_error > norm_inf) then
       norm_inf = abs_error
       which_cell = icell
   end if
   norm_1 = norm_1 + abs_error
   norm_2 = norm_2 + sq_error
end do
norm_1 = norm_1/dble(ncell)
norm_2 = norm_2/dble(ncell)
return
end subroutine


!< Prints an array to file
subroutine printout(arr,filename)
use stm_precision
implicit none
real(STM_REAL),intent(in) :: arr(:)       !< array values
character(LEN=*)          :: filename     !< name of file to write
integer icell
open(unit = 11, file = filename)
do icell = 1,size(arr)
  write(11,*)arr(icell)
end do
close(11)
end subroutine

end module