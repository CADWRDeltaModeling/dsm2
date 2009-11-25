!> Sample initial conditions for tests!>@ingroup test

module example_initial_conditions
use stm_precision
private gaussian_cdf

contains

!> Gaussian cdf (integrated Guassian pdf) from -inf to x0
real(STM_REAL) function gaussian_cdf(x0,mean,sd)
use stm_precision
implicit none
real(STM_REAL), intent(in) :: x0   !< end of integration
real(STM_REAL), intent(in) :: mean !< mean
real(STM_REAL), intent(in) :: sd   !< sigma/standard deviation
gaussian_cdf = half + half*erf((x0-mean)/(sqrt(two)*sd))
return
end function


!> Fill array with 1D gaussian shape
!> This routine expects a 1D array, so multi-constituents
!> have to be initialized separately
subroutine fill_gaussian(vals,nloc,origin,dx,mean,sd)
use stm_precision
implicit none
integer, intent(in) :: nloc
real(STM_REAL), intent(out) :: vals(nloc)
real(STM_REAL), intent(in)  :: origin
real(STM_REAL), intent(in)  :: dx
real(STM_REAL), intent(in)  :: mean
real(STM_REAL), intent(in)  :: sd
!-----locals
real(STM_REAL) :: xlo
real(STM_REAL) :: xhi
integer        :: iloc
!-----------
do iloc = 1,nloc
   xlo = origin + dble(iloc - 1)*dx
   xhi = origin + dble(iloc)*dx
  ! need to populate using cell averages
   vals(iloc) =  gaussian_cdf(xhi,mean,sd) & 
                -gaussian_cdf(xlo,mean,sd)
end do
return
end subroutine

!> Initialize the concentration fields with a step function
subroutine fill_discontinuity(vals,nloc,origin,dx,x0,value_lo,value_hi)
use stm_precision
implicit none
integer, intent(in) ::  nloc
real(STM_REAL), intent(out) :: vals(nloc)
real(STM_REAL), intent(in)  :: origin
real(STM_REAL), intent(in)  :: dx
real(STM_REAL), intent(in)  :: x0
real(STM_REAL), intent(in)  :: value_lo
real(STM_REAL), intent(in)  :: value_hi
!---locals
real(STM_REAL) :: fraction_lo
real(STM_REAL) :: fraction_hi
real(STM_REAL) :: xlo
real(STM_REAL) :: xhi
integer :: iloc

!----------------------------------
do iloc = 1,nloc
   xlo = origin + dble(iloc - 1)*dx
   xhi = origin + dble(iloc)*dx
   fraction_lo = (x0 - xlo)/dx
   ! tend to the usual cases where cell is entirely on lo/hi side of discontinuity
   fraction_lo = max(fraction_lo,zero)
   fraction_lo = min(fraction_lo,one)        
   fraction_hi = one - fraction_lo      
  ! need to populate using cell averages
   vals(iloc) =  fraction_lo*value_lo + fraction_hi*value_hi
end do

return
end subroutine


subroutine test_example_initial_conditions
use stm_precision
use fruit
implicit none
integer, parameter :: nloc = 100
real(STM_REAL) :: vals(nloc,2)
real(STM_REAL), parameter  :: dx = ten
real(STM_REAL), parameter  :: origin = zero
real(STM_REAL), parameter  :: center1 = 405.  ! middle of cell 41
real(STM_REAL), parameter  :: center2 = 605.  ! middle of cell 61
real(STM_REAL), parameter  :: sd = dx*4
real(STM_REAL), parameter  :: epsilon = 1.D-08 ! mediocre precision for a double because using tabulated values
real(STM_REAL) :: offline_calc
real(STM_REAL) :: cell_calc41
real(STM_REAL) :: cell_calc61
character(LEN=32) :: message
integer :: icell

! Check symmetry and two constituents
call fill_gaussian(vals(:,1),nloc,origin,dx,center1,sd)
call fill_gaussian(vals(:,2),nloc,origin,dx,center2,sd)

! test the center cell for each plume. 
! The cell edges lo/hi are half a dx = 1/8 of sd from center
! so use tabulated values of the cdf at the mean +/- 1/8*sigma
offline_calc = 0.09947645
cell_calc41 = vals(41,1)
call assertEquals(cell_calc41,offline_calc,epsilon,"Integral gaussian in cell 41")
cell_calc61 = vals(61,2)
call assertEquals(cell_calc41,cell_calc61,"Symmetry of integral gaussian in cells 41, 61")

call fill_discontinuity(vals(:,1),nloc,origin,dx,center1,zero,one)
call fill_discontinuity(vals(:,2),nloc,origin,dx,center2,zero,one)
call assertEquals(vals(41,1),half,"Discontinuity IC halfway in cell (41)")
call assertEquals(vals(41,2),zero,"Discontinuity IC halfway in cell (41) -- non-discontinuous constituent")
call assertEquals(vals(61,2),half,"Discontinuity IC halfway in cell (61)")
call assertEquals(vals(61,1),one,"Discontinuity IC halfway in cell (61) -- non-discontinuous constituent")
do icell = 1,40
    message = "Discontinuity IC, lo cells (constituent 1)"
    call assertEquals(vals(icell,1),zero, message)
    message = "Discontinuity IC, lo cells (constituent 2)"    
    call assertEquals(vals(icell,2),zero, message)
end do
do icell = 42,60
    message = "Discontinuity IC, middle (constituent 1)"
    call assertEquals(vals(icell,1),one, message)
    message = "Discontinuity IC, middle (constituent 2)"    
    call assertEquals(vals(icell,2),zero, message)
end do
do icell = 62,100
    message = "Discontinuity IC, hi cells (constituent 1)"
    call assertEquals(vals(icell,1),one, message)
    message = "Discontinuity IC, hi cells (constituent 2)"
    call assertEquals(vals(icell,2),one, message)
end do
return
end subroutine


end module