!<!license>
!    Copyright (C) 2017 State of California,
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

!> Tests the coarsening subroutine
!>@ingroup test_transport
module test_coarsening


contains

!> Tests the coarsening subroutine
subroutine test_coarsen
use fruit
use test_utility
use gtm_precision
implicit none
 

!---arg
integer :: ncell_coarse                           !< Number of cells at coarser grid
integer,parameter :: ncell_fine = 6               !< Number of cells at finer grid 
integer,parameter :: nvar = 2                     !< Number of constituents
real(gtm_real) :: fine_data(ncell_fine,nvar)      !< Fine data values
real(gtm_real), allocatable :: coarse_data(:,:)   !< Coarse data values
!---- local
real(gtm_real), parameter :: tol = 1.d-15         !< Acceptable tolerance 

fine_data(:,1) = [1:6]
fine_data(:,2) = [11:16]


!> It have to output same array if the coarsening factor is one 
ncell_coarse = 6
allocate (coarse_data(ncell_coarse,nvar))
call coarsen(coarse_data,fine_data,ncell_fine,ncell_coarse, nvar)
call assertEquals(coarse_data(1,1),one,tol,"error in coarsening, no refinement (1,1)")
call assertEquals(coarse_data(6,2),16.d0,tol,"error in coarsening, no refinement (6,2)")
deallocate(coarse_data)

!> Check for coarsening factor of two
ncell_coarse = 3
allocate (coarse_data(ncell_coarse,nvar))
call coarsen(coarse_data,fine_data,ncell_fine,ncell_coarse, nvar)
call assertEquals (coarse_data(1,1),1.5d0,tol,"error in coarsening (1,1)")
call assertEquals (coarse_data(3,2),15.5d0,tol,"error in coarsening constituent (2,3)")
deallocate (coarse_data)

return
end subroutine 


!> Tests the detect wiggle subroutine
subroutine test_detect_wiggle
use fruit
use test_utility
use gtm_precision
implicit none
 
!---arg
integer,parameter :: ncell = 6            !< Number of cells 
integer :: oscillation                    !< Number of extra oscillation in value_2
real(gtm_real) :: base_val(ncell)         !< Benchmark values    
real(gtm_real) :: numeric_val(ncell)      !< Numerical solution values
real(gtm_real), parameter :: tol = 1.d-15 !< Acceptable tolerance 

base_val = [1:6]
numeric_val = [11:16]

call detect_wiggle(numeric_val,  &
                   base_val,     &
                   ncell,        &
                   oscillation)

call assertEquals(oscillation,0,"error in detect wiggles")

base_val = [1.0d0,2.0d0,3.0d0,4.0d0,3.0d0,4.0d0]
numeric_val = [1.0d0,2.0d0,1.0d0,2.0d0,1.0d0,2.0d0]

call detect_wiggle(numeric_val,  &
                   base_val,     &
                   ncell,        &
                   oscillation)


call assertEquals(oscillation,2,"error in detect wiggles")

call detect_wiggle(base_val,     &
                   base_val,     &
                   ncell,        &
                   oscillation)


call assertEquals(oscillation,0,"error in detect wiggles")

call detect_wiggle(base_val,     &
                   numeric_val,  &
                   ncell,        &
                   oscillation)

call assertEquals(oscillation,-2,"error in detect wiggles")

return
end subroutine 


!> Tests the detect wiggle subroutine
subroutine test_mass_comparison
use fruit
use test_utility
use gtm_precision
implicit none
 
!---arg
integer,parameter :: ncell = 6            !< Number of cells 
real(gtm_real) :: base_val(ncell)         !< Benchmark values    
real(gtm_real) :: numeric_val(ncell)      !< Numerical solution values
real(gtm_real), parameter :: tol = 1.d-15 !< Acceptable tolerance 
real(gtm_real), parameter :: dx = half    !< dx 
real(gtm_real) :: error
logical :: alarm


base_val = [1d0,2d0,1d0,2d0,1d0,2d0]
numeric_val = base_val*1.1d0

call mass_comparison(numeric_val,     &
                     base_val,        &
                     ncell,           &
                     dx,              &
                     error,           &
                     alarm)

call assertEquals(error,-0.1d0,tol," error in mass comparison percentage")
call assertEquals(alarm,.false.,  " error in mass comparison percentage")

base_val = [1d0,2d0,1d0,-2d0,1d0,2d0]
numeric_val = base_val*0.9d0

call mass_comparison(numeric_val,     &
                     base_val,        &
                     ncell,           &
                     dx,              &
                     error,           &
                     alarm)
                     
call assertEquals(error,+0.1d0,tol," error in mass comparison percentage")
call assertEquals(alarm,.true.,  " error in mass comparison percentage")


return
end subroutine 



end module