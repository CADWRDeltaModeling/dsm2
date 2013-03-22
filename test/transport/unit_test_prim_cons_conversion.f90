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

!> Testing of primitive-conservative conversion
!>@ingroup test_transport
module test_prim_cons_conversion
use fruit

contains

!> Test code that converts between primitive and conservative forms of the variables
subroutine test_prim_cons_convert

use gtm_precision
use primitive_variable_conversion

implicit none
  integer,parameter :: nx = 3             !< Number of cells 
  ! the above is for interior and two ends
  integer,parameter :: nconst = 2         !< Number of constituents 
  character(LEN=64) :: message            !< Message 
  real(gtm_real) :: mass(nx,nconst)       !< Mass of the constiuent
  real(gtm_real) :: conc(nx,nconst)       !< Concentration of the constiuent I
  real(gtm_real) :: conc2(nx,nconst)      !< Concentration of the constiuent II
  real(gtm_real) :: area(nx)              !< Flow area
  integer :: ic, ix                       !< Counters 
  logical :: nan_result                   !< Flag for not a number detection
  
  conc(1,1) = sixteen
  conc(2,1) = zero
  conc(3,1) = one
  conc(1,2) = sixteen
  conc(2,2) = zero
  conc(3,2) = one
  area(1) = eight
  area(2) = zero
  area(3) = four  
  call prim2cons(mass,conc,area,nx,nconst)
  call assertEquals(mass(1,1),128.d0,"Conversion(1,1)")
  call assertEquals(mass(2,1),zero,"Conversion(1,1)")
  call assertEquals(mass(3,1),four,"Conversion(1,1)")
  ! check multiple constituents
  call assertEquals(mass(1,1),mass(1,2),"Conversion same for two constituents: (1,1) and (1,2)")
  call assertEquals(mass(2,1),mass(2,2),"Conversion same for two constituents: (2,1) and (2,2)")
  call assertEquals(mass(3,1),mass(3,2),"Conversion same for two constituents: (3,1) and (3,2)")
  call cons2prim(conc2,mass,area,nx,nconst)

  do ic=1,2
      do ix = 1,3
          if (ix .ne. 2) then
              write(message,"('Prim-cons round trip: (',i1,',',i1,')')")ix,ic
              call assertEquals(conc2(ix,ic),conc(ix,ic),trim(message))
          else
              nan_result = isnan(conc2(ix,ic))
              write(message,"('Prim-cons round trip should be NaN for zero area: (',i2,',',i2,')')")ix,ic
              call assert_true(nan_result,trim(message))          
          end if
      end do
  end do
  
return
end subroutine

end module



