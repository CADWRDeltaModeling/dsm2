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

!> Test for subroutine primitive increment to conservative 
!>@ingroup test_transport
module test_prim_increment_to_cons



contains
!>Test for subroutine primitive increment to conservative 
subroutine test_prim_increment2cons()

use primitive_variable_conversion
use fruit
use gtm_precision

    implicit none
    !--- args
    integer,parameter:: nloc = 3          !< Number of cells or faces
    integer,parameter:: nvar = 2          !< Number of variables
    real(gtm_real) :: mass(nloc,nvar)     !< Mass per unit length (converted from concentration)
    real(gtm_real) :: conc(nloc,nvar)     !< Concentrations to convert
    real(gtm_real) :: area(nloc)          !< Area at conversion locations
    ! todo: here is a compiler bug and the Fortran forums told it would be fixed 
    ! change the scale intent to just (in)
    real(gtm_real) :: scale               !< Scale factor
    real(gtm_real) :: reference(nloc,nvar)!< To compare solution with
    
  
     !---test positive and negetive scales with very large scale 
    mass(:,1) = [1:3]
    mass(:,2) = [11:13]
    reference = mass
    scale =LARGEREAL
    area (:) = four*four
    conc = half 
      
    call prim_increment_to_cons(mass,conc,area,nloc,nvar,scale)
 
    scale = minus*scale
    
    call prim_increment_to_cons(mass,conc,area,nloc,nvar,scale)
      
    call assertEquals (mass(1,1),reference(1,1),1d-12,"problem in converting prim increment to mass, array 1,1 ")
    call assertEquals (mass(3,2),reference(3,2),1d-12,"problem in converting prim increment to mass, array 3,2 ")
     
      !---test  scales equal to zero  
    mass(:,1) = [1:3]
    mass(:,2) = [11:13]
    reference = mass
    scale = zero
    
    call prim_increment_to_cons(mass,conc,area,nloc,nvar,scale)
    
    call assertEquals (mass(1,1),reference(1,1),1d-12,"problem in converting prim increment to mass, array 1,1 ")
    call assertEquals (mass(3,2),reference(3,2),1d-12,"problem in converting prim increment to mass, array 3,2 ")
  
    !---test area = zero ,mass must be the same at the end
    mass(:,1) = [1:3]
    mass(:,2) = [11:13]
    reference = mass
    scale = LARGEREAL
    conc = LARGEREAL
    area = zero
    
    call prim_increment_to_cons(mass,conc,area,nloc,nvar,scale)
    
    call assertEquals (mass(1,1),reference(1,1),1d-12,"problem in converting prim increment to mass, array 1,1 ")
    call assertEquals (mass(3,2),reference(3,2),1d-12,"problem in converting prim increment to mass, array 3,2 ")
    
    !--- test 
    mass(:,1) = [1:3]
    mass(:,2) = [11:13]
    reference = mass + ten
    scale = one
    conc = five
    area = two
    
    call prim_increment_to_cons(mass,conc,area,nloc,nvar,scale)
    
    call assertEquals (mass(1,1),reference(1,1),1d-12,"problem in converting prim increment to mass, array 1,1 ")
    call assertEquals (mass(3,2),reference(3,2),1d-12,"problem in converting prim increment to mass, array 3,2 ")
  
  

return
end subroutine 


end module