!<license>
!    Copyright (C) 2017 State of California,
!    Department of Water Resources.
!    This file is part of DSM2-GTM.
!
!    The Delta Simulation Model 2 (DSM2) - General Transport Model (GTM) 
!    is free software: you can redistribute it and/or modify
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
!> Module containing routines for converting mass to concentraion, and concentration to mass
!>@ingroup transport
module primitive_variable_conversion

  contains
  !> Convert conservative variables (ie mass) to primitive (concentration)
  !> There is no precaution against divide by zero, so if you put in zero for area
  !> you will get NaN
  pure subroutine cons2prim(conc,mass,area,nloc,nvar)
 
      use gtm_precision

      implicit none
      !--- args
      real(gtm_real),intent(out) :: conc(nloc,nvar)   !< Concentration (converted from mass per unit length )
      real(gtm_real),intent(in)  :: mass(nloc,nvar)   !< Mass per unit length 
      real(gtm_real),intent(in)  :: area(nloc)        !< Area at conversion locations
      integer,intent(in)  :: nloc                     !< Number of cells or faces
      integer,intent(in)  :: nvar                     !< Number of variables
      !--- locals
      integer :: ivar

      do ivar = 1,nvar
          conc(:,ivar) = mass(:,ivar)/area
      end do

      return
  end subroutine

  !> Convert primitive (concentration) to conservative variables (ie mass)
  pure subroutine prim2cons(mass,conc,area,nloc,nvar)

      use gtm_precision

      implicit none
      !--- args
      real(gtm_real),intent(out) :: mass(nloc,nvar)  !< Mass per unit length (converted from concentration)
      real(gtm_real),intent(in)  :: conc(nloc,nvar)  !< Concentrations to convert
      real(gtm_real),intent(in)  :: area(nloc)       !< Area at conversion locations
      integer,intent(in)  :: nloc                    !< Number of cells or faces
      integer,intent(in)  :: nvar                    !< Number of variables
      !--- locals
      integer :: ivar
   
      do ivar = 1,nvar
          mass(:,ivar) = conc(:,ivar)*area
      end do
    
      return
  end subroutine

  !> Increment a conservative variable using an increment (e.g. source) in primitive terms
  !> Converts increment of primitive (concentration) to conservative variable (mass) and
  !> adds it to the conservative variable. "Scale" will typically involve dt or half*dt.
  pure subroutine prim_increment_to_cons(mass,conc,area,nloc,nvar,scale)

      use gtm_precision

      implicit none
      !--- args
      real(gtm_real),intent(inout) :: mass(nloc,nvar)  !< Mass per unit length (converted from concentration)
      real(gtm_real),intent(in)    :: conc(nloc,nvar)  !< Concentrations to convert
      real(gtm_real),intent(in)    :: area(nloc)       !< Area at conversion locations
      ! todo: here is a compiler bug and the fortran forums told it would be fixed 
      ! change the scale intent to just (in)
      ! todo: should we change scale to scale(nvar)?
      real(gtm_real),intent(in)    :: scale           !< Scale factor
      integer,intent(in)  :: nloc                     !< Number of cells or faces
      integer,intent(in)  :: nvar                     !< Number of variables
      !--- locals
      integer :: ivar

      do ivar = 1,nvar
      ! todo: check this line
          mass(:,ivar) =  mass(:,ivar) + scale*conc(:,ivar)*area
      end do

      return
  end subroutine

end module