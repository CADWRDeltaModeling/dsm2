module primitive_variable_conversion

contains
!> Convert conservative variables (ie mass) to primitive (concentration)
subroutine cons2prim(conc,mass,area,nloc,nvar)
use stm_precision
implicit none
real(STM_REAL),intent(out) :: conc(nloc,nvar)   !< concentration (converted from mass per unit length )
real(STM_REAL),intent(in) :: mass(nloc,nvar)    !< mass per unit length 
real(STM_REAL),intent(in) :: area(nloc)         !< area at conversion locations
!--- args
integer,intent(in)  :: nloc  !< Number of cells or faces
integer,intent(in)  :: nvar   !< Number of variables
!--- locals
integer :: ivar
!-------------------
do ivar = 1,nvar
    conc(:,ivar) = mass(:,ivar)/area
end do
return
end subroutine

!> Convert  primitive (concentration) to conservative variables (ie mass)
subroutine prim2cons(mass,conc,area,nloc,nvar)
use stm_precision
implicit none
real(STM_REAL),intent(out) :: mass(nloc,nvar) !< mass per unit length (converted from concentration)
real(STM_REAL),intent(in) :: conc(nloc,nvar)  !< concentrations to convert
real(STM_REAL),intent(in) :: area(nloc)       !< area at conversion locations
!--- args
integer,intent(in)  :: nloc  !< Number of cells or faces
integer,intent(in)  :: nvar   !< Number of variables
!--- locals
integer :: ivar
!-------------------

do ivar = 1,nvar
    mass(:,ivar) = conc(:,ivar)*area
end do
return
end subroutine

end module


