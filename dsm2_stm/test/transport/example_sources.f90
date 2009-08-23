!> Simple source term subroutines for testing
!>@ingroup test
module example_sources

contains

!> Empty source implementation
subroutine no_source(source,conc,area,flow,a_ncell,a_nvar)
 use stm_precision
 implicit none
 !--- args
 integer,intent(in)  :: a_ncell  !< Number of cells
 integer,intent(in)  :: a_nvar   !< Number of variables
 real(STM_REAL),intent(out) :: source(a_ncell,a_nvar) !< cell centered source 
 real(STM_REAL),intent(in) :: conc(a_ncell,a_nvar)    !< Concentration
 real(STM_REAL),intent(in) :: area(a_ncell,a_nvar)    !< area at source
 !> flow at source location
 real(STM_REAL),intent(in) :: flow(a_ncell,a_nvar)
 source = zero
 return
end subroutine

end module