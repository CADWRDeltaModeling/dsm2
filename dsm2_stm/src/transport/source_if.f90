
!> Source interface to be fulfilled by driver or application
!>@ingroup transport
module source_if
 !> Calculate source
 interface compute_source
   !> Generic interface for calculating source that should be fulfilled by
   !> client programs
   subroutine compute_source(source,conc,area,flow,ncell,nvar,time)
     use stm_precision
     implicit none
     !--- args
     integer,intent(in)  :: ncell  !< Number of cells
     integer,intent(in)  :: nvar   !< Number of variables
     real(STM_REAL),intent(out) :: source(ncell,nvar) !< cell centered source 
     real(STM_REAL),intent(in)  :: conc(ncell,nvar)   !< Concentration
     real(STM_REAL),intent(in)  :: area(ncell)        !< area at source     
     real(STM_REAL),intent(in)  :: flow(ncell)        !< flow at source location
     real(STM_REAL),intent(in)  :: time               !< flow at source location
   end subroutine
 end interface
end module