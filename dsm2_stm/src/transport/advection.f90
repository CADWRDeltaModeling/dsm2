
!> Module orchestrating the advection scheme. The main
!> routine in the module is advection().
!>@ingroup transport
module advection
use stm_precision

contains

!///////////////////////////////////////////////////////////////////////

!> Advection plus sources for a time step.
!> The final argument to advect is a callback for computing the source term,
!> which should conform to the source_if interface
!> The algoritm looks like this:
!>   - Convert to primitive variables
!>   - Extrapolate to faces 
!>       - difference()
!>       - limiter()
!>       - extrapolate()
!>   - upwind()
!>   - compute_flux()
!>   - replace_boundary_flux()
!>   - Compute conservative divergence
!>   - Apply divergence in conservative_update
!>


subroutine advect(mass,     &
                     mass_prev,&  
                     flow_lo,  &
                     flow_hi,  &
                     area,     &
                     area_prev,&
                     area_lo,  &
                     area_hi,  &
                     ncell,&
                     nvar, &
                     time, &
                     dt,   &
                     dx, &
                     compute_source)
!use source_if
implicit none

!--- args
integer,intent(in)  :: ncell  !< Number of cells
integer,intent(in)  :: nvar   !< Number of variables

real(STM_REAL),intent(out) :: mass(ncell,nvar)     !< mass concentration at new time
real(STM_REAL),intent(in) :: mass_prev(ncell,nvar) !< mass concentration at new time
real(STM_REAL),intent(in) :: flow_lo(ncell,nvar)   !< flow on lo side of cells centered in time
real(STM_REAL),intent(in) :: flow_hi(ncell,nvar)   !< flow on hi side of cells centered in time
real(STM_REAL),intent(in) :: area_prev(ncell,nvar) !< cell-centered area at old time??
real(STM_REAL),intent(in) :: area(ncell,nvar)      !< cell-centered area at new time
real(STM_REAL),intent(in) :: area_lo(ncell,nvar)   !< lo side area centered in time
real(STM_REAL),intent(in) :: area_hi(ncell,nvar)   !< hi side area centered in time
real(STM_REAL), intent(in) :: time                 !< current time
real(STM_REAL), intent(in) :: dt                   !< current time step
real(STM_REAL), intent(in) :: dx                   !< spatial step

!>@param compute_source Callback for computing source. See the source_if interface
external :: compute_source

!---------- locals


real(STM_REAL) :: source(ncell,nvar) !< cell centered source 
real(STM_REAL) :: conc(ncell,nvar) !< cell centered source 
real(STM_REAL) :: flow(ncell) !< cell centered source 


!--------------------

mass=zero
call compute_source(source,conc,area,flow,ncell,nvar)

return
end subroutine

!///////////////////////////////////////////////////////////////////////

!> Extrapolate primitive data from cell center at the old time
!> to cell edges at the half time. The extrapolation is done by 
!> a Taylor series in time and space in which an explicit discretization
!> of the PDE is used to represent the time part.
subroutine extrapolate(conc_lo,  &
                       conc_hi,  &
                       conc,     &
                       flow,     &  
                       area,     &
                       ncell,&
                       nvar, &
                       time, &
                       dt,   &
                       dx)
use stm_precision
implicit none
!--- args
integer,intent(in)  :: ncell  !< Number of cells
integer,intent(in)  :: nvar   !< Number of variables

!> estimate from this cell extrapolated to lo face at half time
real(STM_REAL),intent(out) :: conc_lo(ncell,nvar)

!> estimate from this cell extrapolated to hi face at half time
real(STM_REAL),intent(out) :: conc_hi(ncell,nvar)
real(STM_REAL),intent(in) :: conc(ncell,nvar) !< cell centered conc at old time
real(STM_REAL),intent(in) :: area(ncell,nvar) !< cell-centered area at old time
real(STM_REAL),intent(in) :: flow(ncell,nvar) !< cell-centered flow at old time
real(STM_REAL), intent(in) :: time            !< time
real(STM_REAL), intent(in) :: dt              !< length of current time step being advanced
real(STM_REAL), intent(in) :: dx              !< spatial step


!--------------------

conc_lo=LARGEREAL
conc_hi=LARGEREAL

return
end subroutine

!///////////////////////////////////////////////////////////////////////

!> Compute the fluxes naively -- no boundary considerations yet
subroutine compute_flux(flux_lo,  &
                        flux_hi,  &
                        conc_lo,  &
                        conc_hi,  &                       
                        flow_lo,  &
                        flow_hi,  &
                        ncell,    &
                        nvar      &
                        )
use stm_precision
implicit none
!--- args
integer,intent(in)  :: ncell  !< Number of cells
integer,intent(in)  :: nvar   !< Number of variables

real(STM_REAL),intent(out) :: flux_lo(ncell,nvar) !< Flux on lo face at half time
real(STM_REAL),intent(out) :: flux_hi(ncell,nvar) !< Flux on hi face at half time
real(STM_REAL),intent(in) :: conc_lo(ncell,nvar)  !< upwinded concentration at half time at lo face
real(STM_REAL),intent(in) :: conc_hi(ncell,nvar)  !< upwinded concentration at half time at hi face
real(STM_REAL),intent(in) :: flow_lo(ncell,nvar)  !< time-centered flow at lo face
real(STM_REAL),intent(in) :: flow_hi(ncell,nvar)  !< time-centered flow at hi face


!--------------------

flux_lo=LARGEREAL
flux_hi=LARGEREAL

return
end subroutine

!///////////////////////////////////////////////////////////////////////

!> Replace original calculated flux at boundary locations
!> todo: figure out if the arguments are right and move this routine to the 
!>       application -- it should just be an interface like sources and hydro_data
!>       Also, eventually have to think out channel network
subroutine replace_boundary_flux(flux_lo,  &
                                 flux_hi,  &
                                 conc_lo,  &
                                 conc_hi,  &                       
                                 flow_lo,  &
                                 flow_hi,  &
                                 ncell,    &
                                 nvar,     &
                                 time,     &
                                 dt,       &
                                 dx)
use stm_precision
implicit none
!--- args
integer,intent(in)  :: ncell  !< Number of cells
integer,intent(in)  :: nvar   !< Number of variables

real(STM_REAL),intent(inout) :: flux_lo(ncell,nvar) !< Flux on lo face at half time
real(STM_REAL),intent(inout) :: flux_hi(ncell,nvar) !< Flux on hi face at half time
real(STM_REAL),intent(in) :: conc_lo(ncell,nvar)    !< upwinded concentration at half time at lo face
real(STM_REAL),intent(in) :: conc_hi(ncell,nvar)    !< upwinded concentration at half time at hi face
real(STM_REAL),intent(in) :: flow_lo(ncell,nvar)    !< time-centered flow at lo face
real(STM_REAL),intent(in) :: flow_hi(ncell,nvar)    !< time-centered flow at hi face
real(STM_REAL), intent(in) :: time                  !< time
real(STM_REAL), intent(in) :: dt                    !< length of current time step
real(STM_REAL), intent(in) :: dx !< spatial step


!--------------------

flux_lo=LARGEREAL
flux_hi=LARGEREAL
return
end subroutine


!///////////////////////////////////////////////////////////////////////

!> Given flows and concentrations on either side of a cell
!> calculate the upwind mass flux
subroutine upwind(flux_lo,flux_hi,conc_lo,conc_hi,flow_lo,flow_hi,ncell,nvar)
use stm_precision
implicit none
!--- args
integer,intent(in)  :: ncell  !< Number of cells
integer,intent(in)  :: nvar   !< Number of variables

real(STM_REAL),intent(out) :: flux_lo(ncell,nvar) !< flux on lo side of cells centered in time
real(STM_REAL),intent(out) :: flux_hi(ncell,nvar) !< flux on hi side of cells centered in time
real(STM_REAL),intent(in) :: conc_lo(ncell,nvar)  !< conc on lo side of cells centered in time
real(STM_REAL),intent(in) :: conc_hi(ncell,nvar)  !< conc on hi side of cells centered in time
real(STM_REAL),intent(in) :: flow_lo(ncell,nvar)  !< flow on lo side of cells centered in time
real(STM_REAL),intent(in) :: flow_hi(ncell,nvar)  !< flow on hi side of cells centered in time


!--------

flux_lo = LARGEREAL
flux_hi = LARGEREAL

return
end subroutine


end module








