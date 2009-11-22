
!> Module orchestrating the advection scheme. The main
!> routine in the module is advection().
!>@ingroup transport
module advection
use stm_precision

contains

!///////////////////////////////////////////////////////////////////////

!> Integrate advection plus sources for a time step.
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
!>   - replace_boundary_flux()   for boundary and special cases
!>   - Compute conservative divergence
!>   - Apply divergence in conservative_update along with Huen's method for sources
!>   Note that all these steps are operations on entire arrays of values -- this keeps things efficient
subroutine advect(mass,     &
                  mass_prev,&  
                  flow_lo,  &
                  flow_hi,  &
                  area,     &
                  area_prev,&
                  area_lo,  &
                  area_hi,  &
                  ncell,    &
                  nvar,     &
                  time,     &
                  dt,       &
                  dx,       &
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

call cons2prim(conc,mass_prev,area)

! Calculate the (undivided) differences
call difference(grad_lo,grad_hi,grad_center,vals,ncell,nvar)
call limiter(grad_lim,grad_lo,grad_hi,grad_center,ncell,nvar)

! Adjust differences to account for places (boundaries, gates, etc) where one-sided
! or other differencing is required
call adjust_differences(grad,grad_lim,grad_lo,grad_hi,dx)

call extrapolate(conc_lo,  &
            conc_hi,  &
            conc,     &
            grad,     &            
            flow,     &  
            area,     &
            ncell,&
            nvar, &
            time, &
            dt,   &
            dx)

! Compute upwind value of fluxes. This is a naive guess based on the extrapolated states
! It doesn't include any node-based sources or reservoirs or the like.
call compute_flux(flux_lo,           &
             flux_hi,  &
             conc_lo,  &
             conc_hi,  &                       
             flow_lo,  &
             flow_hi,  &
             ncell,    &
             nvar      &
             )

! Replace fluxes for special cases having to do with boundaries, network and structures
! Keeps the dirty stuff in one place. For now this is an empty call
call replace_boundary_flux()

! Combine the fluxes into a divergence term at the half time at cell edges.
! Computing and storing the divergence separately gives some flexibility with integrating
! the source term, e.g. Huen's method
call compute_divergence( div_flux, flux_lo, flux_hi)

! conservative update including source. 
call conservative_update(mass,mass_prev,div_flux,old_source,ncell,nvar)

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
                       grad,     &
                       source,     &                       
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
real(STM_REAL),intent(in) :: area(ncell) !< cell-centered area at old time
real(STM_REAL),intent(in) :: flow(ncell) !< cell-centered flow at old time
real(STM_REAL),intent(in) :: source(ncell) !< source terms at old time
real(STM_REAL), intent(in) :: time            !< time
real(STM_REAL), intent(in) :: dt              !< length of current time step being advanced
real(STM_REAL), intent(in) :: dx              !< spatial step
!--------------------
!--------------------
vel=flow/area
! todo make sure source is in terms of primitive variables
conc_lo = conc - half*grad + half*dt*(one-grad*vel)+half*dt*source
conc_hi = conc + half*grad + half*dt*(one-grad*vel)+half*dt*source

return
end subroutine

!///////////////////////////////////////////////////////////////////////

!> Compute the upwinded fluxes 
!> The calculation here does not include tributaries, boundaries or special objects
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
real(STM_REAL),intent(in) :: conc_lo(ncell,nvar)  !< upwinded conc at half time at lo face
real(STM_REAL),intent(in) :: conc_hi(ncell,nvar)  !< upwinded conc at half time at hi face
real(STM_REAL),intent(in) :: flow_lo(ncell,nvar)  !< time-centered flow at lo face
real(STM_REAL),intent(in) :: flow_hi(ncell,nvar)  !< time-centered flow at hi face

!--------------------
! For each constitutuent, go through the cells and calculate the upwinded flux
! todo: make sure this tests OK for the variables
do ivar = 1,nvar
    do icell = 1,ncell
        if (flow_lo(icell) > zero) then
            flux_lo(icell)=conc_hi(icell-1,ivar)*flow_hi(icell-1)
            flux_hi(icell-1)=flux_lo(icell)
        else
            flux_lo(icell)=conc_lo(icell,ivar)*flow_lo(icell)
            flux_hi(icell-1)=flux_lo(icell)
        end if
    end do
end do
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


!///////////////////////////////////////////////////////////////////////

!> Update the conservative variables using divergence of fluxes and integrate the
!> source term using Huen's method
subroutine update_conservative(mass,      &
                               mass_prev, &
                               div_flux,  &
                               old_source, &
                               source,    &
                               ncell,     &
                               nvar)
use stm_precision
implicit none
!--- args
integer,intent(in)  :: ncell  !< Number of cells
integer,intent(in)  :: nvar   !< Number of variables

real(STM_REAL),intent(out) :: mass(ncell,nvar)       !< update of mass
real(STM_REAL),intent(in)  :: mass_prev(ncell,nvar)  !< old time mass
real(STM_REAL),intent(in)  :: mass_prev(ncell,nvar)  !< old time source term
real(STM_REAL),intent(in)  :: div_flux(ncell,nvar)   !< flux divergence

!--------------------
dtbydx = dt/dx

! obtain a guess at the new state (predictor part of huen) using the flux divergence and source evaluated at the
! old time step
mass = mass_prev + dtbydx*(div_flux) + dt*source_prev

! compute the source at the new time from the predictor
call cons2prim(conc,mass)
call compute_source(source,conc)

! now recalculate the update using a source half from the old state and half from the new state guess 
mass =   mass_prev &
       + half*dtbydx*divflux &
       + half*dtbydx*source_prev &
       + half*dtbydx*source

return
end subroutine

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


end module








