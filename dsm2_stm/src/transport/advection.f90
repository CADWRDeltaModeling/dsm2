
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
                  flow,     &                  
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
                  dx)
!use source_if
use primitive_variable_conversion
use gradient
implicit none

!--- args
integer,intent(in)  :: ncell  !< Number of cells
integer,intent(in)  :: nvar   !< Number of variables

real(STM_REAL),intent(out) :: mass(ncell,nvar)     !< mass concentration at new time
real(STM_REAL),intent(in) :: mass_prev(ncell,nvar) !< mass concentration at new time
real(STM_REAL),intent(in) :: flow   (ncell,nvar)   !< cell-centered flow, old time
real(STM_REAL),intent(in) :: flow_lo(ncell,nvar)   !< flow on lo side of cells centered in time
real(STM_REAL),intent(in) :: flow_hi(ncell,nvar)   !< flow on hi side of cells centered in time
real(STM_REAL),intent(in) :: area_prev(ncell,nvar) !< cell-centered area at old time??
real(STM_REAL),intent(in) :: area(ncell,nvar)      !< cell-centered area at new time
real(STM_REAL),intent(in) :: area_lo(ncell,nvar)   !< lo side area centered in time
real(STM_REAL),intent(in) :: area_hi(ncell,nvar)   !< hi side area centered in time
real(STM_REAL), intent(in) :: time                 !< current time
real(STM_REAL), intent(in) :: dt                   !< current time step
real(STM_REAL), intent(in) :: dx                   !< spatial step


!---------- locals


real(STM_REAL) :: source(ncell,nvar) !< cell centered source 
real(STM_REAL) :: conc(ncell,nvar) !< cell centered concentration
real(STM_REAL) :: conc_lo(ncell,nvar) !< concentration extrapolated to lo face
real(STM_REAL) :: conc_hi(ncell,nvar) !< concentration extrapolated to hi face
real(STM_REAL) :: grad_lo(ncell,nvar) !< gradient based on lo side difference
real(STM_REAL) :: grad_hi(ncell,nvar) !< gradient based on hi side difference
real(STM_REAL) :: grad_center(ncell,nvar) !< cell centered difference
real(STM_REAL) :: grad_lim(ncell,nvar) !< limited cell centered difference
real(STM_REAL) :: grad(ncell,nvar)     !< cell centered difference adusted for boundaries and hydraulic devices

real(STM_REAL) :: flux_lo(ncell,nvar) !< flux on lo side of cell, time centered
real(STM_REAL) :: flux_hi(ncell,nvar) !< flux on hi side of cell, time centered
 
real(STM_REAL) :: div_flux(ncell,nvar)!< cell centered flux divergence, time centered


call cons2prim(conc,mass_prev,area,ncell,nvar)

! Calculate the (undivided) differences of concentrations
call difference(grad_lo,grad_hi,grad_center,conc,ncell,nvar)
!call printout(grad_center(:,1),ncell)
call limiter(grad_lim,grad_lo,grad_hi,grad_center,ncell,nvar)

! Adjust differences to account for places (boundaries, gates, etc) where one-sided
! or other differencing is required
! todo: needs to be implemented
call adjust_differences(grad,grad_lim,grad_lo,grad_hi,ncell,nvar)

! todo: commented
!call compute_source(source,conc,ncell,nvar)
!todo: source hardwired
source = zero

call extrapolate(conc_lo,  &
                 conc_hi,  & 
                 conc,     &
                 grad,     &            
                 source,   &
                 flow,     &  
                 area,     &
                 ncell,    &
                 nvar,     &
                 time,     &
                 dt,       &
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
call replace_boundary_flux(flux_lo,flux_hi,conc_lo,conc_hi,flow_lo,flow_hi,ncell,nvar,time,dt,dx)

! Combine the fluxes into a divergence term at the half time at cell edges.
! Computing and storing the divergence separately gives some flexibility with integrating
! the source term, e.g. Huen's method
! todo: commented
call compute_divergence( div_flux, flux_lo, flux_hi, ncell, nvar)


! conservative update including source. 
call update_conservative(mass,mass_prev,div_flux,source,area,ncell,nvar,dt,dx)

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
real(STM_REAL),intent(in) :: grad(ncell,nvar) !< cell centered gradient of conc at old time, currently assuming these are undivided differences
real(STM_REAL),intent(in) :: area(ncell)      !< cell-centered area at old time
real(STM_REAL),intent(in) :: flow(ncell)      !< cell-centered flow at old time
real(STM_REAL),intent(in) :: source(ncell,nvar) !< source terms at old time
real(STM_REAL), intent(in) :: time            !< time
real(STM_REAL), intent(in) :: dt              !< length of current time step being advanced
real(STM_REAL), intent(in) :: dx              !< spatial step
!----- locals
real(STM_REAL) :: vel(ncell) !< cell-centered flow at old time
integer        :: ivar
real(STM_REAL) :: dtbydx
!--------------------
vel=flow/area
dtbydx = dt/dx

do ivar = 1,nvar
    ! todo make sure source is in terms of primitive variables
    conc_lo(:,ivar) = conc(:,ivar) !- half*grad(:,ivar) - half*dtbydx*grad(:,ivar)*flow/area+half*dt*source(:,ivar)
    conc_hi(:,ivar) = conc(:,ivar) !+ half*grad(:,ivar) - half*dtbydx*grad(:,ivar)*flow/area+half*dt*source(:,ivar)
end do
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
real(STM_REAL),intent(in) :: flow_lo(ncell)  !< time-centered flow at lo face
real(STM_REAL),intent(in) :: flow_hi(ncell)  !< time-centered flow at hi face
!---- locals
integer :: ivar
integer :: icell

!--------------------
! For each constitutuent, go through the cells and calculate the upwinded flux
! todo: make sure this tests OK for the variables
do ivar = 1,nvar
    do icell = 2,ncell
        if (flow_lo(icell) > zero) then
            flux_lo(icell,ivar)=conc_hi(icell-1,ivar)*flow_hi(icell-1)
        else
            flux_lo(icell,ivar)=conc_lo(icell,ivar)*flow_lo(icell)
        end if
    end do
    do icell = 1,(ncell-1)
        if (flow_hi(icell) > zero) then
            flux_hi(icell,ivar)=conc_hi(icell,ivar)*flow_hi(icell)
        else
            flux_hi(icell,ivar)=conc_lo(icell+1,ivar)*flow_lo(icell+1)
        end if
    end do
    if (flow_lo(1) > zero) flux_lo(1,ivar) = LARGEREAL                   ! boundary: handled elsewhere
    if (flow_lo(1) < zero) flux_lo(1,ivar) = conc_lo(1,ivar)*flow_lo(1)  ! interior
    if (flow_hi(ncell) < zero) flux_hi(ncell,ivar) = LARGEREAL           ! boundary: handled elsewhere
    if (flow_hi(ncell) > zero) flux_hi(ncell,ivar) = conc_hi(ncell,ivar)*flow_hi(ncell)  ! interior
end do

return
end subroutine

!//////////////////////////////////////////////////////////////////////

!> compute the divergence of fluxes
subroutine compute_divergence(div_flux, flux_lo, flux_hi, ncell, nvar)
implicit none

!--- args
integer,intent(in)  :: ncell  !< Number of cells
integer,intent(in)  :: nvar   !< Number of variables
real(STM_REAL),intent(out) :: div_flux(ncell,nvar)!< cell centered flux divergence, time centered
real(STM_REAL),intent(in)  :: flux_lo(ncell,nvar) !< flux on lo side of cell, time centered
real(STM_REAL),intent(in)  :: flux_hi(ncell,nvar) !< flux on hi side of cell, time centered 
!-----------

div_flux = (flux_hi - flux_lo)
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

flux_lo(1,:)= zero
flux_hi(ncell,:)=zero
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
subroutine update_conservative(mass,       &
                               mass_prev,  &
                               div_flux,   &
                               source_prev,&
                               area,       &
                               ncell,      &
                               nvar,   &
                               dt,     &
                               dx      &                   
                               )
use stm_precision
use primitive_variable_conversion
implicit none
!--- args
integer,intent(in)  :: ncell  !< Number of cells
integer,intent(in)  :: nvar   !< Number of variables

real(STM_REAL),intent(out) :: mass(ncell,nvar)       !< update of mass
real(STM_REAL),intent(in)  :: mass_prev(ncell,nvar)  !< old time mass
real(STM_REAL),intent(in)  :: area(ncell)            !< area of cells
real(STM_REAL),intent(in)  :: source_prev(ncell,nvar)!< old time source term
real(STM_REAL),intent(in)  :: div_flux(ncell,nvar)   !< flux divergence, time centered
real(STM_REAL), intent(in) :: dt                     !< length of current time step
real(STM_REAL), intent(in) :: dx                     !< spatial step

!--- locals
real(STM_REAL) :: dtbydx
real(STM_REAL) :: source(ncell,nvar)             !< new time source term
real(STM_REAL) :: conc(ncell,nvar)               !< concentration


!--------------------
dtbydx = dt/dx

! obtain a guess at the new state (predictor part of huen) using the flux divergence and source evaluated at the
! old time step
mass = mass_prev - div_flux + dt*source_prev

! compute the source at the new time from the predictor
call cons2prim(conc,mass,area,ncell,nvar)

! todo:commented
!call compute_source(source,conc,ncell,nvar)
source = zero

! now recalculate the update using a source half from the old state and half from the new state guess 
mass =   mass_prev &
       - div_flux &
       + dt*half*source_prev &
       + dt*half*source

return
end subroutine

end module
!//////////////////////////////

subroutine adjust_differences(grad,grad_lim,grad_lo,grad_hi,ncell,nvar)
use stm_precision
implicit none
!--- args
integer,intent(in)  :: ncell  !< Number of cells
integer,intent(in)  :: nvar   !< Number of variables

real(STM_REAL) :: grad_lo(ncell,nvar) !< gradient based on lo side difference
real(STM_REAL) :: grad_hi(ncell,nvar) !< gradient based on hi side difference
real(STM_REAL) :: grad_lim(ncell,nvar) !< limited cell centered difference
real(STM_REAL) :: grad(ncell,nvar)     !< cell centered difference adusted for boundaries and hydraulic devices
!---------
grad          = grad_lim  !todo: not complete, but correct for simple channel
grad(1,:)     = grad_hi(1,:)
grad(ncell,:) = grad_lo(ncell,:)
return
end subroutine


subroutine printout(arr,lenarr)
use stm_precision
implicit none
integer, intent(in) :: lenarr
real(STM_REAL),intent(in) :: arr(lenarr)
integer icell
print*,"printing"
open(unit = 11, file = 'd:\temp\out.txt')
do icell = 1,lenarr
  write(11,*)arr(icell)
end do
close(11)
end subroutine


