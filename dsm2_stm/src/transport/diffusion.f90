
!> Explicit and implicit diffusion operators go here
!>@ingroup transport
module diffusion

! We need a single step routine with inputs and outputs that
! do not involve computation detail (like matrices)
! This is just stolen from the interface for advection, 
! but the real diffusion API will look similar
!subroutine diffuse(mass,     &
!                  mass_prev,&
!                  flow,     &                  
!                  flow_lo,  &
!                  flow_hi,  &
!                  area,     &
!                  area_prev,&
!                  area_lo,  &
!                  area_hi,  &
!                  ncell,    &
!                  nvar,     &
!                  time,     &
!                  dt,       &
!                  dx)

! This routine should give the effects of diffusion fluxes on each cell
! for a single time step (ie, explicit). This is needed for the advection step.
! It is also probably part of the right hand side of the implicit diffusion solver 
! matrix calculation. 
!subroutine diffusion_operator(mass,     &
!                  mass_prev,&
!                  flow,     &                  
!                  flow_lo,  &
!                  flow_hi,  &
!                  area,     &
!                  area_prev,&
!                  area_lo,  &
!                  area_hi,  &
!                  ncell,    &
!                  nvar,     &
!                  time,     &
!                  dt,       &
!                  dx)

! The rest of this should be neat, but is not important to the public use of the library


end module