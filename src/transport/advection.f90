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

!> Module orchestrating the advection scheme. The main
!> routine in the module is advection().
!>@ingroup transport
module advection

    contains

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
    !>   - advection_boundary_flux() for boundary and special cases
    !>   - Compute conservative divergence
    !>   - Apply divergence in conservative_update along with Heun's method for sources.
    !>   Note that all these steps are operations on entire arrays of values -- this keeps things efficient
    subroutine advect(mass,                 &
                      mass_prev,            &
                      flow_prev,            &      
                      flow_lo,              &
                      flow_hi,              &
                      area,                 &
                      area_prev,            & 
                      area_lo,              &
                      area_hi,              &
                      ncell,                &
                      nvar,                 &
                      time,                 &
                      dt,                   &
                      dx,                   &
                      use_limiter)  
   
        use gtm_precision
        use primitive_variable_conversion
        use gradient
        use source_sink
        use boundary_advection
        use boundary_concentration
        use gradient_adjust        
        use boundary_concentration

        implicit none
 
        !--- args
        integer, intent(in) :: ncell                         !< Number of cells
        integer, intent(in) :: nvar                          !< Number of variables

        real(gtm_real),intent(out) :: mass(ncell,nvar)       !< mass at new time
        real(gtm_real),intent(in)  :: mass_prev(ncell,nvar)  !< mass at old time
        real(gtm_real),intent(in)  :: flow_prev(ncell)       !< cell-centered flow, old time
        real(gtm_real),intent(in)  :: flow_lo(ncell)         !< flow on lo side of cells centered in time
        real(gtm_real),intent(in)  :: flow_hi(ncell)         !< flow on hi side of cells centered in time
        real(gtm_real),intent(in)  :: area_prev(ncell)       !< cell-centered area at old time. not used in algorithm?
        real(gtm_real),intent(in)  :: area(ncell)            !< cell-centered area at new time. not used in algorithm?

        ! todo: area_lo is time centered here? I think currently it is correct for advection only.
        !       however, area_lo is also needed for diffusion at old time and new time.
        !       including being needed here if we want to include explicit diffusion op (though strictly speaking,
        !       it may be adequately accurate to have a first order estimate and the half time estimate is first order)

        ! todo: should we separate hydro_if for centered and face data?
        real(gtm_real),intent(in)  :: area_lo(ncell)                  !< lo side area (todo: at new time?)
        real(gtm_real),intent(in)  :: area_hi(ncell)                  !< hi side area (todo: at new time?
        real(gtm_real),intent(in)  :: time                            !< new time
        real(gtm_real),intent(in)  :: dt                              !< current time step from old time to new time
        real(gtm_real),intent(in)  :: dx(ncell)                       !< spatial step
        logical,intent(in),optional :: use_limiter                    !< whether to use slope limiter
        
        !-----locals
        real(gtm_real) :: source_prev(ncell,nvar) !< cell centered source at old time
        real(gtm_real) :: conc_prev(ncell,nvar)   !< cell centered concentration at old time
        real(gtm_real) :: conc_lo(ncell,nvar)     !< concentration extrapolated to lo face at half time
        real(gtm_real) :: conc_hi(ncell,nvar)     !< concentration extrapolated to hi face at half time
        real(gtm_real) :: grad_lo(ncell,nvar)     !< gradient based on lo side difference
        real(gtm_real) :: grad_hi(ncell,nvar)     !< gradient based on hi side difference
        real(gtm_real) :: grad_center(ncell,nvar) !< cell centered difference
        real(gtm_real) :: grad_lim(ncell,nvar)    !< limited cell centered difference
        real(gtm_real) :: grad(ncell,nvar)        !< cell centered difference adujsted for boundaries and hydraulic devices
        real(gtm_real) :: flux_lo(ncell,nvar)     !< flux on lo side of cell, time centered
        real(gtm_real) :: flux_hi(ncell,nvar)     !< flux on hi side of cell, time centered
        real(gtm_real) :: div_flux(ncell,nvar)    !< cell centered flux divergence, time centered
        logical        :: limit_slope             !< whether slope limiter is used
        real(gtm_real) :: old_time                !< previous time
        real(gtm_real) :: half_time               !< half time
        integer :: i, icell

        old_time = time - dt
        half_time = time - half*dt
    
        if (present(use_limiter))then
            limit_slope = use_limiter
        else
            limit_slope = .true.
        end if
        ! Converts the conservative variable (mass) to the primitive variable (concentration)
        call cons2prim(conc_prev,&
                       mass_prev,&
                       area_prev,&
                       ncell,    &
                       nvar)

        ! Calculate the (undivided) differences of concentrations
        call difference(grad_lo,    &
                        grad_hi,    &
                        grad_center,&
                        conc_prev,  &
                        dx,         &
                        ncell,      &
                        nvar)
                        
        ! Adjust differences to account for places (boundaries, gates, etc) where one-sided
        ! or other differencing is required
        if (associated(adjust_gradient)) then
            call adjust_gradient(grad,         &
                                 grad_lo,      &  
                                 grad_hi,      &
                                 grad_center,  &
                                 conc_prev,    &
                                 dx,           &
                                 ncell,        &
                                 nvar,         &
                                 use_limiter)    
        else
            adjust_gradient => adjust_differences_single_channel
            call adjust_differences_single_channel(grad,         &
                                                   grad_lo,      &  
                                                   grad_hi,      &
                                                   grad_center,  &
                                                   conc_prev,    &
                                                   dx,           &
                                                   ncell,        &
                                                   nvar,         &
                                                   use_limiter)                              
        end if        
                    
                                
        ! Compute sources and sinks for each constituent
        call compute_source(source_prev, & 
                            conc_prev,   &
                            area_prev,   &
                            flow_prev,   &
                            ncell,       &
                            nvar,        &
                            old_time)
                            
        ! Extrapolate primitive data from cell center at the old time
        call extrapolate(conc_lo,     &
                         conc_hi,     & 
                         conc_prev,   &
                         grad,        &            
                         source_prev, &
                         flow_prev,   &  
                         area_prev,   &
                         ncell,       &
                         nvar,        &
                         time,        &
                         dt,          &
                         dx)

        ! Compute upwind value of fluxes. This is a naive guess based on the extrapolated states
        ! It doesn't include any node-based sources or reservoirs or the like.
        call compute_flux(flux_lo,    &
                          flux_hi,    &
                          conc_lo,    &
                          conc_hi,    &                       
                          flow_lo,    &
                          flow_hi,    &
                          ncell,      &
                          nvar)

        ! Assign boundary concentration if it is given
        if (associated(boundary_conc)) then        
            call boundary_conc(conc_lo,              &
                               conc_hi,              & 
                               ncell,                &
                               nvar)
        end if                              
               
        if (associated(advection_boundary_flux)) then
            call advection_boundary_flux(flux_lo,     &
                                         flux_hi,     &
                                         conc_lo,     &
                                         conc_hi,     &
                                         flow_lo,     &
                                         flow_hi,     &
                                         ncell,       &
                                         nvar,        &
                                         half_time,   &
                                         dt,          &
                                         dx)        
        else        
            advection_boundary_flux => bc_advection_flux
            call bc_advection_flux(flux_lo,     &
                                   flux_hi,     &
                                   conc_lo,     &
                                   conc_hi,     &
                                   flow_lo,     &
                                   flow_hi,     &
                                   ncell,       &
                                   nvar,        &
                                   half_time,   &
                                   dt,          &
                                   dx)       
        end if
                                  
        ! Combine the fluxes into a divergence term at the half time at cell edges.
        ! Computing and storing the divergence separately gives some flexibility with integrating
        ! the source term, e.g. Heun's method
        ! todo: commented
        call compute_divergence(div_flux,   &
                                flux_lo,    &
                                flux_hi,    &
                                ncell,      &
                                nvar)
                                    
        !Conservative update including source. 
        call update_conservative(mass,        &
                                 mass_prev,   &
                                 div_flux,    &
                                 source_prev, & 
                                 area,        &
                                 area_prev,   &                         
                                 ncell,       &
                                 nvar,        &
                                 time,        &
                                 dt,          &
                                 dx)
         return
    end subroutine


    !> Extrapolate primitive data from cell center at the old time
    !> to cell edges at the half time. The extrapolation is done by 
    !> a Taylor series in time and space in which an explicit discretization
    !> of the PDE is used to represent the time part.
    pure subroutine extrapolate(conc_lo,  &
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
        use gtm_precision
        implicit none
        !--- args
        integer,intent(in)  :: ncell                     !< Number of cells
        integer,intent(in)  :: nvar                      !< Number of variables
        real(gtm_real),intent(out):: conc_lo(ncell,nvar) !< estimate from this cell extrapolated to lo face at half time
        real(gtm_real),intent(out):: conc_hi(ncell,nvar) !< estimate from this cell extrapolated to hi face at half time
        real(gtm_real),intent(in) :: conc(ncell,nvar)    !< cell centered conc at old time
        real(gtm_real),intent(in) :: grad(ncell,nvar)    !< cell centered difference of conc at old time, currently assuming these are undivided differences
        real(gtm_real),intent(in) :: area(ncell)         !< cell-centered area at old time
        real(gtm_real),intent(in) :: flow(ncell)         !< cell-centered flow at old time
        real(gtm_real),intent(in) :: source(ncell,nvar)  !< source terms at old time
        real(gtm_real),intent(in) :: time                !< time
        real(gtm_real),intent(in) :: dt                  !< length of current time step being advanced
        real(gtm_real),intent(in) :: dx(ncell)           !< spatial step
        !----- locals
        integer        :: ivar
        real(gtm_real) :: vel(ncell)                     !< cell-centered flow at old time - todo: velocity or flow?
        !--------------------
        vel = flow/area

        do ivar = 1,nvar
            ! todo: make sure source is in terms of primitive variables
            ! todo: this only works if I disable extrapolation (first order Godunov)
            conc_lo(:,ivar) = conc(:,ivar) + half*(-grad(:,ivar)*dx - dt*grad(:,ivar)*vel + dt*source(:,ivar))
            conc_hi(:,ivar) = conc(:,ivar) + half*( grad(:,ivar)*dx - dt*grad(:,ivar)*vel + dt*source(:,ivar))
        end do

        return
    end subroutine

    !> Compute the upwinded fluxes 
    !> The calculation here does not include tributaries, boundaries or special objects
    pure subroutine compute_flux(flux_lo,  &
                                 flux_hi,  &
                                 conc_lo,  &
                                 conc_hi,  &                       
                                 flow_lo,  &
                                 flow_hi,  &
                                 ncell,    &
                                 nvar)
                             
        use gtm_precision
        implicit none
        !--- args
        integer,intent(in)  :: ncell                      !< Number of cells
        integer,intent(in)  :: nvar                       !< Number of variables
        real(gtm_real),intent(out) :: flux_lo(ncell,nvar) !< Flux on lo face at half time
        real(gtm_real),intent(out) :: flux_hi(ncell,nvar) !< Flux on hi face at half time
        real(gtm_real),intent(in)  :: conc_lo(ncell,nvar) !< upwinded conc at half time at lo face
        real(gtm_real),intent(in)  :: conc_hi(ncell,nvar) !< upwinded conc at half time at hi face
        real(gtm_real),intent(in)  :: flow_lo(ncell)      !< time-centered flow at lo face
        real(gtm_real),intent(in)  :: flow_hi(ncell)      !< time-centered flow at hi face
        !---- locals
        integer :: ivar
        integer :: icell

        ! For each constitutuent, go through the cells and calculate the upwinded flux
        ! todo: make sure this tests OK for the variables
        ! todo: this could cause problems in mass conservation. kevin
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
            flux_lo(1,ivar) = conc_lo(1,ivar)*flow_lo(1) 
            flux_hi(ncell,ivar) = conc_hi(ncell,ivar)*flow_hi(ncell)
            !if (flow_lo(1) > zero) flux_lo(1,ivar) = LARGEREAL                   ! boundary: handled elsewhere
            !if (flow_lo(1) < zero) flux_lo(1,ivar) = conc_lo(1,ivar)*flow_lo(1)  ! interior
            !if (flow_hi(ncell) < zero) flux_hi(ncell,ivar) = LARGEREAL           ! boundary: handled elsewhere
            !if (flow_hi(ncell) > zero) flux_hi(ncell,ivar) = conc_hi(ncell,ivar)*flow_hi(ncell)  ! interior
        end do

        return
    end subroutine

    !> Compute the divergence of fluxes.
    ! todo: At present, this is undivided...which may be not what we want.
    subroutine compute_divergence(div_flux, &
                                  flux_lo,  &
                                  flux_hi,  &
                                  ncell,    &
                                  nvar)

        use gtm_precision
        implicit none
        !--- args
        integer,intent(in)  :: ncell                      !< Number of cells
        integer,intent(in)  :: nvar                       !< Number of variables
        real(gtm_real),intent(out) :: div_flux(ncell,nvar)!< Cell centered flux divergence, time centered
        real(gtm_real),intent(in)  :: flux_lo(ncell,nvar) !< Flux on lo side of cell, time centered
        real(gtm_real),intent(in)  :: flux_hi(ncell,nvar) !< Flux on hi side of cell, time centered 

        div_flux = (flux_hi - flux_lo)

        return
    end subroutine

    !> Update the conservative variables using divergence of fluxes and integrate the
    !> source term using Heun's method
    subroutine update_conservative(mass,       &
                                   mass_prev,  &
                                   div_flux,   &
                                   source_prev,&
                                   area,       &
                                   area_prev,  &                               
                                   ncell,      &
                                   nvar,       &
                                   time,       &
                                   dt,         &
                                   dx)      
        use gtm_precision
        use primitive_variable_conversion
        use source_sink

        implicit none
        !--- args
        integer,intent(in)  :: ncell                         !< Number of cells
        integer,intent(in)  :: nvar                          !< Number of variables

        real(gtm_real),intent(out) :: mass(ncell,nvar)       !< Update of mass
        real(gtm_real),intent(in)  :: mass_prev(ncell,nvar)  !< Old time mass
        real(gtm_real),intent(in)  :: area(ncell)            !< Area of cells
        real(gtm_real),intent(in)  :: area_prev(ncell)       !< Area of cells at old time step
        real(gtm_real),intent(in)  :: source_prev(ncell,nvar)!< Old time source term
        real(gtm_real),intent(in)  :: div_flux(ncell,nvar)   !< Flux divergence, time centered
        real(gtm_real),intent(in)  :: time                   !< Current (new) time
        real(gtm_real),intent(in)  :: dt                     !< Length of current time step
        real(gtm_real),intent(in)  :: dx(ncell)              !< Spatial step

        !--- locals
        real(gtm_real) :: dtbydx(ncell)
        real(gtm_real) :: source(ncell,nvar)                 !< New time source term
        real(gtm_real) :: conc(ncell,nvar)                   !< Concentration
        real(gtm_real) :: flow(ncell)                        !< Cell centered flow 
        integer :: ivar
        !--------------------
        dtbydx = dt/dx

       !todo: kaveh, this is major. Up to now flow has been just an uninitialized, essentially random number
       !      I did this to make it safer. However, I doubt that we will be able to use flow in a source term
       !      at multiple time steps
       !todo: Eli, what do you mean by 'we will be able to use flow in a source term...'?
       flow = LARGEREAL

       ! obtain a guess at the new state (predictor part of Huen) using the flux divergence and source evaluated at the
       ! old time step
       do ivar=1,nvar
           mass(:,ivar) = mass_prev(:,ivar) - dtbydx*div_flux(:,ivar) + dt*source_prev(:,ivar)*area_prev
       end do
       
       ! compute the source at the new time from the predictor
       call cons2prim(conc,    &
                      mass,    &
                      area,    &
                      ncell,   &
                      nvar)

       call compute_source(source, & 
                           conc,   &
                           area,   &
                           flow,   &    ! todo: this is not really available yet
                           ncell,  &
                           nvar,   &
                           time) 

       ! now recalculate the update using a source half from the old state 
       ! and half from the new state guess 
       do ivar = 1,nvar
           mass(:,ivar) =  mass_prev(:,ivar)                     &
                         - dtbydx*div_flux(:,ivar)               &
                         + dt*half*source_prev(:,ivar)*area_prev &
                         + dt*half*source(:,ivar)*area
       end do    
       return
    end subroutine

end module
