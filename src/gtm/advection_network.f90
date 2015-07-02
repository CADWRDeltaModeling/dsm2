!<license>
!    Copyright (C) 2015 State of California,
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

!> Module orchestrating the advection scheme. This has some enhancements 
!> to accommodate network features. 
!>@ingroup gtm_driver
module advection_network

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
    subroutine advect_network(mass,                 &
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
        use advection
        use gtm_precision
        use primitive_variable_conversion
        use gradient
        use source_sink
        use boundary_advection
        use boundary_concentration
        use gradient_adjust        
        use boundary_concentration
        use dsm2_gtm_network

        implicit none
 
        !--- args
        integer, intent(in) :: ncell                         !< Number of cells
        integer, intent(in) :: nvar                          !< Number of variables
        real(gtm_real), intent(out) :: mass(ncell,nvar)      !< mass at new time
        real(gtm_real), intent(in)  :: mass_prev(ncell,nvar) !< mass at old time
        real(gtm_real), intent(in)  :: flow_prev(ncell)      !< cell-centered flow, old time
        real(gtm_real), intent(in)  :: flow_lo(ncell)        !< flow on lo side of cells centered in time
        real(gtm_real), intent(in)  :: flow_hi(ncell)        !< flow on hi side of cells centered in time
        real(gtm_real), intent(in)  :: area_prev(ncell)      !< cell-centered area at old time. not used in algorithm?
        real(gtm_real), intent(in)  :: area(ncell)           !< cell-centered area at new time. not used in algorithm?
        real(gtm_real), intent(in)  :: area_lo(ncell)        !< lo side area (todo: at new time?)
        real(gtm_real), intent(in)  :: area_hi(ncell)        !< hi side area (todo: at new time?
        real(gtm_real), intent(in)  :: time                  !< new time
        real(gtm_real), intent(in)  :: dt                    !< current time step from old time to new time
        real(gtm_real), intent(in)  :: dx(ncell)             !< spatial step
        logical, intent(in), optional :: use_limiter         !< whether to use slope limiter
        
        !-----locals
        real(gtm_real) :: source_prev(ncell,nvar)            !< cell centered source at old time
        real(gtm_real) :: conc_prev(ncell,nvar)              !< cell centered concentration at old time
        real(gtm_real) :: conc_lo(ncell,nvar)                !< concentration extrapolated to lo face at half time
        real(gtm_real) :: conc_hi(ncell,nvar)                !< concentration extrapolated to hi face at half time
        real(gtm_real) :: grad_lo(ncell,nvar)                !< gradient based on lo side difference
        real(gtm_real) :: grad_hi(ncell,nvar)                !< gradient based on hi side difference
        real(gtm_real) :: grad_center(ncell,nvar)            !< cell centered difference
        real(gtm_real) :: grad_lim(ncell,nvar)               !< limited cell centered difference
        real(gtm_real) :: grad(ncell,nvar)                   !< cell centered difference adujsted for boundaries and hydraulic devices
        real(gtm_real) :: flux_lo(ncell,nvar)                !< flux on lo side of cell, time centered
        real(gtm_real) :: flux_hi(ncell,nvar)                !< flux on hi side of cell, time centered
        real(gtm_real) :: div_flux(ncell,nvar)               !< cell centered flux divergence, time centered
        logical        :: limit_slope                        !< whether slope limiter is used
        real(gtm_real) :: old_time                           !< previous time
        real(gtm_real) :: half_time                          !< half time
        integer :: i, j, icell

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
        call difference_network(grad_lo,    &
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

        ! Assign boundary concentration if it is given
        if (associated(boundary_conc)) then        
            call boundary_conc(conc_lo,              &
                               conc_hi,              & 
                               ncell,                &
                               nvar)
        end if  

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
                                 
        ! Adjust flux for boundaries and junctions
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
            ! default method is for single channel which only updates at two ends of the channel
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
        ! Computing and storing the divergence separately gives some flexibility with
        ! integrating the source term, e.g. Heun's method
        call compute_divergence(div_flux,   &
                                flux_lo,    &
                                flux_hi,    &
                                ncell,      &
                                nvar)
                                    
        ! Conservative update including source. 
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
        
end module