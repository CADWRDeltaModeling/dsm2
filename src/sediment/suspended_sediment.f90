!<license>
!    Copyright (C) 2016 State of California,
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

!> Defines the common variables for the sediment transport sources.
!> The module defines the basic parameters for the sediment transport sources based
!>@ingroup sediment
module suspended_sediment

    use gtm_precision
 
    contains
    
    subroutine sediment_flux(source,          & 
                             erosion_flux,    &
                             deposition_flux, &
                             conc,            & 
                             flow,            & 
                             area,            & 
                             width,           & 
                             hydro_radius,    & 
                             manning_n,       & 
                             diameter,        & 
                             dx,              & 
                             dt,              & 
                             ncell,           & 
                             available_bed)
        use cohesive_source
        use non_cohesive_source
        implicit none
        !--- args
        integer, intent(in) :: ncell                              !< number of cells      
        real(gtm_real), intent(out) :: source(ncell)              !< sediment source/sink
        real(gtm_real), intent(out) :: erosion_flux(ncell)        !< sediment erosion flux
        real(gtm_real), intent(out) :: deposition_flux(ncell)     !< sediment deposition flux
        real(gtm_real), intent(in) :: conc(ncell)                 !< concentration
        real(gtm_real), intent(in) :: flow(ncell)                 !< flow
        real(gtm_real), intent(in) :: area(ncell)                 !< area
        real(gtm_real), intent(in) :: width(ncell)                !< channel width
        real(gtm_real), intent(in) :: hydro_radius(ncell)         !< hydraulic radius
        real(gtm_real), intent(in) :: manning_n(ncell)            !< Manning's n
        real(gtm_real), intent(in) :: diameter(ncell)             !< diameter
        real(gtm_real), intent(in) :: dx(ncell)                   !< dx
        real(gtm_real), intent(in) :: dt                          !< dt
        real(gtm_real), intent(in) :: available_bed(ncell)        !< available bed sediment flux
        !--- local
        real(gtm_real), parameter :: default_fines_size = 0.00002d0
        real(gtm_real), parameter :: default_sand_size =  0.00005d0
        real(gtm_real), parameter :: velocity_for_erosion = 1.0d0
        real(gtm_real) :: vertical_flux(ncell)
        real(gtm_real) :: vertical_flux_nc(ncell)
        real(gtm_real) :: erosion_flux_nc(ncell), deposition_flux_nc(ncell)
        real(gtm_real) :: conc_si(ncell), flow_si(ncell), area_si(ncell), dx_si(ncell)
        real(gtm_real) :: width_si(ncell), hydro_radius_si(ncell), diameter_si(ncell)
        real(gtm_real) :: diameter_tmp(ncell)
        real(gtm_real) :: velocity(ncell)
        integer :: icell
        
        call si_unit(conc_si, flow_si, area_si, width_si, hydro_radius_si, dx_si, diameter_si, &
                     conc, flow, area, width, hydro_radius, dx, diameter, ncell)
  
        velocity = abs(flow_si/area_si)     
        
        diameter_tmp = diameter_si
        where (diameter_si.ge.0.00100d0) diameter_tmp = default_fines_size           
        call source_cohesive(vertical_flux,      & 
                             erosion_flux,       & 
                             deposition_flux,    & 
                             conc_si,            &
                             flow_si,            & 
                             area_si,            & 
                             width_si,           & 
                             hydro_radius_si,    & 
                             manning_n,          & 
                             diameter_tmp,       & 
                             ncell,              &         
                             available_bed)     
             
        diameter_tmp = default_sand_size
        call source_non_cohesive(vertical_flux_nc,    &
                                 erosion_flux_nc,     &
                                 deposition_flux_nc,  &
                                 conc_si,             &
                                 flow_si,             & 
                                 area_si,             & 
                                 width_si,            & 
                                 hydro_radius_si,     & 
                                 manning_n,           & 
                                 diameter_tmp,        &
                                 ncell,               &
                                 available_bed)

        do icell = 1, ncell
            if ((diameter_si(icell).ge.0.00100d0 .and. erosion_flux(icell).ne.zero) &
                .or.(diameter_si(icell).ge.0.00300d0)) then
                vertical_flux(icell) = zero
                erosion_flux(icell) = deposition_flux(icell)
            end if    
            if ((diameter_si(icell).ge.0.00200d0 .and. diameter_si(icell).lt.0.00250d0) &
                .and.(velocity(icell).gt.velocity_for_erosion)) then
                vertical_flux(icell) = vertical_flux_nc(icell)
                erosion_flux(icell) = erosion_flux_nc(icell)
                deposition_flux(icell) = deposition_flux_nc(icell)
            end if 
        end do
         
        source = vertical_flux*dx_si*width_si/area_si
        where (area_si .eq. zero) source = zero               
       
        return 
    end subroutine
    
    !> Convert all variables from English unit to SI unit
    subroutine si_unit(conc_si,        &
                       flow_si,        &
                       area_si,        &
                       width_si,       &
                       hyd_radius_si,  &
                       dx_si,          &                       
                       diameter_si,    &
                       conc,           &
                       flow,           &
                       area,           &
                       width,          &
                       hyd_radius,     &
                       dx,             &
                       diameter,       &
                       ncell) 
        implicit none
        integer, intent(in) :: ncell
        real(gtm_real), intent(out) :: conc_si(ncell)
        real(gtm_real), intent(out) :: flow_si(ncell)
        real(gtm_real), intent(out) :: area_si(ncell)
        real(gtm_real), intent(out) :: width_si(ncell)
        real(gtm_real), intent(out) :: hyd_radius_si(ncell)
        real(gtm_real), intent(out) :: dx_si(ncell)
        real(gtm_real), intent(out) :: diameter_si(ncell)
        real(gtm_real), intent(in) :: conc(ncell)
        real(gtm_real), intent(in) :: flow(ncell)
        real(gtm_real), intent(in) :: area(ncell)
        real(gtm_real), intent(in) :: width(ncell)
        real(gtm_real), intent(in) :: hyd_radius(ncell)
        real(gtm_real), intent(in) :: dx(ncell)
        real(gtm_real), intent(in) :: diameter(ncell)
        real(gtm_real), parameter :: L = 0.3048d0
        
        conc_si = conc*0.001d0         ! mg/L-->kg/m3
        flow_si = flow*L*L*L           ! cfs-->m3/s
        area_si = area*L*L             ! ft^2-->m^2
        width_si = width*L             ! ft-->m
        hyd_radius_si = hyd_radius*L   ! ft-->m
        dx_si = dx*L                   ! ft-->m
        diameter_si = diameter*0.001d0 ! mm-->m
        
        return
    end subroutine    
    
end module    