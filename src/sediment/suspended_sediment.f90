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
    
    subroutine sediment_flux(source,      & !< sediment source/sink
                             conc,        & !< concentration
                             flow,        & !< flow
                             area,        & !< cross-section area
                             width,       & !< channel width
                             hyd_radius,  & !< hydraulic radius
                             manning_n,   & !< Manning's n
                             diameter,    & !< sediment diameter
                             method,      & !< sediment equation
                             dx,          & !< spatial steps
                             dt,          & !< time step
                             ncell)         !< number of cells
        use non_cohesive_source
        use cohesive_source
        implicit none
        !--- args
        integer, intent(in) :: ncell                      !< number of cells      
        real(gtm_real), intent(out) :: source(ncell)      !< source
        real(gtm_real), intent(in) :: conc(ncell)         !< concentration
        real(gtm_real), intent(in) :: flow(ncell)         !< flow
        real(gtm_real), intent(in) :: area(ncell)         !< area
        real(gtm_real), intent(in) :: width(ncell)        !< channel width
        real(gtm_real), intent(in) :: hyd_radius(ncell)   !< hydraulic radius
        real(gtm_real), intent(in) :: manning_n(ncell)    !< Manning's n
        real(gtm_real), intent(in) :: diameter            !< diameter
        real(gtm_real), intent(in) :: dx(ncell)           !< dx
        real(gtm_real), intent(in) :: dt                  !< dt
        integer, intent(in) :: method                     !< method  1: non-cohesive, 2: cohesive, 3: organic matters
        !--- local
        real(gtm_real) :: vertical_flux(ncell)
        real(gtm_real) :: erosion_flux(ncell)
        real(gtm_real) :: deposition_flux(ncell)
        real(gtm_real) :: conc_si(ncell), flow_si(ncell), area_si(ncell), dx_si(ncell)
        real(gtm_real) :: width_si(ncell), hyd_radius_si(ncell), diameter_si
        integer :: i
     
        call si_unit(conc_si, flow_si, area_si, width_si, hyd_radius_si, dx_si, diameter_si, &
                     conc, flow, area, width, hyd_radius, dx, diameter, ncell)
     
        if (method.eq.1) then     ! non-cohesive sediment            
            call source_non_cohesive(vertical_flux,    &
                                     erosion_flux,     &
                                     deposition_flux,  &
                                     conc_si,          &
                                     flow_si,          &
                                     area_si,          &
                                     width_si,         &
                                     hyd_radius_si,    &
                                     manning_n,        &
                                     diameter_si,      &
                                     ncell)

        elseif (method.eq.2) then ! cohesive sediment
            call source_cohesive(vertical_flux,       &
                                 erosion_flux,        &
                                 deposition_flux,     &
                                 conc_si,             &
                                 flow_si,             &
                                 area_si,             &
                                 width_si,            &
                                 hyd_radius_si,       &
                                 manning_n,           &
                                 diameter_si,         &
                                 ncell)
        elseif (method.eq.3) then ! organic matters
            vertical_flux = zero
        else
            write(*,*) "Invalid method for sediment calculation!!! Only accept NC, C and O"
        end if
       
        source = vertical_flux*dt*width_si/area_si
        where (area_si .eq. zero) source = zero               
       
        return 
    end subroutine
    
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
        real(gtm_real), intent(out) :: diameter_si
        real(gtm_real), intent(in) :: conc(ncell)
        real(gtm_real), intent(in) :: flow(ncell)
        real(gtm_real), intent(in) :: area(ncell)
        real(gtm_real), intent(in) :: width(ncell)
        real(gtm_real), intent(in) :: hyd_radius(ncell)
        real(gtm_real), intent(in) :: dx(ncell)
        real(gtm_real), intent(in) :: diameter
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