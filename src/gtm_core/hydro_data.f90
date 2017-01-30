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

!> Hydrodynamics interface to be fulfilled by driver or application
!>@ingroup gtm_core
module hydro_data
      !> Generic interface for fetching hydrodynamic data
      abstract interface
        !> Fill in hydrodynamic data.
        !> This data might be calculated from a function or provided by another module
        !> Note that continuity must be satisfied between time steps. The implementation
        !> must be provided by the driver or application 
        subroutine hydro_data_if(flow,    &
                                 flow_lo, &
                                 flow_hi, &
                                 area,    &
                                 area_lo, &
                                 area_hi, &
                                 ncell,   &
                                 time,    &
                                 dx,      &
                                 dt)
            use gtm_precision
            implicit none
            integer, intent(in) :: ncell                   !< Number of cells
            real(gtm_real), intent(in)  :: time            !< Time of request
            real(gtm_real), intent(in)  :: dx(ncell)       !< Spatial step 
            real(gtm_real), intent(in)  :: dt              !< Time step 
            real(gtm_real), intent(out) :: flow(ncell)     !< Cell and time centered flow
            real(gtm_real), intent(out) :: flow_lo(ncell)  !< Lo face flow, time centered
            real(gtm_real), intent(out) :: flow_hi(ncell)  !< Hi face flow, time centered
            real(gtm_real), intent(out) :: area(ncell)     !< Cell center area, old time
            real(gtm_real), intent(out) :: area_lo(ncell)  !< Area lo face, time centered
            real(gtm_real), intent(out) :: area_hi(ncell)  !< Area hi face, time centered        
        end subroutine
        
      end interface
      
      !> This pointer should be set by the driver or client code to specify the 
      !> treatment at the boundaries
      procedure(hydro_data_if), pointer :: fill_hydro_data  => null()
      
      !> Generic interface for fetching hydrodynamic data 
      !> with channel top water width and hydraulic radius
      abstract interface
        !> Fill in hydrodynamic data.
        !> This data might be calculated from a function or provided by another module
        !> Note that continuity must be satisfied between time steps. The implementation
        !> must be provided by the driver or application 
        subroutine hydro_info_if(flow,         &
                                 flow_lo,      &
                                 flow_hi,      &
                                 area,         &
                                 area_lo,      &
                                 area_hi,      &
                                 width,        &
                                 hydro_radius, &
                                 depth,        &
                                 ncell,        &
                                 time,         &
                                 dx,           &
                                 dt)
            use gtm_precision
            implicit none
            integer, intent(in) :: ncell                       !< Number of cells
            real(gtm_real), intent(in)  :: time                !< Time of request
            real(gtm_real), intent(in)  :: dx(ncell)           !< Spatial step 
            real(gtm_real), intent(in)  :: dt                  !< Time step 
            real(gtm_real), intent(out) :: flow(ncell)         !< Cell and time centered flow
            real(gtm_real), intent(out) :: flow_lo(ncell)      !< Lo face flow, time centered
            real(gtm_real), intent(out) :: flow_hi(ncell)      !< Hi face flow, time centered
            real(gtm_real), intent(out) :: area(ncell)         !< Cell center area, old time
            real(gtm_real), intent(out) :: area_lo(ncell)      !< Area lo face, time centered
            real(gtm_real), intent(out) :: area_hi(ncell)      !< Area hi face, time centered     
            real(gtm_real), intent(out) :: width(ncell)        !< Channel top water width
            real(gtm_real), intent(out) :: hydro_radius(ncell) !< Hydraulic radius
            real(gtm_real), intent(out) :: depth(ncell)        !< water depth
        end subroutine
        
      end interface
      
      !> This pointer should be set by the driver or client code to specify the 
      !> treatment at the boundaries
      procedure(hydro_info_if), pointer :: fill_hydro_info  => null()

      
end module