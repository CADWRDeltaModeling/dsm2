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

!> Boundary diffusive flux interface to be fulfilled by driver or application
!>@ingroup gtm
module network_boundary_diffusion

    contains

    !> Diffusion boundary condition that enforces zero constituent flux 
    subroutine network_boundary_diffusive_flux(diffusive_flux_lo, &
                                               diffusive_flux_hi, &
                                               conc,              &
                                               area_lo,           &
                                               area_hi,           &
                                               disp_coef_lo,      &  
                                               disp_coef_hi,      &
                                               ncell,             &
                                               nvar,              &
                                               time,              &
                                               dx,                &
                                               dt)
        use gtm_precision
        use error_handling
        use common_variables, only : n_node, dsm2_network
        use state_variables_network, only : node_conc
        implicit none
        !--- args
        integer, intent(in)  :: ncell                               !< Number of cells
        integer, intent(in)  :: nvar                                !< Number of variables
        real(gtm_real),intent(inout):: diffusive_flux_lo(ncell,nvar)!< Face flux, lo side
        real(gtm_real),intent(inout):: diffusive_flux_hi(ncell,nvar)!< Face flux, hi side
        real(gtm_real),intent(in)   :: area_lo(ncell)               !< Low side area centered at time
        real(gtm_real),intent(in)   :: area_hi(ncell)               !< High side area centered at time
        real(gtm_real),intent(in)   :: time                         !< Time
        real(gtm_real),intent(in)   :: conc(ncell,nvar)             !< Concentration 
        real(gtm_real),intent(in)   :: disp_coef_lo(ncell)          !< Low side constituent dispersion coef.
        real(gtm_real),intent(in)   :: disp_coef_hi(ncell)          !< High side constituent dispersion coef.
        real(gtm_real),intent(in)   :: dt                           !< Spatial step  
        real(gtm_real),intent(in)   :: dx(ncell)                    !< Time step     
        integer :: i, icell
 
        diffusive_flux_lo(1,:) = zero
        diffusive_flux_hi(ncell,:) = zero
        do i = 1, n_node
            if ( (dsm2_network(i)%boundary_no.ne.0) .and. (dsm2_network(i)%node_conc.eq.1) ) then  !if boundary and node concentration is given
                icell = dsm2_network(i)%cell_no(1)
                if (dsm2_network(i)%up_down(1) .eq. 1) then     ! upstream boundary
                    diffusive_flux_lo(icell,:) = zero
                    !diffusive_flux_lo(icell,:) = area_lo(icell)*disp_coef_lo(icell)*(conc(icell,:)-node_conc(i,:))/(half*dx(icell))
                else                                            ! downstream boundary
                    diffusive_flux_hi(icell,:) = zero
                    !diffusive_flux_hi(icell,:) = area_hi(icell)*disp_coef_hi(icell)*(node_conc(i,:)-conc(icell,:))/(half*dx(icell))
                end if    
            end if
        end do 
           
        return
    end subroutine

end module  