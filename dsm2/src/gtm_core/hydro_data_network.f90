!<license>
!    Copyright (C) 2017 State of California,
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

!> Hydrodynamics interface for reservoirs, external flows and tranfer flows to be fulfilled by driver or application
!>@ingroup gtm_core
module hydro_data_network
    !> Generic interface for fetching hydrodynamic data of reservoirs and external flows
    interface
        !> Fill in hydrodynamic data.
        !> This data might be calculated from a function or provided by another module
        !> Note that continuity must be satisfied between time steps. The implementation
        !> must be provided by the driver or application
        subroutine hydro_data_network_if(resv_height, &
                                         resv_flow,   &
                                         qext,        &
                                         transf,      &
                                         nresv,       &
                                         nresv_conn,  &
                                         nqext,       &
                                         ntran,       &
                                         time_index)
            use constants
            implicit none
            integer, intent(in) :: nresv                         !< Number of reservoirs
            integer, intent(in) :: nresv_conn                    !< Number of reservoir connections
            integer, intent(in) :: nqext                         !< Number of external flows
            integer, intent(in) :: ntran                         !< Number of transfer flows
            real(gtm_real), intent(in)  :: time_index            !< Time index
            real(gtm_real), intent(out) :: resv_height(nresv)    !< reservoir height
            real(gtm_real), intent(out) :: resv_flow(nresv_conn) !< reservoir flow
            real(gtm_real), intent(out) :: qext(nqext)           !< external flow
            real(gtm_real), intent(out) :: transf(ntran)         !< transfer flow
        end subroutine

    end interface

    !> This pointer should be set by the driver or client code to specify the
    !> treatment at the boundaries
    procedure(hydro_data_network_if), pointer :: fill_hydro_network  => null()

end module