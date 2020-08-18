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

!> Routines to obtain hydro data from hydro tidefile
!>@ingroup gtm_core
module hydro_data_tidefile

    use gtm_precision
    use error_handling
    use gtm_logging
    use common_variables
    use common_xsect       
    use hdf_util   
 
    contains  
   
   !> This subroutine is used to read DSM2 hydro tidefile,  
   !> including attributes and geometry. 
    subroutine dsm2_hdf_geom()
        implicit none                      
        call get_hydro_attr
        call read_channel_tbl
        call find_non_sequential
        call read_comp_tbl
        call read_channel_bottom_tbl        
        call assign_chan_comppt
        call assign_segment   
        call read_reservoir_tbl
        call read_qext_tbl
        call read_tran_tbl
        call read_xsect_tbl    
        call read_gate_tbl
        call read_source_flow_tbl
        call read_boundary_tbl
        call get_dsm2_network_info
    end subroutine     
    
    !> This subroutine is used to read DSM2 hydro tidefile,
    !> mainly time series.
    subroutine dsm2_hdf_ts(time_offset, time_buffer)
        implicit none              
        integer, intent(in) :: time_offset
        integer, intent(in) :: time_buffer
        call get_ts_from_hdf5(hydro_flow, "inst flow", n_comp, time_offset, time_buffer)
        call get_ts_from_hdf5(hydro_ws, "inst water surface", n_comp, time_offset, time_buffer)
        if (n_resv_conn > 0) call get_ts_from_hdf5(hydro_resv_flow, "inst reservoir flow", n_resv_conn, time_offset, time_buffer)
        if (n_resv > 0) call get_ts_from_hdf5(hydro_resv_height, "reservoir height", n_resv, time_offset, time_buffer)
        if (n_qext > 0) call get_ts_from_hdf5(hydro_qext_flow, "inst qext flow", n_qext, time_offset, time_buffer)
        if (n_tran > 0) call get_ts_from_hdf5(hydro_tran_flow, "inst transfer flow", n_tran, time_offset, time_buffer)
        return  
    end subroutine      
    
    !> This subroutine is used to read one slice of DSM2 hydro tidefile,
    !> mainly time series.
    subroutine dsm2_hdf_slice(flow_arr, ws_arr, resv_arr, resv_conn_arr, qext_arr, tran_arr, num_comp, num_resv, num_resv_conn, num_qext, num_tran, time_offset)
        implicit none
        integer, intent(in) :: num_comp                             !< number of computational points
        integer, intent(in) :: num_resv                             !< number of reservoirs
        integer, intent(in) :: num_resv_conn                        !< number of reservoir connections
        integer, intent(in) :: num_qext                             !< number of external flows
        integer, intent(in) :: num_tran                             !< number of transfer flows
        integer, intent(in) :: time_offset                          !< time offset in hydro tidefile
        real(gtm_real), intent(out) :: flow_arr(num_comp)           !< output array for the slice at time of request
        real(gtm_real), intent(out) :: ws_arr(num_comp)             !< output array for the slice at time of request
        real(gtm_real), intent(out) :: resv_arr(num_resv)           !< output array for the sloce at time of request
        real(gtm_real), intent(out) :: resv_conn_arr(num_resv_conn) !< output array for the sloce at time of request
        real(gtm_real), intent(out) :: qext_arr(num_qext)           !< output array for the sloce at time of request
        real(gtm_real), intent(out) :: tran_arr(num_tran)           !< output array for the sloce at time of request
        if (num_comp>0) call get_ts_from_hdf5(flow_arr,"inst flow", num_comp, time_offset,1)
        if (num_comp>0) call get_ts_from_hdf5(ws_arr,"inst water surface", num_comp, time_offset,1) 
        if (num_resv>0) call get_ts_from_hdf5(resv_arr,"reservoir height", num_resv, time_offset,1)
        if (num_resv_conn>0) call get_ts_from_hdf5(resv_conn_arr,"inst reservoir flow", num_resv_conn, time_offset,1)
        if (num_qext>0) call get_ts_from_hdf5(qext_arr,"inst qext flow", num_qext, time_offset,1)
        if (num_tran>0) call get_ts_from_hdf5(tran_arr,"inst transfer flow", num_tran, time_offset,1)
        return
    end subroutine
         
end module     

