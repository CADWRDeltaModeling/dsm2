!<license>
!    Copyright (C) 2013 State of California,
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
        call read_comp_tbl
        call read_xsect_tbl   !todo: this consumes lots of stack memory. Try to reduce.
        call assign_segment   
        call assign_chan_comppt
        call get_dsm2_node_info        
    end subroutine     
    
    !> This subroutine is used to read DSM2 hydro tidefile,
    !> mainly time series.
    subroutine dsm2_hdf_ts(time_offset, time_buffer)
        implicit none              
        integer, intent(in) :: time_offset
        integer, intent(in) :: time_buffer
        call get_ts_from_hdf5(hydro_flow, "flow", time_offset, time_buffer)
        call get_ts_from_hdf5(hydro_ws, "water surface", time_offset, time_buffer)
        call get_ts_from_hdf5(hydro_avga, "avg area", time_offset, time_buffer)    ! todo::if we decide to go with elevation, this can be removed. 
        return  
    end subroutine      
    
end module     

