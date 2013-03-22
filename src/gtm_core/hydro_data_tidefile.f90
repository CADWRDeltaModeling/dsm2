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

!> Routines to obtain hydro data from hydro tidefile
!>@ingroup gtm_core
module hydro_data_tidefile

    use gtm_precision
    use error_handling
    use logging
    use common_variables
    use common_xsect       
    use hdf_util   
    
    contains   
   
   !> This subroutine is used to read DSM2 hydro tidefile,  
   !> including attributes, geometry and time series. 
    subroutine dsm2_hdf_data(hdf5file)
        implicit none              
        character(len=*), intent(in) :: hdf5file        !< DSM2 hydro tidefile name
        !procedure(hydro_data_if), pointer :: hdf_flow     
        call hdf5_init(hdf5file)
        call get_hydro_attr()
        call read_channel_tbl()
        call read_comp_tbl()
        call read_xsect_tbl() 
        call assign_segment()           
        call allocate_hydro_ts()
        call get_ts_from_hdf5("flow", hydro_flow)
        !call get_ts_from_hdf5("area", hydro_area)        ! todo::if we decide to go with elevation, this can be removed.
        call get_ts_from_hdf5("water surface", hydro_ws)
        call get_ts_from_hdf5("avg area", hydro_avga)    ! todo::if we decide to go with elevation, this can be removed.
        call hdf5_close()
        return  
    end subroutine      
    
end module     

