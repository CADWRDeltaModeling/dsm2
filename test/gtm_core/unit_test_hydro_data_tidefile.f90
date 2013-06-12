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

!> Routines to test hydro_util and hydro_data_tidefile subroutines
!> To test those routines, make sure test_hydro.h5 and test_hydro_fine.h5 are under /testdata folder.
!>@ingroup test_gtm_core
module ut_hydro_data_tide

    use fruit
    use gtm_precision
    use error_handling
    contains
   
   !> test to read hydro hdf5 flie by spot checking data in test_hydro.h5
    subroutine test_hdf_util()
        use hdf_util
        use common_variables
        use common_xsect
        implicit none
        character(len=*), parameter :: h5_file_name = 'test_hydro.h5'   !< hydro tidefile for testing
        real(gtm_real) :: area_from_CxArea
        integer :: time_offset, time_buffer
        
        ! test loading hdf5 file
        call hdf5_init(h5_file_name)   
       
        ! test reading attributes from tidefile
        call get_hydro_attr()          
        call assertEquals (dble(n_chan), dble(7), weakest_eps, "problem in reading number of channels")
        call assertEquals (dble(n_xsect), dble(1114), weakest_eps, "problem in reading number of virtual xsects")
        call assertEquals (dble(n_comp), dble(26), weakest_eps, "problem in reading number of computational points")      
        call assertEquals (dble(n_time), dble(8641), weakest_eps, "problem in reading number of time entries")
        call assertEquals (hydro_start_julmin, 51546240, "problem in reading starting time in tidefile")
        call assertEquals (hydro_end_julmin, 51675840, "problem in reading end time in tidefile")
        call assertEquals (hydro_time_interval, 15, "problem in reading time interval in tidefile")
        
        ! test reading channel table
        call read_channel_tbl()        
        call assertEquals (dble(chan_geom(2)%chan_no), dble(2), weakest_eps, "problem in reading channel index number")
        call assertEquals (dble(chan_geom(2)%channel_num), dble(387), weakest_eps, "problem in reading real channel number")
        call assertEquals (dble(chan_geom(2)%channel_length), dble(10000), weakest_eps, "problem in reading channel length")
       
        ! test reading computational info
        call read_comp_tbl()           
        call assertEquals (dble(comp_pt(15)%comp_index), dble(15), weakest_eps, "problem in reading comp_index")
        call assertEquals (dble(comp_pt(15)%chan_no), dble(4), weakest_eps, "problem in reading channel_no for computational pt")
        call assertEquals (dble(comp_pt(15)%distance), dble(10000), weakest_eps, "problem in reading distance for computational pt")
       
        ! test reading xsect table and construct its type
        call read_xsect_tbl()          
        call assertEquals (dble(virt_xsect(23)%chan_no), dble(4), weakest_eps, "problem in reading chan_no for virt_xsect")
        call assertEquals (dble(virt_xsect(23)%num_virt_sec), dble(5), weakest_eps, "problem in reading num_virt_sec for virt_xsect")
        call assertEquals (dble(virt_xsect(23)%vsecno), dble(2), weakest_eps, "problem in reading vsecno for virt_xsect")
        call assertEquals (dble(virt_xsect(23)%num_elev), dble(23), weakest_eps, "problem in reading num_elev for virt_xsect")
        call assertEquals (dble(virt_xsect(23)%min_elev), dble(-17.389), weakest_eps, "problem in reading min_elev for virt_xsect")
        call assertEquals (dble(virt_xsect(23)%elevation(7)), dble(-15.539), weakest_eps, "problem in reading elevation for virt_xsect")
        call assertEquals (dble(virt_xsect(23)%area(7)), dble(307.13649), weakest_eps, "problem in reading area for virt_xsect")
        call assertEquals (dble(virt_xsect(23)%wet_p(7)), dble(298.1), weakest_eps, "problem in reading wet_p for virt_xsect")      
        call assertEquals (dble(virt_xsect(23)%width(7)), dble(298.0), weakest_eps, "problem in reading width for virt_xsect")

        ! test assign segment
        call assign_segment()
        call assertEquals (dble(n_segm), dble(19), weakest_eps, "problem in assigning segment for n_segm")        
        call assertEquals (dble(segm(4)%segm_no), dble(4), weakest_eps, "problem in assigning segment for segm_no")    
        call assertEquals (dble(segm(4)%chan_no), dble(2), weakest_eps, "problem in assigning segment for chan_no")    
        call assertEquals (dble(segm(4)%up_comppt), dble(5), weakest_eps, "problem in assigning segment for up_comppt")  
        call assertEquals (dble(segm(4)%down_comppt), dble(6), weakest_eps, "problem in assigning segment for down_comppt")  
        call assertEquals (dble(segm(4)%length), dble(5000), weakest_eps, "problem in assigning segment for length")  
       
        ! test reading time series data
        call allocate_hydro_ts()
        time_offset = 3
        time_buffer = 10
        call get_ts_from_hdf5(hydro_flow, "flow", time_offset, time_buffer)
        call get_ts_from_hdf5(hydro_ws, "water surface", time_offset, time_buffer)
        call assertEquals (dble(hydro_flow(6,1)), dble(225612.1), weakest_eps, "problem in reading flow")
        call assertEquals (dble(hydro_ws(6,1)), dble(20.848833), weakest_eps, "problem in reading water surface")
        time_offset = 10
        time_buffer = 3
        call get_ts_from_hdf5(hydro_flow, "flow", time_offset, time_buffer)
        call get_ts_from_hdf5(hydro_ws, "water surface", time_offset, time_buffer)
        call assertEquals (dble(hydro_flow(6,1)), dble(27650.672), weakest_eps, "problem in reading flow")
        call assertEquals (dble(hydro_ws(6,1)), dble(1.9490309), weakest_eps, "problem in reading water surface")
       
        ! test CxArea calculation
        call CxArea(area_from_CxArea, dble(5000),hydro_ws(6,1),2)                   
        call assertEquals (area_from_CxArea, dble(7524.127), weakest_eps, "problem in calculating CxArea")
        
        call deallocate_hydro_ts() 
        call hdf5_close()         
        return
    end subroutine
               
end module   
