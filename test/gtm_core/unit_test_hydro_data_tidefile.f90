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
        use hdf5
        use hdf_util
        use common_variables
        use common_xsect
        implicit none
        character(len=*), parameter :: h5_file_name = 'historical.h5'   !< hydro tidefile for testing
        real(gtm_real) :: area_from_CxArea
        integer :: time_offset, time_buffer
        integer :: branch, nt, nx
        real(gtm_real) :: ws_a, ws_b, ws_c, ws_d, dt, up_x, dx, x, ws, area
        integer :: err
        
        call h5open_f(err)
        
        ! test loading hdf5 file
        call hdf5_init(h5_file_name)   
       
        ! test reading attributes from tidefile
        call get_hydro_attr          
        call assertEquals (dble(n_chan), dble(517), weakest_eps, "problem in reading number of channels")
        call assertEquals (dble(n_xsect), dble(53613), weakest_eps, "problem in reading number of virtual xsects")
        call assertEquals (dble(n_comp), dble(1210), weakest_eps, "problem in reading number of computational points")    
        call assertEquals (dble(n_resv), dble(5), weakest_eps, "problem in reading number of reservoirs")  
        call assertEquals (dble(n_resv_conn), dble(14), weakest_eps, "problem in reading number of reservoir connections")    
        call assertEquals (hydro_start_jmin, 51412320, "problem in reading starting time in tidefile")
        call assertEquals (hydro_end_jmin, 51544800, "problem in reading end time in tidefile")
        call assertEquals (hydro_time_interval, 15, "problem in reading time interval in tidefile")
        
        ! test reading channel table
        call read_channel_tbl
        call assertEquals (dble(chan_geom(120)%chan_no), dble(120), weakest_eps, "problem in reading channel index number")
        call assertEquals (dble(chan_geom(120)%channel_num), dble(131), weakest_eps, "problem in reading real channel number")
        call assertEquals (dble(chan_geom(120)%channel_length), dble(5640), weakest_eps, "problem in reading channel length")

        ! test reading reservoir table
        call read_reservoir_tbl
        call assertEquals (resv_geom(2)%name, "clifton_court", "problem in reading reservoir name")
        call assertEquals (dble(resv_geom(2)%area), dble(91.868), weakest_eps, "problem in reading reservoir area")
        call assertEquals (dble(resv_geom(2)%bot_elev), dble(-10.1), weakest_eps, "problem in reading reservoir bot_elev")
        call assertEquals (dble(resv_geom(2)%is_gated(1)), dble(1), weakest_eps, "problem in reading reservoir connection type")
        call assertEquals (dble(resv_geom(3)%is_gated(1)), dble(0), weakest_eps, "problem in reading reservoir connection type")
        call assertEquals (dble(resv_geom(3)%n_resv_conn), dble(2), weakest_eps, "problem in reading reservoir connection number")
        call assertEquals (dble(resv_geom(3)%int_node_no(1)), dble(183), weakest_eps, "problem in reading reservoir connection internal node number")
        call assertEquals (dble(resv_geom(3)%ext_node_no(1)), dble(197), weakest_eps, "problem in reading reservoir connection external node number")

        ! test reading external flows table
        call read_qext_tbl
        call assertEquals (trim(qext(2)%name), "cosumnes", "problem in reading external flow name")
        call assertEquals (dble(qext(2)%attach_obj_type),  dble(2), "problem in reading external flow type")
        call assertEquals (dble(qext(2)%attach_obj_no),  dble(425), "problem in reading external flow connect number")

        call read_tran_tbl
        
        ! test reading computational info
        call read_comp_tbl        
        call assertEquals (dble(comp_pt(15)%comp_index), dble(15), weakest_eps, "problem in reading comp_index")
        call assertEquals (dble(comp_pt(15)%chan_no), dble(5), weakest_eps, "problem in reading channel_no for computational pt")
        call assertEquals (dble(comp_pt(15)%distance), dble(6175), weakest_eps, "problem in reading distance for computational pt")
        call assign_chan_comppt
        
        ! test reading xsect table and construct its type
        call read_xsect_tbl          
        call assertEquals (dble(virt_xsect(23)%chan_no), dble(5), weakest_eps, "problem in reading chan_no for virt_xsect")
        call assertEquals (dble(virt_xsect(23)%num_virt_sec), dble(5), weakest_eps, "problem in reading num_virt_sec for virt_xsect")
        call assertEquals (dble(virt_xsect(23)%vsecno), dble(1), weakest_eps, "problem in reading vsecno for virt_xsect")
        call assertEquals (dble(virt_xsect(23)%num_elev), dble(18), weakest_eps, "problem in reading num_elev for virt_xsect")
        call assertEquals (dble(virt_xsect(23)%min_elev), dble(-7.589000), weakest_eps, "problem in reading min_elev for virt_xsect")
        call assertEquals (dble(virt_xsect(23)%elevation(7)), dble(-1.489000), weakest_eps, "problem in reading elevation for virt_xsect")
        call assertEquals (dble(virt_xsect(23)%area(7)), dble(1269.827), weakest_eps, "problem in reading area for virt_xsect")
        call assertEquals (dble(virt_xsect(23)%wet_p(7)), dble(265.8740), weakest_eps, "problem in reading wet_p for virt_xsect")      
        call assertEquals (dble(virt_xsect(23)%width(7)), dble(264.6632), weakest_eps, "problem in reading width for virt_xsect")

        ! test assign segment
        call assign_segment
        call assertEquals (dble(n_segm), dble(693), weakest_eps, "problem in assigning segment for n_segm")        
        call assertEquals (dble(segm(4)%segm_no), dble(4), weakest_eps, "problem in assigning segment for segm_no")    
        call assertEquals (dble(segm(4)%chan_no), dble(2), weakest_eps, "problem in assigning segment for chan_no")    
        call assertEquals (dble(segm(4)%up_comppt), dble(5), weakest_eps, "problem in assigning segment for up_comppt")  
        call assertEquals (dble(segm(4)%down_comppt), dble(6), weakest_eps, "problem in assigning segment for down_comppt")  
        call assertEquals (dble(segm(4)%length), dble(7000), weakest_eps, "problem in assigning segment for length")  
       
        call get_dsm2_network_info
        call assertEquals (dble(n_node), dble(430), weakest_eps, "problem in assigning segment for n_node") 
        call assertEquals (dble(dsm2_network(210)%dsm2_node_no), dble(218), weakest_eps, "problem in assigning segment for dsm2_network(210)%dsm2_node_no") 
        call assertEquals (dble(dsm2_network(210)%n_conn_cell), dble(4), weakest_eps, "problem in assigning segment for dsm2_network(210)%n_conn_cell") 
        call assertEquals (dble(dsm2_network(221)%dsm2_node_no), dble(232), weakest_eps, "problem in assigning segment for dsm2_network(221)%dsm2_node_no") 
        call assertEquals (dble(dsm2_network(221)%reservoir_no), dble(4), weakest_eps, "problem in assigning segment for dsm2_network(221)%reservoir_no") 
        call assertEquals (dble(dsm2_network(17)%dsm2_node_no), dble(17), weakest_eps, "problem in assigning segment for dsm2_network(17)%dsm2_node_no") 
        call assertEquals (dble(dsm2_network(17)%boundary_no), dble(1), weakest_eps, "problem in assigning segment for dsm2_network(17)%boundary_no") 
        call assertEquals (dble(dsm2_network(17)%cell_no(1)), dble(125), weakest_eps, "problem in assigning segment for dsm2_network(17)%cell_no") 
        call assertEquals (dble(dsm2_network(17)%up_down(1)), dble(1), weakest_eps, "problem in assigning segment for dsm2_network(17)%up_down") 
        call assertEquals (dble(dsm2_network(344)%dsm2_node_no), dble(361), weakest_eps, "problem in assigning segment for dsm2_network(344)%dsm2_node_no") 
        call assertEquals (dble(dsm2_network(344)%boundary_no), dble(21), weakest_eps, "problem in assigning segment for dsm2_network(344)%boundary_no") 
        call assertEquals (dble(dsm2_network(344)%cell_no(1)), dble(2169), weakest_eps, "problem in assigning segment for dsm2_network(344)%cell_no") 
        call assertEquals (dble(dsm2_network(344)%up_down(1)), dble(0), weakest_eps, "problem in assigning segment for dsm2_network(344)%up_down") 
               
        ! test reading time series data
        call allocate_hydro_ts
        time_offset = 3
        time_buffer = 10
        call get_ts_from_hdf5(hydro_flow, "flow", n_comp, time_offset, time_buffer)
        call get_ts_from_hdf5(hydro_ws, "water surface", n_comp, time_offset, time_buffer)
        call get_ts_from_hdf5(hydro_resv_height, "reservoir height", n_resv, time_offset, time_buffer)
        call get_ts_from_hdf5(hydro_resv_flow, "reservoir flow", n_resv_conn, time_offset, time_buffer)
        call get_ts_from_hdf5(hydro_qext_flow, "qext flow", n_qext, time_offset, time_buffer)
        call assertEquals (dble(hydro_flow(6,1)), dble(890.294067382812), weakest_eps, "problem in reading flow")
        call assertEquals (dble(hydro_ws(6,1)), dble(5.62393283843994), weakest_eps, "problem in reading water surface")
        time_offset = 10
        time_buffer = 3
        call get_ts_from_hdf5(hydro_flow, "flow", n_comp, time_offset, time_buffer)
        call get_ts_from_hdf5(hydro_ws, "water surface", n_comp, time_offset, time_buffer)
        call get_ts_from_hdf5(hydro_resv_height, "reservoir height", n_resv, time_offset, time_buffer)
        call get_ts_from_hdf5(hydro_resv_flow, "reservoir flow", n_resv_conn, time_offset, time_buffer)
        call get_ts_from_hdf5(hydro_qext_flow, "qext flow", n_qext, time_offset, time_buffer)        
        call assertEquals (dble(hydro_flow(6,1)), dble(1231.77636718750), weakest_eps, "problem in reading flow")
        call assertEquals (dble(hydro_ws(6,1)), dble(5.14460611343384), weakest_eps, "problem in reading water surface")
        call assertEquals (dble(hydro_resv_height(3,2)), dble(1.9372283220291), weakest_eps, "problem in reading hydro_resv_height(3,2)")
        call assertEquals (dble(hydro_resv_flow(3,2)), dble(-640.6436), weakest_eps, "problem in reading hydro_resv_flow(3,2)")
        call assertEquals (dble(hydro_qext_flow(6,2)), dble(121.99999), weakest_eps, "problem in reading hydro_qext(5,2)")
               
        ! test CxArea calculation
        call CxArea(area_from_CxArea, dble(5000),hydro_ws(6,1),2)                   
        call assertEquals (area_from_CxArea, dble(735.497178324158), weakest_eps, "problem in calculating CxArea")

        branch = 2
        nt = 4
        nx = 5
        dt = five
        up_x = dble(5000.)
        dx = dble(1250.)
        ws_a = dble(2.355571)
        ws_b = dble(1.307376)
        ws_c = dble(2.302306)
        ws_d = dble(1.252751)
        x = dble(5000) + two*dx
        ws = ws_a + two/(dble(nx)-one)*(ws_b-ws_a)
        call CxArea(area, x, ws, branch)           
        x = dble(5000) + three*dx
        ws = (ws_a+(ws_c-ws_a)*two/(dble(nt)-one))+((ws_b+(ws_d-ws_b)*two/(dble(nt)-one))-(ws_a+(ws_c-ws_a)*two/(dble(nt)-one)))*three/(dble(nx)-one) 
        call CxArea(area, x, ws, branch)              
        call deallocate_hydro_ts 
        call hdf5_close         
        return
    end subroutine
               
end module   
