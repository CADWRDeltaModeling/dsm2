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
        call assertEquals (orig_start_julmin, 51546240, "problem in reading starting time in tidefile")
        call assertEquals (orig_end_julmin, 51675840, "problem in reading end time in tidefile")
        call assertEquals (orig_time_interval, 15, "problem in reading time interval in tidefile")
        
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
 
   !> test to resample coarse grid from a finer hydro run (for testing comparison only)
   !> This is hard-coded for testing only and it is an example to resampling 
   !> 5min-1250ft hydro run to a 15min-5000ft scale.
   !> This requires file test_hydro_fine.h5 and will overwrite 
   !> hydro_flow, hydro_area, hydro_ws and hydro_avga.
    subroutine test_resample()
        use hdf_util
        use common_variables
        use common_xsect
        use interpolation   
        implicit none
        character(len=*), parameter :: hdf5file_fine = "test_hydro_fine.h5" !< finer grid tidefile for testing
        integer, parameter :: n_comp_fine = 83                              !< number of computational pts in fine grid
        integer, parameter :: n_time_fine = 25921                           !< number of time in fine grid
        real(gtm_real), dimension(n_comp_fine,n_time_fine) :: fine_flow     ! Data arrays
        real(gtm_real), dimension(n_comp_fine,n_time_fine) :: fine_area     ! Data arrays
        real(gtm_real), dimension(n_comp_fine,n_time_fine) :: fine_ws       ! Data arrays
        real(gtm_real), dimension(n_comp_fine,n_time_fine) :: fine_avga     ! Data arrays        
        real(gtm_real), dimension(4,5) :: block_flow, block_area, block_ws, block_avga, cx  ! finer grid block for testing
        real(gtm_real), dimension(4-1,5-1) :: volume_change_flow, volume_change_area
        real(gtm_real), dimension(4-1,5-1) :: volume_change_ws, volume_change_avga  ! finer grid block for testing
        real(gtm_real), dimension(4-1,5-1) :: volume_change_theta_flow        
        real(gtm_real) :: time, dx, dt
        real(gtm_real) :: cal_area_up, cal_area_down
        real(gtm_real) :: ws_up, ws_down, ws, X
        integer :: i, j, t
        
        dt = 5
        dx = 1250        
        
       ! n_comp and n_time from test_hdf_util() will be used to allocate array (n_comp=26,n_time=8641) 
        call allocate_hydro_ts() 
        
       ! resample time series data from a finer grid tidefile, for testing only
       ! if hdf5_init() were called, make sure it is closed before calling resample().
        call resample(hdf5file_fine,                                   &
                      fine_flow, fine_area, fine_ws, fine_avga,        & 
                      hydro_flow, hydro_area, hydro_ws, hydro_avga)               
        call assertEquals (hydro_flow(2,1),24455.1D0,weakest_eps,"problem in resampling hydro flow")
        call assertEquals (hydro_area(3,5230),8343.032D0,weakest_eps,"problem in resampling hydro area")
        call assertEquals (hydro_area(6,8001), 7771.1133D0, weakest_eps, "problem in obtaining hydro area")
        call assertEquals (hydro_area(7,8001), 6209.562D0, weakest_eps, "problem in obtaining hydro area")
       
       ! extract a block from finer grid and print that to debug_unit for comparison 
        call extract_block(83, 25921,                                  &
                           24001, 24004, 18, 22,                       &
                           fine_flow, fine_area, fine_ws, fine_avga,   &
                           block_flow, block_area, block_ws, block_avga)              
        do i = 1, 4
            do j = 1, 5
                X = 5000.0 + (j-1)*dx
                call CxArea(cx(i,j), X, block_ws(i,j),2)
            end do
        end do        
        do i = 1, 4-1
            do j = 1, 5-1  
                volume_change_flow(i,j) = (block_flow(i+1,j)-block_flow(i+1,j+1))*dt*60
                volume_change_area(i,j) = 0.5*(block_area(i+1,j)+block_area(i+1,j+1))*dx -  &
                                          0.5*(block_area(i,j)+block_area(i,j+1))*dx
                volume_change_avga(i,j) = block_avga(i+1,j)*dx - block_avga(i,j)*dx
                volume_change_ws(i,j) =  0.5*(cx(i+1,j)+cx(i+1,j+1))*dx -  &
                                         0.5*(cx(i,j)+cx(i,j+1))*dx
                volume_change_theta_flow(i,j) = (0.4*block_flow(i,j)+0.6*block_flow(i+1,j)-0.4*block_flow(i,j+1)-0.6*block_flow(i+1,j+1))*dt*60
            end do
        end do        
        write(debug_unit,'(a90)') "Flow output from 5min 1250ft DSM2 hydro run (mass balance calculated by (Q(t-1)-Q(t))*dt):"   
        call print_mass_balance_check(debug_unit, 4, 5, block_flow, volume_change_flow)  
        write(debug_unit,'(/a90)') "Flow output from 5min 1250ft DSM2 hydro run: (mass balance calculated by theta average):  "   
        call print_mass_balance_check(debug_unit, 4, 5, block_flow, volume_change_theta_flow)     
        write(debug_unit,'(/a90)') "Area output from 5min 1250ft DSM2 hydro run:                                              "   
        call print_mass_balance_check(debug_unit, 4, 5, block_area, volume_change_area)  
        write(debug_unit,'(/a90)') "Area calculated from elevation output from 5min 1250ft DSM2 hydro run:                    "   
        call print_mass_balance_check(debug_unit, 4, 5, cx, volume_change_ws)
        write(debug_unit,'(/a90)') "Average Area output from 5min 1250ft DSM2 hydro run:                                      "  
        call print_mass_balance_check(debug_unit, 4, 5, block_avga, volume_change_avga)                                 
        return
    end subroutine
   
   !> Resample time series from finer scale tide file - for testing purpose only 
   !> 5min-->15min, 1250ft-->5000ft, assume testing channel length increment 5000ft for uniform sampling.
   !> This required providing another tidefile output with finer grid.
   subroutine resample(hdf5file_name_fine,                               & 
                       fine_flow, fine_area, fine_ws, fine_avga,         &
                       coarse_flow, coarse_area, coarse_ws, coarse_avga)  
       use hdf5 
       implicit none            
       integer(HID_T) :: file_id_tmp                ! HDF5 File identifier
       integer(HID_T) :: hydro_id_tmp               ! Group identifier
       integer(HID_T) :: data_id                    ! Group identifier
       integer(HID_T) :: dset_id                    ! Dataset identifier
       integer(HSIZE_T), dimension(2) :: data_dims  ! Datasets dimensions
       integer :: error                             ! Error flag
       integer, parameter :: n_comp_fine = 83
       integer, parameter :: n_time_fine = 25921
       integer, parameter :: n_comp_coarse = 26
       integer, parameter :: n_time_coarse = 8641
       character(len=*), intent(in) :: hdf5file_name_fine                                   !< finer grid tidefile from hydro run
       real(gtm_real), dimension(n_comp_coarse,n_time_coarse), intent(out) :: coarse_flow   ! Data arrays
       real(gtm_real), dimension(n_comp_coarse,n_time_coarse), intent(out) :: coarse_area   ! Data arrays
       real(gtm_real), dimension(n_comp_coarse,n_time_coarse), intent(out) :: coarse_ws     ! Data arrays
       real(gtm_real), dimension(n_comp_coarse,n_time_coarse), intent(out) :: coarse_avga   ! Data arrays     
       real(gtm_real), dimension(n_comp_fine,n_time_fine), intent(out) :: fine_flow         ! Data arrays
       real(gtm_real), dimension(n_comp_fine,n_time_fine), intent(out) :: fine_area         ! Data arrays
       real(gtm_real), dimension(n_comp_fine,n_time_fine), intent(out) :: fine_ws           ! Data arrays
       real(gtm_real), dimension(n_comp_fine,n_time_fine), intent(out) :: fine_avga         ! Data arrays
       integer :: i, j, ti
       integer, dimension(n_comp_coarse):: ci
       
       ! hardcode the corresponding index ci for coarse grid, not good but may be acceptable for one time testing.
       ci = (/1,5,9,13,14,18,22,23,27,31,35,39,40,44,48,49,53,57,58,62,66,70,71,75,79,83/) 
       data_dims(1) = n_comp_fine
       data_dims(2) = n_time_fine     
       call h5open_f(error)
       call verify_error(error, "opening hdf interface")
       call h5fopen_f ("test_hydro_fine.h5", H5F_ACC_RDONLY_F, file_id_tmp, error)
       call verify_error(error, "opening tidefile")
       call h5gopen_f(file_id_tmp, "hydro", hydro_id_tmp, error)
       call h5gopen_f(hydro_id_tmp, "data", data_id, error)
       call h5dopen_f(data_id, "flow", dset_id, error) 
       call h5dread_f(dset_id, H5T_NATIVE_DOUBLE, fine_flow, data_dims, error)
       call h5dopen_f(data_id, "area", dset_id, error) 
       call h5dread_f(dset_id, H5T_NATIVE_DOUBLE, fine_area, data_dims, error)
       call h5dopen_f(data_id, "water surface", dset_id, error) 
       call h5dread_f(dset_id, H5T_NATIVE_DOUBLE, fine_ws, data_dims, error)       
       call h5dopen_f(data_id, "avg area", dset_id, error) 
       call h5dread_f(dset_id, H5T_NATIVE_DOUBLE, fine_avga, data_dims, error)             
       call h5dclose_f(dset_id, error)   
       call h5gclose_f(data_id, error)
       call h5gclose_f(hydro_id_tmp, error)
       call h5fclose_f(file_id_tmp, error)                    
       call h5close_f(error)     
       do i = 1, n_comp_coarse
           do j = 1, n_time_coarse
               ti = (j-1)*3 + 1
               coarse_flow(i,j) = fine_flow(ci(i), ti)  
               coarse_area(i,j) = fine_area(ci(i), ti)   
               coarse_ws(i,j) = fine_ws(ci(i), ti)   
               coarse_avga(i,j) = fine_avga(ci(i), ti)         
           end do
       end do      
       return
   end subroutine
   
   !> extract a selected block from fine grid
   subroutine extract_block(n_comp_fine, n_time_fine,                         &       !< dimensions for finer grid
                            qa_start_t, qa_end_t, qa_start_col, qa_end_col,   &       !< QA/QC block specification (intent(in))                  
                            fine_flow, fine_area, fine_ws, fine_avga,         &       !< finer grid data
                            block_flow, block_area, block_ws, block_avga)             !< QA/QC block output
       implicit none                    
       integer, intent(in) :: n_comp_fine, n_time_fine                                                        !< dimension of finer grid
       integer, intent(in) :: qa_start_t, qa_end_t                                                            !< QA/QC start/end time row index (index from 1)
       integer, intent(in) :: qa_start_col, qa_end_col                                                        !< QA/QC start/end column (index from 1)
       real(gtm_real), dimension(n_comp_fine,n_time_fine), intent(in) :: fine_flow                            !< flow finer grid 
       real(gtm_real), dimension(n_comp_fine,n_time_fine), intent(in) :: fine_area                            !< area finer grid
       real(gtm_real), dimension(n_comp_fine,n_time_fine), intent(in) :: fine_ws                              !< water surface finer grid
       real(gtm_real), dimension(n_comp_fine,n_time_fine), intent(in) :: fine_avga                            !< average area finer grid
       real(gtm_real), dimension(qa_end_t-qa_start_t+1, qa_end_col-qa_start_col+1), intent(out) :: block_flow !< selected flow block from finer grid 
       real(gtm_real), dimension(qa_end_t-qa_start_t+1, qa_end_col-qa_start_col+1), intent(out) :: block_area !< selected area block from finer grid  
       real(gtm_real), dimension(qa_end_t-qa_start_t+1, qa_end_col-qa_start_col+1), intent(out) :: block_ws   !< selected ws block from finer grid 
       real(gtm_real), dimension(qa_end_t-qa_start_t+1, qa_end_col-qa_start_col+1), intent(out) :: block_avga !< selected avga block from finer grid      
       integer :: i, j                                                                                        ! local variables
       do i = 1, qa_end_t-qa_start_t+1
           do j = 1, qa_end_col-qa_start_col+1
               block_flow(i,j) = fine_flow(qa_start_col+j-1,qa_start_t+i-1)
               block_area(i,j) = fine_area(qa_start_col+j-1,qa_start_t+i-1)
               block_ws(i,j) = fine_ws(qa_start_col+j-1,qa_start_t+i-1)
               block_avga(i,j) = fine_avga(qa_start_col+j-1,qa_start_t+i-1)
           end do
       enddo       
       return
   end subroutine
               
end module   
