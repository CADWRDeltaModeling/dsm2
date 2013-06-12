

module resample_for_test
   use common_variables
   use gtm_precision  
   use error_handling
   use hdf5
   integer(HID_T) :: file_id            !< HDF5 File identifier
   integer(HID_T) :: hydro_id           !< hydro group identifier      
   integer(HID_T) :: access_plist       !< HDF5 property identifier    
   contains
      
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