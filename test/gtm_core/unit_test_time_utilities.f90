
!> Test routines of time functionalities
!>@ingroup test_process_io
module ut_time_utilities

    use fruit
    use gtm_precision
    use error_handling
    contains
    
    !> Routine to test functions in time_utitlities
    subroutine test_time_util()
        use time_utilities
        implicit none
        integer :: jday
        character(len=14) :: cdt
        integer :: num_blocks, remainder
        integer :: hdf_start_jmin, hdf_end_jmin
        integer :: iblock, slice_in_block
        real(gtm_real) :: time_in_slice
        real(gtm_real) :: gtm_start_jmin, gtm_end_jmin        
        real(gtm_real) :: current_time
        integer :: start_hydro_block, memory_buffer, hdf_time_interval
        integer, allocatable :: memlen(:)
        real(gtm_real) :: gtm_time_interval
        
        jday = cdt2jmin('01FEB1998 2400')
        call assertEquals (jday, 51590880, "problem in cdt2jmin function")
        jday = cdt2jmin('01MAR1998 2400')
        call assertEquals (jday, 51631200, "problem in cdt2jmin function")               
        cdt = jmin2cdt(51546240)
        call assertEquals (cdt, '01JAN1998 2400', "problem in jmin2cdt function")  
        
        memory_buffer = 20
        hdf_time_interval = 15
        hdf_start_jmin = 51546240
        hdf_end_jmin = 51675855
        gtm_start_jmin = 51590880.d0
        gtm_end_jmin = 51631200.d0
        
        ! test check_runtime for 15min tidefile data
        call check_runtime(num_blocks, memlen,                   & 
                           memory_buffer, hdf_time_interval,     & 
                           hdf_start_jmin, hdf_end_jmin,         &
                           gtm_start_jmin, gtm_end_jmin)   
        call assertEquals (num_blocks, 433, "Problem in check_runtime num_buffers")
        call assertEquals (memlen(433), 1, "Problem in check_runtime remainder")

        ! test get_loc_in_hydro_buffer
        start_hydro_block = 51546240
        memory_buffer = 20
        hdf_time_interval = 15
       
        gtm_time_interval = five
        current_time = 51546240.d0
        call get_loc_in_hydro_buffer(iblock, slice_in_block, time_in_slice,          &
                                     current_time, start_hydro_block, memory_buffer, &
                                     hdf_time_interval, gtm_time_interval)      
        call assertEquals (iblock, 1, "Problem in get_loc_in_hydro_buffer iblock current_time = 51546240.d0")
        call assertEquals (slice_in_block, 1, "Problem in get_loc_in_hydro_buffer slice_in_block current_time = 51546240.d0")            
        call assertEquals (time_in_slice, zero, "Problem in get_loc_in_hydro_buffer time_in_slice current_time = 51546240.d0")                          

        current_time = 51546555.d0
        call get_loc_in_hydro_buffer(iblock, slice_in_block, time_in_slice,          &
                                     current_time, start_hydro_block, memory_buffer, &
                                     hdf_time_interval, gtm_time_interval)      
        call assertEquals (iblock, 2, "Problem in get_loc_in_hydro_buffer iblock current_time = 51546555.d0")
        call assertEquals (slice_in_block, 2, "Problem in get_loc_in_hydro_buffer slice_in_block current_time = 51546555.d0")            
        call assertEquals (time_in_slice, zero, "Problem in get_loc_in_hydro_buffer time_in_slice current_time = 51546555.d0")  

        current_time = 51546580.d0
        call get_loc_in_hydro_buffer(iblock, slice_in_block, time_in_slice,          &
                                     current_time, start_hydro_block, memory_buffer, &
                                     hdf_time_interval, gtm_time_interval)      
        call assertEquals (iblock, 2, "Problem in get_loc_in_hydro_buffer iblock current_time = 51546580.d0")
        call assertEquals (slice_in_block, 3, "Problem in get_loc_in_hydro_buffer slice_in_block current_time = 51546580.d0")            
        call assertEquals (time_in_slice, 10.d0, "Problem in get_loc_in_hydro_buffer time_in_slice current_time = 51546580.d0")
      
        gtm_time_interval = 2.5d0
        current_time = 51546240.d0
        call get_loc_in_hydro_buffer(iblock, slice_in_block, time_in_slice,          &
                                     current_time, start_hydro_block, memory_buffer, &
                                     hdf_time_interval, gtm_time_interval)      
        call assertEquals (iblock, 1, "Problem in get_loc_in_hydro_buffer iblock current_time = 51546240.d0")
        call assertEquals (slice_in_block, 1, "Problem in get_loc_in_hydro_buffer slice_in_block current_time = 51546240.d0")            
        call assertEquals (time_in_slice, zero, "Problem in get_loc_in_hydro_buffer time_in_slice current_time = 51546240.d0")                          

        current_time = 51546242.5d0
        call get_loc_in_hydro_buffer(iblock, slice_in_block, time_in_slice,          &
                                     current_time, start_hydro_block, memory_buffer, &
                                     hdf_time_interval, gtm_time_interval)      
        call assertEquals (iblock, 1, "Problem in get_loc_in_hydro_buffer iblock current_time = 51546242.5d0")
        call assertEquals (slice_in_block, 1, "Problem in get_loc_in_hydro_buffer slice_in_block current_time = 51546242.5d0")            
        call assertEquals (time_in_slice, 2.5d0, "Problem in get_loc_in_hydro_buffer time_in_slice current_time = 51546242.5d0")                          

        return
    end subroutine
    
end module    