
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
        integer :: offset, num_buffers, remainder
        integer :: runtime_hydro_start, runtime_hydro_end
        integer :: iblock, slice_in_block
        real(gtm_real) :: current_time
        integer :: start_hydro_block, memory_buffer, hdf_time_interval
        integer, allocatable :: memlen(:)
        integer :: skip
        real(gtm_real) :: gtm_time_interval
        real(gtm_real) :: time_index
        
        jday = cdt2jmin('01SEP2001 2400')
        call assertEquals (jday, 53474400, "problem in cdt2jmin function")
        cdt = jmin2cdt(53474400)
        call assertEquals (cdt, '01SEP2001 2400', "problem in jmin2cdt function")  
        
        ! test check_runtime for 15min tidefile data
        call check_runtime(offset, num_buffers, memlen,                           &  
                           runtime_hydro_start, runtime_hydro_end, skip,          &
                           1500, 39402720.d0, 39445920.d0,                        &  ! '30NOV1974 2400':39402720, '30DEC1974 2400':39445920
                           39314880, 39840480, 15, five)                             ! 01OCT1974 0000-01OCT1975 0000
        call assertEquals (skip, 3, "Problem in check_runtime 15min skip") 
        call assertEquals (offset, 5855, "Problem in check_runtime offset")
        call assertEquals (num_buffers, 2, "Problem in check_runtime num_buffers")
        call assertEquals (memlen(2), 1381, "Problem in check_runtime remainder")
        call assertEquals (runtime_hydro_start, 39402705, "Problem in check_runtime runtime_hydro_start")
        call assertEquals (runtime_hydro_end, 39445920, "Problem in check_runtime runtime_hydro_end")

        ! test check_runtime for 15min tidefile data and dt=2.5min
        call check_runtime(offset, num_buffers, memlen,                           &  
                           runtime_hydro_start, runtime_hydro_end, skip,          &
                           1500, 39402720.d0, 39445920.d0,                        &  ! '30NOV1974 2400':39402720, '30DEC1974 2400':39445920
                           39314880, 39840480, 15, 2.5d0)                             ! 01OCT1974 0000-01OCT1975 0000
        call assertEquals (skip, 6, "Problem in check_runtime 15min skip") 
        call assertEquals (offset, 5855, "Problem in check_runtime offset")
        call assertEquals (num_buffers, 2, "Problem in check_runtime num_buffers")
        call assertEquals (memlen(2), 1381, "Problem in check_runtime remainder")
        call assertEquals (runtime_hydro_start, 39402705, "Problem in check_runtime runtime_hydro_start")
        call assertEquals (runtime_hydro_end, 39445920, "Problem in check_runtime runtime_hydro_end")


        ! test check_runtime for 1hour tidefile data
        call check_runtime(offset, num_buffers, memlen,                           &  
                           runtime_hydro_start, runtime_hydro_end, skip,          &
                           1500, 39402720.d0, 39445935.d0,                        &  ! '30NOV1974 2400':39402720, '30DEC1974 2400':39445920
                           39314880, 39840480, 60, five)                             ! 01OCT1974 0000-01OCT1975 0000
        call assertEquals (offset, 1463, "Problem in check_runtime offset")
        call assertEquals (num_buffers, 1, "Problem in check_runtime num_buffers")
        call assertEquals (runtime_hydro_end, 39445980, "Problem in check_runtime runtime_hydro_end")
                
        ! test get_loc_in_hydro_buffer
        start_hydro_block = 39402720
        memory_buffer = 20
        hdf_time_interval = 15
        gtm_time_interval = five
        skip = 0
        
        current_time = 39404220.d0  !at the end of 5th block
        call get_loc_in_hydro_buffer(iblock, slice_in_block, time_index,   &
                                     current_time, start_hydro_block,      &
                                     memory_buffer, skip,                  &
                                     hdf_time_interval, gtm_time_interval)      
        call assertEquals (iblock, 6, "Problem in get_loc_in_hydro_buffer iblock")
        call assertEquals (slice_in_block, 1, "Problem in get_loc_in_hydro_buffer slice_in_block")            
        call assertEquals (time_index, one, "Problem in get_loc_in_hydro_buffer time_index")                          
                                     
        current_time = 39404250.d0  !the 2nd slice in the 6th blcok
        call get_loc_in_hydro_buffer(iblock, slice_in_block, time_index,  &
                                     current_time, start_hydro_block,     &
                                     memory_buffer, skip,                 &
                                     hdf_time_interval,gtm_time_interval)        
        call assertEquals (iblock, 6, "Problem in get_loc_in_hydro_buffer iblock")
        call assertEquals (slice_in_block, 3, "Problem in get_loc_in_hydro_buffer slice_in_block")
        
        ! test for 15min tidefile data & 2.5min gtm runtime interval

        memory_buffer = 20
        hdf_time_interval = 15
        gtm_time_interval = 2.5d0
       
        call check_runtime(offset, num_buffers, memlen,                           &  
                           runtime_hydro_start, runtime_hydro_end, skip,          &
                           1500, 39402720.d0, 39445920.d0,                        &  ! '30NOV1974 2400':39402720, '30DEC1974 2400':39445920
                           39314880, 39840480, 15, gtm_time_interval)                             ! 01OCT1974 0000-01OCT1975 0000
        call assertEquals (skip, 6, "Problem in check_runtime 15min skip") 
        call assertEquals (offset, 5855, "Problem in check_runtime offset")
        call assertEquals (num_buffers, 2, "Problem in check_runtime num_buffers")
        call assertEquals (memlen(2), 1381, "Problem in check_runtime remainder")
        call assertEquals (runtime_hydro_start, 39402705, "Problem in check_runtime runtime_hydro_start")
        call assertEquals (runtime_hydro_end, 39445920, "Problem in check_runtime runtime_hydro_end")        
        
        current_time = 39404225.d0  !roughly at the beginning of 6th block
        call get_loc_in_hydro_buffer(iblock, slice_in_block, time_index,   &
                                     current_time, start_hydro_block,      &
                                     memory_buffer, skip,                  &
                                     hdf_time_interval, gtm_time_interval)  
        call assertEquals (iblock, 6, "Problem in get_loc_in_hydro_buffer iblock")
        call assertEquals (slice_in_block, 2, "Problem in get_loc_in_hydro_buffer slice_in_block")            
        call assertEquals (time_index, three, "Problem in get_loc_in_hydro_buffer time_index")                                              
                     
        current_time = 39404230.d0  !roughly at the beginning of 6th block
        call get_loc_in_hydro_buffer(iblock, slice_in_block, time_index,   &
                                     current_time, start_hydro_block,      &
                                     memory_buffer, skip,                  &
                                     hdf_time_interval, gtm_time_interval)  
        call assertEquals (iblock, 6, "Problem in get_loc_in_hydro_buffer iblock")
        call assertEquals (slice_in_block, 2, "Problem in get_loc_in_hydro_buffer slice_in_block")            
        call assertEquals (time_index, five, "Problem in get_loc_in_hydro_buffer time_index")     
                
        return
    end subroutine
    
end module    