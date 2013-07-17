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
!> Routines to test data filling to GTM network
!>@ingroup test_gtm_core

module ut_gtm_network

    use fruit
   
    contains
    
   !> Test to create flow and area array for entire network
   !> Make sure that ut_hydro_data_tide is executed before this test 
   !> so that all values in common_variables are filled.
    subroutine test_gtm_network()
        use gtm_precision
        use error_handling
        use gtm_logging
        use common_variables
        use hdf_util
        use hydro_data
        use gtm_network
        implicit none
        character(len=*), parameter :: h5_file_name = 'test_hydro.h5'   !< hydro tidefile for testing
        integer :: time_offset, time_buffer       
        integer :: hydro_time_index
        integer :: icell, t, i
        procedure(hydro_data_if), pointer :: dsm2_hydro=> null()  !< Hydrodynamic pointer to be filled by the driver
        dsm2_hydro  => gtm_flow_area
        open(debug_unit, file = "gtm_network_debug.txt")       !< output text file
        npartition_x = 4
        npartition_t = 3
        
        ! this read hydro_ts
        time_offset = 3 
        time_buffer = 20        
        call hdf5_init(h5_file_name)
        call allocate_hydro_ts()       
        call get_ts_from_hdf5(hydro_flow, "flow", time_offset, time_buffer)
        call get_ts_from_hdf5(hydro_ws, "water surface", time_offset, time_buffer)  

        call allocate_network_tmp()
        hydro_time_index = 10        
        call interp_network(npartition_x, npartition_t, hydro_time_index)
        do t = 1, npartition_t+1
            do icell = 1, npartition_x
                write(debug_unit,'(6f15.6)') flow_hi_tmp(icell,t),flow_tmp(icell,t),flow_lo_tmp(icell,t),  &
                                    area_hi_tmp(icell,t), area_tmp(icell,t), area_lo_tmp(icell,t)
            end do        
            write(debug_unit,*) ""
        end do
        do i = 1, n_segm
            write(debug_unit,'(4i5,f15.4)') segm(i)%segm_no, segm(i)%chan_no, segm(i)%up_comppt, segm(i)%down_comppt, segm(i)%length
        end do
        hydro_time_index = 11
        call interp_network(npartition_x, npartition_t, hydro_time_index)
        do t = 1, npartition_t+1
            do icell = 1, npartition_x
                write(debug_unit,'(6f15.6)') flow_hi_tmp(icell,t),flow_tmp(icell,t),flow_lo_tmp(icell,t),  &
                                    area_hi_tmp(icell,t), area_tmp(icell,t), area_lo_tmp(icell,t)
            end do      
            write(debug_unit,*) ""
        end do
        call deallocate_network_tmp()
        close(debug_unit)        
        !call deallocate_hydro_ts() !don't deallocate this one for later use in test_advection_reaction_hdf5
        call hdf5_close()         
        return
    end subroutine
    
end module