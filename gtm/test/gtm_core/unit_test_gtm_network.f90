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
!> Routines to test data filling to GTM network
!>@ingroup test_gtm_core

module ut_gtm_network

    use fruit
   
    contains
    
   !> Test to create flow and area array for entire network
   !> Make sure that ut_hydro_data_tide is executed before this test 
   !> so that all values in common_variables are filled.
   !>       (5)=====5500======(4)=====12000=====(3)=====9000=======(2)=====11000========(1)
   !>                [4]               [3]       ||      [2]                 [1]
   !>                                             \\
   !>                                              ===2400===(6)
   !>                                                  [5]
    subroutine test_gtm_network()
        use gtm_precision
        use error_handling
        use gtm_logging
        use common_variables
        use hdf_util
        use hydro_data_tidefile
        use hydro_data
        use gtm_network
        implicit none
        character(len=*), parameter :: h5_file_name = 'network.h5'   !< hydro tidefile for testing
        integer :: time_offset, time_buffer       
        integer :: hydro_time_index
        integer :: icell, t, i
        real(gtm_real), allocatable :: prev_comp_flow(:) 
        real(gtm_real), allocatable :: prev_comp_ws(:)
        real(gtm_real), allocatable :: prev_resv(:)
        real(gtm_real), allocatable :: prev_resv_conn(:)
        real(gtm_real), allocatable :: prev_qext(:)
        real(gtm_real), allocatable :: prev_tran(:)
        real(gtm_real), allocatable :: prev_flow_cell_lo(:)
        real(gtm_real), allocatable :: prev_flow_cell_hi(:)
        procedure(hydro_data_if), pointer :: dsm2_hydro=> null()  !< Hydrodynamic pointer to be filled by the driver
        dsm2_hydro  => gtm_flow_area
        open(debug_unit, file = "gtm_network_debug.txt")       !< output text file
        npartition_t = 3
        gtm_dx = 1250.d0
        
        ! this read hydro_ts
        time_offset = 3 
        time_buffer = 20        
        call hdf5_init(h5_file_name)
        call dsm2_hdf_geom
        allocate(prev_comp_flow(n_comp), prev_comp_ws(n_comp))
        allocate(prev_resv(n_resv), prev_resv_conn(n_resv_conn))
        allocate(prev_qext(n_qext), prev_tran(n_tran))

        prev_flow_cell_lo = LARGEREAL
        prev_flow_cell_hi = LARGEREAL
        
        call allocate_hydro_ts
        call dsm2_hdf_ts(time_offset, time_buffer) 
        call dsm2_hdf_slice(prev_comp_flow, prev_comp_ws, prev_resv, prev_resv_conn, prev_qext, prev_tran, n_comp, n_resv, n_resv_conn, n_qext, n_tran, time_offset-1)             

        allocate(prev_flow_cell_lo(28))
        allocate(prev_flow_cell_hi(28))    
            
        call allocate_network_tmp(npartition_t)
        hydro_time_index = 10 
        call interp_network_linear(npartition_t, hydro_time_index, n_comp, prev_comp_flow, prev_comp_ws, n_cell, prev_flow_cell_lo, prev_flow_cell_hi)
        !call assertEquals (junc(1)%dsm2_network_no, 3, "problem in allocate network junc(1)%dsm2_network_no")
        !call assertEquals (junc(1)%cell_no(1), 15, "problem in allocate network junc(1)%cell_no(1)")
        !call assertEquals (junc(1)%cell_no(2), 16, "problem in allocate network junc(1)%cell_no(2)")
        !call assertEquals (junc(1)%cell_no(3), 28, "problem in allocate network junc(1)%cell_no(3)")
        call assertEquals (dx_arr(1), dble(1375), weakest_eps, "problem in allocate network dx_arr(1)")
        call assertEquals (dx_arr(9), dble(1285.71429), weakest_eps, "problem in allocate network dx_arr(9)")
        call assertEquals (segm(7)%up_comppt, 9, "problem in allocate network segm(7)%up_comppt")
        call assertEquals (segm(7)%down_comppt, 10, "problem in allocate network segm(7)%down_comppt") 
        write(debug_unit,*) "flow_mesh_lo at hydro_time_index=10:"      
        do t = 1, npartition_t+1
            write(debug_unit,'(28f15.6)') (flow_mesh_lo(t,icell),icell=1,28)
        end do
        write(debug_unit,*) ""       
        write(debug_unit,*) "flow_mesh_hi at hydro_time_index=10:"
        do t = 1, npartition_t+1
            write(debug_unit,'(28f15.6)') (flow_mesh_hi(t,icell),icell=1,28)
        end do
        write(debug_unit,*) ""     
                
        hydro_time_index = 11
        call dsm2_hdf_slice(prev_comp_flow, prev_comp_ws, prev_resv, prev_resv_conn, prev_qext, prev_tran, n_comp, n_resv, n_resv_conn, n_qext, n_tran, time_offset)           
        call interp_network_linear(npartition_t, hydro_time_index, n_comp, prev_comp_flow, prev_comp_ws, n_cell, prev_flow_cell_lo, prev_flow_cell_hi)
        write(debug_unit,*) "flow_mesh_lo at hydro_time_index=11:"
        do t = 1, npartition_t+1
            write(debug_unit,'(28f15.6)') (flow_mesh_lo(t,icell),icell=1,28)  
        end do
        write(debug_unit,*) ""
        write(debug_unit,*) "flow_mesh_hi at hydro_time_index=11:"
        do t = 1, npartition_t+1
            write(debug_unit,'(28f15.6)') (flow_mesh_hi(t,icell),icell=1,28)  
        end do
        write(debug_unit,*) ""        
        deallocate(prev_flow_cell_lo, prev_flow_cell_hi)
        call deallocate_network_tmp
        call deallocate_geometry
        call deallocate_virt_xsect
        call deallocate_hydro_ts
        close(debug_unit)        
        !call deallocate_hydro_ts() !don't deallocate this one for later use in test_advection_reaction_hdf5
        call hdf5_close()         
        return
    end subroutine
    
end module