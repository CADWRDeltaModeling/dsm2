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
!> Routines to test interpolation scheme with order of accuracy
!>@ingroup test_gtm_core

module ut_interpolation

    use fruit
   
    contains
    
    subroutine test_interpolation_ooa
        use common_variables
        use common_xsect
        use gtm_network
        use interpolation
        implicit none
        real(gtm_real) :: A(31), Q(31)
        real(gtm_real) :: dx, dt
        real(gtm_real) :: dA, dQ
        integer :: i
        integer :: nx
        real(gtm_real) :: hydro_dx, hydro_dt
        real(gtm_real) :: fine_dx, fine_dt
        real(gtm_real), allocatable :: flow_mesh_lo_ref(:,:), flow_mesh_hi_ref(:,:)
        real(gtm_real), allocatable :: prev_flow_cell_lo(:), prev_flow_cell_hi(:)
        
        nx = 4
        npartition_t = 3
        n_chan = 1
        n_resv = 0
        n_qext = 0
        n_comp = 31
        n_segm = 30
        n_cell = n_segm * nx 
        n_irreg = 2
        n_boun = 2
        n_junc = 0
        n_conn = 0
        allocate(flow_mesh_lo_ref(npartition_t+1,n_segm*nx))
        allocate(flow_mesh_hi_ref(npartition_t+1,n_segm*nx))
        allocate(prev_flow_cell_lo(n_cell))
        allocate(prev_flow_cell_hi(n_cell))
        call allocate_chan_virt_xsect
        call allocate_virt_xsec_geom
        virt_xsect(:)%min_elev = 0d0
        virt_xsect(:)%ID = 1
        virt_xsect(:)%chan_no = 1
        virt_xsect(1)%vsecno = 1           
        virt_xsect(2)%vsecno = 2   
        virt_xsect(:)%num_elev = 6         !< number of elevations in the sec
        virt_xsect(:)%num_virt_sec = 1     !< number of virt_xsect in this channel
        virt_xsect(1)%elevation(1) = 0d0
        virt_xsect(1)%elevation(2) = 20d0
        virt_xsect(1)%elevation(3) = 40d0
        virt_xsect(1)%elevation(4) = 60d0
        virt_xsect(1)%elevation(5) = 80d0
        virt_xsect(1)%elevation(6) = 100d0
        virt_xsect(1)%width(1) = 25d0
        virt_xsect(1)%width(2) = 25d0
        virt_xsect(1)%width(3) = 25d0
        virt_xsect(1)%width(4) = 25d0
        virt_xsect(1)%width(5) = 25d0
        virt_xsect(1)%width(6) = 25d0        
        virt_xsect(1)%area(1) = 0d0
        virt_xsect(1)%area(2) = 500d0
        virt_xsect(1)%area(3) = 1000d0
        virt_xsect(1)%area(4) = 1500d0
        virt_xsect(1)%area(5) = 2000d0
        virt_xsect(1)%area(6) = 2500d0
        virt_xsect(1)%wet_p(1) = 0d0
        virt_xsect(1)%wet_p(2) = 65d0
        virt_xsect(1)%wet_p(3) = 105d0
        virt_xsect(1)%wet_p(4) = 145d0
        virt_xsect(1)%wet_p(5) = 185d0
        virt_xsect(1)%wet_p(6) = 225d0
        virt_xsect(2)%elevation(2) = 20d0
        virt_xsect(2)%elevation(3) = 40d0
        virt_xsect(2)%elevation(4) = 60d0
        virt_xsect(2)%elevation(5) = 80d0
        virt_xsect(2)%elevation(6) = 100d0
        virt_xsect(2)%width(1) = 25d0
        virt_xsect(2)%width(2) = 25d0
        virt_xsect(2)%width(3) = 25d0
        virt_xsect(2)%width(4) = 25d0
        virt_xsect(2)%width(5) = 25d0
        virt_xsect(2)%width(6) = 25d0        
        virt_xsect(2)%area(1) = 0d0
        virt_xsect(2)%area(2) = 500d0
        virt_xsect(2)%area(3) = 1000d0
        virt_xsect(2)%area(4) = 1500d0
        virt_xsect(2)%area(5) = 2000d0
        virt_xsect(2)%area(6) = 2500d0
        virt_xsect(2)%wet_p(1) = 0d0
        virt_xsect(2)%wet_p(2) = 65d0
        virt_xsect(2)%wet_p(3) = 105d0
        virt_xsect(2)%wet_p(4) = 145d0
        virt_xsect(2)%wet_p(5) = 185d0
        virt_xsect(2)%wet_p(6) = 225d0


        call allocate_geometry
        num_xsect_chan(1) = 1
        chan_geom(1)%channel_num = 1
        chan_geom(1)%chan_no = 1
        chan_geom(1)%channel_length = 120000
        chan_geom(1)%up_node = 1
        chan_geom(1)%down_node = 2                     
        chan_geom(1)%up_comp = 1               
        chan_geom(1)%down_comp = 31
        
        do i = 1, n_segm
            segm(i)%segm_no = i
            segm(i)%chan_no = 1
            segm(i)%up_comppt = i
            segm(i)%down_comppt = i + 1
            segm(i)%nx = nx 
            segm(i)%start_cell_no = (i-1)*segm(i)%nx +1
            segm(i)%length = 4000            
            segm(i)%up_distance = (i-1)*segm(i)%length
            segm(i)%down_distance = i*segm(i)%length
        end do                
        call allocate_cell_property
        call allocate_network_tmp(npartition_t)
        call allocate_hydro_ts
        
        hydro_dx = 4000
        hydro_dt = 900
        fine_dx = 1000
        fine_dt = 300
        A(1) = 1330d0
        Q(1) = 1820d0
        
        do i = 2, n_comp
            dA = dsin(two*pi/dble(30)*(dble(i)-one))*dble(100)
            A(i) = A(i-1) + dA
            dQ = dA * hydro_dx / hydro_dt
            Q(i) = Q(1) + dQ
        end do
        
        ! reference flow
        flow_mesh_lo_ref(1,:) = 1820d0
        do i = 1, n_segm*nx
            flow_mesh_lo_ref(2,i) = flow_mesh_lo_ref(1,i) + dsin(two*pi/dble(30*nx)*(dble(i)-one))*dble(100)*fine_dx/fine_dt
            flow_mesh_lo_ref(3,i) = flow_mesh_lo_ref(2,i) + dsin(two*pi/dble(30*nx)*(dble(i)-one))*dble(100)*fine_dx/fine_dt
            flow_mesh_lo_ref(4,i) = flow_mesh_lo_ref(3,i) + dsin(two*pi/dble(30*nx)*(dble(i)-one))*dble(100)*fine_dx/fine_dt
        end do
        
        hydro_flow(:,1) = 1820d0
        hydro_flow(:,2) = Q(:)
        hydro_ws(:,1) = A(:)/25d0
        hydro_ws(:,2) = A(:)/25d0
        prev_flow_cell_lo = 1820d0
        prev_flow_cell_hi = 1820d0
        do i = 1, n_segm
            call interp_flow_area(flow_mesh_lo, flow_mesh_hi, area_mesh_lo, area_mesh_hi,  width_mesh, hydro_radius_mesh, depth_mesh, &
                                  flow_volume_change, area_volume_change, n_cell, segm(i)%start_cell_no , segm(i)%chan_no, segm(i)%up_distance, fine_dx, fine_dt,  npartition_t+1, segm(i)%nx,hydro_flow(segm(i)%up_comppt,1), hydro_flow(segm(i)%down_comppt,1), hydro_flow(segm(i)%up_comppt,2), hydro_flow(segm(i)%down_comppt,2), hydro_ws(segm(i)%up_comppt,1), hydro_ws(segm(i)%down_comppt,1), hydro_ws(segm(i)%up_comppt,2), hydro_ws(segm(i)%down_comppt,2), prev_flow_cell_lo, prev_flow_cell_hi)
        end do    
        do i = 1, n_segm*nx
            write(debug_unit,'(i4,12f10.3)') i, flow_mesh_lo_ref(1,i), flow_mesh_lo_ref(2,i), flow_mesh_lo_ref(3,i), flow_mesh_lo_ref(4,i), flow_mesh_lo(1,i), flow_mesh_lo(2,i), flow_mesh_lo(3,i), flow_mesh_lo(4,i), area_mesh_lo(1,i), area_mesh_lo(2,i), area_mesh_lo(3,i), area_mesh_lo(4,i)
        end do 
        deallocate(flow_mesh_lo_ref, flow_mesh_hi_ref)         
        deallocate(prev_flow_cell_lo, prev_flow_cell_hi)
        call deallocate_virt_xsect
        call deallocate_network_tmp
        call deallocate_geometry
        call deallocate_hydro_ts
        return
    end subroutine
    
end module
