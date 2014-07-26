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

!> Routines to fill data into the entire network of GTM grids
!>@ingroup gtm_core
module gtm_network    

    use gtm_precision
    use error_handling
    use gtm_logging
    use common_variables
    use common_xsect
    
    real(gtm_real), allocatable :: flow_mesh_lo(:,:)
    real(gtm_real), allocatable :: flow_mesh_hi(:,:)
    real(gtm_real), allocatable :: area_mesh_lo(:,:)
    real(gtm_real), allocatable :: area_mesh_hi(:,:)
    real(gtm_real), allocatable :: resv_height_mesh(:,:)
    real(gtm_real), allocatable :: resv_flow_mesh(:,:)
    real(gtm_real), allocatable :: qext_flow_mesh(:,:)
    real(gtm_real), allocatable :: tran_flow_mesh(:,:)     
    
    real(gtm_real), allocatable :: prev_flow_cell_lo(:)
    real(gtm_real), allocatable :: prev_flow_cell_hi(:)
    real(gtm_real), allocatable :: flow_volume_change(:,:)
    real(gtm_real), allocatable :: area_volume_change(:,:)

    contains      
    
    !> Allocate network temporary array
    subroutine allocate_network_tmp()
        implicit none
        integer :: istat = 0
        character(len=128) :: message
        allocate(flow_mesh_lo(npartition_t+1, n_cell), stat = istat)
        allocate(flow_mesh_hi(npartition_t+1, n_cell), stat = istat)
        allocate(area_mesh_lo(npartition_t+1, n_cell), stat = istat)
        allocate(area_mesh_hi(npartition_t+1, n_cell), stat = istat)
        allocate(resv_height_mesh(npartition_t+1, n_resv), stat = istat)
        allocate(resv_flow_mesh(npartition_t+1, n_resv_conn), stat = istat)
        allocate(qext_flow_mesh(npartition_t+1, n_qext), stat = istat)
        allocate(tran_flow_mesh(npartition_t+1, n_tran), stat = istat)        
        allocate(flow_volume_change(npartition_t, n_cell), stat = istat)
        allocate(area_volume_change(npartition_t, n_cell), stat = istat)        
        allocate(prev_flow_cell_lo(n_cell), stat = istat)
        allocate(prev_flow_cell_hi(n_cell), stat = istat)
        if (istat .ne. 0 )then
           call gtm_fatal(message)
        end if
        flow_mesh_lo = LARGEREAL
        flow_mesh_hi = LARGEREAL
        area_mesh_lo = LARGEREAL
        area_mesh_hi = LARGEREAL
        resv_height_mesh = LARGEREAL
        resv_flow_mesh = LARGEREAL
        qext_flow_mesh = LARGEREAL
        tran_flow_mesh = LARGEREAL
        flow_volume_change = LARGEREAL
        area_volume_change = LARGEREAL
        prev_flow_cell_lo = LARGEREAL
        prev_flow_cell_hi = LARGEREAL
        return
    end subroutine
 
    !> Deallocate network temporary array
    subroutine deallocate_network_tmp()
        implicit none
        deallocate(flow_mesh_lo, flow_mesh_hi)
        deallocate(area_mesh_lo, area_mesh_hi)
        deallocate(resv_height_mesh)
        deallocate(resv_flow_mesh)
        deallocate(qext_flow_mesh)
        deallocate(tran_flow_mesh)
        deallocate(prev_flow_cell_lo, prev_flow_cell_hi)
        return
    end subroutine
   
    
    !> Return flow_tmp(1:ncell) and area_tmp(1:ncell) for the entire network 
    !> at the specified hydro time index
    subroutine interp_network(npart_t, hydro_time_index, ncomp, prev_flow, prev_ws)
        use interpolation
        implicit none
        integer, intent(in) :: npart_t                    !< number of partitions in time
        integer, intent(in) :: hydro_time_index           !< starting time step index in DSM2 hydro 
        integer, intent(in) :: ncomp                    
        real(gtm_real), intent(in) :: prev_flow(ncomp)
        real(gtm_real), intent(in) :: prev_ws(ncomp)
        real(gtm_real) :: dt, dx                                                                  ! local variables
        integer :: nx, nt                                                                         ! local variables
        integer :: up_comp, down_comp                                                             ! local variables
        integer :: i, j, t, icell, t_index                                                        ! local variables
                        
        nt = npart_t + 1
        t_index = hydro_time_index 
        do i = 1, n_segm
            nx = segm(i)%nx + one
            dt = hydro_time_interval/npart_t
            dx = segm(i)%length/segm(i)%nx
            up_comp = segm(i)%up_comppt
            down_comp = segm(i)%down_comppt        
            if (prev_flow_cell_lo(segm(i)%start_cell_no)==LARGEREAL) then
               do j = 1, segm(i)%nx
                   icell = segm(i)%start_cell_no + j - 1
                   prev_flow_cell_lo(icell) = hydro_flow(up_comp,t_index-1)+(hydro_flow(down_comp,t_index-1) &
                                       -hydro_flow(up_comp,t_index-1))*(j-1)/(segm(i)%nx)
                   prev_flow_cell_hi(icell) = hydro_flow(up_comp,t_index-1)+(hydro_flow(down_comp,t_index-1) &
                                       -hydro_flow(up_comp,t_index-1))*j/(segm(i)%nx)                                       
               end do
            end if   
            
            call interp_flow_area(flow_mesh_lo, flow_mesh_hi, area_mesh_lo, area_mesh_hi,             &
                                  flow_volume_change, area_volume_change,                             &
                                  n_cell, segm(i)%start_cell_no,                                      &
                                  segm(i)%chan_no, segm(i)%up_distance, dx, dt, nt, segm(i)%nx,       &
                                  prev_flow(up_comp), prev_flow(down_comp), hydro_flow(up_comp,t_index), hydro_flow(down_comp,t_index),   &
                                  prev_ws(up_comp), prev_ws(down_comp), hydro_ws(up_comp,t_index), hydro_ws(down_comp,t_index),           &
                                  prev_flow_cell_lo, prev_flow_cell_hi)                              
        end do
        prev_flow_cell_lo(:) = flow_mesh_lo(nt,:)
        prev_flow_cell_hi(:) = flow_mesh_hi(nt,:)
        return
    end subroutine
    
    !> interpolate external flows data (reservoir, qext and transfer) for node
    !> use common_variabls, only : npartition_t, n_resv, nresv_conn
    subroutine interp_node(hydro_time_index, prev_resv, prev_resv_conn, prev_qext, prev_tran)
        implicit none
        integer, intent(in) :: hydro_time_index
        real(gtm_real), intent(in) :: prev_resv(n_resv)
        real(gtm_real), intent(in) :: prev_resv_conn(n_resv_conn)
        real(gtm_real), intent(in) :: prev_qext(n_qext)
        real(gtm_real), intent(in) :: prev_tran(n_tran)
        call interp_linear_in_time(resv_height_mesh, npartition_t+1, n_resv, hydro_resv_height(:,hydro_time_index), prev_resv)
        call interp_linear_in_time(resv_flow_mesh, npartition_t+1, n_resv_conn, hydro_resv_flow(:,hydro_time_index), prev_resv_conn)
        call interp_linear_in_time(qext_flow_mesh, npartition_t+1, n_qext, hydro_qext_flow(:,hydro_time_index), prev_qext)
        call interp_linear_in_time(tran_flow_mesh, npartition_t+1, n_tran, hydro_tran_flow(:,hydro_time_index), prev_tran)        
        return
    end subroutine    
    
    !> return linear interpolated values (only interpolate over time, not space, because they are not spatially related.)
    subroutine interp_linear_in_time(mesh, nt, n_dim, current_ts, prev_ts)
        implicit none
        integer, intent(in) :: nt                        !< number of partition in time
        integer, intent(in) :: n_dim                     !< dimension
        real(gtm_real), intent(in) :: prev_ts(n_dim)     !< time series at previous time slice
        real(gtm_real), intent(in) :: current_ts(n_dim)  !< time series at current time slice
        real(gtm_real), intent(out) :: mesh(n_dim, nt)
        integer :: i, j
        do i = 1, n_dim
            do j = 1, nt
                mesh(i, j) = prev_ts(i) + (current_ts(i)-prev_ts(i))*(dble(j)-one)/(dble(nt)-one)
            end do
        end do        
        return
    end subroutine
    
    
    !> hydrodynamic interface to retrieve area and flow
    subroutine gtm_flow_area(flow,    &
                             flow_lo, &
                             flow_hi, &
                             area,    &
                             area_lo, &
                             area_hi, &
                             ncell,   &
                             time,    &
                             dx,      &                        
                             dt)                
        implicit none
        integer, intent(in) :: ncell                   !< Number of cells
        real(gtm_real), intent(in) :: time             !< Time of request
        real(gtm_real), intent(in) :: dt               !< Time step 
        real(gtm_real), intent(in) :: dx(ncell)        !< Spatial step
        real(gtm_real), intent(out) :: flow(ncell)     !< Cell and time centered flow
        real(gtm_real), intent(out) :: flow_lo(ncell)  !< Low face flow, time centered
        real(gtm_real), intent(out) :: flow_hi(ncell)  !< High face flow, time centered
        real(gtm_real), intent(out) :: area(ncell)     !< Cell center area, old time
        real(gtm_real), intent(out) :: area_lo(ncell)  !< Area lo face, time centered
        real(gtm_real), intent(out) :: area_hi(ncell)  !< Area hi face, time centered
        integer :: time_in_mesh, i                     ! local variable
        !if (mod(int(time),npartition_t)==0) then
        !    time_in_mesh = npartition_t+1       
        !else
        !    time_in_mesh = mod(int(time),npartition_t)+1
        !end if    
        time_in_mesh = int(time)
        flow_lo = flow_mesh_lo(time_in_mesh,:)
        flow_hi = flow_mesh_hi(time_in_mesh,:)
        flow    = half * (flow_mesh_lo(time_in_mesh,:)+flow_mesh_hi(time_in_mesh,:))
        area_lo = area_mesh_lo(time_in_mesh,:)
        area_hi = area_mesh_hi(time_in_mesh,:)
        area    = half * (area_mesh_lo(time_in_mesh,:)+area_mesh_hi(time_in_mesh,:))
        !write(debug_unit,'(f8.0,i4,5f10.1)') time, time_in_mesh ,flow_tmp(1,1),flow_tmp(1,2),flow_tmp(1,3),flow_tmp(1,4), flow(1)
        return
    end subroutine       


    !> hydrodynamic interface to retrieve reservor, external and transfer flows
    !> connected to nodes
    subroutine gtm_node(resv_height,  &
                        resv_flow,    &
                        qext_flow,    &
                        tran_flow,    &
                        nresv,        &
                        nresv_conn,   &
                        nqext,        &
                        ntran,        &
                        time)
        implicit none
        integer, intent(in) :: nresv                         !< number of reservoirs
        integer, intent(in) :: nresv_conn                    !< number of reservoir connections
        integer, intent(in) :: nqext                         !< number of external flows
        integer, intent(in) :: ntran                         !< number of transfer flows
        real(gtm_real), intent(in) :: time                   !< time index in the mesh
        real(gtm_real), intent(out) :: resv_height(nresv)    !< reservoir height
        real(gtm_real), intent(out) :: resv_flow(nresv_conn) !< reservoir flow
        real(gtm_real), intent(out) :: qext_flow(nqext)      !< external flow
        real(gtm_real), intent(out) :: tran_flow(ntran)      !< transfer flow
        integer :: time_index
        time_index = int(time)
        resv_height(:) = resv_height_mesh(time_index,:)
        resv_flow(:) = resv_flow_mesh(time_index,:)
        qext_flow(:) = qext_flow_mesh(time_index,:)
        tran_flow(:) = tran_flow_mesh(time_index,:)                        
        return                
    end subroutine                    

end module