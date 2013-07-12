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

!> Main program for General Transport Model
!>@ingroup driver
program gtm

    use gtm_precision
    use error_handling
    use gtm_logging
    use time_util
    use common_variables
    use common_xsect   
   ! use process_input
    use hydro_data_tidefile
    use interpolation
    use gtm_network 
    use hydro_data
    use state_variables
    use primitive_variable_conversion
    use advection
    use source_sink
    use boundary_advection
    
    implicit none
    character(len=*), parameter :: hdf5file = "historical.h5"   !< HDF5 file name
    integer, parameter :: nt = 4
    integer, parameter :: nx = 5
    integer :: nconc = 1
    real(gtm_real), dimension(nt, nx) :: flow_mesh, area_mesh
    real(gtm_real), dimension(nt-1, nx-1) ::  flow_volume_change, area_volume_change
    real(gtm_real), dimension(2000,nx) :: prev_flow_cell !todo::allocate with real number
    real(gtm_real), allocatable :: prev_up_comp_flow(:), prev_down_comp_flow(:)
    real(gtm_real), allocatable :: prev_up_comp_ws(:), prev_down_comp_ws(:)
    real(gtm_real) :: dt
    integer :: up_comp, down_comp
    real(gtm_real) :: total_flow_volume_change, total_area_volume_change
    real(gtm_real) :: avga_volume_change, diff
    integer :: time_offset
    integer :: i, j, t, ibuffer, start_buffer, icell
    real(gtm_real) :: time
    integer :: current_time
    character(len=14) :: gtm_start_time = "02JAN1998 2400"
    character(len=14) :: gtm_end_time = "03JAN1998 0600"
    integer :: gtm_start_jmin, gtm_end_jmin
    real(gtm_real) :: current_julmin
    integer :: offset, num_buffers, jday
    integer, allocatable :: memlen(:)
    procedure(hydro_data_if), pointer :: hydro => null()       !< Hydrodynamic pointer to be filled by the driver
    logical, parameter :: limit_slope = .false.         !< Flag to switch on/off slope limiter  
    real(gtm_real), allocatable :: init_conc(:,:)
    real(gtm_real),parameter :: constant_decay = 1.552749d-5
    hydro => gtm_flow_area
    compute_source => no_source
    !compute_source => linear_decay_source
    advection_boundary_flux => zero_advective_flux
    gtm_time_interval = 5
    npartition_x = 4
    
    
    !call read_input_text("gtm.inp") 
    call cdt2jmin(gtm_start_jmin, gtm_start_time)
    call cdt2jmin(gtm_end_jmin, gtm_end_time)
    
    open(debug_unit, file = "gtm_debug_unit.txt")                   !< output text file
    call hdf5_init(hdf5file)
    write(*,*) "Process DSM2 geometry info...."
    call dsm2_hdf_geom()
    allocate(prev_up_comp_flow(n_comp), prev_down_comp_flow(n_comp))
    allocate(prev_up_comp_ws(n_comp), prev_down_comp_ws(n_comp))

    do i=1,n_segm
        write(debug_unit,*) segm(i)%segm_no, segm(i)%chan_no, segm(i)%length
    end do

    write(debug_unit,*) " "
    do i=1,n_conn
        write(debug_unit,*) conn(i)%conn_no, conn(i)%chan_no, conn(i)%cell_no, conn(i)%conn_up_down
    end do
    
    call check_runtime(offset, num_buffers, memlen,                              &
                       memory_buffer, gtm_start_jmin, gtm_end_jmin,              &
                       hydro_start_julmin, hydro_end_julmin, hydro_time_interval) 
                       
    call get_npartition_t(npartition_t, hydro_time_interval, gtm_time_interval)
    
    call allocate_hydro_ts()
    call allocate_network_tmp()
    call allocate_cell_property()
    call allocate_state(ncell, nconc)

    allocate(init_conc(ncell,nvar))
    init_conc = one
    allocate(linear_decay(nvar))
    linear_decay = constant_decay
       
    write(*,*) "Process time series...."
    dt = hydro_time_interval/(nt-1.)   
    do ibuffer = 1, num_buffers
        write(*,*) ibuffer
        time_offset = offset+memory_buffer*(ibuffer-1)
        call dsm2_hdf_ts(time_offset, memlen(ibuffer))
        if (ibuffer == 1) then   ! skip the first row of data and use it to calculate prev_flow_cell
            start_buffer = 2
        else
            start_buffer = 1
        end if        
        do t = start_buffer, memlen(ibuffer)
            current_time = time_offset + t
            do i = 1, n_segm
               up_comp = segm(i)%up_comppt
               down_comp = segm(i)%down_comppt
               !avga_volume_change = (hydro_avga(up_comp,t)-hydro_avga(up_comp,t-1)) * segm(i)%length
               
               if ((ibuffer==1).and.(t==2)) then
                   do j = 1, nx
                       prev_flow_cell(i,j) = hydro_flow(up_comp,t-1) + (hydro_flow(down_comp,t-1)-hydro_flow(up_comp,t-1))*(j-1)/(nx-1)
                   end do    
                   prev_up_comp_flow(up_comp) = hydro_flow(up_comp,1)
                   prev_down_comp_flow(down_comp) = hydro_flow(down_comp,1)
                   prev_up_comp_ws(up_comp) = hydro_ws(up_comp,1)
                   prev_down_comp_ws(down_comp) = hydro_ws(down_comp,1) 
               end if   
               call interp_flow_area(flow_mesh, area_mesh, flow_volume_change, area_volume_change,   &
                                     segm(i)%chan_no, segm(i)%up_distance, segm(i)%length/(nx-1.), dt, nt, nx,           &
                                     prev_up_comp_flow(up_comp), prev_down_comp_flow(down_comp),     &
                                     hydro_flow(up_comp,t), hydro_flow(down_comp,t),                 &
                                     prev_up_comp_ws(up_comp), prev_down_comp_ws(down_comp),         &
                                     hydro_ws(up_comp,t), hydro_ws(down_comp,t), prev_flow_cell(i,:))
               call fill_network(i, flow_mesh, area_mesh)                      
               !write(debug_unit,'(3i8,7f15.4)')  segm(i)%chan_no,up_comp,down_comp,hydro_flow(up_comp,t), hydro_flow(down_comp,t), prev_flow_cell(i,1), prev_flow_cell(i,2), prev_flow_cell(i,3), prev_flow_cell(i,4), prev_flow_cell(i,5)
               !call calc_total_volume_change(total_flow_volume_change, nt-1, nx-1, flow_volume_change)
               !call calc_total_volume_change(total_area_volume_change, nt-1, nx-1, area_volume_change)
               !diff = (total_flow_volume_change-avga_volume_change)/avga_volume_change * 100
               !write(debug_unit,'(3i6,4f10.4)') segm(i)%chan_no,up_comp, down_comp, prev_up_comp_ws(up_comp), prev_down_comp_ws(down_comp),hydro_ws(up_comp,t), hydro_ws(down_comp,t)

               prev_flow_cell(i,:) = flow_mesh(nt,:)
               prev_up_comp_flow(up_comp) = hydro_flow(up_comp,t)
               prev_down_comp_flow(down_comp) = hydro_flow(down_comp,t)
               prev_up_comp_ws(up_comp) = hydro_ws(up_comp,t)
               prev_down_comp_ws(down_comp) = hydro_ws(down_comp,t)              
               
               !if (diff.gt.ten*two)then   !todo::I tried to figure out when and why there are huge inconsistency of volume change from interpolation and average area
               !    write(debug_unit,'(2a8,5a20)') "t","chan_no","segm_length","flow_vol_change","area_vol_change","avga_vol_change","% difference"           
               !    write(debug_unit,'(2i8,5f20.5)') t, segm(i)%chan_no, segm(i)%length, total_flow_volume_change,total_area_volume_change, avga_volume_change, diff
               !    write(debug_unit,'(a10)') "flow mesh"
               !    call print_mass_balance_check(debug_unit, nt, nx, flow_mesh, flow_volume_change) 
               !    write(debug_unit,'(a10)') "area mesh"
               !    call print_mass_balance_check(debug_unit, nt, nx, area_mesh, area_volume_change)
               !    !write(debug_unit,*) prev_flow_cell(1),prev_flow_cell(2),prev_flow_cell(3),prev_flow_cell(4),prev_flow_cell(5)               
               !    write(debug_unit,*) ""
               !end if
            end do   !end for segment loop
            do time = 1, nt
               write(debug_unit,*) current_time, time
                call hydro(flow,     &
                           flow_lo,  &
                           flow_hi,  &
                           area,     &
                           area_lo,  &
                           area_hi,  &
                           ncell,    &
                           time,     &
                           dx_arr,   &
                           dt)            
                if ((ibuffer==1).and.(t==2).and.(time==1)) then
                    call prim2cons(mass_prev, init_conc, area, ncell, nvar)
                    area_prev = area
                end if
                current_julmin = current_time * hydro_time_interval + (time-1)*gtm_time_interval
                ! call advection and source

                call advect(mass,              &
                            mass_prev,         &  
                            flow,              &
                            flow_lo,           &
                            flow_hi,           &
                            area,              &
                            area_prev,         &
                            area_lo,           &
                            area_hi,           &
                            ncell,             &
                            nvar,              &
                            current_julmin,    &
                            gtm_time_interval, &
                            dx_arr,            &
                            limit_slope)     
                call cons2prim(conc, mass, area, ncell, nvar)
                write(debug_unit,'(1000f10.5)') (conc(icell,1),icell=1,ncell)
                mass_prev = mass
                area_prev = area         
            end do                           
        end do
    end do    
    call deallocate_network_tmp()
    call deallocate_hydro_ts()
    call hdf5_close()
    close(debug_unit)
end program

