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
    use logging
    use common_variables
    use common_xsect   
    use hydro_data_tidefile
    use interpolation
    
    implicit none
    character(len=*), parameter :: hdf5file = "historical.h5"   !< HDF5 file name
    integer, parameter :: nt = 4
    integer, parameter :: nx = 5 
    real(gtm_real), dimension(nt, nx) :: flow_mesh, area_mesh
    real(gtm_real), dimension(nt-1, nx-1) ::  flow_volume_change, area_volume_change
    real(gtm_real), dimension(693,nx) :: prev_flow_cell
    real(gtm_real) :: dt, dx
    integer :: up_comp, down_comp
    real(gtm_real) :: total_flow_volume_change, total_area_volume_change
    real(gtm_real) :: avga_volume_change, diff
    integer :: i, j, t
    
    open(debug_unit, file = "gtm_debug_unit.txt")                   !< output text file
    
    call dsm2_hdf_data(hdf5file)
    
   
    do t = 2, 20 !n_time
        write(*,*) t
        do i = 1, n_segm
           dt = orig_time_interval/(nt-1.)
           dx = segm(i)%length/(nx-1.)
           up_comp = segm(i)%up_comppt
           down_comp = segm(i)%down_comppt
           avga_volume_change = (hydro_avga(up_comp,t)-hydro_avga(up_comp,t-1)) * segm(i)%length
           if (t==2) then
               do j = 1, nx
                   prev_flow_cell(i,j) = hydro_flow(up_comp,t-1) + (hydro_flow(down_comp,t-1)-hydro_flow(up_comp,t-1))*(j-1)/(nx-1)
               end do    
           end if
           call interp_flow_area(flow_mesh, area_mesh, flow_volume_change, area_volume_change,   &
                                 segm(i)%chan_no, segm(i)%up_distance, dx, dt, nt, nx,           &
                                 hydro_flow(up_comp,t-1), hydro_flow(down_comp,t-1),             &
                                 hydro_flow(up_comp,t), hydro_flow(down_comp,t),                 &
                                 hydro_ws(up_comp,t-1), hydro_ws(down_comp,t-1),                 &
                                 hydro_ws(up_comp,t), hydro_ws(down_comp,t),                     &
                                 prev_flow_cell(i,:))
           call calc_total_volume_change(total_flow_volume_change, nt-1, nx-1, flow_volume_change)
           call calc_total_volume_change(total_area_volume_change, nt-1, nx-1, area_volume_change)
           diff = (total_flow_volume_change-avga_volume_change)/avga_volume_change * 100
           prev_flow_cell(i,:) = flow_mesh(nt,:)
           if (diff.gt.ten*two)then   !todo::I tried to figure out when and why there are huge inconsistency of volume change from interpolation and average area
               write(debug_unit,'(2a8,5a20)') "t","chan_no","segm_length","flow_vol_change","area_vol_change","avga_vol_change","% difference"           
               write(debug_unit,'(2i8,5f20.5)') t, segm(i)%chan_no, segm(i)%length, total_flow_volume_change,total_area_volume_change, avga_volume_change, diff
               write(debug_unit,'(a10)') "flow mesh"
               call print_mass_balance_check(debug_unit, nt, nx, flow_mesh, flow_volume_change) 
               write(debug_unit,'(a10)') "area mesh"
               call print_mass_balance_check(debug_unit, nt, nx, area_mesh, area_volume_change)
               !write(debug_unit,*) prev_flow_cell(1),prev_flow_cell(2),prev_flow_cell(3),prev_flow_cell(4),prev_flow_cell(5)               
               write(debug_unit,*) ""
           end if
        end do
    end do    
    call deallocate_hydro_ts()
    close(debug_unit)
end program
