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

!> Routines to test interpolation schemes
!>@ingroup test_gtm_core
module ut_hydro_data_interp

    use fruit
   
    contains
   
    !> Test all interpolation functions
    subroutine test_interpolation()
        use common_variables
        use common_xsect
        implicit none
        ! spot checking interpolated results with pre-calculated numbers
        call test_interp_spot_checking
        ! print out mesh for different sizes to debug_unit text file 
        call test_interp_flow_area(4, 4, dble(5), dble(1250.))     ! inputs: nt, nx, dt, dx
        !call test_interp_flow_area(6, 17, dble(3), dble(312.5))    ! inputs: nt, nx, dt, dx
        !call test_interp_flow_area(16, 33, dble(1), dble(156.25))  ! inputs: nt, nx, dt, dx
        call deallocate_virt_xsect
        call deallocate_channel
        call deallocate_reservoir
        call deallocate_qext
        call deallocate_comp_pt
        call deallocate_segment
        deallocate(conn)
        return
    end subroutine
   
    !> Test interpolation functions by spot checking
    subroutine test_interp_spot_checking()
        use interpolation  
        implicit none
        integer :: branch, nt, nx
        real(gtm_real) :: avga, up_x, dx, dt
        real(gtm_real) :: flow_a, flow_b, flow_c, flow_d
        real(gtm_real) :: area_a, area_b, area_c, area_d
        real(gtm_real) :: ws_a, ws_b, ws_c, ws_d
        real(gtm_real), allocatable :: flow_volume_change(:,:)
        real(gtm_real), allocatable :: area_volume_change(:,:)
        real(gtm_real), allocatable :: new_flow_volume_change(:,:)
        real(gtm_real), allocatable :: prev_flow_cell_lo(:)
        real(gtm_real), allocatable :: prev_flow_cell_hi(:)
        real(gtm_real), allocatable :: flow_mesh_lo(:,:), flow_mesh_hi(:,:)
        real(gtm_real), allocatable :: area_mesh_lo(:,:), area_mesh_hi(:,:)
        real(gtm_real), allocatable :: new_flow_mesh_lo(:,:), new_flow_mesh_hi(:,:)
        real(gtm_real) :: total_volume_change
        integer :: start_c, ncell
                
        branch = 2
        nt = 4
        nx = 4
        dt = 5
        up_x = dble(5000.)
        dx = dble(1250.)
        avga = dble(-179682.615)
        flow_a = dble(25156.55469)
        flow_b = dble(25409.25586)
        flow_c = dble(25022.16406)
        flow_d = dble(25197.66992)
        area_a = dble(7771.113281)
        area_b = dble(6209.562012)
        area_c = dble(7738.685547)
        area_d = dble(6170.749023)
        ws_a = dble(2.355571)
        ws_b = dble(1.307376)
        ws_c = dble(2.302306)
        ws_d = dble(1.252751)
        ncell = 4
        start_c = 1
        allocate(flow_mesh_lo(nt,ncell), flow_mesh_hi(nt,ncell))
        allocate(area_mesh_lo(nt,ncell), area_mesh_hi(nt,ncell))
        allocate(new_flow_mesh_lo(nt,ncell), new_flow_mesh_hi(nt,ncell))
        allocate(prev_flow_cell_lo(ncell))
        allocate(prev_flow_cell_hi(ncell))
        allocate(flow_volume_change(nt-1,ncell))
        allocate(area_volume_change(nt-1,ncell))
        allocate(new_flow_volume_change(nt-1,ncell))
        prev_flow_cell_lo = (/25156.55469, 25219.72998, 25282.90527, 25346.08057/)
        prev_flow_cell_hi = (/25219.72998, 25282.90527, 25346.08057, 25409.25586/)
        
        call interp_flow_linear(flow_mesh_lo, flow_mesh_hi, flow_volume_change, ncell, start_c, nx, dt, nt, flow_a, flow_b, flow_c, flow_d)
        call assertEquals (flow_mesh_lo(3,1), dble(25066.96094), weakest_eps, "problem in interp_flow_linear")
        call assertEquals (flow_mesh_lo(2,3), dble(25225.24251), weakest_eps, "problem in interp_flow_linear")
        call assertEquals (flow_mesh_lo(3,4), dble(25217.88916), weakest_eps, "problem in interp_flow_linear")
        call assertEquals (flow_mesh_lo(3,3), dble(25167.57975), weakest_eps, "problem in interp_flow_linear")

        call interp_area_byCxArea(area_mesh_lo, area_mesh_hi, area_volume_change, branch, up_x, dx,            &
                                  ncell, start_c, nx, dt, nt, ws_a, ws_b, ws_c, ws_d, flow_volume_change)        
        call assertEquals (area_mesh_lo(3,1), dble(271.318706144350), weakest_eps, "problem in interp_area_byCxArea")
        call assertEquals (area_mesh_lo(2,3), dble(655.317367643859), weakest_eps, "problem in interp_area_byCxArea")
        call assertEquals (area_mesh_lo(3,4), dble(970.497544141241), weakest_eps, "problem in interp_area_byCxArea")
        call assertEquals (area_mesh_lo(3,3), dble(652.512302996901), weakest_eps, "problem in interp_area_byCxArea")
        write(debug_unit,'(/a90)') "Area from Water Surface Interpolation:                                                    "
        call print_mass_balance_check(debug_unit, nt, nx, area_mesh_lo, area_volume_change)
        call print_mass_balance_check(debug_unit, nt, nx, area_mesh_hi, area_volume_change)

        call interp_flow_from_area_theta(new_flow_mesh_lo, new_flow_mesh_hi, new_flow_volume_change,ncell, start_c, dt, nt, nx,  &
              flow_a, flow_b, flow_c, flow_d, area_volume_change, prev_flow_cell_lo, prev_flow_cell_hi)      
        call assertEquals (new_flow_mesh_hi(2,4), dble(25038.5330755125), weakest_eps, "problem in interp_flow_from_area_theta new_flow_mesh_hi(2,4)")
        call assertEquals (new_flow_mesh_hi(3,4), dble(25086.3840707929), weakest_eps, "problem in interp_flow_from_area_theta new_flow_mesh_hi(3,4)")
        call assertEquals (new_flow_mesh_hi(4,4), dble(25197.6699218750), weakest_eps, "problem in interp_flow_from_area_theta new_flow_mesh_hi(4,4)")
        call assertEquals (new_flow_mesh_lo(4,4), dble(25122.3361147159), weakest_eps, "problem in interp_flow_from_area_theta new_flow_mesh_lo(4,4)")        
        call assertEquals (new_flow_mesh_hi(4,3), dble(25122.3361147159), weakest_eps, "problem in interp_flow_from_area_theta new_flow_mesh_hi(4,3)")        
        write(debug_unit,'(/a90)') "New Flow from Water Surface Interpolation - theta average:                                "
        call print_mass_balance_check(debug_unit, nt, nx, new_flow_mesh_lo, new_flow_volume_change)       
                                      
        deallocate(flow_mesh_lo, flow_mesh_hi)
        deallocate(area_mesh_lo, area_mesh_hi)
        deallocate(prev_flow_cell_lo)
        deallocate(prev_flow_cell_hi)
        deallocate(flow_volume_change)
        deallocate(area_volume_change)
        deallocate(new_flow_volume_change)

        return
    end subroutine
         
     
    !> Test function interp_flow_area() for different size of grid
    subroutine test_interp_flow_area(nt, nx, dt, dx)
        use interpolation  
        implicit none      
        integer, intent(in) :: nt          !< number of points in time
        integer, intent(in) :: nx          !< number of points in space
        integer :: ncell, start_c
        real(gtm_real), intent(in) :: dt   !< dt
        real(gtm_real), intent(in) :: dx   !< dx
        integer :: branch
        real(gtm_real) :: up_x
        real(gtm_real) :: flow_a, flow_b, flow_c, flow_d, ws_a, ws_b, ws_c, ws_d
        real(gtm_real), allocatable :: flow_volume_change(:,:)
        real(gtm_real), allocatable :: area_volume_change(:,:)
        real(gtm_real), allocatable :: new_flow_volume_change(:,:)
        real(gtm_real), allocatable :: prev_flow_cell_lo(:)
        real(gtm_real), allocatable :: prev_flow_cell_hi(:)
        real(gtm_real), allocatable :: flow_mesh_lo(:,:), flow_mesh_hi(:,:)
        real(gtm_real), allocatable :: area_mesh_lo(:,:), area_mesh_hi(:,:)
        real(gtm_real) :: total 
        branch = 2
        up_x = dble(5000.)
        flow_a = dble(25156.55469)
        flow_b = dble(25409.25586)
        flow_c = dble(25022.16406)
        flow_d = dble(25197.66992)
        ws_a = dble(2.355571)
        ws_b = dble(1.307376)
        ws_c = dble(2.302306)
        ws_d = dble(1.252751)
        ncell = 4
        start_c = 1

        allocate(flow_mesh_lo(nt,ncell), flow_mesh_hi(nt,ncell))
        allocate(area_mesh_lo(nt,ncell), area_mesh_hi(nt,ncell))
        allocate(prev_flow_cell_lo(ncell))
        allocate(prev_flow_cell_hi(ncell))
        allocate(flow_volume_change(nt-1,ncell))
        allocate(area_volume_change(nt-1,ncell))
        allocate(new_flow_volume_change(nt-1,ncell))
        prev_flow_cell_lo = (/25156.55469, 25219.72998, 25282.90527, 25346.08057/)
        prev_flow_cell_hi = (/25219.72998, 25282.90527, 25346.08057, 25409.25586/)
        
        call interp_flow_area(flow_mesh_lo, flow_mesh_hi, area_mesh_lo, area_mesh_hi, flow_volume_change, area_volume_change,       &
                              ncell, start_c, branch, up_x, dx, dt, nt, nx, flow_a, flow_b, flow_c, flow_d, ws_a, ws_b, ws_c, ws_d, &
                              prev_flow_cell_lo, prev_flow_cell_hi)
        write(debug_unit,'(/a17,i3,a5,i3,a5,f4.1,a8,f10.3,a2)') "Flow mesh for nt=", nt,", nx=", nx,", dt=", dt, "min, dx=", dx,"ft"
        call print_mass_balance_check(debug_unit, nt, nx, flow_mesh_lo, flow_volume_change)            
        write(debug_unit,'(/a17,i3,a5,i3,a5,f4.1,a8,f10.3,a2)') "Area mesh for nt=", nt,", nx=", nx,", dt=", dt, "min, dx=", dx,"ft"
        call print_mass_balance_check(debug_unit, nt, nx, area_mesh_lo, area_volume_change)               
        
        deallocate(flow_mesh_lo, flow_mesh_hi)
        deallocate(area_mesh_lo, area_mesh_hi)
        deallocate(prev_flow_cell_lo)
        deallocate(prev_flow_cell_hi)
        deallocate(flow_volume_change)
        deallocate(area_volume_change)
        deallocate(new_flow_volume_change)      
        return
    end subroutine    
	
end module   
