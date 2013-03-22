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
        implicit none
        ! spot checking interpolated results with pre-calculated numbers
        call test_interp_spot_checking()
        ! print out mesh for different sizes to debug_unit text file 
        call test_interp_flow_area(4, 5, dble(5), dble(1250.))     ! inputs: nt, nx, dt, dx
        call test_interp_flow_area(6, 17, dble(3), dble(312.5))    ! inputs: nt, nx, dt, dx
        call test_interp_flow_area(16, 33, dble(1), dble(156.25))  ! inputs: nt, nx, dt, dx
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
        real(gtm_real), dimension(4,5) :: flow_mesh, area_mesh, new_flow_mesh
        real(gtm_real), dimension(4-1,5-1) :: flow_volume_change
        real(gtm_real), dimension(4-1,5-1) :: area_volume_change
        real(gtm_real), dimension(4-1,5-1) :: new_flow_volume_change
        real(gtm_real) :: total_volume_change
        branch = 2
        nt = 4
        nx = 5
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
        
        call interp_flow_linear(dt, nt, nx, flow_a, flow_b, flow_c, flow_d, flow_mesh, flow_volume_change)
        call assertEquals (flow_mesh(3,1), dble(25066.96094), weakest_eps, "problem in interp_flow_linear")
        call assertEquals (flow_mesh(2,3), dble(25225.24251), weakest_eps, "problem in interp_flow_linear")
        call assertEquals (flow_mesh(3,4), dble(25217.88916), weakest_eps, "problem in interp_flow_linear")
        call assertEquals (flow_mesh(3,3), dble(25167.57975), weakest_eps, "problem in interp_flow_linear")
        
        call interp_flow_linear_with_target(dt, nt, nx, flow_a, flow_b, flow_c, flow_d, avga, flow_mesh, flow_volume_change)
        call assertEquals (flow_mesh(3,1), dble(25067.75608), weakest_eps, "problem in interp_flow_linear_with_target")
        call assertEquals (flow_mesh(2,1), dble(25113.34809), weakest_eps, "problem in interp_flow_linear_with_target")
        call assertEquals (flow_mesh(3,5), dble(25267.40343), weakest_eps, "problem in interp_flow_linear_with_target")
        call assertEquals (flow_mesh(2,5), dble(25337.13693), weakest_eps, "problem in interp_flow_linear_with_target")
        write(debug_unit,'(/a90)') "interp_flow_linear_with_mass balance target_from average area:                            "
        call print_mass_balance_check(debug_unit, nt, nx, flow_mesh, flow_volume_change)
        
        call interp_area(dx, nt, nx, area_a, area_b, area_c, area_d, flow_volume_change, area_mesh, area_volume_change)
        call assertEquals (area_mesh(3,1), dble(7749.494792), weakest_eps, "problem in interp_area")
        call assertEquals (area_mesh(2,3), dble(6979.528402), weakest_eps, "problem in interp_area")
        call assertEquals (area_mesh(3,4), dble(6571.40202), weakest_eps, "problem in interp_area")
        call assertEquals (area_mesh(3,3), dble(6968.719157), weakest_eps, "problem in interp_area")
        write(debug_unit,'(/a90)') "Area Interpolation from linear flow interpolation with mass balance target:               "
        call print_mass_balance_check(debug_unit, nt, nx, area_mesh, area_volume_change)        
        
        call interp_flow_linear(dt, nt, nx, flow_a, flow_b, flow_c, flow_d, flow_mesh, flow_volume_change)      
        call interp_area_byCxArea(branch, up_x, dx, nt, nx, ws_a, ws_b, ws_c, ws_d, flow_volume_change, area_mesh, area_volume_change)
        call assertEquals (area_mesh(3,1), dble(7748.17759), weakest_eps, "problem in interp_area_byCxArea")
        call assertEquals (area_mesh(2,3), dble(6569.20053), weakest_eps, "problem in interp_area_byCxArea")
        call assertEquals (area_mesh(3,4), dble(6368.82653), weakest_eps, "problem in interp_area_byCxArea")
        call assertEquals (area_mesh(3,3), dble(6556.31834), weakest_eps, "problem in interp_area_byCxArea")
        write(debug_unit,'(/a90)') "Area from Water Surface Interpolation:                                                    "
        call print_mass_balance_check(debug_unit, nt, nx, area_mesh, area_volume_change)
        
        
        call interp_flow_from_area_theta(dt, nt, nx, flow_a, flow_b, flow_c, flow_d, area_volume_change, new_flow_mesh, new_flow_volume_change)
        call assertEquals (new_flow_mesh(3,1), dble(25066.96094), weakest_eps, "problem in interp_flow_from_area_theta")
        call assertEquals (new_flow_mesh(2,3), dble(25219.96664), weakest_eps, "problem in interp_flow_from_area_theta")
        call assertEquals (new_flow_mesh(3,4), dble(25216.99209), weakest_eps, "problem in interp_flow_from_area_theta")
        call assertEquals (new_flow_mesh(3,3), dble(25166.34508), weakest_eps, "problem in interp_flow_from_area_theta")        
        write(debug_unit,'(/a90)') "New Flow from Water Surface Interpolation - theta average:                                "
        call print_mass_balance_check(debug_unit, nt, nx, new_flow_mesh, new_flow_volume_change)  
            
        call interp_flow_from_area_inst(dt, nt, nx, flow_a, flow_b, flow_c, flow_d, area_volume_change, new_flow_mesh, new_flow_volume_change)
        call assertEquals (new_flow_mesh(3,1), dble(25066.96094), weakest_eps, "problem in interp_flow_from_area_inst")
        call assertEquals (new_flow_mesh(2,3), dble(25231.89136), weakest_eps, "problem in interp_flow_from_area_inst")
        call assertEquals (new_flow_mesh(3,4), dble(25228.27663), weakest_eps, "problem in interp_flow_from_area_inst")
        call assertEquals (new_flow_mesh(3,3), dble(25174.54297), weakest_eps, "problem in interp_flow_from_area_inst")
        write(debug_unit,'(/a90)') "New Flow from Water Surface Interpolation - instantaneous:                                "
        call print_mass_balance_check(debug_unit, nt, nx, new_flow_mesh, new_flow_volume_change)   
                
        call calc_total_volume_change(nt-1, nx-1, new_flow_volume_change, total_volume_change)
        !call assertEquals (total_volume_change, dble(-189452.439905135), weakest_eps, "problem in calc_total_volume_change")   !todo:: weird, not sure why dble(-189452.439905135) return me -189452.4375

        return
    end subroutine
         
     
    !> Test function interp_flow_area() for different size of grid
    subroutine test_interp_flow_area(nt, nx, dt, dx)
        use interpolation  
        implicit none      
        integer, intent(in) :: nt          !< number of points in time
        integer, intent(in) :: nx          !< number of points in space
        real(gtm_real), intent(in) :: dt   !< dt
        real(gtm_real), intent(in) :: dx   !< dx
        integer :: branch
        real(gtm_real) :: up_x
        real(gtm_real) :: flow_a, flow_b, flow_c, flow_d, ws_a, ws_b, ws_c, ws_d
        real(gtm_real), dimension(nt,nx) :: flow_mesh, area_mesh, new_flow_mesh              
        real(gtm_real), dimension(nt-1,nx-1) :: flow_volume_change, area_volume_change, new_flow_volume_change    
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
        call interp_flow_area(branch, up_x, dx, dt, nt, nx,       &
                              flow_a, flow_b, flow_c, flow_d,     &
                              ws_a, ws_b, ws_c, ws_d,             &
                              flow_mesh, area_mesh,               &
                              flow_volume_change, area_volume_change)
        write(debug_unit,'(/a17,i3,a5,i3,a5,f4.1,a8,f10.3,a2)') "Flow mesh for nt=", nt,", nx=", nx,", dt=", dt, "min, dx=", dx,"ft"
        call print_mass_balance_check(debug_unit, nt, nx, flow_mesh, flow_volume_change)            
        write(debug_unit,'(/a17,i3,a5,i3,a5,f4.1,a8,f10.3,a2)') "Area mesh for nt=", nt,", nx=", nx,", dt=", dt, "min, dx=", dx,"ft"
        call print_mass_balance_check(debug_unit, nt, nx, area_mesh, area_volume_change)                     
        return
    end subroutine    
	
end module   
