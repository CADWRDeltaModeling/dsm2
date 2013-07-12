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

!> This module contains functions used for interpolation (flow, area, or area 
!> calculated from CxArea()). Those functions return interpolated mesh and volume
!> changes among cells.
!>@ingroup gtm_core
module interpolation

    use gtm_precision
    use error_handling
    use gtm_logging
    use common_variables
   
    contains
   
   !! interpolation mesh grid with four given points
   !! For example, space divided into four and time divided into three (*: given, nx=5, nt=4)
   !! Tii stands for volume change for each cell.
   !!    1      2      3      4      nx
   !! 1  a*----c12----c13----c14-----b*  t-1       -----------------------------
   !!    |      |      |      |      |             |      |      |      |      |
   !!    |      |      |      |      |             |  T11 |  T12 |  T13 |  T14 |
   !! 2 c21----c22----c23----c24----c25  t-2t/3    -----------------------------
   !!    |      |      |      |      |             |      |      |      |      |
   !!    |      |      |      |      |             |  T21 |  T22 |  T23 |  T24 |
   !! 3 c31----c32----c33----c34----c35  t-1t/3    -----------------------------
   !!    |      |      |      |      |             |      |      |      |      |
   !!    |      |      |      |      |             |  T31 |  T32 |  T33 |  T34 | 
   !! nt c*----c42----c43----c44-----d*  t         -----------------------------          
   !! <-- upstream               downstream -->
   
   !> Linear flow interpolation with four given points
    subroutine interp_flow_linear(mesh, volume_change,    & 
                                  dt, nt, nx,             &
                                  a, b, c, d )
        implicit none
        real(gtm_real), intent(in) :: dt                                   !< finer time step (in minutes)
        integer, intent(in) :: nt                                          !< nt: number of points in time
        integer, intent(in) :: nx                                          !< nx: number of points in space
        real(gtm_real), intent(in) :: a, b, c, d                           !< input four corner points
        real(gtm_real), dimension(nt,nx), intent(out) :: mesh              !< interpolated mesh
        real(gtm_real), dimension(nt-1,nx-1), intent(out) :: volume_change !< volume change for each cell 
        real(gtm_real) :: total_volume_change, factor                      ! local variable
        integer :: i, j                                                    ! local variable      
        mesh = LARGEREAL
        volume_change = LARGEREAL
        mesh(1,1) = a
        mesh(1,nx) = b
        mesh(nt,1) = c
        mesh(nt,nx) = d
        do i = 2, nt-1
            factor = (i-one)/(nt-one)
            mesh(i,1) = a + factor*(c-a)
            mesh(i,nx) = b + factor*(d-b)
        end do
        do i = 1, nt
            do j = 2, nx-1
                factor = (j-one)/(nx-one)
                mesh(i,j) = mesh(i,1) + factor*(mesh(i,nx)-mesh(i,1))
            end do
        end do        
        ! calculate volume change for mesh cells
        do i = 1, nt-1
            do j = 1, nx-1
                !volume_change(i,j) = (mesh(i+1,j)-mesh(i+1,j+1))*dt*sixty        ! todo:: once we decide to use inst flow or theta average flow, this should be cleaned up. 
                volume_change(i,j) = ((one-hydro_theta)*mesh(i,j)+hydro_theta*mesh(i+1,j) &
                                    -(one-hydro_theta)*mesh(i,j+1)-hydro_theta*mesh(i+1,j+1))*dt*sixty ! theta average
            end do
        end do           
        return
    end subroutine
       
   !> Linear flow interpolation with four given points and total mass balance target
    subroutine interp_flow_linear_with_target(mesh, volume_change,       &    ! todo:: if we decide to go for CxArea(), we may get rid of this function.
                                              dt, nt, nx,                &
                                              a, b, c, d,                &
                                              mass_balance_target)
        implicit none
        real(gtm_real), intent(in) :: dt                                   !< finer time step (in minutes)
        integer, intent(in) :: nt                                          !< nt: number of points in time
        integer, intent(in) :: nx                                          !< nx: number of points in space
        real(gtm_real), intent(in) :: a, b, c, d, mass_balance_target      !< input four corner points and mass balance target (calculated from avg_a*segment_length)
        real(gtm_real), dimension(nt,nx), intent(out) :: mesh              !< interpolated mesh
        real(gtm_real), dimension(nt-1,nx-1), intent(out) :: volume_change !< volume change for each cell 
        real(gtm_real), dimension(nt) :: subtotal_volume_change            ! local variable to check mass balance (sub total in time)
        real(gtm_real) :: total_volume_change, factor                      ! local variable
        integer :: i, j                                                    ! local variable      
        mesh = LARGEREAL
        volume_change = LARGEREAL
        mesh(1,1) = a
        mesh(1,nx) = b
        mesh(nt,1) = c
        mesh(nt,nx) = d
        subtotal_volume_change(nt) = (c-d)*dt*sixty
        total_volume_change = subtotal_volume_change(nt)
        ! interpolate in time first. It is easier to get mass balance right.
        do i = 1, nt-3
            mesh(nt-i,1) = half*(a+b+(c-a)*(nt-1-i)/(nt-1)+(d-b)*(nt-1-i)/(nt-1)+mass_balance_target/(nt-1)/dt/sixty)
            mesh(nt-i,nx) = half*(a+b+(c-a)*(nt-1-i)/(nt-1)+(d-b)*(nt-1-i)/(nt-1)-mass_balance_target/(nt-1)/dt/sixty)
            subtotal_volume_change(nt-i) = (mesh(nt-i,1)-mesh(nt-i,nx))*dt*sixty
            total_volume_change = total_volume_change + subtotal_volume_change(nt-i)
        end do   
        mesh(nt-2,1) = half*(a+b+(c-a)*(nt-1-i)/(nt-1)+(d-b)*(nt-1-i)/(nt-1)+ &
                      (mass_balance_target-total_volume_change)/dt/sixty)
        mesh(nt-2,nx) = half*(a+b+(c-a)*(nt-1-i)/(nt-1)+(d-b)*(nt-1-i)/(nt-1)- &
                        (mass_balance_target-total_volume_change)/dt/sixty)
        ! fill space mesh by linear interpolation
        do i = 1, nt
            do j = 2, nx-1
                factor = (j-one)/(nx-one)
                mesh(i,j) = mesh(i,1) + factor*(mesh(i,nx)-mesh(i,1))
            end do
        end do
        ! calculate volume change for mesh cells
        do i = 1, nt-1
            do j = 1, nx-1
                !volume_change(i,j) = (mesh(i+1,j)-mesh(i+1,j+1))*dt*sixty   ! todo:: once we decide to use inst flow or theta average flow, this should be cleaned up. 
                volume_change(i,j) = ((one-hydro_theta)*mesh(i,j)+hydro_theta*mesh(i+1,j)- &
                                     (one-hydro_theta)*mesh(i,j+1)-hydro_theta*mesh(i+1,j+1))*dt*sixty ! theta average
            end do
        end do           
        return
    end subroutine
   
   !> Linear area interpolation with four given points and mass balance target from flow interpolation
    subroutine interp_area_from_flow_vol_change(area_mesh,                 &
                                                area_volume_change,        &                      
                                                branch, up_x, dx, nt, nx,  &
                                                ws_a, ws_b, ws_c, ws_d,    &
                                                mass_balance_target)
        use common_xsect 
        implicit none
        integer, intent(in) :: branch                                           !< hydro channel number (required by CxArea())
        real(gtm_real), intent (in) :: up_x                                     !< upstream point distance (required for CxArea())  
        real(gtm_real), intent(in) :: dx                                        !< finer cell size( in feet)
        integer, intent(in) :: nt                                               !< nt: number of points in time
        integer, intent(in) :: nx                                               !< nx: number of points in space
        real(gtm_real), intent(in) :: ws_a, ws_b, ws_c, ws_d                    !< input four corner points (water surface)
        real(gtm_real), dimension(nt-1,nx-1), intent(in) :: mass_balance_target !< mass balance from flow interpolation
        real(gtm_real), dimension(nt-1,nx-1), intent(out) :: area_volume_change !< volume change for each cell from area interpolation
        real(gtm_real), dimension(nt,nx), intent(out) :: area_mesh              !< interpolated area mesh         
        real(gtm_real), dimension(nt) :: subtotal_volume_change                 ! local variable to check mass balance (sub total in time)
        real(gtm_real) :: total_volume_change, factor                           ! local variable
        real(gtm_real) :: a, b, c, d                                            ! local variable
        integer :: i, j                                                         ! local variable      
        area_mesh = LARGEREAL
        area_volume_change = LARGEREAL
        call CxArea(a, up_x, ws_a, branch)
        call CxArea(b, up_x+(nx-one)*dx, ws_b, branch)
        call CxArea(c, up_x, ws_c, branch)
        call CxArea(d, up_x+(nx-one)*dx, ws_d, branch)        
        area_mesh(1,1) = a
        area_mesh(1,nx) = b
        area_mesh(nt,1) = c
        area_mesh(nt,nx) = d
        ! initialize mesh boundary by linear interpolation. 
        ! todo:: shall be taken from adjacent segement and previous time steps
        do i = 2, nx-1
             area_mesh(1,i) = a + (i-one)/(nx-one)*(b-a)
        end do
        do i = 2, nt-1    
             area_mesh(i,1) = a + (i-one)/(nt-one)*(c-a)
        end do
        ! fill the mesh to satisfy mass balance for each cell
        do i = 2, nt
            do j = 2, nx
                area_mesh(i,j) = two*mass_balance_target(i-1,j-1)/dx + area_mesh(i-1,j-1) + area_mesh(i-1,j) - area_mesh(i,j-1)
            end do
        end do   
        area_mesh(nt,nx) = d    
        ! force the last cell to conserve the total mass because last cell has extra freedom
        area_mesh(nt,nx-1) = (mass_balance_target(nt-1,nx-1) + mass_balance_target(nt-1,nx-2))/dx    &
                             + half*(area_mesh(nt-1,nx-2)+area_mesh(nt-1,nx-1)+area_mesh(nt-1,nx-1)  &
                             + area_mesh(nt-1,nx)-area_mesh(nt,nx-2)-area_mesh(nt,nx))
        ! to make last row conserve mass for each cell, re-adjust the row before
        area_mesh(nt-1,nx-1) = area_mesh(nt,nx-2) + area_mesh(nt,nx-1) - area_mesh(nt-1,nx-2)        &
                               - two*mass_balance_target(nt-1,nx-2)/dx        
        area_mesh(nt-1,nx) = area_mesh(nt,nx-1) + area_mesh(nt,nx) - area_mesh(nt-1,nx-1)            &
                             - two*mass_balance_target(nt-1,nx-1)/dx 
        do i = 2, nx
            do j  = 2, nt
                area_volume_change(j-1,i-1) = half*(area_mesh(j,i)+area_mesh(j,i-1)-area_mesh(j-1,i)-area_mesh(j-1,i-1))*dx
            end do     
        end do        
        return   
    end subroutine
    
   !> Area calculated from interpolation of four given water surface elevations
    subroutine interp_area_byCxArea(mesh, volume_change,                    &    
                                    branch, up_x, dx, nt, nx,               &  
                                    a, b, c, d,                             &
                                    mass_balance_from_flow)
        use common_xsect    
        implicit none
        integer, intent(in) :: branch                                              !< hydro channel number (required by CxArea())
        real(gtm_real), intent (in) :: up_x                                        !< upstream point distance (required for CxArea())
        real(gtm_real), intent(in) :: dx                                           !< finer cell size (in feet)
        integer, intent(in) :: nt                                                  !< nt: number of points in time
        integer, intent(in) :: nx                                                  !< nx: number of points in space
        real(gtm_real), intent(in) :: a, b, c, d                                   !< input four corner points (water surface elevation)
        real(gtm_real), dimension(nt-1,nx-1), intent(in) :: mass_balance_from_flow !< mass balance from flow interpolation (to calculate factors to interpolate water surface in time)
        real(gtm_real), dimension(nt,nx), intent(out) :: mesh                      !< interpolated area mesh
        real(gtm_real), dimension(nt-1,nx-1), intent(out) :: volume_change         !< volume change for each cell 
        real(gtm_real), dimension(nt,nx) :: ws                                     ! interpolated water surface mesh
        real(gtm_real), dimension(nt) :: subtotal_volume_change                    ! local variable to check mass balance (sub total in time)
        real(gtm_real) :: total_volume_change, factor, sub_flow_vol                ! local variable
        real(gtm_real), dimension(nt-1) :: ratio                                   ! local variable
        integer :: i, j, OK                                                        ! local variable  
        OK = 0             ! use linear ratio, not distribute by flow
        mesh = LARGEREAL                                                        
        ws = LARGEREAL
        volume_change = LARGEREAL
        ws(1,1) = a
        ws(1,nx) = b
        ws(nt,1) = c
        ws(nt,nx) = d
        ! calculate the ratio to distribute water surface in time from flow interpolation
        sub_flow_vol = zero
        do i = 1, nt-1
            sub_flow_vol = sub_flow_vol + mass_balance_from_flow(i,1)
        end do
        if ((sub_flow_vol.ne.zero).and.((b-a)*(d-c).gt.zero)) then
            do i = 1, nt-1
                ratio(i) = zero
                do j = 1, i
                    ratio(i) = ratio(i) + mass_balance_from_flow(j,1)/sub_flow_vol
                end do    
                if ((abs(ratio(i))>one).or.(ratio(i)<zero)) OK = 1
            end do
        else
            OK = 1 
        end if 
        ! if no mass change or flow in transition status, interpolate linearly.
        if (OK == 1) then
            do i = 1, nt-1
                ratio(i) = i/(nt-one)
            end do
        end if  
        
        ! apply flow mass balance as ratio to interpolate water surface in time
        do i = 2, nt-1
            ws(i,1) = a + ratio(i-1)*(c-a)
            ws(i,nx) = b + ratio(i-1)*(d-b)
        end do    
        ! linear interpolating water surface in space  
        do i = 1, nt 
            do j = 2, nx-1
                factor = (j-one)/(nx-one)
                ws(i,j) = ws(i,1) + factor*(ws(i,nx)-ws(i,1))
            end do
        end do   
  
        ! call CxArea to obtain area
        do j  = 1, nt
            call CxArea(mesh(j,1), up_x, ws(j,1), branch)
        end do       
        do i = 1, nx
            call CxArea(mesh(1,i), up_x+dx*(i-1.), ws(1,i), branch)
        end do
        do i = 2, nx
            do j  = 2, nt
                call CxArea(mesh(j,i), up_x+dx*(i-1.), ws(j,i), branch)
                volume_change(j-1,i-1) = half*(mesh(j,i)+mesh(j,i-1)-mesh(j-1,i)-mesh(j-1,i-1))*dx
            end do     
        end do     
        return   
    end subroutine    
    
   !> Flow interpolation based on area interpolation from CxArea() while theta average is used for calculation
   !> This pushes mass balance inconsistency to T24 and T31.
    subroutine interp_flow_from_area_theta_m1(mesh, volume_change,         &
                                              dt, nt, nx,                  &
                                              a, b, c, d,                  &
                                              mass_balance_target,         & 
                                              prev_flow_cell)
        implicit none
        real(gtm_real), intent(in) :: dt                                        !< finer time step (in minutes)
        integer, intent(in) :: nt                                               !< nt: number of points in time
        integer, intent(in) :: nx                                               !< nx: number of points in space
        real(gtm_real), intent(in) :: a, b, c, d                                !< input four corner points
        real(gtm_real), dimension(nt-1,nx-1), intent(in) :: mass_balance_target !< mass balance from flow interpolation
        real(gtm_real), dimension(nx), intent(in) :: prev_flow_cell             !< previous time step interpolated flow cells
        real(gtm_real), dimension(nt,nx), intent(out) :: mesh                   !< interpolated mesh
        real(gtm_real), dimension(nt-1,nx-1), intent(out) :: volume_change      !< volume change for each cell 
        real(gtm_real) :: total_volume_change, factor                           ! local variables
        integer :: i, j                                                         ! local variables     
        mesh = LARGEREAL
        volume_change = LARGEREAL
        mesh(1,1) = a
        mesh(1,nx) = b
        mesh(nt,1) = c
        mesh(nt,nx) = d
        ! assigne initial row and interpolate boundary column linearly       
        do j = 2, nx-1
            mesh(1,j) = prev_flow_cell(j)
        end do
        do i = 2, nt-1
            factor = (i-one)/(nt-one)
            mesh(i,1) = mesh(1,1) + factor*(mesh(nt,1)-mesh(1,1))
        end do
        
        ! interpolate all cells initially (mass balance error will accumulate at the end)
        do i = 2, nt
            do j = 2, nx
            mesh(i,j) = ((one-hydro_theta)*mesh(i-1,j-1) + hydro_theta*mesh(i,j-1) -  & 
                        (one-hydro_theta)*mesh(i-1,j) - mass_balance_target(i-1,j-1)/dt/sixty)/hydro_theta
            end do
        end do   
        mesh(nt,nx) = d
        ! adjust one of last cell c35 to have total mass balanced
        mesh(nt-1,nx) = mesh(nt-1,nx-1) + (one-hydro_theta)*mesh(nt-2,nx-1) + hydro_theta*mesh(nt,nx-1) - &
                        (one-hydro_theta)*mesh(nt-2,nx) - hydro_theta*mesh(nt,nx) - (mass_balance_target(nt-2,nx-1)+ &
                        mass_balance_target(nt-1,nx-1))/dt/sixty
        ! adjust the other one of last cell c44 to make sure mass balanced at the end
        mesh(nt,nx-1) = ((one-hydro_theta)*mesh(nt-1,nx) + hydro_theta*mesh(nt,nx) - (one-hydro_theta)*mesh(nt-1,nx-1) + &
                        mass_balance_target(nt-1,nx-1)/dt/sixty)/hydro_theta
        
        ! calculate volume change for mesh cells
        do i = 1, nt-1
            do j = 1, nx-1
                volume_change(i,j) = ((one-hydro_theta)*mesh(i,j)+hydro_theta*mesh(i+1,j)- &
                                     (one-hydro_theta)*mesh(i,j+1)-hydro_theta*mesh(i+1,j+1))*dt*sixty ! theta average
            end do
        end do                           
        return
    end subroutine

   !> Flow interpolation based on area interpolation from CxArea() while theta average is used for calculation
   !> This pushes mass inconsistency to T21, T31 and T14.
    subroutine interp_flow_from_area_theta_m2(mesh, volume_change,         &
                                              dt, nt, nx,                  &
                                              a, b, c, d,                  &
                                              mass_balance_target,         &
                                              prev_flow_cell)
        implicit none
        real(gtm_real), intent(in) :: dt                                        !< finer time step (in minutes)
        integer, intent(in) :: nt                                               !< nt: number of points in time
        integer, intent(in) :: nx                                               !< nx: number of points in space
        real(gtm_real), intent(in) :: a, b, c, d                                !< input four corner points
        real(gtm_real), dimension(nt-1,nx-1), intent(in) :: mass_balance_target !< mass balance from flow interpolation
        real(gtm_real), dimension(nx), intent(in) :: prev_flow_cell             !< previous time step interpolated flow cells
        real(gtm_real), dimension(nt,nx), intent(out) :: mesh                   !< interpolated flow mesh
        real(gtm_real), dimension(nt-1,nx-1), intent(out) :: volume_change      !< volume change for each cell 
        real(gtm_real) :: total_volume_change, factor, vol_change_tmp           ! local variables
        integer :: i, j                                                         ! local variables     
        mesh = LARGEREAL
        volume_change = LARGEREAL
        mesh(1,1) = a
        mesh(1,nx) = b
        mesh(nt,1) = c
        mesh(nt,nx) = d
        ! assigne initial row and interpolate boundary column linearly       
        do j = 2, nx-1
            mesh(1,j) = prev_flow_cell(j)
        end do
        do i = 2, nt-1
            factor = (i-one)/(nt-one)
            mesh(i,1) = mesh(1,1) + factor*(mesh(nt,1)-mesh(1,1))
        end do
        
        ! interpolate all cells initially (mass balance error will accumulate at the end)
        do i = 2, nt
            do j = 2, nx
            mesh(i,j) = mesh(i,j-1) - (mass_balance_target(i-1,j-1)/dt/sixty  &
                       - (one-hydro_theta)*mesh(i-1,j-1)                      &
                       + (one-hydro_theta)*mesh(i-1,j))/hydro_theta
            end do
        end do   
        mesh(nt,nx) = d
        ! adjust cell c31-c34
        do j = nx-1, 1, -1
            mesh(nt-1,j) = (mass_balance_target(nt-1,j)/dt/sixty                &
                           - hydro_theta*mesh(nt,j) + hydro_theta*mesh(nt,j+1)     &
                           + (one-hydro_theta)*mesh(nt-1,j+1))/(one-hydro_theta)
        end do
        ! adjust cell c25
        mesh(nt-2,nx) = (hydro_theta*mesh(nt-1,nx-1) - hydro_theta*mesh(nt-1,nx)    &
                        + (one-hydro_theta)*mesh(nt-2,nx-1)                         &
                        - mass_balance_target(nt-2,nx-1)/dt/sixty)/(one-hydro_theta)

        ! adjust c31 to make sure total mass balance
        vol_change_tmp = ((one-hydro_theta)*mesh(nt-3,nx-1)+hydro_theta*mesh(nt-2,nx-1)-           &
                         (one-hydro_theta)*mesh(nt-3,nx)-hydro_theta*mesh(nt-2,nx))*dt*sixty
        mesh(nt-1,1) = (mass_balance_target(nt-1,1)                    &
                       + mass_balance_target(nt-2,1)                   &
                       + mass_balance_target(nt-3,nx-1)                &
                       - vol_change_tmp)/dt/sixty                      &
                       + mesh(nt-1,2) - (one-hydro_theta)*mesh(nt-2,1) & 
                       + (one-hydro_theta)*mesh(nt-2,2)                &
                       - hydro_theta*mesh(nt,1)                        &
                       + hydro_theta*mesh(nt,2)
        
        ! calculate volume change for mesh cells
        do i = 1, nt-1
            do j = 1, nx-1
                volume_change(i,j) = ((one-hydro_theta)*mesh(i,j)+hydro_theta*mesh(i+1,j)- &
                                     (one-hydro_theta)*mesh(i,j+1)-hydro_theta*mesh(i+1,j+1))*dt*sixty ! theta average
            end do
        end do                           
        return
    end subroutine
    
   !> Flow interpolation based on area interpolation from CxArea() 
   !> while instantaneous flow is directly used for calculation
    subroutine interp_flow_from_area_inst(mesh, volume_change,       &         ! todo:: once we decide to use inst flow or theta
                                          dt, nt, nx, a, b, c, d,    &         !        average flow, this should be cleaned up. 
                                          mass_balance_target)
        implicit none
        real(gtm_real), intent(in) :: dt                                        !< finer time step (in minutes)
        integer, intent(in) :: nt                                               !< nt: number of points in time
        integer, intent(in) :: nx                                               !< nx: number of points in space
        real(gtm_real), intent(in) :: a, b, c, d                                !< input four corner points
        real(gtm_real), dimension(nt-1,nx-1), intent(in) :: mass_balance_target !< mass balance from flow interpolation
        real(gtm_real), dimension(nt,nx), intent(out) :: mesh                   !< interpolated mesh
        real(gtm_real), dimension(nt-1,nx-1), intent(out) :: volume_change      !< volume change for each cell 
        real(gtm_real) :: total_volume_change, factor, diff                     ! local variable
        integer :: i, j                                                         ! local variable      
        mesh = LARGEREAL
        volume_change = LARGEREAL
        mesh(1,1) = a
        mesh(1,nx) = b
        mesh(nt,1) = c
        mesh(nt,nx) = d
        ! interpolate boundary row/column linearly
        do j = 2, nx-1
            factor = (j-one)/(nx-one)
            mesh(1,j) = mesh(1,1) + factor*(mesh(1,nx)-mesh(1,1))
        end do
        do i = 2, nt-1
            factor = (i-one)/(nt-one)
            mesh(i,1) = mesh(1,1) + factor*(mesh(nt,1)-mesh(1,1))
        end do
        
        ! interpolate all cells initially (mass balance error will accumulate at the end)
        do i = 2, nt
            do j = 2, nx
            mesh(i,j) = mesh(i,j-1) - mass_balance_target(i-1,j-1)/dt/sixty
            end do
        end do   
        mesh(nt,nx) = d
        ! adjust c44 to make mass balance at the last cell right
        do i = 2, nt
            mesh(i,nx-1) = mass_balance_target(i-1,nx-1)/dt/sixty + mesh(i,nx)
        end do            
        ! adjust c43 to push difference to the middle
        mesh(nt,nx-2) = (mass_balance_target(nt-1,nx-2)+mass_balance_target(nt-1,nx-1))/dt/sixty + mesh(nt,nx)
        mesh(nt,nx-1) = mesh(nt,nx-2) - mass_balance_target(nt-1,nx-2)/dt/sixty
        ! figure out the total difference to meet mass balance (this comes from the last row)
        diff = 0 
        do j = 1, nx-1
            diff = diff + mass_balance_target(nt-1,j) - (mesh(nt,j)-mesh(nt,j+1))*dt*sixty
        end do
        diff = diff / (nt-two)
        ! push difference to other cells (c23, c33)
        do i = 2, nt-1
            mesh(i,nx-2) = mesh(i,nx-3) - (mass_balance_target(i-1,nx-3)+diff)/dt/sixty
            mesh(i,nx-1) = mesh(i,nx-2) - mass_balance_target(i-1,nx-2)/dt/sixty
            mesh(i,nx) = mesh(i,nx-1) - mass_balance_target(i-1,nx-1)/dt/sixty
        end do
        ! calculate volume change for mesh cells
        do i = 1, nt-1
            do j = 1, nx-1
                volume_change(i,j) = (mesh(i+1,j)-mesh(i+1,j+1))*dt*sixty
            end do
        end do           
        return
    end subroutine 
                                           

   !> Interpolated flow and area mesh based on given four points of flows and water surface elevations
    subroutine interp_flow_area(flow_mesh, area_mesh,                   &
                                flow_volume_change, area_volume_change, &
                                branch, up_x, dx, dt, nt, nx,           &
                                flow_a, flow_b, flow_c, flow_d,         &
                                ws_a, ws_b, ws_c, ws_d,                 &
                                prev_flow_cell)
        implicit none
        integer, intent(in) :: branch                                               !< hydro channel number (required by CxArea())
        real(gtm_real), intent (in) :: up_x                                         !< upstream point distance (required for CxArea())
        real(gtm_real), intent(in) :: dx                                            !< finer cell size (in feet) 
        real(gtm_real), intent(in) :: dt                                            !< finer time step (in minutes)
        integer, intent(in) :: nt                                                   !< nt: number of points in time
        integer, intent(in) :: nx                                                   !< nx: number of points in space
        real(gtm_real), intent(in) :: flow_a, flow_b, flow_c, flow_d                !< input four corner flow points
        real(gtm_real), intent(in) :: ws_a, ws_b, ws_c, ws_d                        !< input four corner water surface points 
        real(gtm_real), dimension(nx), intent(in) :: prev_flow_cell                 !< last row of flow cells from previous interpolation
        real(gtm_real), dimension(nt,nx), intent(out) :: flow_mesh                  !< interpolated flow mesh
        real(gtm_real), dimension(nt,nx), intent(out) :: area_mesh                  !< interpolated area mesh
        real(gtm_real), dimension(nt-1,nx-1), intent(out) :: flow_volume_change     !< volume change from flow interpolation for each cell 
        real(gtm_real), dimension(nt-1,nx-1), intent(out) :: area_volume_change     !< volume change from area interpolation for each cell 
        real(gtm_real), dimension(nt,nx) :: flow_mesh_tmp                           !< local variable     
        real(gtm_real), dimension(nt-1,nx-1) :: flow_volume_change_tmp              !< local variable
     
        call interp_flow_linear(flow_mesh_tmp, flow_volume_change_tmp,                       &
                                dt, nt, nx, flow_a, flow_b, flow_c, flow_d) 
        call interp_area_byCxArea(area_mesh, area_volume_change, branch, up_x, dx, nt, nx,   &
                                  ws_a, ws_b, ws_c, ws_d, flow_volume_change_tmp)
        call interp_flow_from_area_theta_m1(flow_mesh, flow_volume_change, dt, nt, nx,          &  
                                            flow_a, flow_b, flow_c, flow_d,                     &
                                            area_volume_change, prev_flow_cell)
        return
    end subroutine            

    
   !>  Calculate total volume change
    subroutine calc_total_volume_change(total_volume_change, nt_1, nx_1, volume_change)
        implicit none
        integer, intent(in) :: nt_1                                        !< number in time direction (nt-1)
        integer, intent(in) :: nx_1                                        !< number in space direction (nx-1)
        real(gtm_real), dimension(nt_1,nx_1), intent(in) :: volume_change  !< volume change for each cell
        real(gtm_real), intent(out) :: total_volume_change                 !< total volume change
        integer :: i, j                                                    ! local variables
        total_volume_change = dble(0.0)
        do i = 1, nt_1
            do j = 1, nx_1
                total_volume_change = total_volume_change + volume_change(i,j)
            end do
        end do
        return       
    end subroutine
   
   !>  Print out interpolated mesh and volume change among cells into a file     
    subroutine print_mass_balance_check(file_unit, nt, nx, mesh, volume_change)
        implicit none
        integer, intent(in) :: file_unit                                   !< output file unit
        integer, intent(in) :: nt                                          !< nt: number of points in time
        integer, intent(in) :: nx                                          !< nx: number of points in space
        real(gtm_real), dimension(nt,nx), intent(in) :: mesh               !< interpolated mesh
        real(gtm_real), dimension(nt-1,nx-1), intent(in) :: volume_change  !< volume change for each cell
        real(gtm_real) :: total   
        character(len=10) :: format_str
        integer :: i, j, n
        format_str = '(   f15.5)'
        n = 2*nx
        write(format_str(2:4),'(I3)') n       
        write(file_unit, format_str) (mesh(1,j), j=1,nx)
        total = 0
        do i = 2, nt
            do j = 1, nx-1
                total = total + volume_change(i-1,j)
            end do  
            if (i.eq.nt) then
                write(file_unit, format_str) (mesh(i,j), j=1,nx), (volume_change(i-1,j),j=1,nx-1), total
            else      
                write(file_unit, format_str) (mesh(i,j), j=1,nx), (volume_change(i-1,j),j=1,nx-1)
            end if
        end do    
        return
    end subroutine    
    
end module