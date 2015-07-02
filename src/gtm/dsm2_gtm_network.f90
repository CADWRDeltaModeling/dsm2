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
 
!> Routines that fulfill interfaces to accomandate DSM2 network
!> Use some back door information, such as dsm2_network, node_concentration
!>@ingroup gtm_driver
module dsm2_gtm_network

    contains

    !> Calculate the divided lo, hi, and centered differences. 
    !> This has adjustments for non-sequential cells.
    subroutine difference_network(grad_lo,     & 
                                  grad_hi,     &
                                  grad_center, &
                                  vals,        &
                                  dx,          &
                                  ncell,       &
                                  nvar)
                          
        use gtm_precision
        use common_variables, only : n_node, dsm2_network        
        implicit none

        !---- args
        integer, intent(in) :: ncell                          !< Number of cells
        integer, intent(in) :: nvar                           !< Number of variables
        real(gtm_real), intent(in) :: vals(ncell,nvar)        !< Data to be differenced
        real(gtm_real), intent(in) :: dx(ncell)               !< Cell length
        real(gtm_real), intent(out):: grad_lo(ncell,nvar)     !< Difference on lo side, LARGEREAL in first index
        real(gtm_real), intent(out):: grad_hi(ncell,nvar)     !< Difference on hi side (n+1) minus (n) LARGEREAL for last index
        real(gtm_real), intent(out):: grad_center(ncell,nvar) !< Centered diff, LARGEREAL for undefined boundary cells
        !----local
        integer :: up_cell, down_cell
        integer :: ivar
        integer :: i

        do ivar = 1, nvar
            grad_center(2:(ncell-1),ivar) = (vals(3:ncell,ivar) - vals(1:(ncell-2),ivar))/    &
                                          (half*dx(3:ncell) + dx(2:ncell-1) + half*dx(1:ncell-2))
            grad_center(1,ivar) = LARGEREAL
            grad_center(ncell,ivar) = LARGEREAL            
            grad_hi(1:(ncell-1),ivar) = (vals(2:ncell,ivar) - vals(1:(ncell-1),ivar))/        &
                                        (half*dx(2:ncell) + half*dx(1:ncell-1))
            grad_hi(ncell,ivar) = LARGEREAL
            grad_lo(2:ncell,ivar) = grad_hi(1:(ncell-1),ivar)
            grad_lo(1,ivar) = LARGEREAL  
            ! This loop is added to take care of nonsequential numbering cells and because of 
            ! introducing network component. A separate function is written here instead of 
            ! using the one for single channel.
            do i = 1, n_node
                if (dsm2_network(i)%nonsequential.eq.1) then
                    if (dsm2_network(i)%up_down(1) .eq. 0) then   !cell at upstream of junction 
                        up_cell = dsm2_network(i)%cell_no(1)
                        down_cell = dsm2_network(i)%cell_no(2)
                    else                                          !cell at downstream of junction
                        up_cell = dsm2_network(i)%cell_no(2)
                        down_cell = dsm2_network(i)%cell_no(1)
                    end if               
                    grad_hi(up_cell,ivar) = (vals(down_cell,ivar)-vals(up_cell,ivar))/          &
                                            (half*dx(down_cell)+half*dx(up_cell))
                    grad_lo(down_cell,ivar) = grad_hi(up_cell,ivar)                                      
                    grad_center(up_cell,ivar) = (vals(down_cell,ivar)-vals(up_cell-1,ivar))/    &
                                                (half*dx(down_cell)+dx(up_cell)+half*dx(up_cell-1))
                    grad_center(down_cell,ivar) = (vals(down_cell+1,ivar)-vals(up_cell,ivar))/  &
                                                  (half*dx(down_cell+1)+dx(down_cell)+half*dx(up_cell))
                end if    
            end do
        end do
        return
    end subroutine

  
    !> Adjust differences to account for special cases 
    !> (boundaries, structures, junctions, flow reversals)
    !> This routine needs to use back door information from dsm2_network.
    subroutine adjust_differences_network(grad,         &
                                          grad_lo,      &  
                                          grad_hi,      &
                                          grad_center,  &
                                          vals,         &
                                          dx,           &
                                          ncell,        &
                                          nvar,         &
                                          use_limiter)
        use gtm_precision
        use gradient, only : limiter
        use common_variables, only : n_node, dsm2_network
        implicit none
        !--- args
        real(gtm_real), intent(out) :: grad(ncell,nvar)          !< Cell centered difference adjusted for boundaries and hydraulic devices
        real(gtm_real), intent(inout) :: grad_lo(ncell,nvar)     !< Difference on lo side, LARGEREAL in first index
        real(gtm_real), intent(inout) :: grad_hi(ncell,nvar)     !< Difference on hi side (n+1) minus (n) LARGEREAL for last index
        real(gtm_real), intent(inout) :: grad_center(ncell,nvar) !< Dentered diff, LARGEREAL for undefined boundary cells
        real(gtm_real), intent(in) :: vals(ncell,nvar)           !< Data to be differenced          
        real(gtm_real), intent(in) :: dx(ncell)                  !< Cell length
        integer,intent(in)  :: ncell                             !< Number of cells
        integer,intent(in)  :: nvar                              !< Number of variables
        logical,intent(in), optional :: use_limiter              !< whether to use slope limiter
        !--- local variabls
        real(gtm_real) :: upval(nvar), downval(nvar)             ! sum of connected up/down-stream vals
        real(gtm_real) :: up_length, down_length                 ! sum of connected up/down-stream length
        real(gtm_real) :: up_split_ratio, down_split_ratio       ! ratio to apply splitting to up/down-stream        
        integer :: n_up_cell, n_down_cell                        ! num of connected up/down-stream cells        
        integer :: icell, i, j                                   ! local variables
        logical :: limit_slope                                   ! whether slope limiter is used         
        integer :: min_cell_no

        if (present(use_limiter))then
            limit_slope = use_limiter
        else
            limit_slope = .true.
        end if        
         
        if (limit_slope)then    ! Applies flux-limeter on high resolution gradient 
            call limiter(grad, grad_lo, grad_hi ,grad_center, ncell, nvar)
        else    
            grad = grad_center
        end if    
        grad(1,:)     = grad_hi(1,:)      ! in case cell_no=1 does not locate at actual boundary, w/t this line will cause error. 
        grad(ncell,:) = grad_lo(ncell,:)  ! in case cell_no=ncell does not locate at actual boundary, w/t this line will cause error.          

        do i = 1, n_node
            ! adjust boundaries
            if (dsm2_network(i)%boundary_no .ne. 0) then
                icell = dsm2_network(i)%cell_no(1)
                if (dsm2_network(i)%up_down(1) .eq. 1) then  ! upstream boundary
                    grad(icell,:) = grad_hi(icell,:)
                else                                         ! downstream boundary
                    grad(icell,:) = grad_lo(icell,:)
                end if              
            ! assign gradient for cells around junction to be zero--> first order accuracy
            ! but this may run into issue of smoothing two close signals (delta uniform flow case)                
            elseif ((dsm2_network(i)%junction_no .ne. 0) .and. (dsm2_network(i)%n_conn_cell .gt. 2)) then
                do j = 1, dsm2_network(i)%n_conn_cell
                   icell = dsm2_network(i)%cell_no(j)
                   grad(icell,:) = zero
                end do
            end if 
        end do     
        return
    end subroutine
  
  
    !> advective flux that imposes boundary concentration based on the values read from input file
    !> overwrite flux_lo and flux_hi for boundaries and junctions
    subroutine bc_advection_flux_network(flux_lo,    &
                                         flux_hi,    &
                                         conc_lo,    &
                                         conc_hi,    &
                                         flow_lo,    &
                                         flow_hi,    &
                                         ncell,      &
                                         nvar,       &
                                         time,       &
                                         dt,         &
                                         dx)
        use gtm_precision
        use error_handling
        use common_variables, only: n_node, dsm2_network, n_resv, resv_geom
        use state_variables_network
        implicit none
        !--- args          
        integer,intent(in)  :: ncell                            !< Number of cells
        integer,intent(in)  :: nvar                             !< Number of variables
        real(gtm_real),intent(inout) :: flux_lo(ncell,nvar)     !< Flux on lo side of cell, time centered
        real(gtm_real),intent(inout) :: flux_hi(ncell,nvar)     !< Flux on hi side of cell, time centered
        real(gtm_real),intent(in)    :: flow_lo(ncell)          !< Flow on lo side of cells centered in time
        real(gtm_real),intent(in)    :: flow_hi(ncell)          !< Flow on hi side of cells centered in time
        real(gtm_real),intent(in)    :: conc_lo(ncell,nvar)     !< Concentration extrapolated to lo face
        real(gtm_real),intent(in)    :: conc_hi(ncell,nvar)     !< Concentration extrapolated to hi face
        real(gtm_real),intent(in)    :: time                    !< Current time
        real(gtm_real),intent(in)    :: dx(ncell)               !< Spatial step  
        real(gtm_real),intent(in)    :: dt                      !< Time step 
        real(gtm_real) :: flow_tmp
        real(gtm_real) :: mass_tmp(nvar)
        real(gtm_real) :: conc_tmp(nvar)
        real(gtm_real) :: up_count
        real(gtm_real) :: vol
        real(gtm_real) :: mass_resv(nvar)
        integer :: up_cell, down_cell
        integer :: network_id
        integer :: i, j, k, icell
        
        ! recalculate concentration for reservoirs
        do i = 1, n_resv
            vol = resv_geom(i)%area * million * prev_resv_height(i)
            mass_resv(:) = vol * prev_conc_resv(i,:)
            do j = 1, resv_geom(i)%n_resv_conn
                network_id = resv_geom(i)%network_id(j)
                conc_tmp(:) = zero
                up_count = zero 
                do  k = 1, dsm2_network(network_id)%n_conn_cell
                    if (dsm2_network(network_id)%up_down(k) .eq. 0) then !upstream cell
                        up_count = up_count + one
                        conc_tmp(:) = conc_tmp(:) + conc_hi(dsm2_network(network_id)%cell_no(k),:)
                    end if
                end do
                conc_tmp(:) = conc_tmp(:)/up_count  !temporary upstream concentration around DSM2 node                
                vol = vol - resv_flow(resv_geom(i)%resv_conn_no(j))*dt
                if (resv_flow(resv_geom(i)%resv_conn_no(j)).gt.zero) then   ! flow from reservoir
                    mass_resv(:) = mass_resv(:) - resv_flow(resv_geom(i)%resv_conn_no(j))*dt*prev_conc_resv(i,:)                
                else   ! flow from channel
                    mass_resv(:) = mass_resv(:) - resv_flow(resv_geom(i)%resv_conn_no(j))*dt*conc_tmp(:)
                end if
            end do
            conc_resv(i,:) = mass_resv(:)/vol
        end do
   
        do i = 1, n_node
            ! adjust flux for boundaries
            if (dsm2_network(i)%boundary_no > 0) then       
                icell = dsm2_network(i)%cell_no(1)
                if (( dsm2_network(i)%up_down(1) .eq. 1).and.(flow_lo(icell).ge.zero)) then    ! upstream boundary
                    flux_lo(icell,:) = conc_lo(icell,:)*flow_lo(icell)
                elseif(( dsm2_network(i)%up_down(1) .eq. 1).and.(flow_lo(icell).lt.zero)) then
                    flux_lo(icell,:) = conc_hi(icell,:)*flow_hi(icell)
                elseif(( dsm2_network(i)%up_down(1) .eq. 0).and.(flow_hi(icell).ge.zero)) then ! downstream boundary
                    flux_hi(icell,:) = conc_lo(icell,:)*flow_lo(icell)
                else
                    flux_hi(icell,:) = conc_hi(icell,:)*flow_hi(icell)
                end if
            end if
            ! adjust flux for non-sequential adjacent cells
            if (dsm2_network(i)%nonsequential.eq.1) then
                if (dsm2_network(i)%up_down(1) .eq. 0) then   !cell at upstream of junction 
                    up_cell = dsm2_network(i)%cell_no(1)
                    down_cell = dsm2_network(i)%cell_no(2)
                else                                          !cell at downstream of junction
                    up_cell = dsm2_network(i)%cell_no(2)
                    down_cell = dsm2_network(i)%cell_no(1)
                end if
                if (flow_hi(up_cell) .gt. zero) then
                    flux_hi(up_cell,:) = conc_hi(up_cell,:)*flow_hi(up_cell)
                else
                    flux_hi(up_cell,:) = conc_lo(down_cell,:)*flow_lo(down_cell)
                end if        
                if (flow_lo(down_cell) .gt. zero) then
                    flux_lo(down_cell,:) = conc_hi(up_cell,:)*flow_hi(up_cell)
                else
                    flux_lo(down_cell,:) = conc_lo(down_cell,:)*flow_lo(down_cell)
                end if                 
            end if            
            ! adjust flux for junctions
            if (dsm2_network(i)%junction_no .gt. 0) then
                flow_tmp = zero 
                mass_tmp(:) = zero
                conc_tmp(:) = zero
                do j = 1, dsm2_network(i)%n_conn_cell     ! counting flow into the junctions
                    icell = dsm2_network(i)%cell_no(j)
                    if (dsm2_network(i)%up_down(j).eq.0 .and. flow_hi(icell).gt.zero) then     !cell at updstream of junction
                        mass_tmp(:) = mass_tmp(:) + conc_hi(icell,:)*flow_hi(icell)
                        flow_tmp = flow_tmp + flow_hi(icell)                       
                    elseif (dsm2_network(i)%up_down(j).eq.1 .and. flow_lo(icell).lt.zero) then !cell at downdstream of junction
                        mass_tmp(:) = mass_tmp(:) + conc_lo(icell,:)*abs(flow_lo(icell))
                        flow_tmp = flow_tmp + abs(flow_lo(icell))
                    endif                   
                end do
                ! temporarily calculated concentration to assign to seepage and diversion
                if (flow_tmp .lt. one) then
                    !write(*,*) "WARNING: No flow flows into junction!!",icell               
                    conc_tmp(:) = conc_lo(icell,:)
                else     
                    conc_tmp(:) = mass_tmp(:)/flow_tmp
                end if
                ! add external flows
                if ((dsm2_network(i)%boundary_no.eq.0).and.(dsm2_network_extra(i)%n_qext.gt.0)) then
                    do j = 1, dsm2_network_extra(i)%n_qext
                        if ((qext_flow(dsm2_network_extra(i)%qext_no(j)).gt.0).and.(dsm2_network(i)%node_conc.eq.1)) then    !drain
                            mass_tmp(:) = mass_tmp(:) + node_conc(i,:)*qext_flow(dsm2_network_extra(i)%qext_no(j))
                            flow_tmp = flow_tmp + qext_flow(dsm2_network_extra(i)%qext_no(j))
                        elseif ((qext_flow(dsm2_network_extra(i)%qext_no(j)).gt.0).and.(dsm2_network(i)%node_conc.eq.0)) then !drain but node concentration is absent
                            mass_tmp(:) = mass_tmp(:) + conc_tmp(:)*qext_flow(dsm2_network_extra(i)%qext_no(j))
                            flow_tmp = flow_tmp + qext_flow(dsm2_network_extra(i)%qext_no(j))                            
                            !write(*,*) "WARNING: No node concentration is given for DSM2 Node No. !!",dsm2_network(i)%dsm2_node_no
                        else     ! seepage and diversion
                            mass_tmp(:) = mass_tmp(:) + conc_tmp(:)*qext_flow(dsm2_network_extra(i)%qext_no(j))
                            flow_tmp = flow_tmp + qext_flow(dsm2_network_extra(i)%qext_no(j))                        
                        end if
                    end do
                    if (flow_tmp .lt. one) then
                        !write(*,*) "WARNING: No flow flows into junction!!",icell               
                        conc_tmp(:) = conc_lo(icell,:)
                    else     
                        conc_tmp(:) = mass_tmp(:)/flow_tmp
                    end if     
                end if     
                if (dsm2_network_extra(i)%dsm2_node_no.eq.316) then           
                !write(201,'(f14.4,6f10.0)') flow_tmp,mass_tmp(1),conc_tmp(1),conc_hi(2157,1),conc_lo(2157,1),flow_hi(2157),flow_lo(2157)
                end if
                ! assign average concentration to downstream cell faces
                do j = 1, dsm2_network(i)%n_conn_cell
                    icell = dsm2_network(i)%cell_no(j)
                    if ((dsm2_network(i)%up_down(j).eq.0) .and. (flow_hi(icell).le.zero)) then  !cell at updstream of junction and flow away from junction
                        flux_hi(icell,:) = conc_tmp(:)*flow_hi(icell)
                    elseif ((dsm2_network(i)%up_down(j).eq.1) .and. (flow_lo(icell).ge.zero)) then !cell at downdstream of junction
                        flux_lo(icell,:) = conc_tmp(:)*flow_lo(icell)
                    endif               
                end do              
            end if

        end do          
        return
    end subroutine  
    
    
    !> No assignment for boundary flow and leave it as it is
    subroutine assign_boundary_concentration(conc_lo,  &
                                             conc_hi,  &
                                             ncell,    &
                                             nvar)
        use gtm_precision
        use error_handling
        use common_variables, only : n_node, dsm2_network, dsm2_network_extra
        use state_variables_network, only : node_conc
        implicit none
        integer, intent(in)  :: ncell                            !< Number of cells
        integer, intent(in)  :: nvar                             !< Number of variables
        real(gtm_real), intent(inout) :: conc_lo(ncell,nvar)     !< Concentration extrapolated to lo face
        real(gtm_real), intent(inout) :: conc_hi(ncell,nvar)     !< Concentration extrapolated to hi face        
        integer :: i, j, icell
        
        do i = 1, n_node
            ! if node concentration is given, assign the value to lo or hi face.
            if (dsm2_network(i)%node_conc.eq.1 .and. dsm2_network_extra(i)%boundary.ne.0) then  
                do j = 1, dsm2_network(i)%n_conn_cell
                    icell = dsm2_network(i)%cell_no(j)
                    if (dsm2_network(i)%up_down(j).eq.0) then !cell at upstream of junction 
                        conc_hi(icell,:) = node_conc(i,:)
                    else                                       !cell at downstream of junction 
                        conc_lo(icell,:) = node_conc(i,:)
                    end if 
                end do                               
            end if
        end do
                        
        return
    end subroutine    
       
end module  