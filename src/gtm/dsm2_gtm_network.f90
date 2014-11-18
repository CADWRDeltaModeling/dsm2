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
 
!> Routines that fulfill interfaces to accomandate DSM2 network
!> Use some back door information, such as dsm2_network, node_concentration
!>@ingroup gtm
module dsm2_gtm_network

    contains
  
    !> Adjust differences to account for special cases (boundaries, structures, junctions, flow reversals)
    !> This routine needs to use back door information: dsm2_network
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
        grad(1,:)     = grad_hi(1,:)             ! in case cell_no=1 does not locate at actual boundary, w/t this line will cause error. 
        grad(ncell,:) = grad_lo(ncell,:)         ! in case cell_no=ncell does not locate at actual boundary, w/t this line will cause error.          

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
            ! assign gradient for nonsequential adjucent cells to be zero--> first order accuracy     
            elseif (dsm2_network(i)%nonsequential==1) then                
                do j = 1, 2
                    icell = dsm2_network(i)%cell_no(j)
                    if (dsm2_network(i)%up_down(j)==0) then   !cell at upstream of link
                        grad(icell,:) = grad_lo(icell,:)
                    else
                        grad(icell,:) = grad_hi(icell,:)
                    end if    
                end do                        
            end if 
        end do     
        grad(941,:)=grad_hi(817,:)  !todo: remove later
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
                !write(22,'(f15.0,i3,3f20.3,2f20.10)') time, j, resv_flow(resv_geom(i)%resv_conn_no(j)), vol, mass_resv(1), prev_conc_resv(i,1), conc_tmp(1)
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
                if (flow_tmp==zero) then
                    !write(*,*) "WARNING: No flow flows into junction!!",icell               
                    conc_tmp(:) = zero
                else     
                    conc_tmp(:) = mass_tmp(:)/flow_tmp
                end if
                ! add external flows
                if ((dsm2_network(i)%boundary_no.eq.0).and.(dsm2_network(i)%n_qext.gt.0)) then
                    do j = 1, dsm2_network(i)%n_qext
                        if ((qext_flow(dsm2_network(i)%qext_no(j)).gt.0).and.(dsm2_network(i)%node_conc.eq.1)) then    !drain
                            mass_tmp(:) = mass_tmp(:) + node_conc(i,:)*qext_flow(dsm2_network(i)%qext_no(j))
                            flow_tmp = flow_tmp + qext_flow(dsm2_network(i)%qext_no(j))
                        elseif ((qext_flow(dsm2_network(i)%qext_no(j)).gt.0).and.(dsm2_network(i)%node_conc.eq.0)) then !drain but node concentration is absent
                            !write(*,*) "WARNING: No node concentration is given for DSM2 Node No. !!",dsm2_network(i)%dsm2_node_no
                        else     ! seepage and diversion
                            mass_tmp(:) = mass_tmp(:) + conc_tmp(:)*qext_flow(dsm2_network(i)%qext_no(j))
                            flow_tmp = flow_tmp + qext_flow(dsm2_network(i)%qext_no(j))                        
                        end if
                    end do
                    if (flow_tmp==zero) then
                        !write(*,*) "WARNING: No flow flows into junction!!",icell               
                        conc_tmp(:) = zero
                    else     
                        conc_tmp(:) = mass_tmp(:)/flow_tmp
                    end if       
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
       
            if (dsm2_network(i)%nonsequential .eq. 1) then  ! without this fixup, the error can be seen at DSM2 node 239
                do j = 1, 2                                 ! assign lo/hi face of the same cell to avoid discontinuity
                    icell = dsm2_network(i)%cell_no(j)
                    if (dsm2_network(i)%up_down(j)==0 .and. flow_hi(icell)<zero) then     !cell at updstream of link                        
                        flux_hi(icell,:) = conc_lo(icell,:)*flow_hi(icell)                       
                    elseif (dsm2_network(i)%up_down(j)==1 .and. flow_lo(icell)<zero) then !cell at downdstream of link
                        flux_lo(icell,:) = conc_hi(icell,:)*flow_lo(icell)                      
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
        use common_variables, only : n_node, dsm2_network
        use state_variables_network, only : node_conc
        implicit none
        integer, intent(in)  :: ncell                            !< Number of cells
        integer, intent(in)  :: nvar                             !< Number of variables
        real(gtm_real), intent(inout) :: conc_lo(ncell,nvar)     !< Concentration extrapolated to lo face
        real(gtm_real), intent(inout) :: conc_hi(ncell,nvar)     !< Concentration extrapolated to hi face        
        integer :: i, icell
        
        do i = 1, n_node
            if ( (dsm2_network(i)%boundary_no.ne.0) .and. (dsm2_network(i)%node_conc.eq.1) ) then  !if boundary and node concentration is given
                icell = dsm2_network(i)%cell_no(1)
                if (dsm2_network(i)%up_down(1) .eq. 1) then     ! upstream boundary
                    conc_lo(icell,:) = node_conc(i,:)
                else
                    conc_hi(icell,:) = node_conc(i,:)
                end if    
            end if
        end do
                        
        return
    end subroutine    
       
end module  