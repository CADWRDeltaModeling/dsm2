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
                else                               ! downstream boundary
                    grad(icell,:) = grad_lo(icell,:)
                end if              
            elseif (dsm2_network(i)%junction_no .ne. 0) then
                ! assign gradient for cells around junction to be zero--> first order accuracy
                ! but this may run into issue of smoothing two close signals (delta uniform flow case)
                do j = 1, dsm2_network(i)%n_conn_cell
                   icell = dsm2_network(i)%cell_no(j)
                   grad(icell,:) = zero
                end do
            elseif (dsm2_network(i)%nonsequential==1) then
                ! assign gradient for nonsequential adjucent cells to be zero--> first order accuracy
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
 
        return
    end subroutine
  
  
    !> Example advective flux that imposes boundary concentration based on the values read from input file
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
        use common_variables, only: n_node, dsm2_network
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
        integer :: i, j, icell   
   
        do i = 1, n_node
            if (dsm2_network(i)%boundary_no > 0) then                    ! is a boundary
                icell = dsm2_network(i)%cell_no(1)
                if ( dsm2_network(i)%up_down(1) .eq. 1) then             ! upstream boundary
                    flux_lo(icell,:) = conc_lo(icell,:)*flow_lo(icell)
                else
                    flux_hi(icell,:) = conc_hi(icell,:)*flow_hi(icell)
                end if
            end if
         
            if (dsm2_network(i)%junction_no > 0) then
                flow_tmp = zero 
                mass_tmp(:) = zero
                conc_tmp(:) = zero
                do j = 1, dsm2_network(i)%n_conn_cell    ! counting flow into the junctions
                    icell = dsm2_network(i)%cell_no(j)
                    if (dsm2_network(i)%up_down(j)==0 .and. flow_hi(icell)>zero) then        !cell at updstream of junction
                        mass_tmp(:) = mass_tmp(:) + conc_hi(icell,:)*flow_hi(icell)
                        flow_tmp = flow_tmp + flow_hi(icell)
                    elseif (dsm2_network(i)%up_down(j)==1 .and. flow_lo(icell)<zero) then    !cell at downdstream of junction
                        mass_tmp(:) = mass_tmp(:) + conc_lo(icell,:)*abs(flow_lo(icell))
                        flow_tmp = flow_tmp + abs(flow_lo(icell))
                    endif                   
                end do
                if (flow_tmp==zero) then
                    write(*,*) "WARNING: No flow flows into junction!!",icell               
                    conc_tmp(:) = zero
                else     
                    conc_tmp(:) = mass_tmp(:)/flow_tmp
                end if
                ! assign average concentration to downstream cell faces
                do j = 1, dsm2_network(i)%n_conn_cell
                    icell = dsm2_network(i)%cell_no(j)
                    if (dsm2_network(i)%up_down(j)==0 .and. flow_hi(icell)<zero) then     !cell at updstream of junction
                        flux_hi(icell,:) = conc_tmp(:)*flow_hi(icell)                                         
                    elseif (dsm2_network(i)%up_down(j)==1 .and. flow_lo(icell)>zero) then !cell at downdstream of junction
                        flux_lo(icell,:) = conc_tmp(:)*flow_lo(icell)
                    elseif (dsm2_network(i)%up_down(j)==1 .and. flow_lo(icell)<zero) then
                        flux_lo(icell,:) = conc_hi(icell,:)*flow_hi(icell)                        
                    endif                
                end do              
            end if
       
            if (dsm2_network(i)%nonsequential .eq. 1) then  ! without this fixup, the error can be seen at DSM2 node 239
                do j = 1, 2                              ! assign lo/hi face of the same cell to avoid discontinuity
                    icell = dsm2_network(i)%cell_no(j)
                    if (dsm2_network(i)%up_down(j)==0 .and. flow_hi(icell)<zero) then     !cell at updstream of link
                        flux_hi(icell,:) = conc_lo(icell,:)*flow_hi(icell)
                    elseif (dsm2_network(i)%up_down(j)==1 .and. flow_lo(icell)>zero) then !cell at downdstream of link
                        flux_lo(icell,:) = conc_hi(icell,:)*flow_lo(icell)
                    endif                
                end do
            end if   
            
            !flow_chk = flow_chk + resv_flow(dsm2_node(i)%resv_conn_no(j))
             
            !do j = 1, dsm2_node(i)%n_qext
            !    flow_chk = flow_chk + qext_flow(dsm2_node(i)%qext_no(j))
            !end do
            
            !do j = 1, dsm2_node(i)%n_tran 
            !    flow_chk = flow_chk + tran_flow(dsm2_node(i)%tran_no(j))
            !end do 
            !write(11,*) dsm2_node(i)%dsm2_node_no, flow_chk
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
        implicit none
        integer, intent(in)  :: ncell                            !< Number of cells
        integer, intent(in)  :: nvar                             !< Number of variables
        real(gtm_real), intent(inout) :: conc_lo(ncell,nvar)     !< Concentration extrapolated to lo face
        real(gtm_real), intent(inout) :: conc_hi(ncell,nvar)     !< Concentration extrapolated to hi face        
        integer :: i, icell
        
        do i = 1, n_node
            if ( (dsm2_network(i)%boundary_no.ne.0) .and. (dsm2_network(i)%node_conc==1) ) then  !if boundary and node concentration is given
                icell = dsm2_network(i)%cell_no(1)
                !if (dsm2_network(i)%up_down(1) .eq. 1) then     ! upstream boundary
                !    conc_lo(icell,:) = node_conc(i,:)
                !else
                !    conc_hi(icell,:) = node_conc(i,:)
                !end if    
            end if
        end do
                        
        return
    end subroutine    
       
end module  