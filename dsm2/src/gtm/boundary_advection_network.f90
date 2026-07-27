!<license>
!    Copyright (C) 2017 State of California,
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
module boundary_advection_network

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

        use constants
        use gtm_vars, only : n_node, dsm2_network, constituents
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
        integer :: up_cell, down_cell, updown, updown_next, c1, c2
        integer :: ivar
        integer :: i

        do ivar = 1, nvar
            if (constituents(ivar)%simulate) then
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
                if (dsm2_network(i)%nonsequential == 1) then
                    updown = dsm2_network(i)%up_down(1)
                    updown_next = dsm2_network(i)%up_down(2)
                    c1 = dsm2_network(i)%cell_no(1)
                    c2 = dsm2_network(i)%cell_no(2)

                    ! Converging to the node.  --> o <--
                    if (updown == TO_NODE .and. updown_next == TO_NODE) then
                        grad_hi(c1, ivar) = (vals(c2, ivar) - vals(c1, ivar)) / &
                            (half * dx(c1) + half * dx(c2))
                        grad_hi(c2, ivar) = - grad_hi(c1, ivar)
                        grad_center(c1, ivar) = (vals(c2, ivar) - vals(c1 - 1, ivar)) / &
                            (half * dx(c2) + dx(c1) + half * dx(c1 - 1))
                        grad_center(c2, ivar) = (vals(c1, ivar) - vals(c2 - 1, ivar)) / &
                            (half * dx(c1) + dx(c2) + half * dx(c2 - 1))
                    ! Diverging from the node. <-- o -->
                    else if (updown == FROM_NODE .and. updown_next == FROM_NODE) then
                        grad_lo(c1, ivar) = (vals(c1, ivar) - vals(c2, ivar)) / &
                            (half * dx(c1) + half * dx(c2))
                        grad_lo(c2, ivar) = - grad_lo(c1, ivar)
                        grad_center(c1, ivar) = (vals(c1 + 1, ivar) - vals(c2, ivar)) / &
                            (half * dx(c1 + 1) + dx(c1) + half * dx(c2))
                        grad_center(c2, ivar) = (vals(c2 + 1, ivar) - vals(c1, ivar)) / &
                            (half * dx(c2 + 1) + dx(c2) + half * dx(c1))
                    else
                        if (updown == TO_NODE) then
                            up_cell = dsm2_network(i)%cell_no(1)
                            down_cell = dsm2_network(i)%cell_no(2)
                        else
                            up_cell = dsm2_network(i)%cell_no(2)
                            down_cell = dsm2_network(i)%cell_no(1)
                        end if
                        grad_hi(up_cell, ivar) = (vals(down_cell, ivar) - vals(up_cell, ivar)) / &
                                                 (half * dx(down_cell) + half * dx(up_cell))
                        grad_lo(down_cell, ivar) = grad_hi(up_cell, ivar)
                        grad_center(up_cell, ivar) = (vals(down_cell, ivar) - vals(up_cell - 1, ivar)) / &
                                                     (half * dx(down_cell) + dx(up_cell) + half * dx(up_cell - 1))
                        grad_center(down_cell, ivar) = (vals(down_cell + 1, ivar) - vals(up_cell, ivar)) / &
                                                       (half * dx(down_cell  + 1) + dx(down_cell) + half * dx(up_cell))
                    end if
                end if
            end do
            end if
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
        use constants
        use gradient, only : limiter
        use gtm_vars, only : n_node, dsm2_network, n_qext
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
        !--- local variables
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
                if (dsm2_network(i)%up_down(1) == FROM_NODE) then  ! upstream boundary
                    grad(icell,:) = grad_hi(icell,:)
                else                                         ! downstream boundary
                    grad(icell,:) = grad_lo(icell,:)
                end if
            ! adjust gradient for cells around junctions
            elseif ((dsm2_network(i)%junction_no .ne. 0) .and. (dsm2_network(i)%n_conn_cell .gt. 2)) then
                do j = 1, dsm2_network(i)%n_conn_cell
                   icell = dsm2_network(i)%cell_no(j)
                   if (dsm2_network(i)%up_down(j) == TO_NODE) then  ! cell at upstream of junction
                       grad(icell,:) = grad_lo(icell,:)
                   else
                       grad(icell,:) = grad_hi(icell,:)
                   end if
                end do
            end if
        end do
        return
    end subroutine

    ! for first time setup of the run
    subroutine build_boundary_sediment_fraction_array(nvar, ivar, sed_percent)

        use constants
        use state_variables_network
        use gtm_vars, only: n_node, dsm2_network
        use common_gtm_vars, only: pathinput
        implicit none
        integer,intent(in)  :: nvar                                     !< Number of variables
        integer,intent(in)  :: ivar                                     !< variable index
        real(gtm_real),intent(out) :: sed_percent(n_node,n_qext,nvar)!<percentages of compositions at boundaries  & 10 is the maximum number of
                                                                                         !external flows        !<TODO: make array dimensions effective
        integer :: i, j, s, st
        logical :: boundary_composition_not_found =.true.

        do i = 1, n_node
            ! Loop thourgh external flows at node i
            do j = 1, dsm2_network_extra(i)%n_qext
                ! If there are associated data to this external flow
                if (dsm2_network_extra(i)%qext_path(j,ivar).eq.0) cycle
                ! If the associated data is SSC,
                if (trim(pathinput(dsm2_network_extra(i)%qext_path(j,ivar))%variable).eq.'ssc') then
                    ! Loop through all the sediment classes
                    do st = 1, n_sediment
                        boundary_composition_not_found = .true.
                        ! Loop through all sediment boundaries in the model
                        do s = 1, n_sediment_bc
                            ! Find out matching boundary condition time series
                            if ((trim(pathinput(dsm2_network_extra(i)%qext_path(j,ivar))%name) .eq. trim(sediment_bc(s)%name)) &
                                .and. (trim(sediment(st)%composition) .eq. trim(sediment_bc(s)%composition))) then
                                ! Copy the sediment fraction
                                sed_percent(i,j,nvar-n_sediment+st) = sediment_bc(s)%percent
                                boundary_composition_not_found = .false.
                            end if
                        end do
                        ! If a corresponding data is not found, exit.
                        if (boundary_composition_not_found) then
                            write(*,*) 'DICU input classes less than specified'
                            stop
                        end if
                    end do
                end if
            end do
        end do

    return

    end subroutine build_boundary_sediment_fraction_array

    subroutine adjust_boundary_fluxes(flux_lo, flux_hi, conc_lo, conc_hi, flow_lo, flow_hi, ncell, nvar, ivar)

        use constants
        use state_variables_network
        use gtm_vars, only: n_node, dsm2_network

        implicit none
        integer,intent(in)  :: ncell                            !< Number of cells
        integer,intent(in)  :: nvar                             !< Number of variables
        integer,intent(in)  :: ivar                             !< variable index
        real(gtm_real),intent(inout) :: flux_lo(ncell,nvar)     !< Flux on lo side of cell, time centered
        real(gtm_real),intent(inout) :: flux_hi(ncell,nvar)     !< Flux on hi side of cell, time centered
        real(gtm_real),intent(in)    :: flow_lo(ncell)          !< Flow on lo side of cells centered in time
        real(gtm_real),intent(in)    :: flow_hi(ncell)          !< Flow on hi side of cells centered in time
        real(gtm_real),intent(in)    :: conc_lo(ncell,nvar)     !< Concentration extrapolated to lo face
        real(gtm_real),intent(in)    :: conc_hi(ncell,nvar)     !< Concentration extrapolated to hi face
        integer :: i, icell, updown

        do i = 1, n_node
            ! adjust flux for boundaries
            updown = dsm2_network(i)%up_down(1)
            if (dsm2_network(i)%boundary_no <= 0) cycle
            icell = dsm2_network(i)%cell_no(1)
            ! Away from the node
            if ((updown == FROM_NODE)) then
                if (flow_lo(icell).ge.zero) then
                    flux_lo(icell,ivar) = conc_stip(icell,ivar)*flow_lo(icell)
                else if (flow_lo(icell).lt.zero) then
                    flux_lo(icell,ivar) = conc_lo(icell,ivar)*flow_lo(icell)
                end if
            ! coming to the node
            else
                ! outflow
                if (flow_hi(icell).ge.zero) then
                    flux_hi(icell,ivar) = conc_hi(icell,ivar)*flow_hi(icell)
                ! inflow
                else
                    flux_hi(icell,ivar) = conc_stip(icell,ivar)*flow_hi(icell)
                end if
            end if
        end do

        return

    end subroutine adjust_boundary_fluxes

    subroutine adjust_nonsequential_cell_fluxes(flux_lo, flux_hi, conc_lo, conc_hi, flow_lo, flow_hi, ncell, nvar, ivar)

        use constants
        use state_variables_network
        use gtm_vars, only: n_node, dsm2_network

        implicit none
        integer,intent(in)  :: ncell                            !< Number of cells
        integer,intent(in)  :: nvar                             !< Number of variables
        integer,intent(in)  :: ivar                             !< variable index
        real(gtm_real),intent(inout) :: flux_lo(ncell,nvar)     !< Flux on lo side of cell, time centered
        real(gtm_real),intent(inout) :: flux_hi(ncell,nvar)     !< Flux on hi side of cell, time centered
        real(gtm_real),intent(in)    :: flow_lo(ncell)          !< Flow on lo side of cells centered in time
        real(gtm_real),intent(in)    :: flow_hi(ncell)          !< Flow on hi side of cells centered in time
        real(gtm_real),intent(in)    :: conc_lo(ncell,nvar)     !< Concentration extrapolated to lo face
        real(gtm_real),intent(in)    :: conc_hi(ncell,nvar)     !< Concentration extrapolated to hi face
        integer :: i, updown, updown_next
        integer :: up_cell, down_cell, c1, c2

        do i = 1, n_node
            ! adjust flux for non-sequential adjacent cells
            ! BUG This works only when there are only two cells connected to the node
            updown = dsm2_network(i)%up_down(1)
            if (dsm2_network(i)%nonsequential.ne.1) cycle
            if (dsm2_network(i)%n_conn_cell > 2) then
                write(*,*) 'Error: nonsequential with more than 2 connected cells is not supported'
                call exit(-1)
            end if
            ! If the flow is toward to the node, meaning the node is the downstream of the cell (up_down == 0)
            updown_next = dsm2_network(i)%up_down(2)
            c1 = dsm2_network(i)%cell_no(1)
            c2 = dsm2_network(i)%cell_no(2)
            ! Converging to the node.  --> o <--
            if ((updown == TO_NODE) .and. (updown_next == TO_NODE)) then
                if ((flow_hi(c1) < zero) .and. (flow_hi(c2) > zero)) then
                    flux_hi(c1, ivar) = - conc_hi(c2, ivar) * flow_hi(c2)
                end if
                if ((flow_hi(c2) < zero) .and. (flow_hi(c1) > zero)) then
                    flux_hi(c2, ivar) = - conc_hi(c1, ivar) * flow_hi(c1)
                end if
            ! Diverging from the node. <-- o -->
            else if ((updown == FROM_NODE) .and. (updown_next == FROM_NODE)) then
                if ((flow_lo(c1) > zero) .and. (flow_lo(c2) < zero)) then
                    flux_lo(c1, ivar) = - conc_lo(c2, ivar) * flow_lo(c2)
                end if
                if ((flow_lo(c2) > zero) .and. (flow_lo(c1) < zero)) then
                    flux_lo(c2, ivar) = - conc_lo(c1, ivar) * flow_lo(c1)
                end if
            ! Simply non-sequential but the direction of cells are the same.
            else
                if (updown == TO_NODE) then
                    up_cell = c1
                    down_cell = c2
                else
                    up_cell = c2
                    down_cell = c1
                end if
                if ((flow_lo(down_cell) > zero) .and. (flow_hi(up_cell) > zero)) then
                    flux_lo(down_cell, ivar) = conc_hi(up_cell, ivar) * flow_hi(up_cell)
                end if
                if ((flow_hi(up_cell) < zero) .and. (flow_lo(down_cell) < zero)) then
                    flux_hi(up_cell, ivar) = conc_lo(down_cell, ivar) * flow_lo(down_cell)
                end if
            end if
        end do

        return
    end subroutine adjust_nonsequential_cell_fluxes

    subroutine adjust_junction_fluxes(flow_tmp, flux_in, conc_lo, conc_hi, flow_lo, flow_hi, ncell, nvar, ivar)

        use constants
        use state_variables_network
        use gtm_vars, only: n_node, dsm2_network

        implicit none
        integer,intent(in)  :: ncell                            !< Number of cells
        integer,intent(in)  :: nvar                             !< Number of variables
        integer,intent(in)  :: ivar                             !< variable index
        real(gtm_real),intent(in)    :: flow_lo(ncell)          !< Flow on lo side of cells centered in time
        real(gtm_real),intent(in)    :: flow_hi(ncell)          !< Flow on hi side of cells centered in time
        real(gtm_real),intent(in)    :: conc_lo(ncell,nvar)     !< Concentration extrapolated to lo face
        real(gtm_real),intent(in)    :: conc_hi(ncell,nvar)     !< Concentration extrapolated to hi face
        real(gtm_real),intent(inout) :: flow_tmp(n_node)        !< Temporary flow array to calculate junction concentration
        real(gtm_real),intent(inout) :: flux_in(n_node)         !< Flux into the junctions
        integer :: i, j, icell

        do i = 1, n_node
            ! adjust flux for junctions
            if (dsm2_network(i)%junction_no .eq. 0) cycle

            do j = 1, dsm2_network(i)%n_conn_cell     ! counting flow into the junctions
                icell = dsm2_network(i)%cell_no(j)
                if (dsm2_network(i)%up_down(j).eq.0 .and. flow_hi(icell).gt.zero) then     !cell at updstream of junction
                    flux_in(i) = flux_in(i) + conc_hi(icell,ivar)*flow_hi(icell)
                    flow_tmp(i) = flow_tmp(i) + flow_hi(icell)
                elseif (dsm2_network(i)%up_down(j).eq.1 .and. flow_lo(icell).lt.zero) then !cell at downdstream of junction
                    flux_in(i) = flux_in(i) - conc_lo(icell,ivar)*flow_lo(icell)
                    flow_tmp(i) = flow_tmp(i) - flow_lo(icell)
                endif
            end do
        end do

        return
    end subroutine adjust_junction_fluxes

    subroutine add_external_flow(flow_tmp, flux_in, nvar, ivar, sed_percent)

        use constants
        use gtm_vars, only: n_node, dsm2_network, dsm2_network_extra, n_sediment, n_qext
        use state_variables_network, only : qext_flow, conc_qext
        use common_gtm_vars, only: pathinput
        implicit none
        integer,intent(in)  :: nvar                             !< Number of variables
        integer,intent(in)  :: ivar                             !< variable index
        real(gtm_real),intent(inout) :: flow_tmp(n_node)        !< Temporary flow array to calculate junction concentration
        real(gtm_real),intent(inout) :: flux_in(n_node)         !< Flux into the junctions
        real(gtm_real),intent(in) :: sed_percent(n_node,n_qext,nvar)!<percentages of compositions at boundaries  & 10 is the maximum number of
                                                                                         !external flows        !<TODO: make array dimensions effective
        real(gtm_real) :: qext_fl
        real(gtm_real) :: conc_ext
        integer :: i, j, st
        integer :: qext_path_id

        do i = 1, n_node
            if (dsm2_network(i)%junction_no .eq. 0) cycle
            ! add external flows
            if ((dsm2_network(i)%boundary_no.eq.0).and.(dsm2_network_extra(i)%n_qext.gt.0)) then
                ! loop through external flows
                do j = 1, dsm2_network_extra(i)%n_qext
                    qext_fl = qext_flow(dsm2_network_extra(i)%qext_no(j))
                    ! If drain and if there are associated data to it
                    qext_path_id = dsm2_network_extra(i)%qext_path(j,ivar)
                    if (qext_fl > 0) then ! drain
                        flow_tmp(i) = flow_tmp(i) + qext_fl
                        if (qext_path_id /= 0) then
                            conc_ext = pathinput(qext_path_id)%value
                            ! If the associated data is SSC,
                            if (trim(pathinput(qext_path_id)%variable).eq.'ssc') then
                                ! Loop through all the sediment classes
                                do st = 1, n_sediment
                                    conc_ext = pathinput(qext_path_id)%value &
                                        * sed_percent(i,j,nvar-n_sediment+st) * 0.01d0
                                end do
                            end if
                            flux_in(i) = flux_in(i) + conc_ext * qext_fl
                        else !drain but node concentration is absent
                            flux_in(i) = flux_in(i) + conc_ext * qext_fl
                            write(*,*) "WARNING: No node concentration is given for DSM2 Node No. !!",dsm2_network(i)%dsm2_node_no
                        end if
                    end if
                end do
            end if
        end do

        return
    end subroutine add_external_flow

    subroutine add_reservoir_flow(flow_tmp, flux_in, vol, mass_resv, nvar, ivar, dt)

        use constants
        use gtm_vars, only: n_node, n_resv, dsm2_network, dsm2_network_extra
        use state_variables_network
        use common_gtm_vars
        implicit none
        integer,intent(in)  :: nvar                             !< Number of variables
        integer,intent(in)  :: ivar                             !< variable index
        real(gtm_real),intent(in)    :: dt                      !< Time step
        real(gtm_real),intent(inout) :: flow_tmp(n_node)        !< Temporary flow array to calculate junction concentration
        real(gtm_real),intent(inout) :: flux_in(n_node)         !< Flux into the junctions
        real(gtm_real),intent(inout) :: vol(n_resv)             !< Volume of the reservoirs
        real(gtm_real),intent(inout) :: mass_resv(n_resv,nvar)  !< Mass in the reservoirs
        integer :: i
        integer :: reservoir_id, resv_conn_id

        do i = 1, n_node
            if (dsm2_network(i)%junction_no .eq. 0) cycle
            if (dsm2_network_extra(i)%reservoir_no.ne.0) then
                reservoir_id = dsm2_network_extra(i)%reservoir_no
                resv_conn_id = dsm2_network_extra(i)%resv_conn_no
                vol(reservoir_id) = vol(reservoir_id) - resv_flow(resv_conn_id)*dt
                ! Flow going out of the reservoir
                if (resv_flow(resv_conn_id).gt.zero) then
                    mass_resv(reservoir_id,ivar) = mass_resv(reservoir_id,ivar) - resv_flow(resv_conn_id)*dt*conc_resv_prev(reservoir_id,ivar)
                    flux_in(i) = flux_in(i) + conc_resv_prev(reservoir_id,ivar)*resv_flow(resv_conn_id)
                    flow_tmp(i) = flow_tmp(i) + resv_flow(resv_conn_id)
                end if
            end if
        end do

        return
    end subroutine add_reservoir_flow

    subroutine update_external_flow_conc(conc_tmp, nvar, ivar)

        use constants
        use gtm_vars, only : n_node, dsm2_network, dsm2_network_extra, n_qext
        use state_variables_network, only : qext_flow, conc_qext
        use common_gtm_vars, only: pathinput
        implicit none
        real(gtm_real),intent(inout) :: conc_tmp(n_node, nvar)  !< Concentration at each node
        integer,intent(in)  :: nvar                             !< Number of variables
        integer,intent(in)  :: ivar                             !< variable index
        real(gtm_real) :: conc_ext, qext_fl
        integer :: i, j
        integer :: qext_path_id

        do i = 1, n_node
            if (dsm2_network(i)%junction_no .eq. 0) cycle
            if ((dsm2_network(i)%boundary_no.eq.0).and.(dsm2_network_extra(i)%n_qext.gt.0)) then
                ! loop through external flows
                do j = 1, dsm2_network_extra(i)%n_qext
                    qext_fl = qext_flow(dsm2_network_extra(i)%qext_no(j))
                    if (qext_fl > 0) then ! drain
                        ! If drain and if there are associated data to it
                        qext_path_id = dsm2_network_extra(i)%qext_path(j,ivar)
                        if (qext_path_id /= 0) then
                            conc_ext = pathinput(qext_path_id)%value
                            ! save the concentration at drains.
                            conc_qext(dsm2_network_extra(i)%qext_no(j),ivar) = conc_ext
                        else !drain but node concentration is absent
                            write(*,*) "WARNING: No node concentration is given for DSM2 Node No. !!",dsm2_network(i)%dsm2_node_no
                        end if
                    else if (qext_fl <= 0) then ! seepage or diversion
                        ! Save concentration for seepage and diversions.
                        conc_qext(dsm2_network_extra(i)%qext_no(j),ivar) = conc_tmp(i,ivar)
                    end if
                end do
            end if
        end do

        return

    end subroutine update_external_flow_conc

    subroutine assign_cell_face_conc(flux_lo, flux_hi, conc_tmp, flow_lo, flow_hi, ncell, nvar, ivar)
        use constants
        use gtm_vars, only : n_node, dsm2_network, dsm2_network_extra, n_qext
        use state_variables_network, only : conc_stip, prev_conc_stip
        implicit none
        real(gtm_real),intent(inout) :: flux_lo(ncell,nvar)             !< Flux on lo side of cell, time centered
        real(gtm_real),intent(inout) :: flux_hi(ncell,nvar)             !< Flux on hi side of cell, time centered
        real(gtm_real),intent(inout) :: conc_tmp(n_node, nvar)          !< Concentration at each node
        real(gtm_real),intent(in) :: flow_lo(ncell)                     !< Flow on lo side of cells centered in time
        real(gtm_real),intent(in) :: flow_hi(ncell)                     !< Flow on hi side of cells centered in time
        integer, intent(in) :: ncell                                    !< Number of cells
        integer, intent(in) :: nvar                                     !< Number of variables
        integer, intent(in) :: ivar                                     !< variable index
        integer :: i, j, icell
        do i = 1, n_node
            if (dsm2_network(i)%junction_no .eq. 0) cycle
            ! assign average concentration to downstream cell faces
            do j = 1, dsm2_network(i)%n_conn_cell
                icell = dsm2_network(i)%cell_no(j)
                prev_conc_stip(icell,ivar) = conc_stip(icell,ivar)
                conc_stip(icell,ivar) = LARGEREAL
                if ((dsm2_network(i)%up_down(j).eq.0) .and. (flow_hi(icell).le.zero)) then  !cell at updstream of junction and flow away from junction
                    flux_hi(icell,ivar) = conc_tmp(i,ivar)*flow_hi(icell)
                    conc_stip(icell,ivar) = conc_tmp(i,ivar)
                elseif ((dsm2_network(i)%up_down(j).eq.1) .and. (flow_lo(icell).ge.zero)) then !cell at downdstream of junction
                    flux_lo(icell,ivar) = conc_tmp(i,ivar)*flow_lo(icell)
                    conc_stip(icell,ivar) = conc_tmp(i,ivar)
                endif
            end do
        end do

        return
    end subroutine assign_cell_face_conc

    subroutine update_reservoir_conc(conc_tmp, vol, mass_resv, nvar, ivar, dt, use_previous_ts_val)

        use constants
        use gtm_vars, only: n_node, n_resv, dsm2_network, dsm2_network_extra
        use state_variables_network
        use common_gtm_vars
        implicit none
        integer,intent(in)  :: nvar                             !< Number of variables
        integer,intent(in)  :: ivar                             !< variable index
        integer,intent(in)  :: use_previous_ts_val              !< whether to use previous time step values
        real(gtm_real),intent(in)    :: dt                      !< Time step
        real(gtm_real),intent(inout) :: conc_tmp(n_node, nvar)  !< Concentration at each node
        real(gtm_real),intent(inout) :: vol(n_resv)             !< Volume of the reservoirs
        real(gtm_real),intent(inout) :: mass_resv(n_resv,nvar)  !< Mass in the reservoirs
        integer :: i,j
        integer :: reservoir_id, resv_conn_id

        if (use_previous_ts_val .eq. 1) then
            do i = 1, n_resv
                vol(i) = resv_geom(i)%area * million * (prev_resv_height(i)-resv_geom(i)%bot_elev)
                mass_resv(i,ivar) = vol(i) * conc_resv_prev(i,ivar)
            end do

        else if (use_previous_ts_val .eq. 0) then
            ! assign the average concentration to the reservoir
            do i = 1, n_node
                if (dsm2_network(i)%junction_no .eq. 0) cycle
                ! assign the average concentration to the reservoir
                if (dsm2_network_extra(i)%reservoir_no.ne.0) then
                    reservoir_id = dsm2_network_extra(i)%reservoir_no
                    resv_conn_id = dsm2_network_extra(i)%resv_conn_no
                    ! Flow going into the reservoir
                    if (resv_flow(resv_conn_id) < 0.0) then
                        mass_resv(reservoir_id,ivar) = mass_resv(reservoir_id,ivar) - resv_flow(resv_conn_id)*dt*conc_tmp(i,ivar)
                    end if
                end if
            end do

            do i = 1, n_resv
                if (resv_geom(i)%n_qext > 0) then
                    do j = 1, resv_geom(i)%n_qext
                        vol(i) = vol(i) + qext_flow(resv_geom(i)%qext_no(j))*dt
                        if (qext_flow(resv_geom(i)%qext_no(j)).gt.zero) then
                            mass_resv(i,ivar) = mass_resv(i,ivar) + dble(pathinput(resv_geom(i)%qext_path(j,ivar))%value)*qext_flow(resv_geom(i)%qext_no(j))*dt
                        else
                            mass_resv(i,ivar) = mass_resv(i,ivar) + conc_resv_prev(i,ivar)*qext_flow(resv_geom(i)%qext_no(j))*dt
                        end if
                    end do
                end if
                if (vol(i).gt.zero) then
                    conc_resv(i,ivar) = mass_resv(i,ivar)/vol(i)
                else
                    conc_resv(i,ivar) = conc_resv_prev(i,ivar)
                end if
            end do

        else
            write(*,*) "Error: invalid option for reservoir concentration calculation"
            call exit(-1)
        end if

        return

    end subroutine update_reservoir_conc

    subroutine compute_junction_conc(flux_in, flow_tmp, conc_tmp, nvar, ivar)

        use constants
        use state_variables_network, only: tran_flow
        use gtm_vars, only : n_node, dsm2_network, n_tran, tran, receiving_nodes, source_nodes
        implicit none
        integer,intent(in)  :: nvar                             !< Number of variables
        integer,intent(in)  :: ivar                             !< variable index
        real(gtm_real) :: flow_tmp(n_node),flux_in(n_node)
        real(gtm_real) :: conc_tmp(n_node, nvar)
        integer :: i,j,receiving_node,source_node

        ! compute concentration at all junctions except for nodes that reveive transfer flows
        do i = 1, n_node
            if (dsm2_network(i)%junction_no .gt. 0) then
                if (any(receiving_nodes == i)) cycle ! skips if the node receives transfer flow.
                ! Note: the assumption is a node can only receive or send transfer flows.
                if (flow_tmp(i) < 0.01) cycle ! skip for very small flow
                conc_tmp(i,ivar) = flux_in(i) / flow_tmp(i)
            end if
        end do

        ! update flow and flux at nodes receiving transfer flows, then compute concentration.
        do j = 1, n_tran
            receiving_node = receiving_nodes(j)
            source_node = source_nodes(j)

            flow_tmp(receiving_node) = flow_tmp(receiving_node) + tran_flow(j)
            flux_in(receiving_node) = flux_in(receiving_node) + conc_tmp(source_node,ivar) * tran_flow(j)

            if (flow_tmp(receiving_node) < 0.01) cycle  ! skip for very small flow
            conc_tmp(receiving_node,ivar) = flux_in(receiving_node) / flow_tmp(receiving_node)
        end do

        return

    end subroutine compute_junction_conc

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
                                         dx,         &
                                         tstp,       &
                                         sed_percent)
        use constants
        use error_handling
        use gtm_vars, only: n_node, n_tran, tran
        use state_variables_network
        implicit none
        !--- args
        integer,intent(in)  :: ncell                            !< Number of cells
        integer,intent(in)  :: nvar                             !< Number of variables
        integer,intent(in)  :: tstp                             !< Time step index
        real(gtm_real),intent(inout) :: flux_lo(ncell,nvar)     !< Flux on lo side of cell, time centered
        real(gtm_real),intent(inout) :: flux_hi(ncell,nvar)     !< Flux on hi side of cell, time centered
        real(gtm_real),intent(out) :: sed_percent(n_node,n_qext,nvar)!<percentages of compositions at boundaries  & 10 is the maximum number of
                                                                                 !external flows        !<TODO: make array dimensions effective
        real(gtm_real),intent(in)    :: flow_lo(ncell)          !< Flow on lo side of cells centered in time
        real(gtm_real),intent(in)    :: flow_hi(ncell)          !< Flow on hi side of cells centered in time
        real(gtm_real),intent(in)    :: conc_lo(ncell,nvar)     !< Concentration extrapolated to lo face
        real(gtm_real),intent(in)    :: conc_hi(ncell,nvar)     !< Concentration extrapolated to hi face
        real(gtm_real),intent(in)    :: time                    !< Current time
        real(gtm_real),intent(in)    :: dx(ncell)               !< Spatial step
        real(gtm_real),intent(in)    :: dt                      !< Time step
        real(gtm_real) :: flow_tmp(n_node), flux_in(n_node)
        real(gtm_real) :: conc_tmp(n_node, nvar)
        real(gtm_real) :: vol(n_resv)
        real(gtm_real) :: mass_resv(n_resv,nvar)
        integer :: i,ivar

        do ivar = 1, nvar

            ! initialize
            conc_tmp(:,ivar) = zero
            flow_tmp(:) = zero
            flux_in(:) = zero

            if (.not. constituents(ivar)%simulate) cycle

            ! recalculate concentration for reservoirs
            call update_reservoir_conc(conc_tmp, vol, mass_resv,nvar, ivar, dt, use_previous_ts_val=1)

            ! If this is the first time step of the run,
            ! build up a sediment fraction array at boundaries and external flows
            ! TODO: Need to double-check if this works for the restart as well.
            ! TODO: Hopefully we will modulize and move this out from the loop.
            if (tstp .eq. one) then
                call build_boundary_sediment_fraction_array(nvar, ivar, sed_percent)
            end if

            ! adjust flux for boundaries
            call adjust_boundary_fluxes(flux_lo, flux_hi, conc_lo, conc_hi, flow_lo, flow_hi, ncell, nvar, ivar)

            ! adjust flux for non-sequential adjacent cells
            ! BUG This works only when there are only two cells connected to the node
            call adjust_nonsequential_cell_fluxes(flux_lo, flux_hi, conc_lo, conc_hi, flow_lo, flow_hi, ncell, nvar, ivar)

            ! adjust flux for junctions
            call adjust_junction_fluxes(flow_tmp, flux_in, conc_lo, conc_hi, flow_lo, flow_hi, ncell, nvar, ivar)

            ! add external flows
            call add_external_flow(flow_tmp, flux_in, nvar, ivar, sed_percent)

            ! add reservoir flows
            call add_reservoir_flow(flow_tmp, flux_in, vol, mass_resv, nvar, ivar, dt)

            ! compute junction concentration
            call compute_junction_conc(flux_in, flow_tmp, conc_tmp, nvar, ivar)

            ! compute and store concentration at external flow locations
            call update_external_flow_conc(conc_tmp, nvar, ivar)

            ! assign concentration to cell faces
            call assign_cell_face_conc(flux_lo, flux_hi, conc_tmp, flow_lo, flow_hi, ncell, nvar, ivar)

            ! update reservoir concentration based on the new concentration at junctions
            call update_reservoir_conc(conc_tmp, vol, mass_resv, nvar, ivar, dt, use_previous_ts_val=0)

        end do

        return
    end subroutine


    !> No assignment for boundary flow and leave it as it is
    subroutine assign_boundary_concentration(conc_lo,  &
                                             conc_hi,  &
                                             ncell,    &
                                             nvar)
        use constants
        use error_handling
        use gtm_vars, only: n_node, dsm2_network, dsm2_network_extra, n_bfbs, bfbs, &
                                    n_sediment, n_sediment_bc, sediment, sediment_bc, n_node_ts
        use common_gtm_vars, only: n_inputpaths, pathinput
        use state_variables_network, only : node_conc, conc_stip, qext_flow
        implicit none
        integer, intent(in)  :: ncell                            !< Number of cells
        integer, intent(in)  :: nvar                             !< Number of variables
        real(gtm_real), intent(inout) :: conc_lo(ncell,nvar)     !< Concentration extrapolated to lo face
        real(gtm_real), intent(inout) :: conc_hi(ncell,nvar)     !< Concentration extrapolated to hi face
        integer :: i, j, k, s, st, icell, inode, qext_id, sed_id
        real(gtm_real) :: conc_ext(nvar)

        do i = 1, n_bfbs
            inode = bfbs(i)%i_node
            do j = 1, n_node_ts
                ! Skip meteorological inputs (cloud, dry_bulb, wet_bulb, wind, atm_pressure,! solar):
                ! they have i_var=0 because their variable name does not match any constituent.
                if (pathinput(j)%i_var <= 0) cycle
                if (pathinput(j)%i_no .eq. inode .and. dsm2_network(inode)%boundary_no.ne.0) then
                        if (trim(pathinput(j)%variable) .eq. 'ssc') then
                            node_conc(inode,pathinput(j)%i_var) = pathinput(j)%value
                            dsm2_network_extra(inode)%node_conc(pathinput(j)%i_var) = 1
                            do st = 1, n_sediment
                                do s = 1, n_sediment_bc
                                    if ((trim(pathinput(j)%name) .eq. trim(sediment_bc(s)%name)) .and. (trim(sediment(st)%composition) .eq. trim(sediment_bc(s)%composition))) then
                                        node_conc(inode,nvar-n_sediment+st) = pathinput(j)%value * sediment_bc(s)%percent * 0.01d0
                                        dsm2_network_extra(inode)%node_conc(nvar-n_sediment+st) = 1
                                    end if
                                end do
                            end do
                        else
                            node_conc(inode,pathinput(j)%i_var) = pathinput(j)%value
                            dsm2_network_extra(inode)%node_conc(pathinput(j)%i_var) = 1
                        end if
                        do k = 1, dsm2_network(inode)%n_conn_cell
                            icell = dsm2_network(inode)%cell_no(k)
                            if (trim(pathinput(j)%variable) .eq. 'ssc') then
                                conc_stip(icell,pathinput(j)%i_var) = node_conc(inode,pathinput(j)%i_var)
                                do st = 1, n_sediment
                                    conc_stip(icell,nvar-n_sediment+st) = node_conc(inode,nvar-n_sediment+st)
                                end do
                            else
                                conc_stip(icell,pathinput(j)%i_var) = node_conc(inode,pathinput(j)%i_var)
                            end if
                        end do
                end if
            end do
        end do

        !> Assign node concentration to the upstream boundaries that no node concentration is given.
        !> This will update state variables node_conc.
        do i = 1, n_node
            if (dsm2_network(i)%boundary_no > 0) then
                icell = dsm2_network(i)%cell_no(1)
                do j = 1, nvar
                    if (node_conc(i, j) == LARGEREAL) then
                        if (dsm2_network_extra(i)%n_qext > 0) then
                            do k = 1, dsm2_network_extra(i)%n_qext
                                if (qext_flow(dsm2_network_extra(i)%qext_no(k)) > 0) then ! if it is a drain
                                    qext_id = dsm2_network_extra(i)%qext_path(k,j)
                                    if (qext_id /= 0) then
                                        conc_ext(j) = pathinput(qext_id)%value
                                        conc_stip(icell,j) = conc_ext(j)
                                        node_conc(i,j) = conc_ext(j)
                                        if (trim(pathinput(qext_id)%variable).eq.'ssc') then
                                            do st = 1, n_sediment
                                                sed_id = nvar - n_sediment + st
                                                conc_ext(sed_id) = conc_ext(j) * sediment_bc(st)%percent * 0.01d0
                                                conc_stip(icell, sed_id) = conc_ext(sed_id)
                                                node_conc(i, sed_id) = conc_ext(sed_id)
                                            end do
                                        end if
                                    end if
                                end if
                            end do
                        else
                            if (dsm2_network(i)%up_down(1) .eq. 1) then
                                conc_stip(icell,j) = conc_hi(icell,j)
                                node_conc(i,j) = conc_hi(icell,j) ! upstream boundary
                            else
                                conc_stip(icell,j) = conc_lo(icell,j)
                                node_conc(i,j) = conc_lo(icell,j) ! downstream boundary
                            end if
                        end if
                    end if
                end do
            end if
        end do
        return
    end subroutine

end module
