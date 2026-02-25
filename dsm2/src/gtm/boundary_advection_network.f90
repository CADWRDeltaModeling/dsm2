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
        integer :: up_cell, down_cell
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
                if (dsm2_network(i)%nonsequential.eq.1) then
                    ! Converging
                    if (dsm2_network(i)%up_down(1) .eq. 0) then   !cell at upstream of node
                        up_cell = dsm2_network(i)%cell_no(1)
                        down_cell = dsm2_network(i)%cell_no(2)
                        grad_hi(up_cell,ivar) = (vals(down_cell,ivar)-vals(up_cell,ivar))/          &
                                                (half*dx(down_cell)+half*dx(up_cell))
                        grad_hi(down_cell,ivar) = -grad_hi(up_cell,ivar)
                        grad_center(up_cell,ivar) = (vals(down_cell,ivar)-vals(up_cell-1,ivar))/    &
                                                    (half*dx(down_cell)+dx(up_cell)+half*dx(up_cell-1))
                        grad_center(down_cell,ivar) = (vals(up_cell,ivar)-vals(down_cell-1,ivar))/  &
                                                      (half*dx(down_cell-1)+dx(down_cell)+half*dx(up_cell))
                    ! Diverging
                    else                                          !cell at downstream of node
                        up_cell = dsm2_network(i)%cell_no(2)
                        down_cell = dsm2_network(i)%cell_no(1)
                        grad_lo(up_cell,ivar) = (vals(up_cell,ivar)-vals(down_cell,ivar))/          &
                                                (half*dx(down_cell)+half*dx(up_cell))
                        grad_lo(down_cell,ivar) = -grad_lo(up_cell,ivar)
                        grad_center(up_cell,ivar) = (vals(up_cell+1,ivar)-vals(down_cell,ivar))/    &
                                                    (half*dx(down_cell)+dx(up_cell)+half*dx(up_cell+1))
                        grad_center(down_cell,ivar) = (vals(down_cell+1,ivar) - vals(up_cell,ivar))/  &
                                                      (half*dx(up_cell)+dx(down_cell)+half*dx(down_cell+1))
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
            ! adjust gradient for cells around junctions
            elseif ((dsm2_network(i)%junction_no .ne. 0) .and. (dsm2_network(i)%n_conn_cell .gt. 2)) then
                do j = 1, dsm2_network(i)%n_conn_cell
                   icell = dsm2_network(i)%cell_no(j)
                   if (dsm2_network(i)%up_down(j) .eq. 0) then  ! cell at upstream of junction
                       grad(icell,:) = grad_lo(icell,:)
                   else
                       grad(icell,:) = grad_hi(icell,:)
                   end if
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
                                         dx,         &
                                         tstp,       &
                                         sed_percent)
        use constants
        use error_handling
        use gtm_vars, only: n_node, dsm2_network, n_resv, resv_geom, no_flow, gate_close, constituents
        use common_gtm_vars, only: pathinput
        use state_variables, only: conc_prev
        use state_variables_network
        implicit none
        !--- args
        integer,intent(in)  :: ncell                            !< Number of cells
        integer,intent(in)  :: nvar                             !< Number of variables
        integer,intent(in)  :: tstp                             !< Time step
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
        real(gtm_real) :: flow_tmp
        real(gtm_real) :: mass_tmp(nvar)
        real(gtm_real) :: conc_tmp(nvar)
        real(gtm_real) :: up_count
        real(gtm_real) :: vol(n_resv)
        real(gtm_real) :: mass_resv(n_resv,nvar)
        real(gtm_real) :: conc_tmp0(nvar)                       ! when no flow flows into junction, use this temp value.
        integer :: network_id
        integer :: i, j, k, s, st, icell, inode, ivar
        integer :: reservoir_id, resv_conn_id
        integer :: up_down
        real(gtm_real) :: conc_ext
        logical :: boundary_composition_not_found =.true.
        integer :: up_cell, down_cell
        real(gtm_real) :: flux_in
            !! total flux into the junction
        integer :: qext_path_id
            !! path ID for external flow (qext)
        real(gtm_real) :: qext_fl
            !! external flow (qext) value

        do ivar = 1, nvar
        if (constituents(ivar)%simulate) then
        ! recalculate concentration for reservoirs
        do i = 1, n_resv
            vol(i) = resv_geom(i)%area * million * (prev_resv_height(i)-resv_geom(i)%bot_elev)
            mass_resv(i,ivar) = vol(i) * conc_resv_prev(i,ivar)
        end do

        ! If this is the first time step of the run,
        ! build up a sediment fraction array at boundaries and external flows
        ! TODO: Need to double-check if this works for the restart as well.
        ! TODO: Hopefully we will modulize and move this out from the loop.
        if (tstp .eq. one) then
            do i = 1, n_node
                ! Loop thourgh external flows at node i
                do j = 1, dsm2_network_extra(i)%n_qext
                    ! If there are associated data to this external flow
                    if (dsm2_network_extra(i)%qext_path(j,ivar).ne.0) then
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
                    end if
                end do
            end do
        end if

        do i = 1, n_node
            ! adjust flux for boundaries
            up_down = dsm2_network(i)%up_down(1)
            if (dsm2_network(i)%boundary_no > 0) then
                icell = dsm2_network(i)%cell_no(1)
                ! Away from the node
                if ((up_down .eq. 1)) then
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
            end if
            ! adjust flux for non-sequential adjacent cells
            ! BUG This works only when there are only two cells connected to the node
            if (dsm2_network(i)%nonsequential.eq.1) then
                if (dsm2_network(i)%n_conn_cell > 2) then
                    write(*,*) 'Error: nonsequential with more than 2 connected cells is not supported'
                    call exit(-1)
                end if
                ! If the flow is toward to the node, meaning the node is the downstream of the cell (up_down == 0)
                if (up_down .eq. 0) then
                    up_cell = dsm2_network(i)%cell_no(1)
                    down_cell = dsm2_network(i)%cell_no(2)
                    ! up_cell fix, outflow
                    if (flow_hi(up_cell) .gt. zero) then
                        flux_hi(up_cell,ivar) = conc_hi(up_cell,ivar)*flow_hi(up_cell)
                    ! inflow
                    else
                        flux_hi(up_cell,ivar) = conc_hi(down_cell,ivar)*flow_hi(up_cell)
                    end if
                    ! down_cell fix, outflow
                    if (flow_hi(down_cell) .gt. zero) then
                        flux_hi(down_cell,ivar) = conc_hi(down_cell,ivar)*flow_hi(down_cell)
                    ! inflow
                    else
                        flux_hi(down_cell,ivar) = conc_hi(up_cell,ivar)*flow_hi(down_cell)
                    end if
                else
                    up_cell = dsm2_network(i)%cell_no(2)
                    down_cell = dsm2_network(i)%cell_no(1)
                    ! up_cell fix, inflow
                    if (flow_lo(up_cell) .gt. zero) then
                        flux_lo(up_cell,ivar) = conc_lo(down_cell,ivar)*flow_lo(up_cell)
                    ! outflow
                    else
                        flux_lo(up_cell,ivar) = conc_lo(up_cell,ivar)*flow_lo(up_cell)
                    end if
                    ! Flow at the downstream (low) end, inflow
                    if (flow_lo(down_cell) .gt. zero) then
                        flux_lo(down_cell,ivar) = conc_lo(up_cell,ivar)*flow_lo(down_cell)
                    ! outflow
                    else
                        flux_lo(down_cell,ivar) = conc_lo(down_cell,ivar)*flow_lo(down_cell)
                    end if
                end if
            end if
            ! adjust flux for junctions
            flow_tmp = zero
            mass_tmp(ivar) = zero
            conc_tmp(ivar) = zero
            conc_tmp0(ivar) = zero
            flux_in = zero
            do j = 1, dsm2_network(i)%n_conn_cell     ! counting flow into the junctions
                icell = dsm2_network(i)%cell_no(j)
                if (dsm2_network(i)%up_down(j).eq.0 .and. flow_hi(icell).gt.zero) then     !cell at downstream of junction
                    flux_in = flux_in + conc_hi(icell,ivar)*flow_hi(icell)
                    flow_tmp = flow_tmp + flow_hi(icell)
                elseif (dsm2_network(i)%up_down(j).eq.1 .and. flow_lo(icell).lt.zero) then !cell at upstream of junction
                    flux_in = flux_in - conc_lo(icell,ivar)*flow_lo(icell)
                    flow_tmp = flow_tmp - flow_lo(icell)
                endif
            end do

            ! add external flows
            if ((dsm2_network(i)%boundary_no.eq.0).and.(dsm2_network_extra(i)%n_qext.gt.0)) then
                ! loop through external flows
                do j = 1, dsm2_network_extra(i)%n_qext
                    qext_fl = qext_flow(dsm2_network_extra(i)%qext_no(j))
                    ! If drain and if there are associated data to it
                    qext_path_id = dsm2_network_extra(i)%qext_path(j,ivar)
                    if (qext_fl > 0) then ! drain
                        flow_tmp = flow_tmp + qext_fl
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
                            flux_in = flux_in + conc_ext * qext_fl
                        else !drain but node concentration is absent
                            flux_in = flux_in + conc_ext * qext_fl
                            write(*,*) "WARNING: No node concentration is given for DSM2 Node No. !!",dsm2_network(i)%dsm2_node_no
                        end if
                    end if
                end do
            end if

            ! add reservoir flows
            if (dsm2_network_extra(i)%reservoir_no.ne.0) then
                reservoir_id = dsm2_network_extra(i)%reservoir_no
                resv_conn_id = dsm2_network_extra(i)%resv_conn_no
                vol(reservoir_id) = vol(reservoir_id) - resv_flow(resv_conn_id)*dt
                ! Flow going out of the reservoir
                if (resv_flow(resv_conn_id).gt.zero) then
                    mass_resv(reservoir_id,ivar) = mass_resv(reservoir_id,ivar) - resv_flow(resv_conn_id)*dt*conc_resv_prev(reservoir_id,ivar)
                    flux_in = flux_in + conc_resv_prev(reservoir_id,ivar)*resv_flow(resv_conn_id)
                    flow_tmp = flow_tmp + resv_flow(resv_conn_id)
                end if
            end if

            if (flow_tmp < 0.01) then
                conc_tmp(ivar) = zero
            else
                conc_tmp(ivar) = flux_in / flow_tmp
            end if
            ! assign average concentration to downstream cell faces
            do j = 1, dsm2_network(i)%n_conn_cell
                icell = dsm2_network(i)%cell_no(j)
                prev_conc_stip(icell,ivar) = conc_stip(icell,ivar)
                conc_stip(icell,ivar) = LARGEREAL
                if ((dsm2_network(i)%up_down(j).eq.0) .and. (flow_hi(icell).le.zero)) then  !cell at updstream of junction and flow away from junction
                    flux_hi(icell,ivar) = conc_tmp(ivar)*flow_hi(icell)
                    conc_stip(icell,ivar) = conc_tmp(ivar)
                elseif ((dsm2_network(i)%up_down(j).eq.1) .and. (flow_lo(icell).ge.zero)) then !cell at downdstream of junction
                    flux_lo(icell,ivar) = conc_tmp(ivar)*flow_lo(icell)
                    conc_stip(icell,ivar) = conc_tmp(ivar)
                endif
            end do
            ! assign the average concentration to the reservoir
            if (dsm2_network_extra(i)%reservoir_no.ne.0) then
                reservoir_id = dsm2_network_extra(i)%reservoir_no
                resv_conn_id = dsm2_network_extra(i)%resv_conn_no
                ! Flow going into the reservoir
                if (resv_flow(resv_conn_id) < 0.0) then
                    mass_resv(reservoir_id,ivar) = mass_resv(reservoir_id,ivar) - resv_flow(resv_conn_id)*dt*conc_tmp(ivar)
                end if
            end if

        end do ! do through all nodes

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

        end if
        end do

        return
    end subroutine


    !> No assignment for boundary flow and leave it as it is
    subroutine assign_boundary_concentration(conc_lo,  &
                                             conc_hi,  &
                                             ncell,    &
                                             nvar,     &
                                             tstp)
        use constants
        use error_handling
        use gtm_vars, only: n_node, dsm2_network, dsm2_network_extra, n_bfbs, bfbs, &
                                    n_sediment, n_sediment_bc, sediment, sediment_bc, n_node_ts
        use common_gtm_vars, only: n_inputpaths, pathinput
        use state_variables_network, only : node_conc, conc_stip
        implicit none
        integer, intent(in)  :: ncell                            !< Number of cells
        integer, intent(in)  :: nvar                             !< Number of variables
        integer, intent(in)  :: tstp
        real(gtm_real), intent(inout) :: conc_lo(ncell,nvar)     !< Concentration extrapolated to lo face
        real(gtm_real), intent(inout) :: conc_hi(ncell,nvar)     !< Concentration extrapolated to hi face
        integer :: i, j, k, s, st, icell, inode

        do i = 1, n_bfbs
            inode = bfbs(i)%i_node
            do j = 1, n_node_ts
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
                    if (dsm2_network(i)%up_down(1) .eq. 1 .and. node_conc(i,j).eq.LARGEREAL) then
                        conc_stip(icell,j) = conc_hi(icell,j)
                        node_conc(i,j) = conc_hi(icell,j) ! upstream boundary
                    end if
                    if (dsm2_network(i)%up_down(1) .eq. 0 .and. node_conc(i,j).eq.LARGEREAL) then
                        conc_stip(icell,j) = conc_lo(icell,j)
                        node_conc(i,j) = conc_lo(icell,j) ! downstream boundary
                    end if
                end do
            end if
        end do
        return
    end subroutine

end module
