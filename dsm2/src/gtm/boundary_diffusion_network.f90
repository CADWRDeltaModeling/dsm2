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
!> Boundary diffusive flux interface for network problem
!> to be fulfilled by driver or application
!>@ingroup gtm_driver
module boundary_diffusion_network

    use gtm_precision
    use error_handling
    use diffusion
    use boundary_diffusion
    use dispersion_coefficient
    use common_dsm2_vars, only: print_level
    real(gtm_real), allocatable, save :: disp_coef_arr(:)
    integer, allocatable :: rcind(:)        ! row and column count index
    integer, allocatable :: kin(:)          ! lookup index for aax() transpose
    integer, allocatable :: row(:)          ! row number
    integer, allocatable :: col(:)          ! column number
    integer, allocatable :: nco(:)          ! num of connected cells
    integer, allocatable :: ncc(:)          ! boundary concentration array index
    character(len=1), allocatable :: typ(:) ! cell type
    character(len=1), allocatable :: uds(:) ! up stream or downstream of node
    integer :: n_nonzero = LARGEINT
    integer :: print_matrix = 0

    contains

    !> Set diffusion flux and diffusion matrix pointers to diffusion boundary
    subroutine set_network_diffusion_boundary(network_bc_matrix)
        use error_handling
        implicit none
        procedure(boundary_diffusive_matrix_if), pointer :: network_bc_matrix  !< Diffusion matrix routine
        boundary_diffusion_matrix => network_bc_matrix
        return
    end subroutine


    !> Set dispersion coefficient interface to an implementation
    !> that is read from input file.
    subroutine set_dispersion_arr(d_arr,  &
                                  ncell)
        implicit none
        integer, intent(in) :: ncell              !< number of cells
        real(gtm_real),intent(in) :: d_arr(ncell) !< array of dispersion coefficients
        integer :: i, j

        allocate(disp_coef_arr(ncell))
        disp_coef_arr = d_arr
        return
    end subroutine

    !> Implementation of diffusion_coef_if that sets dipsersion
    !> by multiplying input dispersion coefficients with flow velocities
    subroutine adjust_dispersion_coef_with_velocity(disp_coef_lo,  &
                                                    disp_coef_hi,  &
                                                    flow,          &
                                                    flow_lo,       &
                                                    flow_hi,       &
                                                    area,          &
                                                    area_lo,       &
                                                    area_hi,       &
                                                    time,          &
                                                    dx,            &
                                                    dt,            &
                                                    ncell,         &
                                                    nvar)
        use gtm_precision
        use error_handling
        use common_variables, only: n_gate, gate, dsm2_network
        implicit none
        !--- args
        integer,intent(in)  :: ncell                     !< Number of cells
        integer,intent(in)  :: nvar                      !< Number of variables
        real(gtm_real),intent(in) :: time                !< Current time
        real(gtm_real),intent(in) :: dx(ncell)           !< Spatial step
        real(gtm_real),intent(in) :: dt                  !< Time step
        real(gtm_real),intent(in) :: flow_lo(ncell)      !< flow on lo side of cells centered in time
        real(gtm_real),intent(in) :: flow_hi(ncell)      !< flow on hi side of cells centered in time
        real(gtm_real),intent(in) :: flow(ncell)         !< flow on center of cells
        real(gtm_real),intent(in) :: area_lo(ncell)      !< area on lo side of cells centered in time
        real(gtm_real),intent(in) :: area_hi(ncell)      !< area on hi side of cells centered in time
        real(gtm_real),intent(in) :: area(ncell)         !< area on center of cells
        real(gtm_real),intent(out):: disp_coef_lo(ncell) !< Low side constituent dispersion coef.
        real(gtm_real),intent(out):: disp_coef_hi(ncell) !< High side constituent dispersion coef.
        integer :: i, j, inode

        disp_coef_hi = disp_coef_arr*abs(flow_hi/area_hi)
        disp_coef_lo = disp_coef_arr*abs(flow_lo/area_lo)

        do i = 1, n_gate
            if (gate(i)%from_obj_int .eq. 1) then   ! from_obj_int = 1: channel
                inode = gate(i)%to_node_int
                do j = 1, dsm2_network(inode)%n_conn_cell
                    if (dsm2_network(inode)%up_down(j) .eq. 0) then   !cell at upstream of junction
                        disp_coef_hi(dsm2_network(inode)%cell_no(j)) = zero
                    else
                        disp_coef_lo(dsm2_network(inode)%cell_no(j)) = zero
                    end if
                end do
            end if
        end do

        return
    end subroutine

    !> Implementation of diffusion_coef_if that sets dipsersion
    !> by directly using input dispersion coefficients
    subroutine assign_dispersion_coef(disp_coef_lo,  &
                                      disp_coef_hi,  &
                                      flow,          &
                                      flow_lo,       &
                                      flow_hi,       &
                                      area,          &
                                      area_lo,       &
                                      area_hi,       &
                                      time,          &
                                      dx,            &
                                      dt,            &
                                      ncell,         &
                                      nvar)
        use gtm_precision
        use error_handling
        use common_variables, only: n_gate, gate, dsm2_network
        implicit none
        !--- args
        integer,intent(in)  :: ncell                     !< Number of cells
        integer,intent(in)  :: nvar                      !< Number of variables
        real(gtm_real),intent(in) :: time                !< Current time
        real(gtm_real),intent(in) :: dx(ncell)           !< Spatial step
        real(gtm_real),intent(in) :: dt                  !< Time step
        real(gtm_real),intent(in) :: flow_lo(ncell)      !< flow on lo side of cells centered in time
        real(gtm_real),intent(in) :: flow_hi(ncell)      !< flow on hi side of cells centered in time
        real(gtm_real),intent(in) :: flow(ncell)         !< flow on center of cells
        real(gtm_real),intent(in) :: area_lo(ncell)      !< area on lo side of cells centered in time
        real(gtm_real),intent(in) :: area_hi(ncell)      !< area on hi side of cells centered in time
        real(gtm_real),intent(in) :: area(ncell)         !< area on center of cells
        real(gtm_real),intent(out):: disp_coef_lo(ncell) !< Low side constituent dispersion coef.
        real(gtm_real),intent(out):: disp_coef_hi(ncell) !< High side constituent dispersion coef.
        integer :: i, j, inode

        disp_coef_hi = disp_coef_arr
        disp_coef_lo = disp_coef_arr

        do i = 1, n_gate
            if (gate(i)%from_obj_int .eq. 1) then   ! from_obj_int = 1: channel
                inode = gate(i)%to_node_int
                do j = 1, dsm2_network(inode)%n_conn_cell
                    if (dsm2_network(inode)%up_down(j) .eq. 0) then   !cell at upstream of junction
                        disp_coef_hi(dsm2_network(inode)%cell_no(j)) = zero
                    else
                        disp_coef_lo(dsm2_network(inode)%cell_no(j)) = zero
                    end if
                end do
            end if
        end do

        return
    end subroutine


    !> Adjustments for network diffusion boundary flux
    subroutine network_boundary_diffusive_flux(diffusive_flux_lo, &
                                               diffusive_flux_hi, &
                                               conc,              &
                                               area_lo,           &
                                               area_hi,           &
                                               disp_coef_lo,      &
                                               disp_coef_hi,      &
                                               ncell,             &
                                               nvar,              &
                                               time,              &
                                               dx,                &
                                               dt)
        use gtm_precision
        use error_handling
        use common_variables, only : n_node, n_var, dsm2_network, dsm2_network_extra, constituents
        use state_variables_network, only : node_conc, conc_stip
        implicit none
        !--- args
        integer, intent(in)  :: ncell                               !< Number of cells
        integer, intent(in)  :: nvar                                !< Number of variables
        real(gtm_real),intent(inout):: diffusive_flux_lo(ncell,nvar)!< Face flux, lo side
        real(gtm_real),intent(inout):: diffusive_flux_hi(ncell,nvar)!< Face flux, hi side
        real(gtm_real),intent(in)   :: area_lo(ncell)               !< Low side area centered at time
        real(gtm_real),intent(in)   :: area_hi(ncell)               !< High side area centered at time
        real(gtm_real),intent(in)   :: time                         !< Time
        real(gtm_real),intent(in)   :: conc(ncell,nvar)             !< Concentration
        real(gtm_real),intent(in)   :: disp_coef_lo(ncell)          !< Low side constituent dispersion coef.
        real(gtm_real),intent(in)   :: disp_coef_hi(ncell)          !< High side constituent dispersion coef.
        real(gtm_real),intent(in)   :: dt                           !< Spatial step
        real(gtm_real),intent(in)   :: dx(ncell)                    !< Time step
        integer :: i, j, k, icell
        integer :: up_cell, down_cell
        real(gtm_real) :: extrp

        do k = 1, n_var
          if (constituents(k)%simulate) then
          do i = 1, n_node
            if ( dsm2_network(i)%boundary_no .ne. 0 ) then   ! if boundary and node concentration is given
                icell = dsm2_network(i)%cell_no(1)
                if ( dsm2_network(i)%up_down(1) .eq. 1 ) then   ! upstream boundary
                    extrp = conc(icell,k)-half*(conc(icell+1,k)-conc(icell,k))
                    diffusive_flux_lo(icell,k) = minus*area_lo(icell)*disp_coef_lo(icell)*  &
                                                (conc(icell,k)-extrp)/(half*dx(icell))
                else                                            ! downstream boundary
                    extrp = conc(icell,k)+half*(conc(icell,k)-conc(icell-1,k))
                    diffusive_flux_hi(icell,k) = minus*area_hi(icell)*disp_coef_hi(icell)*  &
                                                (extrp-conc(icell,k))/(half*dx(icell))
                end if
            end if
            if ((dsm2_network(i)%junction_no .ne. 0) .and. (dsm2_network(i)%n_conn_cell .gt. 2)) then
                do j = 1, dsm2_network(i)%n_conn_cell
                   icell = dsm2_network(i)%cell_no(j)
                   if (dsm2_network(i)%up_down(j) .eq. 0) then  !cell at upstream of junction
                       diffusive_flux_hi(icell,k) = zero
                   else                                         !cell at downstream of junction
                       diffusive_flux_lo(icell,k) = zero
                   end if
                end do
            end if
            if (dsm2_network(i)%nonsequential==1) then
                if (dsm2_network(i)%up_down(1) .eq. 0) then   !cell at upstream of junction
                    up_cell = dsm2_network(i)%cell_no(1)
                    down_cell = dsm2_network(i)%cell_no(2)
                else                                          !cell at downstream of junction
                    up_cell = dsm2_network(i)%cell_no(2)
                    down_cell = dsm2_network(i)%cell_no(1)
                end if
                diffusive_flux_hi(up_cell,k) = -(area_hi(up_cell)*disp_coef_hi(up_cell)*       &
                             (conc(down_cell,k) - conc(up_cell,k)))/(half*dx(down_cell)+half*dx(up_cell))
                diffusive_flux_lo(down_cell,k) = -(area_lo(down_cell)*disp_coef_lo(down_cell)* &
                             (conc(down_cell,k) - conc(up_cell,k)))/(half*dx(down_cell)+half*dx(up_cell))
            end if
          end do
          end if
        end do
        return
    end subroutine


    !> Adjustments for network diffusion boundary flux
    subroutine network_boundary_diffusive_dirichlet_flux(diffusive_flux_lo, &
                                                          diffusive_flux_hi, &
                                                          conc,              &
                                                          area_lo,           &
                                                          area_hi,           &
                                                          disp_coef_lo,      &
                                                          disp_coef_hi,      &
                                                          ncell,             &
                                                          nvar,              &
                                                          time,              &
                                                          dx,                &
                                                          dt)
        use gtm_precision
        use error_handling
        use common_variables, only : n_node, n_var, dsm2_network, dsm2_network_extra
        use state_variables_network, only : node_conc, conc_stip
        implicit none
        !--- args
        integer, intent(in)  :: ncell                               !< Number of cells
        integer, intent(in)  :: nvar                                !< Number of variables
        real(gtm_real),intent(inout):: diffusive_flux_lo(ncell,nvar)!< Face flux, lo side
        real(gtm_real),intent(inout):: diffusive_flux_hi(ncell,nvar)!< Face flux, hi side
        real(gtm_real),intent(in)   :: area_lo(ncell)               !< Low side area centered at time
        real(gtm_real),intent(in)   :: area_hi(ncell)               !< High side area centered at time
        real(gtm_real),intent(in)   :: time                         !< Time
        real(gtm_real),intent(in)   :: conc(ncell,nvar)             !< Concentration
        real(gtm_real),intent(in)   :: disp_coef_lo(ncell)          !< Low side constituent dispersion coef.
        real(gtm_real),intent(in)   :: disp_coef_hi(ncell)          !< High side constituent dispersion coef.
        real(gtm_real),intent(in)   :: dt                           !< Spatial step
        real(gtm_real),intent(in)   :: dx(ncell)                    !< Time step
        integer :: i, j, k, icell
        integer :: up_cell, down_cell
        real(gtm_real) :: extrp

        do k = 1, n_var
          do i = 1, n_node
            if ( dsm2_network(i)%boundary_no .ne. 0 ) then   ! if boundary and node concentration is given
                icell = dsm2_network(i)%cell_no(1)
                if ( dsm2_network(i)%up_down(1) .eq. 1 ) then   ! upstream boundary
                    extrp = conc(icell,k)-half*(conc(icell+1,k)-conc(icell,k))
                    diffusive_flux_lo(icell,k) = minus*area_lo(icell)*disp_coef_lo(icell)*  &
                                                (conc(icell,k)-extrp)/(half*dx(icell))
                else                                            ! downstream boundary
                    extrp = conc(icell,k)+half*(conc(icell,k)-conc(icell-1,k))
                    diffusive_flux_hi(icell,k) = minus*area_hi(icell)*disp_coef_hi(icell)*  &
                                                (extrp-conc(icell,k))/(half*dx(icell))
                end if
            end if
            if ((dsm2_network(i)%junction_no .ne. 0) .and. (dsm2_network(i)%n_conn_cell .gt. 2)) then
                do j = 1, dsm2_network(i)%n_conn_cell
                   icell = dsm2_network(i)%cell_no(j)
                   if (dsm2_network(i)%up_down(j) .eq. 0) then  !cell at upstream of junction
                       if (conc_stip(icell,k).ne.LARGEREAL) then
                           extrp = conc_stip(icell,k)
                       else
                           extrp = conc(icell,k)+half*(conc(icell,k)-conc(icell-1,k))
                       end if
                       diffusive_flux_hi(icell,k) = minus*area_hi(icell)*disp_coef_hi(icell)*  &
                                                (extrp-conc(icell,k))/(half*dx(icell))
                   else                                         !cell at downstream of junction
                       if (conc_stip(icell,k).ne.LARGEREAL) then
                           extrp = conc_stip(icell,k)
                       else
                           extrp = conc(icell,k)-half*(conc(icell+1,k)-conc(icell,k))
                       end if
                       diffusive_flux_lo(icell,k) = minus*area_lo(icell)*disp_coef_lo(icell)*  &
                                                   (conc(icell,k)-extrp)/(half*dx(icell))
                   end if
                end do
            end if
            if (dsm2_network(i)%nonsequential==1) then
                if (dsm2_network(i)%up_down(1) .eq. 0) then   !cell at upstream of junction
                    up_cell = dsm2_network(i)%cell_no(1)
                    down_cell = dsm2_network(i)%cell_no(2)
                else                                          !cell at downstream of junction
                    up_cell = dsm2_network(i)%cell_no(2)
                    down_cell = dsm2_network(i)%cell_no(1)
                end if
                diffusive_flux_hi(up_cell,k) = -(area_hi(up_cell)*disp_coef_hi(up_cell)*       &
                             (conc(down_cell,k) - conc(up_cell,k)))/(half*dx(down_cell)+half*dx(up_cell))
                diffusive_flux_lo(down_cell,k) = -(area_lo(down_cell)*disp_coef_lo(down_cell)* &
                             (conc(down_cell,k) - conc(up_cell,k)))/(half*dx(down_cell)+half*dx(up_cell))
            end if
          end do
        end do
        return
    end subroutine


    !> compute diffusive flux at old time
    subroutine network_boundary_diffusive_flux_prev(diffusive_flux_lo, &
                                                    diffusive_flux_hi, &
                                                    conc,              &
                                                    area_lo,           &
                                                    area_hi,           &
                                                    disp_coef_lo,      &
                                                    disp_coef_hi,      &
                                                    ncell,             &
                                                    nvar,              &
                                                    time,              &
                                                    dx,                &
                                                    dt)
        use gtm_precision
        use common_variables, only : n_node, dsm2_network, dsm2_network_extra, constituents
        use state_variables_network, only : prev_node_conc, prev_conc_stip
        implicit none
        !--- args
        integer,intent(in)  :: ncell                                 !< Number of cells
        integer,intent(in)  :: nvar                                  !< Number of variables
        real(gtm_real),intent(inout) :: diffusive_flux_lo(ncell,nvar)  !< lo face diffusive flux at old time
        real(gtm_real),intent(inout) :: diffusive_flux_hi(ncell,nvar)  !< hi face diffusive flux at old time
        real(gtm_real),intent(in) :: conc(ncell,nvar)                !< cell centered conc at old time
        real(gtm_real),intent(in) :: area_lo(ncell)                  !< low face area at old time
        real(gtm_real),intent(in) :: area_hi(ncell)                  !< high face area at old time
        real(gtm_real),intent(in) :: disp_coef_lo(ncell)             !< low face disp coef at old time
        real(gtm_real),intent(in) :: disp_coef_hi(ncell)             !< high face disp coef at old time
        real(gtm_real),intent(in) :: time                            !< time
        real(gtm_real),intent(in) :: dt                              !< length of current time step being advanced
        real(gtm_real),intent(in) :: dx(ncell)                       !< spatial step

        !----- locals
        integer :: i, j, k, icell, up_cell, down_cell
        real(gtm_real) :: extrp

        do k = 1, nvar
          if (constituents(k)%simulate) then
          do i = 1, n_node
            if ( dsm2_network(i)%boundary_no .ne. 0 ) then   ! if boundary and node concentration is given
                icell = dsm2_network(i)%cell_no(1)
                if ( dsm2_network(i)%up_down(1) .eq. 1 ) then   ! upstream boundary
                    extrp = conc(icell,k)-half*(conc(icell+1,k)-conc(icell,k))
                    diffusive_flux_lo(icell,:) = minus*area_lo(icell)*disp_coef_lo(icell)*  &
                                                (conc(icell,k)-extrp)/(half*dx(icell))
                else                                            ! downstream boundary
                    extrp = conc(icell,k)+half*(conc(icell,k)-conc(icell-1,k))
                    diffusive_flux_hi(icell,k) = minus*area_hi(icell)*disp_coef_hi(icell)*  &
                                                (extrp-conc(icell,k))/(half*dx(icell))
                end if
            end if
            if ((dsm2_network(i)%junction_no .ne. 0) .and. (dsm2_network(i)%n_conn_cell .gt. 2)) then
                do j = 1, dsm2_network(i)%n_conn_cell
                   icell = dsm2_network(i)%cell_no(j)
                   if (dsm2_network(i)%up_down(j) .eq. 0) then  !cell at upstream of junction
                       diffusive_flux_hi(icell,k) = zero
                   else                                         !cell at downstream of junction
                       diffusive_flux_lo(icell,k) = zero
                   end if
                end do
            end if
            if (dsm2_network(i)%nonsequential==1) then
                if (dsm2_network(i)%up_down(1) .eq. 0) then   !cell at upstream of junction
                    up_cell = dsm2_network(i)%cell_no(1)
                    down_cell = dsm2_network(i)%cell_no(2)
                else                                          !cell at downstream of junction
                    up_cell = dsm2_network(i)%cell_no(2)
                    down_cell = dsm2_network(i)%cell_no(1)
                end if
                diffusive_flux_hi(up_cell,:) = -(area_hi(up_cell)*disp_coef_hi(up_cell)*       &
                             (conc(down_cell,k) - conc(up_cell,k)))/(half*dx(down_cell)+half*dx(up_cell))
                diffusive_flux_lo(down_cell,k) = -(area_lo(down_cell)*disp_coef_lo(down_cell)* &
                             (conc(down_cell,k) - conc(up_cell,k)))/(half*dx(down_cell)+half*dx(up_cell))
            end if
          end do
        end if
        end do
        return
    end subroutine


    !> Adjustments for network diffusion matrix by assuming zero diffusive flux across junctions
    subroutine network_diffusion_diag_matrix_zero_at_junctions(center_diag,        &
                                                               up_diag,            &
                                                               down_diag,          &
                                                               right_hand_side,    &
                                                               explicit_diffuse_op,&
                                                               conc_prev,          &
                                                               mass,               &
                                                               area_lo_prev,       &
                                                               area_hi_prev,       &
                                                               disp_coef_lo_prev,  &
                                                               disp_coef_hi_prev,  &
                                                               conc,               &
                                                               flow_lo,            &
                                                               flow_hi,            &
                                                               area,               &
                                                               area_lo,            &
                                                               area_hi,            &
                                                               disp_coef_lo,       &
                                                               disp_coef_hi,       &
                                                               theta_gtm,          &
                                                               ncell,              &
                                                               time,               &
                                                               nvar,               &
                                                               dx,                 &
                                                               dt)
        use gtm_precision
        use error_handling
        use common_variables, only : n_node, dsm2_network, constituents
        use state_variables_network, only : node_conc, prev_node_conc, conc_stip, prev_conc_stip
        implicit none
        !--- args
        integer, intent(in) :: ncell                                   !< Number of cells
        integer, intent(in) :: nvar                                    !< Number of variables
        real(gtm_real), intent(inout) :: down_diag(ncell)              !< Values of the coefficients below diagonal in matrix
        real(gtm_real), intent(inout) :: center_diag(ncell)            !< Values of the coefficients at the diagonal in matrix
        real(gtm_real), intent(inout) :: up_diag(ncell)                !< Values of the coefficients above the diagonal in matrix
        real(gtm_real), intent(inout):: right_hand_side(ncell,nvar)    !< Values of the coefficients of the right hand side
        real(gtm_real), intent(in) :: conc_prev(ncell, nvar)           !< concentration from previous time step
        real(gtm_real), intent(in) :: mass(ncell,nvar)                 !< mass from mass_prev+mass_divergence_advect
        real(gtm_real), intent(in) :: area_lo_prev(ncell)              !< Low side area at previous time
        real(gtm_real), intent(in) :: area_hi_prev(ncell)              !< High side area at previous time
        real(gtm_real), intent(in) :: disp_coef_lo_prev(ncell)         !< Low side constituent dispersion coef. at previous time
        real(gtm_real), intent(in) :: disp_coef_hi_prev(ncell)         !< High side constituent dispersion coef. at previous time
        real(gtm_real), intent(in) :: conc(ncell,nvar)                 !< Concentration
        real(gtm_real), intent(in) :: explicit_diffuse_op(ncell,nvar)  !< Explicit diffusive operator
        real(gtm_real), intent(in) :: flow_lo(ncell)                   !< Low side flow at new time
        real(gtm_real), intent(in) :: flow_hi(ncell)                   !< High side flow at new time
        real(gtm_real), intent(in) :: area (ncell)                     !< Cell centered area at new time
        real(gtm_real), intent(in) :: area_lo(ncell)                   !< Low side area at new time
        real(gtm_real), intent(in) :: area_hi(ncell)                   !< High side area at new time
        real(gtm_real), intent(in) :: disp_coef_lo(ncell)              !< Low side constituent dispersion coef. at new time
        real(gtm_real), intent(in) :: disp_coef_hi(ncell)              !< High side constituent dispersion coef. at new time
        real(gtm_real), intent(in) :: time                             !< Current time
        real(gtm_real), intent(in) :: theta_gtm                        !< Explicitness coefficient; 0 is explicit, 0.5 Crank-Nicolson, 1 full implicit
        real(gtm_real), intent(in) :: dx(ncell)                        !< Spatial step
        real(gtm_real), intent(in) :: dt                               !< Time step
        !---local
        real(gtm_real) :: x_int
        integer :: i, j, k, kk, h, icell, ivar
        integer :: up_cell, down_cell
        integer :: u_cell, n_cell, d_cell
        real(gtm_real) :: exp_diffusion_op_plus(nvar)
        real(gtm_real) :: exp_diffusion_op_minus(nvar)
        real(gtm_real), allocatable :: matrix(:,:)
        real(gtm_real) :: disp, extrp_prev(nvar)
        real(gtm_real) :: prev_nc(nvar), nc(nvar)

        j = 1
        do i = 1, ncell
            if (typ(j).eq."u") then              ! "u": upstream boundary
                x_int = half*(dx(i+1)+dx(i))
                down_diag(i) = zero
                do k = j, j + 1
                    if (uds(k).eq."n") then
                        center_diag(i) = area(i)+theta_gtm*dt*area_lo(i)*disp_coef_lo(i)/(dx(i)*dx(i)*half) &
                                        +theta_gtm*dt*area_hi(i)*disp_coef_hi(i)/(dx(i)*x_int)
                    elseif (uds(k).eq."d") then
                        up_diag(i) = -theta_gtm*dt*area_hi(i)*disp_coef_hi(i)/(dx(i)*x_int)
                    end if
                end do
                do ivar = 1, nvar
                extrp_prev(ivar) = conc_prev(i,ivar)-half*(conc_prev(i+1,ivar)-conc_prev(i,ivar))
                exp_diffusion_op_plus(ivar)  = -(one-theta_gtm)*dt*area_hi_prev(i)*disp_coef_hi_prev(i) &
                                            *(conc_prev(i+1,ivar)-conc_prev(i,ivar))/x_int/dx(i)
                exp_diffusion_op_minus(ivar) = -(one-theta_gtm)*dt*area_lo_prev(i)*disp_coef_lo_prev(i) &
                                            *(conc_prev(i,ivar)-extrp_prev(ivar))/(half*dx(i))/dx(i)
                right_hand_side(i,ivar) = mass(i,ivar) - (exp_diffusion_op_plus(ivar)-exp_diffusion_op_minus(ivar)) &
                                       + theta_gtm*dt*area_lo(i)*disp_coef_lo(i)*extrp_prev(ivar)/(half*dx(i)*dx(i))
                enddo
                j = j + 2
            elseif (typ(j).eq."d") then         ! "d": downstream boundary
                x_int = half*(dx(i)+dx(i-1))
                up_diag(i) = zero
                do k = j, j + 1
                    if (uds(k).eq."n") then
                        center_diag(i) = area(i)+theta_gtm*dt*area_hi(i)*disp_coef_hi(i)/(dx(i)*dx(i)*half)   &
                                        +theta_gtm*dt*area_lo(i)*disp_coef_lo(i)/(dx(i)*x_int)
                    elseif (uds(k).eq."u") then
                        down_diag(i) = -theta_gtm*dt*area_lo(i)*disp_coef_lo(i)/(dx(i)*x_int)
                    end if
                end do
                do ivar = 1, nvar
                extrp_prev(ivar) = conc_prev(i,ivar)+half*(conc_prev(i,ivar)-conc_prev(i-1,ivar))
                exp_diffusion_op_plus(ivar) = -(one-theta_gtm)*dt*area_hi_prev(i)*disp_coef_hi_prev(i) &
                                            *(extrp_prev(ivar)-conc_prev(i,ivar))/(half*dx(i))/dx(i)
                exp_diffusion_op_minus(ivar) = -(one-theta_gtm)*dt*area_lo_prev(i)*disp_coef_lo_prev(i) &
                                         *(conc_prev(i,ivar)- conc_prev(i-1,ivar))/x_int/dx(i)
                right_hand_side(i,ivar) = mass(i,ivar) - (exp_diffusion_op_plus(ivar)-exp_diffusion_op_minus(ivar)) &
                                       + theta_gtm*dt*area_hi(i)*disp_coef_hi(i)*extrp_prev(ivar)/(half*dx(i)*dx(i))
                enddo
                j = j + 2
            elseif (typ(j).eq."h") then        ! "h": u/s of junction
                exp_diffusion_op_plus = zero
                exp_diffusion_op_minus = zero
                do k = j, j + nco(j) - 1
                    if (uds(k).eq."u") then
                        down_diag(i) = -theta_gtm*dt*area_lo(i)*disp_coef_lo(i)/dx(i)/(half*dx(i)+half*dx(i-1))
                    elseif (uds(k).eq."d" .or. uds(k).eq."f") then
                        up_diag(i) = zero
                    elseif (uds(k).eq."n") then
                        center_diag(i) = area(i) + theta_gtm*dt*area_lo(i)*disp_coef_lo(i)/dx(i)/(half*dx(i)+half*dx(i-1))
                    end if
                end do
                do ivar = 1, nvar
                exp_diffusion_op_minus(ivar) = -(one-theta_gtm)*dt*area_lo_prev(i)*disp_coef_lo_prev(i) &
                                            *(conc_prev(i,ivar)- conc_prev(i-1,ivar))/(half*dx(i)+half*dx(i-1))/dx(i)
                right_hand_side(i,ivar) = mass(i,ivar)-(exp_diffusion_op_plus(ivar)-exp_diffusion_op_minus(ivar))
                end do
                j = j + nco(j)
            elseif (typ(j).eq."l") then        ! "l": d/s of junction
                exp_diffusion_op_plus = zero
                exp_diffusion_op_minus = zero
                do k = j, j + nco(j) - 1
                    if (uds(k).eq."d") then
                        up_diag(i) = -theta_gtm*dt*area_hi(i)*disp_coef_hi(i)/dx(i)/(half*dx(i)+half*dx(i+1))
                    elseif ( uds(k).eq."u" .or. uds(k).eq."f") then
                        down_diag(i) = zero
                    elseif (uds(k).eq."n") then
                        center_diag(i) = area(i) + theta_gtm*dt*area_hi(i)*disp_coef_hi(i)/dx(i)/(half*dx(i)+half*dx(i+1))
                    end if
                end do
                do ivar = 1, nvar
                exp_diffusion_op_plus(ivar) = -(one-theta_gtm)*dt*area_hi_prev(i)*disp_coef_hi_prev(i) &
                                            *(conc_prev(i+1,ivar)- conc_prev(i,ivar))/(half*dx(i)+half*dx(i+1))/dx(i)
                right_hand_side(i,ivar) = mass(i,ivar)-(exp_diffusion_op_plus(ivar)-exp_diffusion_op_minus(ivar))
                end do
                j = j + nco(j)
            else                          ! "n": normal
                j = j + 3
            end if
        end do
        return
    end subroutine


  !> Adjustments for network diffusion matrix
    subroutine network_diffusion_sparse_geom(ncell)
        use gtm_precision
        use error_handling
        use common_variables, only : n_node, n_junc, dsm2_network
        use state_variables_network, only : node_conc
        use utils
        implicit none
        !--- args
        integer, intent(in) :: ncell            !< Number of cells

        !---local
        integer, parameter :: max_conn = 9
        integer :: ind(max_conn)
        integer :: a(ncell, max_conn)          ! cell id of associated cells to each cell
        integer :: aa(ncell, max_conn)         ! cell id of associated cells to each cell after sorting
        integer :: s(ncell)                    ! number of associated cells for each cell
        integer :: i, j, k, kk, icell, nconn   ! local variables
        character(len=1) :: t(ncell)           ! cell type. "u": upstream boundary, "d": downstream boundary
                                               !            "h": u/s of junction, "l": d/s of junction, "s": non-sequantial, "n": normal
        character(len=1) :: ud(ncell,max_conn) ! upstream or downstream of cell i, "u": upstream, "d":downstream, "n":exact cell
        character(len=1) :: uu(ncell,max_conn) ! temporary variable for ud before sorting
        integer :: nc(ncell)                   ! array index to obtain DSM2 boundary concentrations
        integer :: atmp(max_conn)              ! temporary array

        a(:,:) = LARGEINT
        a(2:ncell,1) = (/(j,j=1,ncell-1)/)
        a(1:ncell,2) = (/(j,j=1,ncell)/)
        a(1:ncell-1,3) = (/(j,j=2,ncell)/)
        s(1) = 2
        s(ncell) = 2
        s(2:ncell-1) = 3
        t = "n"
        ud(:,:) = "n"
        nc = 0

        do i = 1, n_node
            if ( dsm2_network(i)%boundary_no.ne.0 ) then
                icell = dsm2_network(i)%cell_no(1)
                s(icell) = 2
                nc(icell) = i
                if (dsm2_network(i)%up_down(1) .eq. 1) then     ! upstream boundary
                    t(icell) = "u"
                    a(icell,:) = LARGEINT
                    a(icell,1) = icell
                    a(icell,2) =  icell+1
                    ud(icell,1) = "n"
                    ud(icell,2) = "d"
                else                                            ! downstream boundary
                    t(icell) = "d"
                    a(icell,:) = LARGEINT
                    a(icell,1) = icell-1
                    a(icell,2) = icell
                    ud(icell,1) = "u"
                    ud(icell,2) = "n"
                end if
            end if
            if ((dsm2_network(i)%junction_no .ne. 0) .and. (dsm2_network(i)%n_conn_cell .gt. 2)) then
                nconn = dsm2_network(i)%n_conn_cell
                do j = 1, nconn
                    ind(1:max_conn) = (/ (j,j=1,max_conn,1) /)
                    icell = dsm2_network(i)%cell_no(j)
                    s(icell) = nconn + 1
                    if (dsm2_network(i)%up_down(j) .eq. 0) then  !cell at upstream of junction
                        t(icell) = "h"
                        a(icell,1) = icell-1
                        a(icell,2:nconn+1) = dsm2_network(i)%cell_no(1:nconn)
                        ud(icell,1) = "u"
                        ud(icell,2:nconn+1) = "d"
                        do k = 1, nconn
                            if ((dsm2_network(i)%up_down(k).eq.0).and.(k.ne.j)) ud(icell,k+1) = "f"
                        end do
                    else                        !cell at downstream of junction
                        t(icell) = "l"
                        a(icell,1:nconn) = dsm2_network(i)%cell_no(1:nconn)
                        a(icell,nconn+1) = icell+1
                        ud(icell,1:nconn) = "u"
                        ud(icell,nconn+1) = "d"
                        do k = 1, nconn
                            if ((dsm2_network(i)%up_down(k).eq.1).and.(k.ne.j)) ud(icell,k) = "f"
                        end do
                    end if
                    do k = 1, nconn+1
                        if (icell.eq.a(icell,k)) ud(icell,k) = "n"
                    end do
                    aa(icell,:) = a(icell,:)
                    uu(icell,:) = ud(icell,:)
                    call QsortCI(a(icell,:),ind)
                    a(icell,1:max_conn) = aa(icell,ind(1:max_conn))
                    ud(icell,1:max_conn) = uu(icell,ind(1:max_conn))
                end do
            end if
            if (dsm2_network(i)%nonsequential==1) then
               ind(1:max_conn) = (/ (j,j=1,max_conn,1) /)
               if (dsm2_network(i)%up_down(1) .eq. 0) then   !cell at upstream of node
                   icell = dsm2_network(i)%cell_no(1)
                   t(icell) = "s"
                   a(icell,1) = dsm2_network(i)%cell_no(1)-1
                   a(icell,2) = dsm2_network(i)%cell_no(1)
                   a(icell,3) = dsm2_network(i)%cell_no(2)
                   ud(icell,1) = "u"
                   ud(icell,2) = "n"
                   ud(icell,3) = "d"
                   aa(icell,:) = a(icell,:)
                   uu(icell,:) = ud(icell,:)
                   call data_sort(a(icell,:), max_conn, ind)
                   a(icell,1:max_conn) = aa(icell,ind(1:max_conn))
                   ud(icell,1:max_conn) = uu(icell,ind(1:max_conn))
                   icell = dsm2_network(i)%cell_no(2)
                   t(icell) = "s"
                   a(icell,1) = dsm2_network(i)%cell_no(1)
                   a(icell,2) = dsm2_network(i)%cell_no(2)
                   a(icell,3) = dsm2_network(i)%cell_no(2)+1
                   ud(icell,1) = "u"
                   ud(icell,2) = "n"
                   ud(icell,3) = "d"
                   aa(icell,:) = a(icell,:)
                   uu(icell,:) = ud(icell,:)
                   call data_sort(a(icell,:), max_conn, ind)
                   a(icell,1:max_conn) = aa(icell,ind(1:max_conn))
                   ud(icell,1:max_conn) = uu(icell,ind(1:max_conn))
                else                                          !cell at downstream of node
                   icell = dsm2_network(i)%cell_no(1)
                   t(icell) = "s"
                   a(icell,1) = dsm2_network(i)%cell_no(2)
                   a(icell,2) = dsm2_network(i)%cell_no(1)
                   a(icell,3) = dsm2_network(i)%cell_no(1)+1
                   ud(icell,1) = "u"
                   ud(icell,2) = "n"
                   ud(icell,3) = "d"
                   aa(icell,:) = a(icell,:)
                   uu(icell,:) = ud(icell,:)
                   call data_sort(a(icell,:), max_conn, ind)
                   a(icell,1:max_conn) = aa(icell,ind(1:max_conn))
                   ud(icell,1:max_conn) = uu(icell,ind(1:max_conn))
                   icell = dsm2_network(i)%cell_no(2)
                   t(icell) = "s"
                   a(icell,1) = dsm2_network(i)%cell_no(2)-1
                   a(icell,2) = dsm2_network(i)%cell_no(2)
                   a(icell,3) = dsm2_network(i)%cell_no(1)
                   ud(icell,1) = "u"
                   ud(icell,2) = "n"
                   ud(icell,3) = "d"
                   aa(icell,:) = a(icell,:)
                   uu(icell,:) = ud(icell,:)
                   call data_sort(a(icell,:), max_conn, ind)
                   a(icell,1:max_conn) = aa(icell,ind(1:max_conn))
                   ud(icell,1:max_conn) = uu(icell,ind(1:max_conn))
                end if
            end if
        end do

        n_nonzero = 0
        do i = 1, ncell
            do j = 1, max_conn
                if (a(i,j).ne.LARGEINT) n_nonzero = n_nonzero + 1
            end do
        enddo
        allocate(rcind(n_nonzero))
        allocate(kin(n_nonzero))
        allocate(row(n_nonzero))
        allocate(col(n_nonzero))
        allocate(nco(n_nonzero))
        allocate(typ(n_nonzero))
        allocate(uds(n_nonzero))
        allocate(ncc(n_nonzero))
        rcind = LARGEINT
        kin = LARGEINT
        row = LARGEINT
        col = LARGEINT
        nco = LARGEINT
        typ = "n"
        uds = "n"
        ncc = LARGEINT
        k = 0
        do i = 1, ncell
            do j = 1, max_conn
                if (a(i,j).ne.LARGEINT) then
                    k = k + 1
                    rcind(k) = k
                    row(k) = i
                    col(k) = a(i,j)
                    nco(k) = s(i)
                    typ(k) = t(i)
                    uds(k) = ud(i,j)
                    ncc(k) = nc(i)
                end if
            end do
        end do
        ! to calculate the lookup k index to assign aax to column direction instead of row direction
        kk = 0
        do i = 1, ncell
            do j = 1, n_nonzero
                if (col(j).eq.i) then
                    kk = kk + 1
                    kin(j) = kk
                end if
            end do
        end do
        !if (print_level .ge. 3) then
        !    do k = 1, n_nonzero
        !        write(101,'(5i7,2a7,i7)') rcind(k), kin(k), row(k), col(k), nco(k), typ(k), uds(k), ncc(k)
        !    end do
        !end if
        allocate(aap(ncell+1), aai(n_nonzero), aax(n_nonzero))
        call rowcol2apai(aap, aai, row, col, rcind, n_nonzero, ncell)
        return
    end subroutine


    !> deallocate global variables for sparse matrix
    subroutine deallocate_geom_arr()
        implicit none
        deallocate(aap, aai, aax)
        deallocate(rcind, row, col)
        deallocate(nco, ncc)
        deallocate(typ, uds)
        n_nonzero = LARGEINT
        return
    end subroutine


end module
