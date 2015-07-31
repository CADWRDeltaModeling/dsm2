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

!> Defines state variables for the transport problem
!> as well as functions to allocate them
!>@ingroup gtm_driver
module state_variables_network

    use gtm_precision
    use common_variables

    !> State variables for data read from hydro
    real(gtm_real), allocatable :: prev_comp_flow(:)
    real(gtm_real), allocatable :: prev_comp_ws(:)
    real(gtm_real), allocatable :: prev_hydro_resv(:)
    real(gtm_real), allocatable :: prev_hydro_resv_flow(:)
    real(gtm_real), allocatable :: prev_hydro_qext(:)
    real(gtm_real), allocatable :: prev_hydro_tran(:)
    real(gtm_real), allocatable :: prev_flow_cell_lo(:)
    real(gtm_real), allocatable :: prev_flow_cell_hi(:)

    !> Concentration in the current/new time step for reservoir, 
    !> dimensions (nresv, nvar)
    real(gtm_real), save, allocatable :: conc_resv(:,:)
    
    !> Concentration in the previous time step,
    !> dimensions (nresv, nvar)
    real(gtm_real), save, allocatable :: prev_conc_resv(:,:)

    real(gtm_real), save, allocatable :: resv_height(:)
    real(gtm_real), save, allocatable :: resv_flow(:)
    real(gtm_real), save, allocatable :: qext_flow(:)
    real(gtm_real), save, allocatable :: tran_flow(:)
    real(gtm_real), save, allocatable :: node_conc(:,:)
    
    real(gtm_real), save, allocatable :: prev_resv_height(:)
    real(gtm_real), save, allocatable :: prev_resv_flow(:)
    real(gtm_real), save, allocatable :: prev_qext_flow(:)
    real(gtm_real), save, allocatable :: prev_tran_flow(:) 
    real(gtm_real), save, allocatable :: prev_node_conc(:,:)
 
    contains
    
    !> Allocate time series from hydro computational points
    subroutine allocate_state_hydro(a_comp, a_resv, a_resv_conn, a_qext, a_tran, a_cell)
        use error_handling
        implicit none
        integer, intent(in) :: a_comp
        integer, intent(in) :: a_resv
        integer, intent(in) :: a_resv_conn
        integer, intent(in) :: a_qext
        integer, intent(in) :: a_tran
        integer, intent(in) :: a_cell  
        integer :: istat = 0      
        character(LEN=128) :: message
        
        allocate(prev_comp_flow(n_comp), stat = istat)
        allocate(prev_comp_ws(n_comp), stat = istat)
        allocate(prev_hydro_resv(n_resv), stat = istat)
        allocate(prev_hydro_resv_flow(n_resv_conn), stat = istat)
        allocate(prev_hydro_qext(n_qext), stat = istat)
        allocate(prev_hydro_tran(n_tran), stat = istat)
        allocate(prev_flow_cell_lo(n_cell), prev_flow_cell_hi(n_cell), stat = istat)      
        if (istat .ne. 0 )then
           call gtm_fatal(message)
        end if
        prev_comp_flow = LARGEREAL
        prev_comp_ws = LARGEREAL
        prev_hydro_resv = LARGEREAL
        prev_hydro_resv_flow = LARGEREAL
        prev_hydro_qext = LARGEREAL
        prev_hydro_tran = LARGEREAL
        prev_flow_cell_lo = LARGEREAL
        prev_flow_cell_hi = LARGEREAL        
        return
    end subroutine    
    
    !> Allocate the state variables consistently for reservoir/qext/transfer flows.
    !> Initial value is LARGEREAL
    subroutine allocate_state_network(a_nresv,a_nresv_conn,a_nqext,a_ntran,a_nnode,a_nvar)
        use error_handling
        implicit none
        character(LEN=128) :: message
        integer :: istat = 0
        integer, intent(in) :: a_nresv      !< Number of requested 
        integer, intent(in) :: a_nresv_conn !< Number of requested
        integer, intent(in) :: a_nqext      !< Number of requested
        integer, intent(in) :: a_ntran      !< Number of requested
        integer, intent(in) :: a_nnode      !< Number of requested
        integer, intent(in) :: a_nvar       !< Number of constituents
        
        write(message,*)"Could not allocate state variable. " //&
         "This could be due to allocating several times in " // &
         "a row without deallocating (memory leak)"
        
        allocate(resv_height(a_nresv), stat = istat)
        allocate(prev_resv_height(a_nresv), stat = istat)
        if (istat .ne. 0 )then
           call gtm_fatal(message)
        end if
        resv_height        = LARGEREAL
        prev_resv_height   = LARGEREAL

        allocate(resv_flow(a_nresv_conn), stat = istat)
        allocate(prev_resv_flow(a_nresv_conn), stat = istat)
        if (istat .ne. 0 )then
           call gtm_fatal(message)
        end if
        resv_flow         = LARGEREAL
        prev_resv_flow    = LARGEREAL

        allocate(qext_flow(a_nqext), stat = istat)
        allocate(prev_qext_flow(a_nqext), stat = istat)
        if (istat .ne. 0 )then
           call gtm_fatal(message)
        end if
        qext_flow         = LARGEREAL
        prev_qext_flow    = LARGEREAL 

        allocate(tran_flow(a_ntran), stat = istat)
        allocate(prev_tran_flow(a_ntran), stat = istat)
        if (istat .ne. 0 )then
           call gtm_fatal(message)
        end if
        tran_flow         = LARGEREAL 
        prev_tran_flow    = LARGEREAL 
        
        allocate(conc_resv(a_nresv,a_nvar), stat = istat)
        allocate(prev_conc_resv(a_nresv,a_nvar), stat = istat)

        allocate(node_conc(a_nnode,a_nvar), stat = istat)
        allocate(prev_node_conc(a_nnode,a_nvar), stat = istat)
        node_conc = LARGEREAL 
        prev_node_conc = LARGEREAL 
         
        return
    end subroutine
    
    
    !> Deallocate the state variables for hydro
    !> time series reading
    subroutine deallocate_state_hydro   
        implicit none        
        deallocate(prev_comp_flow)
        deallocate(prev_comp_ws)
        deallocate(prev_hydro_resv)
        deallocate(prev_hydro_resv_flow)
        deallocate(prev_hydro_qext)
        deallocate(prev_hydro_tran)
        deallocate(prev_flow_cell_lo, prev_flow_cell_hi)
        return
    end subroutine
    
    
    !> Deallocate the state variables for nodes
    !> including concentration and hydrodynamics
    !> and reset ncell and nvar to zero.
    subroutine deallocate_state_network
        implicit none
        n_resv = 0
        n_resv_conn = 0
        n_qext = 0
        n_tran = 0
        n_node = 0
        n_var = 0
        deallocate(resv_height, prev_resv_height)
        deallocate(resv_flow, prev_resv_flow)
        deallocate(qext_flow, prev_qext_flow)
        deallocate(tran_flow, prev_tran_flow)
        deallocate(conc_resv, prev_conc_resv)
        deallocate(node_conc, prev_node_conc)
        return
    end subroutine    

end module



