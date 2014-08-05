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

!> Defines state variables for the transport problem
!> as well as functions to allocate them
!>@ingroup transport
module state_variables_network

    use gtm_precision
    use common_variables

    !> Concentration in the current/new time step for reservoir, 
    !> dimensions (nresv, nvar)
    real(gtm_real), save, allocatable :: conc_resv(:,:)
    
    !> Concentration in the previous time step,
    !> dimensions (nresv, nvar)
    real(gtm_real), save, allocatable :: conc_resv_prev(:,:)

    real(gtm_real), save, allocatable :: resv_height(:)
    real(gtm_real), save, allocatable :: resv_flow(:)
    real(gtm_real), save, allocatable :: qext_flow(:)
    real(gtm_real), save, allocatable :: tran_flow(:)
        
    contains
    
    !> Allocate the state variables consistently for reservoir/qext/transfer flows.
    !> Initial value is LARGEREAL
    subroutine allocate_state_network(a_nresv,a_nresv_conn,a_nqext,a_ntran,a_nvar)
        use error_handling
        implicit none
        character(LEN=128) :: message
        integer :: istat = 0
        integer, intent(in) :: a_nresv      !< Number of requested 
        integer, intent(in) :: a_nresv_conn !< Number of requested
        integer, intent(in) :: a_nqext      !< Number of requested
        integer, intent(in) :: a_ntran      !< Number of requested
        integer, intent(in) :: a_nvar       !< Number of constituents
        
        write(message,*)"Could not allocate state variable. " //&
         "This could be due to allocating several times in " // &
         "a row without deallocating (memory leak)"
        
        allocate(resv_height(a_nresv), stat = istat)
        if (istat .ne. 0 )then
           call gtm_fatal(message)
        end if
        resv_height      = LARGEREAL

        allocate(resv_flow(a_nresv_conn), stat = istat)
        if (istat .ne. 0 )then
           call gtm_fatal(message)
        end if
        resv_flow        = LARGEREAL

        allocate(qext_flow(a_nqext), stat = istat)
        if (istat .ne. 0 )then
           call gtm_fatal(message)
        end if
        qext_flow        = LARGEREAL          

        allocate(tran_flow(a_ntran), stat = istat)
        if (istat .ne. 0 )then
           call gtm_fatal(message)
        end if
        tran_flow        = LARGEREAL           
        
        allocate(conc_resv(a_nresv,a_nvar), stat = istat)
        allocate(conc_resv_prev(a_nresv,a_nvar), stat = istat)
        
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
        n_var = 0
        deallocate(resv_height)
        deallocate(resv_flow)
        deallocate(qext_flow)
        deallocate(tran_flow)
        deallocate(conc_resv, conc_resv_prev)
        return
    end subroutine    

end module



