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
!>@ingroup transport
module state_variables

    use gtm_precision
    use common_variables

    !> Mass of constituent in the current/new time step,
    !> dimensions (ncell, nvar)
    real(gtm_real), save, allocatable :: mass(:,:)
    
    !> Mass of constituent in the previous time step,
    !> dimensions (ncell, nvar)
    real(gtm_real), save, allocatable :: mass_prev(:,:)
    
    
    !> Concentration in the current/new time step,
    !> dimensions (ncell, nvar)
    real(gtm_real), save, allocatable :: conc(:,:)
    
    !> Concentration in the previous time step,
    !> dimensions (ncell, nvar)
    real(gtm_real), save, allocatable :: conc_prev(:,:)
    
    !> Cell-centered area
    !> dimensions (ncell)
    real(gtm_real), save, allocatable :: area(:)
    
    !> Cell-centered area at old time step
    !> dimensions (ncell)
    real(gtm_real), save, allocatable :: area_prev(:)
    
    !> Face area on lo side of cell (so this is cell-indexed),
    !> dimensions (ncell)
    real(gtm_real), save, allocatable :: area_lo(:)
    
    !> Face-centered area on hi side of cell (so this is cell-indexed),
    !> dimensions (ncell)
    real(gtm_real), save, allocatable :: area_hi(:)

    !> Face area on lo side of cell (so this is cell-indexed),
    !> dimensions (ncell)
    real(gtm_real), save, allocatable :: area_lo_prev(:)
    
    !> Face-centered area on hi side of cell (so this is cell-indexed),
    !> dimensions (ncell)
    real(gtm_real), save, allocatable :: area_hi_prev(:)

    !> face-centered flow on lo side of cell  (so this is cell-indexed),
    !> dimensions (ncell)
    real(gtm_real), save, allocatable :: flow_prev(:)
        
    !> face-centered flow on lo side of cell  (so this is cell-indexed),
    !> dimensions (ncell)
    real(gtm_real), save, allocatable :: flow(:)
    
    !> face-centered flow on lo side of cell  (so this is cell-indexed),
    !> dimensions (ncell)
    real(gtm_real), save, allocatable :: flow_lo(:)
    
    !> face-centered flow on hi side of cell  (so this is cell-indexed),
    !> dimensions (ncell)
    real(gtm_real), save, allocatable :: flow_hi(:)
 
    
    contains
    
    !> Allocate the state variables consistently
    !> including concentration and hydrodynamics.
    !> Initial value is LARGEREAL
    subroutine allocate_state(a_ncell,a_nvar)
        use error_handling
        implicit none
        character(LEN=128) :: message
        integer :: istat = 0
        integer, intent(in) :: a_ncell !< Number of requested cells
        integer, intent(in) :: a_nvar  !< Number of constituents
        integer :: ncell, nvar
        ncell = a_ncell
        nvar  = a_nvar
        write(message,*)"Could not allocate state variable. " //&
         "This could be due to allocating several times in " // &
         "a row without deallocating (memory leak)"
        
        allocate(conc(ncell,nvar), conc_prev(ncell,nvar), stat = istat)
        if (istat .ne. 0 )then
           call gtm_fatal(message)
        end if
        conc      = LARGEREAL  ! absurd value helps expose bugs  
        conc_prev = LARGEREAL
        allocate(mass(ncell,nvar), mass_prev(ncell,nvar),stat = istat)
        if (istat .ne. 0 )then
           call gtm_fatal(message)
        end if
        mass      = LARGEREAL  ! absurd value helps expose bugs  
        mass_prev = LARGEREAL       
        allocate(area(ncell), area_prev(ncell), area_lo(ncell), area_hi(ncell), &
                 area_lo_prev(ncell), area_hi_prev(ncell),stat = istat)
        if (istat .ne. 0 )then
           call gtm_fatal(message)
        end if
        area      = LARGEREAL
        area_prev = LARGEREAL
        area_lo   = LARGEREAL
        area_hi   = LARGEREAL
        area_lo_prev   = LARGEREAL
        area_hi_prev   = LARGEREAL
        
        allocate(flow_prev(ncell), stat = istat)
        allocate(flow(ncell),flow_lo(ncell), flow_hi(ncell),stat = istat)
        if (istat .ne. 0 )then
           call gtm_fatal(message)
        end if
        flow_prev = LARGEREAL
        flow      = LARGEREAL
        flow_lo   = LARGEREAL
        flow_hi   = LARGEREAL
        return
    end subroutine
     
    
    !> Deallocate the state variables
    !> including concentration and hydrodynamics
    !> and reset ncell and nvar to zero.
    subroutine deallocate_state
        implicit none
        n_cell = 0
        n_var  = 0
        deallocate(conc, conc_prev, mass, mass_prev)
        deallocate(area)
        deallocate(area_prev)
        deallocate(area_lo,area_hi)
        deallocate(area_lo_prev, area_hi_prev)
        deallocate(flow_prev)
        deallocate(flow, flow_lo, flow_hi)
        return
    end subroutine 

end module



