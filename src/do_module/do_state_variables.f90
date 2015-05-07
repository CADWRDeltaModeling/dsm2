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
!>@ingroup do_module
module do_state_variables

    use gtm_precision
    
    real(gtm_real), save :: atmpr
    real(gtm_real), save :: wetblb
    real(gtm_real), save :: dryblb
    real(gtm_real), save :: cloud
    real(gtm_real), save :: wind
    real(gtm_real), save, allocatable :: depth(:)

    contains 
    
    !> Allocate the state variables for DO module
    !> Initial value is LARGEREAL
    subroutine allocate_do_state(a_ncell)
        use error_handling
        implicit none
        character(LEN=128) :: message
        integer :: istat = 0
        integer, intent(in) :: a_ncell !< Number of requested cells
        integer :: ncell
        ncell = a_ncell
        write(message,*)"Could not allocate DO state variable. " //&
         "This could be due to allocating several times in " // &
         "a row without deallocating (memory leak)"
        
        allocate(depth(ncell), stat = istat)
        if (istat .ne. 0 )then
           call gtm_fatal(message)
        end if
        depth      = LARGEREAL
        return
    end subroutine
     
    
    !> Deallocate the DO state variables
    subroutine deallocate_do_state
        implicit none
        deallocate(depth)
        return
    end subroutine 

end module