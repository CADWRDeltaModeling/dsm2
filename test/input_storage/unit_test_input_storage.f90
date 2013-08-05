!<license>
!    Copyright (C) 2013 State of California,
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

!> Routines to test input storage API
!>@ingroup test_input_storage
module ut_input_storage
    
    use error_handling
    
    contains
    
    !> Routine to test processing gtm.inp file
    subroutine test_input_storage
        use common_dsm2_vars
        use process_gtm_input
        implicit none       
        call read_input_text("gtm_do.inp")
        ! todo: way to test is to check if the echo file and tidefile are successfully generated. 
        !       any better way to verify this?
      return          
    end subroutine
   
end module