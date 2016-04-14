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
!>@ingroup process_io      
module process_gtm_suspended_sediment
      
    contains  


    subroutine process_suspended_sediment_type(composition,  &
                                               method)
        use common_variables, only: constituents
        implicit none
        character :: composition*16
        character :: method*16
        
        
        return
    end subroutine    


    subroutine process_suspended_sediment(name,       &   ! user defined name
                                          composition,     &   ! DSM2 node number
                                          percent,      &   ! ec, doc, etc
                                          grain_size)         ! value or path
      use common_dsm2_vars
      use io_utilities
      implicit none
      character :: name*32,                  &
                   composition*16
      real*8 :: percent, grain_size
      
      call locase(name)
      call locase(composition) 
    end subroutine

end module    