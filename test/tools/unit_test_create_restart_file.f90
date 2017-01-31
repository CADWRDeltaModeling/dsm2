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

!> This module contains unit test for creating restart file
!>@ingroup test_process_io
module ut_create_restart

    use fruit
 
    contains
    
    !> Routine to test creating restart text file
    subroutine test_create_restart()
        use gtm_precision
        use create_restart
        implicit none
        integer :: file_unit
        character(len=11) :: restart_filename
        real(gtm_real) :: init(64,2)
        integer :: ncell, nvar
        integer :: i, j
        
        file_unit = 131
        restart_filename = "restart.txt"
        call create_restart_file(restart_filename, "channel_gtm.h5", "01FEB1998 0900")
        open(file_unit, file = restart_filename)
        read(file_unit,*)
        read(file_unit,*)
        read(file_unit,*) nvar
        read(file_unit,*) ncell
        do i = 1, ncell
            read(file_unit,*) (init(i,j),j=1,nvar)
        end do    
        call assertEquals (init(1,1), dble(459.276732218191), weakest_eps, "problem in create_restart")
        close(file_unit) 
        return
    end subroutine
    
end module    

