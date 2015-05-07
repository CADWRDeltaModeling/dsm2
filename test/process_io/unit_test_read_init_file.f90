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
!> This module contains unit test for read_init_file.
!>@ingroup test_process_io
module ut_read_init_file

    use fruit
 
    contains
    
    subroutine test_read_init_file
        use gtm_precision
        use read_init
        implicit none
        integer, parameter :: ncell = 64
        integer, parameter :: nresv = 2
        integer, parameter :: nvar = 1
        character(len=15) :: restart_file_name
        real(gtm_real) :: init_c(ncell, nvar)
        real(gtm_real) :: init_r(nresv, nvar)
        restart_file_name = "channel_gtm.qrf"
        call read_init_file(init_c, init_r, restart_file_name, ncell, nresv, nvar)
        call assertEquals (init_c(5,1), dble(83.0), weakest_eps, "problem in reading init_i in read_init_file")
        call assertEquals (init_r(1,1), dble(1.0), weakest_eps, "problem in reading init_r in read_init_file")
        return
    end subroutine

end module        