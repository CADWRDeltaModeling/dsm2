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

!> Routines to test DSM2 time utilities
!>@ingroup test_process_io
module ut_dsm2_time_utils

    use fruit
 
    contains
    
    !> test dsm2 time utils incr_intv()
    subroutine test_dsm2_time_utils
        use dsm2_time_utils
        implicit none
        integer :: jmins, incr_intv
        integer :: boundary
        real(gtm_real) :: incr_intv_r
        integer*4 :: inctim, nom_mins, number, juls, istime, jule, ietime, istat
                
        boundary = 2       
        jmins = 51589460
        nom_mins = 15
        number = -1
        juls = 35826
        istime = 0
        jule = juls
        ietime = istime
        !istat = inctim(nom_mins, 0, number, juls, istime, jule, ietime)
        call incr_intvl(incr_intv, jmins,'15MIN', boundary)
        call assertEquals (dble(incr_intv), dble(51589470), weakest_eps, "problem in incr_intvl for 15MIN")
        call incr_intvl(incr_intv, jmins,'-15MIN', boundary)
        call assertEquals (dble(incr_intv), dble(51589455), weakest_eps, "problem in incr_intvl for -15MIN")
        return
    end subroutine    

end module