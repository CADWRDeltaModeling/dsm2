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
!> Interface to request time-varying data by specified julmin
!> and prev_julmin. 
!> The time series could be time-varying data from DSS or constant
!> value. It calls readtvd module which was originally developed
!> by Ralph Finch and last modified in September 1996.
!>@ingroup process_io
module gtm_dss_main

    contains
    
    !> Return values from all time-varying data for the time specified
    subroutine get_inp_value(jmin, prev_jmin)
        use common_dsm2_vars, only: n_inputpaths, pathinput
        use gtm_dss
        use gtm_dss_readtvd
        implicit none
        integer, intent(in) :: jmin        !< current julmin
        integer, intent(in) :: prev_jmin   !< previous julmin
        integer :: i 
        
        call readtvd(datain_min15, jmin, prev_jmin, npthsin_min15, mins15, n_inputpaths, ptin_min15)
        call readtvd(datain_hour1, jmin, prev_jmin, npthsin_hour1, hrs, n_inputpaths, ptin_hour1)
        call readtvd(datain_day1, jmin, prev_jmin, npthsin_day1, dys, n_inputpaths, ptin_day1)
        call readtvd(datain_week1, jmin, prev_jmin, npthsin_week1, wks, n_inputpaths, ptin_week1)
        call readtvd(datain_month1, jmin, prev_jmin, npthsin_month1, mths, n_inputpaths, ptin_month1)
        call readtvd(datain_year1, jmin, prev_jmin, npthsin_year1, yrs, n_inputpaths, ptin_year1)
        call readtvd(datain_irr, jmin, prev_jmin, npthsin_irr, irrs, n_inputpaths, ptin_irr)
      
        do i = 1, n_inputpaths
            call get_inp_data(i)
        end do
          
        return
    end subroutine
    
end module