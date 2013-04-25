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

!> Routines for time functionalities
!>@ingroup gtm_core	
module time_util

    use gtm_precision
    use error_handling
    contains
    
    !> Convert from character date/time to julian minute
    subroutine cdt2jmin(cdatx, julmin)
        implicit none
        character(len=*), intent(in) :: cdatx     !> character date/time (e.g. 05JUN1983 0510) (input)
        integer, intent(out) :: julmin            !> julian minute
        integer*4 :: julday  &                    ! days since 31dec1899
                    ,minute  &                    ! minutes past midnight
                    ,ihm2m                        ! DSS function
        integer*4 :: ierror                       ! error flag
        call datjul(cdatx(1:9), julday, ierror)
        if (cdatx(11:14) .eq. ' ') then           ! assume empty time means 0000
            minute = 0
        else
            minute = ihm2m(cdatx(11:14))
        endif
        if (ierror .ne. 0 .or. minute .eq. -1) then
            julmin = miss_val_i
            goto 900
        endif
        julmin = julday*24*60 + minute
        return
 900    continue
    end subroutine
   
    !> Convert from Julian minute to character date/time
    subroutine jmin2cdt(julmin, cdt)
        implicit none
        integer*4, intent(in) ::  julmin           !> minutes since 31dec1899 2400
        character(len=14), intent(out) :: cdt      !> character date/time
        integer*4 ::  julday    &                  ! days since 31dec1899
                     ,minute    &                  ! minutes past midnight
                     ,ndate     &                  ! number of characters in date
                     ,m2ihm     &                  ! DSS function
                     ,itime     &                  ! integer time
                     ,jtmp,itmp                    ! temporary julday & minutes
        cdt = '              '
        jtmp = julmin/(24*60)       ! julday
        itmp = mod(julmin,24*60)    ! minutes past midnight
        call datcll(jtmp,itmp,julday,minute)
        call juldat(julday, 104, cdt(1:9), ndate)
        itime = m2ihm(minute,cdt(11:14))
        if (itime .eq. -1) then
            cdt(1:1) = miss_val_c
        endif
        return
      end subroutine

end module