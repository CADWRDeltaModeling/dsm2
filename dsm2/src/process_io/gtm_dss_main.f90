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
!> Interface to request time-varying data by specified julmin
!> and prev_julmin.
!> The time series could be time-varying data from DSS or constant
!> value. It calls readtvd module which was originally developed
!> by Ralph Finch and last modified in September 1996.
!>@ingroup process_io
module gtm_dss_main
    use constants, only: gtm_real
    implicit none
    integer, save :: ndx_cloud  = 0  !< pathinput index for cloud cover
    integer, save :: ndx_dryblb = 0  !< pathinput index for dry bulb temperature
    integer, save :: ndx_wetblb = 0  !< pathinput index for wet bulb temperature
    integer, save :: ndx_wind   = 0  !< pathinput index for wind speed
    integer, save :: ndx_atmpr  = 0  !< pathinput index for atmospheric pressure
    integer, save :: ndx_solar  = 0  !< pathinput index for solar radiation
    real(gtm_real), save :: cloud  = 0.d0  !< cloud cover fraction (0-1)
    real(gtm_real), save :: dryblb = 0.d0  !< dry bulb air temperature (deg F)
    real(gtm_real), save :: wetblb = 0.d0  !< wet bulb air temperature (deg F)
    real(gtm_real), save :: wind   = 0.d0  !< wind speed (mph)
    real(gtm_real), save :: atmpr  = 0.d0  !< atmospheric pressure (mmHg)
    real(gtm_real), save :: solar  = 0.d0  !< measured solar radiation (W/m^2)
    contains

    !> Process DSS input files
    subroutine opendss(ifltab,        & ! ifltab_in
                       num_dssfiles,  & ! number of dss files
                       dssfilenames)    ! dss filenames

        use io_units, only : unit_error
        implicit none
        integer, intent(in):: num_dssfiles
        character(len=130), intent(in) :: dssfilenames(num_dssfiles)
        integer, intent(out) :: ifltab(600,num_dssfiles) ! ifltab must have a length of 600 short integer words (DSS documentation, this is not an arbitrary number)
        character(len=150) :: ctmp
        logical :: lstat
        integer :: nlen, istat, i

        ! Open the DSS files for reading
        do i =1, num_dssfiles
            call zfname (trim(dssfilenames(i)), ctmp, nlen, lstat)
            if (.not. lstat) then
                write(unit_error, '(a/a/a)') 'Fatal error - DSS input file',  &
                                             ctmp(:nlen), 'does not exist.'
            endif
            call zopen (ifltab(1,i), trim(dssfilenames(i)), istat)
            if (istat .gt. 0) then
                 write(unit_error, '(a,a)') 'Unable to open the file ', dssfilenames(i)
            endif
        enddo
        return
    end subroutine

    !> Return values from all time-varying data for the time specified
    subroutine get_inp_value(jmin, prev_jmin)
        use common_gtm_vars, only: n_inputpaths, pathinput
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

        ! update met module variables directly using precomputed pathinput indices
        if (ndx_cloud  > 0) cloud  = pathinput(ndx_cloud)%value
        if (ndx_dryblb > 0) dryblb = pathinput(ndx_dryblb)%value
        if (ndx_wetblb > 0) wetblb = pathinput(ndx_wetblb)%value
        if (ndx_wind   > 0) wind   = pathinput(ndx_wind)%value
        if (ndx_atmpr  > 0) atmpr  = pathinput(ndx_atmpr)%value
        if (ndx_solar  > 0) solar  = pathinput(ndx_solar)%value

        return
    end subroutine

    !> Scan pathinput once during setup to record indices for meteorological climate inputs.
    !> Mirrors assign_input_ts_group_var; must be called after buffer_input_qual.
    subroutine assign_met_indices()
        use common_gtm_vars, only: pathinput
        use gtm_vars, only: n_node_ts
        implicit none
        integer :: i
        do i = 1, n_node_ts
            select case (trim(pathinput(i)%variable))
                case ('cloud');        ndx_cloud  = i
                case ('dry_bulb');     ndx_dryblb = i
                case ('wet_bulb');     ndx_wetblb = i
                case ('wind');         ndx_wind   = i
                case ('atm_pressure'); ndx_atmpr  = i
                case ('solar');        ndx_solar  = i
            end select
        end do
        return
    end subroutine assign_met_indices

end module