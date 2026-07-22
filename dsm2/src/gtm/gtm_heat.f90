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
!> compute heat for DO module
!>@ingroup do_module
module gtm_heat
    use constants
    use time_utilities, only : jmin2iso
    use common_gtm_vars, only: print_level
    use gtm_dss_main, only: cloud, dryblb, wetblb, wind, atmpr, solar, &
                             ndx_cloud, ndx_dryblb, ndx_wetblb, ndx_wind, ndx_atmpr, ndx_solar
    use common_qual, only: lat, longitude, long_std_merid, elev, dust_attcoeff, evapcoeff_a, evapcoeff_b
    implicit none
    integer, save :: i_temp = 0  !< constituent index for temperature in conc array

    contains

    !> Scan constituents once during setup to find the temperature index.
    !> Must be called after constituents array is populated.
    subroutine set_heat_temp_index()
        use gtm_vars, only: constituents, n_var
        implicit none
        integer :: i
        do i = 1, n_var
            if (trim(constituents(i)%name) == 'temp') then
                i_temp = constituents(i)%conc_no
                exit
            end if
        end do
        return
    end subroutine set_heat_temp_index

    subroutine calc_heat_budget(source_heat,     &
                               conc,            &
                               wind,            &
                               cloud,           &
                               atmpr,           &
                               dryblb,          &
                               wetblb,          &
                               solar,           &
                               depth,           &
                               dt,              &
                               julmin,          &
                               ncell)
            implicit none
            integer, intent(in) :: ncell
            real(gtm_real), intent(out) :: source_heat(ncell,1)
            real(gtm_real), intent(in) :: conc(ncell,*)
            real(gtm_real), intent(in) :: wind
            real(gtm_real), intent(in) :: cloud
            real(gtm_real), intent(in) :: atmpr
            real(gtm_real), intent(in) :: dryblb
            real(gtm_real), intent(in) :: wetblb
            real(gtm_real), intent(in) :: solar
            real(gtm_real), intent(in) :: depth(ncell)             !< depth at source location
            real(gtm_real), intent(in) :: dt
            integer, intent(in) :: julmin

            ! local varaibles
            real(gtm_real) :: tw, hb, vpw, evapor, he, hc, hsnet, vpair,hs, ha

            integer :: i

            do i = 1, ncell
                call calc_net_solar_long_wave(vpair,hs,ha,julmin,dt,atmpr,wetblb,dryblb,cloud,wind,solar)
                tw = conc(i,i_temp)*1.8d0 + 32.d0
                ! water surface back radiation (HB)
                hb = 1.6781d-9 * (tw+460.d0)**four
                ! Evaporation (HE): vpair is calculated in heat.f;
                ! latent heat of vap. is represented by (1084.0-0.5*tw) in HE & HC
                vpw = 0.1001d0*exp(0.03d0*tw)-0.0837d0
                evapor = 62.4d0*(evapcoeff_a + evapcoeff_b*wind)
                he = evapor * (vpw - vpair)*(1084.0d0-half*tw)     !todo::vpair from heat.f
                hc = 0.01d0*evapor*(dryblb-tw)*(1084.0d0-half*tw)  ! conduction (HC)

                ! compute net heat flux from all sources
                hsnet = hs + (ha + hc - hb - he)
                !contribution to temperature is net radiation/(depth*density*heat)
                source_heat(i,i_temp) = (hsnet/(depth(i)*62.4d0))/1.8d0 - conc(i,i_temp)
            end do
    return
    end subroutine calc_heat_budget

    !> It computes net solar radiation for the time interval delta t
    !> and rate of atmospheric radiation exchanged through the air-water interface
    subroutine calc_net_solar_long_wave(vpair,     &
                         tsolhr,    &
                         ha,        &
                         julmin,    &
                         time_step, &
                         atmpr,     &
                         wetblb,    &
                         dryblb,    &
                         cloud,     &
                         wind,      &
                         solar)
        implicit none

        real(gtm_real), intent(out) :: vpair     !< vapor pressure
        real(gtm_real), intent(out) :: tsolhr    !< net solar radiation
        real(gtm_real), intent(out) :: ha        !< heat flux due to long wave atmospheric radiation
        real(gtm_real), intent(in)  :: atmpr     !<
        real(gtm_real), intent(in)  :: wetblb    !<
        real(gtm_real), intent(in)  :: dryblb    !<
        real(gtm_real), intent(in)  :: cloud     !<
        real(gtm_real), intent(in)  :: wind      !<
        real(gtm_real), intent(in)  :: solar     !< measured solar radiation (W/m^2)
        real(gtm_real), intent(in) :: time_step  !< time_step in minutes
        integer, intent(in) :: julmin            !< Julian minutes


        integer :: iyr,imon,iday    ! year, month, day
        integer :: julday, iymdjl         ! HEC function to return day of year
        integer :: dayof_year             ! julian day of year (1-366)
        integer :: timeof_day             ! minute of day (0-1440)
        integer :: nl
        real(gtm_real) :: deltsl, declin, eqtime
        real(gtm_real) :: str, sts, stb, ste
        real(gtm_real) :: declon, tana, tanb, acs, xx, vpwb
        real(gtm_real) :: tb, te, alpha, talt, y, ar, br, rs
        real(gtm_real) :: cnl
        real(gtm_real) :: clc
        real(gtm_real) :: con1, con2, con3, con4, con5, con6
        character*19 :: current_date

        ! declare variables for diagnostics
        integer :: num, nummax
        real(gtm_real) :: pi, day_tsolhr, day_ha, tot_tsolhr, tot_ha

        parameter(pi = 3.141592654d0,        &
                  con1 = two*pi/365.0d0,     &
                  con3 = 180.0d0/pi,         &
                  con4 = 23.45d0*pi/180.0d0, &
                  con5 = pi/12.0d0,          &
                  con6 = 12.0d0/pi)

        timeof_day = mod(julmin,60*24)
        julday = int(julmin/dble(24*60))
        call jliymd(julday,iyr,imon,iday)
        dayof_year = iymdjl(iyr,imon,iday) - iymdjl(iyr,1,1) + 1

        ! compute and/or define required constants.
        con2 = pi/180.0d0*lat
        deltsl = (longitude - long_std_merid)/15.0d0
        declin = con4*cos(con1*(172.d0-dayof_year))
        eqtime = 0.000121d0-0.12319d0*sin(con1*dble(dayof_year-1)-0.07014d0)  &
                -0.16549d0*sin(two*con1*dble(dayof_year-1)+0.3088d0)
        declon = abs(declin)
        tana = tan(con2)
        tanb = tan(declon)
        acs = tana*tanb
        if (acs.eq.zero) then
            acs = pi/two
        else
            xx = sqrt(one-acs*acs)
            xx = xx/acs
            acs = atan(xx)
            if (declin.gt.zero) acs = pi - acs
        end if

        ! calculate the standard time of sunrise (str) and sunset (sts).
        str = 12.0d0 - con6*acs + deltsl
        sts = 24.0d0 - str + two*deltsl

        ! increment the variables that define the time of the beginning(stb)
        ! and the end (ste) of the time interval.
        stb = dble(timeof_day)/60.d0 ! in decimal hours (e.g. 13.5 is 1330)
        ste = stb + dble(time_step)/60.d0

        ! compute vapor pressures (vpwb and vpair) and cloud class index (nl) for albedo
        vpwb = 0.1001d0*exp(0.03d0*wetblb)-0.0837d0
        vpair = vpwb-0.000367d0*atmpr*(dryblb-wetblb)*(one+(wetblb-32.0d0)/1571.0d0)
        cnl = cloud*10.0d0 + 1.0d0
        nl = int(cnl)

        if ((sts.le.stb) .or. (str.ge.ste)) then
             goto 30
        elseif ((str.gt.stb).and.(str.lt.ste)) then
            tb = str - 12.0d0 - deltsl + eqtime
            te = ste - 12.0d0 - deltsl + eqtime
        elseif ((sts.lt.ste).and.(sts.gt.stb)) then
            tb = stb - 12.0d0 - deltsl + eqtime
            te = sts - 12.0d0 - deltsl + eqtime
        else
            tb = stb - 12.0d0 - deltsl + eqtime
            te = ste - 12.0d0 - deltsl + eqtime
        end if
        talt = (tb + te)/two

        alpha = sin(con2)*sin(declin)+cos(con2)*cos(declin)*cos(con5*talt)
        if (alpha .eq. -1.0d0) then
            alpha = -pi/two
        elseif (alpha .eq. 1.0d0) then
            alpha = pi/two
        else
            y = sqrt(one-alpha*alpha)
            y = alpha/y
            alpha = atan(y)
        end if
        if (alpha.lt.0.01) goto 30

        ! compute reflectivity coefficient (rs)
        if (nl.eq.1) then
            ar = 1.18d0
            br = -0.77d0
        elseif ((nl.ge.2).and.(nl.le.6)) then
            ar = 2.20d0
            br = -0.97d0
        elseif ((nl.ge.7).and.(nl.le.10)) then
            ar = 0.95d0
            br = -0.75d0
        elseif (nl.eq.11) then
            ar = 0.35d0
            br = -0.45d0
        else
            ar = 1.d0
            br = 1.d0
        end if
        rs = ar*(con3*alpha)**br


        ! compute net solar radiation for the time interval delta t
        ! solar is measured (W/m^2), already accounts for cloud attenuation;
        ! convert to btu/(sq ft-hr) and apply surface albedo (1-rs) only
        tsolhr = solar*0.3170d0*(one-rs)
        goto 31
30      tsolhr = zero
31      clc=1.0d0+0.17d0*cloud**2

        ! compute heat flux due to long wave atmospheric radiation (ha).
        ha = 0.97*1.73e-09*2.89e-06*(dryblb+460.0)**6*clc

        if (print_level .gt. 3) then
            nummax = 24*60/dble(time_step)
            if (num .eq. 0) then
                tot_tsolhr = zero
                tot_ha = zero
            endif
            num = num + 1
            tot_tsolhr = tot_tsolhr + tsolhr
            tot_ha = tot_ha + ha
            if (num .eq. nummax)then
                day_tsolhr = 24*tot_tsolhr/nummax
                day_ha = 24*tot_ha/nummax
                write(*,999) current_date, day_tsolhr, day_ha
                num = 0
            endif
 999        format(a,'daily solar rad =',f10.2, 'daily atm. rad =',f10.2)
        end if
    return
    end subroutine

end module
