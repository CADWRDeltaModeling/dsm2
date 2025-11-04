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

!> DO source terms to fulfill source interface
!>@ingroup do_module
module do_source

    use constants, only: gtm_real
    use source_sink

    contains

    !> DO module sources.
    !> This source term multiplies each constituent by a decay rate
    subroutine do_module_source(source, &
                                conc,   &
                                area,   &
                                flow,   &
                                ncell,  &
                                nvar,   &
                                time)
        use constants
        use do_parameter
        use do_state_variables
        use do_heat
        implicit none
        integer, intent(in)  :: ncell                      !< Number of cells
        integer, intent(in)  :: nvar                       !< Number of variables
        real(gtm_real), intent(inout) :: source(ncell,nvar)!< cell centered source
        real(gtm_real), intent(in)  :: conc(ncell,nvar)    !< Concentration
        real(gtm_real), intent(in)  :: area(ncell)         !< area at source
        real(gtm_real), intent(in)  :: flow(ncell)         !< flow at source location
        real(gtm_real), intent(in)  :: time                !< time
        real(gtm_real) :: thetadj(16)                      ! temperature adjustment coefficients
        real(gtm_real) :: decay                            ! temporary variables
        real(gtm_real) :: settle                           ! temporary variables
        real(gtm_real) :: benthic                          ! temporary variables
        real(gtm_real) :: reaer                            ! temporary variables
        real(gtm_real) :: tw, twk, twksq, twk4             ! temporary variables for DO
        real(gtm_real) :: phi, pwv, tempval                ! temporary variables for DO
        real(gtm_real) :: os, osf, oss                     ! temporary variables for DO
        real(gtm_real) :: kwind, k2, k2min, k2max          ! temporary variables for DO
        real(gtm_real) :: vel(ncell)                       ! temporary variables for DO
        real(gtm_real) :: xn, fn, fp, cpo4                 ! temporary variables for Algae
        real(gtm_real) :: lambda, inten, fl, fnnn          ! temporary variables for Algae
        real(gtm_real) :: grw, set, resp, mort             ! temporary variables for Algae
        real(gtm_real) :: factor                           ! temporary variables for Algae
        real(gtm_real) :: ha, hb, hc, he                   ! temporary variables for temp
        real(gtm_real) :: vpw, evapor, hsnet               ! temporary variables for temp
        real(gtm_real) :: vpair, tsolhr, julmin, time_step ! temporary variables for temp
        real(gtm_real) :: avg_temp                         ! temporary variables for temp
        integer :: ivar                                    ! Counter on constituents
        integer :: i
        integer :: p ! temporary variables for DO

        source = zero
        vel = flow/area
        avg_temp = sum(conc(:,i_temp))/dble(ncell)
        call tempfactors(avg_temp, thetadj)

        do i = 1, ncell

         !--- calculate source and sink terms for BOD ---
          if (i_bod .gt. 0) then
            decay = bod_decay(i) * conc(i,i_bod)          ! contribution due to reactions
            settle = bod_settle(i) * conc(i,i_bod)        ! loss of mass due to settling
            if (i_temp .gt. 0) then
                decay = decay * thetadj(it_bod_decay)
                settle = settle * thetadj(it_bod_set)
            end if
            source(i,i_bod) = - decay - settle
            ! if DO is simulated, calculate demand on DO
            if (i_do .gt. 0) source(i,i_do) = - decay
          end if

          !--- calculate source and sink terms for DO ---
          if (i_do .gt. 0) then
            p = 1
            if (i_temp .eq. 0) then
                tw = 20
            else
                tw = conc(i,i_temp)
            end if
            twk = tw + 273.d0
            twksq = twk * twk
            twk4 = twksq * twksq
            phi = 9.75d-4 - 1.426d-5*tw + 6.436d-8*(tw*tw)
            pwv = exp(11.8571d0 - 3840.7d0/twk - 216961.d0/(twksq))
            tempval = -139.3441d0 + 157570.1d0/twk - 66423080.d0/(twksq)      &
                      + 1.2438d10/(twk*twksq) - 8.621949d11/(twk4)
            osf = exp(tempval)

            ! if conservative constituents are simulated, then include
            ! the effects of salinity or chloride on DO
            if (i_tds .gt. 0) then
                oss = exp(tempval-0.001d0*conc(i,i_tds)*(0.017674d0-10.754d0/twk + 2140.7d0/(twksq)))
            elseif (i_cl .gt. 0) then
                oss = exp(tempval-0.001d0*1.80655d0*conc(i,i_cl)*(0.017674d0-10.754d0/twk + 2140.7d0/(twksq)))
            elseif (i_ec .gt. 0) then
                oss = exp(tempval-0.001d0*0.64d0*conc(i,i_ec)*(0.017674d0-10.754d0/twk + 2140.7d0/(twksq)))
            else
                oss = osf
            endif

            if (p .eq. 1)then
                os = oss
            else
                os = oss*p*(1-pwv/p)*(1-phi*p)/(1-pwv)/(1-phi)
            endif

            ! compute reaeration using O'Connor Dobbins' formulation
            kwind = 0.015d0*2.1511d0*wind**two/depth(i)
            k2 = 12.96d0*(sqrt(abs(vel(i)))/(depth(i)*depth(i)*depth(i)))+kwind
            k2min = 3.d0/depth(i)
            if (k2 .lt. k2min) k2 = k2min
            k2 = k2/24.d0
            if (k2 .gt. k2max) k2 = k2max

            reaer = k2*(os-conc(i,i_do))

            !account for benthic oxygen demand
            benthic = 0.0353d0 * do_benthic(i)/depth(i)

            if (i_temp .gt. 0) then
                reaer = reaer * thetadj(it_reaer)
                benthic = benthic * thetadj(it_do_ben)
            endif
            source(i,i_do) = source(i,i_do) + reaer - benthic
         end if

         !--- calculate source and sink terms for ORG-N ---
         if (i_orgn .gt. 0) then
            decay = orgn_decay(i) * conc(i,i_orgn)       ! hydrolysis of Org-N to NH3-N
            settle = orgn_settle(i) * conc(i,i_orgn)     ! loss of mass due to settling
            if (i_temp .gt. 0) then
                decay = decay * thetadj(it_orgn_decay)
                settle = settle * thetadj(it_orgn_set)
            end if
            source(i,i_orgn) = - decay - settle
            ! if NH3 is simulated, decay of Org-N is a source of NH3
            if (i_nh3 .gt. 0) source(i, i_nh3) = decay
         end if

         !--- calculate source and sink terms for NH3-N ---
         if (i_nh3 .gt. 0) then
            decay = nh3_decay(i)*conc(i,i_nh3)        ! contribution due to reactions
            benthic = nh3_benthic(i)/depth(i)         ! account for benthos source
            if (i_temp .gt. 0) then
                decay = decay * thetadj(it_nh3_decay)
                benthic = benthic * thetadj(it_nh3_ben)
            end if
            source(i,i_nh3) = source(i,i_nh3) - decay + benthic
            ! if NO2 is simulated, biologic oxidation of NH3 is a source of NO2
            if (i_no2 .gt. 0) source(i,i_no2) = decay
            ! if DO is simulated, oxidation of NH3 exerts demand on DO
            if (i_do .gt. 0) source(i,i_do) = - oxy_nh3*decay
         end if

         !--- calculate source and sink terms for NO2-N and NO3-N ---
         if (i_no2 .gt. 0) then
            decay = no2_decay(i) * conc(i,i_no2)
            if (i_temp .gt. 0) decay = decay * thetadj(it_no2_decay)
            source(i,i_no2) = source(i,i_no2) - decay
            ! if NO3 is simulated, biologic oxidation of NO2 is a source of NO3
            if (i_no3 .gt. 0) source(i,i_no3) = decay
            ! if DO is simulated, oxidation of NO2 exerts demand on DO
            if (i_do .gt. 0) source(i,i_do) = source(i,i_do) - oxy_no2*decay
         end if

         !--- calculate source and sink terms for ORG-P ---
         if (i_orgp .gt. 0) then
            decay = orgp_decay(i) * conc(i,i_orgp)
            settle = orgp_settle(i) * conc(i,i_orgp)
            if (i_temp .gt. 0) then
                decay = decay * thetadj(it_orgp_decay)
                settle = settle * thetadj(it_orgp_set)
            end if
            source(i,i_orgp) = - decay - settle
            ! if PO4 is simulated, decay of ORG-P is a source of PO4
            if (i_po4 .gt. 0) source(i,i_po4) = decay
         end if

         !--- calculate source and sink terms for PO4 ---
         if (i_po4 .gt. 0) then
            benthic = po4_benthic(i)/depth(i)
            if (i_temp .gt. 0) benthic = benthic * thetadj(it_po4_ben)
            source(i,i_po4) = benthic
         end if

         !--- calculate source and sink terms for algae ---
         if (i_alg .gt. 0) then
            ! calculate growth
            xn = zero
            if (i_nh3 .gt. 0) xn = conc(i,i_nh3)
            if (i_no3 .gt. 0) xn = xn + conc(i,i_no3)
            fn = one
            fp = one
            cpo4 = max(zero,conc(i,i_po4))
            if (xn .gt. zero) fn = xn / (xn + knit_half)
            if (i_po4 .gt. 0) fp = cpo4 / (cpo4 + kpho_half)
            ! account for algae self shading
            lambda = lambda0 + lambda1*alg_chl_ratio*conc(i,i_alg) + lambda2*(alg_chl_ratio*conc(i,i_alg))**(two/three)
            !inten = tsolhr !!!todo:: heat
            fl = one/(lambda*depth(i))*log((klight_half+inten)/(klight_half+inten*exp(-lambda*depth(i))))
            fnnn = fl * min(fn, fp)
            grw = alg_grow(i) * conc(i,i_alg) * fnnn
            resp = alg_resp(i) * conc(i,i_alg)
            set = alg_settle(i) / depth(i) * conc(i,i_alg)
            mort = alg_die(i) * conc(i,i_alg)
            if (i_temp .gt. 0) then
               grw = grw * thetadj(it_alg_grow)
               resp = resp * thetadj(it_alg_resp)
               set = set * thetadj(it_alg_set)
               mort = mort * thetadj(it_alg_die)
            end if
            source(i,i_alg) = grw - resp - set - mort
            ! if BOD is simulated, algae mortality is a source of BOD
            if (i_bod .gt. 0) source(i,i_bod) = source(i,i_bod) + alg_bod*mort
            ! if ORG-N is simulated, algae respiration is a source of ORG-N
            if (i_orgn .gt. 0) source(i,i_orgn) = source(i,i_orgn) + algaefract_n*resp
            ! if NH3-N is simulated, algae growth demands NH3-N
            if ((i_nh3.gt.0) .and. (i_no3.eq.0)) then
                ! algae growth demands NH3-N
                source(i,i_nh3) = source(i,i_nh3) - algaefract_n*grw
            elseif ((i_nh3.eq.0) .and. (i_no3.gt.0)) then
                ! algae growth demands NO3-N
                source(i,i_no3) = source(i,i_no3) - algaefract_n*grw
            elseif ((i_nh3.gt.0) .and. (i_no3.gt.0)) then
                ! if both NH3 and NO3 are simulated, a preference factor is used.
                if ((conc(i,i_nh3).eq.zero) .and. (conc(i,i_no3).eq.zero)) then
                    factor = pref_factor
                else
                    factor = pref_factor*conc(i,i_nh3)/(pref_factor*conc(i,i_nh3)+(one-pref_factor)*conc(i,i_no3))
                    source(i,i_nh3) = source(i,i_nh3) - factor*algaefract_n*grw
                    source(i,i_no3) = source(i,i_no3) - (one-factor)*algaefract_n*grw
                end if
            end if
            ! if ORG-P is simulated, algae respiration is a source of ORG-P
            if (i_orgp .gt. 0) source(i,i_orgp) = source(i,i_orgp) + algaefract_p*resp
            ! if PO4-P is simulated, algae growth demands PO4-P
            if (i_po4 .gt. 0) source(i,i_po4) = source(i,i_po4) - algaefract_p*grw
            ! if DO is simulated, algae growth is a source, respiration demands DO
            if (i_do .gt. 0) source(i,i_do) = source(i,i_do) + oxy_photo*grw - oxy_resp*resp
         end if

         !--- Calculate source and sink terms for temeperature ---
         if (i_temp .gt. 0) then
            call calc_heat(vpair,     &
                           tsolhr,    &
                           ha,        &
                           int(time),      & !julmin,    &
                           time_step, &
                           atmpr,     &
                           wetblb,    &
                           dryblb,    &
                           cloud,     &
                           wind)

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
            hsnet = tsolhr + (ha + hc - hb - he)
            ! contribution to temperature is net radiation/(depth*density*heat)
            source(i,i_temp) = (hsnet/(depth(i)*62.4d0))/1.8d0 - conc(i,i_temp)
         end if

        end do
        return
    end subroutine

 end module