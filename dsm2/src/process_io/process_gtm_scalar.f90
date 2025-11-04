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

!> Process scalars in input textfile and save them to common_variables module
!>@ingroup process_io
module process_gtm_scalar

    contains

    subroutine process_scalar(Param, Val)
      use constants
      use common_vars
      use common_gtm_vars
      use common_qual
      use time_utilities

      implicit none
      character(len=32), intent(in)    :: Val    ! parameter Val
      character(len=32), intent(in)    :: Param  ! parameter
      integer :: itmp

 610  format(/'Unrecognized Parameter in SCALAR section:'/'Name=',a,' Value= ',a)
 615  format(/'Theta must be between 0.5 and 1.0:',f5.2)
 620  format(/a, ' ',a/a)
 630  format(/a)

      !-----run start date and time can be a DSS date (e.g. 01jan1994 0100),
      !-----or 'restart' (use date from restart file), or
      !-----'tide' (use date from tidefile)
      if (Param .eq. 'run_start_date') then
         run_start_date(1:9)=Val(1:9)
         if (run_start_date(11:14).ne.'    ') gtm_start_jmin = dble(cdt2jmin(run_start_date))
      elseif (Param .eq. 'run_start_time') then
         run_start_date(11:14)=Val(1:4)
         if (run_start_date(1:9).ne.'         ') gtm_start_jmin = dble(cdt2jmin(run_start_date))
      elseif (Param .eq. 'run_end_date') then
         run_end_date(1:9)=Val(1:9)
         if (run_end_date(11:14).ne.'    ')     gtm_end_jmin = dble(cdt2jmin(run_end_date))
      elseif (Param .eq. 'run_end_time') then
         run_end_date(11:14)=Val(1:4)
         if (run_end_date(1:9).ne.'         ') gtm_end_jmin = dble(cdt2jmin(run_end_date))
      elseif (Param .eq. 'run_length') then
         run_length=Val
      elseif (Param .eq. 'gtm_dx') then
         read(Val, '(f20.5)') gtm_dx
      elseif (Param .eq. 'hdf_out') then
         hdf_out = Val(1:7)
      elseif (Param .eq. 'dss_out') then
         dss_out = Val(1:7)
      elseif (Param .eq. 'nonnegative') then
         read(Val, '(l2)', err=810) nonnegative
      elseif (Param .eq. 'title') then
         ntitles=1
         title(ntitles)=' '
      elseif (Param .eq. 'print_start_date') then
         if (nprints .eq. 0) nprints=1
         print_start_date(nprints)(1:9)=Val(1:9)
         nprints=nprints+1
      elseif (Param .eq. 'print_start_time') then
         if (nprints .eq. 0) nprints=1
         print_start_date(nprints)(11:14)=Val(1:4)
      elseif (Param .eq. 'flush_output') then
         flush_intvl=Val
      elseif (Param .eq. 'binary_output') then
         read(Val, '(l4)', err=810) binary_output
      elseif (Param .eq. 'dss_direct') then
         read(Val, '(l4)', err=810) dss_direct
      elseif (Param .eq. 'hydro_time_step') then
         time_step_intvl_hydro=Val
      elseif (Param .eq. 'qual_time_step') then
         time_step_intvl_qual=Val
      elseif (Param .eq. 'gtm_time_step') then
         time_step_intvl_gtm=Val
         call get_time_intvl_min(gtm_time_interval, trim(time_step_intvl_gtm))
      elseif (Param .eq. 'mass_tracking') then
         read(Val, '(l2)', err=810) mass_tracking
      elseif (Param .eq. 'init_conc') then
         read(Val, '(f10.0)', err=810) init_conc
      elseif (Param .eq. 'checkdata') then
         read(Val, '(l2)', err=810) checkdata
      elseif (Param .eq. 'cont_bad') then
         read(Val, '(l2)', err=810) cont_bad
      elseif (Param .eq. 'cont_missing') then
         read(Val, '(l2)', err=810) cont_missing
      elseif (Param .eq. 'cont_question') then
         read(Val, '(l2)', err=810) cont_question
      elseif (Param .eq. 'cont_unchecked') then
         read(Val, '(l2)', err=810) cont_unchecked
      elseif (Param .eq. 'warn_missing') then
         read(Val, '(l2)', err=810) warn_missing
      elseif (Param .eq. 'warn_question') then
         read(Val, '(l2)', err=810) warn_question
      elseif (Param .eq. 'warn_unchecked') then
         read(Val, '(l2)', err=810) warn_unchecked
      elseif (Param .eq. 'dispersion') then
         read(Val, '(l2)', err=810) apply_diffusion
      elseif (Param .eq. 'disp_coef') then
         read(Val, '(f20.5)', err=810) disp_coeff
      elseif (Param .eq. 'ero_coef') then
         read(Val, '(f20.15)', err=810) ero_coeff
      elseif (Param .eq. 'calc_budget') then
         read(Val, '(l2)', err=810) calc_budget
      elseif (Param .eq. 'run_pdaf') then
         read(Val, '(l2)', err=810) run_pdaf
      !--------------mercury and sediment bed related parameters
      elseif (Param .eq. 'run_mercury') then
         read(Val, '(l2)', err=810) run_mercury

      elseif (Param .eq. 'use_mercury_ic') then
         read(Val, '(l2)', err=810) use_mercury_ic
      elseif (Param .eq. 'use_sediment_bed') then
         read(Val, '(l2)', err=810) use_sediment_bed

      elseif (Param .eq. 'mole_XOH_1_w') then
         read(Val, '(f20.15)', err=810) solid_parms_wat1.mole_XOH
      elseif (Param .eq. 'mole_XOH_2_w') then
         read(Val, '(f20.15)', err=810) solid_parms_wat2.mole_XOH
      elseif (Param .eq. 'mole_XOH_3_w') then
         read(Val, '(f20.15)', err=810) solid_parms_wat3.mole_XOH
      elseif (Param .eq. 'frac_exchg_1_w') then
         read(Val, '(f20.15)', err=810) solid_parms_wat1.frac_exchg
      elseif (Param .eq. 'frac_exchg_2_w') then
         read(Val, '(f20.15)', err=810) solid_parms_wat2.frac_exchg
      elseif (Param .eq. 'frac_exchg_3_w') then
         read(Val, '(f20.15)', err=810) solid_parms_wat3.frac_exchg

      elseif (Param .eq. 'k_XOHg_wat') then
         read(Val,'(f20.15)', err=810) k_eq_solids_wat.xohg
         k_eq_solids_wat.xohg = 10**k_eq_solids_wat.xohg
      elseif (Param .eq. 'k_XOMeHg_wat') then
         read(Val, '(f20.15)', err=810) k_eq_solids_wat.xomehg
         k_eq_solids_wat.xomehg = 10**k_eq_solids_wat.xomehg
      elseif (Param .eq. 'mole_RS_wat') then
         read(Val, '(f20.15)', err=810) mole_rs

      elseif (Param .eq. 'k_HgRS') then             !equilibrium constant for DOC binding sites
         read(Val, '(f20.15)', err=810) k_eq.HgRS
         k_eq.HgRS = 10**k_eq.HgRS
      elseif (Param .eq. 'k_HgRS_2') then
         read(Val, '(f20.15)', err=810) k_eq.Hg_RS_2
         k_eq.Hg_RS_2 = 10**k_eq.Hg_RS_2
      elseif (Param .eq. 'k_MeHgRS') then
         read(Val, '(f20.15)', err=810) k_eq.MeHgRS
         k_eq.MeHgRS = 10**k_eq.MeHgRS

      elseif (Param .eq. 'cfrac_labile') then
         read(Val, '(f20.15)', err=810) cfrac_labile
      elseif (Param .eq. 'cfrac_refract') then
         read(Val, '(f20.15)', err=810) cfrac_refract

      elseif (Param .eq. 'r_methyl_int') then
      !   read(Val, '(f20.15)', err=810) r_methyl_int
      elseif (Param .eq. 'k_adsorp') then
        read(Val, '(f20.15)', err=810) k_adsorp
      elseif (Param .eq. 'k_desorp') then
        read(Val, '(f20.15)', err=810) k_desorp
      elseif (Param .eq. 'k_PhotoDeg_UVA') then
        read(Val, '(f20.15)', err=810) k_PhotoDeg_UVA
      elseif (Param .eq. 'k_PhotoDeg_UVB') then
        read(Val, '(f20.15)', err=810) k_PhotoDeg_UVB
      elseif (Param .eq. 'k_PhotoDeg_PAR') then
        read(Val, '(f20.15)', err=810) k_PhotoDeg_PAR
      elseif (Param .eq. 'k_PhotoRed_UVA') then
        read(Val, '(f20.15)', err=810) k_PhotoRed_UVA
      elseif (Param .eq. 'k_PhotoRed_UVB') then
        read(Val, '(f20.15)', err=810) k_PhotoRed_UVB
      elseif (Param .eq. 'k_PhotoRed_PAR') then
        read(Val, '(f20.15)', err=810) k_PhotoRed_PAR
      elseif (Param .eq. 'k_PhotoOxid_UVA') then
        read(Val, '(f20.15)', err=810) k_PhotoOxid_UVA
      elseif (Param .eq. 'k_PhotoOxid_UVB') then
        read(Val, '(f20.15)', err=810) k_PhotoOxid_UVB
      elseif (Param .eq. 'k_PhotoOxid_PAR') then
        read(Val, '(f20.15)', err=810) k_PhotoOxid_PAR
      elseif (Param .eq. '') then
        read(Val, '(f20.15)', err=810)
      elseif (Param .eq. 'k_oxid_1') then
        read(Val, '(f20.15)', err=810) k_oxid_1
      elseif (Param .eq. 'k_oxid_2') then
        read(Val, '(f20.15)', err=810) k_oxid_2
      elseif (Param .eq. 'k_oxid_3') then
        read(Val, '(f20.15)', err=810) k_oxid_3
      elseif (Param .eq. '') then
        read(Val, '(f20.15)', err=810)
      elseif (Param .eq. 'ke_UVA_1') then
        read(Val, '(f20.15)', err=810) ke_UVA_1
      elseif (Param .eq. 'ke_UVA_2') then
        read(Val, '(f20.15)', err=810) ke_UVA_2
      elseif (Param .eq. 'ke_UVA_3') then
        read(Val, '(f20.15)', err=810) ke_UVA_3
      elseif (Param .eq. 'ke_UVB_1') then
        read(Val, '(f20.15)', err=810) ke_UVB_1
      elseif (Param .eq. 'ke_UVB_2') then
        read(Val, '(f20.15)', err=810) ke_UVB_2
      elseif (Param .eq. 'ke_UVB_3') then
        read(Val, '(f20.15)', err=810) ke_UVB_3
      elseif (Param .eq. 'ke_PAR_1') then
        read(Val, '(f20.15)', err=810) ke_PAR_1
      elseif (Param .eq. 'ke_PAR_2') then
        read(Val, '(f20.15)', err=810) ke_PAR_2
      elseif (Param .eq. 'ke_PAR_3') then
        read(Val, '(f20.15)', err=810) ke_PAR_3
      elseif (Param .eq. 'Henry_Hg0') then
        read(Val, '(f20.15)', err=810) Henry_Hg0
      elseif (Param .eq. 'MTC_Hg0Wat') then
        read(Val, '(f20.15)', err=810) MTC_Hg0Wat
      elseif (Param .eq. 'MTC_Hg0Air') then
        read(Val, '(f20.15)', err=810) MTC_Hg0Air
      elseif (Param .eq. 'Henry_MeHg') then
        read(Val, '(f20.15)', err=810) Henry_MeHg
      elseif (Param .eq. 'MTC_MeHgWat') then
        read(Val, '(f20.15)', err=810) MTC_MeHgWat
      elseif (Param .eq. 'MTC_MeHgAir') then
        read(Val, '(f20.15)', err=810) MTC_MeHgAir
      elseif (Param .eq. 'v_rgm') then
        read(Val, '(f20.15)', err=810) v_rgm
      elseif (Param .eq. 'Q10meth') then
        read(Val, '(f20.15)', err=810) Q10meth
      elseif (Param .eq. 'Tbmeth') then
        read(Val, '(f20.15)', err=810) Tbmeth
      elseif (Param .eq. 'Q10biodemeth') then
        read(Val, '(f20.15)', err=810) Q10biodemeth
      elseif (Param .eq. 'Tbbiodemeth') then
        read(Val, '(f20.15)', err=810) Tbbiodemeth
      elseif (Param .eq. 'kSO4') then
        read(Val, '(f20.15)', err=810) kSO4
      elseif (Param .eq. 'uSO4') then
        read(Val, '(f20.15)', err=810) uSO4
      elseif (Param .eq. 'mtc_sed_sed') then
        read(Val, '(f20.15)', err=810) mtc_sed_sed
      elseif (Param .eq. 'mtc_sed_wat') then
        read(Val, '(f20.15)', err=810) mtc_sed_wat


      !--------global rates for non-conserative const.
      elseif (Param .eq. 'algaefract_n') then
         read(Val, '(f8.3)', err=810) algaefract_n
      elseif (Param .eq. 'algaefract_p') then
         read(Val, '(f8.3)', err=810) algaefract_p
      elseif (Param .eq. 'oxy_photo') then
         read(Val, '(f8.3)', err=810) oxy_photo
      elseif (Param .eq. 'oxy_resp') then
         read(Val, '(f8.3)', err=810) oxy_resp
      elseif (Param .eq. 'oxy_nh3') then
         read(Val, '(f8.3)', err=810) oxy_nh3
      elseif (Param .eq. 'oxy_no2') then
         read(Val, '(f8.3)', err=810) oxy_no2
      elseif (Param .eq. 'alg_chl_ratio') then
         read(Val, '(f8.3)', err=810) alg_chl_ratio
      elseif (Param .eq. 'pref_factor') then
         read(Val, '(f8.3)', err=810) pref_factor
      elseif (Param .eq. 'klight_half') then
         read(Val, '(f8.3)', err=810) klight_half
      elseif (Param .eq. 'knit_half') then
         read(Val, '(f8.3)', err=810) knit_half
      elseif (Param .eq. 'kpho_half') then
         read(Val, '(f8.3)', err=810) kpho_half
      elseif (Param .eq. 'lambda0') then
         read(Val, '(f8.2)', err=810) lambda0
      elseif (Param .eq. 'lambda1') then
         read(Val, '(f8.4)', err=810) lambda1
      elseif (Param .eq. 'lambda2') then
         read(Val, '(f8.4)', err=810) lambda2
      !--------------heat and temperature related parameters
      elseif (Param .eq. 'elev') then
         read(Val, '(f8.2)', err=810) elev
      elseif (Param .eq. 'lat') then
         read(Val, '(f8.2)', err=810) lat
      elseif (Param .eq. 'long') then
         read(Val, '(f8.2)', err=810) longitude
      elseif (Param .eq. 'long_std_merid') then
         read(Val, '(f8.2)', err=810) long_std_merid
      elseif (Param .eq. 'dust_attcoeff') then
         read(Val, '(f8.2)', err=810) dust_attcoeff
      elseif (Param .eq. 'evapcoeff_a') then
         read(Val, '(f10.5)', err=810) evapcoeff_a
      elseif (Param .eq. 'evapcoeff_b') then
         read(Val, '(f10.5)', err=810) evapcoeff_b
      elseif (Param .eq. 'temp_bod_decay') then
         read(Val, '(f8.3)', err=810) thet(temp_bod_decay)
      elseif (Param .eq. 'temp_bod_set') then
         read(Val, '(f8.3)', err=810) thet(temp_bod_set)
      elseif (Param .eq. 'temp_reaer') then
         read(Val, '(f8.3)', err=810) thet(temp_reaer)
      elseif (Param .eq. 'temp_do_ben') then
         read(Val, '(f8.3)', err=810) thet(temp_do_ben)
      elseif (Param .eq. 'temp_orgn_decay') then
         read(Val, '(f8.3)', err=810) thet(temp_orgn_decay)
      elseif (Param .eq. 'temp_orgn_set') then
         read(Val, '(f8.3)', err=810) thet(temp_orgn_set)
      elseif (Param .eq. 'temp_nh3_decay') then
         read(Val, '(f8.3)', err=810) thet(temp_nh3_decay)
      elseif (Param .eq. 'temp_nh3_ben') then
         read(Val, '(f8.3)', err=810) thet(temp_nh3_ben)
      elseif (Param .eq. 'temp_no2_decay') then
         read(Val, '(f8.3)', err=810) thet(temp_no2_decay)
      elseif (Param .eq. 'temp_orgp_decay') then
         read(Val, '(f8.3)', err=810) thet(temp_orgp_decay)
      elseif (Param .eq. 'temp_orgp_set') then
         read(Val, '(f8.3)', err=810) thet(temp_orgp_set)
      elseif (Param .eq. 'temp_po4_ben') then
         read(Val, '(f8.3)', err=810) thet(temp_po4_ben)
      elseif (Param .eq. 'temp_alg_grow') then
         read(Val, '(f8.3)', err=810) thet(temp_alg_grow)
      elseif (Param .eq. 'temp_alg_resp') then
         read(Val, '(f8.3)', err=810) thet(temp_alg_resp)
      elseif (Param .eq. 'temp_alg_set') then
         read(Val, '(f8.3)', err=810) thet(temp_alg_set)
      elseif (Param .eq. 'tf_start_date') then
         tf_start_date(1:9)=Val(1:9)
      elseif (Param .eq. 'tf_start_time') then
         tf_start_date(11:14)=Val(1:4)
	  elseif (Param .eq. 'alg_bod' ) then
           read(Val, '(f8.4)', err=810) alg_bod
	  elseif (Param .eq. 'temp_alg_die') then
           read(Val, '(f8.3)', err=810) thet(temp_alg_die)
      elseif (Param .eq. 'display_intvl') then
         display_intvl=Val
      elseif (Param .eq. 'printlevel') then
            read(Val, '(i5)', err=810) print_level
      elseif (Param .eq. 'temp_dir') then
         temp_dir=Val
      elseif (Param .eq. 'repeating_tide') then
         write(unit_error,610)"repeating_tide is deprecated"
      elseif (Param .eq. 'warmup_run') then
         call exit(-2)
      elseif (Param .eq. 'debug_print') then
         read(Val, '(l2)', err=810) debug_print
      else
         write(unit_error,610), Param, Val
         call exit(-1)
      endif
      return
 810  continue
      write(unit_error, '(a,a)') 'Type conversion error on field ' //   &
          Param
      call exit(-2)
    end subroutine

end module