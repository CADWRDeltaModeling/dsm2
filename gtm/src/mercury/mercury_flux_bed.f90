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
!> ================================================
!> The Mercury Cycling Module is developed by:
!> Dave Hutchinson and Reed Harris
!> Reed Harris Environmental Ltd.
!> ================================================
!> hg reactions & water/air boundary fluxes
!>@ingroup mercury

module mercury_flux_bed

use gtm_precision
use sed_type_defs
use sed_internal_vars
use hg_internal_vars
use equilibrium
use mercury_state_variables
use hg_hdf
use state_variables
use sediment_bed, only: get_sed_wet_p

implicit none

contains

subroutine hg_flux_sed(n_cell, delta_t, f_wat, doc, ph, ec, rkstep)
    !args 
    integer, intent(in) :: rkstep
    integer, intent(in) :: n_cell
    type(hg_flux_def), intent(inout) :: f_wat(n_cell) 
    real (gtm_real), intent(in) :: delta_t
    real (gtm_real), intent(in)     :: doc(n_cell)             
    real (gtm_real), intent(inout)  :: ph(n_cell)              
    real (gtm_real), intent(in)     :: ec(n_cell) 
    !local
    integer             :: icell, izone, ilayer
    real (gtm_real)     :: volume_pw
    real (gtm_real)     :: check_erosion(n_cell)
    real (gtm_real)     :: check_settling(n_cell)
    real (gtm_real)     :: check_conc(n_cell)
    
    if (rkstep.eq.1) then   
        sed_hg0(:,:,:,1) = sed_hg0(:,:,:,3)
        sed_hgii(:,:,:,1) =  sed_hgii(:,:,:,3)
        sed_mehg(:,:,:,1) = sed_mehg(:,:,:,3)
        sed_s1_hgii(:,:,:,1) = sed_s1_hgii(:,:,:,3)
        sed_s2_hgii(:,:,:,1) = sed_s2_hgii(:,:,:,3)
        sed_s3_hgii(:,:,:,1) = sed_s3_hgii(:,:,:,3)
        hg_conc_sed(:,:,:,1)%hgii_inert(1) = sed_s1_hgii(:,:,:,3)/sedsolids(:,:,:,1,3)
        hg_conc_sed(:,:,:,1)%hgii_inert(2) = sed_s2_hgii(:,:,:,3)/sedsolids(:,:,:,2,3)
        hg_conc_sed(:,:,:,1)%hgii_inert(3) = sed_s3_hgii(:,:,:,3)/sedsolids(:,:,:,3,3)
    end if
   call set_sed_fluxes_to_zero(rkstep)    
    do icell =1, n_cell
        do izone = 1, n_zones
            
            do ilayer = 1, n_layers
                volume_pw = bed(icell,izone,ilayer).wp_wet*bed(icell,izone,ilayer).thickness*bed(icell,izone,ilayer).porosity
                if (ph(icell).LT.three) then
                    ph(icell)=seven
                end if
                call sed_partitioning(icell,izone,ilayer,doc(icell), ph(icell), ec(icell),rkstep)
            end do
           do ilayer = 1, n_layers
                call sed_fluxes_hg(icell,izone,ilayer,f_wat(icell), rkstep)  
            end do
        end do   
        

    end do
    
    !return !debug
    !call set_sed_fluxes_to_zero(1)
    !call set_sed_fluxes_to_zero(2)
    !call set_sed_fluxes_to_zero(3)
    if (rkstep==1) then  ! do the sed hg integration - Huens method  
        sed_hg0(:,:,:,rkstep+1)     = sed_hg0(:,:,:,rkstep)     + delta_t * f_sed_hg(:,:,:,mf_hg0, rkstep)
        sed_hgii(:,:,:,rkstep+1)    = sed_hgii(:,:,:,rkstep)    + delta_t * f_sed_hg(:,:,:,mf_hgii, rkstep)
        sed_mehg(:,:,:,rkstep+1)    = sed_mehg(:,:,:,rkstep)    + delta_t * f_sed_hg(:,:,:,mf_mehg, rkstep)
        sed_s1_hgii(:,:,:,rkstep+1) = sed_s1_hgii(:,:,:,rkstep) + delta_t * f_sed_hg(:,:,:,mf_hgii_s1, rkstep)
        sed_s2_hgii(:,:,:,rkstep+1) = sed_s2_hgii(:,:,:,rkstep) + delta_t * f_sed_hg(:,:,:,mf_hgii_s2, rkstep)
        sed_s3_hgii(:,:,:,rkstep+1) = sed_s3_hgii(:,:,:,rkstep) + delta_t * f_sed_hg(:,:,:,mf_hgii_s3, rkstep)
    else
        sed_hg0(:,:,:,rkstep+1)     = sed_hg0(:,:,:,rkstep-1)     + delta_t * (f_sed_hg(:,:,:,mf_hg0, rkstep-1)     + f_sed_hg(:,:,:,mf_hg0, rkstep))/two
        sed_hgii(:,:,:,rkstep+1)    = sed_hgii(:,:,:,rkstep-1)    + delta_t * (f_sed_hg(:,:,:,mf_hgii, rkstep-1)    + f_sed_hg(:,:,:,mf_hgii, rkstep))/two
        sed_mehg(:,:,:,rkstep+1)    = sed_mehg(:,:,:,rkstep-1)    + delta_t * (f_sed_hg(:,:,:,mf_mehg, rkstep-1)    + f_sed_hg(:,:,:,mf_mehg, rkstep))/two
        sed_s1_hgii(:,:,:,rkstep+1) = sed_s1_hgii(:,:,:,rkstep-1) + delta_t * (f_sed_hg(:,:,:,mf_hgii_s1, rkstep-1) + f_sed_hg(:,:,:,mf_hgii_s1, rkstep))/two
        sed_s2_hgii(:,:,:,rkstep+1) = sed_s2_hgii(:,:,:,rkstep-1) + delta_t * (f_sed_hg(:,:,:,mf_hgii_s2, rkstep-1) + f_sed_hg(:,:,:,mf_hgii_s2, rkstep))/two
        sed_s3_hgii(:,:,:,rkstep+1) = sed_s3_hgii(:,:,:,rkstep-1) + delta_t * (f_sed_hg(:,:,:,mf_hgii_s3, rkstep-1) + f_sed_hg(:,:,:,mf_hgii_s3, rkstep))/two

    end if
    hg_conc_sed(:,:,:,rkstep+1)%hgii_inert(1) = sed_s1_hgii(:,:,:,rkstep+1)/sedsolids(:,:,:,1,rkstep+1)
    hg_conc_sed(:,:,:,rkstep+1)%hgii_inert(2) = sed_s2_hgii(:,:,:,rkstep+1)/sedsolids(:,:,:,2,rkstep+1)
    hg_conc_sed(:,:,:,rkstep+1)%hgii_inert(3) = sed_s3_hgii(:,:,:,rkstep+1)/sedsolids(:,:,:,3,rkstep+1)
     
        !print *, f_sed_hg(144,3,1,mf_hgii_s1, rkstep), f_sed_adsorption_s1(144,3,1, rkstep), sed_s1_hgii(144,3,1, rkstep), f_sed_hg(144,3,1,mf_hgii, rkstep)
        !print *, "inert1 -wat", hg_conc_wat(1140,rkstep)%HgII_inert(1)
        !print *, "solids1-wat", sedsolids(1140,1,2,1,rkstep+1)
        !print *, "inert3", hg_conc_sed(1140,1,1,rkstep+1)%hgii_inert(3),sedsolids(1140,1,1,3,rkstep+1)
        !print *, "erosionhgs1  ", f_wat(1140)%erosion(mf_hgii_s1)
        !print *, "settlehgs11  ", f_wat(1140)%settle(mf_hgii_s1)
    check_erosion(:) = f_erosion_hg(:,1,mf_hgii,rkstep)+f_erosion_hg(:,2,mf_hgii,rkstep)+f_erosion_hg(:,3,mf_hgii,rkstep)
    f_wat%erosion(mf_hgii) =f_wat%erosion(mf_hgii) 
    !check_concs(:) = hg_conc_sed(:,1, 1, rkstep)%hgii_ssx(1)
    return
end subroutine hg_flux_sed

subroutine sed_partitioning(icell,izone,ilayer,doc, ph, ec,rkstep)
    !args 
    integer, intent(in) :: icell
    integer, intent(in) :: izone
    integer, intent(in) :: ilayer
    real (gtm_real), intent(in)     :: doc              
    real (gtm_real), intent(in)     :: ph              
    real (gtm_real), intent(in)     :: ec   
    integer, intent(in) :: rkstep
    !local
    integer                     :: iter
    logical                     :: converge
    type (molar_total)          :: total 
    type (eq_complexes)         :: m 
    real(gtm_real)              :: mass_total
    real(gtm_real)              :: volume_pw
    real(gtm_real)              :: hh
    real(gtm_real)              :: cl
    
    if (icell.eq.410) then
        iter = 0
    end if
    iter = 0
    volume_pw = bed(icell,izone,ilayer).wp_wet*bed(icell,izone,ilayer).thickness*bed(icell,izone,ilayer).porosity
    mass_total = sedsolids(icell,izone,ilayer,1,rkstep) + sedsolids(icell,izone,ilayer,2,rkstep) + sedsolids(icell,izone,ilayer,3,rkstep)
    total%XOH = sedsolids(icell,izone,ilayer,1,rkstep)*solid_parms_sed(icell,izone,ilayer,1).mole_XOH * solid_parms_sed(icell,izone,ilayer,1).frac_exchg + &
                        sedsolids(icell,izone,ilayer,2,rkstep)*solid_parms_sed(icell,izone,ilayer,2).mole_XOH * solid_parms_sed(icell,izone,ilayer,2).frac_exchg + &
                        sedsolids(icell,izone,ilayer,3,rkstep)*solid_parms_sed(icell,izone,ilayer,3).mole_XOH * solid_parms_sed(icell,izone,ilayer,3).frac_exchg
    
    cl = max(0.285d0*ec-50.0,0.15d0*ec-12.0d0)
    if (volume_pw.gt.zero) then
        total%XOH = total%XOH /volume_pw
        total%hgii = sed_hgii(icell,izone,ilayer,rkstep)/ (1.0e6*mole_hg)/ (volume_pw*1.0d3)
        total%mehg = sed_mehg(icell,izone,ilayer,rkstep)/ (1.0e6*mole_hg)/ (volume_pw*1.0d3)
        !total%rs = conc_doc_pw(icell)*mole_rs_sed(icell,izone,ilayer)
        total%rs = doc*mole_rs_sed(icell,izone,ilayer)
        if (.not.eq_vals_sed(icell,izone,ilayer)%initialized) then
            hh = 10.d0**(-conc_pH_pw(icell))
            eq_vals_sed(icell,izone,ilayer)%hgii =  (total%hgii*hh)/(k_eq_solids_sed(icell,izone,ilayer)%xohg*total%XOH)
            eq_vals_sed(icell,izone,ilayer)%mehg = (total%mehg*hh)/(k_eq_solids_sed(icell,izone,ilayer)%xomehg*total%XOH)                        
            eq_vals_sed(icell,izone,ilayer)%rs = total%rs / (one + k_eq%hgrs*eq_vals_sed(icell,izone,ilayer)%hgii + k_eq%mehgrs*eq_vals_sed(icell,izone,ilayer)%mehg)
            eq_vals_sed(icell,izone,ilayer)%xoh = total%XOH-eq_vals_sed(icell,izone,ilayer)%hgii-eq_vals_sed(icell,izone,ilayer)%mehg
            eq_vals_sed(icell,izone,ilayer)%initialized = .true.
        end if
        
        call equil_solver(eq_vals_sed(icell,izone,ilayer),  &               !> initial guess for unknowns(in) - results (out)
                            cl,                      &       !> Cl (mg/L)
                            pH,                      & 
                            zero,                                   &       !> phytoplankton (mg/L)
                            bed(icell,izone,ilayer)%porosity,       &       !> bed(icell,izone,ilayer)%porosity 
                            total, 4, 0,                            &       !> Molar concentration totals (known), number of unknowns,  itype = 0 known total hgii and Mehg (ng/L),itype = 1 known total sediment hgii and Mehg (ng/g)
                            k_eq_solids_sed(icell,izone,ilayer),    &       !> equilibrium constants for solids partitioning (compartment specific)
                            iter,                                   &       !> number of iterations to reach solution
                            converge,                               &       !> true if solution converged
                            m,                                      &
                            icell,                                  &
                            izone)
        call hg_reactant_concs(m, nosolids, sedsolids(icell,izone,ilayer,:,rkstep)/volume_pw, solid_parms_sed(icell,izone,ilayer,:), hg_conc_sed(icell,izone,ilayer,rkstep))   
    else
        eq_vals_sed(icell,izone,ilayer)%initialized = .false. 
        call set_complexes_to_zero(m)
        m%XOhg = sed_hgii(icell,izone,ilayer,rkstep)
        m%XOmehg = sed_mehg(icell,izone,ilayer,rkstep)
        call hg_reactant_concs(m, nosolids, sedsolids(icell,izone,ilayer,:,rkstep), solid_parms_sed(icell,izone,ilayer,:), hg_conc_sed(icell,izone,ilayer,rkstep))
        hg_conc_sed(icell,izone,ilayer,rkstep)%hgii_ssX(1) = hg_conc_sed(icell,izone,ilayer,rkstep)%hgii_ssX(1)/(mole_hg*1.0d9)
        hg_conc_sed(icell,izone,ilayer,rkstep)%hgii_ssX(2) = hg_conc_sed(icell,izone,ilayer,rkstep)%hgii_ssX(2)/(mole_hg*1.0d9)
        hg_conc_sed(icell,izone,ilayer,rkstep)%hgii_ssX(3) = hg_conc_sed(icell,izone,ilayer,rkstep)%hgii_ssX(3)/(mole_hg*1.0d9)
        hg_conc_sed(icell,izone,ilayer,rkstep)%mehg_ss(1) = hg_conc_sed(icell,izone,ilayer,rkstep)%mehg_ss(1)/(mole_hg*1.0d9)
        hg_conc_sed(icell,izone,ilayer,rkstep)%mehg_ss(2) = hg_conc_sed(icell,izone,ilayer,rkstep)%mehg_ss(2)/(mole_hg*1.0d9)
        hg_conc_sed(icell,izone,ilayer,rkstep)%mehg_ss(3) = hg_conc_sed(icell,izone,ilayer,rkstep)%mehg_ss(3)/(mole_hg*1.0d9)
        hg_conc_sed(icell,izone,ilayer,rkstep)%mehg_diss = zero
        hg_conc_sed(icell,izone,ilayer,rkstep)%hgii_diss = zero
    end if
    
    
    
end subroutine sed_partitioning
   
    
    
subroutine sed_fluxes_hg(icell,izone,ilayer,f_wat,rkstep)
    
    !args 
    integer, intent(in) :: icell
    integer, intent(in) :: izone
    integer, intent(in) :: ilayer
    type(hg_flux_def), intent(inout) :: f_wat
    integer, intent(in) :: rkstep

    call hg_sed_methyl(icell,izone, ilayer, conc_temp(icell), conc_so4_pw(icell),rkstep)
    call hg_sed_inert(icell,izone,ilayer,rkstep)
    call hg_sed_diffusion(icell,izone,ilayer,rkstep) !diffusion
    call hg_sed_solids(icell,izone,ilayer,rkstep)    !burial and resuspension
    call sum_hg_sed_solids_flux(icell,izone,ilayer,f_wat, rkstep)
end subroutine sed_fluxes_hg

subroutine hg_sed_methyl(icell,izone,ilayer,T,SO4, rkstep)
     !args 
    integer, intent(in) :: icell
    integer, intent(in) :: izone
    integer, intent(in) :: ilayer
    Real (gtm_real) , intent(in) :: T,SO4
    integer, intent(in) :: rkstep
    !local
    Real (gtm_real)     :: turnover
    
    turnover = carbonturnover(icell,izone,ilayer,1,rkstep) + carbonturnover(icell,izone,ilayer,2,rkstep)
    f_sed_methyl(icell,izone,ilayer,rkstep) = k_sed(icell,izone,ilayer)%methyl * hg_conc_sed(icell,izone,ilayer,rkstep)%hgii_methyl * (one + uSO4 * (SO4/ (SO4 + kSO4))) * (Q10Meth**((T-Tbmeth) / ten)) &
                                            * turnover
	f_sed_demethyl(icell,izone,ilayer,rkstep) = k_sed(icell,izone,ilayer)%biodemethyl * hg_conc_sed(icell,izone,ilayer,rkstep)%mehg_biodemeth * (Q10biodemeth**((T-Tbbiodemeth) / 10.0)) &
                                            * turnover
end subroutine hg_sed_methyl

subroutine hg_sed_inert(icell,izone,ilayer,rkstep)
    !args 
    integer, intent(in) :: icell
    integer, intent(in) :: izone
    integer, intent(in) :: ilayer
    integer, intent(in) :: rkstep
    !local
    real(gtm_real) :: adsorp
    if(icell.eq.1140) then
       adsorp = zero
    endif
    adsorp = sedsolids(icell,izone,ilayer,1,rkstep)* hg_conc_sed(icell,izone,ilayer,rkstep)%hgii_ssX(1) - sed_s1_hgii(icell,izone,ilayer,rkstep)
    if ( adsorp >= zero) then
        f_sed_adsorption_s1(icell,izone,ilayer,rkstep) = k_adsorp*adsorp 
    else
        f_sed_adsorption_s1(icell,izone,ilayer,rkstep) = k_desorp*adsorp 
    endif
    adsorp = sedsolids(icell,izone,ilayer,2,rkstep)*hg_conc_sed(icell,izone,ilayer,rkstep)%hgii_ssX(2) - sed_s2_hgii(icell,izone,ilayer,rkstep)
    if ( adsorp >= zero) then
        f_sed_adsorption_s2(icell,izone,ilayer,rkstep) = k_adsorp*adsorp
    else
        f_sed_adsorption_s2(icell,izone,ilayer,rkstep) = k_desorp*adsorp 
    end if
    
    adsorp = sedsolids(icell,izone,ilayer,3,rkstep)*hg_conc_sed(icell,izone,ilayer,rkstep)%hgii_ssX(3) - sed_s3_hgii(icell,izone,ilayer,rkstep)
    if ( adsorp >= zero) then
        f_sed_adsorption_s3(icell,izone,ilayer,rkstep) = k_adsorp*adsorp
    else
        f_sed_adsorption_s3(icell,izone,ilayer,rkstep) = k_desorp*adsorp 
    endif
    f_sed_turnover_s1(icell,izone,ilayer,rkstep)  = sed_s1_hgii(icell,izone,ilayer,rkstep) * decomposition(icell,izone,ilayer,1,rkstep) / sedsolids(icell,izone,ilayer,1,rkstep)
    f_sed_turnover_s2(icell,izone,ilayer,rkstep) = sed_s2_hgii(icell,izone,ilayer,rkstep) * decomposition(icell,izone,ilayer,2,rkstep) / sedsolids(icell,izone,ilayer,2,rkstep)

end subroutine hg_sed_inert

subroutine hg_sed_diffusion(icell,izone,ilayer,rkstep)
    !args 
    integer, intent(in) :: icell
    integer, intent(in) :: izone
    integer, intent(in) :: ilayer
    integer, intent(in) :: rkstep
    !diffusion up is +ve
    if (ilayer.eq.1) then !difussion from sed to water
        f_diffusion_hg(icell,izone,1,rkstep) =  mtc_sed_wat * (hg_conc_sed(icell,izone,ilayer,rkstep)%hgii_diss - hg_conc_wat(icell,rkstep)%hgii_diss)*bed(icell,izone,ilayer).wp_wet*length(icell)*ft_to_m
        f_diffusion_hg(icell,izone,2,rkstep) =  mtc_sed_wat * (hg_conc_sed(icell,izone,ilayer,rkstep)%mehg_diss - hg_conc_wat(icell,rkstep)%mehg_diss)*bed(icell,izone,ilayer).wp_wet*length(icell)*ft_to_m
        f_diffusion_hg(icell,izone,3,rkstep) =  mtc_sed_wat * (hg_conc_sed(icell,izone,ilayer,rkstep)%hg0 - hg_conc_wat(icell,rkstep)%hg0)*bed(icell,izone,ilayer).wp_wet*length(icell)*ft_to_m
    else  !difussion from sed layer 2 to sed layer 1
        f_sed_diffusion_hg(icell,izone,1,rkstep) =  mtc_sed_sed * (hg_conc_sed(icell,izone,ilayer,rkstep)%hgii_diss - hg_conc_sed(icell,izone,ilayer-1,rkstep)%hgii_diss)*bed(icell,izone,ilayer).wp_wet*length(icell)*ft_to_m
        f_sed_diffusion_hg(icell,izone,2,rkstep) =  mtc_sed_sed * (hg_conc_sed(icell,izone,ilayer,rkstep)%mehg_diss - hg_conc_sed(icell,izone,ilayer-1,rkstep)%mehg_diss)*bed(icell,izone,ilayer).wp_wet*length(icell)*ft_to_m
        f_sed_diffusion_hg(icell,izone,3,rkstep) =  mtc_sed_sed * (hg_conc_sed(icell,izone,ilayer,rkstep)%hg0 - hg_conc_sed(icell,izone,ilayer-1,rkstep)%hg0)*bed(icell,izone,ilayer).wp_wet*length(icell)*ft_to_m
    end if
    
end subroutine  hg_sed_diffusion

subroutine hg_sed_solids(icell,izone,ilayer, rkstep)
    !burial, settling and resuspension
    !args 
    integer, intent(in) :: icell
    integer, intent(in) :: izone
    integer, intent(in) :: ilayer
    integer, intent(in) :: rkstep
    !local
    integer             :: isolids
    
    if (ilayer.eq.1) then
        if (hg_conc_wat(icell, rkstep)%hgii_diss.GT.2.0d-3) then  !debug
        f_settling_hg(icell,izone,mf_hgii,rkstep) = (settling(icell,izone,1, rkstep)*hg_conc_wat(icell, rkstep)%hgii_ssX(1) & 
                                                  + settling(icell,izone,2, rkstep)*hg_conc_wat(icell, rkstep)%hgii_ssX(2)  &
                                                  + settling(icell,izone,3, rkstep)*hg_conc_wat(icell, rkstep)%hgii_ssX(3))
        endif
        f_settling_hg(icell,izone,mf_mehg,rkstep) = (settling(icell,izone,1, rkstep)*hg_conc_wat(icell, rkstep)%mehg_ss(1) & 
                                                  +  settling(icell,izone,2, rkstep)*hg_conc_wat(icell, rkstep)%mehg_ss(2) &
                                                  +  settling(icell,izone,3, rkstep)*hg_conc_wat(icell, rkstep)%mehg_ss(3))

        f_settling_hg(icell,izone,mf_HgII_s1,rkstep) = (settling(icell,izone,1, rkstep)*hg_conc_wat(icell, rkstep)%hgii_inert(1))
        f_settling_hg(icell,izone,mf_HgII_s2,rkstep) = (settling(icell,izone,2, rkstep)*hg_conc_wat(icell, rkstep)%hgii_inert(2))
        f_settling_hg(icell,izone,mf_HgII_s3,rkstep) = (settling(icell,izone,3, rkstep)*hg_conc_wat(icell, rkstep)%hgii_inert(3))
    
        f_erosion_hg(icell,izone,1,rkstep) = (erosion_sb(icell,izone,1,rkstep)*hg_conc_sed(icell,izone, 1, rkstep)%hgii_ssx(1) &
                                            + erosion_sb(icell,izone,2,rkstep)*hg_conc_sed(icell,izone, 1, rkstep)%hgii_ssx(2) &
                                            + erosion_sb(icell,izone,3,rkstep)*hg_conc_sed(icell,izone, 1, rkstep)%hgii_ssx(3))
    
        f_erosion_hg(icell,izone,2,rkstep) = (erosion_sb(icell,izone,1,rkstep)*hg_conc_sed(icell,izone, 1, rkstep)%mehg_ss(1) &
                                            + erosion_sb(icell,izone,2,rkstep)*hg_conc_sed(icell,izone, 1, rkstep)%mehg_ss(2) &
                                            + erosion_sb(icell,izone,3,rkstep)*hg_conc_sed(icell,izone, 1, rkstep)%mehg_ss(3))
    
        f_erosion_hg(icell,izone,4,rkstep) = (erosion_sb(icell,izone,1,rkstep)*hg_conc_sed(icell,izone, 1, rkstep)%hgii_inert(1))
        f_erosion_hg(icell,izone,5,rkstep) = (erosion_sb(icell,izone,2,rkstep)*hg_conc_sed(icell,izone, 1, rkstep)%hgii_inert(2))
        f_erosion_hg(icell,izone,6,rkstep) = (erosion_sb(icell,izone,3,rkstep)*hg_conc_sed(icell,izone, 1, rkstep)%hgii_inert(3))
    else
        f_burial_hg(icell,izone,:,rkstep) = zero
        
        if (burial(icell,izone,1,1,rkstep).gt.zero) then    !burial
            f_burial_hg(icell,izone,1,rkstep) = burial(icell,izone,1,1,rkstep)*hg_conc_sed(icell,izone,1,rkstep)%hgii_ssx(1)
            f_burial_hg(icell,izone,2,rkstep) = burial(icell,izone,1,1,rkstep)*hg_conc_sed(icell,izone,1,rkstep)%mehg_ss(1)
            f_burial_hg(icell,izone,mf_HgII_s1,rkstep) = burial(icell,izone,1,1,rkstep)*hg_conc_sed(icell,izone,1,rkstep)%hgii_inert(1)
        else                                                !erosion
            f_burial_hg(icell,izone,1,rkstep) = burial(icell,izone,1,1,rkstep)*hg_conc_sed(icell,izone,2,rkstep)%hgii_ssx(1)
            f_burial_hg(icell,izone,2,rkstep) = burial(icell,izone,1,1,rkstep)*hg_conc_sed(icell,izone,2,rkstep)%mehg_ss(1)
            f_burial_hg(icell,izone,mf_HgII_s1,rkstep) = burial(icell,izone,1,1,rkstep)*hg_conc_sed(icell,izone,2,rkstep)%hgii_inert(1)
        end if
        
        if (burial(icell,izone,1,2,rkstep).gt.zero) then
            f_burial_hg(icell,izone,1,rkstep) = f_burial_hg(icell,izone,1,rkstep) + burial(icell,izone,1,2,rkstep)*hg_conc_sed(icell,izone,1,rkstep)%hgii_ssx(2)
            f_burial_hg(icell,izone,2,rkstep) = f_burial_hg(icell,izone,2,rkstep) + burial(icell,izone,1,2,rkstep)*hg_conc_sed(icell,izone,1,rkstep)%mehg_ss(2)
            f_burial_hg(icell,izone,mf_HgII_s2,rkstep) =                            burial(icell,izone,1,2,rkstep)*hg_conc_sed(icell,izone,1,rkstep)%hgii_inert(2)
        else
            f_burial_hg(icell,izone,1,rkstep) = f_burial_hg(icell,izone,1,rkstep) + burial(icell,izone,1,2,rkstep)*hg_conc_sed(icell,izone,2,rkstep)%hgii_ssx(2)
            f_burial_hg(icell,izone,2,rkstep) = f_burial_hg(icell,izone,2,rkstep) + burial(icell,izone,1,2,rkstep)*hg_conc_sed(icell,izone,2,rkstep)%mehg_ss(2)
            f_burial_hg(icell,izone,mf_HgII_s2,rkstep) =                            burial(icell,izone,1,2,rkstep)*hg_conc_sed(icell,izone,2,rkstep)%hgii_inert(2)
        end if
        
        if (burial(icell,izone,1,3,rkstep).gt.zero) then
            f_burial_hg(icell,izone,1,rkstep) = f_burial_hg(icell,izone,1,rkstep) + burial(icell,izone,1,3,rkstep)*hg_conc_sed(icell,izone,1,rkstep)%hgii_ssx(3)
            f_burial_hg(icell,izone,2,rkstep) = f_burial_hg(icell,izone,2,rkstep) + burial(icell,izone,1,3,rkstep)*hg_conc_sed(icell,izone,1,rkstep)%mehg_ss(3)
            f_burial_hg(icell,izone,mf_HgII_s3,rkstep) =                            burial(icell,izone,1,3,rkstep)*hg_conc_sed(icell,izone,1,rkstep)%hgii_inert(3)
        else
            f_burial_hg(icell,izone,1,rkstep) = f_burial_hg(icell,izone,1,rkstep) + burial(icell,izone,1,3,rkstep)*hg_conc_sed(icell,izone,2,rkstep)%hgii_ssx(3)
            f_burial_hg(icell,izone,2,rkstep) = f_burial_hg(icell,izone,2,rkstep) + burial(icell,izone,1,3,rkstep)*hg_conc_sed(icell,izone,2,rkstep)%mehg_ss(3)
            f_burial_hg(icell,izone,mf_HgII_s3,rkstep) =                            burial(icell,izone,1,3,rkstep)*hg_conc_sed(icell,izone,2,rkstep)%hgii_inert(3)
        end if
    end if
    
end subroutine  hg_sed_solids

subroutine sum_hg_sed_solids_flux(icell,izone,ilayer, f_wat, rkstep)
    !args 
    integer, intent(in) :: icell
    integer, intent(in) :: izone
    integer, intent(in) :: ilayer
    type(hg_flux_def), intent(inout) :: f_wat
    integer, intent(in) :: rkstep
    !local
    integer             :: isolids
    !real (gtm_real)     :: convert_to_seconds
    
    !todo:dont forget hg0
    
    f_sed_hg(icell,izone,ilayer,mf_mehg, rkstep) =  f_sed_methyl(icell,izone,ilayer,rkstep) - f_sed_demethyl(icell,izone,ilayer,rkstep)  !hgii -> mehg
    f_sed_hg(icell,izone,ilayer,mf_hgii, rkstep) = -f_sed_methyl(icell,izone,ilayer,rkstep) + f_sed_demethyl(icell,izone,ilayer,rkstep)  !mehg -> hgii
    
    f_sed_hg(icell,izone,ilayer,mf_hgii_s1, rkstep) =                                                f_sed_adsorption_s1(icell,izone,ilayer,rkstep)   !hgii_s1 <-> hgii
    f_sed_hg(icell,izone,ilayer,mf_hgii, rkstep)    = f_sed_hg(icell,izone,ilayer,mf_hgii, rkstep) - f_sed_adsorption_s1(icell,izone,ilayer,rkstep)  
   
    f_sed_hg(icell,izone,ilayer,mf_hgii_s2, rkstep) =                                                f_sed_adsorption_s2(icell,izone,ilayer,rkstep)   !hgii <-> hgii_s2
    f_sed_hg(icell,izone,ilayer,mf_hgii, rkstep)    = f_sed_hg(icell,izone,ilayer,mf_hgii, rkstep) - f_sed_adsorption_s2(icell,izone,ilayer,rkstep)
    
    f_sed_hg(icell,izone,ilayer,mf_hgii_s3, rkstep) =                                                f_sed_adsorption_s3(icell,izone,ilayer,rkstep)
    f_sed_hg(icell,izone,ilayer,mf_hgii, rkstep)    = f_sed_hg(icell,izone,ilayer,mf_hgii, rkstep) - f_sed_adsorption_s3(icell,izone,ilayer,rkstep)   !hgii <-> hgii_s3
    
    f_sed_hg(icell,izone,ilayer,mf_hgii_s1, rkstep) = f_sed_hg(icell,izone,ilayer,mf_hgii_s1, rkstep) - f_sed_turnover_s1(icell,izone,ilayer,rkstep)  !hgii_s1 <-> hgii
    f_sed_hg(icell,izone,ilayer,mf_hgii, rkstep)    = f_sed_hg(icell,izone,ilayer,mf_hgii, rkstep)    + f_sed_turnover_s1(icell,izone,ilayer,rkstep)  
    
    f_sed_hg(icell,izone,ilayer,mf_hgii_s2, rkstep) = f_sed_hg(icell,izone,ilayer,mf_hgii_s2, rkstep) - f_sed_turnover_s2(icell,izone,ilayer,rkstep)  !hgii_s1 <-> hgii
    f_sed_hg(icell,izone,ilayer,mf_hgii, rkstep)    = f_sed_hg(icell,izone,ilayer,mf_hgii, rkstep)    + f_sed_turnover_s2(icell,izone,ilayer,rkstep) 
    
        ! dimensions (ncells, nzones, mercury form, RK/huens step)
    if (ilayer.eq.1) then                               !+ burial down (layer 1 -> layer 2)
        f_sed_hg(icell,izone,ilayer,mf_hgii,rkstep)    = f_sed_hg(icell,izone,ilayer,mf_hgii, rkstep)    - f_burial_hg(icell,izone,mf_hgii,rkstep) 
        f_sed_hg(icell,izone,ilayer,mf_mehg,rkstep)    = f_sed_hg(icell,izone,ilayer,mf_mehg, rkstep)    - f_burial_hg(icell,izone,mf_mehg,rkstep) 
        f_sed_hg(icell,izone,ilayer,mf_hgii_s1,rkstep) = f_sed_hg(icell,izone,ilayer,mf_hgii_s1, rkstep) - f_burial_hg(icell,izone,mf_hgii_s1,rkstep)
        f_sed_hg(icell,izone,ilayer,mf_hgii_s2,rkstep) = f_sed_hg(icell,izone,ilayer,mf_hgii_s2, rkstep) - f_burial_hg(icell,izone,mf_hgii_s2,rkstep)
        f_sed_hg(icell,izone,ilayer,mf_hgii_s3,rkstep) = f_sed_hg(icell,izone,ilayer,mf_hgii_s3, rkstep) - f_burial_hg(icell,izone,mf_hgii_s3,rkstep)
        
        f_sed_hg(icell,izone,2,mf_hgii,rkstep)    = f_sed_hg(icell,izone,2,mf_hgii,rkstep)    + f_burial_hg(icell,izone,mf_hgii,rkstep) 
        f_sed_hg(icell,izone,2,mf_mehg,rkstep)    = f_sed_hg(icell,izone,2,mf_mehg,rkstep)    + f_burial_hg(icell,izone,mf_mehg,rkstep) 
        f_sed_hg(icell,izone,2,mf_hgii_s1,rkstep) = f_sed_hg(icell,izone,2,mf_hgii_s1,rkstep) + f_burial_hg(icell,izone,mf_hgii_s1,rkstep)
        f_sed_hg(icell,izone,2,mf_hgii_s2,rkstep) = f_sed_hg(icell,izone,2,mf_hgii_s2,rkstep) + f_burial_hg(icell,izone,mf_hgii_s2,rkstep)
        f_sed_hg(icell,izone,2,mf_hgii_s3,rkstep) = f_sed_hg(icell,izone,2,mf_hgii_s3,rkstep) + f_burial_hg(icell,izone,mf_hgii_s3,rkstep)
        
        
        !f_sed_diffusion_hg(icell,izone,mf,rkstep)         !layer 2 -> layer 1  todo: check sign
        f_sed_hg(icell,izone,ilayer,mf_hgii,rkstep) = f_sed_hg(icell,izone,ilayer,mf_hgii,rkstep) + f_sed_diffusion_hg(icell,izone,mf_hgii,rkstep) 
        f_sed_hg(icell,izone,ilayer,mf_mehg,rkstep) = f_sed_hg(icell,izone,ilayer,mf_mehg,rkstep) + f_sed_diffusion_hg(icell,izone,mf_mehg,rkstep)  
        f_sed_hg(icell,izone,ilayer,mf_hg0,rkstep)  = f_sed_hg(icell,izone,ilayer,mf_hg0,rkstep)  + f_sed_diffusion_hg(icell,izone,mf_hg0,rkstep)
         
        f_sed_hg(icell,izone,2,mf_hgii,rkstep) = f_sed_hg(icell,izone,2,mf_hgii,rkstep) - f_sed_diffusion_hg(icell,izone,mf_hgii,rkstep) 
        f_sed_hg(icell,izone,2,mf_mehg,rkstep) = f_sed_hg(icell,izone,2,mf_mehg,rkstep) - f_sed_diffusion_hg(icell,izone,mf_mehg,rkstep)  
        f_sed_hg(icell,izone,2,mf_hg0,rkstep)  = f_sed_hg(icell,izone,2,mf_hg0,rkstep)  - f_sed_diffusion_hg(icell,izone,mf_hg0,rkstep)

        !fluxes between sediments and water column
            !f_erosion_hg(icell,izone,mf,rkstep)           ! +ve sed layer 1 -> wat
            !f_settling_hg(icell,izone,mf,rkstep)          ! +ve wat -> sed layer 1
        f_sed_hg(icell,izone,ilayer,mf_hgii,rkstep)    = f_sed_hg(icell,izone,ilayer,mf_hgii,rkstep)    - f_erosion_hg(icell,izone,mf_hgii,rkstep)    + f_settling_hg(icell,izone,mf_hgii,rkstep)
        f_wat%erosion(mf_hgii) =  f_wat%erosion(mf_hgii) + f_erosion_hg(icell,izone,mf_hgii,rkstep) !for debug
       f_wat%settle(mf_hgii)  =  f_wat%settle(mf_hgii) + f_settling_hg(icell,izone,mf_hgii,rkstep) !for debug
        
        f_sed_hg(icell,izone,ilayer,mf_mehg,rkstep)    = f_sed_hg(icell,izone,ilayer,mf_mehg,rkstep)    - f_erosion_hg(icell,izone,mf_mehg,rkstep)    + f_settling_hg(icell,izone,mf_mehg,rkstep)
        f_wat%erosion(mf_mehg) =  f_wat%erosion(mf_mehg) + f_erosion_hg(icell,izone,mf_mehg,rkstep)  !for debug
        f_wat%settle(mf_mehg)  =   f_wat%settle(mf_mehg) + f_settling_hg(icell,izone,mf_mehg,rkstep) !for debug
        
        f_sed_hg(icell,izone,ilayer,mf_hgii_s1,rkstep) = f_sed_hg(icell,izone,ilayer,mf_hgii_s1,rkstep) - f_erosion_hg(icell,izone,mf_hgii_s1,rkstep) + f_settling_hg(icell,izone,mf_hgii_s1,rkstep)
        f_wat%erosion(mf_hgii_s1) =  f_wat%erosion(mf_hgii_s1) + f_erosion_hg(icell,izone,mf_hgii_s1,rkstep) !for debug
        f_wat%settle(mf_hgii_s1)  =  f_wat%settle(mf_hgii_s1) + f_settling_hg(icell,izone,mf_hgii_s1,rkstep) !for debug
        
        f_sed_hg(icell,izone,ilayer,mf_hgii_s2,rkstep) = f_sed_hg(icell,izone,ilayer,mf_hgii_s2,rkstep) - f_erosion_hg(icell,izone,mf_hgii_s2,rkstep) + f_settling_hg(icell,izone,mf_hgii_s2,rkstep)
        f_wat%erosion(mf_hgii_s2) =  f_wat%erosion(mf_hgii_s2) + f_erosion_hg(icell,izone,mf_hgii_s2,rkstep) !for debug
        f_wat%settle(mf_hgii_s2)  =  f_wat%settle(mf_hgii_s2) + f_settling_hg(icell,izone,mf_hgii_s2,rkstep) !for debug
        
        f_sed_hg(icell,izone,ilayer,mf_hgii_s3,rkstep) = f_sed_hg(icell,izone,ilayer,mf_hgii_s3,rkstep) - f_erosion_hg(icell,izone,mf_hgii_s3,rkstep) + f_settling_hg(icell,izone,mf_hgii_s3,rkstep)
        f_wat%erosion(mf_hgii_s3) =  f_wat%erosion(mf_hgii_s3) + f_erosion_hg(icell,izone,mf_hgii_s3,rkstep) !for debug
        f_wat%settle(mf_hgii_s3)  =  f_wat%settle(mf_hgii_s3) + f_settling_hg(icell,izone,mf_hgii_s3,rkstep) !for debug
            !f_diffusion_hg(icell,izone,mf,rkstep)         !+ve sed layer 1 <-> wat
        
        f_sed_hg(icell,izone,ilayer,mf_hgii,rkstep) = f_sed_hg(icell,izone,ilayer,mf_hgii,rkstep) - f_diffusion_hg(icell,izone,mf_hgii,rkstep)
        !f_wat%hgii                                  = f_wat%hgii                                  + f_diffusion_hg(icell,izone,mf_hgii,rkstep)
        
        f_sed_hg(icell,izone,ilayer,mf_mehg,rkstep) = f_sed_hg(icell,izone,ilayer,mf_mehg,rkstep) - f_diffusion_hg(icell,izone,mf_mehg,rkstep)
        !f_wat%mehg                                  = f_wat%mehg                                  + f_diffusion_hg(icell,izone,mf_mehg,rkstep)
        
        f_sed_hg(icell,izone,ilayer,mf_hg0,rkstep)  = f_sed_hg(icell,izone,ilayer,mf_hg0,rkstep)  - f_diffusion_hg(icell,izone,mf_hg0,rkstep)
        !f_wat%hg0                                   = f_wat%hg0                                   + f_diffusion_hg(icell,izone,mf_hg0,rkstep)
        
    end if
    
end subroutine sum_hg_sed_solids_flux

subroutine set_sed_fluxes_to_zero(rkstep)
    !args
    integer, intent(in)     :: rkstep 
    
    f_sed_methyl(:,:,:,rkstep) = zero
    f_sed_demethyl(:,:,:,rkstep) = zero
    f_sed_adsorption_s1(:,:,:,rkstep) = zero
    f_sed_adsorption_s2(:,:,:,rkstep) = zero
    f_sed_adsorption_s3(:,:,:,rkstep) = zero

    f_sed_turnover_s1(:,:,:,rkstep) = zero
    f_sed_turnover_s2(:,:,:,rkstep) = zero

    f_burial_hg(:,:,:,rkstep) = zero
    f_sed_diffusion_hg(:,:,:,rkstep) = zero

   
    f_sed_hg(:,:,:,:,rkstep) = zero

!fluxes between sediments and water column
    f_erosion_hg(:,:,:,rkstep) = zero
    f_settling_hg(:,:,:,rkstep) = zero
    f_diffusion_hg(:,:,:,rkstep) = zero

end subroutine set_sed_fluxes_to_zero

end module