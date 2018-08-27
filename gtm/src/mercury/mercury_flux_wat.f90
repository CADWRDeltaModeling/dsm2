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
!> Hg reactions & water/air boundary fluxes
!>@ingroup mercury

module mercury_flux_wat
    
    use gtm_precision
    use equilibrium
    use mercury_state_variables
    use hg_internal_vars

contains
    
!> water compartment fluxes for cell
subroutine hg_flux_wat(area,        &
                   volume,          &
                   depth,           &
                   ipar,            &
                   iuva,            &
                   iuvb,            &
                   doc,             &
                   pH,              &
                   nosolids,        &
                   solids,          &
                   Hg_inert,        &
                   SO4,             &
                   T,               &
                   Hg0_atm,         &
                   MeHg_atm,        &
                   precip,          &
                   wetdep_HgII,     &
                   drydep_HgII,     &
                   rgm_atm,         &
                   wetdep_MeHg,     &
                   drydep_MeHg,     &
                   dgm_ratio,       &
                   rct_interface,   &
                   k,               &
                   solid_in,        &
                   concs,           &   
                   r)
!arguments
    real (gtm_real), intent (in)                :: area         !> cell surface area (m2)
    real (gtm_real), intent (in)                :: volume       !> cell area (m3)
    real (gtm_real), intent (in)                :: depth        !> cell depth (m)
    real (gtm_real), intent (in)                :: ipar         !> surface light intensity - PAR (kJ/m2/d) - from TS
    real (gtm_real), intent (in)                :: iuva         !> surface light intensity - UVA (kJ/m2/d) - from TS
    real (gtm_real), intent (in)                :: iuvb         !> surface light intensity - UVB (kJ/m2/d) - from TS
    real (gtm_real), intent (in)                :: doc          !> doc (mg/L) - from TS or GTM (?)
    real (gtm_real), intent (in)                :: pH           !> doc (mg/L) - from TS
    integer , intent (in)                       :: nosolids     !> total suspended solids (mg/L) - from GTM
    real (gtm_real), dimension(nosolids), intent (in) :: solids !> total suspended solids (mg/L) - from GTM
    real (gtm_real), dimension(nosolids), intent (in) :: Hg_Inert     !> Hg_Inert on each solids type - from GTM (ug/g)
    real (gtm_real), intent (in)                :: SO4          !> sulfate (mg/L) - from TS
    real (gtm_real), intent (in)                :: T            !> temperature (C) - from TS or GTM (?)
    real (gtm_real), intent (in)                :: Hg0_atm      !> conc of Hg0 in atmosphere (ug/m3) - from TS
    real (gtm_real), intent (in)                :: MeHg_atm     !> conc of MeHg in atmosphere (ug/m3) - from TS
    real (gtm_real), intent (in)                :: precip       !> prcipitation (mm/d) - from TS
    real (gtm_real), intent (in)                :: wetdep_HgII  !> dry deposition (ng/L) - from TS
    real (gtm_real), intent (in)                :: drydep_HgII  !> dry deposition (ug/m2/d) - from TS
    real (gtm_real), intent (in)                :: rgm_atm      !> dry deposition (pg/m3) - from TS
    real (gtm_real), intent (in)                :: wetdep_MeHg  !> dry deposition (ng/L) - from TS
    real (gtm_real), intent (in)                :: drydep_MeHg  !> dry deposition (ug/m2/d) - from TS
    real (gtm_real), intent (in)                :: dgm_ratio    !> DGM ratio - ratio of Hg0 to HgII - from TS
    real (gtm_real), intent (in)                :: rct_interface    !> carbon turnover at interface (g/m2/day) from GTM or TS (?)
    type (hg_rate_parms_t), intent (in)           :: k            !> hg reaction rate parmeters
    type (solids_inputs_t), dimension (nosolids),intent(in) :: solid_in    !> solids adsorption desorption inputs
    type (hg_concs), intent (in)                :: concs        !> hg concs for reactions (ng/L) -> (ug/m3)
    type (hg_flux_def), intent (inout)          :: r            !> reaction and fluxes at air/water boundary
!local
    real (gtm_real) :: IntegralPAR
    real (gtm_real) :: IntegralUVA
    real (gtm_real) :: IntegralUVB
    real (gtm_real) :: MTC_Hg0
    real (gtm_real) :: MTC_MeHg
    real (gtm_real) :: total_solids             !> for light extinction calcs
    real (gtm_real) :: delta_conc               !> driving force for rate limited adsorption
    real (gtm_real) :: dgmtoHgII
    real (gtm_real) :: day_to_sec
    integer         :: ii
    
    day_to_sec = 60d0*60d0*24d0
    total_solids = 0.0          
    do ii= 1, nosolids
        total_solids = total_solids + solids(ii)
    enddo
    !photochemical reactions
    call light_extinction_integration(ke_PAR_1, ke_PAR_2, ke_PAR_3, doc, total_solids, depth, integralPAR)
    call light_extinction_integration(ke_UVA_1, ke_UVA_2, ke_UVA_3, doc, total_solids, depth, integralUVA)
    call light_extinction_integration(ke_UVB_1, ke_UVB_2, ke_UVB_3, doc, total_solids, depth, integralUVB)
    r%photodemethyl  = area*(ipar*IntegralPAR*k_PhotoDeg_PAR  + iuva* IntegralUVA*k_PhotoDeg_UVA  + iuvb* IntegralUVB*k_PhotoDeg_UVB) *concs%MeHg_photo
    r%photoreduction = area*(ipar*IntegralPAR*k_PhotoRed_PAR  + iuva* IntegralUVA*k_PhotoRed_UVA  + iuvb* IntegralUVB*k_PhotoRed_UVB) *concs%HgII_photo
    !r%oxidation      = area*(ipar*IntegralPAR*k_PhotoOxid_PAR + iuva* IntegralUVA*k_PhotoOxid_UVA + iuvb* IntegralUVB*k_PhotoOxid_UVB)*concs%Hg0
    dgmtoHgII = concs%Hg0/concs%HgII_diss
    !dgm_ratio = k_oxid_1*doc**k_oxid_2*pH**k_oxid_3   !>to calculate locally
    if (((dgmtoHgII) > dgm_ratio)) then
        r%oxidation = r%photoreduction * dgmtoHgII / dgm_ratio
    else
        r%oxidation = zero
    end if
                   
    !>evasion/volotilization
    MTC_Hg0  =  1.0/ ((1.0/MTC_Hg0Wat) + (1.0/(MTC_Hg0Air*Henry_Hg0)))
	MTC_MeHg =  1.0/ ((1.0/MTC_MeHgWat) + (1.0/(MTC_MeHgAir*Henry_MeHg)))
    r%evasion_Hg0 = area*MTC_Hg0* (concs%Hg0 - (Hg0_atm/Henry_Hg0))
    r%volatil_MeHg = area*MTC_MeHg*(concs%MeHg_Cl - (MeHg_atm/Henry_MeHg))
    
    !>deposition
    r%drydep_MeHg = area*drydep_MeHg
    r%wetdep_MeHg = area*precip*wetdep_MeHg
    r%drydep_HgII = area*drydep_HgII
    r%wetdep_HgII = area*precip*wetdep_HgII
    r%rgmdep_HgII = area*v_rgm*rgm_atm *1.0d-6    !todo: check units
    
    !>methylation/bio-demethylation at interface
    r%methyl_int        = area * k%methyl_int * concs%HgII_methyl * (one + uSO4 * (SO4/ (SO4 + kSO4))) * (Q10Meth**((T-Tbmeth) / ten)) * rct_interface
	r%biodemethyl_int   = area * k%biodemethyl_int * concs%MeHg_biodemeth * (Q10biodemeth**((T-Tbbiodemeth) / ten))  * rct_interface
    !r%methyl            = k%methyl *rct_water* concs%HgII_methyl * Q10meth**((T-Tbmeth) / ten)*vol_frac   !> methylation fraction
	!r%biodemethyl       = k%biodemethyl*rct_water* concs%MeHg_biodemeth* (Q10biodemeth**((T-Tbbiodemeth)/ten))
    
    !> rate limited adsorption/desorption HgII <-> HgII_inert
    do ii = 1, nosolids
        if (solid_in(ii)%frac_exchg < 1.0d0) then
            delta_conc = (concs%HgII_ssX(ii) * (one-solid_in(ii)%frac_exchg)) /solid_in(ii)%frac_exchg - Hg_inert(ii)
            if (delta_conc >= zero) then
                r%adsorption(ii) = k_adsorp * delta_conc * solids(ii) * area * depth  !> adsorption
            else
                r%adsorption(ii) = k_desorp * delta_conc * solids(ii) * area * depth  !> desorption
            end if
        end if
        r%decomposition(ii) = area * rct_interface * Hg_inert(2)/ cfrac_labile !> for watercolumn interface carbon turnover todo: this is wrong
    end do
      
    
end subroutine hg_flux_wat


!>light extinction in water column
!>  given mean depth (m) of cell
!>  integral needs to be multiplied by cell area and
!>  surface light intensity when calculating
!>  photochemical flux for entire cell
subroutine light_extinction_integration(ke1,        &           !> light extinction parameter for DOC (mg/L)
                                        ke2,        &           !> light extinction parameter for solids (mg/L)
                                        ke3,        &           !> light extinction parameter for water (mg/L)
                                        DOC,        &           !> doc (mg/L)
                                        solids,     &           !> total solids (mg/L)
                                        depth,      &           !> mean cell depth
                                        integral)               ! light extinction integral
!arguments
    real (gtm_real), intent (in) :: ke1
    real (gtm_real), intent (in) :: ke2
    real (gtm_real), intent (in) :: ke3
    real (gtm_real), intent (in) :: DOC
    real (gtm_real), intent (in) :: solids
    real (gtm_real), intent (in) :: depth
    real (gtm_real), intent (inout) :: integral
!local
    real (gtm_real) :: ke               !> overall light extinction coefficient
    real (gtm_real) :: xtmp
    
    ke = ke1*DOC + ke2*solids + ke3
    if (ke > 0) then
        ke = ke1*DOC + ke2*solids + ke3
        integral = (one-exp(-ke * depth))/ke
    else
        integral = depth
    endif
end subroutine light_extinction_integration
    
    
!>sum reactions and fluxes at air/water boundary
subroutine sum_fluxes(r)
    !args
    type (hg_flux_def) , intent (inout) :: r            !< reaction and fluxes at air/water boundary
    !local
    integer :: ii
    !todo:: order fluxes to minimize round off error
    r%MeHg = r%methyl + r%methyl_int - r%photodemethyl - r%biodemethyl - r%biodemethyl_int + r%drydep_Mehg + r%wetdep_MeHg -r%volatil_MeHg
    r%HgII = r%oxidation - r%photoreduction + r%wetdep_HgII + r%drydep_HgII + r%RGMdep_HgII &
             + r%photodemethyl + r%biodemethyl + r%biodemethyl_int - r%methyl - r%methyl_int !&
             !- r%adsorption
    do ii=1,nosolids
        r%Hg_inert(ii) = r%Hg_inert(ii) + r%adsorption(ii)
        r%HgII = r%HgII - r%adsorption(ii)
        if (ii.eq.2) then       !interface decomposition
            r%Hg_inert(ii) = r%Hg_inert(ii) - r%decomposition(ii)
            r%HgII = r%HgII + r%decomposition(ii)
        endif
    end do
         
    !todo: add interface turnover to Hg_inert flux
    !r%Hg_inert = r%adsorption
    r%Hg0  = r%photoreduction - r%oxidation - r%evasion_Hg0
end subroutine sum_fluxes

subroutine fluxes_to_hdfarray(time_index,           &
                              cellno,               &
                              area,                 &
                              r,                    &
                              ncells,               &
                              nfluxvals,            &
                              hgii_flux_data)
!arguments
    integer, intent (in):: time_index
    integer, intent (in):: cellno
    type (hg_flux_def) , intent (in) :: r
    real (gtm_real), intent (in)     :: area
    integer, intent (in)             :: ncells
    integer, intent (in)             :: nfluxvals
    real (gtm_real), dimension(ncells,nfluxvals,1), intent (inout)  :: hgii_flux_data
   !local
    integer :: ii
    hgii_flux_data(cellno, 1,1) = 365.0d0*r%methyl/area
    hgii_flux_data(cellno, 2,1) = 365.0d0*r%methyl_int/area
    hgii_flux_data(cellno, 3,1) = 365.0d0*r%biodemethyl/area
    hgii_flux_data(cellno, 4,1) = 365.0d0*r%biodemethyl_int/area
    hgii_flux_data(cellno, 5,1) = 365.0d0*r%photodemethyl/area
    hgii_flux_data(cellno, 6,1) = 365.0d0*r%drydep_MeHg/area
    hgii_flux_data(cellno, 7,1) = 365.0d0*r%wetdep_MeHg/area
    hgii_flux_data(cellno, 8,1) = 365.0d0*r%volatil_MeHg/area
    hgii_flux_data(cellno, 9,1) = 365.0d0*r%drydep_HgII/area
    hgii_flux_data(cellno,10,1) = 365.0d0*r%wetdep_HgII/area
    hgii_flux_data(cellno,11,1) = 365.0d0*r%rgmdep_HgII/area
    hgii_flux_data(cellno,12,1) = 365.0d0*r%photoreduction/area
    hgii_flux_data(cellno,13,1) = 365.0d0*r%oxidation/area
    hgii_flux_data(cellno,14,1) = 365.0d0*r%evasion_Hg0/area
    hgii_flux_data(cellno,15,1) = zero
    do ii =1, nosolids
        hgii_flux_data(cellno,15,1) = hgii_flux_data(cellno,15,1) + r%adsorption(ii)
    end do
    hgii_flux_data(cellno,15,1) = 365.0d0*hgii_flux_data(cellno,15,1) / area
end subroutine fluxes_to_hdfarray

subroutine set_fluxes_to_zero(r, ncells)
   !arguments
   type (hg_flux_def) , intent (inout) :: r(ncells)            !< reaction and fluxes at air/water boundary
   integer, intent(in)                 :: ncells
   !local
   integer                             :: ii
   integer                             :: jj
   r%methyl = zero
   r%methyl_int = zero
   r%biodemethyl = zero
   r%biodemethyl_int = zero
   r%photodemethyl = zero
   r%drydep_MeHg = zero
   r%wetdep_MeHg = zero
   r%volatil_MeHg = zero
   r%photoreduction = zero
   r%drydep_HgII = zero
   r%wetdep_HgII = zero
   r%rgmdep_HgII = zero
   r%oxidation = zero
   r%evasion_Hg0 = zero
   r%adsorption(1) = zero
   r%adsorption(2) = zero
   r%adsorption(3) = zero
   r%decomposition(1) = zero
   r%decomposition(2) = zero
   r%decomposition(3) = zero
   r%MeHg = zero
   r%HgII = zero
   r%Hg_inert(1) = zero
   r%Hg_inert(2) = zero
   r%Hg_inert(3) = zero
   r%Hg0 = zero
   r%erosion(1) = zero
   r%erosion(2) = zero
   r%erosion(3) = zero
   r%erosion(4) = zero
   r%erosion(5) = zero
   r%erosion(6) = zero
   r%settle(1) = zero
   r%settle(2) = zero
   r%settle(3) = zero
   r%settle(4) = zero
   r%settle(5) = zero
   r%settle(6) = zero
   
end subroutine set_fluxes_to_zero 
    
subroutine wat_partitioning(icell, conc_hgii, conc_mehg, solids, doc, ph, ec, nsolids, rkstep)
    !args 
    integer, intent(in)             :: icell
    real (gtm_real), intent(in)     :: conc_hgii             !ng/L ????
    real (gtm_real), intent(in)     :: conc_mehg             !ng/L ????
    real (gtm_real), intent(in)     :: doc              
    real (gtm_real), intent(inout)  :: ph              
    real (gtm_real), intent(in)     :: ec     
    integer, intent(in)             :: nsolids
    real (gtm_real), intent(in)     :: solids(nsolids) 
    integer, intent(in) :: rkstep
    !local
    integer                     :: iter
    logical                     :: converge
    type (molar_total)          :: total 
    type (eq_complexes)         :: m 
    real(gtm_real)              :: mass_total
    real(gtm_real)              :: hh
    real(gtm_real)              :: cl
    integer                     :: order
    real(gtm_real)              :: kd   !debug
    if (ph.LT.three) then      !debug
        !print *,"invalid ph for cell  :", icell
        !print *,"       ph            :", ph
        !print *,"       ph set to 7.0"
        ph = seven
    end if
    iter = 0
    order = 4
    total%XOH = solids(1)*solid_parms_wat(1).mole_XOH * solid_parms_wat(1).frac_exchg + &
                solids(2)*solid_parms_wat(2).mole_XOH * solid_parms_wat(2).frac_exchg + &
                solids(3)*solid_parms_wat(3).mole_XOH * solid_parms_wat(3).frac_exchg
    total%XOH = total%XOH !/1.0d3                  !mg->g assuming solids are in units of mg/L
    total%hgii = conc_hgii/ (1.0e9*mole_hg)           !todo: check units
    total%mehg = conc_mehg/ (1.0e9*mole_hg)           !todo: check units
    total%rs = doc*mole_rs
    hh = 10.d0**(ph)
    if (.not.eq_vals_wat(icell)%initialized) then
        
        !if (total%XOH.gt.zero) then
            eq_vals_wat(icell)%hgii =  (total%hgii)/(one + k_eq_solids_wat%xohg*total%XOH/hh + k_eq%hgrs*total%rs )
            eq_vals_wat(icell)%mehg = (total%mehg*hh)/(one + k_eq_solids_wat%xomehg*total%XOH + k_eq%mehgrs*total%rs)
        !else
        !    eq_vals_wat(icell)%hgii = total%hgii/(one + k_eq%hgrs*total%rs)
        !    eq_vals_wat(icell)%hgii = total%mehg/(one + k_eq%mehgrs*total%rs)
        !    order = 3
        !end if
        eq_vals_wat(icell)%rs = total%rs / (one + k_eq%hgrs*eq_vals_wat(icell)%hgii + k_eq%mehgrs*eq_vals_wat(icell)%mehg)
        eq_vals_wat(icell)%xoh = total%XOH-eq_vals_wat(icell)%hgii-eq_vals_wat(icell)%mehg 
        eq_vals_wat(icell)%initialized = .true.
    end if
    if (eq_vals_wat(icell)%xoh.le.zero) then
        if (total%XOH.le.zero) then
            order = 3
        else
            eq_vals_wat(icell)%xoh = total%XOH
        endif   
    end if
    cl = max(0.285d0*ec-50.0,0.15d0*ec-12.0d0)
    
    call equil_solver(eq_vals_wat(icell),                           &       !> initial guess for unknowns(in) - results (out)
                            cl,                                     &       !> Cl (mg/L)
                            ph,                                     & 
                            zero,                                   &       !> phytoplankton (mg/L)
                            one,                                    &       !> bed(icell,izone,ilayer)%porosity 
                            total, order, 0,                        &       !> Molar concentration totals (known), number of unknowns,  itype = 0 known total HgII and MeHg (ng/L),itype = 1 known total sediment HgII and MeHg (ng/g)
                            k_eq_solids_wat,                        &       !> equilibrium constants for solids partitioning (compartment specific)
                            iter,                                   &       !> number of iterations to reach solution
                            converge,                               &       !> true if solution converged
                            m,                                      &       !> solution (molar)
                            icell, 0)
    if (iter.gt.9) then
        print *, "equilibrium module  no of iterations (>9):", iter
        print *, "                                  cell no:", icell
    end if
    if (.not.converge) then !todo:debug
        print *, icell, conc_hgii, conc_mehg, solids(1), solids(2), solids(3), doc, ph
        pause
    end if
    if (converge) eq_vals_wat(icell)%initialized = .true.
    call Hg_reactant_concs(m, nosolids, solids, solid_parms_wat, hg_conc_wat(icell, rkstep) )  
    

end subroutine wat_partitioning   
    
end module