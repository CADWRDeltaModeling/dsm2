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

module mercury_fluxes

    use gtm_precision
    use equilibrium
    use mercury_state_variables
    use hg_internal_vars
    use mercury_flux_bed
    use mercury_flux_wat
    use sed_internal_vars, only: settling, erosion_sb
    contains
   
    !> Main interface between Mercury Module and DSM2-GTM
    !> Implementation of compute_source_if for 
    !> mercury source
    subroutine mercury_source(source_mercury, & !< mercury source/sink term to interact with DSM2-GTM
                              conc_mercury,   & !< GTM results from previous step, 1:HgII, 2, MeHg, 3: Hg0, 4:HgII_s1, 5:HgII_s2, 6:HgII_s3
                              conc_sed,       & !< suspended sediment (mg/L)
                              !**conc_doc,       & !< suspended sediment (mg/L)
                              conc_ec,        & !< 
                              area,           & !< hydrodynamic data from Hydro
                              width,          & !< hydrodynamic data from Hydro
                              depth,          & !< hydrodynamic data from Hydro 
                              dx,             & !< dx
                              dt,             & !< dt
                              ncell,          & !< number of cells
                              nsediment,      & !< number of sediments
                              nmercury,       & !< number of mercury related constituents
                              deposition,     &
                              erosion,        &  
                              rkstep)
        implicit none
        integer, intent(in) :: ncell                                  !< Number of cell
        integer, intent(in) :: nsediment                              !< Number of sediment
        integer, intent(in) :: nmercury                               !< Number of mercury
        real(gtm_real), intent(out) :: source_mercury(ncell,nmercury) !< cell source
        real(gtm_real), intent(inout) :: conc_mercury(ncell,nmercury)    !< cell conc
        real(gtm_real), intent(in) :: conc_sed(ncell,nsediment)
        !**real(gtm_real), intent(in) :: conc_doc(ncell)
        real(gtm_real), intent(in) :: conc_ec(ncell)                 
        real(gtm_real), intent(in) :: area(ncell)                     !< cell area (ft2) 
        real(gtm_real), intent(in) :: width(ncell)                    !< cell area (ft)
        real(gtm_real), intent(in) :: depth(ncell)                    !< cell depth (ft) 
        real(gtm_real), intent(in) :: dx(ncell)                       !< dx
        real(gtm_real), intent(in) :: dt                              !< dt
        real(gtm_real), intent(in) :: deposition(ncell,nsediment)
        real(gtm_real), intent(in) :: erosion(ncell,nsediment)
        integer , intent(in)       :: rkstep                          ! added dhh 20170804
        !--local
        real(gtm_real) :: conc_doc(ncell)
               
        integer :: icell
        integer :: isolid
        real(gtm_real) :: area_wet(ncell)
        real(gtm_real) :: volume(ncell)
        real(gtm_real) :: solids(nsediment)
        type(hg_rate_parms_t) :: k                                    !> hg reaction rate parmeters
        type(solids_inputs_t) :: solid_in(nsediment)                  !> solids adsorption desorption inputs
        !type(hg_concs) :: concs_hg                                    !> hg concs for reactions (ng/L) -> (ug/m3)
        !type(hg_flux_def) :: r                                        !> reaction and fluxes at air/water boundary
        real(gtm_real), parameter :: L = 0.3048d0
        real(gtm_real)            :: day_to_sec
        real(gtm_real) :: kd(ncell)
        real(gtm_real) :: debug_conc_erode(ncell)
        real(gtm_real) :: debug_conc_settle(ncell)
        real(gtm_real) :: debug_erode(ncell)
        real(gtm_real) :: debug_settle(ncell)
        where (conc_mercury.lt.1.0d-5) conc_mercury = 1.0d-4
        source_mercury = zero 
        area_wet = width*dx*L*L                         !m2 surface area
        !volume(:) = area(:)*dx(:)*L*L*L                !m3
        volume(:) = dx(:)*width(:)*depth(:)*L*L*L       !m3 
        day_to_sec = 8.64d04                            !convert rates from days to seconds
        call set_fluxes_to_zero(f_wat_hg,ncell)
        conc_doc(:) = conc(:,doc_ivar)
        
        do icell = 1, ncell
            solids(:) = conc_sed(icell,:)   
            where (solids.lt.1.0d-5) solids = 1.0d-4
            if (conc_doc(icell).NE.conc(icell,doc_ivar)) then
                print *, "weird DOC ", icell, rkstep, conc_doc(icell)-conc(icell,doc_ivar)
                if (icell.GT.726) then
                    pause
                endif
            endif
            
            call wat_partitioning(icell, conc_mercury(icell,1), conc_mercury(icell,2), solids, conc_doc(icell), conc_ph(icell), conc_ec(icell), nsediment, rkstep)
            hg_conc_wat(icell, rkstep)%Hg0 = conc_mercury(icell,6)
            do isolid = 1,nsediment
                if(conc_sed(icell,isolid).gt.zero) then
                    hg_conc_wat(icell, rkstep)%HgII_inert(isolid) = conc_mercury(icell, 4+isolid-1)/conc_sed(icell,isolid)  !(ug/g)
                else
                    hg_conc_wat(icell, rkstep)%HgII_inert(isolid) = zero
                end if
            end do           
          
            call hg_flux_wat(area_wet(icell),    &
                         volume(icell),          &
                         depth(icell),           &
                         val_ipar(icell),        &
                         val_iuva(icell),        &
                         val_iuvb(icell),        &
                         conc_doc(icell),        &
                         conc_ph(icell),         &
                         nsediment,              &
                         conc_sed(icell,:),      &
                         hg_conc_wat(icell, rkstep)%HgII_inert, &
                         conc_so4(icell),        &
                         conc_temp(icell),       &
                         val_hg0_air(icell),     &
                         val_mehg_air(icell),    &
                         val_precip(icell),      &
                         val_wet_hgii(icell),    &
                         val_dry_hgii(icell),    &
                         val_rgm_air(icell),     &
                         val_wet_mehg(icell),    &
                         val_dry_mehg(icell),    &
                         val_dgm_ratio(icell),   &
                         r_ct_interface(icell),  &
                         k,                      & ! not given yet
                         solid_in,               &
                         hg_conc_wat(icell, rkstep),          & 
                         f_wat_hg(icell) )         ! not given yet
            call sum_fluxes(f_wat_hg(icell))  
        end do

        call hg_flux_sed(ncell, dt, f_wat_hg, conc_doc, conc_ph, conc_ec, rkstep) 
        
                                                        !Todo: check units
        f_wat_hg(:)%hgii = f_wat_hg(:)%hgii/day_to_sec  + f_wat_hg(:)%erosion(mf_hgii) - f_wat_hg(:)%settle(mf_hgii)
        f_wat_hg(:)%mehg = f_wat_hg(:)%mehg/day_to_sec  + f_wat_hg(:)%erosion(mf_mehg) - f_wat_hg(:)%settle(mf_mehg)
        f_wat_hg(:)%hg0 = f_wat_hg(:)%hg0/day_to_sec
        f_wat_hg(:)%hg_inert(1) = f_wat_hg(:)%hg_inert(1)/day_to_sec !- f_wat_hg(:)%settle(mf_hgii_s1) + f_wat_hg(:)%erosion(mf_hgii_s1)
        f_wat_hg(:)%hg_inert(2) = f_wat_hg(:)%hg_inert(2)/day_to_sec !- f_wat_hg(:)%settle(mf_hgii_s2) + f_wat_hg(:)%erosion(mf_hgii_s2)
        f_wat_hg(:)%hg_inert(3) = f_wat_hg(:)%hg_inert(3)/day_to_sec !- f_wat_hg(:)%settle(mf_hgii_s3) + f_wat_hg(:)%erosion(mf_hgii_s3)
                        
        source_mercury(:,1) = f_wat_hg(:)%hgii/(volume(:))   
        source_mercury(:,2) = f_wat_hg(:)%mehg/(volume(:)) 
        source_mercury(:,3) = f_wat_hg(:)%hg0/(volume(:))  
        source_mercury(:,4) = f_wat_hg(:)%hg_inert(1)/(volume(:))
        source_mercury(:,5) = f_wat_hg(:)%hg_inert(2)/(volume(:))
        source_mercury(:,6) = f_wat_hg(:)%hg_inert(3)/(volume(:))
        
        debug_erode(:) = erosion(:,1)+erosion(:,2)+erosion(:,3)
        debug_settle(:) = deposition(:,1)*hg_conc_wat(:,rkstep)%hgii_ssX(1) + deposition(:,2)*hg_conc_wat(:,rkstep)%hgii_ssX(2) + deposition(:,3)*hg_conc_wat(:,rkstep)%hgii_ssX(3)
        
        debug_conc_erode(:) = f_wat_hg(:)%erosion(mf_hgii)/(debug_erode(:)*volume(:))
        debug_conc_settle(:) = f_wat_hg(:)%settle(mf_hgii)/(debug_settle(:)*volume(:))
       
        !source_mercury(:,1) = zero
        !source_mercury(:,2) = zero
        !source_mercury(:,3) = zero
        !source_mercury(:,4) = zero
        !source_mercury(:,5) = zero
        !source_mercury(:,6) = zero
      
    return
    end subroutine



    
end module mercury_fluxes