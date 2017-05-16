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
!>@ingroup gtm_driver
!> Interface to implement source term
module gtm_source
    
    contains
 
     !> Given source term by cell 
     subroutine gtm_source_implement(source,       & 
                                     conc,         &
                                     flow,         &
                                     area,         &
                                     width,        &
                                     depth,        &
                                     hyd_radius,   &
                                     dx,           &
                                     dt,           &
                                     time,         &
                                     ncell,        &
                                     nvar,         &
                                     constraint,   &
                                     name,         &
                                     rkstep)
        use gtm_precision
        use common_variables, only: n_sediment, use_sediment_bed, n_layers, &
                                    run_mercury, n_mercury, mercury_ivar
        use suspended_sediment
        use sediment_bed
        use turbidity
        use mercury_fluxes
        implicit none
        !--- args  
        integer, intent(in) :: ncell                            !< Number of cells
        integer, intent(in) :: nvar                             !< Number of variables   
        integer, intent(in) :: rkstep                           !< reaction step in Huen's method
        real(gtm_real), intent(out) :: source(ncell,nvar)       !< cell centered source 
        real(gtm_real), intent(in)  :: conc(ncell,nvar)         !< Concentration 
        real(gtm_real), intent(in)  :: flow(ncell)              !< flow at source location
        real(gtm_real), intent(in)  :: area(ncell)              !< Cell centered area at source     
        real(gtm_real), intent(in)  :: width(ncell)             !< Cell centered width at source
        real(gtm_real), intent(in)  :: depth(ncell)             !< depth at source location
        real(gtm_real), intent(in)  :: hyd_radius(ncell)        !< hydraulic radius at source location       
        real(gtm_real), intent(in)  :: dx(ncell)                !< dx
        real(gtm_real), intent(in)  :: dt                       !< dt
        real(gtm_real), intent(in)  :: time                     !< time
        real(gtm_real), intent(inout) :: constraint(ncell,nvar) !< Constraint 
        character(len=32), intent(in) :: name(nvar)             !< Constituent name
        !--- local
        integer :: ivar, isediment
        real(gtm_real) :: erosion(ncell,n_sediment)   
        real(gtm_real) :: deposition(ncell,n_sediment)
        real(gtm_real) :: conc_mercury(ncell,n_mercury)
        real(gtm_real) :: source_mercury(ncell,n_mercury)
                
        ! source must be in primitive variable 
        do ivar = 1, nvar
            source(:,ivar) = zero
            if (trim(name(ivar)).eq."sediment") then
                isediment = ivar - (nvar - n_sediment)
                call suspended_sediment_source(source(:,ivar),          & 
                                               erosion(:,isediment),    &
                                               deposition(:,isediment), &
                                               conc(:,ivar),            & 
                                               flow,                    & 
                                               area,                    & 
                                               depth,                   &
                                               hyd_radius,              & 
                                               dx,                      & 
                                               dt,                      & 
                                               ncell,                   & 
                                               constraint(:,ivar),      &
                                               isediment)      
                ! Sediment Bed Module
                if (use_sediment_bed) then
                    call sediment_bed_main(erosion(:,isediment),        &
                                           deposition(:,isediment),     &
                                           dx*width,                    &   
                                           dt,                          & 
                                           rkstep,                      &
                                           n_layers,                    &
                                           ncell,                       &
                                           n_sediment)           
                    constraint(:,ivar) = erosion(:,isediment)
                end if 
            elseif (trim(name(ivar)).eq."turbidity") then
                call turbidity_source(source(:,ivar),       & 
                                      conc(:,ivar),         & 
                                      dt,                   & 
                                      ncell,                & 
                                      conc(:,ivar))
            end if
        end do
        if (run_mercury) then
                conc_mercury(:,1:n_mercury) = conc(:,mercury_ivar(1:n_mercury))
                source_mercury(:,1:n_mercury) = source(:,mercury_ivar(1:n_mercury))
                call mercury_source(source_mercury, & !< mercury source/sink term to interact with DSM2-GTM
                                    conc_mercury,   & !< GTM results from previous step, 1:HgII, 2, MeHg, 3: Hg0, 4:HgII_s1, 5:HgII_s2, 6:HgII_s3
                                    area,           & !< hydrodynamic data from Hydro
                                    width,          & !< hydrodynamic data from Hydro
                                    depth,          & !< hydrodynamic data from Hydro 
                                    dx,             & !< dx
                                    dt,             & !< dt
                                    ncell,          & !< number of cells
                                    n_sediment,     & !< number of sediments
                                    n_mercury)         !< number of mercury related constituents              
        end if
    return
    end subroutine 
 
end module