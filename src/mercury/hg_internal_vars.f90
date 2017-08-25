module hg_internal_vars
    
use gtm_precision
use common_variables
use hg_type_defs
use sed_type_defs, only: hg_rate_parms_t

implicit none

!mercury parameters
type (solids_inputs_t), dimension (:), allocatable        :: solid_parms_sed   !dimension no solids
type (solids_inputs_t), dimension (:), allocatable        :: solid_parms_wat   !dimension no solids
!inputs for process_gtm_scalar



type (hg_rate_parms_t), allocatable, dimension(:) :: k_wat



!sediment mercury fluxes

        ! dimensions (ncells,nzones,nlayers, RK/huens step)
real (gtm_real), allocatable, dimension (:,:,:,:)     :: sed_methyl           !hgii -> mehg
real (gtm_real), allocatable, dimension (:,:,:,:)     :: sed_demethyl         !mehg -> hgii
real (gtm_real), allocatable, dimension (:,:,:,:)     :: sed_adsorption_s1    !hgii <-> hgii_s1
real (gtm_real), allocatable, dimension (:,:,:,:)     :: sed_adsorption_s2    !hgii <-> hgii_s2
real (gtm_real), allocatable, dimension (:,:,:,:)     :: sed_adsorption_s3    !hgii <-> hgii_s3

real (gtm_real), allocatable, dimension (:,:,:,:)     :: sed_turnover_s1      !hgii_s1 -> hgii
real (gtm_real), allocatable, dimension (:,:,:,:)     :: sed_turnover_s2      !hgii_s2 -> hgii

        ! dimensions (ncells, nzones, mercury form, RK/huens step)
real (gtm_real), allocatable, dimension (:,:,:,:)     :: burial_hg            !layer 1 -> layer 2
real (gtm_real), allocatable, dimension (:,:,:,:)     :: sed_diffusion_hg     !layer 1 -> layer 2

!state variables and net fluxes by compartment
        ! dimensions (ncells,nzones,nlayers, mercury form, RK/huens step)
real (gtm_real), allocatable, dimension (:,:,:,:,:)   :: sed_mercury
real (gtm_real), allocatable, dimension (:,:,:,:,:)   :: sed_mercury_fluxes

!fluxes between sediments and water column
        ! dimensions (ncells, nzones, mercury form, RK/huens step)
real (gtm_real), allocatable, dimension (:,:,:,:)     :: erosion_hg           !sed layer 1 -> wat
real (gtm_real), allocatable, dimension (:,:,:,:)     :: settling_hg          !wat -> sed layer 1
real (gtm_real), allocatable, dimension (:,:,:,:)     :: diffusion_hg         !sed layer 1 <-> wat

!water column mercury fluxes
        ! dimensions (ncells, RK/huens step)
real (gtm_real), allocatable, dimension (:,:)       :: reduction_hg         !hgii->hg0
real (gtm_real), allocatable, dimension (:,:)       :: oxidation            !hgii->hg0
real (gtm_real), allocatable, dimension (:,:)       :: photodegradation     !mehg->hgii

real (gtm_real), allocatable, dimension (:,:)       :: wet_dep_hgii
real (gtm_real), allocatable, dimension (:,:)       :: dry_dep_hgii

real (gtm_real), allocatable, dimension (:,:)       :: wet_dep_mehg
real (gtm_real), allocatable, dimension (:,:)       :: dry_dep_mehg

real (gtm_real), allocatable, dimension (:,:)       :: evasion_hg0

!state variables and net fluxes by compartment for water
        ! dimensions (ncells, mercury form, RK/huens step)
real (gtm_real), allocatable, dimension (:,:,:)     :: wat_mercury
real (gtm_real), allocatable, dimension (:,:,:)     :: wat_mercury_fluxes

type (eq_vals), allocatable, dimension (:,:,:)      :: eq_vals_sed    !(ncells, nzones, nlayers)
type (eq_vals), allocatable, dimension (:)          :: eq_vals_wat    !(ncells)

        ! dimensions (ncells, nzones, nlayers)
real (gtm_real), allocatable, dimension (:,:,:)     :: ic_hgII_sed    !sediment exchangeable HgII (ug/g)
real (gtm_real), allocatable, dimension (:,:,:)     :: ic_mehg_sed    !sediment MeHgII (ug/g)

!fluxes for GTM
        ! dimensions (ncells, mercury form)
real (gtm_real), allocatable, dimension (:,:)       :: gtm_fluxes_hg

    contains

subroutine setup_hg_internals(ncells,nzones,nlayers,nsolids,nmf)
!args
    integer, intent (in)            :: ncells
    integer, intent (in)            :: nzones
    integer, intent (in)            :: nlayers
    integer, intent (in)            :: nsolids
    integer, intent (in)            :: nmf          !no of mercury forms
!local
    integer :: steps = 3            !todo - might only need 2 check later
        
!sediment fluxes
    allocate (sed_methyl(ncells,nzones,nlayers,steps))
    allocate (sed_demethyl(ncells,nzones,nlayers,steps))
    allocate (sed_adsorption_s1(ncells,nzones,nlayers,steps))
    allocate (sed_adsorption_s2(ncells,nzones,nlayers,steps))
    allocate (sed_adsorption_s3(ncells,nzones,nlayers,steps))

    allocate (sed_turnover_s1(ncells,nzones,nlayers,steps))
    allocate (sed_turnover_s2(ncells,nzones,nlayers,steps))

    allocate (burial_hg(ncells,nzones,nmf,steps))
    allocate (sed_diffusion_hg(ncells,nzones,nmf,steps))

!state variables and net fluxes by compartment
    allocate (sed_mercury(ncells,nzones,nlayers,nmf,steps))
    allocate (sed_mercury_fluxes(ncells,nzones,nlayers,nmf,steps))

!fluxes between sediments and water column
    allocate (erosion_hg(ncells,nzones,nmf,steps))
    allocate (settling_hg(ncells,nzones,nmf,steps))
    allocate (diffusion_hg(ncells,nzones,nmf,steps))

!water column mercury fluxes
    allocate (reduction_hg(ncells,steps))
    allocate (oxidation(ncells,steps))
    allocate (photodegradation(ncells,steps))

    allocate (wet_dep_hgii(ncells,steps))
    allocate (dry_dep_hgii(ncells,steps))

    allocate (wet_dep_mehg(ncells,steps))
    allocate (dry_dep_mehg(ncells,steps))

    allocate (evasion_hg0(ncells,steps))

!state variables and net fluxes by compartment for water
    allocate (wat_mercury(ncells,nmf,steps))
    allocate (wat_mercury_fluxes(ncells,nmf,steps))

!fluxes for GTM
    allocate (GTM_fluxes_hg(ncells,nmf))

!equlibrium
    allocate (solid_parms_wat(nsolids))
    solid_parms_wat(1) = solid_parms_wat1
    solid_parms_wat(2) = solid_parms_wat2
    solid_parms_wat(3) = solid_parms_wat3
    allocate (solid_parms_sed(nsolids))
    solid_parms_sed(1) = solid_parms_sed1
    solid_parms_sed(2) = solid_parms_sed2
    solid_parms_sed(3) = solid_parms_sed3
    
    allocate (eq_vals_sed(ncells,nzones,nlayers))
    allocate (eq_vals_wat(ncells))
    allocate (k_wat(ncells))
    
    
end subroutine setup_hg_internals

subroutine deallocate_hg_internals()
    deallocate (sed_methyl)
    deallocate (sed_demethyl)
    deallocate (sed_adsorption_s1)
    deallocate (sed_adsorption_s2)
    deallocate (sed_adsorption_s3)
    deallocate (sed_turnover_s1)
    deallocate (sed_turnover_s2)
    deallocate (burial_hg)
    deallocate (sed_diffusion_hg)
    deallocate (sed_mercury)
    deallocate (sed_mercury_fluxes)
    deallocate (erosion_hg)
    deallocate (settling_hg)
    deallocate (diffusion_hg)
    deallocate (reduction_hg)
    deallocate (oxidation)
    deallocate (photodegradation)
    deallocate (wet_dep_hgii)
    deallocate (dry_dep_hgii)
    deallocate (wet_dep_mehg)
    deallocate (dry_dep_mehg)
    deallocate (evasion_hg0)
    deallocate (wat_mercury)
    deallocate (wat_mercury_fluxes)
    deallocate (GTM_fluxes_hg)
    deallocate (solid_parms_wat)   
    deallocate (solid_parms_sed)
    deallocate (eq_vals_sed)
    deallocate (eq_vals_wat)
    deallocate (k_wat)
end subroutine deallocate_hg_internals
end module