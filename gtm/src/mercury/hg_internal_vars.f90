module hg_internal_vars
    
use gtm_precision
use common_variables
use hg_type_defs
use sed_type_defs, only: hg_rate_parms_t
use sed_internal_vars

implicit none

!mercury parameters
type (solids_inputs_t), dimension (:), allocatable      :: solid_parms_wat      !dimension no solids


! dimensions (ncells,nzones,nlayers, RK/huens step)
real (gtm_real), allocatable, dimension (:,:,:,:)       :: sed_MeHg
real (gtm_real), allocatable, dimension (:,:,:,:)       :: sed_HgII
real (gtm_real), allocatable, dimension (:,:,:,:)       :: sed_s1_HgII
real (gtm_real), allocatable, dimension (:,:,:,:)       :: sed_s2_HgII
real (gtm_real), allocatable, dimension (:,:,:,:)       :: sed_s3_HgII
real (gtm_real), allocatable, dimension (:,:,:,:)       :: sed_Hg0

type (hg_rate_parms_t), allocatable, dimension(:) :: k_wat                      !dimension nocells

!hg concentrations
type (hg_concs), allocatable, dimension(:,:,:,:) :: hg_conc_sed                !dimensions include rk step
type (hg_concs), allocatable, dimension(:,:)     :: hg_conc_wat

!sediment mercury fluxes

        ! dimensions (ncells,nzones,nlayers, RK/huens step)
real (gtm_real), allocatable, dimension (:,:,:,:)     :: f_sed_methyl           !hgii -> mehg
real (gtm_real), allocatable, dimension (:,:,:,:)     :: f_sed_demethyl         !mehg -> hgii
real (gtm_real), allocatable, dimension (:,:,:,:)     :: f_sed_adsorption_s1    !hgii <-> hgii_s1
real (gtm_real), allocatable, dimension (:,:,:,:)     :: f_sed_adsorption_s2    !hgii <-> hgii_s2
real (gtm_real), allocatable, dimension (:,:,:,:)     :: f_sed_adsorption_s3    !hgii <-> hgii_s3

real (gtm_real), allocatable, dimension (:,:,:,:)     :: f_sed_turnover_s1      !hgii_s1 -> hgii
real (gtm_real), allocatable, dimension (:,:,:,:)     :: f_sed_turnover_s2      !hgii_s2 -> hgii

        ! dimensions (ncells, nzones, mercury form, RK/huens step)
real (gtm_real), allocatable, dimension (:,:,:,:)     :: f_burial_hg            !layer 1 -> layer 2
real (gtm_real), allocatable, dimension (:,:,:,:)     :: f_sed_diffusion_hg     !layer 1 -> layer 2

!state variables and net fluxes by compartment
        ! dimensions (ncells,nzones,nlayers, mercury form, RK/huens step)
real (gtm_real), allocatable, dimension (:,:,:,:,:)   :: sed_mercury
real (gtm_real), allocatable, dimension (:,:,:,:,:)   :: f_sed_hg

!fluxes between sediments and water column
        ! dimensions (ncells, nzones, mercury form, RK/huens step)
real (gtm_real), allocatable, dimension (:,:,:,:)     :: f_erosion_hg           !sed layer 1 -> wat
real (gtm_real), allocatable, dimension (:,:,:,:)     :: f_settling_hg          !wat -> sed layer 1
real (gtm_real), allocatable, dimension (:,:,:,:)     :: f_diffusion_hg         !sed layer 1 <-> wat

!water column mercury fluxes
        ! dimensions (ncells)

type (hg_flux_def), allocatable, dimension(:)     :: f_wat_hg !todo:

        ! dimensions (ncells, mercury form, RK/huens step)
!real (gtm_real), allocatable, dimension (:,:,:)     :: wat_mercury todo:delete
!real (gtm_real), allocatable, dimension (:,:,:)     :: wat_mercury_fluxes todo:delete

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
    allocate (f_sed_methyl(ncells,nzones,nlayers,steps))
    allocate (f_sed_demethyl(ncells,nzones,nlayers,steps))
    allocate (f_sed_adsorption_s1(ncells,nzones,nlayers,steps))
    allocate (f_sed_adsorption_s2(ncells,nzones,nlayers,steps))
    allocate (f_sed_adsorption_s3(ncells,nzones,nlayers,steps))

    allocate (f_sed_turnover_s1(ncells,nzones,nlayers,steps))
    allocate (f_sed_turnover_s2(ncells,nzones,nlayers,steps))

    allocate (f_burial_hg(ncells,nzones,nmf,steps))
    allocate (f_sed_diffusion_hg(ncells,nzones,nmf,steps))

!state variables and net fluxes by compartment
    allocate (sed_mercury(ncells,nzones,nlayers,nmf,steps))
    allocate (f_sed_hg(ncells,nzones,nlayers,nmf,steps))

!fluxes between sediments and water column
    allocate (f_erosion_hg(ncells,nzones,nmf,steps))
    allocate (f_settling_hg(ncells,nzones,nmf,steps))
    allocate (f_diffusion_hg(ncells,nzones,nmf,steps))

!state variables and net fluxes by compartment for water
    !allocate (wat_mercury(ncells,nmf,steps)) todo:delete
    !allocate (wat_mercury_fluxes(ncells,nmf,steps)) todo:delete

!fluxes for GTM
    allocate (GTM_fluxes_hg(ncells,nmf))

!equlibrium
    allocate (solid_parms_wat(nsolids))
    solid_parms_wat(1) = solid_parms_wat1
    solid_parms_wat(2) = solid_parms_wat2
    solid_parms_wat(3) = solid_parms_wat3
    allocate (solid_parms_sed(ncells,nzones,nlayers,nsolids))
    allocate (k_eq_solids_sed(ncells,nzones,nlayers))
    !solid_parms_sed(1) = solid_parms_sed1
    !solid_parms_sed(2) = solid_parms_sed2
    !solid_parms_sed(3) = solid_parms_sed3
    allocate (mole_rs_sed(ncells,nzones,nlayers))
    allocate (eq_vals_sed(ncells,nzones,nlayers))
    allocate (eq_vals_wat(ncells))
    allocate (k_wat(ncells))
    
!sed concentration
    allocate (sed_MeHg(ncells,nzones,nlayers,steps))
    allocate (sed_HgII(ncells,nzones,nlayers,steps))
    allocate (sed_s1_HgII(ncells,nzones,nlayers,steps))
    allocate (sed_s2_HgII(ncells,nzones,nlayers,steps))
    allocate (sed_s3_HgII(ncells,nzones,nlayers,steps))
    allocate (sed_Hg0(ncells,nzones,nlayers,steps))

    allocate (sed_MeHg_ic(ncells,nzones,nlayers))
    allocate (sed_HgII_ic(ncells,nzones,nlayers))
    allocate (sed_s1_HgII_ic(ncells,nzones,nlayers))
    allocate (sed_s2_HgII_ic(ncells,nzones,nlayers))
    allocate (sed_s3_HgII_ic(ncells,nzones,nlayers))
    allocate (sed_Hg0_ic(ncells,nzones,nlayers))
    !for outputs
       
    allocate (hg_conc_sed(ncells,nzones,nlayers,steps)) 
    allocate (hg_conc_wat(ncells,steps))
    
    allocate (f_wat_hg(ncells))
    
end subroutine setup_hg_internals

subroutine deallocate_hg_internals()
    deallocate (f_sed_methyl)
    deallocate (f_sed_demethyl)
    deallocate (f_sed_adsorption_s1)
    deallocate (f_sed_adsorption_s2)
    deallocate (f_sed_adsorption_s3)
    deallocate (f_sed_turnover_s1)
    deallocate (f_sed_turnover_s2)
    deallocate (f_burial_hg)
    deallocate (f_sed_diffusion_hg)
    deallocate (sed_mercury)
    deallocate (f_sed_hg)
    deallocate (f_erosion_hg)
    deallocate (f_settling_hg)
    deallocate (f_diffusion_hg)
    !deallocate (wat_mercury) todo:delete
    !deallocate (wat_mercury_fluxes)todo:delete
    deallocate (GTM_fluxes_hg)
    deallocate (solid_parms_wat)   
    deallocate (solid_parms_sed)
    deallocate (k_eq_solids_sed)
    deallocate (mole_rs_sed)
    deallocate (eq_vals_sed)
    deallocate (eq_vals_wat)
    deallocate (k_wat)
    
    deallocate (sed_MeHg)
    deallocate (sed_HgII)
    deallocate (sed_s1_HgII)
    deallocate (sed_s2_HgII)
    deallocate (sed_s3_HgII)
    deallocate (sed_Hg0)

    deallocate (sed_MeHg_ic)
    deallocate (sed_HgII_ic)
    deallocate (sed_s1_HgII_ic)
    deallocate (sed_s2_HgII_ic)
    deallocate (sed_s3_HgII_ic)
    deallocate (sed_Hg0_ic)
        
    deallocate (hg_conc_sed) 
    deallocate (hg_conc_wat)
    
    deallocate (f_wat_hg)
end subroutine deallocate_hg_internals
end module