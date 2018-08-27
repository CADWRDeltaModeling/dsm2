module sed_internal_vars
    
use gtm_precision
use sed_type_defs
use common_variables, only: k_eq_solids_t, solids_inputs_t

implicit none
!these variables contain sediment bed mass fluxes for the two Huens method steps

logical :: use_sed_hdf
character(len=130) :: file_name_qrf
character(len=130) :: file_name_hdf_sed
character(len=130) :: file_name_hdf_bed         !sed inputs for cell,layers zones

character(len=130) :: file_name_qrf_sedhg
character(len=130) :: file_name_hdf_sedHg

real(gtm_real), allocatable, dimension(:,:)          :: conc_tss
real(gtm_real), allocatable, dimension(:,:)          :: conc_wat_hdf
real(gtm_real), allocatable, dimension(:,:)          :: conc_wat_flux_hdf
        ! dimensions (ncells,nzones,nlayers, nosolids, RK/Huens step)
real (gtm_real), allocatable, dimension (:,:,:,:,:)   :: decomposition
real (gtm_real), allocatable, dimension (:,:,:,:,:)   :: carbonturnover
real (gtm_real), allocatable, dimension (:,:,:,:,:)   :: burial
real (gtm_real), allocatable, dimension (:,:,:,:,:)   :: sedsolids
real (gtm_real), allocatable, dimension (:,:,:,:,:)   :: sedsolidsflux
        ! dimensions (ncells,nozones,nosolids, RK/Huens step)
real (gtm_real), allocatable, dimension (:,:,:,:)     :: settling
real (gtm_real), allocatable, dimension (:,:,:,:)     :: erosion_sb
        ! dimensions (ncells, nzones,RK/Huens step)
real (gtm_real), allocatable, dimension (:,:,:)   :: decomposition_inter    !
real (gtm_real), allocatable, dimension (:,:,:)   :: carbonturnover_inter   !for Hg module
        ! dimensions (ncells,RK/Huens step) for water column
real (gtm_real), allocatable, dimension (:,:)   :: wat_decomposition_inter    !
real (gtm_real), allocatable, dimension (:,:)   :: wat_carbonturnover_inter   !for Hg module
        ! dimensions (ncells)
real (gtm_real), allocatable, dimension (:)       :: r_ct_interface         !for Hg module drives interface methylation
        ! dimensions (ncells,nzones)
real (gtm_real), allocatable, dimension (:,:)         :: burial_total  !for outputs layer1 to layer2

         ! dimensions (ncells)  
real (gtm_real), allocatable, dimension (:)           :: length         !cell length
         ! dimensions (ncells, nzones,nlayers)
type (k_eq_solids_t), allocatable, dimension (:,:,:)   :: k_eq_solids_sed

type (solids_inputs_t), dimension (:,:,:,:), allocatable      :: solid_parms_sed      !dimension no solids
real (gtm_real), allocatable, dimension (:,:,:)     :: mole_rs_sed

type (hg_rate_parms_t), allocatable, dimension(:,:,:) :: k_sed

! dimensions (ncells,nzones,nlayers)
real (gtm_real), allocatable, dimension (:,:,:)       :: sed_MeHg_ic      !ug/g solid
real (gtm_real), allocatable, dimension (:,:,:)       :: sed_HgII_ic      !ug/g solid
real (gtm_real), allocatable, dimension (:,:,:)       :: sed_s1_HgII_ic   !ug/g solid
real (gtm_real), allocatable, dimension (:,:,:)       :: sed_s2_HgII_ic   !ug/g solid
real (gtm_real), allocatable, dimension (:,:,:)       :: sed_s3_HgII_ic   !ug/g solid
real (gtm_real), allocatable, dimension (:,:,:)       :: sed_Hg0_ic       !ng/l porewater
    contains

subroutine setup_sed_internals(ncells,nzones,layers,nosolids)
!args
    integer, intent (in)            :: ncells
    integer, intent (in)            :: nzones
    integer, intent (in)            :: layers
    integer, intent (in)            :: nosolids
    
    
    allocate (decomposition(ncells,nzones,layers,nosolids,2))
    allocate (carbonturnover(ncells,nzones,layers,nosolids,2))
    allocate (burial(ncells,nzones,layers,nosolids,2))
    allocate (settling(ncells,nzones,nosolids,2))
    allocate (erosion_sb(ncells,nzones,nosolids,2))
    allocate (sedsolids(ncells,nzones,layers,nosolids,3))
    allocate (sedsolidsflux(ncells,nzones,layers,nosolids,2))
    allocate (burial_total(ncells,nzones))
    !allocate (length(ncells))
    
    allocate (decomposition_inter(ncells,nzones,2))   
    allocate (carbonturnover_inter(ncells,nzones,2))
    
    allocate (wat_decomposition_inter(ncells,2))
    allocate (wat_carbonturnover_inter(ncells,2)) 
    
    allocate (r_ct_interface(ncells))
    allocate (k_sed(ncells, nzones, layers))
    k_sed(:,:,:)%methyl_int = zero
    k_sed(:,:,:)%biodemethyl_int = zero
    
    allocate (conc_tss(ncells,3))
    allocate (conc_wat_hdf(ncells,9))
    allocate (conc_wat_flux_hdf(ncells,10))
end subroutine setup_sed_internals

subroutine deallocate_sed_internals()

    deallocate (decomposition)
    deallocate (carbonturnover)
    deallocate (burial)
    deallocate (settling)
    deallocate (erosion_sb)
    
    deallocate (sedsolids)
    deallocate (sedsolidsflux)
    deallocate (burial_total)
    deallocate (length)
    deallocate (decomposition_inter)   
    deallocate (carbonturnover_inter)
    
    deallocate (wat_decomposition_inter)
    deallocate (wat_carbonturnover_inter) 
    
    deallocate (r_ct_interface)
    deallocate (k_sed)
    
    deallocate (conc_tss)
    deallocate (conc_wat_hdf)
    deallocate (conc_wat_flux_hdf)
    
end subroutine deallocate_sed_internals




end module