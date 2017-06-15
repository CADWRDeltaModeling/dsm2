module sed_internal_vars
    
use gtm_precision

implicit none
!these variables contain sediment bed mass fluxes for the two Huens method steps

logical :: use_sed_hdf
character(len=130) :: file_name_qrf
character(len=130) :: file_name_hdf_sed
character(len=130) :: file_name_hdf_sed_flux
character(len=130) :: file_name_hdf_bed
        ! dimensions (ncells,nzones,nlayers, nosolids, RK/Huens step)
real (gtm_real), allocatable, dimension (:,:,:,:,:)   :: decomposition
real (gtm_real), allocatable, dimension (:,:,:,:,:)   :: carbonturnover
real (gtm_real), allocatable, dimension (:,:,:,:,:)   :: burial
real (gtm_real), allocatable, dimension (:,:,:,:,:)   :: sedsolids
real (gtm_real), allocatable, dimension (:,:,:,:,:)   :: sedsolidsflux
        ! dimensions (ncells,nozones,nosolids, RK/Huens step)
real (gtm_real), allocatable, dimension (:,:,:,:)     :: settling
real (gtm_real), allocatable, dimension (:,:,:,:)     :: erosion
    ! dimensions (ncells, nzones,RK/Huens step)
real (gtm_real), allocatable, dimension (:,:,:)   :: decomposition_inter   !for Hg module drives interface methylation
real (gtm_real), allocatable, dimension (:,:,:)   :: carbonturnover_inter  !for Hg module

        ! dimensions (ncells,nzones)
real (gtm_real), allocatable, dimension (:,:)         :: burial_total  !for outputs layer1 to layer2

         ! dimensions (ncells)  
real (gtm_real), allocatable, dimension (:)           :: length         !cell length

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
    allocate (erosion(ncells,nzones,nosolids,2))
    allocate (sedsolids(ncells,nzones,layers,nosolids,3))
    allocate (sedsolidsflux(ncells,nzones,layers,nosolids,2))
    allocate (burial_total(ncells,nzones))
    !allocate (length(ncells))
    
    allocate (decomposition_inter(ncells,nzones,2))   
    allocate (carbonturnover_inter(ncells,nzones,2))
    
end subroutine setup_sed_internals

subroutine deallocate_sed_internals()

    deallocate (decomposition)
    deallocate (carbonturnover)
    deallocate (burial)
    deallocate (settling)
    deallocate (erosion)
    
    deallocate (sedsolids)
    deallocate (sedsolidsflux)
    deallocate (burial_total)
    deallocate (length)
    deallocate (decomposition_inter)   
    deallocate (carbonturnover_inter)
end subroutine deallocate_sed_internals

end module