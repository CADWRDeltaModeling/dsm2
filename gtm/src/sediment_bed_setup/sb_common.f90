module sb_common
    
    use gtm_precision
    use hdf_util , only: n_comp
    use common_variables, only: n_cell,chan_geom, top_wet_p, top_elev
    !use common_xsect , only: top_wet_p, top_elev
    
    implicit none
    
    type elevation_t
        real(gtm_real) :: max
        real(gtm_real) :: min
        real(gtm_real) :: top
    end type
    
    type (elevation_t), allocatable, dimension(:) :: elevation 
    
    type sedcell_t
        integer :: n_zone
        integer :: cell_no
        real(gtm_real) :: up            !distance from channel beginning
        real(gtm_real) :: down          !distance to channel end
        real(gtm_real) :: min_elev
        real(gtm_real) :: max_elev
        real(gtm_real) :: length        !cell length
        integer        :: segm_no 
        integer        :: chan_no 
        integer        :: up_comp_no 
        integer        :: down_comp_no
        real(gtm_real), allocatable, dimension(:) ::  elev
        real(gtm_real), allocatable, dimension(:) ::  wet_p
        real(gtm_real), allocatable, dimension(:) ::  width
    end type
     
    type (sedcell_t), allocatable, dimension(:) :: sedcell
    
    integer :: n_zone
    
     type sed_hdf_t
        integer :: cell
        integer :: zone
        real(gtm_real) :: elev
        real(gtm_real) :: cell_wet_p 
        real(gtm_real) :: zone_wet_p 
        real(gtm_real) :: width 
     end type
     
    type (sed_hdf_t), allocatable, dimension(:) :: sed_hdf
    real(gtm_real) :: max_elev
    real(gtm_real) :: min_elev
    real(gtm_real) :: max_elev_dx
        
    contains
    
    subroutine allocate_all
        integer :: ii
        allocate(elevation(n_comp))
        allocate(sedcell(n_cell))
        do ii = 1, n_cell
            allocate(sedcell(ii)%elev(n_zone))
            allocate(sedcell(ii)%wet_p(n_zone))
            allocate(sedcell(ii)%width(n_zone))
        end do
        allocate (sed_hdf(n_cell*n_zone))
       
        sedcell(:)%n_zone = n_zone
    end subroutine
    
    subroutine deallocate_all
        integer :: ii
        deallocate(elevation)
        do ii = 1, n_cell
            deallocate(sedcell(ii)%elev)
            deallocate(sedcell(ii)%wet_p)
            deallocate(sedcell(ii)%width)
        end do
        deallocate (sed_hdf)
        deallocate(sedcell)
    end subroutine
    
    
end module