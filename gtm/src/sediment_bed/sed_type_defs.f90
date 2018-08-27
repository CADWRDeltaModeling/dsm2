module sed_type_defs
    
use gtm_precision
use hdf5, only: HID_T, HSIZE_T
use common_variables, only: k_eq_solids_t
implicit none

real(gtm_real), parameter  :: g_cm3_to_g_m3     = 1.d06
real(gtm_real), parameter  :: ft_to_m           = 0.3048d00
real(gtm_real), parameter  :: ft3_to_m3         = 0.028316846592d00
real(gtm_real), parameter  :: kg_to_g           = 1.0d03
real(gtm_real), parameter  :: min_to_sec        = 60.0d0
real(gtm_real), parameter  :: day_to_sec        = 86400.0d0
 
integer, parameter     :: isolids = 3  !< number of solids types defined in GTM ?
integer :: n_zones =3

type bed_properties_t                                   !also used for ic's 
    real (gtm_real)     :: thickness                    !constant except for deepest layer (m)
    real (gtm_real)     :: area_zone                    !< constant (ft^2)  (top surface area of zone - bottom area)  area for for settling 
    real (gtm_real)     :: area_cell                    !< constant (ft^2)  total surface area of cell at top of zone 
    real (gtm_real)     :: area_wet                     !< wetted surface area (horizontal projection) (wet surface area - bottom area)
    real (gtm_real)     :: wp_zone                      !< constant (m2)  (cell wetted primeter top of zone  - wp bottom of zone)
    real (gtm_real)     :: wp_cell                      !< constant (ft)  total WP of cell at top of zone
    real (gtm_real)     :: wp_wet                       !< (ft) wetted perimeter of wetted portion of zone
    real (gtm_real)     :: volume                       !< (m3) zone volume
    real (gtm_real)     :: T = 15.0                     !< temperature (degrees C)
    real (gtm_real)     :: porosity = 0.85d0            !< constant porosity assumed
    real (gtm_real)     :: q10_ct   = 2.0d0             !< constant
    real (gtm_real)     :: tb_ct    = 15.0d0            !< constant base temp for Q10 (degrees C)
    real (gtm_real)     :: r_ct_labile  = LARGEREAL     !< constant (d-1)
    real (gtm_real)     :: r_ct_refract  = 6.0d-6       !< constant (d-1)
    
    real (gtm_real)     :: inter_frac_base
    real (gtm_real)     :: inter_frac                   !calculated interface fraction at t
    real (gtm_real)     :: inter_frac_tb = 0.2d0        !< constant input - fraction of settling labile carbon @ Tb_ct_inter = 20.0d0
    real (gtm_real)     :: inter_frac_max = 0.5d0       !< input - maximum turnover fraction of settling labile carbon
    real (gtm_real)     :: q10_ct_inter = 2.0d0         !< constant for sed-wat interface
    real (gtm_real)     :: tb_ct_inter = 20.0d0         !< constant sed-wat interface base temp for Q10 (degrees C)
   
    real (gtm_real)     :: inter_k                      !< sigmoid constant for turnover calculated
    real (gtm_real)     :: inter_a1                     !< sigmoid constant for turnover calculated
    real (gtm_real)     :: inter_a2                     !< sigmoid constant calculated
    !real (gtm_real), dimension (nosolids)::  vol_frac  !< fraction of total solids volume for each particle type i.e. ((1-porosity)* volume)
    real (gtm_real), dimension (isolids)::  mass_frac   !< mass for each particle type
    integer             :: channel                      !< channel no for debugging
end type


!particle properties
!real (gtm_real), dimension (isolids)   :: diameter  = 0.0005       !< units (m)
real (gtm_real), dimension (isolids)   :: density   = 2.6          !< density (g/m^3) or (g/cm3) and convert



type (bed_properties_t), allocatable, dimension(:,:,:) :: bed     ! dimensions(cell_no,zone, layer)

!bed fluxes for GTM ???
!real (gtm_real), allocatable, dimension (:,:)  :: deposition
real (gtm_real), allocatable, dimension (:,:)  :: resuspension

type gtm_sed_hdf_t 
        character*128 file_name
        !character*128 file_name_hg
        real(gtm_real) :: write_interval
        real(gtm_real) :: start_julmin
        real(gtm_real) :: time_index  
        integer(HID_T) :: file_id  !_sed   
        !integer(HID_T) :: file_id_Hg
        integer(HID_T) :: data_id !_sed
        integer(HID_T) :: data_id_flux !sed_flux
        
        integer(HID_T) :: bed_out_id
        integer(HID_T) :: bed_out_flux_id
        integer(HID_T) :: wat_hg_id
        integer(HID_T) :: wat_hg_flux_id
       ! integer(HSIZE_T) :: conc_dim
       ! integer(HSIZE_T) :: cell_dim
       ! integer(HSIZE_T) :: chan_dim
       ! integer(HSIZE_T) :: resv_dim
       ! integer(HSIZE_T) :: time_dim
end type

type bed_mercury_inputs_t
        real (gtm_real), dimension (isolids):: mole_xoh         !<moles of XOH site per g of particle
        real (gtm_real), dimension (isolids):: frac_exchg       !<fraction of X0H sites that are exchangeable
end type

type hg_rate_parms_t
    real (gtm_real) :: methyl               !> methylation rate constant
    real (gtm_real) :: biodemethyl          !> methylation rate constant 
    real (gtm_real) :: methyl_int           !> methylation rate constant
    real (gtm_real) :: biodemethyl_int      !> methylation rate constant  
end type hg_rate_parms_t

end module