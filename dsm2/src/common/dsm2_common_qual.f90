module common_vars_qual
    use constants
    use array_limits
    use type_defs, only : max_constituent
    use common_vars
    implicit none

    type constituent_t
        sequence
        character*16 :: name = ' '            ! constituent name
        integer :: object = miss_val_i        ! object type of injection
        integer :: object_no = miss_val_i     ! object number of injection
        integer :: group_ndx = miss_val_i     ! index to group
        logical :: conservative = .true.      ! true if conservative, false if nonconservative
    end type

    logical :: mass_tracking
    logical :: dispersion
    integer :: nres_conc                     ! Number of reservoirs for which initial conc. specified
    real*8:: init_conc = LARGEREAL          ! initial concentration

    integer :: no_of_nonconserve_constituent       ! number of constituents tracked from all sources
    integer :: no_all_source                 ! number of constituents tracked from all sources

    integer :: nonconserve_ptr(max_constituent) ! pointer to correctly transfer rate coefficients
    integer :: all_source_ptr(max_constituent) ! pointer to correctly transfer rate coefficients
    integer :: constituent_ptr(max_constituent) ! pointer to correctly transfer rate coefficients

    character*20, dimension(max_constituent) :: nonconserve_list = ' '

    ! integer :: num_res
    ! real*8:: rcoef_chan(max_constituent,ncoef_type,max_channels)
    ! real*8:: rcoef_res_temp(max_constituent,ncoef_type,max_reservoirs)
    ! real*8:: rcoef(max_constituent,ncoef_type)
    ! real*8:: rcoef_res(max_constituent,ncoef_type,max_reservoirs)

    real*8 :: algaefract_n
    real*8 :: algaefract_p
    real*8 :: oxy_photo
    real*8 :: oxy_resp
    real*8 :: oxy_nh3
    real*8 :: oxy_no2
    real*8 :: alg_chl_ratio
    real*8 :: pref_factor
    real*8 :: klight_half
    real*8 :: knit_half
    real*8 :: kpho_half
    real*8 :: lambda0
    real*8 :: lambda1
    real*8 :: lambda2
    real*8 :: alg_bod

    integer, parameter :: temp_coeff_type = 16

    real*8 :: elev
    real*8 :: lat
    real*8 :: longitude
    real*8 :: long_std_merid
    real*8 :: dust_attcoeff
    real*8 :: evapcoeff_a
    real*8 :: evapcoeff_b
    real*8 :: thet(temp_coeff_type)
    real*8 :: thetadj(temp_coeff_type)
    real*8 :: thettbl(temp_coeff_type,81)

end module common_vars_qual
