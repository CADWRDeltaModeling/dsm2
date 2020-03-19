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
!> Collection of common variables required from DSM2-Qual
!>@ingroup process_io
module common_dsm2_qual

    use common_variables
    use common_dsm2_vars

    !-----magic numbers for temperature coefficients
    !     total number of these should be equal to temp_coeff_type,
    !     so temp_coeff_type must be increased if you add more of these
    !     temp_ coefficients
    integer, parameter :: temp_bod_decay = 1
    integer, parameter :: temp_bod_set = 2
    integer, parameter :: temp_reaer = 3
    integer, parameter :: temp_do_ben = 4
    integer, parameter :: temp_orgn_decay = 5
    integer, parameter :: temp_orgn_set = 6
    integer, parameter :: temp_nh3_decay = 7
    integer, parameter :: temp_nh3_ben = 8
    integer, parameter :: temp_no2_decay = 9
    integer, parameter :: temp_orgp_decay = 10
    integer, parameter :: temp_orgp_set = 11
    integer, parameter :: temp_po4_ben = 12
    integer, parameter :: temp_alg_grow = 13
    integer, parameter :: temp_alg_resp = 14
    integer, parameter :: temp_alg_set = 15
    integer, parameter :: temp_alg_die = 16

    !-----Qual Parameters

    type constituent_t
        sequence
        character*16 :: name = ' '            ! constituent name
        integer :: object = miss_val_i        ! object type of injection
        integer :: object_no = miss_val_i     ! object number of injection
        integer :: group_ndx = miss_val_i     ! index to group
        logical :: conservative = .true.      ! true if conservative, false if nonconservative
    end type

    integer, parameter :: max_constituent = 24 ! Maximum number of constituents for an external flow
    integer, parameter :: max_conqext = 12     ! Maximum number of constituents for an external flow
    integer, parameter :: max_channels = 800
    integer, parameter :: max_reservoirs = 100

    logical :: mass_tracking
    logical :: dispersion
    integer :: nres_conc                     ! Number of reservoirs for which initial conc. specified
    real*8:: init_conc = LARGEREAL          ! initial concentration

    !integer :: no_of_constituent             ! number of constituents tracked from all sources
    integer :: no_of_nonconserve_constituent       ! number of constituents tracked from all sources
    integer :: no_all_source                 ! number of constituents tracked from all sources

    !type(constituent_t) :: constituents(max_constituent)
    !-----parameter max_constituent is defined in defs.f=60, Jon 4/12/06

    integer :: nonconserve_ptr(max_constituent) ! pointer to correctly transfer rate coefficients
    integer :: all_source_ptr(max_constituent) ! pointer to correctly transfer rate coefficients
    integer :: constituent_ptr(max_constituent) ! pointer to correctly transfer rate coefficients

    ! list of nonconservative constituents names
    character*20, dimension(max_constituent) :: nonconserve_list = ' '


    !-----misc max values for non-conservative constituents
    integer, parameter :: ncoef_type = 10


    !-----reaction rate coefficients
    integer :: num_res
    real*8:: rcoef_chan(max_constituent,ncoef_type,max_channels)
    real*8:: rcoef_res_temp(max_constituent,ncoef_type,max_reservoirs)
    real*8:: rcoef(max_constituent,ncoef_type)
    real*8:: rcoef_res(max_constituent,ncoef_type,max_reservoirs)

    !character*20:: coeff_res_name(max_reservoirs)

    !-----qual global parameters
    !-----constituents related
    real*8 :: algaefract_n             ! Nonlinear algal self shading coefficient
    real*8 :: algaefract_p             ! Nonlinear algal self shading coefficient
    real*8 :: oxy_photo             ! Nonlinear algal self shading coefficient
    real*8 :: oxy_resp             ! Nonlinear algal self shading coefficient
    real*8 :: oxy_nh3             ! Nonlinear algal self shading coefficient
    real*8 :: oxy_no2             ! Nonlinear algal self shading coefficient
    real*8 :: alg_chl_ratio             ! Nonlinear algal self shading coefficient
    real*8 :: pref_factor             ! Nonlinear algal self shading coefficient
    real*8 :: klight_half             ! Nonlinear algal self shading coefficient
    real*8 :: knit_half             ! Nonlinear algal self shading coefficient
    real*8 :: kpho_half             ! Nonlinear algal self shading coefficient
    real*8 :: lambda0             ! Nonlinear algal self shading coefficient
    real*8 :: lambda1             ! Nonlinear algal self shading coefficient
    real*8 :: lambda2             ! Nonlinear algal self shading coefficient
    real*8 :: alg_bod             ! Nonlinear algal self shading coefficient

    integer, parameter :: temp_coeff_type = 16

    !-----heat and temperature related
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

end module
