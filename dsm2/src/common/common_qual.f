!<license>
!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
!    Department of Water Resources.
!    This file is part of DSM2.

!    The Delta Simulation Model 2 (DSM2) is free software:
!    you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.

!    DSM2 is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.

!    You should have received a copy of the GNU General Public License
!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
!</license>

module common_qual
    use type_defs
    use constants
    use grid_data
    !C-----Qual Parameters

    logical :: mass_tracking
    logical :: dispersion
    integer :: nres_conc             ! Number of reservoirs for which initial conc. specified
    real*8:: init_conc = miss_val_r ! initial concentration

    integer :: &
        no_of_constituent       ! number of constituents tracked from all sources
    integer :: no_of_nonconserve_constituent       ! number of constituents tracked from all sources
    integer :: no_all_source       ! number of constituents tracked from all sources

    type(constituent_t) :: constituents(max_constituent)
    !-----parameter max_constituent is defined in defs.f=60, Jon 4/12/06

    integer :: &
        nonconserve_ptr(max_constituent) ! pointer to correctly transfer rate coefficients
    integer :: all_source_ptr(max_constituent) ! pointer to correctly transfer rate coefficients
    integer :: constituent_ptr(max_constituent) ! pointer to correctly transfer rate coefficients

    ! list of nonconservative constituents names
    character*20, dimension(max_constituent) :: nonconserve_list = ' '


    !-----misc max values for non-conservative constituents
    integer :: ncoef_type
    parameter (ncoef_type=10)


    !-----reaction rate coefficients
    integer :: num_res
    real*8:: rcoef_chan(max_constituent,ncoef_type,max_channels)
    real*8:: rcoef_res_temp(max_constituent,ncoef_type,max_reservoirs)
    real*8:: rcoef(max_constituent,ncoef_type)
    real*8:: rcoef_res(max_constituent,ncoef_type,max_reservoirs)
      
    character*20:: coeff_res_name(max_reservoirs)

    !-----qual global parameters
    !-----constituents related
    real*8 :: &
        algaefract_n             ! Nonlinear algal self shading coefficient
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
    real*8 :: &
        elev
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
