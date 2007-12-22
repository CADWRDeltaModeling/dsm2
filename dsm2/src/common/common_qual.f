!<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    DSM2 is free software: you can redistribute it and/or modify
C!    it under the terms of the GNU General Public License as published by
C!    the Free Software Foundation, either version 3 of the License, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public License for more details.

C!    You should have received a copy of the GNU General Public License
C!    along with DSM2.  If not, see <http://www.gnu.org/licenses/>.
!</license>

      module common_qual
      use type_defs
      use constants
      use grid_data
C-----Qual Parameters

      logical mass_tracking
      logical dispersion
      integer nres_conc         ! Number of reservoirs for which initial conc. specified
      real*8 init_conc            ! initial concentration

      integer
     &     no_of_constituent    ! total number of conserv+nonconserv constituents
     &     ,no_of_nonconserve_constituent ! no. of nonconservative constituents
     &     ,no_all_source       ! number of constituents tracked from all sources

      type(constituent_t) constituents(max_constituent)
c-----parameter max_constituent is defined in defs.f=60, Jon 4/12/06

      integer
     &     nonconserve_ptr(max_constituent) ! pointer back to constituents structure
     &     ,all_source_ptr(max_constituent) ! pointer back to sonstituents from all sources
     &     ,constituent_ptr(max_constituent) ! pointer to correctly transfer rate coefficients

      ! list of nonconservative constituents names
      character*20, dimension(max_constituent) :: nonconserve_list = ' '


c-----misc max values for non-conservative constituents
      integer
     &     ncoef_type
      parameter (ncoef_type=10)


c-----reaction rate coefficients
      integer num_res
      real*8 :: rcoef_chan(max_constituent,ncoef_type,max_channels)
      real*8 :: rcoef_res_temp(max_constituent,ncoef_type,max_reservoirs)
      real*8 :: rcoef(max_constituent,ncoef_type)
      real*8 :: rcoef_res(max_constituent,ncoef_type,max_reservoirs)
      
      character*20 :: coeff_res_name(max_reservoirs)

c-----qual global parameters
c-----constituents related
      real*8
     &     algaefract_n	 	! fraction of algae as Nitrogen
     &     ,algaefract_p        ! fraction of algae as Phosphorus
     &     ,oxy_photo 	 	! Oxygen produced per unit of algal growth
     &     ,oxy_resp            ! O2 uptake per unit of algal respiration
     &     ,oxy_nh3             ! Oxygen used in oxidiation of NH3 to NO2
     &     ,oxy_no2             ! Oxygen used in oxidiation of NO2 to NO3
     &     ,alg_chl_ratio       ! Chlorophyll to biomass ratio
     &     ,pref_factor         ! algal pref. factor for NH3
     &     ,klight_half	 	! MM half-saturation constant for light
     &     ,knit_half	 	! MM half-saturation constant for nitrogen
     &     ,kpho_half	 	! MM half-saturation const. for phosphorus
     &     ,lambda0             ! Non algal light extinction coefficient
     &     ,lambda1             ! Linear algal self shading coefficient
     &     ,lambda2
     &     ,alg_bod          ! Nonlinear algal self shading coefficient
	              



      integer, parameter :: temp_coeff_type = 30

c-----heat and temperature related
      real*8
     &     elev			! Basin elevation, ft
     &     ,lat			! Latitude, degrees
     &     ,longitude   ! Longitude,degrees  !fixme: changed from "long",
	                                         ! this will break qual
     &     ,long_std_merid      ! Longitude of stand. meridian,degrees
     &     ,dust_attcoeff       ! Dust attenuation coefficient
     &     ,evapcoeff_a		! Evaporation coefficient, A
     &     ,evapcoeff_b		! Evaporation coefficient, B
     &     ,thet(temp_coeff_type)
     &     ,thetadj(temp_coeff_type)
     &     ,thettbl(temp_coeff_type,81)


      end module


















