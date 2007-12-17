

C-----Qual Parameters

      logical mass_tracking,dispersion
      integer nres_conc         ! Number of reservoirs for which initial conc. specified
      real*8 init_conc            ! initial concentration

      common/qual_scalars/nres_conc,mass_tracking,
     &     init_conc,dispersion

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

      character*20
     &     nonconserve_list(max_constituent) ! list of nonconservative constituents

      common /constituent_param/ constituents,
     &     no_of_constituent,no_of_nonconserve_constituent,no_all_source
     &     ,nonconserve_list,nonconserve_ptr,all_source_ptr
     &     ,constituent_ptr

c-----misc max values for non-conservative constituents
      integer
     &     ncoef_type
      parameter (ncoef_type=10)


c-----reaction rate coefficients
      integer num_res
      real*8 rcoef_chan, rcoef_res_temp, rcoef_res, rcoef
      character*20 coeff_res_name

      common /rate_coeff_c_i/num_res
      common /rate_coeff_c_r/
     &     rcoef_chan(max_constituent,ncoef_type,max_channels)
     &     ,rcoef_res_temp(max_constituent,ncoef_type,max_reservoirs)
     &     ,rcoef_res(max_constituent,ncoef_type,max_reservoirs)
     &     ,rcoef(max_constituent,ncoef_type)
     &     ,coeff_res_name(max_reservoirs)

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
	              


      common/global_param/
     &     algaefract_n
     &     ,algaefract_p
     &     ,oxy_photo
     &     ,oxy_resp
     &     ,oxy_nh3
     &     ,oxy_no2
     &     ,alg_chl_ratio
     &     ,pref_factor
     &     ,klight_half
     &     ,knit_half
     &     ,kpho_half
     &     ,lambda0
     &     ,lambda1
     &     ,lambda2
     &     ,alg_bod 

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
     &     ,thet
     &     ,thetadj
     &     ,thettbl

      integer
     &     temp_coeff_type

      parameter (temp_coeff_type=30)

      common /temperature_param/
     &     elev
     &     ,lat
     &     ,longitude
     &     ,long_std_merid
     &     ,dust_attcoeff
     &     ,evapcoeff_a
     &     ,evapcoeff_b
     &     ,thet(temp_coeff_type)
     &     ,thetadj(temp_coeff_type)
     &     ,thettbl(temp_coeff_type,81)
















