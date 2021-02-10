module hg_hdf

    use common_dsm2_vars, only: gtm, io_hdf5, io_write, io_files, NEAREST_BOUNDARY, TO_BOUNDARY
    use common_variables, only:hdf_out
    use state_variables
    use sed_internal_vars
    use  gtm_hdf_ts_write, only: gtm_hdf_t, add_timeseries_attributes,  close_gtm_hdf, write_gtm_chan_hdf , write_gtm_hdf_resv
    use dsm2_time_utils, only: incr_intvl
    use error_handling
    use gtm_precision
    use sed_type_defs
    use sed_bed_hdf
    use hg_internal_vars
	use equilibrium

    implicit none

    type(gtm_sed_hdf_t) :: gtm_sed_Hg_hdf
    type(gtm_hdf_t)     :: gtm_wat_Hg_hdf

    contains

    subroutine init_sed_hg_hdf(n_cells, n_chans, n_res, sim_start, sim_end, hdf_interval_char, use_hdf)
        use hdf5
        use common_variables, only: unit_error,unit_screen
        use common_dsm2_vars, only: print_level
        !this routine is adapted from subroutine init_gtm_hdf in gtm_hdf_ts_write
        !todo: this could probably be rolled into gtm_hdf_ts_write
        !args
        integer, intent(in) :: n_cells
        integer, intent(in) :: n_chans
		integer, intent(in) :: n_res
        integer, intent(in) :: sim_start
        integer, intent(in) :: sim_end
        character*16 :: hdf_interval_char
        logical, intent (in) :: use_hdf
        !local
        logical :: file_exists

        integer :: hdf_start
        real(gtm_real) :: hdf_end
        integer :: hdf_interval
        integer :: ntime                            ! number of time points in hdf5 file
  	    integer(HID_T) :: access_plist              ! Dataset trasfer property
	    integer(SIZE_T) :: rddc_nelmts
	    integer(SIZE_T) :: rddc_nbytes
	    integer :: nelmts
	    real :: rdcc_w0
        logical :: h5_file_exists
	    integer :: error	                        ! HDF5 Error flag

        use_sed_hdf = use_hdf
        if (.not.use_sed_hdf) return
        inquire (file=file_name_hdf_sedhg, exist=file_exists)
    	if (file_exists) write(*,*) 'Deleting existing file: ' // trim(file_name_hdf_sedhg)

	    gtm_sed_Hg_hdf%file_name = file_name_hdf_sedhg
	    ! set up stdio to allow for buffered read/write
	    call H5Pcreate_f(H5P_FILE_ACCESS_F, access_plist, error)

	    call h5pget_cache_f(access_plist, nelmts,   &
                            rddc_nelmts, rddc_nbytes, rdcc_w0,error)
  	    rddc_nbytes = 8000000
	    call h5pset_cache_f(access_plist, nelmts, rddc_nelmts,    &
                            rddc_nbytes, rdcc_w0,error)
	    call verify_error(error, "Cache set")

	    ! start up garbage collection
	    !call h5pset_gc_references_f(access_plist_id,1,error)

        ! Create the HDF5 file
	    if (print_level .gt. 1) then
	         write(unit_screen,*) "Creating new HDF5 file"
	    end if
	    call h5fcreate_f(file_name_hdf_sedhg, H5F_ACC_TRUNC_F,gtm_sed_Hg_hdf%file_id, error, &
                         H5P_DEFAULT_F, access_plist)

        ! create group for output
	    call h5gcreate_f(gtm_sed_Hg_hdf%file_id, "output", gtm_sed_Hg_hdf%data_id, error)

	    call incr_intvl(hdf_interval, 0,hdf_interval_char,TO_BOUNDARY)
	    gtm_sed_Hg_hdf%write_interval = hdf_interval

	    ! This would be more complex if time averages were stored
        call incr_intvl(hdf_start, sim_start, hdf_interval_char, NEAREST_BOUNDARY)
        gtm_sed_Hg_hdf%start_julmin = hdf_start


        ! todo: is this "1+" always right? It wasn't in the original code
        ! record the dimensions of the simulation
	    ntime = 1+(sim_end - hdf_start)/hdf_interval
        hdf_end = hdf_start + (ntime-1)*hdf_interval

        call write_1D_string_array_sed_bed(gtm_sed_Hg_hdf.data_id, "zone", sed_bed_zone)
        call write_1D_string_array_sed_bed(gtm_sed_Hg_hdf.data_id, "layer", sed_bed_layer)
        call write_1D_string_array_sed_bed(gtm_sed_Hg_hdf.data_id, "bed_hg_names", sed_bed_hg)
        call write_1D_string_array_sed_bed(gtm_sed_Hg_hdf.data_id, "bed_hgflux_names", sed_bed_hg_flux)
        call write_1D_string_array_sed_bed(gtm_sed_Hg_hdf.data_id, "wat_hg_names", wat_hg)
        call write_1D_string_array_sed_bed(gtm_sed_Hg_hdf.data_id, "wat_hgflux_names", wat_hg_flux)
        if (trim(hdf_out).eq.'channel') then
	        call init_chan_gtm_sed_hdf5(gtm_sed_hg_hdf, n_chans, sed_bed_hg_count, ntime, "bed_hg", 1)
            call init_chan_gtm_sed_hdf5(gtm_sed_hg_hdf, n_chans, sed_bed_hg_flux_count, ntime, "bed_hgflux", 2)
            call init_chan_gtm_sed_hdf5(gtm_sed_hg_hdf, n_chans, wat_hg_count, ntime, "wat_hg", 3)
            call init_chan_gtm_sed_hdf5(gtm_sed_hg_hdf, n_chans, wat_hg_flux_count, ntime, "wat_hgflux",4)
			call init_chan_gtm_sed_hdf5(gtm_sed_hg_hdf, n_res, wat_hg_count, ntime, "resv_hg",5)              !dhh*** same number of outputs as channels
	    else
	        !call init_cell_gtm_sed_hdf5(gtm_sed_hdf, n_cell, 8, ntime)
        endif
        call add_timeseries_attributes(gtm_sed_Hg_hdf%bed_out_id,  gtm_sed_Hg_hdf%start_julmin,gtm_sed_Hg_hdf%write_interval)
        call add_timeseries_attributes(gtm_sed_Hg_hdf%bed_out_flux_id,  gtm_sed_Hg_hdf%start_julmin,gtm_sed_Hg_hdf%write_interval)
        call add_timeseries_attributes(gtm_sed_Hg_hdf%wat_hg_id,  gtm_sed_Hg_hdf%start_julmin,gtm_sed_Hg_hdf%write_interval)
        call add_timeseries_attributes(gtm_sed_Hg_hdf%wat_hg_flux_id,  gtm_sed_Hg_hdf%start_julmin,gtm_sed_Hg_hdf%write_interval)
		call add_timeseries_attributes(gtm_sed_Hg_hdf%res_hg_id,  gtm_sed_Hg_hdf%start_julmin,gtm_sed_Hg_hdf%write_interval)  !dhh***

        call write_channel_labels(gtm_sed_Hg_hdf.data_id, n_chans)

        gtm_wat_hg_hdf%file_name       = gtm_sed_hdf%file_name
        gtm_wat_hg_hdf%write_interval  = gtm_sed_hdf%write_interval
        gtm_wat_hg_hdf%start_julmin    = gtm_sed_hdf%start_julmin
        gtm_wat_hg_hdf%time_index      = gtm_sed_hdf%time_index
        gtm_wat_hg_hdf%file_id         = gtm_sed_hdf%file_id
        !gtm_wat_hg_hdf%qual_id         = gtm_sed_hdf%qual_id
        gtm_wat_hg_hdf%data_id         = gtm_sed_hdf%data_id
        !gtm_wat_hdf%cell_conc_id    = gtm_sed_hdf%cell_conc_id
        gtm_wat_hg_hdf%chan_conc_id    = gtm_sed_hg_hdf%wat_hg_id
        gtm_wat_hg_hdf%resv_conc_id    = gtm_sed_hg_hdf%res_hg_id
        !gtm_wat_hdf%resv_conc_id
        !gtm_wat_hdf%cell_flow_id
        !gtm_wat_hdf%cell_area_id
        !gtm_wat_hdf%cell_cfl_id
        !gtm_wat_hdf%conc_dim
        !gtm_wat_hdf%cell_dim
        !gtm_wat_hdf%chan_dim
        !gtm_wat_hdf%resv_dim
        !gtm_wat_hdf%time_dim
        return
    end subroutine init_sed_hg_hdf

    subroutine write_gtm_hg_hdf(conc, nconc, nchan, ncell, conc_resv, nresv, time_index, dx, width, name)
        !args
        real (gtm_real), intent(in) :: conc(ncell, nconc)
        integer, intent(in) :: nconc
        integer, intent(in) :: nchan
        integer, intent(in) :: ncell
        real (gtm_real), intent(in) :: conc_resv(nresv, nconc)
        integer, intent(in) :: nresv
        integer, intent(in) :: time_index
        real (gtm_real), intent(in) :: dx(ncell)           !length ft
        real (gtm_real), intent(in) :: width(ncell)        !width ft
        character(len=32), intent(in) :: name(nconc)
        !local
        real(gtm_real) :: area_wet(ncell)
        integer             ::ivar
        integer             ::isediment
        real(gtm_real), parameter :: L = 0.3048d0
          ! source must be in primitive variable
        area_wet = width*dx*L*L
        
        do ivar = 1, nconc
            !conc(:,ivar) = zero
            if (trim(name(ivar)).eq."sediment") then
                isediment = ivar - (nconc - n_sediment)
                conc_tss(:,isediment) = conc(:,ivar)
				conc_tss_resv(:, isediment) = conc_resv(:,ivar)
            endif
        end do
        conc_wat_hdf(:,1) = conc(:,mercury_ivar(1))+ conc(:,mercury_ivar(4)) + conc(:,mercury_ivar(5)) + conc(:,mercury_ivar(6))     !unfiltered hgii
        conc_wat_hdf(:,5) = conc(:,mercury_ivar(2))       !unfiltered mehg

		conc_resv_hdf(:,1) = conc_resv(:,mercury_ivar(1))+ conc_resv(:,mercury_ivar(4)) + conc_resv(:,mercury_ivar(5)) + conc_resv(:,mercury_ivar(6))   !hgii
        conc_resv_hdf(:,5) = conc_resv(:,mercury_ivar(2))     !mehg
        conc_resv_hdf(:,9) = conc_resv(:,mercury_ivar(3))     !hg0
        if (hdf_out .eq. 'channel') then
            call write_gtm_chan_sed_hg_hdf( gtm_sed_hg_hdf, bed, nchan, ncell, n_zones, sed_bed_Hg_count, time_index)
            call write_gtm_chan_wat_hg_hdf(gtm_wat_hg_hdf,nchan, ncell, time_index, area_wet)
			call write_gtm_resv_wat_hg_hdf(gtm_wat_hg_hdf, nresv, time_index)
        end if

        return
    end subroutine write_gtm_hg_hdf

    subroutine write_gtm_chan_sed_hg_hdf(hdf_file, &
                                  bed,          &
                                  nchan,        &
                                  ncell,        &
                                  nzone,        &
                                  nconc,        &
                                  time_index)
        use hdf5
        use common_variables, only: hdf_out, chan_geom
        implicit none
        type(gtm_sed_hdf_t), intent(in) :: hdf_file                    !< hdf file structure
        integer, intent(in) :: nchan                               !< number of channels
        integer, intent(in) :: ncell                               !< number of cells
        integer, intent(in) :: nzone                               !< number of cells
        integer, intent(in) :: nconc                               !< number of constituents
        type(bed_properties_t), intent(in) :: bed(ncell, nzone, 2) !< cell data from transport module
        integer, intent(in) :: time_index                          !< time index to write the data
        real(gtm_real) :: chan_conc(2,nzone, nchan, nconc)         !< channel data from transport module
		integer :: nflux = 10
        real(gtm_real) :: chan_flux(2,nzone, nchan, 10)         !< channel data from transport module
        integer :: chan_rank
        integer(HID_T) :: fspace_id
        integer(HID_T) :: memspace_id
     	integer(HSIZE_T), dimension(4) :: mdata_dims  = 0       ! Dims of data in memory
      	integer(HSIZE_T), dimension(5) :: subset_dims  = 0      ! Dims of subset for time step
    	integer(HSIZE_T), dimension(5) :: h_offset = (/0,0,0,0,0/)
    	integer :: ichan, izone
        integer :: error                                        ! HDF5 Error flag
        integer :: imid
        real(gtm_real)  :: sed_mass(2)                           !nlayers
        real(gtm_real)  :: hgii_mass(2)                           !nlayers
        real(gtm_real)  :: mehg_mass(2)                           !nlayers
        real(gtm_real)  :: volume_pw(2)
        if (mod(time_index,24*10) .eq. 1) call h5garbage_collect_f(error)

        if (nchan .ne. 0) then
            !-----cell conc
            h_offset(1) = 0
            h_offset(2) = 0
            h_offset(3) = 0
            h_offset(4) = 0
            h_offset(5) = time_index

	        subset_dims(1) = 2
            subset_dims(2) = nzone
	        subset_dims(3) = nchan
	        subset_dims(4) = nconc
	        subset_dims(5) = 1

	        mdata_dims(1) = 2
            mdata_dims(2) = nzone
	        mdata_dims(3) = nchan
	        mdata_dims(4) = nconc
            !mdata_dims(5) = 1
            chan_rank = 4

            do ichan = 1, nchan
                imid = nint((chan_geom(ichan)%start_cell + chan_geom(ichan)%end_cell)/2.0)  !write for middle cell in channel
                do izone = 1, nzone

                    sed_mass(:) = sedsolids(imid,izone,:,1,3) + sedsolids(imid,izone,:,2,3) + sedsolids(imid,izone,:,3,3)
                    volume_pw(:) = bed(imid,izone,:).wp_wet*bed(imid,izone,:).thickness*bed(imid,izone,:).porosity
                    hgii_mass(:) = sed_hgii(imid,izone,:,3) + sed_s1_hgii(imid,izone,:,3) + sed_s2_hgii(imid,izone,:,3) + sed_s3_hgii(imid,izone,:,3) &
                                   - hg_conc_sed(imid,izone,:,2)%hgii_diss*volume_pw(:)     !filtered not updated after integration
                    mehg_mass(:) = sed_mehg(imid,izone,:,3) - hg_conc_sed(imid,izone,:,2)%mehg_diss*volume_pw(:)


                    chan_conc(:,izone,ichan,1) = hgii_mass(:)/sed_mass(:)
                    chan_conc(:,izone,ichan,2) = hg_conc_sed(imid,izone,:,2)%hgii_diss

                    if ((hg_conc_sed(imid,izone,1,2)%hgii_diss>zero).and.(chan_conc(1,izone,ichan,1).gt.zero)) then
                        chan_conc(1,izone,ichan,3) = log10((chan_conc(1,izone,ichan,1)/hg_conc_sed(imid,izone,1,2)%hgii_diss)*1.0d6)
                    else
                        chan_conc(1,izone,ichan,3) = zero
                    endif

                    if ((hg_conc_sed(imid,izone,2,2)%hgii_diss>zero).and.(chan_conc(2,izone,ichan,1).gt.zero)) then
                        chan_conc(2,izone,ichan,3) = log10((chan_conc(2,izone,ichan,1)/hg_conc_sed(imid,izone,2,2)%hgii_diss)*1.0d6)
                    else
                        chan_conc(2,izone,ichan,3) = zero
                    endif

                    chan_conc(:,izone,ichan,4) = mehg_mass(:)/sed_mass(:)

                    chan_conc(:,izone,ichan,5) = hg_conc_sed(imid,izone,:,2)%mehg_diss



                    if ((hg_conc_sed(imid,izone,1,2)%mehg_diss>zero).and.(chan_conc(1,izone,ichan,5).gt.zero)) then
                        chan_conc(1,izone,ichan,6) = log10((chan_conc(1,izone,ichan,4)/hg_conc_sed(imid,izone,1,2)%mehg_diss)*1.0d6)
                    else
                        chan_conc(1,izone,ichan,6) = zero
                    endif
                    if ((hg_conc_sed(imid,izone,2,2)%mehg_diss>zero).and.(chan_conc(2,izone,ichan,5).gt.zero)) then
                        chan_conc(2,izone,ichan,6) = log10((chan_conc(2,izone,ichan,4)/hg_conc_sed(imid,izone,2,2)%mehg_diss)*1.0d6)
                    else
                        chan_conc(2,izone,ichan,6) = zero
                    endif

                    if (volume_pw(1)>zero) then
                        chan_conc(:,izone,ichan,7) = sed_hg0(imid,izone,:,3)/volume_pw(:)
                    else
                        chan_conc(:,izone,ichan,7) = zero
                    endif

                end do
            end do

            call H5Screate_simple_f(chan_rank,           &
                                    mdata_dims,          &
                                    memspace_id,         &
                                    error);
            call h5dget_space_f(hdf_file%bed_out_id,   &
                                fspace_id,               &
                                error)
            call h5sselect_hyperslab_f(fspace_id,        &
                                       H5S_SELECT_SET_F, &
                                       h_offset,         &
                                       subset_dims,      &
                                       error)
            call h5dwrite_f(hdf_file%bed_out_id,       &
                            H5T_NATIVE_REAL,             &   ! This was H5T_NATIVE_REAL in old DSM2-Qual. Leaving it as REAL will introduce errors.
                            real(chan_conc),             &
                            mdata_dims,                  &
                            error,                       &
                            memspace_id,                 &
                            fspace_id)

	        call verify_error(error,"Sediment bed hg concs - hdf write")
            call h5sclose_f(fspace_id, error)
			call h5sclose_f(memspace_id, error)

            chan_flux = 0
            do ichan = 1, nchan                         !bed hg fluxes
                imid = nint((chan_geom(ichan)%start_cell + chan_geom(ichan)%end_cell)/2.0)  !write for middle cell in channel
                do izone = 1, nzone
                    if (bed(imid,izone,1).area_wet.gt.zero) then
                        chan_flux(1,izone,ichan,1) = f_settling_hg(imid,izone,mf_hgii,1) &
                                                        /(bed(imid,izone,1).area_wet ) * day_to_sec
                        chan_flux(1,izone,ichan,2) = f_erosion_hg(imid,izone,mf_hgii,1) &
                                                        /(bed(imid,izone,1).area_wet ) * day_to_sec
                        chan_flux(1,izone,ichan,3) = f_burial_hg(imid,izone,mf_hgii,1) &
                                                        /(bed(imid,izone,1).area_wet ) * day_to_sec
                        chan_flux(2,izone,ichan,3) = chan_flux(1,izone,ichan,3)
                        chan_flux(1,izone,ichan,4) = f_diffusion_hg(imid,izone, mf_hgii,1) &
                                                        /(bed(imid,izone,1).area_wet ) * day_to_sec
                        chan_flux(2,izone,ichan,4) = f_sed_diffusion_hg(imid,izone,mf_hgii,1) &           !L2->L1
                                                        /(bed(imid,izone,1).area_wet ) * day_to_sec
                        chan_flux(1,izone,ichan,5) = f_settling_hg(imid,izone,mf_mehg,1)  &
                                                        /(bed(imid,izone,1).area_wet ) * day_to_sec
                        chan_flux(1,izone,ichan,6) = f_erosion_hg(imid,izone,mf_mehg,1) &
                                                        /(bed(imid,izone,1).area_wet ) * day_to_sec
                        chan_flux(1,izone,ichan,7) = f_burial_hg(imid,izone,mf_mehg,1) &
                                                        /(bed(imid,izone,1).area_wet ) * day_to_sec
                        chan_flux(2,izone,ichan,7) = chan_flux(1,izone,ichan,7)

                        chan_flux(1,izone,ichan,8) = f_diffusion_hg(imid,izone,mf_mehg,1) &
                                                        /(bed(imid,izone,1).area_wet ) * day_to_sec
                        chan_flux(2,izone,ichan,8) = f_sed_diffusion_hg(imid,izone,mf_mehg,1) &           !L2->L1
                                                        /(bed(imid,izone,1).area_wet ) * day_to_sec
                        chan_flux(:,izone,ichan,9) = f_sed_methyl(imid,izone,:,1) &
                                                        /(bed(imid,izone,1).area_wet ) * day_to_sec
                        chan_flux(:,izone,ichan,10) = f_sed_demethyl(imid,izone,:,1) &
                                                         /(bed(imid,izone,1).area_wet ) * day_to_sec
                    endif
                enddo
            enddo

			mdata_dims(4) = nflux
            subset_dims(4) = nflux

			call H5Screate_simple_f(chan_rank,           &
                                    mdata_dims,          &
                                    memspace_id,         &
                                    error);
            call h5dget_space_f(hdf_file%bed_out_flux_id,   &
                                fspace_id,               &
                                error)
            call h5sselect_hyperslab_f(fspace_id,        &
                                       H5S_SELECT_SET_F, &
                                       h_offset,         &
                                       subset_dims,      &
                                       error)

            call h5dwrite_f(hdf_file%bed_out_flux_id,       &
                            H5T_NATIVE_REAL,             &   ! This was H5T_NATIVE_REAL in old DSM2-Qual. Leaving it as REAL will introduce errors.
                            real(chan_flux),             &
                            mdata_dims,                  &
                            error,                       &
                            memspace_id,                 &
                            fspace_id)
            call verify_error(error,"Sediment bed hg flux - hdf write")
            call h5sclose_f(fspace_id, error)
            call h5sclose_f(memspace_id, error)
        end if
        return
    end subroutine write_gtm_chan_sed_hg_hdf

    subroutine write_gtm_chan_wat_hg_hdf(hdf_file, &
                                  nchan,        &
                                  ncell,        &
                                  time_index)

        implicit none
        type(gtm_hdf_t), intent(in) :: hdf_file                !< hdf file structure
        integer, intent(in) :: nchan                               !< number of channels
        integer, intent(in) :: ncell                               !< number of cells
        integer, intent(in) :: time_index                          !< time index to write the data
        !local
        real (gtm_real)     :: tss_total(ncell)
        real (gtm_real)     :: wet_area(ncell)
        integer             :: ii, izone
        real (gtm_real)     :: gtm_hdf_time_intvl
        integer             :: time_index_in_gtm_hdf
        hg_conc_wat(:,3) = hg_conc_wat(:,1)  !todo: consider updating partitioning etc
        tss_total(:) = conc_tss(:,1)+conc_tss(:,2)+conc_tss(:,3)
        !conc_wat_hdf(:,1) = hg_conc_wat(:,3)%hgii_ssX(1)*conc_tss(:,1)  + hg_conc_wat(:,3)%hgii_ssX(2)*conc_tss(:,2)   + hg_conc_wat(:,3)%hgii_ssX(3)*conc_tss(:,3) + hg_conc_wat(:,3)%hgii_diss &
        !                    +hg_conc_wat(:,3)%HgII_inert(1)*conc_tss(:,1) + hg_conc_wat(:,3)%HgII_inert(2)*conc_tss(:,2) + hg_conc_wat(:,3)%HgII_inert(3)*conc_tss(:,3)


        conc_wat_hdf(:,2) = hg_conc_wat(:,3)%hgii_diss

        conc_wat_hdf(:,4) = zero              !hgii_kd
        !conc_wat_hdf(:,5) = hg_conc_wat(:,3)%mehg_ss(1)*conc_tss(:,1)   + hg_conc_wat(:,3)%mehg_ss(2)*conc_tss(:,2)   + hg_conc_wat(:,3)%mehg_ss(3)*conc_tss(:,3) + &
        !                    hg_conc_wat(:,3)%mehg_diss

        conc_wat_hdf(:,6) = hg_conc_wat(:,3)%mehg_diss

        conc_wat_hdf(:,8) = zero             !mehg_kd


        do ii = 1, ncell
            if (tss_total(ii).gt.zero) then
                conc_wat_hdf(ii,3) = (hg_conc_wat(ii,3)%hgii_ssX(1)*conc_tss(ii,1)   + hg_conc_wat(ii,3)%hgii_ssX(2)*conc_tss(ii,2)   + hg_conc_wat(ii,3)%hgii_ssX(3)*conc_tss(ii,3) + &
                                    hg_conc_wat(ii,3)%HgII_inert(1)*conc_tss(ii,1) + hg_conc_wat(ii,3)%HgII_inert(2)*conc_tss(ii,2) + hg_conc_wat(ii,3)%HgII_inert(3)*conc_tss(ii,3)) &
                                    /tss_total(ii)
                conc_wat_hdf(ii,7) = (hg_conc_wat(ii,3)%mehg_ss(1)*conc_tss(ii,1)   + hg_conc_wat(ii,3)%mehg_ss(2)*conc_tss(ii,2)   + hg_conc_wat(ii,3)%mehg_ss(3)*conc_tss(ii,3) ) &
                                     /tss_total(ii)
            else
                conc_wat_hdf(ii,3) = zero
                conc_wat_hdf(ii,7) = zero
            endif
            if ((conc_wat_hdf(ii,2).gt.zero).and.(conc_wat_hdf(ii,3).gt.zero)) then   !kd
                conc_wat_hdf(ii,4) = log10(conc_wat_hdf(ii,3)/(conc_wat_hdf(ii,2))*1.0d6)
            else
                conc_wat_hdf(ii,4) = zero
            end if
            if ((conc_wat_hdf(ii,6).gt.zero).and.(conc_wat_hdf(ii,7).gt.zero)) then    !kd
                 conc_wat_hdf(ii,8) = log10(conc_wat_hdf(ii,7)/(conc_wat_hdf(ii,6))*1.0d6)
            else
                conc_wat_hdf(ii,8) = zero
            end if
            conc_wat_hdf(ii,9) = hg_conc_wat(ii,3)%hg0  !0.3d0
        end do
        gtm_wat_hg_hdf%chan_conc_id = gtm_sed_hg_hdf%wat_hg_id !(redundant)

        !call write_gtm_chan_hdf(gtm_wat_hg_hdf, &
        !                   conc_wat_hdf,   &
        !                   nchan,          &
        !                   ncell,          &
        !                   wat_hg_count,   &
        !                   time_index)

        call write_gtm_chan_hdf(gtm_wat_hg_hdf,        &
                                flow_lo,               &
                                flow_hi,               &
                                conc_wat_hdf,          &
                                nchan,                 &
                                ncell,                 &
                                wat_hg_count,          &
                                gtm_hdf_time_intvl,    &
                                budget_prev_flow_lo,   &
                                budget_prev_flow_hi,   &
                                budget_prev_conc,      &
                                time_index,            &
                                .false.)
        conc_wat_flux_hdf = zero
        wet_area(:) = bed(:,1,1)%area_wet + bed(:,2,1)%area_wet + bed(:,3,1)%area_wet
		conc_wat_flux_hdf(:,1) = (f_wat_hg(:)%photodemethyl /wet_area(:))* day_to_sec   !photpdegredaton
        conc_wat_flux_hdf(:,2) = (f_wat_hg(:)%photoreduction /wet_area(:))* day_to_sec  !reduction
        conc_wat_flux_hdf(:,3) = (f_wat_hg(:)%oxidation/wet_area(:))* day_to_sec    !oxidation
        conc_wat_flux_hdf(:,4) = (f_wat_hg(:)%evasion_Hg0/wet_area(:))* day_to_sec    !evasion
        conc_wat_flux_hdf(:,5) = (f_wat_hg(:)%wetdep_HgII/wet_area(:))* day_to_sec    !wet dep
        conc_wat_flux_hdf(:,6) = (f_wat_hg(:)%drydep_HgII/wet_area(:))* day_to_sec   !dry dep
            do izone=1,3
                conc_wat_flux_hdf(:,7) = conc_wat_flux_hdf(:,7) + f_settling_hg(:,izone,mf_hgii,1)
                conc_wat_flux_hdf(:,8) = conc_wat_flux_hdf(:,8) + f_erosion_hg(:,izone,mf_hgii,1)        !hgii erosion
                conc_wat_flux_hdf(:,9) = conc_wat_flux_hdf(:,9) + f_settling_hg(:,izone,mf_mehg,1)
                conc_wat_flux_hdf(:,10) = conc_wat_flux_hdf(:,10) + f_erosion_hg(:,izone,mf_mehg,1)     !mehgii erosion
            enddo

        conc_wat_flux_hdf(:,7) =  (conc_wat_flux_hdf(:,7)/wet_area(:))* day_to_sec
        conc_wat_flux_hdf(:,8) =  (conc_wat_flux_hdf(:,8)/wet_area(:))* day_to_sec
        conc_wat_flux_hdf(:,9) =  (conc_wat_flux_hdf(:,9)/wet_area(:))* day_to_sec
        conc_wat_flux_hdf(:,10) = (conc_wat_flux_hdf(:,10)/wet_area(:))* day_to_sec

        gtm_wat_hg_hdf%chan_conc_id = gtm_sed_hg_hdf%wat_hg_flux_id

        !call write_gtm_chan_hdf(gtm_wat_hg_hdf, &
        !                   conc_wat_flux_hdf,   &
        !                   nchan,          &
        !                   ncell,          &
        !                   10,              &
        !                   time_index)
        call write_gtm_chan_hdf(gtm_wat_hg_hdf,        &
                                flow_lo,               &
                                flow_hi,               &
                                conc_wat_flux_hdf,     &
                                nchan,                 &
                                ncell,                 &
                                10,                    &
                                gtm_hdf_time_intvl,    &
                                budget_prev_flow_lo,   &
                                budget_prev_flow_hi,   &
                                budget_prev_conc,      &
                                time_index,            &
                                .false.)
        return
    end subroutine write_gtm_chan_wat_hg_hdf

	subroutine write_gtm_resv_wat_hg_hdf(hdf_file, &
                                  n_res,        &
                                  time_index)
        implicit none
        !args
        type(gtm_hdf_t), intent(in) :: hdf_file                !< hdf file structure
        integer, intent(in) :: n_res                           !< number of reservoirs
        integer, intent(in) :: time_index                      !< time index to write the data
        !local
        real (gtm_real)     :: tss_total(n_res)
        integer             :: ii
        hg_conc_resv(:,3) = hg_conc_resv(:,1)  !rk step 1 ????

        tss_total(:) = conc_tss_resv(:,1)+conc_tss_resv(:,2)+conc_tss_resv(:,3)
        conc_resv_hdf(:,2) = hg_conc_resv(:,3)%hgii_diss
        conc_resv_hdf(:,3) = zero              !solids
        conc_resv_hdf(:,4) = zero              !hgii_kd
        conc_resv_hdf(:,6) = hg_conc_resv(:,3)%mehg_diss
        conc_resv_hdf(:,7) = zero
        conc_resv_hdf(:,8) = zero             !mehg_kd

        !conc_resv_hdf(:,9) = hg_conc_resv(ii,3)%hg0  !0.3d0

        do ii = 1, n_res
            if (tss_total(ii).gt.zero) then
                conc_resv_hdf(ii,3) = (hg_conc_resv(ii,3)%hgii_ssX(1)*conc_tss_resv(ii,1)   + hg_conc_resv(ii,3)%hgii_ssX(2)*conc_tss_resv(ii,2)   + hg_conc_resv(ii,3)%hgii_ssX(3)*conc_tss_resv(ii,3) + &
                                    hg_conc_resv(ii,3)%HgII_inert(1)*conc_tss_resv(ii,1) + hg_conc_resv(ii,3)%HgII_inert(2)*conc_tss_resv(ii,2) + hg_conc_resv(ii,3)%HgII_inert(3)*conc_tss_resv(ii,3)) &
                                    /tss_total(ii)
                conc_resv_hdf(ii,7) = (hg_conc_resv(ii,3)%mehg_ss(1)*conc_tss_resv(ii,1)   + hg_conc_resv(ii,3)%mehg_ss(2)*conc_tss_resv(ii,2)   + hg_conc_resv(ii,3)%mehg_ss(3)*conc_tss_resv(ii,3) ) &
                                     /tss_total(ii)
            endif

            if ((conc_resv_hdf(ii,2).gt.zero).and.(conc_resv_hdf(ii,3).gt.zero)) then   !kd Hgii
                conc_resv_hdf(ii,4) = log10(conc_resv_hdf(ii,3)/(conc_resv_hdf(ii,2))*1.0d6)
            end if
            if ((conc_resv_hdf(ii,6).gt.zero).and.(conc_resv_hdf(ii,7).gt.zero)) then    !kd MeHg
                 conc_resv_hdf(ii,8) = log10(conc_resv_hdf(ii,7)/(conc_resv_hdf(ii,6))*1.0d6)
            end if
        end do

        call  write_gtm_hdf_resv(hdf_file,       &
                                  conc_resv_hdf, &   !dhh***
                                  n_res,         &
                                  wat_hg_count,  &
                                  time_index)
        return

    end subroutine write_gtm_resv_wat_hg_hdf
    subroutine close_gtm_hg_hdf()
        use hdf5
        use common_variables, only: unit_error,unit_screen, hdf_out
        use common_dsm2_vars, only: print_level


        integer        :: error	! HDF5 Error flag

        !-------Close the datasets
        if (print_level .gt.2) write(unit_screen,*)"Closing HDF5 data sets"

        if (hdf_out .eq. 'channel') then
	        call h5dclose_f(gtm_sed_Hg_hdf%bed_out_id,error)
            if (error .ne. 0) then
	            write(unit_error,*)"HDF5 error closing chan conc data set: ",error
            end if
            call h5dclose_f(gtm_sed_Hg_hdf%bed_out_flux_id,error)
            if (error .ne. 0) then
	            write(unit_error,*)"HDF5 error closing chan conc data set: ",error
            end if
            call h5dclose_f(gtm_sed_Hg_hdf%wat_hg_id,error)
            if (error .ne. 0) then
	            write(unit_error,*)"HDF5 error closing chan conc data set: ",error
            end if
            call h5dclose_f(gtm_sed_Hg_hdf%wat_hg_flux_id,error)
            if (error .ne. 0) then
	            write(unit_error,*)"HDF5 error closing chan conc data set: ",error
	        end if
			call h5dclose_f(gtm_sed_Hg_hdf%res_hg_id,error)  !dhh***
            if (error .ne. 0) then
	            write(unit_error,*)"HDF5 error closing chan conc data set: ",error
            end if
	    end if

       ! if (hdf_file%resv_dim.gt.0) then
	   !     call h5dclose_f(hdf_file%resv_conc_id,error)
	   !     if (error .ne. 0) then
	   !         write(unit_error,*)"HDF5 error closing reservoir conc data set: ",error
	   !     end if
	   ! end if

        !-----Close the groups in the dataset. Only the data group should be open
        !     on an ongoing basis
        call h5gclose_f(gtm_sed_Hg_hdf%data_id, error)
        if (error .ne. 0) then
            write(unit_error,*)"HDF5 error closing data group: ",error
        end if

        !-------Close the file
 333    if (print_level .gt.1) write(unit_screen,*)"Closing HDF5 file"
	    call h5fclose_f(gtm_sed_Hg_hdf%file_id, error)
        if (error .ne. 0) then
	       write(unit_error,*)"HDF5 error closing hdf file: ",error
        end if

    end subroutine close_gtm_hg_hdf

	subroutine print_last_stage_hg(cdtdate,        &
                                intdate,        &
                                out_conc_resv,  &
                                ncell,          &
                                nresv,          &
                                nvar)
        use common_variables, only : constituents, resv_geom
        use gtm_precision
        use sed_type_defs
        implicit none
        character(len=14), intent(in) :: cdtdate
        integer, intent(in) :: intdate
        integer, intent(in) :: ncell
        integer, intent(in) :: nresv
        integer, intent(in) :: nvar
        real(gtm_real) :: out_conc_resv(nresv,nvar)
        real(gtm_real)  :: sed_mass(2)                           !nlayers
        real(gtm_real)  :: hgii(2)
        real(gtm_real)  :: hgii_s1(2)
        real(gtm_real)  :: hgii_s2(2)
        real(gtm_real)  :: hgii_s3(2)
        real(gtm_real)  :: mehg(2)
        real(gtm_real)  :: hg0(2)
        real(gtm_real)  ::volume_pw(2)
        integer :: ncol
        integer :: a(nvar)
        character*16 :: c(nvar)
        integer :: i, j , k
        ncol = 12    !<nosolids

        open(801,file="init_sed_hg.txt")
        write(801,*) cdtdate, "/time"
        write(801,*) intdate, "/julmin"
        write(801,*) ncol, "/n_column"
        write(801,*) ncell, "/n_cell"
        write(801,*) n_zones, "/n_zone"
        write(801,'(a7,a,a7,a, <ncol>(a18,a))') "cell_no",achar(9),"zone",achar(9),"hg0_L1",achar(9),"hgII_L1",achar(9),"hg_s1_L1",achar(9),"hg_s2_L1",achar(9),"hg_s3_L1",achar(9),"meHg_L1",achar(9), &
                                                                                   "hg0_L2",achar(9),"hgII_L2",achar(9),"hg_s1_L2",achar(9),"hg_s2_L2",achar(9),"hg_s3_L2",achar(9),"meHg_L2"
        do i = 1, ncell
            do j = 1, n_zones
                sed_mass(:) = sedsolids(i,j,:,1,3) + sedsolids(i,j,:,2,3) + sedsolids(i,j,:,3,3)


                hgii(:) = sed_hgii(i,j,:,3)/sed_mass(:)     !assume pw conc is zero
                hgii_s1(:) = sed_s1_hgii(i,j,:,3)/sedsolids(i,j,:,1,3)
                hgii_s2(:) = sed_s2_hgii(i,j,:,3)/sedsolids(i,j,:,2,3)
                hgii_s3(:) = sed_s3_hgii(i,j,:,3)/sedsolids(i,j,:,3,3)
                mehg(:) = sed_mehg(i,j,:,3)/sed_mass(:)
                if  (bed(i,j,1).wp_wet > 0) then
                    volume_pw(:) = bed(i,j,:).wp_wet*bed(i,j,:).thickness*bed(i,j,:).porosity
                    hg0(:) = sed_hg0(i,j,:,3)/volume_pw(:)
                else
                    hg0(:) = zero
                endif

                write(801,'(i5,a,i5,a,<ncol>(f18.15, a))') i, achar(9), j, achar(9), hg0(1), achar(9), hgii(1), achar(9), hgii_s1(1), achar(9), hgii_s2(1), achar(9), hgii_s3(1),achar(9), mehg(1), achar(9), &
                                                                                     hg0(2), achar(9), hgii(2), achar(9), hgii_s1(2), achar(9), hgii_s2(2), achar(9), hgii_s3(2),achar(9), mehg(2)

            end do
        end do
        close(801)
        return
        write(801,*) nresv, "/n_resv"   !todo: add reservoirs
        write(801,'(a32,<ncol>a32)') "reservoir_name", (c(j),j=1,ncol)
        do i = 1, nresv
            write(801,'(a32,<ncol>f32.16)') resv_geom(i)%name, (out_conc_resv(i,a(j)),j=1,ncol)
        end do
        close(801)
        return
end subroutine
end module