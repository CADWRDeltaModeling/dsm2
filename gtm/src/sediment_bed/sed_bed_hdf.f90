
    
    
module sed_bed_hdf

    use common_dsm2_vars, only: gtm, io_hdf5, io_write, io_files, NEAREST_BOUNDARY, TO_BOUNDARY
    use common_variables, only:hdf_out
    use sed_internal_vars
    use  gtm_hdf_ts_write, only: gtm_hdf_t, add_timeseries_attributes,  close_gtm_hdf
    use dsm2_time_utils, only: incr_intvl
    use error_handling
    use gtm_precision
    use sed_type_defs
    implicit none
  
    type(gtm_sed_hdf_t) :: gtm_sed_hdf
    !type(gtm_sed_hdf_t) :: gtm_sed_flux_hdf
    
    integer, parameter :: sed_bed_solids = 1
    integer, parameter :: sed_bed_solids_flux = 2
    integer, parameter :: sed_bed_zone = 3
    integer, parameter :: sed_bed_layer = 5
    integer, parameter :: sed_bed_hg = 6
    integer, parameter :: sed_bed_hg_flux = 7
    integer, parameter :: wat_hg = 8
    integer, parameter :: wat_hg_flux = 9
    
    integer, parameter :: sed_bed_solids_count = 4
    integer, parameter :: sed_bed_solids_flux_count = 5
    integer, parameter :: sed_bed_hg_count = 7          !hgii, mehg,hgii_pw,mehg_pw,hg0,kd_hgii,kd_mehg
    integer, parameter :: sed_bed_hg_flux_count = 10    !todo: itemize mercury fluxes
    integer, parameter :: wat_hg_count = 9
    integer, parameter :: wat_hg_flux_count = 10
    contains
    
    subroutine init_sed_hdf(n_cells, n_chans, sim_start, sim_end, hdf_interval_char, use_hdf)
        use hdf5
        use common_variables, only: unit_error,unit_screen
        use common_dsm2_vars, only: print_level
        !this routine is adapted from subroutine init_gtm_hdf in gtm_hdf_ts_write
        !todo: this could probably be rolled into gtm_hdf_ts_write
        !args
        integer, intent(in) :: n_cells
        integer, intent(in) :: n_chans
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
        inquire (file=file_name_hdf_sed, exist=file_exists)
    	if (file_exists) write(*,*) 'Deleting existing file: ' // trim(file_name_hdf_sed) 
	                  
	    gtm_sed_hdf%file_name = file_name_hdf_sed
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
	    call h5fcreate_f(file_name_hdf_sed, H5F_ACC_TRUNC_F,gtm_sed_hdf%file_id, error, &     
                         H5P_DEFAULT_F, access_plist)
      
        ! create group for output
	    call h5gcreate_f(gtm_sed_hdf%file_id, "output", gtm_sed_hdf%data_id, error)
	
	    call incr_intvl(hdf_interval, 0,hdf_interval_char,TO_BOUNDARY)
	    gtm_sed_hdf%write_interval = hdf_interval
	   	
	    ! This would be more complex if time averages were stored
        call incr_intvl(hdf_start, sim_start, hdf_interval_char, NEAREST_BOUNDARY)
        gtm_sed_hdf%start_julmin = hdf_start

        
        ! todo: is this "1+" always right? It wasn't in the original code      
        ! record the dimensions of the simulation
	    ntime = 1+(sim_end - hdf_start)/hdf_interval
        hdf_end = hdf_start + (ntime-1)*hdf_interval
        
        call write_1D_string_array_sed_bed(gtm_sed_hdf.data_id, "bed_solid_names", sed_bed_solids)
        call write_1D_string_array_sed_bed(gtm_sed_hdf.data_id, "zone", sed_bed_zone)
        call write_1D_string_array_sed_bed(gtm_sed_hdf.data_id, "layer", sed_bed_layer)
        
        call write_1D_string_array_sed_bed(gtm_sed_hdf.data_id, "bed_solid_flux_names", sed_bed_solids_flux)
        if (trim(hdf_out).eq.'channel') then
	        call init_chan_gtm_sed_hdf5(gtm_sed_hdf, n_chans, sed_bed_solids_count, ntime, "bed_solids", sed_bed_solids)
            call init_chan_gtm_sed_hdf5(gtm_sed_hdf, n_chans, sed_bed_solids_flux_count, ntime, "bed_solid_fluxes", sed_bed_solids_flux)
	    else
	        !call init_cell_gtm_sed_hdf5(gtm_sed_hdf, n_cell, 8, ntime)
        endif
        call add_timeseries_attributes(gtm_sed_hdf.bed_out_id,  gtm_sed_hdf%start_julmin,gtm_sed_hdf%write_interval)
        call add_timeseries_attributes(gtm_sed_hdf.bed_out_flux_id,  gtm_sed_hdf%start_julmin,gtm_sed_hdf%write_interval)
        call write_channel_labels(gtm_sed_hdf.data_id, n_chans)
        
        
        
    end subroutine init_sed_hdf

    subroutine init_chan_gtm_sed_hdf5(hdf_file, nchan, nconc, ntime, name, itype)
        use hdf5
	    use time_utilities, only: jmin2cdt
        use gtm_hdf_ts_write, only : HDF_SZIP_PIXELS_PER_BLOCK, TIME_CHUNK, MIN_STEPS_FOR_CHUNKING
	    
        !args
        type(gtm_sed_hdf_t), intent(inout)  :: hdf_file
        integer, intent (in)            :: nchan
        integer, intent (in)            :: nconc
        integer, intent (in)            :: ntime
        character *(*), intent (in)     :: name
        integer, intent (in)            :: itype
        !local	    
   	    integer(HID_T) :: attr_id                         ! Attribute identifier 
        integer(HID_T) :: aspace_id                       ! Attribute dataspace identifier 
        integer(HID_T) :: atype_id                        ! Attribute type identifier 
        integer(HSIZE_T), dimension(1) :: adims = (/1/)   ! Attribute dimension
        integer     ::   arank = 1                        ! Attribute rank
        integer(HSIZE_T), dimension(7) :: a_data_dims
        integer     ::   chan_rank = 0
   	    !integer(HSIZE_T), dimension(5) :: chan_file_dims  = 0 
	    !integer(HSIZE_T), dimension(5) :: chan_chunk_dims = 0 
        integer(HSIZE_T), allocatable :: chan_file_dims(:)  ! Data size on file
        integer(HSIZE_T), allocatable :: chan_chunk_dims(:) ! Dataset chunk dimensions
	    integer(HID_T) :: fspace_id                       ! File space identifier
	    integer(HID_T) :: cparms                          ! dataset creatation property identifier 
	    integer        :: error                           ! HDF5 Error flag
	    
	    call h5screate_simple_f(arank, adims, aspace_id, error)
	    call h5tcopy_f(H5T_NATIVE_INTEGER, atype_id, error)
        
        select case (itype)
		! Create cell data set, one data point per cell
        case (1,2)
            allocate(chan_file_dims(5))
            allocate(chan_chunk_dims(5))
            chan_file_dims = 0
            chan_chunk_dims = 0
            chan_rank = 5
            chan_file_dims(1) = 2           !number of layers = 2layer
            chan_file_dims(2) = n_zones     !number of zones
            chan_file_dims(3) = nchan
	        chan_file_dims(4) = nconc
	        chan_file_dims(5) = ntime
      
            chan_chunk_dims(1) = 2
            chan_chunk_dims(2) = n_zones
	        chan_chunk_dims(3) = nchan
	        chan_chunk_dims(4) = nconc
	        chan_chunk_dims(5) = min(TIME_CHUNK,ntime)
        case (3:)
            allocate(chan_file_dims(4))
            allocate(chan_chunk_dims(4))
            chan_file_dims = 0
            chan_chunk_dims = 0
            chan_rank = 4
            chan_file_dims(1) = 2           
            chan_file_dims(2) = nchan
	        chan_file_dims(3) = nconc
	        chan_file_dims(4) = ntime
      
            chan_chunk_dims(1) = 2
	        chan_chunk_dims(2) = nchan
	        chan_chunk_dims(3) = nconc
	        chan_chunk_dims(4) = min(TIME_CHUNK,ntime)
        end select
	    cparms = 0
        
		! Add chunking and compression
	    call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, error)
        if (ntime.gt. MIN_STEPS_FOR_CHUNKING) then
          call h5pset_chunk_f(cparms,                   &    
                              chan_rank,                &      
                              chan_chunk_dims,          &     
                              error)
	      call H5Pset_szip_f (cparms, H5_SZIP_NN_OM_F,  &     
                              HDF_SZIP_PIXELS_PER_BLOCK, error);
	    end if
       
        ! initialize table for channel concentration
	    call h5screate_simple_f(chan_rank,              &   
                                chan_file_dims,         &     
                                fspace_id,              &    
                                error)
        select case ( itype)
        case (1)
	        call h5dcreate_f(hdf_file%data_id,          &  
                             trim(name),                    &    
                             H5T_NATIVE_REAL,               &	   
                             fspace_id,                     &
                             hdf_file%bed_out_id,        &    
                             error,                         &     
                             cparms)                      
        case (2)
            call h5dcreate_f(hdf_file%data_id,          &  
                             trim(name),                    &    
                             H5T_NATIVE_REAL,               &	   
                             fspace_id,                     &
                             hdf_file%bed_out_flux_id,        &    
                             error,                         &     
                             cparms)  
        case (3)
            call h5dcreate_f(hdf_file%data_id,          &  
                             trim(name),                    &    
                             H5T_NATIVE_REAL,               &	   
                             fspace_id,                     &
                             hdf_file%wat_hg_id,        &    
                             error,                         &     
                             cparms)
        case (4)
            call h5dcreate_f(hdf_file%data_id,          &  
                             trim(name),                    &    
                             H5T_NATIVE_REAL,               &	   
                             fspace_id,                     &
                             hdf_file%wat_hg_flux_id,        &    
                             error,                         &     
                             cparms)  
        end select
        deallocate(chan_file_dims)
        deallocate(chan_chunk_dims)
	    call verify_error(error,"Channel "//trim(name)//" output dataset creation")
        !call add_timeseries_attributes(hdf_file%chan_conc_id,   &  
        !                               hdf_file%start_julmin,   &
        !                               hdf_file%write_interval)                                                                             
        return
    end subroutine init_chan_gtm_sed_hdf5
    
    !> Write one dimensional string array to dataset
    subroutine write_1D_string_array_sed_bed(dest_id, name, iopt)
    
        use hdf5
        use sed_type_defs, only: n_zones
        implicit none
        integer(HID_T),intent(in) :: dest_id    !< Destination group identifier  
        character*(*), intent(in) :: name       !< Name of array
         integer, intent(in)      :: iopt       
        !args
  	    integer                         :: strlen !< Length of string element
        integer                         :: nstr   !< length of string element
        integer                         :: ii
	    character(len=:), dimension(:), allocatable   :: arr    !< string array
        integer(HID_T) :: dspace_id     ! Dataspace identifier
        integer(HID_T) :: dtype_id      ! Dataspace identifier
        integer(HID_T) :: dset_id       ! Dataspace identifier
        integer, parameter :: drank = 1 ! Dataset rank
        integer, parameter :: arank = 1 ! Attribute rank
        integer :: error                ! HDF5 Error flag
        integer(HSIZE_T), dimension(arank) :: data_dims
        character * 3                ::istr
        
        select case (iopt)
            case (sed_bed_solids)
                strlen = 20
                nstr = sed_bed_solids_count
                allocate(character(strlen) :: arr(nstr))
                arr(1) = "thickness (m)"
                arr(2) = "mass_frac_1(inorg)"
                arr(3) = "mass_frac_2(org)"
                arr(4) = "mass_frac_3(sand)"
            case (sed_bed_solids_flux)
                strlen = 32
                nstr = sed_bed_solids_flux_count
                allocate(character(strlen) :: arr(nstr))
                arr(1) = "deposition (g/m2/yr)"
                arr(2) = "resuspension (g/m2/yr)"
                arr(3) = "mass_turnover (g/m2/yr)"
                arr(4) = "burial/erosion (g/m2/yr)"
                arr(5) = "carbon_turnover (g/m2/yr)"
            case (sed_bed_zone)
                strlen = 20
                nstr = n_zones
                allocate(character(strlen) :: arr(nstr))
                do ii=1,n_zones
                    write (istr,"(I2)") ii
                    istr = adjustl(istr)
                    arr(ii) = "zone_" // istr
                end do
            case (sed_bed_layer)
                strlen = 20
                nstr = 2
                allocate(character(strlen) :: arr(nstr))
                arr(1) = "layer_1"
                arr(2) = "layer_2"
            case (sed_bed_hg)
                strlen = 32
                nstr = sed_bed_hg_count
                allocate(character(strlen) :: arr(nstr))
                arr(1) = "hgii (ug/g)"
                arr(2) = "hgii_pw (ng/L)"
                arr(3) = "kd_hgii (L/kg)"
                arr(4) = "mehg (ug/g)"
                arr(5) = "mehg_pw (ng/L)"
                arr(6) = "kd_mehg (L/kg)"
                arr(7) = "hg0 (ng/L)"
                
               
             case (sed_bed_hg_flux)
                strlen = 32
                nstr = sed_bed_hg_flux_count
                allocate(character(strlen) :: arr(nstr))
                arr(1) = "hgii settle (ug/m2/yr)"
                arr(2) = "hgii resusp (ug/m2/yr)"
                arr(3) = "hgii burial (ug/m2/yr)"
                arr(4) = "hgii diffusion - out (ug/m2/yr)"
                arr(5) = "mehg settle (ug/m2/yr)"
                arr(6) = "mehg resusp (ug/m2/yr)"
                arr(7) = "mehg burial (ug/m2/yr)"
                arr(8) = "mehg diffusion - out (ug/m2/yr)"
                arr(9) = "methyl (ug/m2/yr)"
                arr(10) = "demethyl (ug/m2/yr)"
               
                
            case (wat_hg)
                strlen = 32
                nstr = wat_hg_count
                allocate(character(strlen) :: arr(nstr))
                arr(1) = "hgii unfilt (ng/l)"
                arr(2) = "hgii filt (ng/l)"
                arr(3) = "hgii solid (ug/g)"
                arr(4) = "hgii Kd (L/kg)"
                arr(5) = "mehg unfilt (ng/l)"
                arr(6) = "mehg filt (ng/l)"
                arr(7) = "mehg solid (ug/g)"
                arr(8) = "mehg Kd (L/kg)"
                arr(9) = "hg0 (ng/L)"
            case (wat_hg_flux)
                strlen = 32
                nstr = wat_hg_flux_count
                allocate(character(strlen) :: arr(nstr))
                arr(1) = "photodegradation (ug/m2/yr)"
                arr(2) = "reduction (ug/m2/yr)"
                arr(3) = "oxidation (ug/m2/yr)"
                arr(4) = "evasion (ug/m2/yr)"
                arr(5) = "hgii wet dep (ug/m2/yr)"
                arr(6) = "hgii dry dep (ug/m2/yr)"
                arr(7) = "hgii settle (ug/m2/yr)"
                arr(8) = "hgii erosion (ug/m2/yr)"
                arr(9) = "mehg settle (ug/m2/yr)"
                arr(10) = "mehg erosion (ug/m2/yr)"
        end select
               
	    data_dims(1) = nstr
        call h5tcopy_f(H5T_NATIVE_CHARACTER, dtype_id, error)
        call h5tset_size_f(dtype_id, strlen, error)
	    call h5screate_simple_f(drank, data_dims, dspace_id, error)
	    call h5dcreate_f(dest_id,trim(name),dtype_id, dspace_id, dset_id, error)
        call h5dwrite_f(dset_id,dtype_id, arr(1), data_dims, error)
        call h5tclose_f(dtype_id,error)
        call h5sclose_f(dspace_id,error)
        call h5dclose_f(dset_id,error)
        
        deallocate(arr)
        return
    end subroutine
    
    subroutine write_gtm_sed_hdf(nchan, ncell, time_index)
        !args
        integer, intent(in) :: nchan
        integer, intent(in) :: ncell
        integer, intent(in) :: time_index  
        !local
        integer :: ii
        real(gtm_real), allocatable :: mass_total(:,:,:)
        
        allocate(mass_total(ncell,n_zones,2))
        
        mass_total(:,:,:) = sedsolids(:,:,:,1,3) + sedsolids(:,:,:,2,3) + sedsolids(:,:,:,3,3)  !todo:check that rkstep = 3 is correct
        do ii=1,3 
            bed(:,:,:).mass_frac(ii) = sedsolids(:,:,:,ii,3)/mass_total(:,:,:)
        end do
        
        if (hdf_out .eq. 'channel') then
            call write_gtm_chan_sed_hdf( gtm_sed_hdf, bed, nchan, ncell, n_zones, sed_bed_solids_count, time_index)
            call write_gtm_chan_sed_flux_hdf(gtm_sed_hdf, settling, erosion_sb, decomposition, burial, carbonturnover, nchan,ncell, n_zones, sed_bed_solids_flux_count, 3, time_index)
        end if
        
        deallocate(mass_total)
        return
    end subroutine write_gtm_sed_hdf
    
    subroutine write_gtm_chan_sed_hdf(hdf_file, &
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
        type(bed_properties_t), intent(in) :: bed(ncell, nzone, 2)           !< cell data from transport module
        real(gtm_real) :: chan_conc(2,nzone, nchan, nconc)               !< channel data from transport module
        integer, intent(in) :: time_index                          !< time index to write the data
        integer :: chan_rank
        integer(HID_T) :: fspace_id
        integer(HID_T) :: memspace_id
     	integer(HSIZE_T), dimension(4) :: mdata_dims  = 0       ! Dims of data in memory
      	integer(HSIZE_T), dimension(5) :: subset_dims  = 0      ! Dims of subset for time step
    	integer(HSIZE_T), dimension(5) :: h_offset = (/0,0,0,0,0/)
    	integer :: ichan, izone
        integer :: error                                        ! HDF5 Error flag
        integer :: imid
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
                    chan_conc(:,izone,ichan,1) = bed(imid,izone, :).thickness
                    chan_conc(:,izone,ichan,2) = bed(imid,izone, :).mass_frac(1)
                    chan_conc(:,izone,ichan,3) = bed(imid,izone,:).mass_frac(2)
                    chan_conc(:,izone,ichan,4) = bed(imid,izone,:).mass_frac(3)
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
	        call verify_error(error,"Sediment bed solid fractions write")
            call h5sclose_f(fspace_id, error)
            call h5sclose_f(memspace_id, error)      
        end if
        return
    end subroutine

    subroutine write_gtm_chan_sed_flux_hdf(hdf_file,    &
                                        settling,       & 
                                        erosion,        &
                                        decomposition,  &
                                        burial,         &
                                        carbonturnover, &
                                        nchan,          &
                                        ncell,          &
                                        nzone,          &
                                        nconc,          &
                                        nsolids,        &
                                        time_index)
        use hdf5
        use common_variables, only: hdf_out, chan_geom
        implicit none
        type(gtm_sed_hdf_t), intent(in) :: hdf_file                    !< hdf file structure
        real(gtm_real), intent(in)  :: settling(ncell,nzone, nsolids,2)      ! 2 is the number of steps in Huen's method
        real(gtm_real), intent(in)  :: erosion(ncell, nzone, nsolids,2)
        real(gtm_real), intent(in)  :: decomposition(ncell, nzone, 2, nsolids, 2)
        real(gtm_real), intent(in)  :: burial(ncell, nzone,2, nsolids, 2)
        real(gtm_real), intent(in)  :: carbonturnover(ncell, nzone, 2, nsolids, 2)
        integer, intent(in) :: nchan                               !< number of channels
        integer, intent(in) :: ncell                               !< number of cells
        integer, intent(in) :: nconc                               !< number of constituents
        integer, intent(in) :: nzone                               !< number of constituents
        integer, intent(in) :: nsolids                             !< number of constituents
        integer, intent(in) :: time_index                          !< time index to write the data
        real(gtm_real) :: chan_flux(2, nzone, nchan, nconc)        !< channel data from transport module ( nlayers=2,nzones=3, channel, conc)
        integer :: chan_rank
        integer(HID_T) :: fspace_id
        integer(HID_T) :: memspace_id
     	integer(HSIZE_T), dimension(4) :: mdata_dims  = 0       ! Dims of data in memory
      	integer(HSIZE_T), dimension(5) :: subset_dims  = 0      ! Dims of subset for time step
    	integer(HSIZE_T), dimension(5) :: h_offset = (/0,0,0,0,0/)
    	integer :: ichan
        integer :: error                                        ! HDF5 Error flag
        integer :: imid, izone
        integer :: rkstep = 1   !todo: decide on which fluxes to use
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
            
            chan_rank = 4
            
            chan_flux = zero     ! no burial from layer 2
           ! chan_flux = zero     ! no resuspension from layer 2
           ! chan_flux = zero     ! no resuspension from layer 2
            do ichan = 1, nchan             ! overall Huen flux for the previous time step - all three solid types combined
                imid = nint((chan_geom(ichan)%start_cell + chan_geom(ichan)%end_cell)/2.0)  ! write for middle cell in channel
                do izone =1, nzone
                
                    if (bed(imid,izone,1).area_wet > zero) then
                        chan_flux(1,izone,ichan,1) =  chan_flux(1,izone,ichan,1) + (((settling(imid, izone, 1, 1) + settling(imid,izone, 2,1 ) + settling(imid,izone, 3, 1) + settling(imid,izone, 1, 2) + settling(imid,izone, 2, 2) + settling(imid,izone, 3, 2))/two) &
                                             /(bed(imid,izone,1).area_wet )) * day_to_sec
                        chan_flux(1,izone,ichan,2) = chan_flux(1,izone,ichan,2) + (((erosion(imid,izone, 1, 1) + erosion(imid,izone,2, 1) + erosion(imid,izone, 3, 1) + erosion(imid,izone, 1, 2) + erosion(imid,izone, 2, 2) + erosion(imid,izone, 3, 2))/two) &
                                             /(bed(imid,izone,1).area_wet )) * day_to_sec
                        chan_flux(1,izone,ichan,4) = chan_flux(1,izone,ichan,4) + (((burial(imid,izone,1,1,1) + burial(imid,izone,1,2,1) + burial(imid,izone,1,3,1) + burial(imid,izone,1,1,2) + burial(imid,izone,1,2, 2) + burial(imid,izone,1,3,2))/two) &
                                             /(bed(imid,izone,1).area_wet )) * day_to_sec
                    
                        chan_flux(:,izone,ichan,3) = chan_flux(:,izone,ichan,3) + (((decomposition(imid,izone,:,1,1) + decomposition(imid,izone,:,2,1) + decomposition(imid,izone,:,3,1) + decomposition(imid,izone,:,1, 2) + decomposition(imid,izone,:,2, 2) + decomposition(imid,izone,:,3,2))/two) &
                                             /(bed(imid,izone,:).area_wet )) * day_to_sec
                        chan_flux(:,izone,ichan,5) = chan_flux(:,izone,ichan,5) + (((carbonturnover(imid,izone,:,1,1) + carbonturnover(imid,izone,:,2,1) + carbonturnover(imid,izone,:,3,1) + carbonturnover(imid,izone,:,1,2) + carbonturnover(imid,izone,:,2,2) + carbonturnover(imid,izone,:,3,2))/two) &
                                             /(bed(imid,izone,:).area_wet )) * day_to_sec                                       
                    end if
                end do
                chan_flux(2,:,ichan,1) = chan_flux(1,:,ichan,4)   ! settling into layer 2 = burial from layer 1
            end do
              
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
            call h5dwrite_f(hdf_file% bed_out_flux_id,       &
                            H5T_NATIVE_REAL,             &   ! This was H5T_NATIVE_REAL in old DSM2-Qual. Leaving it as REAL will introduce errors.
                            real(chan_flux),             &
                            mdata_dims,                  &
                            error,                       & 
                            memspace_id,                 & 
                            fspace_id)
	        call verify_error(error,"Sediment bed solid fluxes write")
            call h5sclose_f(fspace_id, error)
            call h5sclose_f(memspace_id, error)      
        end if
        return
    end subroutine

    subroutine close_hdf_sed_all()
        call  close_gtm_sed_hdf(gtm_sed_hdf)
        !call  close_gtm_sed_hdf(gtm_sed_flux_hdf)
        return
    end subroutine close_hdf_sed_all
    
     subroutine close_gtm_sed_hdf(hdf_file)
        use hdf5
        use common_variables, only: unit_error,unit_screen, hdf_out
        use common_dsm2_vars, only: print_level

        implicit none
        type(gtm_sed_hdf_t) :: hdf_file
	
        integer        :: error	! HDF5 Error flag
       
        !-------Close the datasets 
        if (print_level .gt.2) write(unit_screen,*)"Closing HDF5 data sets"

        if (hdf_out .eq. 'channel') then
	        call h5dclose_f(hdf_file%bed_out_id,error)
            if (error .ne. 0) then
	            write(unit_error,*)"HDF5 error closing chan conc data set: ",error
            end if  
            call h5dclose_f(hdf_file%bed_out_flux_id,error)
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
        call h5gclose_f(hdf_file%data_id, error)
        if (error .ne. 0) then 
            write(unit_error,*)"HDF5 error closing data group: ",error
        end if
        
        !-------Close the file
 333    if (print_level .gt.1) write(unit_screen,*)"Closing HDF5 file"
	    call h5fclose_f(hdf_file%file_id, error)
        if (error .ne. 0) then
	       write(unit_error,*)"HDF5 error closing hdf file: ",error
        end if

	    return
     end subroutine
     
    subroutine init_bed_geom_hdf()
        use hdf5
        use h5lt
        use ISO_C_BINDING
        !bed geometry in si units
        !args
        type zone_h5_t                                   ! for reading compound hyperslab
            integer:: cellno
            integer:: zoneno
            real(gtm_real) :: elev
            real(gtm_real) :: cell_wet_p
            real(gtm_real) :: zone_wet_p
            real(gtm_real) :: width
        end type
        !local
        integer(HID_T) :: sed_file_id                     ! HDF5 File identifier
        integer(HID_T) :: geom_id
        integer(HID_T) :: dset_id                         ! local dataset identifier
        integer(SIZE_T) :: offset                         ! local variable   
        integer(HSIZE_T), dimension(1) :: data_dims       ! local datasets dimensions
        integer(HSIZE_T), dimension(1) :: dimsm           ! local variable    
        integer(HID_T) :: dt_id                           ! memory datatype identifier
        integer(HID_T) :: dt1_id, dt2_id, dt3_id          ! memory datatype identifier
        integer(HID_T) :: dt4_id, dt5_id, dt6_id          ! memory datatype identifier
        integer(SIZE_T) :: type_size                      ! local variable
              
        logical :: file_exists
        integer :: error
        integer :: ii, jj
        integer :: ncell
        integer :: nzone
        integer :: count
        integer,dimension(1) :: hdf_dummy_integer
        type (zone_h5_t),allocatable :: zone_h5(:)
        real(gtm_real), parameter :: L = 0.3048d0       !ft->m
        
        write(*,*) "Reading sediment bed zone setup file: "
        write(*,*) "  " // file_name_hdf_bed
        inquire(file = file_name_hdf_bed, exist=file_exists)
        if (file_exists) then   
            
            call h5fopen_f (file_name_hdf_bed, H5F_ACC_RDONLY_F, sed_file_id, error)
            call verify_error(error, "opening tidefile")
            call h5ltget_attribute_int_f(sed_file_id, ".",  "n_cell", hdf_dummy_integer, error)
            ncell =  hdf_dummy_integer(1)
            call h5ltget_attribute_int_f(sed_file_id, ".",  "n_zone", hdf_dummy_integer, error)
            nzone =  hdf_dummy_integer(1)
            
            call sed_setup_solids(ncell,nzone,2)
            
            call h5gopen_f(sed_file_id, "geom", geom_id, error)
            call h5dopen_f(geom_id, "zone_info", dset_id, error) 
            
            dimsm = (/nzone * ncell/)
            data_dims(1) = nzone*ncell
            offset = 0     
            allocate (zone_h5(ncell*nzone))
            call h5tcopy_f(H5T_NATIVE_INTEGER, dt_id, error)
            type_size = 4
            call h5tset_size_f(dt_id, type_size, error)
            !call h5tget_size_f(dt_id, type_size, error)
            offset = 0      
            call h5tcreate_f(H5T_COMPOUND_F, type_size, dt1_id, error)
            call h5tinsert_f(dt1_id, "cell_no", offset, dt_id, error)    
            call h5dread_f(dset_id, dt1_id, zone_h5%cellno, data_dims, error)
            
            call h5tcreate_f(H5T_COMPOUND_F, type_size, dt2_id, error)
            call h5tinsert_f(dt2_id, "zone_no", offset, dt_id, error)    
            call h5dread_f(dset_id, dt2_id, zone_h5%zoneno, data_dims, error)
            
            call h5tcreate_f(H5T_COMPOUND_F, gtm_real, dt3_id, error)
            call h5tinsert_f(dt3_id, "elev", offset, H5T_NATIVE_DOUBLE, error)       
            call h5dread_f(dset_id, dt3_id, zone_h5%elev, data_dims, error) 
            
            call h5tcreate_f(H5T_COMPOUND_F, gtm_real, dt3_id, error)
            call h5tinsert_f(dt3_id, "elev", offset, H5T_NATIVE_DOUBLE, error)       
            call h5dread_f(dset_id, dt3_id, zone_h5%elev, data_dims, error) 
            
            call h5tcreate_f(H5T_COMPOUND_F, gtm_real, dt4_id, error)
            call h5tinsert_f(dt4_id, "cell_wet_p", offset, H5T_NATIVE_DOUBLE, error)       
            call h5dread_f(dset_id, dt4_id, zone_h5%cell_wet_p, data_dims, error) 
            
            call h5tcreate_f(H5T_COMPOUND_F, gtm_real, dt5_id, error)
            call h5tinsert_f(dt5_id, "zone_wet_p", offset, H5T_NATIVE_DOUBLE, error)       
            call h5dread_f(dset_id, dt5_id, zone_h5%zone_wet_p, data_dims, error) 
            
            call h5tcreate_f(H5T_COMPOUND_F, gtm_real, dt6_id, error)
            call h5tinsert_f(dt6_id, "width", offset, H5T_NATIVE_DOUBLE, error)       
            call h5dread_f(dset_id, dt6_id, zone_h5%width, data_dims, error) 
                      
            call h5dclose_f(dset_id, error)
            call h5tclose_f(dt6_id, error)
            
            call h5dopen_f(geom_id, "cell_info", dset_id, error) 
             data_dims(1) = ncell
            call h5tcreate_f(H5T_COMPOUND_F, gtm_real, dt6_id, error)
            call h5tinsert_f(dt6_id, "length", offset, H5T_NATIVE_DOUBLE, error)       
            call h5dread_f(dset_id, dt6_id, length, data_dims, error) 
            
            call h5gclose_f(geom_id, error)           
            call h5dclose_f(dset_id, error)
            call h5fclose_f(sed_file_id, error)
           
            
            call h5tclose_f(dt_id, error)
            call h5tclose_f(dt1_id, error)
            call h5tclose_f(dt2_id, error)
            call h5tclose_f(dt3_id, error)
            call h5tclose_f(dt4_id, error)
            call h5tclose_f(dt5_id, error)
            call h5tclose_f(dt6_id, error)
            length=length*L
            count = 0
            do ii=1,ncell
                do jj=1,nzone
                    count = count+1
                    bed(zone_h5(count)%cellno,zone_h5(count)%zoneno,1)%area_cell = zone_h5(count)%width* length(zone_h5(count)%cellno)*L
                    bed(zone_h5(count)%cellno,zone_h5(count)%zoneno,1)%wp_cell = zone_h5(count)% cell_wet_p*L
                    bed(zone_h5(count)%cellno,zone_h5(count)%zoneno,1)%wp_zone = zone_h5(count)% zone_wet_p*L
                    if (zone_h5(count)%zoneno > 1) then
                        bed(zone_h5(count)%cellno,zone_h5(count)%zoneno,1)%area_zone = ((bed(zone_h5(count)%cellno,zone_h5(count)%zoneno,1)%area_cell &
                                - bed(zone_h5(count)%cellno,zone_h5(count)%zoneno -1 ,1)%area_cell))!*L*L
                    else
                        bed(zone_h5(count)%cellno,zone_h5(count)%zoneno,1)%area_zone = (bed(zone_h5(count)%cellno,zone_h5(count)%zoneno,1)%area_cell)!*L*L
                    end if
                end do
                !bed(zone_h5(count)%cellno,:,1)%volume = bed(zone_h5(count)%cellno,:,1)%wp_zone*length(zone_h5(count)%cellno) ! still needs thickness
            end do
            bed(:,:,2)%area_cell = bed(:,:,1)%area_cell
            bed(:,:,2)%area_zone = bed(:,:,1)%area_zone
            bed(:,:,2)%wp_cell = bed(:,:,1)%wp_cell
            bed(:,:,2)%wp_zone = bed(:,:,1)%wp_zone
            deallocate(zone_h5)
        endif

    end subroutine
    
    subroutine write_channel_labels(loc_id, nchan)
        use hdf5
        use common_variables, only: chan_geom,hdf_out !resv_geom, constituents, 
        implicit none
        !args
        integer (HID_T), intent(in) :: loc_id              !< hdf file data ID
        integer, intent(in) :: nchan                       !< nbumber of channels
        !local
        integer(HSIZE_T), dimension(1)  :: in_dims != (/0/) ! Dataset dimensions
        integer(HID_T)                  :: in_dspace_id     ! Dataspace identifier
        integer(HID_T)                  :: in_dset_id       ! Dataspace identifier
        integer                         :: in_rank = 1      ! Dataset rank
        Integer(HID_T) :: cparms                             ! dataset creatation property identifier
        integer                         :: ierror
        
        if (hdf_out .eq. 'channel') then
                     
            ! Write out external channel numbers int2ext
            call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, ierror)
            in_dims(1) = nchan
            call h5screate_simple_f(in_rank, in_dims, in_dspace_id, ierror)
            call h5dcreate_f(loc_id, "channel_number", H5T_NATIVE_INTEGER,   &
                             in_dspace_id, in_dset_id, ierror, cparms)
            call h5dwrite_f(in_dset_id,H5T_NATIVE_INTEGER, chan_geom%channel_num, in_dims, ierror)
            call h5sclose_f (in_dspace_id, ierror)
            call h5dclose_f(in_dset_id,ierror)
        end if
    end subroutine write_channel_labels
    
    subroutine sed_setup_solids(ncells,nzones,layers)
        integer, intent (in)            :: ncells
        integer, intent (in)            :: nzones
        integer, intent (in)            :: layers
        allocate (bed(ncells,nzones, layers))
        bed(:,:,2).inter_frac = zero
        bed(:,:,2).inter_frac_max = zero
        return
    end subroutine sed_setup_solids
    
end module 