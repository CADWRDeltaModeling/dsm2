

!> Module to write GTM simulation results to HDF5 file. 
!> This module was mainly borrowed from DSM2-Qual with several 
!> modifications to make it stand-alone, testable and in 
!> fortran 90 style. 
!>@ingroup process_io
module gtm_hdf_ts_write

    use hdf5, only: HID_T, HSIZE_T
    use gtm_precision
    use error_handling      
    
    type qual_hdf_t 
        character*128 file_name
        real(gtm_real) :: write_interval
        real(gtm_real) :: start_julmin
        real(gtm_real) :: time_index  
        integer(HID_T) :: file_id
        integer(HID_T) :: qual_id
        integer(HID_T) :: data_id
        integer(HID_T) :: cell_conc_id
        integer(HID_T) :: resv_conc_id
        integer(HID_T) :: cell_flow_id
        integer(HID_T) :: cell_area_id
        integer(HID_T) :: cell_cfl_id        
        integer(HSIZE_T) :: conc_dim
        integer(HSIZE_T) :: cell_dim
        integer(HSIZE_T) :: resv_dim
        integer(HSIZE_T) :: time_dim
    end type
      
    type(qual_hdf_t) :: qual_hdf
    
    logical :: using_qual_hdf = .false.
    integer, parameter :: HDF_SZIP_PIXELS_PER_BLOCK = 16
    integer, parameter :: TIME_CHUNK = HDF_SZIP_PIXELS_PER_BLOCK
    integer, parameter :: MIN_STEPS_FOR_CHUNKING = TIME_CHUNK

    contains
      
    !< Initialize the qual output file, assume HDF5 interface is opne
    subroutine init_qual_hdf(hdf_file,          &
                             hdf_name,          &
                             ncell,             &
                             nresv,             &
                             nconc,             &
                             sim_start,         &
                             sim_end,           &
                             hdf_interval_char)
 
   	    use hdf5		! HDF5 This module contains all necessary modules
        use common_variables, only: unit_error, unit_screen, gtm_time_interval, debug_print
        use dsm2_time_utils, only: incr_intvl
        use common_dsm2_vars, only: NEAREST_BOUNDARY, TO_BOUNDARY, print_level
  	    implicit none
        type(qual_hdf_t), intent(inout) :: hdf_file   !< persistent info about file and datasets
        character*128, intent(in) :: hdf_name         !< name of qual hdf5 file
        integer, intent(in) :: ncell                  !< number of cells
        integer, intent(in) :: nresv                  !< number of reservoirs
        integer, intent(in) :: nconc                  !< number of constituents
        integer, intent(in) :: sim_start       !< first write time
        integer, intent(in) :: sim_end         !< last write time
        character*16 :: hdf_interval_char             !< interval
            
        !----- locals      
        integer :: hdf_start
        integer :: hdf_end
        integer :: hdf_interval
        integer :: time_intv
        integer :: ntime                            ! number of time points in hdf5 file
        real(gtm_real) :: time_step                 ! gtm simulation time step
  	    integer(HID_T) :: access_plist              ! Dataset trasfer property
	    integer(SIZE_T) :: rddc_nelmts
	    integer(SIZE_T) :: rddc_nbytes
	    integer :: nelmts
	    real :: rdcc_w0
        logical :: h5_file_exists
	    integer :: error	                        ! HDF5 Error flag

        time_step = gtm_time_interval

	    ! check to see if file exists
     	inquire (file=hdf_name, exist=h5_file_exists)

    	if (h5_file_exists) then
	         write(unit_error,920) trim(hdf_name)
 920         format(' File already exists... deleting existing file :: ', a )
	    endif

        hdf_file%file_name = hdf_name
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
	    call h5fcreate_f(hdf_name, H5F_ACC_TRUNC_F, hdf_file%file_id, error, &     
                         H5P_DEFAULT_F, access_plist)
      
        ! create group for output
	    call h5gcreate_f(hdf_file%file_id, "output", hdf_file%data_id, error)
	
	    call incr_intvl(hdf_interval, 0,hdf_interval_char,TO_BOUNDARY)
	    qual_hdf%write_interval = hdf_interval
	    if (hdf_interval < time_step) then
	        write(unit_error,*) "HDF write interval is finer than the simulation time step"
	        call exit(-3)
	    end if
	
	    ! This would be more complex if time averages were stored
        call incr_intvl(hdf_start, sim_start, hdf_interval_char, NEAREST_BOUNDARY)
        qual_hdf%start_julmin = hdf_start

        ! todo: is this "1+" always right? It wasn't in the original code      
        ! record the dimensions of the simulation
	    ntime = 1+(sim_end - hdf_start)/hdf_interval
        hdf_end = hdf_start + (ntime-1)*hdf_interval

        hdf_file%cell_dim = ncell
        hdf_file%conc_dim = nconc
        hdf_file%time_dim = ntime
	    hdf_file%resv_dim = nresv
	    hdf_file%time_index = 1
	
	    call write_dimensions(hdf_file%data_id, nresv, nconc)
	
	    ! create the data sets for time-varying output
	    call init_cell_qual_hdf5(hdf_file, ncell, nconc, ntime)
	    if (debug_print .eq. .true.) then
	        call init_cell_qual_hdf5_debug(hdf_file, ncell, ntime)	    
	    end if
	    if (hdf_file%resv_dim .gt. 0)then
	        call init_reservoir_qual_hdf5(hdf_file, nresv, nconc, ntime)
	    end if

        using_qual_hdf = .true.
        
	    return
	end subroutine      


    !> Write out lookup information for cells, reservoirs, and constituents. 
    subroutine write_dimensions(loc_id, nresv, nconc)
        use hdf5
        use common_variables, only: chan_geom, resv_geom, constituents
        implicit none
        integer (HID_T), intent(in) :: loc_id              !< hdf file data ID
        integer, intent(in) :: nresv                       !< number of reservoirs
        integer, intent(in) :: nconc                       !< number of constituents
        integer(HSIZE_T), dimension(1) :: in_dims != (/0/) ! Dataset dimensions
        integer(HID_T) :: in_dspace_id                     ! Dataspace identifier
        integer(HID_T) :: in_dset_id                       ! Dataspace identifier
        integer     ::    in_rank = 1                      ! Dataset rank
        integer(HID_T) :: cparms                           ! dataset creatation property identifier
        integer(HID_T) :: cg_dspace_id                     ! Dataspace identifier
        integer     ::    cg_rank = 2                      ! Dataset rank
        integer(HSIZE_T), dimension(7) :: cg_data_dims
        integer(HID_T) :: memspace                         ! memspace identifier

        integer, parameter :: label_len = 12
	    integer, parameter :: name_len = 32
        character(LEN=name_len),dimension(:), allocatable :: names
        real(gtm_real) :: tmp
        integer :: i
        integer :: ierror
       
  	    ! Write reservoir names
  	    if (nresv.gt.0) then
            in_dims(1) = nresv
            allocate(names(nresv))
	        names = ' '
	        do i = 1, nresv
	            names(i) = resv_geom(i)%name 
            end do
	        call write_1D_string_array(loc_id,"reservoir_names",names,    &     
                                       name_len, nresv)
            deallocate(names)     
        end if
        
        ! Write constituent names
        if (nconc.gt.0) then
  	        in_dims(1) = max(1,nconc)
            allocate(names(max(1,nconc)))
	        names = ' '
	        do i = 1,max(1,nconc)
	            names(i) = constituents(i)%name 
            end do
	        call write_1D_string_array(loc_id,"constituent_names", names,  &     
                                       name_len,max(1,nconc))
            deallocate(names) 
        end if    
        return
    end subroutine

    !> Initialize qual tide file for cell time series (ncell)
	subroutine init_cell_qual_hdf5_debug(hdf_file, ncell, ntime)

	    use hdf5
	    use time_utilities, only: jmin2cdt
	    implicit none
	    
        type(qual_hdf_t), intent(inout) :: hdf_file       !< hdf file properties
	    integer, intent(in) :: ntime                      !< number of time steps
	    integer, intent(in) :: ncell                      !< number of cells
   	    integer(HID_T) :: attr_id                         ! Attribute identifier 
        integer(HID_T) :: aspace_id                       ! Attribute dataspace identifier 
        integer(HID_T) :: atype_id                        ! Attribute type identifier 
        integer(HSIZE_T), dimension(1) :: adims = (/1/)   ! Attribute dimension
        integer     ::   arank = 1                        ! Attribute rank
        integer(HSIZE_T), dimension(7) :: a_data_dims
        integer     ::   cell_rank = 0
   	    integer(HSIZE_T), dimension(2) :: cell_file_dims  = 0 ! Data size on file
	    integer(HSIZE_T), dimension(2) :: cell_chunk_dims = 0 ! Dataset chunk dimensions

	    integer(HID_T) :: fspace_flow_id                  ! File space identifier
	    integer(HID_T) :: fspace_area_id                  ! File space identifier
	    integer(HID_T) :: fspace_cfl_id                   ! File space identifier	    
	    
	    integer(HID_T) :: cparms                          ! dataset creatation property identifier 
	    integer        :: error                           ! HDF5 Error flag
	    
	    call h5screate_simple_f(arank, adims, aspace_id, error)
	    call h5tcopy_f(H5T_NATIVE_INTEGER, atype_id, error)

		! Create cell data set, one data point per cell
        cell_rank = 2
        cell_file_dims(1) = ncell
	    cell_file_dims(2) = ntime
      
	    cell_chunk_dims(1) = ncell
	    cell_chunk_dims(2) = min(TIME_CHUNK,ntime)
	
	    cparms = 0
		! Add chunking and compression
	    call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, error)
        if (ntime.gt. MIN_STEPS_FOR_CHUNKING) then
          call h5pset_chunk_f(cparms,                   &    
                              cell_rank,                &      
                              cell_chunk_dims,          &     
                              error)
	      call H5Pset_szip_f (cparms, H5_SZIP_NN_OM_F,  &     
                              HDF_SZIP_PIXELS_PER_BLOCK, error);
	    end if
       
        ! initialize table for cell flow
	    call h5screate_simple_f(cell_rank,              &   
                                cell_file_dims,         &     
                                fspace_flow_id,         &    
                                error)
	    call h5dcreate_f(hdf_file%data_id,              &  
                         "cell flow",                   & 
                         H5T_NATIVE_DOUBLE,             &	   
                         fspace_flow_id,                &
                         hdf_file%cell_flow_id,         &    
                         error,                         &     
                         cparms)                      
	    call verify_error(error,"Cell flow dataset creation")
        call add_timeseries_attributes(hdf_file%cell_flow_id,   &  
                                       hdf_file%start_julmin,   &
                                       hdf_file%write_interval)                                                                             
                                       
        ! initialize table for cell area
	    call h5screate_simple_f(cell_rank,              &   
                                cell_file_dims,         &     
                                fspace_area_id,         &    
                                error)
	    call h5dcreate_f(hdf_file%data_id,              &  
                         "cell area",                   &    
                         H5T_NATIVE_DOUBLE,             &	   
                         fspace_area_id,                &
                         hdf_file%cell_area_id,         &    
                         error,                         &     
                         cparms)                      
	    call verify_error(error,"Cell area dataset creation")
        call add_timeseries_attributes(hdf_file%cell_area_id,   &  
                                       hdf_file%start_julmin,   &
                                       hdf_file%write_interval)                                        

        ! initialize table for cell cfl number
	    call h5screate_simple_f(cell_rank,              &   
                                cell_file_dims,         &     
                                fspace_cfl_id,          &    
                                error)
	    call h5dcreate_f(hdf_file%data_id,              &  
                         "cell cfl",                    &    
                         H5T_NATIVE_DOUBLE,             &	   
                         fspace_cfl_id,                 &
                         hdf_file%cell_cfl_id,          &    
                         error,                         &     
                         cparms)                      
	    call verify_error(error,"Cell CFL dataset creation")
        call add_timeseries_attributes(hdf_file%cell_cfl_id,    &  
                                       hdf_file%start_julmin,   &
                                       hdf_file%write_interval)       
	    return
	end subroutine


    !> Initialize qual tide file for cell time series (ncell, nvar)
	subroutine init_cell_qual_hdf5(hdf_file, ncell, nconc, ntime)

	    use hdf5
	    use time_utilities, only: jmin2cdt
	    implicit none
	    
        type(qual_hdf_t), intent(inout) :: hdf_file       !< hdf file properties
	    integer, intent(in) :: ntime                      !< number of time steps
	    integer, intent(in) :: ncell                      !< number of cells
        integer, intent(in) :: nconc	                  !< number of constituents
   	    integer(HID_T) :: attr_id                         ! Attribute identifier 
        integer(HID_T) :: aspace_id                       ! Attribute dataspace identifier 
        integer(HID_T) :: atype_id                        ! Attribute type identifier 
        integer(HSIZE_T), dimension(1) :: adims = (/1/)   ! Attribute dimension
        integer     ::   arank = 1                        ! Attribute rank
        integer(HSIZE_T), dimension(7) :: a_data_dims
        integer     ::   cell_rank = 0
   	    integer(HSIZE_T), dimension(3) :: cell_file_dims  = 0 ! Data size on file
	    integer(HSIZE_T), dimension(3) :: cell_chunk_dims = 0 ! Dataset chunk dimensions

	    integer(HID_T) :: fspace_id                       ! File space identifier
	    integer(HID_T) :: cparms                          ! dataset creatation property identifier 
	    integer        :: error                           ! HDF5 Error flag
	    
	    call h5screate_simple_f(arank, adims, aspace_id, error)
	    call h5tcopy_f(H5T_NATIVE_INTEGER, atype_id, error)

		! Create cell data set, one data point per cell
        cell_rank = 3
        cell_file_dims(1) = nconc
        cell_file_dims(2) = ncell
	    cell_file_dims(3) = ntime
      
        cell_chunk_dims(1) = nconc
	    cell_chunk_dims(2) = ncell
	    cell_chunk_dims(3) = min(TIME_CHUNK,ntime)
	
	    cparms = 0
		! Add chunking and compression
	    call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, error)
        if (ntime.gt. MIN_STEPS_FOR_CHUNKING) then
          call h5pset_chunk_f(cparms,                   &    
                              cell_rank,                &      
                              cell_chunk_dims,          &     
                              error)
	      call H5Pset_szip_f (cparms, H5_SZIP_NN_OM_F,  &     
                              HDF_SZIP_PIXELS_PER_BLOCK, error);
	    end if
       
        ! initialize table for cell concentration
	    call h5screate_simple_f(cell_rank,              &   
                                cell_file_dims,         &     
                                fspace_id,              &    
                                error)
	    call h5dcreate_f(hdf_file%data_id,              &  
                         "cell concentration",          &    
                         H5T_NATIVE_DOUBLE,             &	   
                         fspace_id,                     &
                         hdf_file%cell_conc_id,         &    
                         error,                         &     
                         cparms)                      
	    call verify_error(error,"Cell concentration dataset creation")
        call add_timeseries_attributes(hdf_file%cell_conc_id,   &  
                                       hdf_file%start_julmin,   &
                                       hdf_file%write_interval)                                                                             
	    return
	end subroutine


	!> Initialize qual tide file for reservoir time series 
	subroutine init_reservoir_qual_hdf5(hdf_file, nresv, nconc, ntime)
	    use hdf5
        implicit none
        type(qual_hdf_t), intent(inout) :: hdf_file
        integer(HID_T) :: cparms          ! dataset creation property identifier 
        integer        :: error	          ! HDF5 Error flag
        integer        :: res_rank = 3
        integer(HSIZE_T), dimension(3) :: chunk_dims = 0 ! Dataset dimensions
        integer        :: ntime           ! number of time points in tidefile
        integer        :: nresv
        integer        :: nconc
        integer(HSIZE_T), dimension(3) :: file_dims  = 0 ! Data size on file      
	    integer(HID_T) :: fspace_id       ! File space identifier
	    
        !-------Create the datasets
        res_rank = 3
	    file_dims(1) = nconc        
        file_dims(2) = nresv
	    file_dims(3) = ntime

        chunk_dims(1) = nconc
	    chunk_dims(2) = nresv
	    chunk_dims(3) = min(TIME_CHUNK,ntime)

		! Add chunking and compression
	    call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, error)
	    if (ntime .gt. MIN_STEPS_FOR_CHUNKING) then
	        call h5pset_chunk_f(cparms, res_rank, chunk_dims, error)
	        call H5Pset_szip_f (cparms, H5_SZIP_NN_OM_F,           &  
                                HDF_SZIP_PIXELS_PER_BLOCK, error);
        end if

	    call h5screate_simple_f(res_rank,                       &  
                                file_dims,                      &  
                                fspace_id,                      &  
                                error)
	    call h5dcreate_f(hdf_file%data_id,                      & 
                         "reservoir concentration",             & 
                         H5T_NATIVE_DOUBLE,                     & 
                         fspace_id,                             &  
                         hdf_file%resv_conc_id,                 &
                         error,                                 &  
                         cparms)
        call add_timeseries_attributes(hdf_file%resv_conc_id,   &      
                                       hdf_file%start_julmin,   &
                                       hdf_file%write_interval)

        return
	end subroutine


    !> Write flow/area time series data to Qual tidefile (dimension cell)
    subroutine write_qual_hdf_ts(data_id,       &
                                 cell_ts,       & 
                                 ncell,         &
                                 time_index)
        use hdf5
        implicit none
        integer, intent(in) :: data_id                          !< data id
        integer, intent(in) :: ncell                            !< number of cells
        real(gtm_real), intent(in) :: cell_ts(ncell)            !< cell data from transport module
        integer, intent(in) :: time_index                       !< time index to write the data
        integer :: cell_rank
        integer(HID_T) :: fspace_id
        integer(HID_T) :: memspace_id
     	integer(HSIZE_T), dimension(1) :: mdata_dims  = 0       ! Dims of data in memory
      	integer(HSIZE_T), dimension(2) :: subset_dims  = 0      ! Dims of subset for time step
    	integer(HSIZE_T), dimension(2) :: h_offset = (/0,0/)
    	integer :: i, j
        integer :: error                                        ! HDF5 Error flag
        if (mod(time_index,24*10) .eq. 1) call h5garbage_collect_f(error)
     
        if (ncell .ne. 0) then
     
            !-----cell conc
            h_offset(1) = 0
            h_offset(2) = time_index

	        subset_dims(1) = ncell
	        subset_dims(2) = 1

	        mdata_dims(1) = ncell
            cell_rank =1
            call H5Screate_simple_f(cell_rank,           &
                                    mdata_dims,          &
                                    memspace_id,         &
                                    error);  
            call h5dget_space_f (data_id,                &  
                                 fspace_id,              &
                                 error)
            call h5sselect_hyperslab_f(fspace_id,        &
                                       H5S_SELECT_SET_F, &
                                       h_offset,         & 
                                       subset_dims,      &
                                       error)
            call h5dwrite_f(data_id,                     &
                            H5T_NATIVE_DOUBLE,           &   ! This was H5T_NATIVE_REAL in old DSM2-Qual. Leaving it as REAL will introduce errors.
                            cell_ts,                     &
                            mdata_dims,                  &
                            error,                       &
                            memspace_id,                 & 
                            fspace_id)
	        call verify_error(error,"Cell time series write (write_qual_hdf_ts)")
            call h5sclose_f (fspace_id, error)
            call h5sclose_f (memspace_id, error)      
        end if
        return
    end subroutine


    !> Write time series data to Qual tidefile (dimension cell)
    subroutine write_qual_hdf(hdf_file,      &
                              cell_conc,     & 
                              ncell,         &
                              nconc,         &  
                              time_index)
        use hdf5
        implicit none
        type(qual_hdf_t), intent(in) :: hdf_file                !< hdf file structure
        integer, intent(in) :: ncell                            !< number of cells
        integer, intent(in) :: nconc                            !< number of constituents
        real(gtm_real), intent(in) :: cell_conc(ncell, nconc)   !< cell data from transport module
        integer, intent(in) :: time_index                       !< time index to write the data
        integer :: cell_rank
        integer(HID_T) :: fspace_id
        integer(HID_T) :: memspace_id
     	integer(HSIZE_T), dimension(2) :: mdata_dims  = 0       ! Dims of data in memory
      	integer(HSIZE_T), dimension(3) :: subset_dims  = 0      ! Dims of subset for time step
    	integer(HSIZE_T), dimension(3) :: h_offset = (/0,0,0/)
        real(gtm_real) ::  cell_conc_hdf(nconc, ncell)          ! transposed chan data to write
    	integer :: i, j
        integer :: error                                        ! HDF5 Error flag
        if (mod(time_index,24*10) .eq. 1) call h5garbage_collect_f(error)
     
        if (ncell .ne. 0) then
            !----transpose array
            do i = 1, nconc
                do j = 1, ncell
                    cell_conc_hdf(i,j) = cell_conc(j,i)
                end do
            enddo        
     
            !-----cell conc
            h_offset(1) = 0
            h_offset(2) = 0
            h_offset(3) = time_index

	        subset_dims(1) = nconc
	        subset_dims(2) = ncell
	        subset_dims(3) = 1

	        mdata_dims(1) = nconc
	        mdata_dims(2) = ncell
            cell_rank = 2
            call H5Screate_simple_f(cell_rank,           &
                                    mdata_dims,          &
                                    memspace_id,         &
                                    error);  
            call h5dget_space_f (hdf_file%cell_conc_id,  &  
                                 fspace_id,              &
                                 error)
            call h5sselect_hyperslab_f(fspace_id,        &
                                       H5S_SELECT_SET_F, &
                                       h_offset,         & 
                                       subset_dims,      &
                                       error)
            call h5dwrite_f(hdf_file%cell_conc_id,       &
                            H5T_NATIVE_DOUBLE,           &   ! This was H5T_NATIVE_REAL in old DSM2-Qual. Leaving it as REAL will introduce errors.
                            cell_conc_hdf,               &
                            mdata_dims,                  &
                            error,                       &
                            memspace_id,                 & 
                            fspace_id)
	        call verify_error(error,"Cell concentration write")
            call h5sclose_f (fspace_id, error)
            call h5sclose_f (memspace_id, error)      
        end if
        return
    end subroutine

   
    !> Write time series data to Qual tidefile (dimension reservoir)
    subroutine write_qual_hdf_resv(hdf_file,      & 
                                   resv_conc,     & 
                                   nresv,         &
                                   nconc,         &  
                                   time_index)
        use hdf5
        implicit none
        type(qual_hdf_t), intent(in) :: hdf_file                !< hdf file structure
        integer, intent(in) :: nresv                            !< number of reservoirs
        integer, intent(in) :: nconc                            !< number of constituents
        real(gtm_real), intent(in) :: resv_conc(nresv, nconc)   !< resv data from transport module
        integer, intent(in) :: time_index                       !< time index to write the data
        integer :: resv_rank
        integer(HID_T) :: fspace_id
        integer(HID_T) :: memspace_id
     	integer(HSIZE_T), dimension(2) :: mdata_dims  = 0       ! Dims of data in memory
      	integer(HSIZE_T), dimension(3) :: subset_dims  = 0      ! Dims of subset for time step
    	integer(HSIZE_T), dimension(3) :: h_offset = (/0,0,0/)
        real(gtm_real) ::  resv_conc_hdf(nconc, nresv)          ! transposed res data to write 
    	integer :: i, j
        integer :: error                                        ! HDF5 Error flag
        if (mod(time_index,24*10) .eq. 1) call h5garbage_collect_f(error)
     
        if (nresv .ne. 0) then
            !----transpose array
            do i = 1, nconc
                do j = 1, nresv
                    resv_conc_hdf(i,j) = resv_conc(j,i)
                end do
            enddo        
     
            !-----reservoir conc      
            h_offset(1) = 0
            h_offset(2) = 0
            h_offset(3) = time_index
            subset_dims(1) = nconc
	        subset_dims(2) = nresv
	        subset_dims(3) = 1
   	        mdata_dims(1) = nconc
	        mdata_dims(2) = nresv 
	        resv_rank = 2
            call H5Screate_simple_f(resv_rank,           &
                                    mdata_dims,          & 
                                    memspace_id,         &
                                    error); 
            call h5dget_space_f (hdf_file%resv_conc_id,  &
                                 fspace_id,              &
                                 error)
            call h5sselect_hyperslab_f(fspace_id,        &
                                       H5S_SELECT_SET_F, & 
                                       h_offset,         & 
                                       subset_dims,      & 
                                       error) 
            call h5dwrite_f(hdf_file%resv_conc_id,       &
                            H5T_NATIVE_DOUBLE,           &   
                            resv_conc_hdf,               &
                            mdata_dims,                  & 
                            error,                       &
                            memspace_id,                 &
                            fspace_id)
	        call verify_error(error,"Reservoir concentration write")
            call h5sclose_f (fspace_id, error)
            call h5sclose_f (memspace_id, error) 
        end if
        return
    end subroutine

 
    !> Close out the HDF5 file properly, leaves HDF5 API open
    subroutine close_qual_hdf(hdf_file)
        use hdf5
        use common_variables, only: unit_error,unit_screen
        use common_dsm2_vars, only: print_level

        implicit none
        type(qual_hdf_t) :: hdf_file
	
        integer        :: error	! HDF5 Error flag
       
        !-------Close the datasets corresponding to model states
        if (print_level .gt.2) write(unit_screen,*)"Closing HDF5 data sets"

	    call h5dclose_f(hdf_file%cell_conc_id,error)
        if (error .ne. 0) then
	        write(unit_error,*)"HDF5 error closing cell conc data set: ",error
	    end if

        if (hdf_file%resv_dim.gt.0) then
	        call h5dclose_f(hdf_file%resv_conc_id,error)
	        if (error .ne. 0) then
	            write(unit_error,*)"HDF5 error closing reservoir conc data set: ",error
	        end if
	    end if    

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
      
      
    !> Query if it is time to write Qual state to HDF5      
    logical function is_qual_hdf_write_interval(julmin)
        implicit none
        real(gtm_real), intent(in) :: julmin
        is_qual_hdf_write_interval = ( julmin .ge. qual_hdf.start_julmin .and.     &
                                     mod(julmin, qual_hdf.write_interval) .eq. 0)
        return
    end function
        

    !> Write one dimensional string array to dataset
    subroutine write_1D_string_array(dest_id, name, arr, strlen, nstr)
    
        use hdf5
        implicit none
        integer(HID_T),intent(in) :: dest_id    !< Destination group identifier      
        character*(*), intent(in) :: name       !< Name of array
  	    integer, intent(in)       :: strlen     !< Length of string element
	    integer, intent(in)       :: nstr       !< Dimension of string array
	    character(len=strlen),dimension(nstr), intent(in) :: arr    !< string array

        integer(HID_T) :: dspace_id     ! Dataspace identifier
        integer(HID_T) :: dtype_id      ! Dataspace identifier
        integer(HID_T) :: dset_id       ! Dataspace identifier
        integer, parameter :: drank = 1 ! Dataset rank
        integer, parameter :: arank = 1 ! Attribute rank
        integer :: error                ! HDF5 Error flag
        integer(HSIZE_T), dimension(arank) :: data_dims
        
	    data_dims(1) = nstr
        call h5tcopy_f(H5T_NATIVE_CHARACTER, dtype_id, error)
        call h5tset_size_f(dtype_id, strlen, error)
	    call h5screate_simple_f(drank, data_dims, dspace_id, error)
	    call h5dcreate_f(dest_id,trim(name),dtype_id, dspace_id, dset_id, error)
        call h5dwrite_f(dset_id,dtype_id, arr(1), data_dims, error)
        call h5tclose_f(dtype_id,error)
        call h5sclose_f(dspace_id,error)
        call h5dclose_f(dset_id,error)
        
        return
	end subroutine


    !> add time series attributes to tidefile
	subroutine add_timeseries_attributes(dset_id, ts_start, ts_interval)

	    use hdf5
        use time_utilities, only: jmin2iso
        implicit none
        real(gtm_real) :: ts_start
        real(gtm_real) :: ts_interval
        character*30 :: cinterval
   	    integer(HID_T) :: dset_id ! Attribute identifier 
        integer(HID_T) :: aspace_id ! Attribute Dataspace identifier 
	    integer(HID_T) :: atype_id
	    integer(HID_T) :: attr_id
	    integer        :: error	! HDF5 Error flag
	    integer :: nlen
        character(LEN=32) :: buffer
	    integer(HSIZE_T), dimension(1) :: a_dims = (/1/)
        integer     ::   arank = 1                      ! Attribure rank

	    integer*4, parameter :: ISO_LEN = 19  ! includes null termination
        character(LEN=ISO_LEN) :: iso_datetime

        write(cinterval,"(f,'min')") ts_interval
        cinterval=adjustl(cinterval)

        call h5screate_simple_f(arank, a_dims, aspace_id, error)
        call verify_error(error,"time series attributes dataspace")
        call h5tcopy_f(H5T_NATIVE_CHARACTER, atype_id, error)
  	    buffer = 'TIMESERIES'
	    nlen = len_trim(buffer)
        call h5tset_size_f(atype_id, nlen, error)
	    call h5acreate_f(dset_id, "CLASS",                      &
                         atype_id, aspace_id, attr_id, error)
	    call h5awrite_f(attr_id, atype_id, trim(buffer),        &
                        a_dims, error)
	    call h5aclose_f(attr_id,error)

	    iso_datetime=jmin2iso(int(ts_start))
        call h5tset_size_f(atype_id, ISO_LEN, error)
	    call h5acreate_f(dset_id, "start_time",                 &
                         atype_id, aspace_id, attr_id, error)
 	    call h5awrite_f(attr_id, atype_id, iso_datetime(1:19),  &
                        a_dims, error)
	    call h5aclose_f(attr_id,error)

        nlen = len_trim(cinterval)
        call h5tset_size_f(atype_id, nlen, error)
	    call h5acreate_f(dset_id, "interval",                   &
                         atype_id, aspace_id, attr_id, error)
	    call h5awrite_f(attr_id, atype_id,                      &
                        cinterval, a_dims, error)
	    call h5aclose_f(attr_id,error) 
	    
        call h5tclose_f(atype_id,error)
	    call h5sclose_f(aspace_id,error)

        return
	end subroutine
    
    
end module        