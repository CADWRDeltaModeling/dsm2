

!> Module to write GTM simulation results to HDF5 file. 
!> This module was mainly borrowed from DSM2-Qual with several 
!> modifications to make it stand-alone, testable and in 
!> fortran 90 style. 
!>@ingroup process_io
module gtm_hdf_write

    use hdf5, only: HID_T, HSIZE_T
    use gtm_precision
    use error_handling      
    
    type qual_hdf_t 
        character*128 file_name
        integer        :: write_interval
        integer        :: start_julmin
        integer        :: time_index  
        integer(HID_T) :: file_id
        integer(HID_T) :: qual_id
        integer(HID_T) :: data_id
        integer(HID_T) :: chan_conc_id
        integer(HID_T) :: res_conc_id
        integer(HSIZE_T) :: conc_dim
        integer(HSIZE_T) :: channel_dim
        integer(HSIZE_T) :: res_dim
        integer(HSIZE_T) :: time_dim
    end type
      
    type(qual_hdf_t) :: qual_hdf
    
    logical :: using_qual_hdf = .false.
    integer, parameter :: HDF_SZIP_PIXELS_PER_BLOCK = 16
    integer, parameter :: TIME_CHUNK = HDF_SZIP_PIXELS_PER_BLOCK
    integer, parameter :: MIN_STEPS_FOR_CHUNKING = TIME_CHUNK

    contains
      
    !< Initialize the qual output file
    subroutine init_qual_hdf(hdf_file,          &
                             hdf_name,          &
                             nchannel,          &
                             nres,              &
                             nconc,             &
                             sim_start,         &
                             sim_end,           &
                             hdf_interval_char)
 
   	    use hdf5		! HDF5 This module contains all necessary modules
        use common_variables, only: unit_error, unit_screen, gtm_time_interval
        use time_utilities, only: incr_intvl
        use common_dsm2_vars, only: NEAREST_BOUNDARY, TO_BOUNDARY, print_level
  	    implicit none
        type(qual_hdf_t), intent(inout) ::hdf_file  ! persistent info about file and datasets
        character*128, intent(in) :: hdf_name       ! name of qual hdf5 file
        integer :: sim_start                        ! first write time
        integer :: sim_end                          ! last write time
        character*16 :: hdf_interval_char           ! interval
        integer :: nchannel
        integer :: nres
        integer :: nconc
            
        !----- locals      
        integer :: hdf_start
        integer :: hdf_end
        integer :: hdf_interval
        integer :: ntime                            ! number of time points in hdf5 file
        integer :: time_step                        ! gtm simulation time step

  	    integer(HID_T) :: access_plist ! Dataset trasfer property

	    integer(SIZE_T) :: rddc_nelmts
	    integer(SIZE_T) :: rddc_nbytes
	    integer :: nelmts
	    real :: rdcc_w0
      
        logical :: h5_file_exists
	    integer :: error	! HDF5 Error flag

        time_step = gtm_time_interval

	    ! check to see if file exists
     	inquire (file=hdf_name, exist=h5_file_exists)

    	if (h5_file_exists) then ! file already exists
	         write(unit_error,920) trim(hdf_name)
 920         format(' File already exists... deleting existing file :: ', a )
	    endif

        call h5open_f(error)

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

        !todo: does this create unwanted project dependence?
        !call write_input_buffers_hdf5(hdf_file.file_id)   ! comment out by ehsu
      
        ! create group for output
	    call h5gcreate_f(hdf_file%file_id, "output", hdf_file%data_id, error)
	
	    hdf_interval = incr_intvl(0,hdf_interval_char,TO_BOUNDARY)
	    qual_hdf%write_interval = hdf_interval
	    if (hdf_interval < time_step) then
	        write(unit_error,*) "HDF write interval is finer than the simulation time step"
	        call exit(-3)
	    end if
	
	    ! This would be more complex if time averages were stored
        hdf_start = incr_intvl(sim_start,hdf_interval_char, NEAREST_BOUNDARY)
        qual_hdf%start_julmin = hdf_start

        ! todo: is this "1+" always right? It wasn't in the original code      
        ! record the dimensions of the simulation
	    ntime = 1+(sim_end - hdf_start)/hdf_interval
        hdf_end = hdf_start + (ntime-1)*hdf_interval

        hdf_file%channel_dim = nchannel
        hdf_file%conc_dim = nconc
        hdf_file%time_dim = ntime
	    hdf_file%res_dim = nres
	    hdf_file%time_index = 1
	
	    call write_dimensions(hdf_file%data_id, nchannel, nres, nconc)
	
	    ! create the data sets for time-varying output
	    call init_channel_qual_hdf5(hdf_file, nchannel, nconc, ntime)
	    if (hdf_file%res_dim .gt. 0)then
	        call init_reservoir_qual_hdf5(hdf_file, nres, nconc, ntime)
	    end if
	    
        ! initialize attributes and datasets
        ! call write_qual_attributes_hdf5()
        !call attach_qual_dimscales(hdf_file%file_id) !todo:: comment out by ehsu, do we need this?

        using_qual_hdf = .true.
        
	    return
	end subroutine      


    !> Write out lookup information for channels, reservoirs, and constituents. 
    subroutine write_dimensions(loc_id, nchans, nreser, nconc)
        use hdf5
        use common_variables, only: chan_geom, res_geom, constituents
        implicit none
        integer (HID_T), intent(in) :: loc_id              !< hdf file data ID
        integer, intent(in) :: nchans
        integer, intent(in) :: nreser
        integer, intent(in) :: nconc
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
	    integer, parameter :: name_len=32
        character(LEN=name_len),dimension(:), allocatable :: names
        integer :: i
        integer :: ierror
      
        in_dims(1) = nchans    
        ! Write out channel geometry
        call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, ierror)      
 
        ! Write out external channel numbers int2ext
        call h5screate_simple_f(in_rank, in_dims, in_dspace_id, ierror)
        call h5dcreate_f(loc_id, "channel_number", H5T_NATIVE_INTEGER,    &   
                         in_dspace_id, in_dset_id, ierror, cparms)
        call h5dwrite_f(in_dset_id,H5T_NATIVE_INTEGER,                    &   
                        chan_geom(:)%channel_num, in_dims, ierror)
        call h5sclose_f (in_dspace_id, ierror)
        call h5dclose_f(in_dset_id,ierror)
 
  	    ! Write reservoir names
  	    if (nreser.gt.0) then
            in_dims(1) = nreser
            allocate(names(nreser))
	        names = ' '
	        do i = 1, nreser
	            names(i) = res_geom(i)%name 
            end do
	        call write_1D_string_array(loc_id,"reservoir_names",names,    &     
                                       name_len, nreser)
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


    !> Initialize qual tide file for channel time series
	subroutine init_channel_qual_hdf5(hdf_file, nchannel, nconc, ntime)

	    use hdf5
	    use time_utilities, only: jmin2cdt
	    implicit none
	    
        type(qual_hdf_t), intent(inout) :: hdf_file        !< hdf file properties
	    integer, intent(in) :: ntime                    !< number of time steps
	    integer, intent(in) :: nchannel                 !< number of channels
        integer, intent(in) :: nconc	                !< number of constituents
   	    integer(HID_T) :: attr_id                       ! Attribute identifier 
        integer(HID_T) :: aspace_id                     ! Attribute dataspace identifier 
        integer(HID_T) :: atype_id                      ! Attribute type identifier 
        integer(HSIZE_T), dimension(1) :: adims = (/1/) ! Attribute dimension
        integer     ::   arank = 1                      ! Attribute rank
        integer(HSIZE_T), dimension(7) :: a_data_dims
        integer     ::   chan_rank = 0
   	    integer(HSIZE_T), dimension(4) :: chan_file_dims  = 0 ! Data size on file
	    integer(HSIZE_T), dimension(4) :: chan_chunk_dims = 0 ! Dataset chunk dimensions

	    integer(HID_T) :: fspace_id                     ! File space identifier
	    integer(HID_T) :: cparms                        ! dataset creatation property identifier 
	    integer        :: error                         ! HDF5 Error flag
	
	    call h5screate_simple_f(arank, adims, aspace_id, error)
	    call h5tcopy_f(H5T_NATIVE_INTEGER, atype_id, error)

		! Create channel avg area data set, one data point per channel
        chan_rank = 3
        chan_file_dims(1) = nchannel
	    chan_file_dims(2) = nconc
	    chan_file_dims(3) = ntime
      
	    chan_chunk_dims(1) = nchannel
	    chan_chunk_dims(2) = nconc
	    chan_chunk_dims(3) = min(TIME_CHUNK,ntime)
	
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

	    call h5screate_simple_f(chan_rank,              &   
                                chan_file_dims,         &     
                                fspace_id,              &    
                                error)
	    call h5dcreate_f(hdf_file%data_id,              &  
                         "channel concentration",       &    
                         H5T_NATIVE_REAL,               &	   
                         fspace_id,                     &
                         hdf_file%chan_conc_id,         &    
                         error,                         &     
                         cparms) 
	    call verify_error(error,"Channel avg conc dataset creation")
        call add_timeseries_attributes(hdf_file%chan_conc_id,   &  
                                       hdf_file%start_julmin,   &
                                       hdf_file%write_interval)

	    return
	end subroutine

	!> Initialize qual tide file for reservoir time series 
	subroutine init_reservoir_qual_hdf5(hdf_file, nres, nconc, ntime)
	    use hdf5
        implicit none
        type(qual_hdf_t), intent(inout) :: hdf_file
        integer(HID_T) :: cparms          ! dataset creation property identifier 
        integer        :: error	          ! HDF5 Error flag
        integer        :: res_rank = 3
        integer(HSIZE_T), dimension(3) :: chunk_dims = 0 ! Dataset dimensions
        integer        :: ntime           ! number of time points in tidefile
        integer        :: nres
        integer        :: nconc
        integer(HSIZE_T), dimension(4) :: file_dims  = 0 ! Data size on file      
	    integer(HID_T) :: fspace_id       ! File space identifier
	    
        !-------Create the datasets
        res_rank = 3
        file_dims(1) = nres
	    file_dims(2) = nconc
	    file_dims(3) = ntime

	    chunk_dims(1) = nres
	    chunk_dims(2) = nconc
	    chunk_dims(3) = min(TIME_CHUNK,ntime)

		! Add chunking and compression
	    call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, error)
	    if (ntime .gt. MIN_STEPS_FOR_CHUNKING) then
	        call h5pset_chunk_f(cparms, res_rank, chunk_dims, error)
	        call H5Pset_szip_f (cparms, H5_SZIP_NN_OM_F,           &  
                                HDF_SZIP_PIXELS_PER_BLOCK, error);
        end if

	    call h5screate_simple_f(res_rank,                &  
                                file_dims,               &  
                                fspace_id,               &  
                                error)
	    call h5dcreate_f(hdf_file%data_id,               & 
                         "reservoir concentration",      & 
                         H5T_NATIVE_REAL,                & 
                         fspace_id,                      &  
                         hdf_file%res_conc_id,           &
                         error,                          &  
                         cparms)
        call add_timeseries_attributes(hdf_file%res_conc_id,    &      
                                       hdf_file%start_julmin,   &
                                       hdf_file%write_interval)

        return
	end subroutine
      
   
    !> Write time series data to Qual tidefile  
    subroutine write_qual_hdf5(hdf_file,      &
                               chan_conc,     & 
                               res_conc,      &
                               nchan,         & 
                               nres,          &
                               nconc,         &  
                               time_index)
        use hdf5
        implicit none
        type(qual_hdf_t) :: hdf_file
        integer nchan, nres, nconc
        real(gtm_real), intent(in) ::  chan_conc(nchan,nconc) ! chan data to write
        real(gtm_real), intent(in) ::  res_conc(nres,nconc)    ! res data to write
        integer :: time_index
        integer :: chan_rank
        integer :: res_rank
        integer(HID_T) :: fspace_id
        integer(HID_T) :: memspace_id
     	integer(HSIZE_T), dimension(4) :: mdata_dims  = 0   ! Dims of data in memory
      	integer(HSIZE_T), dimension(4) :: subset_dims  = 0  ! Dims of subset for time step
    	integer(HSIZE_T), dimension(4) :: h_offset = (/0,0,0,0/)
        integer :: error   ! HDF5 Error flag
        if (mod(time_index,24*10) .eq. 1) call h5garbage_collect_f(error)
     
        !-----chan conc
        h_offset(1) = 0
        h_offset(2) = 0
        h_offset(3) = time_index

	    subset_dims(1) = nchan
	    subset_dims(2) = nconc
	    subset_dims(3) = 1

	    mdata_dims(1) = nchan
	    mdata_dims(2) = nconc
        chan_rank = 2
        call H5Screate_simple_f(chan_rank,           &
                                mdata_dims,          &
                                memspace_id,         &
                                error);  
        call h5dget_space_f (hdf_file%chan_conc_id,  &  
                             fspace_id,              &
                             error)
        call h5sselect_hyperslab_f(fspace_id,        &
                                   H5S_SELECT_SET_F, &
                                   h_offset,         & 
                                   subset_dims,      &
                                   error)
        call h5dwrite_f(hdf_file%chan_conc_id,       &
                        H5T_NATIVE_REAL,             &
                        chan_conc,                   &
                        mdata_dims,                  &
                        error,                       &
                        memspace_id,                 & 
                        fspace_id)
	    call verify_error(error,"Channel concentration write")
        call h5sclose_f (fspace_id, error)
        call h5sclose_f (memspace_id, error)      
      
        if (nres .gt. 0)then
            h_offset(1) = 0
            h_offset(2) = 0
            h_offset(3) = time_index
	        subset_dims(1) = nres
	        subset_dims(2) = nconc
	        subset_dims(3) = 1
	        mdata_dims(1) = nres
	        mdata_dims(2) = nconc
	        res_rank = 2
            call H5Screate_simple_f(res_rank,            &
                                    mdata_dims,          & 
                                    memspace_id,         &
                                    error); 
            call h5dget_space_f (hdf_file.res_conc_id,   &
                                 fspace_id,              &
                                 error)
            call h5sselect_hyperslab_f(fspace_id,        &
                                       H5S_SELECT_SET_F, & 
                                       h_offset,         & 
                                       subset_dims,      & 
                                       error) 
            call h5dwrite_f(hdf_file%res_conc_id,        &
                            H5T_NATIVE_REAL,             &   
                            res_conc,                    &
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

	    call h5dclose_f(hdf_file%chan_conc_id,error)
        if (error .ne. 0) then
	        write(unit_error,*)"HDF5 error closing channel conc data set: ",error
	    end if

        if (hdf_file%res_dim.gt.0) then
	        call h5dclose_f(hdf_file%res_conc_id,error)
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

        if (print_level .gt.2) write(unit_screen,*)"Closing HDF5"
        call h5close_f(error)
	    if (error .ne. 0) then
	        write(unit_error,*)"HDF5 error closing hdf5: ",error
        end if
        if (print_level .gt.2) write(unit_screen,*)"Closed HDF5"      
	    return
    end subroutine
      
      
    !> Query if it is time to write Qual state to HDF5      
    logical function is_qual_hdf_write_interval(julmin)
        implicit none
        integer, intent(in) :: julmin
        is_qual_hdf_write_interval = ( julmin .ge. qual_hdf.start_julmin .and.     &
                                     mod(julmin, qual_hdf.write_interval) .eq. 0)
        return
    end function
        
    !> Write Qual state to HDF5
    !> This routine includes most coupling between qual and tidefile code
    subroutine write_qual_hdf(julmin,    &
                              nchan,     &
                              nres,      &
                              nconc)
        implicit none
        integer, intent(in) :: julmin
        integer, intent(in) :: nchan
        integer, intent(in) :: nres
        integer, intent(in) :: nconc
        integer :: time_index
        real(gtm_real) :: chan_conc(nchan, nconc)
        real(gtm_real) :: res_conc(nres, nconc)
        if (.not. is_qual_hdf_write_interval(julmin)) then
           return
        end if
        time_index = (julmin - qual_hdf%start_julmin)/qual_hdf%write_interval
        call qual_state_for_hdf5(chan_conc, res_conc, nchan, nres, nconc)
        call write_qual_hdf5(qual_hdf,      &
                             chan_conc,     &
                             res_conc,      & 
                             nchan,         &
                             nres,          &
                             nconc,         &
                             time_index)
        return
    end subroutine

    subroutine qual_state_for_hdf5(chan_conc, res_conc, nchans, nres, nconc)
        implicit none
        integer, intent(in) :: nchans
        integer, intent(in) :: nres
        integer, intent(in) :: nconc
        real(gtm_real), intent(out) :: chan_conc(nchans, nconc)
        real(gtm_real), intent(out) :: res_conc(nres, nconc)
        integer :: i, j
        do i = 1, nchans
            do j = 1,nconc
            chan_conc(i,j) = i + j
            enddo
        enddo
        if (nres.gt.0) then
            do i = 1, nres
                do j = 1,nconc
                res_conc(i,j) = i + j
                enddo
            enddo            
        end if    
        return
    end subroutine    


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
        integer :: ts_start
        integer :: ts_interval
        character*16 :: cinterval
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

        write(cinterval,"(i,'min')")ts_interval
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

	    iso_datetime=jmin2iso(ts_start)
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