module sb_hdf_util
    !use gtm_precision
    use sb_common
    use hdf_util , only: get_int_attribute_from_hdf5, n_chan, n_xsect, n_comp, hydro_ntideblocks,GET_HYDRO_ATTR, Read_Channel_tbl, find_non_sequential, &
                         READ_COMP_TBL, ASSIGN_CHAN_COMPPT,ASSIGN_SEGMENT,READ_XSECT_TBL,GET_TS_FROM_HDF5,VERIFY_ERROR
    use h5lt
    use common_xsect
implicit none

    contains
    subroutine get_hydro_attr_sb()
        implicit none        
        call get_int_attribute_from_hdf5(n_chan, "Number of channels")
        call get_int_attribute_from_hdf5(n_xsect, "Number of virt xsects")
        call get_int_attribute_from_hdf5(n_comp, "Number of comp pts")
        call get_int_attribute_from_hdf5(hydro_ntideblocks, "Number of intervals")
       return
    end subroutine 
    
    subroutine dsm2_hdf_geom_sb()
        implicit none
        write(*,*) "processing channel geometry"
        call get_hydro_attr
        call read_channel_tbl
        call find_non_sequential
        call read_comp_tbl
        !call read_channel_bottom_tbl        
        call assign_chan_comppt
        call assign_segment   
        !call read_reservoir_tbl
        !call read_qext_tbl
        !call read_tran_tbl
        call read_xsect_tbl
        return
        !call read_gate_tbl
        !call read_source_flow_tbl
        !call read_boundary_tbl
        !call get_dsm2_network_info
    end subroutine     
    
        
    subroutine check_attr_sb()
        integer :: nchan
        nchan = n_chan
        return
    end subroutine    
    
    subroutine get_elevations()
        integer :: ii
        integer :: jj
        !real(gtm_real) :: elev(n_comp)
        real(gtm_real), allocatable, dimension(:,:) :: elev
        write(*,*) "processing water surface elevations"
        allocate (elev(n_comp,hydro_ntideblocks))
        elevation.max = zero
        elevation.min = 100.00
        max_elev_dx = zero
        call get_ts_from_hdf5(elev,"inst water surface", n_comp, 0, hydro_ntideblocks)  !todo:this may need to be divided up if hydro_ntideblocks is too big???
        do ii = 1,  hydro_ntideblocks 
            !call get_ts_from_hdf5(elev,"inst water surface", n_comp, ii,1)
            do jj = 1, n_comp 
                if (elev(jj,ii) > elevation(jj).max) elevation(jj).max = elev(jj,ii)
                if (elev(jj,ii) < elevation(jj).min) elevation(jj).min = elev(jj,ii)
                if ((elevation(jj).max - elevation(jj).min) >  max_elev_dx) then
                    min_elev = elevation(jj).min
                    max_elev = elevation(jj).max
                    max_elev_dx = (elevation(jj).max - elevation(jj).min)
                end if
            end do
        end do
        deallocate (elev)
        return
    end subroutine
        
    
    subroutine write_hdf_sb(init_input_file)
        !args
        character init_input_file*(*) 
        !local
        character(len=130) :: file_name_hdf_bed
        integer :: ii
        logical :: file_exists
        
        integer(HID_T) :: bed_file_id
        integer(HID_T) :: bed_data_id
        
        integer(HID_T) :: access_plist              ! Dataset trasfer property
	    integer(SIZE_T) :: rddc_nelmts
	    integer(SIZE_T) :: rddc_nbytes
	    integer :: nelmts
	    real :: rdcc_w0
        logical :: h5_file_exists
	    integer :: error	                        ! HDF5 Error flag
        
        
        file_name_hdf_bed = init_input_file
        ii = index(file_name_hdf_bed,".", .true.)
        file_name_hdf_bed(ii:ii+11) = "_bed_zone.h5" 
        
        inquire (file=file_name_hdf_bed, exist=file_exists)
    	if (file_exists) then
            write(*,*) 'Deleting existing file: '
            write(*,*) ' ' // trim(file_name_hdf_bed) 
        endif
        
         write(*,*) 'Writing to file: ' 
         write(*,*) ' ' // trim(file_name_hdf_bed) 
        ! set up stdio to allow for buffered read/write
	    call H5Pcreate_f(H5P_FILE_ACCESS_F, access_plist, error)
      
	    call h5pget_cache_f(access_plist, nelmts,   &   
                            rddc_nelmts, rddc_nbytes, rdcc_w0,error)
  	    rddc_nbytes = 8000000
	    call h5pset_cache_f(access_plist, nelmts, rddc_nelmts,    &     
                            rddc_nbytes, rdcc_w0,error)
	    call verify_error(error, "Cache set")
        
        call h5fcreate_f(file_name_hdf_bed, H5F_ACC_TRUNC_F,bed_file_id, error, &     
                         H5P_DEFAULT_F, access_plist)
      
        ! create group for output
	    !call h5gcreate_f(bed_file_id, "zones", bed_data_id, error)
        
        call h5gcreate_f(bed_file_id, "geom", bed_data_id, error)
        !call h5dcreate_f(geom_id, "segment", dtype_id, dspace_id, dset_id, error)  
        call write_bed_attributes_sb(bed_file_id)
        call write_zone_info( bed_data_id)
        
        !call h5gclose_f(bed_data_id,error)
        call h5gclose_f(bed_data_id,error)
        call h5fclose_f(bed_file_id,error)
        !call hdf5_close()
    end subroutine write_hdf_sb
    
    subroutine write_bed_attributes_sb(dset_id)
        integer(HID_T) :: dset_id ! Attribute identifier 
        integer     ::   arank = 1
        character(LEN=32) :: buffer
	    integer(HSIZE_T), dimension(1) :: a_dims = (/1/)
        integer :: err
        integer :: scalar = 1
        integer,dimension(1) :: hdf_dummy_integer
        
        hdf_dummy_integer = n_cell
        call h5ltset_attribute_int_f(dset_id,".","n_cell", &
                                     hdf_dummy_integer, scalar, err) 
        hdf_dummy_integer = n_zone
        call h5ltset_attribute_int_f(dset_id,".","n_zone", &
                                     hdf_dummy_integer, scalar, err) 
        
    end subroutine write_bed_attributes_sb
    
     subroutine write_zone_info(zone_id)
        use hdf5
        use common_variables, only: n_cell,n_segm, segm
        use sb_common
        implicit none
        integer(HID_T), intent(in) :: zone_id        !< hdf5 dataset identifier
        integer(HID_T) :: dset_id                    ! dataset identifier
        integer(HID_T) :: dspace_id                  ! dataspace identifier
        integer(HID_T) :: dtype_id                   ! compound datatype identifier
        integer(HID_T) :: dt1_id, dt2_id, dt3_id     ! memory datatype identifier
        integer(HID_T) :: dt4_id, dt5_id, dt6_id     ! memory datatype identifier
        integer(HID_T) :: dt_id                      ! memory datatype identifier      
        integer(HID_T) :: dt10_id                    ! memory datatype identifier
        integer(HID_T) :: plist_id                   ! dataset transfer property
        integer(SIZE_T) :: typesize
        integer(SIZE_T) :: type_size
        integer(SIZE_T) :: type_sizei
        integer(SIZE_T) :: type_sized
        integer(SIZE_T) :: offset
        integer(HSIZE_T), dimension(1) :: data_dims                     
        integer(HSIZE_T), dimension(1) :: dims 
        integer :: rank = 1
        integer :: i, error
        
        if (n_segm .eq. 0) then
            write(*,*) "Number of segments = 0"
            return
        end if    
        !n_cell = 3000
        dims = (/n_zone * n_cell/) 
        data_dims(1) = n_zone * n_cell
       
        call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, error)
        call h5pset_preserve_f(plist_id, .TRUE., error)
       
        call h5screate_simple_f(rank, dims, dspace_id, error)
        
        call h5tcopy_f(H5T_NATIVE_INTEGER, dt_id, error)
        typesize = 4
        call h5tset_size_f(dt_id, typesize, error)
        call h5tget_size_f(dt_id, type_sizei, error)
        call h5tget_size_f(H5T_NATIVE_DOUBLE, type_sized, error)
        type_size = 2*type_sizei + 4*type_sized
        
        call h5tcreate_f(H5T_COMPOUND_F, type_size, dtype_id, error)
        
        offset = 0
        call h5tinsert_f(dtype_id, "cell_no", offset, dt_id, error)

        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "zone_no", offset, H5T_NATIVE_INTEGER, error)

        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "elev", offset, H5T_NATIVE_DOUBLE, error)
                
        offset = offset + type_sized
        call h5tinsert_f(dtype_id, "cell_wet_p", offset, H5T_NATIVE_DOUBLE, error)

        offset = offset + type_sized
        call h5tinsert_f(dtype_id, "zone_wet_p", offset, H5T_NATIVE_DOUBLE, error)
        
        offset = offset + type_sized
        call h5tinsert_f(dtype_id, "width", offset, H5T_NATIVE_DOUBLE, error)        
       
                
        call h5dcreate_f(zone_id, "zone_info", dtype_id, dspace_id, dset_id, error)        
        
        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt1_id, error)
        offset = 0
        call h5tinsert_f(dt1_id, "cell_no", offset, dt_id, error)
        
        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt2_id, error)
        offset = 0
        call h5tinsert_f(dt2_id, "zone_no", offset, H5T_NATIVE_INTEGER, error)         

        call h5tcreate_f(H5T_COMPOUND_F, type_sized, dt3_id, error)
        offset = 0
        call h5tinsert_f(dt3_id, "elev", offset, H5T_NATIVE_DOUBLE, error)   
         
        call h5tcreate_f(H5T_COMPOUND_F, type_sized, dt4_id, error)
        offset = 0
        call h5tinsert_f(dt4_id, "cell_wet_p", offset, H5T_NATIVE_DOUBLE, error)  

        call h5tcreate_f(H5T_COMPOUND_F, type_sized, dt5_id, error)
        offset = 0
        call h5tinsert_f(dt5_id, "zone_wet_p", offset, H5T_NATIVE_DOUBLE, error)         

        call h5tcreate_f(H5T_COMPOUND_F, type_sized, dt6_id, error)
        offset = 0
        call h5tinsert_f(dt6_id, "width", offset, H5T_NATIVE_DOUBLE, error) 
        
        call h5dwrite_f(dset_id, dt1_id, sed_hdf%cell,       data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt2_id, sed_hdf%zone,       data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt3_id, sed_hdf%elev,       data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt4_id, sed_hdf%cell_wet_p, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt5_id, sed_hdf%zone_wet_p, data_dims, error, xfer_prp = plist_id)   
        call h5dwrite_f(dset_id, dt6_id, sed_hdf%width,      data_dims, error, xfer_prp = plist_id) 
        
        call h5dclose_f(dset_id, error)
        call h5sclose_f(dspace_id, error)
        call h5tclose_f(dtype_id, error)
        call h5tclose_f(dt1_id, error)
        call h5tclose_f(dt2_id, error)
        call h5tclose_f(dt3_id, error)
        call h5tclose_f(dt4_id, error)
        call h5tclose_f(dt5_id, error)
        call h5tclose_f(dt6_id, error)
        call h5tclose_f(dt_id, error)    
        
        !write cell lengths and top_elev
        dims = (/n_cell/) 
        data_dims(1) = n_cell
        type_size = 2*type_sizei + 4*type_sized
        
        call h5screate_simple_f(rank, dims, dspace_id, error)
        call h5tcopy_f(H5T_NATIVE_INTEGER, dt_id, error)
        call h5tcreate_f(H5T_COMPOUND_F, type_size, dtype_id, error)
        
        offset = 0
        call h5tinsert_f(dtype_id, "cell_no", offset, dt_id, error)
        
        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "chan_no", offset, dt_id, error)
        
        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "min_elev", offset, H5T_NATIVE_DOUBLE, error)
        
        offset = offset + type_sized
        call h5tinsert_f(dtype_id, "max_elev", offset, H5T_NATIVE_DOUBLE, error)
        
        offset = offset + type_sized
        call h5tinsert_f(dtype_id, "top_elev", offset, H5T_NATIVE_DOUBLE, error)
                
        offset = offset + type_sized
        call h5tinsert_f(dtype_id, "length", offset, H5T_NATIVE_DOUBLE, error)
        
        call h5dcreate_f(zone_id, "cell_info", dtype_id, dspace_id, dset_id, error)        
        
        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt1_id, error)
        offset = 0
        call h5tinsert_f(dt1_id, "cell_no", offset, dt_id, error)
        
        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt2_id, error)
        offset = 0
        call h5tinsert_f(dt2_id, "chan_no", offset, dt_id, error)
        
        call h5tcreate_f(H5T_COMPOUND_F, type_sized, dt3_id, error)
        offset = 0
        call h5tinsert_f(dt3_id, "min_elev", offset, H5T_NATIVE_DOUBLE, error)
       
        call h5tcreate_f(H5T_COMPOUND_F, type_sized, dt4_id, error)
        offset = 0
        call h5tinsert_f(dt4_id, "max_elev", offset, H5T_NATIVE_DOUBLE, error)
        
        call h5tcreate_f(H5T_COMPOUND_F, type_sized, dt5_id, error)
        offset = 0
        call h5tinsert_f(dt5_id, "top_elev", offset, H5T_NATIVE_DOUBLE, error)   
         
        call h5tcreate_f(H5T_COMPOUND_F, type_sized, dt6_id, error)
        offset = 0
        call h5tinsert_f(dt6_id, "length", offset, H5T_NATIVE_DOUBLE, error)  
        
        !write statements go here
        
        call h5dwrite_f(dset_id, dt1_id, sedcell%cell_no,  data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt2_id, sedcell%chan_no,  data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt3_id, sedcell%min_elev, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt4_id, sedcell%max_elev, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt5_id, top_elev,         data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt6_id, sedcell%length,   data_dims, error, xfer_prp = plist_id)
        
        
        call h5dclose_f(dset_id, error)
        call h5sclose_f(dspace_id, error)
        call h5tclose_f(dtype_id, error)
        call h5tclose_f(dt1_id, error)
        call h5tclose_f(dt2_id, error)
        call h5tclose_f(dt3_id, error)
        call h5tclose_f(dt4_id, error)
        call h5tclose_f(dt5_id, error)
        call h5tclose_f(dt6_id, error)
        call h5tclose_f(dt_id, error) 
        return
     end subroutine
     
     
end module sb_hdf_util