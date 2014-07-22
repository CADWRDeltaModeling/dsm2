

!> Module to write GTM simulation results to HDF5 file. 
!> This module was mainly borrowed from DSM2-Qual with several 
!> modifications to make it stand-alone, testable and in 
!> fortran 90 style. 
!>@ingroup process_io
module gtm_hdf_write

    use hdf5, only: HID_T, HSIZE_T
    use gtm_precision
    use error_handling      
    
    contains

    !> Create geometry group
    subroutine create_geometry_group(geom_id, file_id)
        use hdf5
        implicit none
        integer(HID_T), intent(in) :: file_id
        integer(HID_T), intent(out) :: geom_id
        integer :: error
        call h5gcreate_f(file_id, "geometry", geom_id, error)
        return
    end subroutine    

    !> Write out attributes into GTM tidefile
    subroutine write_attributes_gtm(geom_id)
        use hdf5
        use h5lt
        use common_variables
        use time_utilities
        implicit none
        integer(HID_T), intent(in) :: geom_id        !< hdf5 geom dataset identifier
        integer :: err
        integer :: scalar = 1
        integer,dimension(1) :: hdf_dummy_integer
        real(gtm_real), dimension(1) :: hdf_dummy_real
        
        call h5ltset_attribute_string_f(geom_id,".","gtm_start_date", &
                                        trim(jmin2cdt(int(gtm_start_jmin)))//char(0), err)
        hdf_dummy_real = gtm_start_jmin
        call h5ltset_attribute_double_f(geom_id,".","gtm_start_jmin", &
                                     hdf_dummy_real, scalar, err)   
        hdf_dummy_real = gtm_time_interval
        call h5ltset_attribute_double_f(geom_id,".","gtm_time_interval", &
                                     hdf_dummy_real, scalar, err)   
        hdf_dummy_integer = n_comp
        call h5ltset_attribute_int_f(geom_id,".","n_comp", &
                                     hdf_dummy_integer, scalar, err) 
        hdf_dummy_integer = n_chan
        call h5ltset_attribute_int_f(geom_id,".","n_chan", &
                                     hdf_dummy_integer, scalar, err)
        hdf_dummy_integer = n_segm
        call h5ltset_attribute_int_f(geom_id,".","n_segm", &
                                     hdf_dummy_integer, scalar, err)
        hdf_dummy_integer = n_conn
        call h5ltset_attribute_int_f(geom_id,".","n_conn", &
                                     hdf_dummy_integer, scalar, err)
        hdf_dummy_integer = n_junc
        call h5ltset_attribute_int_f(geom_id,".","n_junc", &
                                     hdf_dummy_integer, scalar, err)
        hdf_dummy_integer = n_boun
        call h5ltset_attribute_int_f(geom_id,".","n_boun", &
                                     hdf_dummy_integer, scalar, err)                                                                                                               
        hdf_dummy_integer = n_xsect
        call h5ltset_attribute_int_f(geom_id,".","n_xsect", &
                                     hdf_dummy_integer, scalar, err)
        hdf_dummy_integer = n_resv
        call h5ltset_attribute_int_f(geom_id,".","n_resv", &
                                     hdf_dummy_integer, scalar, err)
        hdf_dummy_integer = n_resv_conn
        call h5ltset_attribute_int_f(geom_id,".","n_resv_conn", &
                                     hdf_dummy_integer, scalar, err)
        hdf_dummy_integer = n_qext
        call h5ltset_attribute_int_f(geom_id,".","n_qext", &
                                     hdf_dummy_integer, scalar, err)                                                                          
        hdf_dummy_integer = n_cell
        call h5ltset_attribute_int_f(geom_id,".","n_cell", &
                                     hdf_dummy_integer, scalar, err)
        hdf_dummy_integer = n_var
        call h5ltset_attribute_int_f(geom_id,".","n_var", &
                                     hdf_dummy_integer, scalar, err)           
        return
    end subroutine
            
    !> Write out channel info into GTM tidefile
    subroutine write_channel_info(geom_id)
        use hdf5
        use common_variables, only: n_chan, chan_geom
        implicit none
        integer(HID_T), intent(in) :: geom_id        !< hdf5 geom dataset identifier
        integer(HID_T) :: dset_id                    ! dataset identifier
        integer(HID_T) :: dspace_id                  ! dataspace identifier
        integer(HID_T) :: dtype_id                   ! compound datatype identifier
        integer(HID_T) :: dt1_id, dt2_id, dt3_id     ! memory datatype identifier
        integer(HID_T) :: dt4_id, dt5_id, dt6_id     ! memory datatype identifier
        integer(HID_T) :: dt_id, dt7_id              ! memory datatype identifier      
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
 
        if (n_chan .eq. 0) then
            write(*,*) "Number of channels = 0"
            return
        end if    
        
        dims = (/n_chan/) 
        data_dims(1) = n_chan
       
        call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, error)
        call h5pset_preserve_f(plist_id, .TRUE., error)
               
        call h5screate_simple_f(rank, dims, dspace_id, error)
        
        call h5tcopy_f(H5T_NATIVE_INTEGER, dt_id, error)
        typesize = 4
        call h5tset_size_f(dt_id, typesize, error)
        call h5tget_size_f(dt_id, type_sizei, error)
        type_size = 7*type_sizei
        
        call h5tcreate_f(H5T_COMPOUND_F, type_size, dtype_id, error)
        
        offset = 0
        call h5tinsert_f(dtype_id, "chan_no", offset, dt_id, error)

        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "channel_num", offset, H5T_NATIVE_INTEGER, error)
        
        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "channel_length", offset, H5T_NATIVE_INTEGER, error)

        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "up_node", offset, H5T_NATIVE_INTEGER, error)
        
        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "down_node", offset, H5T_NATIVE_INTEGER, error)
        
        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "up_comp", offset, H5T_NATIVE_INTEGER, error)
        
        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "down_comp", offset, H5T_NATIVE_INTEGER, error)        
        
        call h5dcreate_f(geom_id, "channel", dtype_id, dspace_id, dset_id, error)        
        
        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt1_id, error)
        offset = 0
        call h5tinsert_f(dt1_id, "chan_no", offset, dt_id, error)
        
        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt2_id, error)
        offset = 0
        call h5tinsert_f(dt2_id, "channel_num", offset, H5T_NATIVE_INTEGER, error)         
 
        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt3_id, error)
        offset = 0
        call h5tinsert_f(dt3_id, "channel_length", offset, H5T_NATIVE_INTEGER, error)  

        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt4_id, error)
        offset = 0
        call h5tinsert_f(dt4_id, "up_node", offset, H5T_NATIVE_INTEGER, error)         

        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt5_id, error)
        offset = 0
        call h5tinsert_f(dt5_id, "down_node", offset, H5T_NATIVE_INTEGER, error)  

        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt6_id, error)
        offset = 0
        call h5tinsert_f(dt6_id, "up_comp", offset, H5T_NATIVE_INTEGER, error)

        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt7_id, error)
        offset = 0
        call h5tinsert_f(dt7_id, "down_comp", offset, H5T_NATIVE_INTEGER, error)
                
        call h5dwrite_f(dset_id, dt1_id, chan_geom%chan_no, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt2_id, chan_geom%channel_num, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt3_id, chan_geom%channel_length, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt4_id, chan_geom%up_node, data_dims, error, xfer_prp = plist_id)                        
        call h5dwrite_f(dset_id, dt5_id, chan_geom%down_node, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt6_id, chan_geom%up_comp, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt7_id, chan_geom%down_comp, data_dims, error, xfer_prp = plist_id)        
        
        call h5dclose_f(dset_id, error)
        call h5sclose_f(dspace_id, error)
        call h5tclose_f(dtype_id, error)
        call h5tclose_f(dt1_id, error)
        call h5tclose_f(dt2_id, error)
        call h5tclose_f(dt3_id, error)
        call h5tclose_f(dt4_id, error)
        call h5tclose_f(dt5_id, error)
        call h5tclose_f(dt6_id, error)
        call h5tclose_f(dt7_id, error)
        call h5tclose_f(dt_id, error)                
        return
    end subroutine
 
 
    !> Write out geomotry info into GTM tidefile
    subroutine write_segment_info(geom_id)
        use hdf5
        use common_variables, only: n_segm, segm
        implicit none
        integer(HID_T), intent(in) :: geom_id        !< hdf5 dataset identifier
        integer(HID_T) :: dset_id                    ! dataset identifier
        integer(HID_T) :: dspace_id                  ! dataspace identifier
        integer(HID_T) :: dtype_id                   ! compound datatype identifier
        integer(HID_T) :: dt1_id, dt2_id, dt3_id     ! memory datatype identifier
        integer(HID_T) :: dt4_id, dt5_id, dt6_id     ! memory datatype identifier
        integer(HID_T) :: dt_id                      ! memory datatype identifier      
        integer(HID_T) :: dt7_id, dt8_id, dt9_id     ! memory datatype identifier             
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
        
        dims = (/n_segm/) 
        data_dims(1) = n_segm
       
        call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, error)
        call h5pset_preserve_f(plist_id, .TRUE., error)
       
        call h5screate_simple_f(rank, dims, dspace_id, error)
        
        call h5tcopy_f(H5T_NATIVE_INTEGER, dt_id, error)
        typesize = 4
        call h5tset_size_f(dt_id, typesize, error)
        call h5tget_size_f(dt_id, type_sizei, error)
        call h5tget_size_f(H5T_NATIVE_DOUBLE, type_sized, error)
        type_size = 6*type_sizei + 4*type_sized
        
        call h5tcreate_f(H5T_COMPOUND_F, type_size, dtype_id, error)
        
        offset = 0
        call h5tinsert_f(dtype_id, "segm_no", offset, dt_id, error)

        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "chan_no", offset, H5T_NATIVE_INTEGER, error)
        
        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "up_comp", offset, H5T_NATIVE_INTEGER, error)

        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "down_comp", offset, H5T_NATIVE_INTEGER, error)
        
        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "nx", offset, H5T_NATIVE_INTEGER, error)        

        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "start_cell_no", offset, H5T_NATIVE_INTEGER, error)  
           
        offset = offset + type_sized
        call h5tinsert_f(dtype_id, "up_distance", offset, H5T_NATIVE_DOUBLE, error)                
        
        offset = offset + type_sized
        call h5tinsert_f(dtype_id, "down_distance", offset, H5T_NATIVE_DOUBLE, error)
        
        offset = offset + type_sized
        call h5tinsert_f(dtype_id, "length", offset, H5T_NATIVE_DOUBLE, error)           
        
                
        call h5dcreate_f(geom_id, "segment", dtype_id, dspace_id, dset_id, error)        
        
        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt1_id, error)
        offset = 0
        call h5tinsert_f(dt1_id, "segm_no", offset, dt_id, error)
        
        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt2_id, error)
        offset = 0
        call h5tinsert_f(dt2_id, "chan_no", offset, H5T_NATIVE_INTEGER, error)         
 
        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt3_id, error)
        offset = 0
        call h5tinsert_f(dt3_id, "up_comp", offset, H5T_NATIVE_INTEGER, error)  

        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt4_id, error)
        offset = 0
        call h5tinsert_f(dt4_id, "down_comp", offset, H5T_NATIVE_INTEGER, error)         

        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt5_id, error)
        offset = 0
        call h5tinsert_f(dt5_id, "nx", offset, H5T_NATIVE_INTEGER, error)
        
        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt6_id, error)
        offset = 0
        call h5tinsert_f(dt6_id, "start_cell_no", offset, H5T_NATIVE_INTEGER, error)        
                
        call h5tcreate_f(H5T_COMPOUND_F, type_sized, dt7_id, error)
        offset = 0
        call h5tinsert_f(dt7_id, "up_distance", offset, H5T_NATIVE_DOUBLE, error)  

        call h5tcreate_f(H5T_COMPOUND_F, type_sized, dt8_id, error)
        offset = 0
        call h5tinsert_f(dt8_id, "down_distance", offset, H5T_NATIVE_DOUBLE, error)

        call h5tcreate_f(H5T_COMPOUND_F, type_sized, dt9_id, error)
        offset = 0
        call h5tinsert_f(dt9_id, "length", offset, H5T_NATIVE_DOUBLE, error)
        
        call h5dwrite_f(dset_id, dt1_id, segm%segm_no, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt2_id, segm%chan_no, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt3_id, segm%up_comppt, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt4_id, segm%down_comppt, data_dims, error, xfer_prp = plist_id)   
        call h5dwrite_f(dset_id, dt5_id, segm%nx, data_dims, error, xfer_prp = plist_id) 
        call h5dwrite_f(dset_id, dt6_id, segm%start_cell_no, data_dims, error, xfer_prp = plist_id)                              
        call h5dwrite_f(dset_id, dt7_id, segm%up_distance, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt8_id, segm%down_distance, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt9_id, segm%length, data_dims, error, xfer_prp = plist_id)        
        
        call h5dclose_f(dset_id, error)
        call h5sclose_f(dspace_id, error)
        call h5tclose_f(dtype_id, error)
        call h5tclose_f(dt1_id, error)
        call h5tclose_f(dt2_id, error)
        call h5tclose_f(dt3_id, error)
        call h5tclose_f(dt4_id, error)
        call h5tclose_f(dt5_id, error)
        call h5tclose_f(dt6_id, error)
        call h5tclose_f(dt7_id, error)
        call h5tclose_f(dt8_id, error)
        call h5tclose_f(dt9_id, error)
        call h5tclose_f(dt_id, error)                
        return
    end subroutine


    !> Write out DSM2 node info into GTM tidefile
    subroutine write_dsm2_node_info(geom_id)
        use hdf5
        use common_variables, only: n_node, dsm2_node
        implicit none
        integer(HID_T), intent(in) :: geom_id              !< hdf5 dataset identifier
        integer(HID_T) :: dset_id                          ! dataset identifier
        integer(HID_T) :: dspace_id                        ! dataspace identifier
        integer(HID_T) :: dtype_id                         ! compound datatype identifier
        integer(HID_T) :: dt_id, dt1_id, dt2_id, dt3_id    ! memory datatype identifier
        integer(HID_T) :: dt4_id, dt5_id, dt6_id, dt7_id   ! memory datatype identifier     
        integer(HID_T) :: dt8_id, dt9_id, dt10_id, dt11_id ! memory datatype identifier 
        integer(HID_T) :: plist_id                         ! dataset transfer property
        integer(SIZE_T) :: typesize
        integer(SIZE_T) :: type_size
        integer(SIZE_T) :: type_sizei
        integer(SIZE_T) :: type_sized
        integer(SIZE_T) :: offset
        integer(HSIZE_T), dimension(1) :: data_dims                     
        integer(HSIZE_T), dimension(1) :: dims 
        integer, allocatable :: dsm2_node_no(:)
        integer, allocatable :: int_node(:)
        integer, allocatable :: n_conn_cell(:)
        integer, allocatable :: cell_no(:)
        integer, allocatable :: up_down(:)
        integer, allocatable :: boundary_no(:)
        integer, allocatable :: junction_no(:)
        integer, allocatable :: reservoir_no(:)
        integer, allocatable :: n_qext(:)
        integer, allocatable :: nonsequential(:)
        integer, allocatable :: no_fixup(:)   
        integer :: rank = 1
        integer :: i, j, k, n, error
        
        if (n_node .eq. LARGEINT) then
            write(*,*) "Number of DSM2 Node = 0"
            return
        end if                  
  
        n = 0
        do i = 1, n_node
            n = n + dsm2_node(i)%n_conn_cell
        end do

        allocate(dsm2_node_no(n))
        allocate(int_node(n))
        allocate(n_conn_cell(n))
        allocate(cell_no(n))
        allocate(up_down(n))
        allocate(boundary_no(n))
        allocate(junction_no(n))
        allocate(reservoir_no(n))
        allocate(n_qext(n))
        allocate(nonsequential(n))
        allocate(no_fixup(n)) 

        dims = (/n/) 
        data_dims(1) = n
       
        call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, error)
        call h5pset_preserve_f(plist_id, .TRUE., error)
               
        call h5screate_simple_f(rank, dims, dspace_id, error)
        
        call h5tcopy_f(H5T_NATIVE_INTEGER, dt_id, error)
        typesize = 4
        call h5tset_size_f(dt_id, typesize, error)
        call h5tget_size_f(dt_id, type_sizei, error)
        type_size = 11*type_sizei
        
        call h5tcreate_f(H5T_COMPOUND_F, type_size, dtype_id, error)
        
        offset = 0
        call h5tinsert_f(dtype_id, "dsm2_node_no", offset, dt_id, error)
        
        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "int_node", offset, H5T_NATIVE_INTEGER, error)

        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "n_conn_cell", offset, H5T_NATIVE_INTEGER, error)
        
        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "cell_no", offset, H5T_NATIVE_INTEGER, error)

        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "up_down", offset, H5T_NATIVE_INTEGER, error)

        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "boundary_no", offset, H5T_NATIVE_INTEGER, error)

        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "junction_no", offset, H5T_NATIVE_INTEGER, error)

        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "reservoir_no", offset, H5T_NATIVE_INTEGER, error)

        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "n_qext", offset, H5T_NATIVE_INTEGER, error)

        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "nonsequential", offset, H5T_NATIVE_INTEGER, error)

        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "no_fixup", offset, H5T_NATIVE_INTEGER, error)
                                                       
        call h5dcreate_f(geom_id, "dsm2_node", dtype_id, dspace_id, dset_id, error)        
        
        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt1_id, error)
        offset = 0
        call h5tinsert_f(dt1_id, "dsm2_node_no", offset, dt_id, error)

        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt2_id, error)
        offset = 0
        call h5tinsert_f(dt2_id, "int_node", offset, H5T_NATIVE_INTEGER, error)    
        
        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt3_id, error)
        offset = 0
        call h5tinsert_f(dt3_id, "n_conn_cell", offset, H5T_NATIVE_INTEGER, error)         
 
        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt4_id, error)
        offset = 0
        call h5tinsert_f(dt4_id, "cell_no", offset, H5T_NATIVE_INTEGER, error)  

        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt5_id, error)
        offset = 0
        call h5tinsert_f(dt5_id, "up_down", offset, H5T_NATIVE_INTEGER, error)         
 
        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt6_id, error)
        offset = 0
        call h5tinsert_f(dt6_id, "boundary_no", offset, H5T_NATIVE_INTEGER, error)  

        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt7_id, error)
        offset = 0
        call h5tinsert_f(dt7_id, "junction_no", offset, H5T_NATIVE_INTEGER, error)  

        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt8_id, error)
        offset = 0
        call h5tinsert_f(dt8_id, "reservoir_no", offset, H5T_NATIVE_INTEGER, error)  

        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt9_id, error)
        offset = 0
        call h5tinsert_f(dt9_id, "n_qext", offset, H5T_NATIVE_INTEGER, error)  

        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt10_id, error)
        offset = 0
        call h5tinsert_f(dt10_id, "nonsequential", offset, H5T_NATIVE_INTEGER, error)  
                                
        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt11_id, error)
        offset = 0
        call h5tinsert_f(dt11_id, "no_fixup", offset, H5T_NATIVE_INTEGER, error) 
       
        k = 0
        do i = 1, n_node
            do j = 1, dsm2_node(i)%n_conn_cell
                k = k + 1
                dsm2_node_no(k) = dsm2_node(i)%dsm2_node_no
                int_node(k) = k
                n_conn_cell(k) = dsm2_node(i)%n_conn_cell
                cell_no(k) = dsm2_node(i)%cell_no(j)
                up_down(k) = dsm2_node(i)%up_down(j)
                boundary_no(k) = dsm2_node(i)%boundary_no
                junction_no(k) = dsm2_node(i)%junction_no
                reservoir_no(k) = dsm2_node(i)%reservoir_no
                n_qext(k) = dsm2_node(i)%n_qext
                nonsequential(k) = dsm2_node(i)%nonsequential
                no_fixup(k) = dsm2_node(i)%no_fixup                                                                                  
            end do
        end do
                
        call h5dwrite_f(dset_id, dt1_id, dsm2_node_no, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt2_id, int_node, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt3_id, n_conn_cell, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt4_id, cell_no, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt5_id, up_down, data_dims, error, xfer_prp = plist_id)                        
        call h5dwrite_f(dset_id, dt6_id, boundary_no, data_dims, error, xfer_prp = plist_id)  
        call h5dwrite_f(dset_id, dt7_id, junction_no, data_dims, error, xfer_prp = plist_id)  
        call h5dwrite_f(dset_id, dt8_id, reservoir_no, data_dims, error, xfer_prp = plist_id)  
        call h5dwrite_f(dset_id, dt9_id, n_qext, data_dims, error, xfer_prp = plist_id) 
        call h5dwrite_f(dset_id, dt10_id, nonsequential, data_dims, error, xfer_prp = plist_id)  
        call h5dwrite_f(dset_id, dt11_id, no_fixup, data_dims, error, xfer_prp = plist_id)                 

        call h5dclose_f(dset_id, error)
        call h5sclose_f(dspace_id, error)
        call h5tclose_f(dtype_id, error)
        call h5tclose_f(dt1_id, error)
        call h5tclose_f(dt2_id, error)
        call h5tclose_f(dt3_id, error)
        call h5tclose_f(dt4_id, error)
        call h5tclose_f(dt5_id, error)
        call h5tclose_f(dt6_id, error)
        call h5tclose_f(dt7_id, error)
        call h5tclose_f(dt8_id, error)
        call h5tclose_f(dt9_id, error)
        call h5tclose_f(dt10_id, error)
        call h5tclose_f(dt11_id, error)
        call h5tclose_f(dt_id, error)        
        deallocate(dsm2_node_no, n_conn_cell)
        deallocate(cell_no, up_down)
        deallocate(boundary_no, junction_no)
        deallocate(reservoir_no, n_qext)
        deallocate(nonsequential, no_fixup)     
        return
    end subroutine    


    !> Write out connection info into GTM tidefile
    subroutine write_connection_info(geom_id)
        use hdf5
        use common_variables, only: n_conn, conn 
        implicit none
        integer(HID_T), intent(in) :: geom_id        !< hdf5 dataset identifier
        integer(HID_T) :: dset_id                    ! dataset identifier
        integer(HID_T) :: dspace_id                  ! dataspace identifier
        integer(HID_T) :: dtype_id                   ! compound datatype identifier
        integer(HID_T) :: dt1_id, dt2_id, dt3_id     ! memory datatype identifier
        integer(HID_T) :: dt4_id, dt5_id, dt6_id     ! memory datatype identifier
        integer(HID_T) :: dt_id, dt7_id              ! memory datatype identifier      
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
        
        if (n_conn .eq. 0) then
            write(*,*) "Number of connections = 0"
            return
        end if    
        
        dims = (/n_conn/) 
        data_dims(1) = n_conn
       
        call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, error)
        call h5pset_preserve_f(plist_id, .TRUE., error)
               
        call h5screate_simple_f(rank, dims, dspace_id, error)
        
        call h5tcopy_f(H5T_NATIVE_INTEGER, dt_id, error)
        typesize = 4
        call h5tset_size_f(dt_id, typesize, error)
        call h5tget_size_f(dt_id, type_sizei, error)
        type_size = 7*type_sizei
        
        call h5tcreate_f(H5T_COMPOUND_F, type_size, dtype_id, error)
        
        offset = 0
        call h5tinsert_f(dtype_id, "conn_no", offset, dt_id, error)

        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "segm_no", offset, H5T_NATIVE_INTEGER, error)
        
        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "cell_no", offset, H5T_NATIVE_INTEGER, error)

        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "comp_pt", offset, H5T_NATIVE_INTEGER, error)
        
        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "chan_no", offset, H5T_NATIVE_INTEGER, error)
        
        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "dsm2_node_no", offset, H5T_NATIVE_INTEGER, error)
        
        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "conn_up_down", offset, H5T_NATIVE_INTEGER, error)        
        
        call h5dcreate_f(geom_id, "connection", dtype_id, dspace_id, dset_id, error)        
        
        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt1_id, error)
        offset = 0
        call h5tinsert_f(dt1_id, "conn_no", offset, dt_id, error)
        
        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt2_id, error)
        offset = 0
        call h5tinsert_f(dt2_id, "segm_no", offset, H5T_NATIVE_INTEGER, error)         
 
        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt3_id, error)
        offset = 0
        call h5tinsert_f(dt3_id, "cell_no", offset, H5T_NATIVE_INTEGER, error)  

        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt4_id, error)
        offset = 0
        call h5tinsert_f(dt4_id, "comp_pt", offset, H5T_NATIVE_INTEGER, error)         

        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt5_id, error)
        offset = 0
        call h5tinsert_f(dt5_id, "chan_no", offset, H5T_NATIVE_INTEGER, error)  

        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt6_id, error)
        offset = 0
        call h5tinsert_f(dt6_id, "dsm2_node_no", offset, H5T_NATIVE_INTEGER, error)

        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt7_id, error)
        offset = 0
        call h5tinsert_f(dt7_id, "conn_up_down", offset, H5T_NATIVE_INTEGER, error)
                
        call h5dwrite_f(dset_id, dt1_id, conn%conn_no, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt2_id, conn%segm_no, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt3_id, conn%cell_no, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt4_id, conn%comp_pt, data_dims, error, xfer_prp = plist_id)                        
        call h5dwrite_f(dset_id, dt5_id, conn%chan_no, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt6_id, conn%dsm2_node_no, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt7_id, conn%conn_up_down, data_dims, error, xfer_prp = plist_id)        
        
        call h5dclose_f(dset_id, error)
        call h5sclose_f(dspace_id, error)
        call h5tclose_f(dtype_id, error)
        call h5tclose_f(dt1_id, error)
        call h5tclose_f(dt2_id, error)
        call h5tclose_f(dt3_id, error)
        call h5tclose_f(dt4_id, error)
        call h5tclose_f(dt5_id, error)
        call h5tclose_f(dt6_id, error)
        call h5tclose_f(dt7_id, error)
        call h5tclose_f(dt_id, error)                
        return
    end subroutine


    !> Write out reservoir info into GTM tidefile
    subroutine write_reservoir_info(geom_id)
        use hdf5
        use common_variables, only: n_resv, n_resv_conn, resv_geom
        implicit none
        integer(HID_T), intent(in) :: geom_id            !< hdf5 dataset identifier
        integer(HID_T) :: dset_id                        ! dataset identifier
        integer(HID_T) :: dspace_id                      ! dataspace identifier
        integer(HID_T) :: dtype_id                       ! compound datatype identifier
        integer(HID_T) :: dt1_id, dt2_id, dt3_id, dt_id  ! memory datatype identifier 
        integer(HID_T) :: dt4_id, dt5_id, dt6_id, dt7_id ! memory datatype identifier        
        integer(HID_T) :: plist_id                       ! dataset transfer property
        integer(SIZE_T) :: typesize
        integer(SIZE_T) :: type_size
        integer(SIZE_T) :: type_sizei
        integer(SIZE_T) :: type_sized
        integer(SIZE_T) :: offset
        integer(HSIZE_T), dimension(1) :: data_dims                     
        integer(HSIZE_T), dimension(1) :: dims 
        integer :: resv_index(n_resv_conn), n_res_conn(n_resv_conn)
        integer :: n_res_connection(n_resv_conn), int_node_no(n_resv_conn)
        integer :: ext_node_no(n_resv_conn), is_gated(n_resv_conn)
        real(gtm_real) :: area(n_resv_conn), bot_elev(n_resv_conn)        
        integer :: rank = 1
        integer :: i, j, k
        integer :: error
        
        if (n_resv_conn .eq. 0) then
            write(*,*) "Number of reservoirs = 0"
            return
        end if    
        
        dims = (/n_resv_conn/) 
        data_dims(1) = n_resv_conn
       
        call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, error)
        call h5pset_preserve_f(plist_id, .TRUE., error)
               
        call h5screate_simple_f(rank, dims, dspace_id, error)
        
        call h5tcopy_f(H5T_NATIVE_INTEGER, dt_id, error)
        typesize = 4
        call h5tset_size_f(dt_id, typesize, error)
        call h5tget_size_f(dt_id, type_sizei, error)
        call h5tget_size_f(H5T_NATIVE_DOUBLE, type_sized, error)
        type_size = 5*type_sizei + 2*type_sized
        
        call h5tcreate_f(H5T_COMPOUND_F, type_size, dtype_id, error)
        
        offset = 0
        call h5tinsert_f(dtype_id, "resv_index", offset, dt_id, error)

        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "n_res_conn", offset, H5T_NATIVE_INTEGER, error)

        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "int_node_no", offset, H5T_NATIVE_INTEGER, error)

        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "ext_node_no", offset, H5T_NATIVE_INTEGER, error)

        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "is_gated", offset, H5T_NATIVE_INTEGER, error)

        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "area", offset, H5T_NATIVE_DOUBLE, error)
        
        offset = offset + type_sized
        call h5tinsert_f(dtype_id, "bot_elev", offset, H5T_NATIVE_DOUBLE, error)
        
        call h5dcreate_f(geom_id, "reservoir", dtype_id, dspace_id, dset_id, error)        
        
        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt1_id, error)
        offset = 0
        call h5tinsert_f(dt1_id, "resv_index", offset, dt_id, error)   
 
        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt2_id, error)
        offset = 0
        call h5tinsert_f(dt2_id, "n_res_conn", offset, H5T_NATIVE_INTEGER, error)  

        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt3_id, error)
        offset = 0
        call h5tinsert_f(dt3_id, "int_node_no", offset, H5T_NATIVE_INTEGER, error)
        
        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt4_id, error)
        offset = 0
        call h5tinsert_f(dt4_id, "ext_node_no", offset, H5T_NATIVE_INTEGER, error)

        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt5_id, error)
        offset = 0
        call h5tinsert_f(dt5_id, "is_gated", offset, H5T_NATIVE_INTEGER, error)
        
        call h5tcreate_f(H5T_COMPOUND_F, type_sized, dt6_id, error)
        offset = 0
        call h5tinsert_f(dt6_id, "area", offset, H5T_NATIVE_DOUBLE, error)
        
        call h5tcreate_f(H5T_COMPOUND_F, type_sized, dt7_id, error)
        offset = 0
        call h5tinsert_f(dt7_id, "bot_elev", offset, H5T_NATIVE_DOUBLE, error)

        k = 0
        do i = 1, n_resv
            do j = 1, resv_geom(i)%n_resv_conn   
                k = k + 1
                resv_index(k) = resv_geom(i)%resv_no
                area(k) = resv_geom(i)%area
                bot_elev(k) = resv_geom(i)%bot_elev            
                n_res_connection(k) = resv_geom(i)%n_resv_conn                        
                int_node_no(k) = resv_geom(i)%int_node_no(j)
                ext_node_no(k) = resv_geom(i)%ext_node_no(j)
                is_gated(k) = resv_geom(i)%is_gated(j)
            end do    
        end do
             
        call h5dwrite_f(dset_id, dt1_id, resv_index, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt2_id, n_res_connection, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt3_id, int_node_no, data_dims, error, xfer_prp = plist_id)                     
        call h5dwrite_f(dset_id, dt4_id, ext_node_no, data_dims, error, xfer_prp = plist_id) 
        call h5dwrite_f(dset_id, dt5_id, is_gated, data_dims, error, xfer_prp = plist_id) 
        call h5dwrite_f(dset_id, dt6_id, area, data_dims, error, xfer_prp = plist_id) 
        call h5dwrite_f(dset_id, dt7_id, bot_elev, data_dims, error, xfer_prp = plist_id) 

        call h5dclose_f(dset_id, error)
        call h5sclose_f(dspace_id, error)
        call h5tclose_f(dtype_id, error)
        call h5tclose_f(dt1_id, error)
        call h5tclose_f(dt2_id, error)
        call h5tclose_f(dt3_id, error)
        call h5tclose_f(dt4_id, error)
        call h5tclose_f(dt5_id, error)
        call h5tclose_f(dt6_id, error)                
        call h5tclose_f(dt7_id, error)        
        call h5tclose_f(dt_id, error)        
        return
    end subroutine    
 
 

    !> Write out external flow info into GTM tidefile
    subroutine write_qext_info(geom_id)
        use hdf5
        use common_variables, only: n_qext, qext
        implicit none
        integer(HID_T), intent(in) :: geom_id            !< hdf5 dataset identifier
        integer(HID_T) :: dset_id                        ! dataset identifier
        integer(HID_T) :: dspace_id                      ! dataspace identifier
        integer(HID_T) :: dtype_id                       ! compound datatype identifier
        integer(HID_T) :: dt1_id, dt2_id, dt_id          ! memory datatype identifier 
        integer(HID_T) :: dt3_id, dt4_id                 ! memory datatype identifier        
        integer(HID_T) :: plist_id                       ! dataset transfer property
        integer(SIZE_T) :: typesize
        integer(SIZE_T) :: type_size
        integer(SIZE_T) :: type_sizei
        integer(SIZE_T) :: type_sizec
        integer(SIZE_T) :: offset
        integer(HSIZE_T), dimension(1) :: data_dims                     
        integer(HSIZE_T), dimension(1) :: dims 
        integer :: rank = 1
        integer :: i, j, k
        integer :: error
        
        if (n_qext .eq. 0) then
            write(*,*) "Number of external flows = 0"
            return
        end if    
        
        dims = (/n_qext/) 
        data_dims(1) = n_qext
       
        call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, error)
        call h5pset_preserve_f(plist_id, .TRUE., error)
               
        call h5screate_simple_f(rank, dims, dspace_id, error)
 
        call h5tcopy_f(H5T_NATIVE_CHARACTER, dt_id, error)
        typesize = 32
        call h5tset_size_f(dt_id, typesize, error)
        call h5tget_size_f(dt_id, type_sizec, error)
        
        type_sizei = 4
        type_size = 3*type_sizei + type_sizec
        
        call h5tcreate_f(H5T_COMPOUND_F, type_size, dtype_id, error)
        
        offset = 0
        call h5tinsert_f(dtype_id, "name", offset, dt_id, error)

        offset = offset + type_sizec
        call h5tinsert_f(dtype_id, "qext_index", offset, H5T_NATIVE_INTEGER, error)

        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "attach_obj_type", offset, H5T_NATIVE_INTEGER, error)

        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "attach_obj_no", offset, H5T_NATIVE_INTEGER, error)
        
        call h5dcreate_f(geom_id, "qext", dtype_id, dspace_id, dset_id, error)        
        
        call h5tcreate_f(H5T_COMPOUND_F, type_sizec, dt1_id, error)
        offset = 0
        call h5tinsert_f(dt1_id, "name", offset, dt_id, error)   
 
        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt2_id, error)
        offset = 0
        call h5tinsert_f(dt2_id, "qext_index", offset, H5T_NATIVE_INTEGER, error)  

        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt3_id, error)
        offset = 0
        call h5tinsert_f(dt3_id, "attach_obj_type", offset, H5T_NATIVE_INTEGER, error)
        
        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt4_id, error)
        offset = 0
        call h5tinsert_f(dt4_id, "attach_obj_no", offset, H5T_NATIVE_INTEGER, error)
             
        call h5dwrite_f(dset_id, dt1_id, qext%name, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt2_id, qext%qext_index, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt3_id, qext%attach_obj_type, data_dims, error, xfer_prp = plist_id)                     
        call h5dwrite_f(dset_id, dt4_id, qext%attach_obj_no, data_dims, error, xfer_prp = plist_id) 

        call h5dclose_f(dset_id, error)
        call h5sclose_f(dspace_id, error)
        call h5tclose_f(dtype_id, error)
        call h5tclose_f(dt1_id, error)
        call h5tclose_f(dt2_id, error)
        call h5tclose_f(dt3_id, error)
        call h5tclose_f(dt4_id, error)        
        call h5tclose_f(dt_id, error)        
        return
    end subroutine     
    
end module        