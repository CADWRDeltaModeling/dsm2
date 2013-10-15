

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


    !> Write out channel info into GTM tidefile
    subroutine write_channel_info(geom_id, num_channel, channel)
        use hdf5
        use common_variables, only: channel_t  
        implicit none
        integer(HID_T), intent(in) :: geom_id        !< hdf5 geom dataset identifier
        integer, intent(in) :: num_channel           !< number of channels
        type(channel_t) :: channel(num_channel)      !< channel info
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
 
        if (num_channel .eq. 0) then
            write(*,*) "Number of channels = 0"
            return
        end if    
        
        dims = (/num_channel/) 
        data_dims(1) = num_channel
       
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
                
        call h5dwrite_f(dset_id, dt1_id, channel%chan_no, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt2_id, channel%channel_num, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt3_id, channel%channel_length, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt4_id, channel%up_node, data_dims, error, xfer_prp = plist_id)                        
        call h5dwrite_f(dset_id, dt5_id, channel%down_node, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt6_id, channel%up_comp, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt7_id, channel%down_comp, data_dims, error, xfer_prp = plist_id)        
        
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
    subroutine write_segment_info(geom_id, num_segment, segment)
        use hdf5
        use common_variables, only: segment_t  
        implicit none
        integer(HID_T), intent(in) :: geom_id        !< hdf5 dataset identifier
        integer, intent(in) :: num_segment           !< number of segments
        type(segment_t) :: segment(num_segment)      !< segment info
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
        
        if (num_segment .eq. 0) then
            write(*,*) "Number of segments = 0"
            return
        end if    
        
        dims = (/num_segment/) 
        data_dims(1) = num_segment  
       
        call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, error)
        call h5pset_preserve_f(plist_id, .TRUE., error)
       
        call h5screate_simple_f(rank, dims, dspace_id, error)
        
        call h5tcopy_f(H5T_NATIVE_INTEGER, dt_id, error)
        typesize = 4
        call h5tset_size_f(dt_id, typesize, error)
        call h5tget_size_f(dt_id, type_sizei, error)
        call h5tget_size_f(H5T_NATIVE_DOUBLE, type_sized, error)
        type_size = 4*type_sizei + 3*type_sized
        
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

        call h5tcreate_f(H5T_COMPOUND_F, type_sized, dt5_id, error)
        offset = 0
        call h5tinsert_f(dt5_id, "up_distance", offset, H5T_NATIVE_DOUBLE, error)  

        call h5tcreate_f(H5T_COMPOUND_F, type_sized, dt6_id, error)
        offset = 0
        call h5tinsert_f(dt6_id, "down_distance", offset, H5T_NATIVE_DOUBLE, error)

        call h5tcreate_f(H5T_COMPOUND_F, type_sized, dt7_id, error)
        offset = 0
        call h5tinsert_f(dt7_id, "length", offset, H5T_NATIVE_DOUBLE, error)
                
        call h5dwrite_f(dset_id, dt1_id, segment%segm_no, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt2_id, segment%chan_no, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt3_id, segment%up_comppt, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt4_id, segment%down_comppt, data_dims, error, xfer_prp = plist_id)                        
        call h5dwrite_f(dset_id, dt5_id, segment%up_distance, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt6_id, segment%down_distance, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt7_id, segment%length, data_dims, error, xfer_prp = plist_id)        
        
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


    !> Write out junction info into GTM tidefile
    subroutine write_junction_info(geom_id, num_junction, junction)
        use hdf5
        use common_variables, only: junction_t
        implicit none
        integer(HID_T), intent(in) :: geom_id        !< hdf5 dataset identifier
        integer, intent(in) :: num_junction          !< number of junctions
        type(junction_t) :: junction(num_junction)   !< junction info
        integer(HID_T) :: dset_id                    ! dataset identifier
        integer(HID_T) :: dspace_id                  ! dataspace identifier
        integer(HID_T) :: dtype_id                   ! compound datatype identifier
        integer(HID_T) :: dt1_id, dt2_id, dt3_id     ! memory datatype identifier
        integer(HID_T) :: dt4_id, dt_id              ! memory datatype identifier     
        integer(HID_T) :: plist_id                   ! dataset transfer property
        integer(SIZE_T) :: typesize
        integer(SIZE_T) :: type_size
        integer(SIZE_T) :: type_sizei
        integer(SIZE_T) :: type_sized
        integer(SIZE_T) :: offset
        integer(HSIZE_T), dimension(1) :: data_dims                     
        integer(HSIZE_T), dimension(1) :: dims 
        integer, allocatable :: dsm2_node_no(:)
        integer, allocatable :: n_conn_cells(:)
        integer, allocatable :: cell_no(:)
        integer, allocatable :: up_down(:)        
        integer :: rank = 1
        integer :: i, j, k, n, error
        
        if (num_junction .eq. 0) then
            write(*,*) "Number of junctions = 0"
            return
        end if    
        
        n = 0
        do i = 1, num_junction
            n = n + junction(i)%n_conn_cells
        end do
        allocate(dsm2_node_no(n))
        allocate(n_conn_cells(n))
        allocate(cell_no(n))
        allocate(up_down(n))
                
        dims = (/n/) 
        data_dims(1) = n
       
        call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, error)
        call h5pset_preserve_f(plist_id, .TRUE., error)
               
        call h5screate_simple_f(rank, dims, dspace_id, error)
        
        call h5tcopy_f(H5T_NATIVE_INTEGER, dt_id, error)
        typesize = 4
        call h5tset_size_f(dt_id, typesize, error)
        call h5tget_size_f(dt_id, type_sizei, error)
        type_size = 4*type_sizei
        
        call h5tcreate_f(H5T_COMPOUND_F, type_size, dtype_id, error)
        
        offset = 0
        call h5tinsert_f(dtype_id, "dsm2_node_no", offset, dt_id, error)

        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "n_conn_cells", offset, H5T_NATIVE_INTEGER, error)
        
        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "cell_no", offset, H5T_NATIVE_INTEGER, error)

        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "up_down", offset, H5T_NATIVE_INTEGER, error)
        
        call h5dcreate_f(geom_id, "junction", dtype_id, dspace_id, dset_id, error)        
        
        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt1_id, error)
        offset = 0
        call h5tinsert_f(dt1_id, "dsm2_node_no", offset, dt_id, error)
        
        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt2_id, error)
        offset = 0
        call h5tinsert_f(dt2_id, "n_conn_cells", offset, H5T_NATIVE_INTEGER, error)         
 
        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt3_id, error)
        offset = 0
        call h5tinsert_f(dt3_id, "cell_no", offset, H5T_NATIVE_INTEGER, error)  

        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt4_id, error)
        offset = 0
        call h5tinsert_f(dt4_id, "up_down", offset, H5T_NATIVE_INTEGER, error)         
        
        k = 0
        do i = 1, num_junction
            do j = 1, junction(i)%n_conn_cells
                k = k + 1
                dsm2_node_no(k) = junction(i)%dsm2_node_no
                n_conn_cells(k) = junction(i)%n_conn_cells
                cell_no(k) = junction(i)%cell_no(j)
                up_down(k) = junction(i)%up_down(j)
            end do
        end do
                
        call h5dwrite_f(dset_id, dt1_id, dsm2_node_no, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt2_id, n_conn_cells, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt3_id, cell_no, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt4_id, up_down, data_dims, error, xfer_prp = plist_id)                        

        call h5dclose_f(dset_id, error)
        call h5sclose_f(dspace_id, error)
        call h5tclose_f(dtype_id, error)
        call h5tclose_f(dt1_id, error)
        call h5tclose_f(dt2_id, error)
        call h5tclose_f(dt3_id, error)
        call h5tclose_f(dt4_id, error)
        call h5tclose_f(dt_id, error)        
        deallocate(dsm2_node_no, n_conn_cells)
        deallocate(cell_no, up_down)
        return
    end subroutine    


    !> Write out boundary info into GTM tidefile
    subroutine write_boundary_info(geom_id, num_boundary, boundary)
        use hdf5
        use common_variables, only: boundary_t
        implicit none
        integer(HID_T), intent(in) :: geom_id           !< hdf5 dataset identifier
        integer, intent(in) :: num_boundary             !< number of boundaries
        type(boundary_t) :: boundary(num_boundary)      !< boundary info
        integer(HID_T) :: dset_id                       ! dataset identifier
        integer(HID_T) :: dspace_id                     ! dataspace identifier
        integer(HID_T) :: dtype_id                      ! compound datatype identifier
        integer(HID_T) :: dt1_id, dt2_id, dt3_id, dt_id ! memory datatype identifier 
        integer(HID_T) :: plist_id                      ! dataset transfer property
        integer(SIZE_T) :: typesize
        integer(SIZE_T) :: type_size
        integer(SIZE_T) :: type_sizei
        integer(SIZE_T) :: type_sized
        integer(SIZE_T) :: offset
        integer(HSIZE_T), dimension(1) :: data_dims                     
        integer(HSIZE_T), dimension(1) :: dims 
        integer :: rank = 1
        integer :: error
        
        if (num_boundary .eq. 0) then
            write(*,*) "Number of boundaries = 0"
            return
        end if    
        
        dims = (/num_boundary/) 
        data_dims(1) = num_boundary
       
        call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, error)
        call h5pset_preserve_f(plist_id, .TRUE., error)
               
        call h5screate_simple_f(rank, dims, dspace_id, error)
        
        call h5tcopy_f(H5T_NATIVE_INTEGER, dt_id, error)
        typesize = 4
        call h5tset_size_f(dt_id, typesize, error)
        call h5tget_size_f(dt_id, type_sizei, error)
        type_size = 3*type_sizei
        
        call h5tcreate_f(H5T_COMPOUND_F, type_size, dtype_id, error)
        
        offset = 0
        call h5tinsert_f(dtype_id, "dsm2_node_no", offset, dt_id, error)

        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "cell_no", offset, H5T_NATIVE_INTEGER, error)

        offset = offset + type_sizei
        call h5tinsert_f(dtype_id, "up_down", offset, H5T_NATIVE_INTEGER, error)
        
        call h5dcreate_f(geom_id, "boundary", dtype_id, dspace_id, dset_id, error)        
        
        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt1_id, error)
        offset = 0
        call h5tinsert_f(dt1_id, "dsm2_node_no", offset, dt_id, error)   
 
        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt2_id, error)
        offset = 0
        call h5tinsert_f(dt2_id, "cell_no", offset, H5T_NATIVE_INTEGER, error)  

        call h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt3_id, error)
        offset = 0
        call h5tinsert_f(dt3_id, "up_down", offset, H5T_NATIVE_INTEGER, error)           
                
        call h5dwrite_f(dset_id, dt1_id, boundary%dsm2_node_no, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt2_id, boundary%cell_no, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt3_id, boundary%up_down, data_dims, error, xfer_prp = plist_id)                     

        call h5dclose_f(dset_id, error)
        call h5sclose_f(dspace_id, error)
        call h5tclose_f(dtype_id, error)
        call h5tclose_f(dt1_id, error)
        call h5tclose_f(dt2_id, error)
        call h5tclose_f(dt3_id, error)
        call h5tclose_f(dt_id, error)        
        return
    end subroutine    


    !> Write out connection info into GTM tidefile
    subroutine write_connection_info(geom_id, num_connection, connection)
        use hdf5
        use common_variables, only: conn_t  
        implicit none
        integer(HID_T), intent(in) :: geom_id        !< hdf5 dataset identifier
        integer, intent(in) :: num_connection        !< number of channels
        type(conn_t) :: connection(num_connection)   !< channel info
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
        
        if (num_connection .eq. 0) then
            write(*,*) "Number of connections = 0"
            return
        end if    
        
        dims = (/num_connection/) 
        data_dims(1) = num_connection
       
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
                
        call h5dwrite_f(dset_id, dt1_id, connection%conn_no, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt2_id, connection%segm_no, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt3_id, connection%cell_no, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt4_id, connection%comp_pt, data_dims, error, xfer_prp = plist_id)                        
        call h5dwrite_f(dset_id, dt5_id, connection%chan_no, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt6_id, connection%dsm2_node_no, data_dims, error, xfer_prp = plist_id)
        call h5dwrite_f(dset_id, dt7_id, connection%conn_up_down, data_dims, error, xfer_prp = plist_id)        
        
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
 
    
end module        