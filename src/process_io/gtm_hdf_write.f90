

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

 
    !> Write out geomotry info into Qual tidefile
    subroutine write_segm_info(file_id, num_segment, segment)
        use hdf5
        use common_variables, only: segment_t  
        implicit none
        integer(HID_T), intent(in) :: file_id        !< hdf5 file id
        integer, intent(in) :: num_segment           !< number of segments
        type(segment_t) :: segment(num_segment)      !< segment info
        integer(HID_T) :: geom_id                    ! dataset identifier
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
        integer, dimension(num_segment) :: intnum
        real(gtm_real), dimension(num_segment) :: realnum
        
        dims = (/num_segment/) 
        data_dims(1) = num_segment  
       
        call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, error)
        call h5pset_preserve_f(plist_id, .TRUE., error)
       
        call h5gcreate_f(file_id, "geometry", geom_id, error)
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
        call h5gclose_f(geom_id, error)
        
        return
    end subroutine
    
end module        