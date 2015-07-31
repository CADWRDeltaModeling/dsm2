!<license>
!    Copyright (C) 2015 State of California,
!    Department of Water Resources.
!    This file is part of DSM2-GTM.
!
!    The Delta Simulation Model 2 (DSM2) - General Transport Model (GTM) 
!    is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    DSM2 is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
!</license>

!> This module contains all utilities that are used to process DSM2 hydro tidefile,
!> such as reading time series (flow, water elevation, area), attributes,
!> vistual cross section info. 
!>@ingroup gtm_core
module hdf_util

   use common_variables
   use gtm_precision  
   use error_handling
   use hdf5
   integer(HID_T) :: hydro_file_id        !< HDF5 File identifier
   integer(HID_T) :: hydro_id             !< hydro group identifier      
   integer(HID_T) :: hydro_access_plist   !< HDF5 property identifier

   contains

         
   !> Open HDF5 interface and hydro tidefile, assume HDF5 interface is open
   subroutine hdf5_init(hdf5_file_name)
       implicit none
       character(len=*), intent(in) :: hdf5_file_name     !< HDF5 file name
	   integer(SIZE_T) :: rddc_nelmts
       integer(SIZE_T) :: rddc_nbytes
	   integer :: nelmts
	   real :: rdcc_w0
       integer :: error                                   ! error flag
       logical :: file_exists
       inquire(file = hdf5_file_name, exist=file_exists)
       if (file_exists) then
           	! set up stdio to allow for buffered read/write          	
	       call h5pcreate_f(H5P_FILE_ACCESS_F, hydro_access_plist, error)      
	       call h5pget_cache_f(hydro_access_plist, nelmts,               &
                               rddc_nelmts, rddc_nbytes, rdcc_w0, error)
	       rddc_nbytes = 8000000
	       call h5pset_cache_f(hydro_access_plist, nelmts, rddc_nelmts,  &
                               rddc_nbytes, rdcc_w0,error)
	       call verify_error(error,"Cache set")
           call h5fopen_f (hdf5_file_name, H5F_ACC_RDONLY_F, hydro_file_id, error)
           call verify_error(error, "opening tidefile")
           call h5gopen_f(hydro_file_id, "hydro", hydro_id, error)
       else
           call gtm_fatal(hdf5_file_name//" is not a valid tidefile under working directory!")  
       end if    
   end subroutine
   
   !> Close HDF5 interface and hydro tidefile 
   subroutine hdf5_close()
       implicit none
       integer :: error                                 ! error flag
       call h5gclose_f(hydro_id, error)
       call h5fclose_f(hydro_file_id, error)
       call h5pclose_f(hydro_access_plist, error)                   
       hydro_id  = LARGEINT 
       hydro_file_id  = LARGEINT 
       hydro_access_plist = LARGEINT     
   end subroutine
   
   !> Read number of objects from hydro tidefile
   subroutine get_hydro_attr()
       implicit none        
       call get_int_attribute_from_hdf5(n_chan, "Number of channels")
       call get_int_attribute_from_hdf5(n_xsect, "Number of virt xsects")
       call get_int_attribute_from_hdf5(n_comp, "Number of comp pts")
       call get_int_attribute_from_hdf5(n_resv, "Number of reservoirs")
       call get_int_attribute_from_hdf5(n_resv_conn, "Number of reservoir node connects")
       call get_int_attribute_from_hdf5(n_qext, "Number of QExt")
       call get_int_attribute_from_hdf5(n_tran, "Number of flow transfers")
       call get_int_attribute_from_hdf5(n_gate, "Number of gates")
       call get_int_attribute_from_hdf5(n_flwbnd, "Number of flow boundaries")
       call get_int_attribute_from_hdf5(n_stgbnd, "Number of stage boundaries")
       call get_int_attribute_from_hdf5(hydro_ntideblocks, "Number of intervals")
       call get_int_attribute_from_hdf5(hydro_start_jmin, "Start time")
       call get_int_attribute_from_hdf5(hydro_time_interval, "Time interval")
       hydro_end_jmin = hydro_start_jmin + (hydro_ntideblocks-1)*hydro_time_interval
       return
   end subroutine    


   !> Read time series dataset from hydro tidefile        
   subroutine get_ts_from_hdf5(dset_data, dset_name, n_coln, time_offset, buffer_size)
       implicit none     
       character(len=*), intent(in) :: dset_name                                !< dataset name   
       integer, intent(in) :: time_offset                                       !< time offset                                 
       integer, intent(in) :: buffer_size                                       !< time buffer size
       integer, intent(in) :: n_coln                                            !< number of columns in tidefile
       real(gtm_real), dimension(n_coln, buffer_size), intent(out) :: dset_data !< output array
       integer(HID_T) :: data_id                                                ! local group identifier
       integer(HID_T) :: dset_id                                                ! local dataset identifier
       integer(HID_T) :: dataspace                                              ! Dataspace identifier
       integer(HID_T) :: memspace                                               ! memspace identifier
       integer(HSIZE_T), dimension(2) :: count                                  ! local variable
       integer(HSIZE_T), dimension(2) :: offset                                 ! local variable
       integer(HSIZE_T), dimension(2) :: offset_out                             ! local variable      
       integer(HSIZE_T), dimension(2) :: data_dims                              ! local datasets dimensions
       integer(HSIZE_T), dimension(2) :: dimsm                                  ! local variable
       integer :: memrank = 2                                                   ! memory rank
       integer :: error                                                         ! error flag              
       call h5gopen_f(hydro_id, "data", data_id, error)
       call h5dopen_f(data_id, dset_name, dset_id, error) 
       call h5dget_space_f(dset_id, dataspace, error)
       dimsm = (/n_coln, buffer_size/)
       offset = (/0,time_offset/)
       count = (/n_coln, buffer_size/) 
       offset_out = (/0,0/)
       call h5sselect_hyperslab_f(dataspace, H5S_SELECT_SET_F, offset, count, error)
       call h5screate_simple_f(memrank, dimsm, memspace, error)
       call h5sselect_hyperslab_f(memspace, H5S_SELECT_SET_F, offset_out, count, error)
       data_dims(1) = n_coln
       data_dims(2) =  buffer_size
       call h5dread_f(dset_id, H5T_NATIVE_DOUBLE, dset_data, data_dims, error, memspace, dataspace)
       
       call h5sclose_f(dataspace, error)
       call h5sclose_f(memspace, error)
       call h5dclose_f(dset_id, error)
       call h5gclose_f(data_id, error)
       return     
   end subroutine  
 
   
   !> Read virtual cross section table from hydro tidefile
   subroutine read_xsect_tbl()
       use common_xsect
       implicit none   
       integer(HID_T) :: geom_id                            ! group identifier
       integer(HID_T) :: dset_id                            ! dataset identifier
       integer(HID_T) :: dt_id                              ! memory datatype identifier
       integer(HID_T) :: dt1_id, dt2_id, dt3_id             ! memory datatype identifier
       integer(HID_T) :: dt4_id, dt5_id, dt6_id             ! memory datatype identifier   
       integer(HID_T) :: dt7_id, dt8_id, dt9_id             ! memory datatype identifier
       integer(SIZE_T):: offset                             ! member's offset
       integer(HSIZE_T), dimension(1) :: dims               ! dataset dimensions
       integer(HSIZE_T), dimension(1) :: data_dims          ! datasets dimensions
       integer(SIZE_T) :: typesize                          ! local variable
       integer(SIZE_T) :: type_size                         ! size of the datatype
       integer :: error                                     ! error flag
       integer :: i, j, k                                   ! local variable
       integer, dimension(n_xsect) :: chan_no, num_virt_sec, vsecno, num_elev
       real(gtm_real), dimension(n_xsect) :: min_elev, elevation, area, wet_p, width  
       
       dims = (/n_xsect/)
       data_dims(1) = n_xsect   

       call h5gopen_f(hydro_id, "geometry", geom_id, error)
       call h5dopen_f(geom_id, "virtual_xsect", dset_id, error) 

       call h5tcopy_f(H5T_NATIVE_INTEGER, dt_id, error)
       typesize = 4
       call h5tset_size_f(dt_id, typesize, error)
       call h5tget_size_f(dt_id, type_size, error)
      
       offset = 0      
       call h5tcreate_f(H5T_COMPOUND_F, type_size, dt1_id, error)
       call h5tinsert_f(dt1_id, "chan_no", offset, dt_id, error)    
       call h5dread_f(dset_id, dt1_id, chan_no, data_dims, error)

       call h5tcreate_f(H5T_COMPOUND_F, type_size, dt2_id, error)
       call h5tinsert_f(dt2_id, "num_virt_sec", offset, H5T_NATIVE_INTEGER, error)    
       call h5dread_f(dset_id, dt2_id, num_virt_sec, data_dims, error)

       call h5tcreate_f(H5T_COMPOUND_F, type_size, dt3_id, error) 
       call h5tinsert_f(dt3_id, "vsecno", offset, H5T_NATIVE_INTEGER, error)    
       call h5dread_f(dset_id, dt3_id, vsecno, data_dims, error)      

       call h5tcreate_f(H5T_COMPOUND_F, type_size, dt4_id, error)
       call h5tinsert_f(dt4_id, "num_elev", offset, H5T_NATIVE_INTEGER, error)    
       call h5dread_f(dset_id, dt4_id, num_elev, data_dims, error)                      
       
       call h5tcreate_f(H5T_COMPOUND_F, gtm_real, dt5_id, error)
       call h5tinsert_f(dt5_id, "min_elev", offset, H5T_NATIVE_DOUBLE, error)       
       call h5dread_f(dset_id, dt5_id, min_elev, data_dims, error)             
 
       call h5tcreate_f(H5T_COMPOUND_F, gtm_real, dt6_id, error)
       call h5tinsert_f(dt6_id, "elevation", offset, H5T_NATIVE_DOUBLE, error)    
       call h5dread_f(dset_id, dt6_id, elevation, data_dims, error)            
 
       call h5tcreate_f(H5T_COMPOUND_F, gtm_real, dt7_id, error)
       call h5tinsert_f(dt7_id, "area", offset, H5T_NATIVE_DOUBLE, error)     
       call h5dread_f(dset_id, dt7_id, area, data_dims, error)             
 
       call h5tcreate_f(H5T_COMPOUND_F, gtm_real, dt8_id, error)
       call h5tinsert_f(dt8_id, "wet_p", offset, H5T_NATIVE_DOUBLE, error)   
       call h5dread_f(dset_id, dt8_id, wet_p, data_dims, error)            
      
       call h5tcreate_f(H5T_COMPOUND_F, gtm_real, dt9_id, error)
       call h5tinsert_f(dt9_id, "width", offset, H5T_NATIVE_DOUBLE, error)    
       call h5dread_f(dset_id, dt9_id, width, data_dims, error)    
       
       call allocate_chan_virt_xsect()
       
       call calc_virt_xsect_dimension(chan_no, num_virt_sec, num_elev)
      
       call allocate_virt_xsec_geom()  
      
       i = 1 
       j = 1
       do while (j < n_xsect)     
          virt_xsect(i)%chan_no = chan_no(j)
          virt_xsect(i)%num_virt_sec = num_virt_sec(j)
          virt_xsect(i)%vsecno = vsecno(j)
          virt_xsect(i)%num_elev = num_elev(j)
          virt_xsect(i)%min_elev = min_elev(j)
          do k=1,num_elev(j)
             virt_xsect(i)%elevation(k) = elevation(j+k-1)
             virt_xsect(i)%area(k) = area(j+k-1)
             virt_xsect(i)%wet_p(k) = wet_p(j+k-1)
             virt_xsect(i)%width(k) = width(j+k-1)
          end do
          i = i + 1
          j = j + num_elev(j)
       end do
       
       call h5tclose_f(dt_id, error)
       call h5tclose_f(dt1_id, error)
       call h5tclose_f(dt2_id, error)
       call h5tclose_f(dt3_id, error)
       call h5tclose_f(dt4_id, error)
       call h5tclose_f(dt5_id, error)
       call h5tclose_f(dt6_id, error)
       call h5tclose_f(dt7_id, error)
       call h5tclose_f(dt8_id, error)
       call h5tclose_f(dt9_id, error)      
       call h5dclose_f(dset_id, error) 
       call h5gclose_f(geom_id, error)               
       return        
   end subroutine
 
    
   !> Read input/channel table from hydro tidefile
   subroutine read_channel_tbl()
       use common_variables
       implicit none
       integer(HID_T) :: input_id                   ! Group identifier
       integer(HID_T) :: dset_id                    ! Dataset identifier
       integer(HID_T) :: dt_id                      ! Memory datatype identifier
       integer(HID_T) :: dt1_id, dt2_id             ! Memory datatype identifier
       integer(HID_T) :: dt3_id, dt4_id             ! Memory datatype identifier
       integer(HID_T) :: dt5_id                     ! Memory datatype identifier
       integer(SIZE_T):: offset                     ! Member's offset
       integer(HSIZE_T), dimension(1) :: data_dims  ! Datasets dimensions
       integer(SIZE_T) :: typesize                  ! Size of the datatype
       integer(SIZE_T) :: type_size                 ! Size of the datatype
       integer :: error                             ! Error flag
       integer :: i
      
       data_dims(1) = n_chan
       call allocate_channel_property()
       call h5gopen_f(hydro_id, "input", input_id, error)
       call h5dopen_f(input_id, "channel", dset_id, error) 

       call h5tcopy_f(H5T_NATIVE_INTEGER, dt_id, error)
       typesize = 4
       call h5tset_size_f(dt_id, typesize, error)
       call h5tget_size_f(dt_id, type_size, error)
       do i = 1,n_chan
           chan_geom(i)%chan_no = i
       end do    
       offset = 0      
       call h5tcreate_f(H5T_COMPOUND_F, type_size, dt1_id, error)
       call h5tinsert_f(dt1_id, "chan_no", offset, dt_id, error)    
       call h5dread_f(dset_id, dt1_id, chan_geom%channel_num, data_dims, error) 

       call h5tcreate_f(H5T_COMPOUND_F, type_size, dt2_id, error)
       call h5tinsert_f(dt2_id, "length", offset, H5T_NATIVE_INTEGER, error)    
       call h5dread_f(dset_id, dt2_id, chan_geom%channel_length, data_dims, error)

       call h5tcreate_f(H5T_COMPOUND_F, type_size, dt3_id, error)
       call h5tinsert_f(dt3_id, "upnode", offset, H5T_NATIVE_INTEGER, error)    
       call h5dread_f(dset_id, dt3_id, chan_geom%up_node, data_dims, error)

       call h5tcreate_f(H5T_COMPOUND_F, type_size, dt4_id, error)
       call h5tinsert_f(dt4_id, "downnode", offset, H5T_NATIVE_INTEGER, error)    
       call h5dread_f(dset_id, dt4_id, chan_geom%down_node, data_dims, error)

       call h5tcreate_f(H5T_COMPOUND_F, gtm_real, dt5_id, error)
       call h5tinsert_f(dt5_id, "dispersion", offset, H5T_NATIVE_DOUBLE, error)    
       call h5dread_f(dset_id, dt5_id, chan_geom%dispersion, data_dims, error)       
                     
       call h5tclose_f(dt_id, error)
       call h5tclose_f(dt1_id, error)
       call h5tclose_f(dt2_id, error)  
       call h5tclose_f(dt3_id, error)
       call h5tclose_f(dt4_id, error)             
       call h5tclose_f(dt5_id, error)
       call h5dclose_f(dset_id, error)  
       call h5gclose_f(input_id, error)              
       return        
   end subroutine  
 
 
   !> Read geometry/hydro_comp_point table from hydro tidefile
   subroutine read_comp_tbl()
       use common_variables
       implicit none
       integer(HID_T) :: geom_id                    ! Group identifier
       integer(HID_T) :: dset_id                    ! Dataset identifier
       integer(HID_T) :: dt_id                      ! Memory datatype identifier
       integer(HID_T) :: dt1_id, dt2_id, dt3_id     ! Memory datatype identifier
       integer(SIZE_T):: offset                     ! Member's offset
       integer(HSIZE_T), dimension(1) :: data_dims  ! Datasets dimensions
       integer(SIZE_T) :: typesize                  ! Size of the datatype
       integer(SIZE_T) :: type_size                 ! Size of the datatype
       integer :: error                             ! Error flag
       integer :: i
      
       data_dims(1) = n_comp
      
       call allocate_comp_pt_property()
      
       call h5gopen_f(hydro_id, "geometry", geom_id, error)
       call h5dopen_f(geom_id, "hydro_comp_point", dset_id, error) 

       call h5tcopy_f(H5T_NATIVE_INTEGER, dt_id, error)
       typesize = 4
       call h5tset_size_f(dt_id, typesize, error)
       call h5tget_size_f(dt_id, type_size, error)
        
       offset = 0      
       call h5tcreate_f(H5T_COMPOUND_F, type_size, dt1_id, error)
       call h5tinsert_f(dt1_id, "comp_index", offset, dt_id, error)    
       call h5dread_f(dset_id, dt1_id, comp_pt%comp_index, data_dims, error)
       
       call h5tcreate_f(H5T_COMPOUND_F, type_size, dt2_id, error)
       call h5tinsert_f(dt2_id, "channel", offset, H5T_NATIVE_INTEGER, error)    
       call h5dread_f(dset_id, dt2_id, comp_pt%chan_no, data_dims, error)

       call h5tcreate_f(H5T_COMPOUND_F, gtm_real, dt3_id, error)
       call h5tinsert_f(dt3_id, "distance", offset, H5T_NATIVE_DOUBLE, error)    
       call h5dread_f(dset_id, dt3_id, comp_pt%distance, data_dims, error)
       
       call h5tclose_f(dt_id, error)
       call h5tclose_f(dt1_id, error)
       call h5tclose_f(dt2_id, error)
       call h5tclose_f(dt3_id, error)            
       call h5dclose_f(dset_id, error)                
       call h5gclose_f(geom_id, error)
       return        
   end subroutine  


   !> Read input/reservoir table from hydro tidefile
   subroutine read_reservoir_tbl()
       use common_variables, only : n_resv, n_resv_conn, resv_geom, allocate_reservoir_property
       implicit none
       integer(HID_T) :: geom_id                        ! Group identifier       
       integer(HID_T) :: input_id                       ! Group identifier
       integer(HID_T) :: dset_id                        ! Dataset identifier
       integer(HID_T) :: dt_id                          ! Memory datatype identifier
       integer(HID_T) :: dt1_id, dt2_id, dt3_id         ! Memory datatype identifier
       integer(HID_T) :: dt4_id, dt5_id, dt6_id, dt7_id ! Memory datatype identifier
       integer(SIZE_T):: offset                         ! Member's offset
       integer(HSIZE_T), dimension(1) :: data_dims      ! Datasets dimensions
       integer(SIZE_T) :: typesize                      ! Size of the datatype
       integer(SIZE_T) :: type_size                     ! Size of the datatype
       integer :: error                                 ! Error flag
       character*8 :: node_type(n_resv_conn)            ! local variables
       integer :: int_node(n_resv_conn)                 ! local variables
       integer :: ext_node(n_resv_conn)                 ! local variables
       integer :: resv_index(n_resv_conn)               ! local variables
       integer :: i, j, k
       character*8 :: node_str
       
       node_type = ' '
       data_dims(1) = n_resv
       call allocate_reservoir_property()
       if (n_resv > 0) then
           call h5gopen_f(hydro_id, "input", input_id, error)
           call h5dopen_f(input_id, "reservoir", dset_id, error) 
           call h5tcopy_f(H5T_NATIVE_CHARACTER, dt_id, error)
           typesize = 32  ! the first column is charater, use typesize 32 to avoid reading errors.
           call h5tset_size_f(dt_id, typesize, error)
           call h5tget_size_f(dt_id, type_size, error)
           
            do i = 1,n_resv
               resv_geom(i)%resv_no = i
           end do     
           
           offset = 0
           call h5tcreate_f(H5T_COMPOUND_F, type_size, dt1_id, error)
           call h5tinsert_f(dt1_id, "name", offset, dt_id, error)    
           call h5dread_f(dset_id, dt1_id, resv_geom%name, data_dims, error) 
           
           type_size = 8
           call h5tcreate_f(H5T_COMPOUND_F, type_size, dt2_id, error)
           call h5tinsert_f(dt2_id, "area", offset, H5T_NATIVE_DOUBLE, error)    
           call h5dread_f(dset_id, dt2_id, resv_geom%area, data_dims, error)

           call h5tcreate_f(H5T_COMPOUND_F, type_size, dt3_id, error)
           call h5tinsert_f(dt3_id, "bot_elev", offset, H5T_NATIVE_DOUBLE, error)    
           call h5dread_f(dset_id, dt3_id, resv_geom%bot_elev, data_dims, error)
                       
           call h5tclose_f(dt_id, error)
           call h5tclose_f(dt1_id, error)
           call h5tclose_f(dt2_id, error)  
           call h5tclose_f(dt3_id, error)          
           call h5dclose_f(dset_id, error)  
           call h5gclose_f(input_id, error)
       
           ! read node connected to reservoir
           data_dims(1) = n_resv_conn
           call h5gopen_f(hydro_id, "geometry", geom_id, error)
           call h5dopen_f(geom_id, "reservoir_node_connect", dset_id, error) 

           call h5tcopy_f(H5T_NATIVE_INTEGER, dt_id, error)
           typesize = 4
           call h5tset_size_f(dt_id, typesize, error)
           call h5tget_size_f(dt_id, type_size, error)
        
           offset = 0
           call h5tcreate_f(H5T_COMPOUND_F, type_size, dt4_id, error)
           call h5tinsert_f(dt4_id, "node_no", offset, dt_id, error)    
           call h5dread_f(dset_id, dt4_id, int_node, data_dims, error)

           call h5tcreate_f(H5T_COMPOUND_F, type_size, dt5_id, error)
           call h5tinsert_f(dt5_id, "ext_node_no", offset, H5T_NATIVE_INTEGER, error)    
           call h5dread_f(dset_id, dt5_id, ext_node, data_dims, error)

           call h5tcreate_f(H5T_COMPOUND_F, type_size, dt6_id, error)
           call h5tinsert_f(dt6_id, "res_index", offset, H5T_NATIVE_INTEGER, error)    
           call h5dread_f(dset_id, dt6_id, resv_index, data_dims, error)
 
           type_size = 8
           call h5tcreate_f(H5T_COMPOUND_F, type_size, dt7_id, error)
           call h5tinsert_f(dt7_id, "connection_type", offset, H5T_NATIVE_CHARACTER, error)    
           call h5dread_f(dset_id, dt7_id, node_type, data_dims, error)
           
           do i = 1, n_resv
               resv_geom(i)%n_resv_conn = 0
               do j = 1, n_resv_conn
                   if (resv_index(j)==i) then
                       resv_geom(i)%n_resv_conn = resv_geom(i)%n_resv_conn + 1
                   end if
               end do
               allocate(resv_geom(i)%resv_conn_no(resv_geom(i)%n_resv_conn))
               allocate(resv_geom(i)%int_node_no(resv_geom(i)%n_resv_conn))
               allocate(resv_geom(i)%ext_node_no(resv_geom(i)%n_resv_conn))
               allocate(resv_geom(i)%network_id(resv_geom(i)%n_resv_conn))
               allocate(resv_geom(i)%is_gated(resv_geom(i)%n_resv_conn))
           end do
           
           k = 0
           do i = 1, n_resv
               do j = 1, resv_geom(i)%n_resv_conn
                   k = k + 1
                   resv_geom(i)%resv_conn_no(j) = k
                   resv_geom(i)%int_node_no(j) = int_node(k)
                   resv_geom(i)%ext_node_no(j) = ext_node(k)
                   node_str = node_type(k)
                   if (node_str(1:1)=='g') then
                       resv_geom(i)%is_gated(j) = 1
                   else 
                       resv_geom(i)%is_gated(j) = 0
                   end if
               end do        
           end do
             
           call h5tclose_f(dt_id, error)
           call h5tclose_f(dt4_id, error)
           call h5tclose_f(dt5_id, error)
           call h5tclose_f(dt6_id, error)
           call h5tclose_f(dt7_id, error)
           call h5dclose_f(dset_id, error)                
           call h5gclose_f(geom_id, error)                    
       end if    
       return        
   end subroutine  


   !> Read input/boundary_flow, source_flow and source_flow_reservoir tables from hydro tidefile to fill in qext
   subroutine read_qext_tbl()
       use common_variables, only : n_qext, qext, allocate_qext_property
       implicit none
       integer(HID_T) :: geom_id                        ! Group identifier
       integer(HID_T) :: dset_id                        ! Dataset identifier
       integer(HID_T) :: dt_id                          ! Memory datatype identifier
       integer(HID_T) :: dt1_id, dt2_id                 ! Memory datatype identifier
       integer(HID_T) :: dt3_id, dt4_id                 ! Memory datatype identifier
       integer(SIZE_T):: offset                         ! Member's offset
       integer(HSIZE_T), dimension(1) :: data_dims      ! Datasets dimensions
       integer(SIZE_T) :: typesize                      ! Size of the datatype
       integer(SIZE_T) :: type_size                     ! Size of the datatype
       integer :: error                                 ! Error flag
       integer :: i
       character*32 :: attach_obj_name(n_qext)
       
       data_dims(1) = n_qext
       call allocate_qext_property()
       if (n_qext > 0) then
           call h5gopen_f(hydro_id, "geometry", geom_id, error)
           call h5dopen_f(geom_id, "qext", dset_id, error) 
           call h5tcopy_f(H5T_NATIVE_CHARACTER, dt_id, error)
           typesize = 32  ! the first column is charater, use typesize 32 to avoid reading errors.
           call h5tset_size_f(dt_id, typesize, error)
           call h5tget_size_f(dt_id, type_size, error)
           
           offset = 0
           call h5tcreate_f(H5T_COMPOUND_F, type_size, dt1_id, error)
           call h5tinsert_f(dt1_id, "attach_obj_name", offset, dt_id, error)    
           call h5dread_f(dset_id, dt1_id, attach_obj_name, data_dims, error) 
           
           call h5tcreate_f(H5T_COMPOUND_F,type_size, dt2_id, error)
           call h5tinsert_f(dt2_id, "name", offset, dt_id, error)    
           call h5dread_f(dset_id, dt2_id, qext%name, data_dims, error)   
                                                
           type_size = 4
           call h5tcreate_f(H5T_COMPOUND_F, type_size, dt3_id, error)
           call h5tinsert_f(dt3_id, "attached_obj_type", offset, H5T_NATIVE_INTEGER, error)    
           call h5dread_f(dset_id, dt3_id, qext%attach_obj_type, data_dims, error)

           call h5tcreate_f(H5T_COMPOUND_F, type_size, dt4_id, error)
           call h5tinsert_f(dt4_id, "attached_obj_no", offset, H5T_NATIVE_INTEGER, error)    
           call h5dread_f(dset_id, dt4_id, qext%attach_obj_no, data_dims, error)        
 
           do i = 1, n_qext
               qext(i)%qext_no = i
               if (qext(i)%attach_obj_type .eq. 2) then      ! node object
                   read(attach_obj_name(i),'(i)') qext(i)%attach_obj_name
               elseif (qext(i)%attach_obj_type .eq. 3) then  ! reservoir object 
                   qext(i)%attach_obj_name = 0  !todo:need to convert to internal reservoir no
                   
               end if    
           end do
                    
           call h5tclose_f(dt4_id, error)                       
           call h5tclose_f(dt3_id, error)          
           call h5tclose_f(dt2_id, error)  
           call h5tclose_f(dt1_id, error)         
           call h5tclose_f(dt_id, error)
           call h5dclose_f(dset_id, error) 
           call h5gclose_f(geom_id, error) 
       end if    
       return        
   end subroutine  
   

   !> Read transfer flow tables from hydro tidefile to fill in transfer flow
   subroutine read_tran_tbl()
       use common_variables, only : n_tran, tran, allocate_tran_property
       implicit none
       integer(HID_T) :: input_id                       ! Group identifier
       integer(HID_T) :: dset_id                        ! Dataset identifier
       integer(HID_T) :: dt_id, dt5_id                  ! Memory datatype identifier
       integer(HID_T) :: dt1_id, dt2_id                 ! Memory datatype identifier
       integer(HID_T) :: dt3_id, dt4_id                 ! Memory datatype identifier
       integer(SIZE_T):: offset                         ! Member's offset
       integer(HSIZE_T), dimension(1) :: data_dims      ! Datasets dimensions
       integer(SIZE_T) :: typesize                      ! Size of the datatype
       integer(SIZE_T) :: type_size                     ! Size of the datatype
       integer :: error                                 ! Error flag
       integer :: i, from_i, to_i                       ! local variables
       character*32 :: from_obj, from_identifier        ! local variables
       character*32 :: to_obj, to_identifier            ! local variables
       
       data_dims(1) = n_tran
       call allocate_tran_property()
       if (n_tran > 0) then
           call h5gopen_f(hydro_id, "input", input_id, error)
           call h5dopen_f(input_id, "transfer", dset_id, error) 
           call h5tcopy_f(H5T_NATIVE_CHARACTER, dt_id, error)
           typesize = 32  ! the first column is charater, use typesize 32 to avoid reading errors.
           call h5tset_size_f(dt_id, typesize, error)
           call h5tget_size_f(dt_id, type_size, error)
           
           offset = 0
           call h5tcreate_f(H5T_COMPOUND_F, type_size, dt1_id, error)
           call h5tinsert_f(dt1_id, "name", offset, dt_id, error)    
           call h5dread_f(dset_id, dt1_id, tran%name, data_dims, error) 
           
           call h5tcreate_f(H5T_COMPOUND_F, type_size, dt2_id, error)
           call h5tinsert_f(dt2_id, "from_obj", offset, H5T_NATIVE_CHARACTER, error)    
           call h5dread_f(dset_id, dt2_id, from_obj, data_dims, error)           
           
           call h5tcreate_f(H5T_COMPOUND_F, type_size, dt3_id, error)
           call h5tinsert_f(dt3_id, "from_identifier", offset, H5T_NATIVE_CHARACTER, error)    
           call h5dread_f(dset_id, dt3_id, from_identifier, data_dims, error)

           call h5tcreate_f(H5T_COMPOUND_F, type_size, dt4_id, error)
           call h5tinsert_f(dt4_id, "to_obj", offset, H5T_NATIVE_CHARACTER, error)    
           call h5dread_f(dset_id, dt4_id, to_obj, data_dims, error)        

           call h5tcreate_f(H5T_COMPOUND_F, type_size, dt5_id, error)
           call h5tinsert_f(dt5_id, "to_identifier", offset, H5T_NATIVE_CHARACTER, error)    
           call h5dread_f(dset_id, dt5_id, to_identifier, data_dims, error)    
           
           call h5tclose_f(dt5_id, error)
           call h5tclose_f(dt4_id, error)                       
           call h5tclose_f(dt3_id, error)          
           call h5tclose_f(dt2_id, error)  
           call h5tclose_f(dt1_id, error)         
           call h5tclose_f(dt_id, error)
           call h5dclose_f(dset_id, error) 
           call h5gclose_f(input_id, error) 
           
           do i = 1, n_tran
               tran(i)%tran_no = i
               if (trim(from_obj)=='NODE') tran(i)%from_obj = 2
               if (trim(from_obj)=='RESERVOIR') tran(i)%from_obj = 3
               if (trim(to_obj)=='NODE') tran(i)%from_obj = 2
               if (trim(to_obj)=='RESERVOIR') tran(i)%from_obj = 3
               read(from_identifier,'(i)') from_i
               read(to_identifier,'(i)') to_i
               tran(i)%from_identifier = from_i
               tran(i)%to_identifier = to_i               
           end do            
       end if    
       return        
   end subroutine  


   !> Read input/gate table from hydro tidefile
   subroutine read_gate_tbl()
       use common_variables
       implicit none
       integer(HID_T) :: input_id                   ! Group identifier
       integer(HID_T) :: dset_id                    ! Dataset identifier
       integer(HID_T) :: dt_id                      ! Memory datatype identifier
       integer(HID_T) :: dt1_id, dt2_id             ! Memory datatype identifier
       integer(HID_T) :: dt3_id, dt4_id             ! Memory datatype identifier
       integer(HID_T) :: dt5_id                     ! Memory datatype identifier
       integer(SIZE_T):: offset                     ! Member's offset
       integer(HSIZE_T), dimension(1) :: data_dims  ! Datasets dimensions
       integer(SIZE_T) :: typesize                  ! Size of the datatype
       integer(SIZE_T) :: type_size                 ! Size of the datatype
       character*32, allocatable :: from_obj(:)     ! from_obj
       integer :: error                             ! Error flag
       integer :: i
      
       call h5gopen_f(hydro_id, "input", input_id, error)
       call h5dopen_f(input_id, "gate", dset_id, error) 
         
       data_dims(1) = n_gate
       call allocate_gate_property()
       if (n_gate > 0) then
           allocate(from_obj(n_gate))       
           call h5tcopy_f(H5T_NATIVE_CHARACTER, dt_id, error)
           typesize = 32  ! the first column is charater, use typesize 32 to avoid reading errors.
           call h5tset_size_f(dt_id, typesize, error)
           call h5tget_size_f(dt_id, type_size, error)         
         
           offset = 0
           call h5tcreate_f(H5T_COMPOUND_F, type_size, dt1_id, error)
           call h5tinsert_f(dt1_id, "name", offset, dt_id, error)    
           call h5dread_f(dset_id, dt1_id, gate%name, data_dims, error) 
           
           call h5tcreate_f(H5T_COMPOUND_F, type_size, dt2_id, error)
           call h5tinsert_f(dt2_id, "from_obj", offset, dt_id, error)    
           call h5dread_f(dset_id, dt2_id, from_obj, data_dims, error)

           call h5tcreate_f(H5T_COMPOUND_F, type_size, dt3_id, error)
           call h5tinsert_f(dt3_id, "from_identifier", offset, dt_id, error)    
           call h5dread_f(dset_id, dt3_id, gate%from_identifier, data_dims, error)
       
           type_size = 4
           call h5tcreate_f(H5T_COMPOUND_F, type_size, dt4_id, error)
           call h5tinsert_f(dt4_id, "to_node", offset, H5T_NATIVE_INTEGER, error)    
           call h5dread_f(dset_id, dt4_id, gate%to_node, data_dims, error)  

           do i = 1, n_gate
               gate(i)%gate_no = i
               if (trim(from_obj(i)).eq.'channel') then
                   gate(i)%from_obj_int = 1
               elseif(trim(from_obj(i)).eq.'reservoir') then
                   gate(i)%from_obj_int = 2
               end if    
           end do                                                                  
                       
           call h5tclose_f(dt1_id, error)
           call h5tclose_f(dt2_id, error)  
           call h5tclose_f(dt3_id, error)   
           call h5tclose_f(dt_id, error)               
           call h5dclose_f(dset_id, error)  
           call h5gclose_f(input_id, error)              
           deallocate(from_obj)
       end if    
       return        
   end subroutine   

   !> Read boundary flow tables from hydro tidefile
   subroutine read_boundary_tbl()
       use common_variables, only : n_flwbnd, n_stgbnd, bfbs
       implicit none
       integer(HID_T) :: input_id                       ! Group identifier
       integer(HID_T) :: dset_id                        ! Dataset identifier
       integer(HID_T) :: dt_id, dt5_id                  ! Memory datatype identifier
       integer(HID_T) :: dt1_id, dt2_id                 ! Memory datatype identifier
       integer(SIZE_T):: offset                         ! Member's offset
       integer(HSIZE_T), dimension(1) :: data_dims      ! Datasets dimensions
       integer(SIZE_T) :: typesize                      ! Size of the datatype
       integer(SIZE_T) :: type_size                     ! Size of the datatype
       integer :: error                                 ! Error flag
       integer :: i, from_i, to_i                       ! local variables
       character*32 :: from_obj, from_identifier        ! local variables
       character*32 :: to_obj, to_identifier            ! local variables
       
       n_bfbs = n_flwbnd + n_stgbnd
       call allocate_bfbs_property()
       call h5gopen_f(hydro_id, "input", input_id, error)  
       
       data_dims(1) = n_flwbnd            
       if (n_flwbnd > 0) then
           call h5dopen_f(input_id, "boundary_flow", dset_id, error) 
           call h5tcopy_f(H5T_NATIVE_CHARACTER, dt_id, error)
           typesize = 32  ! the first column is charater, use typesize 32 to avoid reading errors.
           call h5tset_size_f(dt_id, typesize, error)
           call h5tget_size_f(dt_id, type_size, error)
                      
           bfbs(1:n_flwbnd)%btype = "flow"
                      
           offset = 0
           call h5tcreate_f(H5T_COMPOUND_F, type_size, dt1_id, error)
           call h5tinsert_f(dt1_id, "name", offset, dt_id, error)    
           call h5dread_f(dset_id, dt1_id, bfbs%name, data_dims, error) 
           
           type_size = 4
           call h5tcreate_f(H5T_COMPOUND_F, type_size, dt2_id, error)
           call h5tinsert_f(dt2_id, "node", offset, H5T_NATIVE_INTEGER, error)    
           call h5dread_f(dset_id, dt2_id, bfbs%node, data_dims, error)

           call h5tclose_f(dt2_id, error)  
           call h5tclose_f(dt1_id, error)         
           call h5tclose_f(dt_id, error)
           call h5dclose_f(dset_id, error) 
       end if
       
       data_dims(1) = n_stgbnd
       if (n_stgbnd > 0) then
           call h5dopen_f(input_id, "boundary_stage", dset_id, error) 
           call h5tcopy_f(H5T_NATIVE_CHARACTER, dt_id, error)
           typesize = 32  ! the first column is charater, use typesize 32 to avoid reading errors.
           call h5tset_size_f(dt_id, typesize, error)
           call h5tget_size_f(dt_id, type_size, error)
           
           bfbs(n_flwbnd+1)%btype = "stage"  
           offset = 0
           call h5tcreate_f(H5T_COMPOUND_F, type_size, dt1_id, error)
           call h5tinsert_f(dt1_id, "name", offset, dt_id, error)    
           call h5dread_f(dset_id, dt1_id, bfbs(n_flwbnd+1)%name, data_dims, error) 
           
           type_size = 4
           call h5tcreate_f(H5T_COMPOUND_F, type_size, dt2_id, error)
           call h5tinsert_f(dt2_id, "node", offset, H5T_NATIVE_INTEGER, error)    
           call h5dread_f(dset_id, dt2_id, bfbs(n_flwbnd+1)%node, data_dims, error)

           call h5tclose_f(dt2_id, error)  
           call h5tclose_f(dt1_id, error)         
           call h5tclose_f(dt_id, error)
           call h5dclose_f(dset_id, error)        
       end if           
       call h5gclose_f(input_id, error)     
       return        
   end subroutine  

 
   !> Read source flow tables from hydro tidefile
   subroutine read_source_flow_tbl()
       use common_variables, only : n_sflow, source_flow
       implicit none
       integer(HID_T) :: input_id                       ! Group identifier
       integer(HID_T) :: dset_id                        ! Dataset identifier
       integer(HID_T) :: dt_id, dt5_id                  ! Memory datatype identifier
       integer(HID_T) :: dt1_id, dt2_id                 ! Memory datatype identifier
       integer(SIZE_T):: offset                         ! Member's offset
       integer(HSIZE_T), dimension(1) :: data_dims      ! Datasets dimensions
       integer(SIZE_T) :: typesize                      ! Size of the datatype
       integer(SIZE_T) :: type_size                     ! Size of the datatype
       integer :: error                                 ! Error flag
       integer :: i, from_i, to_i                       ! local variables
       character*32 :: from_obj, from_identifier        ! local variables
       character*32 :: to_obj, to_identifier            ! local variables
       
       n_sflow = 779 !todo:need to read from hydro, modify hydro
       call allocate_source_flow_property()
       call h5gopen_f(hydro_id, "input", input_id, error)  
       
       data_dims(1) = n_sflow         
       if ( n_sflow > 0) then
           call h5dopen_f(input_id, "source_flow", dset_id, error) 
           call h5tcopy_f(H5T_NATIVE_CHARACTER, dt_id, error)
           typesize = 32 
           call h5tset_size_f(dt_id, typesize, error)
           call h5tget_size_f(dt_id, type_size, error)                     
                      
           offset = 0
           call h5tcreate_f(H5T_COMPOUND_F, type_size, dt1_id, error)
           call h5tinsert_f(dt1_id, "name", offset, dt_id, error)    
           call h5dread_f(dset_id, dt1_id, source_flow%name, data_dims, error) 
           
           type_size = 4
           call h5tcreate_f(H5T_COMPOUND_F, type_size, dt2_id, error)
           call h5tinsert_f(dt2_id, "node", offset, H5T_NATIVE_INTEGER, error)    
           call h5dread_f(dset_id, dt2_id, source_flow%node, data_dims, error)

           call h5tclose_f(dt2_id, error)  
           call h5tclose_f(dt1_id, error)         
           call h5tclose_f(dt_id, error)
           call h5dclose_f(dset_id, error) 
           call h5gclose_f(input_id, error)  
       end if

       return        
   end subroutine   
 
 
   !> Read integer attributes from hydro tidefile     
   subroutine get_int_attribute_from_hdf5(attr_value, attr_name)        
       use h5lt
       implicit none
       character(len=*), intent(in) :: attr_name
       integer, intent(out) ::attr_value
       integer, dimension(1) :: hdf5_read_buffer      
       integer  :: error                             ! HDF5 Error flag
       call h5ltget_attribute_int_f(hydro_file_id,"hydro",    &
               attr_name, hdf5_read_buffer, error)
       call verify_error(error, "Reading attribute from hdf5 file")
       attr_value = hdf5_read_buffer(1)
       return
   end subroutine   
   
      
   !> Calculate max dimension for irreg_geom array
   !> Updated variables are n_irreg, chan_index, num_xsect_chan and num_elev_chan.
   subroutine calc_virt_xsect_dimension(chan_no, num_virt_sec, num_elev)
       use common_xsect
       implicit none
       integer, dimension(n_xsect), intent(in) :: chan_no, num_virt_sec, num_elev
       integer :: chan_index_prev                ! to calculate chan_index
       integer :: j
       max_num_elev = 0
       j = 1
       n_irreg = num_virt_sec(1)
       chan_index(1) = 1
       chan_index_prev = 1      
       num_xsect_chan(1) = num_virt_sec(1)
       do while (j < n_xsect) 
           num_xsect_chan(chan_no(j)) = num_virt_sec(j)
           num_elev_chan(chan_no(j)) = num_elev(j)       
           if (chan_no(j).ne.chan_index_prev) then
               chan_index(chan_no(j)) = j
               chan_index_prev = chan_no(j)
               n_irreg = n_irreg + num_virt_sec(j)
           end if
           if (num_elev(j) > max_num_elev) then
               max_num_elev = num_elev(j) 
           end if                  
           j = j + num_virt_sec(j)*num_elev(j)
       end do
   end subroutine     

end module   