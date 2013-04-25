!<license>
!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
!    Department of Water Resources.
!    This file is part of DSM2.
!
!    The Delta Simulation Model 2 (DSM2) is free software: 
!    you can redistribute it and/or modify
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
   integer(HID_T) :: file_id            !< HDF5 File identifier
   integer(HID_T) :: hydro_id           !< hydro group identifier      
   integer(HID_T) :: access_plist       !< HDF5 property identifier
   contains
         
   !> Open HDF5 interface and hydro tidefile 
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
           call h5open_f(error)
           call verify_error(error, "opening hdf interface")
           	! set up stdio to allow for buffered read/write          	
	       call h5pcreate_f(H5P_FILE_ACCESS_F, access_plist, error)      
	       call h5pget_cache_f(access_plist, nelmts,               &
                               rddc_nelmts, rddc_nbytes, rdcc_w0, error)
	       rddc_nbytes = 8000000
	       call h5pset_cache_f(access_plist, nelmts, rddc_nelmts,  &
                               rddc_nbytes, rdcc_w0,error)
	       call verify_error(error,"Cache set")
           call h5fopen_f (hdf5_file_name, H5F_ACC_RDONLY_F, file_id, error)
           call verify_error(error, "opening tidefile")
           call h5gopen_f(file_id, "hydro", hydro_id, error)
       else
           call gtm_fatal(hdf5_file_name//" is not a valid tidefile under working directory!")  
       end if    
   end subroutine
   
   !> Close HDF5 interface and hydro tidefile 
   subroutine hdf5_close()
       implicit none
       integer :: error                                 ! error flag
       call h5gclose_f(hydro_id, error)
       call h5fclose_f(file_id, error)
       call h5pclose_f(access_plist, error)                     
       call h5close_f(error)     
   end subroutine
   
   !> Read number of channels from hydro tidefile
   subroutine get_hydro_attr()
       implicit none        
       call get_int_attribute_from_hdf5(n_chan, "Number of channels")
       call get_int_attribute_from_hdf5(n_xsect, "Number of virt xsects")
       call get_int_attribute_from_hdf5(n_comp, "Number of comp pts")
       call get_int_attribute_from_hdf5(n_time, "Number of intervals")
       call get_int_attribute_from_hdf5(orig_start_julmin, "Start time")
       call get_int_attribute_from_hdf5(orig_time_interval, "Time interval")
       call get_int_attribute_from_hdf5(orig_ntideblocks, "Number of intervals")
       orig_end_julmin = orig_start_julmin + (orig_ntideblocks-1)*orig_time_interval    
       return
   end subroutine    
      
   !> Read time series dataset from hydro tidefile        
   subroutine get_ts_from_hdf5(dset_data, dset_name)
       implicit none     
       character(len=*), intent(in) :: dset_name                           !< dataset name
       real(gtm_real), dimension(n_comp,n_time), intent(out) :: dset_data  !< return data arrays   
       integer(HID_T) :: data_id                                           ! local group identifier
       integer(HID_T) :: dset_id                                           ! local dataset identifier
       integer(HSIZE_T), dimension(2) :: data_dims                         ! local datasets dimensions
       integer :: error                                                    ! error flag          
       data_dims(1) = n_comp
       data_dims(2) = n_time     
       call h5gopen_f(hydro_id, "data", data_id, error)
       call h5dopen_f(data_id, dset_name, dset_id, error) 
       call h5dread_f(dset_id, H5T_NATIVE_DOUBLE, dset_data, data_dims, error)
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
       
       call h5tclose_f(dt_id, error)
       call h5tclose_f(dt1_id, error)
       call h5tclose_f(dt2_id, error)      
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
     
   !> Read integer attributes from hydro tidefile     
   subroutine get_int_attribute_from_hdf5(attr_value, attr_name)        
       use h5lt
       implicit none
       character(len=*), intent(in) :: attr_name
       integer, intent(out) ::attr_value
       integer, dimension(1) :: hdf5_read_buffer      
       integer  :: error                             ! HDF5 Error flag
       call h5ltget_attribute_int_f(file_id,"hydro",    &
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

   !> Assign numbers to segment array
   subroutine assign_segment()
       use common_variables
       implicit none
       integer :: i, j, previous_chan_no
       call allocate_segment_property()
       segm(1)%segm_no = 1
       segm(1)%chan_no = 1
       segm(1)%up_comppt = 1
       segm(1)%down_comppt = 2
       segm(1)%up_distance = 0
       segm(1)%down_distance = comp_pt(2)%distance
       segm(1)%length = segm(1)%down_distance - segm(1)%up_distance
       previous_chan_no = 1
       j = 1
       do i = 3, n_comp
           if (comp_pt(i)%chan_no .eq. previous_chan_no) then
               j = j + 1
               segm(j)%segm_no = j
               segm(j)%chan_no = comp_pt(i)%chan_no
               segm(j)%up_comppt = comp_pt(i-1)%comp_index
               segm(j)%down_comppt = comp_pt(i)%comp_index
               segm(j)%up_distance = comp_pt(i-1)%distance
               segm(j)%down_distance = comp_pt(i)%distance
               segm(j)%length = comp_pt(i)%distance - comp_pt(i-1)%distance
           else
               previous_chan_no = comp_pt(i)%chan_no
           end if
       end do        
       return    
   end subroutine    
      
end module   