!!<license>
!!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
!!    Department of Water Resources.
!!    This file is part of DSM2.

!!    The Delta Simulation Model 2 (DSM2) is free software:
!!    you can redistribute it and/or modify
!!    it under the terms of the GNU General Public License as published by
!!    the Free Software Foundation, either version 3 of the License, or
!!    (at your option) any later version.

!!    DSM2 is distributed in the hope that it will be useful,
!!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!!    GNU General Public License for more details.

!!    You should have received a copy of the GNU General Public License
!!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
!!</license>

!==== BOF tidefile =====================================================

!**********contains routines for writing data to an HDF5 file

!*   Programmed by: Tawnly Pranger
!*   Date:          October 2003



!***********************************************************************
!***********************************************************************

subroutine hdf5_read_attributes()

      use HDF5                  ! HDF5 This module contains all necessary modules
      use h5lt
      use hdfvars
      use inclvars
      use common_tide
      use io_units
      use dsm2_tidefile_input_storage_fortran
      use network
      use utilities
      implicit none

      integer(HID_T) :: attr_id ! Attribute identifier
      integer(HID_T) :: aspace_id ! Attribute Dataspace identifier
      integer(HID_T) :: atype_id ! Attribute Dataspace identifier
      integer(HSIZE_T), dimension(1) :: adims = (/1/) ! Attribute dimension
      integer     ::   arank = 1 ! Attribure rank
      integer        :: error   ! HDF5 Error flag
      integer(HSIZE_T), dimension(7) :: a_data_dims

      integer(HSIZE_T), dimension(2) :: h_offset
      integer(HID_T) :: filespace ! Dataspace identifier
      integer(HID_T) :: memspace ! memspace identifier

      integer(HID_T) :: in_dspace_id ! Dataspace identifier
      integer     ::    in_rank = 1 ! Dataset rank
      integer(HSIZE_T), dimension(7) :: in_data_dims ! = (/4,6,2/) ! Dataset dimensions

      integer(HID_T) :: cg_dspace_id ! Dataspace identifier
      integer     ::    cg_rank = 2 ! Dataset rank
      integer(HSIZE_T), dimension(7) :: cg_data_dims ! = (/4,6,2/) ! Dataset dimensions

      real*4, dimension(MaxChannels) :: bottom_el1,bottom_el2

      real*4, dimension(max_nodes) :: inodeidx,enodeidx
      real*4, dimension(max_reservoirs) :: iresidx,eresidx
      integer :: i,j

      integer(HID_T) :: ng_dspace_id ! Dataspace identifier
      integer     ::    ng_rank = 2 ! Dataset rank
      integer(HSIZE_T), dimension(7) :: ng_data_dims ! = (/4,6,2/) ! Dataset dimensions
      integer, dimension(max_nodes) :: node_obj

      integer(HID_T) :: rg_dspace_id ! Dataspace identifier
      integer     ::    rg_rank = 2 ! Dataset rank
      integer(HSIZE_T), dimension(7) :: rg_data_dims ! = (/4,6,2/) ! Dataset dimensions
      integer, dimension(max_reservoirs) :: res_obj

      integer(HID_T) :: bname_dspace_id ! Dataspace identifier
      integer(HID_T) :: bnode_dspace_id ! Dataspace identifier
      integer     ::    boundary_rank = 1 ! Dataset rank
      integer(HSIZE_T), dimension(7) :: bname_data_dims ! = (/4,6,2/) ! Dataset dimensions
      integer(HSIZE_T), dimension(7) :: bnode_data_dims ! = (/4,6,2/) ! Dataset dimensions
      character*32, dimension(max_stgbnd) :: bname_obj
      integer, dimension(max_stgbnd) :: bnode_obj
      integer(SIZE_T) :: typesize
      integer(HID_T) :: dtc32_id ! Memory datatype identifier

      integer :: nconnect, iconn,dummy
      integer :: connection_index, int_node_no,ext_node_no,node_flow_index,flow_index,res_index,res_flow_index
      character*32 :: flow_name, resv_name
      character*8 ::  flow_type

      integer,dimension(1) :: hdf_dummy_integer
      integer :: scalar
      print*, "Reading HDF5 Header Information"

      a_data_dims(1) = 1

      call h5screate_simple_f(arank, adims, aspace_id, error)
      call h5tcopy_f(H5T_NATIVE_character, atype_id, error)
      call h5tset_size_f(atype_id, 21, error)

!      call h5aopen_name_f(hydro_id,"Hydro Version",attr_id,error)
!      call h5aread_f(attr_id, atype_id, chead, a_data_dims, error)
!      call h5aclose_f(attr_id, error)

      call h5ltget_attribute_string_f(hydro_id, ".", "Hydro Version", &
                chead, error)

      call h5tcopy_f(H5T_NATIVE_INTEGER, atype_id, error)

      call h5aopen_name_f(hydro_id,"Maximum number of reservoirs",attr_id,error)
      call h5aread_f(attr_id, atype_id, dim_res_tf, a_data_dims, error)
!      call h5aclose_f(attr_id, error)
      call h5aopen_name_f(hydro_id,"Maximum number of channels",attr_id,error)
      call h5aread_f(attr_id, atype_id, dim_chan_tf, a_data_dims, error)
!      call h5aclose_f(attr_id, error)
      call h5aopen_name_f(hydro_id,"Number of reservoirs",attr_id,error)
      call h5aread_f(attr_id, atype_id, n_res_tf, a_data_dims, error)
!      call h5aclose_f(attr_id, error)
      call h5aopen_name_f(hydro_id,"Number of channels",attr_id,error)
      call h5aread_f(attr_id, atype_id, n_chan_tf, a_data_dims, error)
!      call h5aclose_f(attr_id, error)
      call h5aopen_name_f(hydro_id,"Number of stage boundaries",attr_id,error)
      call h5aread_f(attr_id, atype_id, nstgbnd, a_data_dims, error)
!      call h5aclose_f(attr_id, error)
      call h5aopen_name_f(hydro_id,"Start time",attr_id,error)
      call h5aread_f(attr_id, atype_id, h5_time_start, a_data_dims, error)
!      call h5aclose_f(attr_id, error)
      print*,"Read starting date:",jmin2cdt(h5_time_start)
      call h5aopen_name_f(hydro_id,"Time interval",attr_id,error)
      call h5aread_f(attr_id, atype_id, h5_time_interval, a_data_dims, error)
!      call h5aclose_f(attr_id, error)
      call h5aopen_name_f(hydro_id,"Number of intervals",attr_id,error)
      call h5aread_f(attr_id, atype_id, hdf5length, a_data_dims, error)
!     call h5aclose_f(attr_id, error)
      in_data_dims(1) = n_chan_tf


      ! Read map of external channel numbers int2ext
      call h5dopen_f(geom_id,"channel_number",in_dset_id,error)
      call h5dget_space_f(in_dset_id, filespace, error)
      call h5dread_f(in_dset_id,H5T_NATIVE_INTEGER, int2ext(1), in_data_dims, error)
      call h5sclose_f(filespace,error)
      call h5dclose_f(in_dset_id,error)


                                ! Read bottom_el
      call h5dopen_f(geom_id,"channel_bottom",cg_dset_id,error)

      call h5dget_space_f(cg_dset_id, filespace, error)
!      call h5sclose_f(filespace,error)
!	call h5dclose_f(cg_dset_id)



      cg_data_dims(1) = n_chan_tf
      cg_data_dims(2) = 1
      h_offset(1) = 0
      h_offset(2) = bottom_elIdx
      call h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, &
          h_offset, cg_data_dims, error)
      call h5screate_simple_f(cg_rank, cg_data_dims, memspace, error)
      call h5dread_f(cg_dset_id,H5T_NATIVE_REAL, bottom_el1, cg_data_dims, &
          error, memspace, filespace)



      h_offset(2) = bottom_elIdx + 1
      call h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, &
          h_offset, cg_data_dims, error)
      call h5screate_simple_f(cg_rank, cg_data_dims, memspace, error)
      call h5dread_f(cg_dset_id,H5T_NATIVE_REAL, bottom_el2, cg_data_dims, &
          error, memspace, filespace)

      Do i = 1,n_chan_tf
         chan_geom(i).bottomelev(1) = bottom_el1(i)
         chan_geom(i).bottomelev(2) = bottom_el2(i)
      end do

      call reservoir_flow_connections_clear_buffer()
      call node_flow_connections_clear_buffer()

      call h5ltget_attribute_int_f(hydro_id,".", &
                "Number of node flow connects", &
                hdf_dummy_integer, error)
      nconnect = hdf_dummy_integer(1)


      if (nconnect .gt. 0)then
         call node_flow_connections_read_buffer_from_hdf5(geom_id, error)
         nconnect = node_flow_connections_buffer_size()
         do iconn = 1, nconnect
             call node_flow_connections_query_from_buffer(iconn, connection_index,int_node_no,ext_node_no, &
                                                         node_flow_index,flow_index,flow_name,flow_type,error)
             if (flow_type(1:4) .eq. "qext")then
                 node_geom(int_node_no).qext(node_flow_index) = flow_index
             else if (flow_type(1:8) .eq. "transfer")then
                 node_geom(int_node_no).qinternal(node_flow_index) = flow_index
             else
                 write(unit_error,*) "Error querying nod flow connections"
             end if
         end do
         call node_flow_connections_clear_buffer()
      end if

      call h5ltget_attribute_int_f(hydro_id,".", &
                "Number of reservoir flow connects", &
                hdf_dummy_integer, error)
      nconnect = hdf_dummy_integer(1)
      if (nconnect .gt. 0)then
         call reservoir_flow_connections_read_buffer_from_hdf5(geom_id, error)
         do iconn = 1, nconnect
             call reservoir_flow_connections_query_from_buffer(iconn, connection_index,resv_name,res_index, &
                                                              res_flow_index,flow_index,flow_name,flow_type,error)

             if (flow_type(1:4) .eq. "qext")then
                 res_geom(res_index).qext(res_flow_index) = flow_index
             else if (flow_type(1:8) .eq. "transfer")then
                 res_geom(res_index).qinternal(res_flow_index) = flow_index
             else
                 write(unit_error,*) "Error querying nod flow connections"
             end if
         end do
         call reservoir_flow_connections_clear_buffer()
      end if


      ! Read Stage Boundaries

      if (nstgbnd .gt. 0)then
          call stage_boundaries_clear_buffer()
          call stage_boundaries_read_buffer_from_hdf5(geom_id,error)
          do iconn = 1,nstgbnd
              call stage_boundaries_query_from_buffer(iconn,stgbnd(iconn).name,stgbnd(iconn).node,dummy,error)
          end do
      call stage_boundaries_clear_buffer()
      end if



      call h5tcopy_f(H5T_NATIVE_INTEGER, atype_id, error)
      call h5aopen_name_f(hydro_id,"Number of QExt",attr_id,error)
      call h5aread_f(attr_id, atype_id, nqext, a_data_dims, error)
      call ReadQExtHDF5()
      return
end subroutine

