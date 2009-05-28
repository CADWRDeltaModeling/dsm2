C!<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    DSM2 is free software: you can redistribute it and/or modify
C!    it under the terms of the GNU General Public !<license as published by
C!    the Free Software Foundation, either version 3 of the !<license, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public !<license for more details.

C!    You should have received a copy of the GNU General Public !<license
C!    along with DSM2.  If not, see <http://www.gnu.org/!<licenses/>.
C!</license>
*==== BOF tidefile =====================================================

c**********contains routines for writing data to an HDF5 file

*   Programmed by: Tawnly Pranger
*   Date:          October 2003
*   Revised:       Eli Ateljevich, October 2005
***********************************************************************
***********************************************************************

      subroutine hdf5_write_attributes()

      use HDF5
      use h5lt
      use hdfvars
      use inclvars
      use runtime_data
      use grid_data
      use common_tide
      use IFPORT

      implicit none

      integer(HID_T) :: attr_id ! Attribute identifier
      integer(HID_T) :: aspace_id ! Attribute Dataspace identifier
      integer(HID_T) :: atype_id ! Attribute Dataspace identifier
      integer(HSIZE_T), dimension(1) :: adims = (/1/) ! Attribute dimension
      integer     :: arank = 1  ! Attribute rank
      integer     :: error      ! HDF5 Error flag
      integer(HSIZE_T), dimension(1) :: a_data_dims
      real(kind=4), dimension(MaxChannels) :: bottom_el1,bottom_el2
      real(kind=4), dimension(max_nodes) :: inodeidx,enodeidx
      real(kind=4), dimension(max_reservoirs) :: iresidx,eresidx
      integer :: i,j
      integer calcHDF5NumberOfTimeIntervals

      integer(HID_T) :: in_dspace_id ! Dataspace identifier
      integer     ::    in_rank = 1 ! Dataset rank
      integer(HSIZE_T), dimension(7) :: in_data_dims
c      integer(HSIZE_T), dimension(1) :: in_dims = (/0/) ! Dataset dimensions

      integer(HID_T) :: cg_dspace_id ! Dataspace identifier
      integer     ::    cg_rank = 2 ! Dataset rank
      integer(HSIZE_T), dimension(7) :: cg_data_dims

      integer(HID_T) :: ng_dspace_id ! Dataspace identifier
      integer     ::    ng_rank = 2 ! Dataset rank
      integer(HSIZE_T), dimension(7) :: ng_data_dims
      integer, dimension(max_nodes) :: node_obj

      integer(HID_T) :: rg_dspace_id ! Dataspace identifier
      integer     ::    rg_rank = 2 ! Dataset rank
      integer(HSIZE_T), dimension(7) :: rg_data_dims
      integer, dimension(max_reservoirs) :: res_obj

      integer(HID_T) :: bname_dspace_id ! Dataspace identifier
      integer(HID_T) :: bnode_dspace_id ! Dataspace identifier
      integer     ::    boundary_rank = 1 ! Dataset rank
      integer(HSIZE_T), dimension(7) :: bname_data_dims
      integer(HSIZE_T), dimension(7) :: bnode_data_dims
      character*32, dimension(max_stgbnd) :: bname_obj
      integer, dimension(max_stgbnd) :: bnode_obj
      integer(SIZE_T) :: typesize
      integer(HID_T) :: dtc32_id ! Memory datatype identifier

      integer(HID_T) :: cparms  !dataset creatation property identifier
      integer(HSIZE_T), dimension(2) :: h_offset
      integer(HID_T) :: filespace ! Dataspace identifier
      integer(HID_T) :: memspace ! memspace identifier

      integer, parameter :: label_len = 12
	integer, parameter :: name_len=32
      character(LEN=label_len),dimension(2) :: chan_location 
     &                                    = (/"upstream","downstream"/)
      character(LEN=name_len),dimension(:), allocatable :: names


      integer :: tempstart,tempend,nlen

      integer,dimension(1) :: hdf5_dummy_integer
      integer(SIZE_T) :: hdf5_int_size

      integer cdt2jmin
      EXTERNAL cdt2jmin

      a_data_dims(1) = 1
      hdf5_int_size = 1

      
                                ! Creating attributes for HDF5
      call h5screate_simple_f(arank, adims, aspace_id, error)

      call h5tcopy_f(H5T_NATIVE_CHARACTER, atype_id, error)

      call h5ltset_attribute_string_f(hydro_id,".", 
     &           "Tidefile created date",
     &           fdate(), error)

      call h5ltset_attribute_string_f(hydro_id,".", 
     &           "Hydro Version",
     &           'Hydro Version ' // trim(dsm2_version),error)

      call h5ltset_attribute_string_f(hydro_id,".", 
     &           "Start time string",
     &           tf_start_date, error)


      hdf5_dummy_integer =  MaxNres
      call h5ltset_attribute_int_f(hydro_id,".",
     &           "Maximum number of reservoirs",
     &           hdf5_dummy_integer, hdf5_int_size, error)

      hdf5_dummy_integer =  MaxChannels
      call h5ltset_attribute_int_f(hydro_id,".", 
     &           "Maximum number of channels",
     &           hdf5_dummy_integer, hdf5_int_size, error)


!      call h5tcopy_f(H5T_NATIVE_INTEGER, atype_id, error)
!      call h5acreate_f(hydro_id, "Maximum number of reservoirs",
!     &     atype_id, aspace_id, attr_id, error)
!      call h5awrite_f(attr_id, atype_id, MaxNres, a_data_dims, error)


      hdf5_dummy_integer =  Nreser
      call h5ltset_attribute_int_f(hydro_id,".", 
     &           "Number of reservoirs",
     &           hdf5_dummy_integer, hdf5_int_size, error)

      hdf5_dummy_integer =  nchans
      call h5ltset_attribute_int_f(hydro_id,".", 
     &           "Number of channels",
     &           hdf5_dummy_integer, hdf5_int_size, error)

      hdf5_dummy_integer =  nstgbnd
      call h5ltset_attribute_int_f(hydro_id,".", 
     &           "Number of stage boundaries",
     &           hdf5_dummy_integer, hdf5_int_size, error)

      hdf5_dummy_integer =  tf_start_julmin
      call h5ltset_attribute_int_f(hydro_id,".", 
     &           "Start time",
     &           hdf5_dummy_integer, hdf5_int_size, error)

      hdf5_dummy_integer =  TideFileWriteInterval
      call h5ltset_attribute_int_f(hydro_id,".", 
     &           "Time interval",
     &           hdf5_dummy_integer, hdf5_int_size, error)

      hdf5_dummy_integer =  calcHDF5NumberOfTimeIntervals()
      call h5ltset_attribute_int_f(hydro_id,".", 
     &           "Number of intervals",
     &           hdf5_dummy_integer, hdf5_int_size, error)






!      nlen = len('Hydro Version ' // trim(dsm2_version))
!      call h5tset_size_f(atype_id, nlen, error)
!      call h5acreate_f(hydro_id, "Hydro Version",
!     &     atype_id, aspace_id, attr_id, error)
!      call h5awrite_f(attr_id, atype_id, 'Hydro Version ' // trim(dsm2_version),
!     &     a_data_dims, error)

!      nlen = len(tf_start_date)
!      call h5tset_size_f(atype_id, nlen, error)
!      call h5acreate_f(hydro_id, "Start time string",
!     &     atype_id, aspace_id, attr_id, error)
!      call h5awrite_f(attr_id, atype_id, tf_start_date, a_data_dims, error)
!	call h5tclose_f(atype_id,error)

!      call h5tcopy_f(H5T_NATIVE_INTEGER, atype_id, error)
!      call h5acreate_f(hydro_id, "Maximum number of reservoirs",
!     &     atype_id, aspace_id, attr_id, error)
!      call h5awrite_f(attr_id, atype_id, MaxNres, a_data_dims, error)

!      call h5acreate_f(hydro_id, "Maximum number of channels",
!     &     atype_id, aspace_id, attr_id, error)
!      call h5awrite_f(attr_id, atype_id, MaxChannels, a_data_dims, error)
!
!      call h5acreate_f(hydro_id, "Number of reservoirs",
!     &     atype_id, aspace_id, attr_id, error)
!      call h5awrite_f(attr_id, atype_id, Nreser, a_data_dims, error)
!
!      call h5acreate_f(hydro_id, "Number of channels",
!     &     atype_id, aspace_id, attr_id, error)
!      call h5awrite_f(attr_id, atype_id, nchans, a_data_dims, error)
!
!      call h5acreate_f(hydro_id, "Number of stage boundaries",
!     &     atype_id, aspace_id, attr_id, error)
!      call h5awrite_f(attr_id, atype_id, nstgbnd, a_data_dims, error)
!
!      call h5acreate_f(hydro_id, "Start time",
!     &     atype_id, aspace_id, attr_id, error)
!      call h5awrite_f(attr_id, atype_id, tf_start_julmin, a_data_dims, error)
!
!      call h5acreate_f(hydro_id, "Time interval",
!     &     atype_id, aspace_id, attr_id, error)
!      call h5awrite_f(attr_id, atype_id, TideFileWriteInterval,
!     &                a_data_dims, error)
!
!      call h5acreate_f(hydro_id, "Number of intervals",
!     &     atype_id, aspace_id, attr_id, error)
!      call h5awrite_f(attr_id, atype_id, calcHDF5NumberOfTimeIntervals(), 
!     &                a_data_dims, error)


      !call h5tclose_f(atype_id,error)

                 ! Write out channel geometry
      call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, error)


      in_dims(1) = MaxChannels
      ! Write out external channel numbers int2ext
      call h5screate_simple_f(in_rank, in_dims, in_dspace_id, error)
      call h5dcreate_f(geom_id, "channel_number", H5T_NATIVE_INTEGER,
     &     in_dspace_id, in_dset_id, error, cparms)
      call h5dwrite_f(in_dset_id,H5T_NATIVE_INTEGER, 
     &     int2ext(1), in_dims, error)
      call h5sclose_f (in_dspace_id, error)
      call h5dclose_f(in_dset_id,error)

      ! Write channel up/down labels
	in_dims(1) = 2
	call Write1DStringArray(geom_id,"channel_location",
     &                        chan_location,label_len,2)
      
	! Write reservoir names
	in_dims(1)=max_reservoirs
      allocate(names(max_reservoirs))
	names=' '
	do i=1,max_reservoirs
	   names(i)=res_geom(i).name 
      end do
	call Write1DStringArray(geom_id,"reservoir_names",names,
     &                      name_len,max_reservoirs)
      deallocate(names)     


	! Write transfer names
	in_dims(1)=max_obj2obj
      allocate(names(max_obj2obj))
	names=' '
	do i=1,max_obj2obj
	   names(i)=obj2obj(i).name 
      end do
	call Write1DStringArray(geom_id,"transfer_names",names,
     &                      name_len,max_obj2obj)
      deallocate(names) 


      ! Write external flow names
	in_dims(1)=max_qext
      allocate(names(max_qext))
	names=' '
	do i=1,max_qext
	   names(i)=qext(i).name 
      end do
	call Write1DStringArray(geom_id,"external_flow_names",names,
     &                      name_len,max_qext)
      deallocate(names) 



      ! Write out bottom_el
      cg_dims(1) = MaxChannels
      cg_dims(2) = 2            ! bottom_el:2
      cg_data_dims(1) = cg_dims(1)
      cg_data_dims(2) =  1
      call h5screate_simple_f(cg_rank, cg_dims, cg_dspace_id, error)
      call h5dcreate_f(geom_id, "channel geometry", H5T_NATIVE_REAL,
     &     cg_dspace_id, cg_dset_id, error, cparms)
      Do i = 1,MaxChannels
         bottom_el1(i) = chan_geom(i).bottomelev(1)
         bottom_el2(i) = chan_geom(i).bottomelev(2)
      end do
      h_offset(1) = 0
      h_offset(2) = bottom_elIdx
      call h5sselect_hyperslab_f(cg_dspace_id, H5S_SELECT_SET_F,
     &     h_offset, cg_data_dims, error)
      call h5screate_simple_f(cg_rank, cg_data_dims, memspace, error)
      call h5dwrite_f(cg_dset_id,H5T_NATIVE_REAL, bottom_el1, cg_data_dims,
     &     error, mem_space_id=memspace, file_space_id=cg_dspace_id)
      h_offset(2) = bottom_elIdx + 1
      call h5sselect_hyperslab_f(cg_dspace_id, H5S_SELECT_SET_F,
     &     h_offset, cg_data_dims, error)
      call h5screate_simple_f(cg_rank, cg_data_dims, memspace, error)
      call h5dwrite_f(cg_dset_id,H5T_NATIVE_REAL, bottom_el2, cg_data_dims,
     &     error, mem_space_id=memspace, file_space_id=cg_dspace_id)

                                ! Write out node geometry
      call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, error)

      ng_dims(1) = max_nodes
      ng_dims(2) = max_qobj * 2

      call h5screate_simple_f(ng_rank, ng_dims, ng_dspace_id, error)
      call h5dcreate_f(geom_id, "node geometry", H5T_NATIVE_INTEGER,
     &     ng_dspace_id, ng_dset_id, error, cparms)

      ng_data_dims(1) = ng_dims(1)
      ng_data_dims(2) = 1

                                ! Creation of hyperslab

      h_offset(1) = 0

                                ! Write out node_geom.qint
      do i = 1,max_qobj
         do j = 1,max_nodes
            node_obj(j) = node_geom(j).qinternal(i)
         end do
         h_offset(2) = i - 1
         call h5dget_space_f(ng_dset_id, filespace, error)
         call h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F,
     &        h_offset, ng_data_dims, error)
         call h5screate_simple_f(ng_rank, ng_data_dims, memspace, error)
         call h5dwrite_f(ng_dset_id,H5T_NATIVE_INTEGER, node_obj, ng_data_dims,
     &        error, mem_space_id=memspace, file_space_id=filespace)
         call h5sclose_f (filespace, error)
      end do
                                ! Write out node_geom.qext
      do i = 1,max_qobj
         do j = 1,max_nodes
            node_obj(j) = node_geom(j).qext(i)
         end do
         h_offset(2) = max_qobj + i - 1
         call h5dget_space_f(ng_dset_id, filespace, error)
         call h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F,
     &        h_offset, ng_data_dims, error)
         call h5screate_simple_f(ng_rank, ng_data_dims, memspace, error)
         call h5dwrite_f(ng_dset_id,H5T_NATIVE_INTEGER, node_obj, ng_data_dims,
     &        error, mem_space_id=memspace, file_space_id=filespace)
         call h5sclose_f (filespace, error)
      end do

                                ! Write out reservoir geometry
      call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, error)

      rg_dims(1) = max_reservoirs
      rg_dims(2) = max_qobj * 2

      call h5screate_simple_f(rg_rank, rg_dims, rg_dspace_id, error)
      call h5dcreate_f(geom_id, "reservoir geometry", H5T_NATIVE_INTEGER,
     &     rg_dspace_id, rg_dset_id, error, cparms)

      rg_data_dims(1) = rg_dims(1)
      rg_data_dims(2) = 1

                                ! Creation of hyperslab
      call h5dget_space_f(rg_dset_id, filespace, error)
      h_offset(1) = 0

                                ! Write out res_geom.qint
      do i = 1,max_qobj
         do j = 1,max_reservoirs
            res_obj(j) = res_geom(j).qinternal(i)
         end do
         call h5dget_space_f(rg_dset_id, filespace, error)
         h_offset(2) = i - 1
         call h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F,
     &        h_offset, rg_data_dims, error)
         call h5screate_simple_f(rg_rank, rg_data_dims, memspace, error)
         call h5dwrite_f(rg_dset_id,H5T_NATIVE_INTEGER, res_obj, rg_data_dims,
     &        error, mem_space_id=memspace, file_space_id=filespace)
         call h5sclose_f (filespace, error)
      end do
                                ! Write out res_geom.qext
      do i = 1,max_qobj
         do j = 1,max_reservoirs
            res_obj(j) = res_geom(j).qext(i)
         end do
         call h5dget_space_f(rg_dset_id, filespace, error)
         h_offset(2) = max_qobj + i - 1
         call h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F,
     &        h_offset, rg_data_dims, error)
         call h5screate_simple_f(rg_rank, rg_data_dims, memspace, error)
         call h5dwrite_f(rg_dset_id,H5T_NATIVE_INTEGER, res_obj, rg_data_dims,
     &        error, mem_space_id=memspace, file_space_id=filespace)
         call h5sclose_f (filespace, error)
      end do

      call InitObj2ObjType()
      if (nobj2obj .gt. 0) then
         call WriteObj2ObjAttribute()
      endif

      call InitQExtType()
      call WriteQExt()

                                ! Write out Stage Boundaries
      call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, error)

      bname_dims(1) = max_stgbnd
      bnode_dims(1) = max_stgbnd

      call h5tcopy_f(H5T_NATIVE_CHARACTER, dtc32_id, error)
      typesize = 32
      call h5tset_size_f(dtc32_id, typesize, error)

      call h5screate_simple_f(boundary_rank, bname_dims, bname_dspace_id, error)
      call h5dcreate_f(geom_id, "stage boundary names", dtc32_id,
     &     bname_dspace_id, bname_dset_id, error, cparms)
      call h5screate_simple_f(boundary_rank, bnode_dims, bnode_dspace_id, error)
      call h5dcreate_f(geom_id, "stage boundary nodes", H5T_NATIVE_INTEGER,
     &     bnode_dspace_id, bnode_dset_id, error, cparms)

      bname_data_dims(1) = bname_dims(1)
      bnode_data_dims(1) = bnode_dims(1)

      do i = 1,max_stgbnd
         bname_obj(i) = stgbnd(i).name
         bnode_obj(i) = stgbnd(i).node
      end do

      call h5screate_simple_f(boundary_rank, bname_data_dims, memspace, error)
      call h5dwrite_f(bname_dset_id,dtc32_id, bname_obj, bname_data_dims, error)
      call h5screate_simple_f(boundary_rank, bnode_data_dims, memspace, error)
      call h5dwrite_f(bnode_dset_id,H5T_NATIVE_INTEGER, bnode_obj, bnode_data_dims, error)

      return
      end


