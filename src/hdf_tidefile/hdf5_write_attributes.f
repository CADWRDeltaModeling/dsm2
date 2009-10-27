C!<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    The Delta Simulation Model 2 (DSM2) is free software: 
C!    you can redistribute it and/or modify
C!    it under the terms of the GNU General Public License as published by
C!    the Free Software Foundation, either version 3 of the License, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public License for more details.

C!    You should have received a copy of the GNU General Public License
C!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
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
      use dsm2_tidefile_input_storage_fortran

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
      character(LEN=8) connect_type       
      integer :: icount,inode,iflow
 
      a_data_dims(1) = 1
      hdf5_int_size = 1
 
      
                                ! Creating attributes for HDF5
      call h5screate_simple_f(arank, adims, aspace_id, error)

      call h5tcopy_f(H5T_NATIVE_CHARACTER, atype_id, error)

      call h5ltset_attribute_string_f(hydro_id,".", 
     &           "Creation date",
     &           trim(fdate())//char(0), error)

      call h5ltset_attribute_string_f(hydro_id,".", 
     &           "Hydro Version",
     &           trim(dsm2_version)//char(0), error)

      call h5ltset_attribute_string_f(hydro_id,".", 
     &           "Subversion",
     &           trim(svn_build)//char(0), error)

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

                 ! Write out channel geometry
      call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, error)


      in_dims(1) = nchans
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
	in_dims(1)=max(1,nreser)
      allocate(names(max(1,nreser)))
	names=' '
	do i=1,max(1,nreser)
	   names(i)=res_geom(i).name 
      end do
	call Write1DStringArray(geom_id,"reservoir_names",names,
     &                      name_len,max(1,nreser))
      deallocate(names)     


	! Write transfer names
	in_dims(1)=max(1,nobj2obj)
      allocate(names(max(1,nobj2obj)))
	names=' '
	do i=1,max(1,nobj2obj)
	   names(i)=obj2obj(i).name 
      end do
	call Write1DStringArray(geom_id,"transfer_names",names,
     &                      name_len,max(1,nobj2obj))
      deallocate(names) 


      ! Write external flow names
	in_dims(1)=max(1,nqext)
      allocate(names(max(1,nqext)))
	names=' '
	do i=1,max(1,nqext)
	   names(i)=qext(i).name 
      end do
	call Write1DStringArray(geom_id,"external_flow_names",names,
     &                      name_len,max(1,nqext))
      deallocate(names) 



      ! Write out bottom_el
      cg_dims(1) = nchans
      cg_dims(2) = 2            ! bottom_el:2
      cg_data_dims(1) = cg_dims(1)
      cg_data_dims(2) =  1
      call h5screate_simple_f(cg_rank, cg_dims, cg_dspace_id, error)
      call h5dcreate_f(geom_id, "channel_bottom", H5T_NATIVE_REAL,
     &     cg_dspace_id, cg_dset_id, error, cparms)
      Do i = 1,nchans
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

      call WriteReservoirFlowConnectionsHDF5
      call WriteNodeFlowConnectionsHDF5
      call WriteQExt()
      call WriteStageBoundariesHDF5
      call WriteReservoirNodeConnectionsHDF5
      call WriteComputationPointsHDF5

      return
      end subroutine



***********************************************************************
***********************************************************************

      subroutine WriteQExt()
      use dsm2_tidefile_input_storage_fortran
      use HDF5
      use hdfvars
      use qextvars
      use inclvars
      use grid_data

      implicit none

      integer ::   error        ! Error flag
      integer::i
      if (nqext .eq. 0) then
         return
      endif
      call qext_clear_buffer()
      do i = 1,nqext
        call qext_append_to_buffer(qext(i).name,
     &                             qext(i).datasource.source_type,
     &                             qext(i).datasource.indx_ptr,
     &                             qext(i).datasource.value,
     &                             qext(i).attach_obj_name,
     &                             qext(i).attach_obj_type,
     &                             qext(i).attach_obj_no,error)
      end do
      call qext_write_buffer_to_hdf5(geom_id,error)
      call qext_clear_buffer()
      return
      end subroutine



***********************************************************************
***********************************************************************

      subroutine WriteStageBoundariesHDF5()
      use dsm2_tidefile_input_storage_fortran
      use HDF5
      use hdfvars
      use qextvars
      use inclvars
      use grid_data

      implicit none

      integer ::   error        ! Error flag
      integer::i
      if (nqext .eq. 0) then
         return
      endif
      call stage_boundaries_clear_buffer()
      do i = 1,nstgbnd
        call stage_boundaries_append_to_buffer(stgbnd(i).name,
     &                                         stgbnd(i).node,
     &                                         node_geom(stgbnd(i).node).node_id,
     &                                         error)
      end do
      if(nstgbnd .gt. 0)call stage_boundaries_write_buffer_to_hdf5(geom_id,error)
      call stage_boundaries_clear_buffer()
      return
      end subroutine



***********************************************************************
***********************************************************************

      subroutine WriteComputationPointsHDF5
      use dsm2_tidefile_input_storage_fortran
      use hdfvars
      use grid_data
      implicit none
      INCLUDE '../hydrolib/network.inc'
      INCLUDE '../hydrolib/chnlcomp.inc'
      integer ::   error        ! Error flag
      integer :: NumberOfChannels
      integer:: i, ichan, icomp
      integer :: up,down
      call hydro_comp_point_clear_buffer()
      do ichan = 1,nchans
         up=UpCompPointer(ichan)
         down=DownCompPointer(ichan)
         do icomp = up,down
            call hydro_comp_point_append_to_buffer(
     &          icomp,ichan,CompLocation(icomp),error)
         end do
      end do
      call hydro_comp_point_write_buffer_to_hdf5(geom_id,error)
      call VerifyHDF5(error,"Computation point write")
      call hydro_comp_point_clear_buffer()
      return
      end subroutine




***********************************************************************
***********************************************************************

      subroutine WriteReservoirNodeConnectionsHDF5
      use dsm2_tidefile_input_storage_fortran
      use hdfvars
      use h5lt
      use hdf5
      use grid_data
      implicit none
      INCLUDE '../hydrolib/network.inc'
      INCLUDE '../hydrolib/chnlcomp.inc'
      integer ::   error = 0       ! Error flag
      integer:: ires, inode
      integer :: icount
      integer:: scalar = 1
      character*8 :: node_type = " "
      character*32 :: resname = " "
      integer,dimension(1) :: hdf_dummy_integer


      call reservoir_node_connect_clear_buffer()
      icount = 0
      do ires = 1,nreser
         do inode = 1,res_geom(ires).nnodes
             if(res_geom(ires).isNodeGated(inode))then
                 node_type = " "
                 node_type = "gate"
             else
                 node_type = " "
                 node_type = "node"
             end if
             icount = icount + 1
             resname = " "
             resname = res_geom(ires).name
             
             call reservoir_node_connect_append_to_buffer(
     &          icount,
     &          resname,
     &          ires,
     &          inode,
     &          res_geom(ires).node_no(inode),
     &          node_geom(res_geom(ires).node_no(inode)).node_id,
     &          node_type,
     &          error)
         end do
      end do
      hdf_dummy_integer = icount
      call h5ltset_attribute_int_f(hydro_id,".", 
     &           "Number of reservoir node connects",
     &           hdf_dummy_integer, scalar, error)


      if (icount .gt. 0) call reservoir_node_connect_write_buffer_to_hdf5(geom_id,error)
      call VerifyHDF5(error,"Reservoir node connections write")
      call reservoir_node_connect_clear_buffer()
      return
      end subroutine


***********************************************************************
***********************************************************************



      subroutine WriteNodeFlowConnectionsHDF5
      use dsm2_tidefile_input_storage_fortran
      use hdfvars
      use h5lt
      use hdf5
      use grid_data
      implicit none
      character(len=8) connect_type       
      integer :: icount,inode,iflow
      integer :: error
      integer:: scalar = 1
      integer,dimension(1) :: hdf_dummy_integer


      connect_type="qext"
      icount = 0
      do inode = 1,nnodes
          iflow=1
          do while(node_geom(inode).qext(iflow) .gt. 0)
              icount = icount + 1
              call node_flow_connections_append_to_buffer(icount,
     &                                                    inode,
     &                                                    node_geom(inode).node_id,iflow,
     &                                                    node_geom(inode).qext(iflow),
     &                                                    qext(node_geom(inode).qext(iflow)).name,     
     &                                                    connect_type,
     &                                                    error)
              iflow = iflow + 1
          end do
      end do
      connect_type = "transfer"
      do inode = 1,nnodes
          iflow=1
          do while(node_geom(inode).qinternal(iflow) .gt. 0)
              icount = icount + 1
              call node_flow_connections_append_to_buffer(icount,
     &                                                    inode,
     &                                                    node_geom(inode).node_id,iflow,
     &                                                    node_geom(inode).qinternal(iflow),
     &                                                    obj2obj(node_geom(inode).qinternal(iflow)).name,
     &                                                    connect_type,
     &                                                    error)
          iflow = iflow + 1
          end do
      end do
      if (icount .gt. 0) call node_flow_connections_write_buffer_to_hdf5(geom_id, error)          
      hdf_dummy_integer = icount
      call h5ltset_attribute_int_f(hydro_id,".", 
     &           "Number of node flow connects",
     &           hdf_dummy_integer, scalar, error)

      return
      end subroutine

***********************************************************************
***********************************************************************


      subroutine WriteReservoirFlowConnectionsHDF5
      use dsm2_tidefile_input_storage_fortran
      use hdfvars
      use h5lt
      use grid_data
      implicit none
      character(len=8) connect_type       
      integer :: icount,inode,iflow
      integer :: error
      integer:: scalar = 1
      integer,dimension(1) :: hdf_dummy_integer

      connect_type="qext"
      icount = 0
      do inode = 1,nreser
          iflow=1
          do while(res_geom(inode).qext(iflow) .gt. 0)
              icount = icount + 1
              call reservoir_flow_connections_append_to_buffer(icount,
     &                                                    res_geom(inode).name,
     &                                                    inode,
     &                                                    iflow,
     &                                                    res_geom(inode).qext(iflow),
     &                                                    qext(res_geom(inode).qext(iflow)).name,     
     &                                                    connect_type,
     &                                                    error)
              iflow = iflow + 1
          end do
      end do
      connect_type = "transfer"
      do inode = 1,nreser
          iflow=1
          do while(res_geom(inode).qinternal(iflow) .gt. 0)
              icount = icount + 1
              call reservoir_flow_connections_append_to_buffer(icount,     
     &                                                    res_geom(inode).name,
     &                                                    inode,iflow,     
     &                                                    res_geom(inode).qinternal(iflow),
     &                                                    obj2obj(res_geom(inode).qinternal(iflow)).name,
     &                                                    connect_type,
     &                                                    error)
          iflow = iflow + 1
          end do
      end do
      if (icount .gt. 0) call reservoir_flow_connections_write_buffer_to_hdf5(geom_id, error)          
      hdf_dummy_integer = icount
      call h5ltset_attribute_int_f(hydro_id,".", 
     &           "Number of reservoir flow connects",
     &           hdf_dummy_integer, scalar, error)

      return 
      end subroutine
      
      







