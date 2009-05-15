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

      subroutine ReadAttributesFromHDF5()

      use HDF5                  ! HDF5 This module contains all necessary modules 
      use hdfvars
      use inclvars
      use common_tide

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
      character*14,external :: jmin2cdt
      print*, "Reading HDF5 Header Information"

      a_data_dims(1) = 1
      
      call h5screate_simple_f(arank, adims, aspace_id, error)
      call h5tcopy_f(H5T_NATIVE_character, atype_id, error)
      call h5tset_size_f(atype_id, 21, error)

      call h5aopen_name_f(hydro_id,"Hydro Version",attr_id,error)
      call h5aread_f(attr_id, atype_id, chead, a_data_dims, error)
      call h5aclose_f(attr_id, error)

      call h5tcopy_f(H5T_NATIVE_INTEGER, atype_id, error)

      call h5aopen_name_f(hydro_id,"Maximum number of reservoirs",attr_id,error)
      call h5aread_f(attr_id, atype_id, dim_res_tf, a_data_dims, error)
c      call h5aclose_f(attr_id, error)
      call h5aopen_name_f(hydro_id,"Maximum number of channels",attr_id,error)
      call h5aread_f(attr_id, atype_id, dim_chan_tf, a_data_dims, error)
c      call h5aclose_f(attr_id, error)
      call h5aopen_name_f(hydro_id,"Number of reservoirs",attr_id,error)
      call h5aread_f(attr_id, atype_id, n_res_tf, a_data_dims, error)
c      call h5aclose_f(attr_id, error)
      call h5aopen_name_f(hydro_id,"Number of channels",attr_id,error)
      call h5aread_f(attr_id, atype_id, n_chan_tf, a_data_dims, error)
c      call h5aclose_f(attr_id, error)
      call h5aopen_name_f(hydro_id,"Number of stage boundaries",attr_id,error)
      call h5aread_f(attr_id, atype_id, nstgbnd, a_data_dims, error)
c      call h5aclose_f(attr_id, error)
      call h5aopen_name_f(hydro_id,"Start time",attr_id,error)
      call h5aread_f(attr_id, atype_id, h5_time_start, a_data_dims, error)
c      call h5aclose_f(attr_id, error)
	print*,"Read starting date:",jmin2cdt(h5_time_start)
      call h5aopen_name_f(hydro_id,"Time interval",attr_id,error)
      call h5aread_f(attr_id, atype_id, h5_time_interval, a_data_dims, error)
c      call h5aclose_f(attr_id, error)
      call h5aopen_name_f(hydro_id,"Number of intervals",attr_id,error)
      call h5aread_f(attr_id, atype_id, hdf5length, a_data_dims, error)
c     call h5aclose_f(attr_id, error)
      in_data_dims(1) = MaxChannels 
      
      
      ! Read map of external channel numbers int2ext
      call h5dopen_f(geom_id,"channel_number",in_dset_id,error)
      call h5dget_space_f(in_dset_id, filespace, error)
      call h5dread_f(in_dset_id,H5T_NATIVE_INTEGER, int2ext(1), in_data_dims, error)
      call h5sclose_f(filespace,error)
	call h5dclose_f(in_dset_id,error)


                                ! Read bottom_el
      call h5dopen_f(geom_id,"channel geometry",cg_dset_id,error)

      call h5dget_space_f(cg_dset_id, filespace, error)
c      call h5sclose_f(filespace,error)
c	call h5dclose_f(cg_dset_id)



      cg_data_dims(1) = MaxChannels 
      cg_data_dims(2) = 1
      h_offset(1) = 0
      h_offset(2) = bottom_elIdx
      call h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, 
     &     h_offset, cg_data_dims, error) 
      call h5screate_simple_f(cg_rank, cg_data_dims, memspace, error)
      call h5dread_f(cg_dset_id,H5T_NATIVE_REAL, bottom_el1, cg_data_dims, 
     &     error, memspace, filespace)



      h_offset(2) = bottom_elIdx + 1
      call h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, 
     &     h_offset, cg_data_dims, error) 
      call h5screate_simple_f(cg_rank, cg_data_dims, memspace, error)
      call h5dread_f(cg_dset_id,H5T_NATIVE_REAL, bottom_el2, cg_data_dims, 
     &     error, memspace, filespace)

      Do i = 1,MaxChannels            
         chan_geom(i).bottomelev(1) = bottom_el1(i)
         chan_geom(i).bottomelev(2) = bottom_el2(i)
      end do



                                ! Read node geometry

      ng_dims(1) = max_nodes 
      ng_dims(2) = max_qobj * 2 
      
      call h5dopen_f(geom_id, "node geometry", ng_dset_id, error)

      ng_data_dims(1) = ng_dims(1)   
      ng_data_dims(2) = 1 

                                ! Creation of hyperslab
      call h5dget_space_f(ng_dset_id, filespace, error)
      h_offset(1) = 0

                                ! Read node_geom.qint
      do i = 1,max_qobj
         h_offset(2) = i - 1
         call h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, 
     &        h_offset, ng_data_dims, error) 
         call h5screate_simple_f(ng_rank, ng_data_dims, memspace, error)
         call h5dread_f(ng_dset_id,H5T_NATIVE_INTEGER, node_obj, ng_data_dims, 
     &        error, memspace, filespace)
         do j = 1,max_nodes
            node_geom(j).qinternal(i) = node_obj(j)
         end do
      end do
                                ! Write out node_geom.qext
      do i = 1,max_qobj
         h_offset(2) = max_qobj + i - 1
         call h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, 
     &        h_offset, ng_data_dims, error) 
         call h5screate_simple_f(ng_rank, ng_data_dims, memspace, error)
         call h5dread_f(ng_dset_id,H5T_NATIVE_INTEGER, node_obj, ng_data_dims, 
     &        error, memspace, filespace)
         do j = 1,max_nodes
            node_geom(j).qext(i) = node_obj(j)
         end do
      end do


                                ! Read reservoir geometry

      rg_dims(1) = max_reservoirs
      rg_dims(2) = max_qobj * 2 
      
      call h5dopen_f(geom_id, "reservoir geometry", rg_dset_id, error)

      rg_data_dims(1) = rg_dims(1)   
      rg_data_dims(2) = 1 

                                ! Creation of hyperslab
      call h5dget_space_f(rg_dset_id, filespace, error)
      h_offset(1) = 0

                                ! Read res_geom.qint
      do i = 1,max_qobj
         h_offset(2) = i - 1
         call h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, 
     &        h_offset, rg_data_dims, error) 
         call h5screate_simple_f(rg_rank, rg_data_dims, memspace, error)
         call h5dread_f(rg_dset_id,H5T_NATIVE_INTEGER, res_obj, rg_data_dims, 
     &        error, memspace, filespace)
         do j = 1,max_reservoirs
            res_geom(j).qinternal(i) = res_obj(j)
         end do
      end do



                                ! Read res_geom.qext
      do i = 1,max_qobj
         h_offset(2) = max_qobj + i - 1
         call h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, 
     &        h_offset, rg_data_dims, error) 
         call h5screate_simple_f(rg_rank, rg_data_dims, memspace, error)
         call h5dread_f(rg_dset_id,H5T_NATIVE_INTEGER, res_obj, rg_data_dims, 
     &        error, memspace, filespace)
         do j = 1,max_reservoirs
            res_geom(j).qext(i) = res_obj(j)
         end do
      end do


                                ! Read Stage Boundaries

      bname_dims(1) = max_stgbnd
      bnode_dims(1) = max_stgbnd

      call h5tcopy_f(H5T_NATIVE_CHARACTER, dtc32_id, error)
      typesize = 32
      call h5tset_size_f(dtc32_id, typesize, error)
      
      call h5dopen_f(geom_id, "stage boundary names", bname_dset_id, error)
      call h5dopen_f(geom_id, "stage boundary nodes", bnode_dset_id, error)

      bname_data_dims(1) = bname_dims(1)   
      bnode_data_dims(1) = bnode_dims(1)   

      call h5dread_f(bname_dset_id,dtc32_id, bname_obj, bname_data_dims, error)
      call h5dread_f(bnode_dset_id,H5T_NATIVE_INTEGER, bnode_obj, bnode_data_dims, error)

      do i = 1,max_stgbnd
         stgbnd(i).name = bname_obj(i)
         stgbnd(i).node = bnode_obj(i)
      end do

      call h5tcopy_f(H5T_NATIVE_INTEGER, atype_id, error)
      call h5aopen_name_f(hydro_id,"Number of QExt",attr_id,error)
      call h5aread_f(attr_id, atype_id, nqext, a_data_dims, error)

      call InitQExtType2()
      call ReadQExtHDF5()

      return
      end

***********************************************************************
***********************************************************************

      subroutine ReadDataFromHDF5(tidetime)
      implicit none
	integer tidetime
	call SetHDF5ToTime(tidetime)
	call LagStageVariables
      call ReadChannelDataFromHDF5()
      call ReadReservoirDataFromHDF5()
      call ReadQExtDataFromHDF5()
      call ReadTransferFlowFromHDF5()
      
      return
      end

***********************************************************************
***********************************************************************


      subroutine LagStageVariables()
      use inclvars
      use common_tide
      implicit none
      AChanPrev=Achan
	YChanPrev=YChan
      return
      end subroutine

***********************************************************************
***********************************************************************




      subroutine ReadChannelDataFromHDF5()

      use HDF5 
      use hdfvars
      use inclvars
      use common_tide

      implicit none

      integer(HSIZE_T), dimension(3) :: h_offset
      integer        :: error   ! HDF5 Error flag

      h_offset(1) = 0
      h_offset(2) = 0
      h_offset(3) = hdf5point


           ! Read channel z area
      call h5dget_space_f (chan_z_dset_id, chan_z_fspace_id, error)
      call h5sselect_hyperslab_f(chan_z_fspace_id, H5S_SELECT_SET_F, 
     &     h_offset, chan_z_fsubset_dims, error) 
      call h5dread_f(chan_z_dset_id,H5T_NATIVE_REAL, YChan, chan_z_mdata_dims, 
     &     error, chan_z_memspace, chan_z_fspace_id)
      call VerifyHDF5(error,"Channel stage read")
      call h5sclose_f (chan_z_fspace_id, error)  
      call VerifyHDF5(error,"Channel stage dataspace closed")


           ! Read Channel Flow
      call h5dget_space_f (chan_q_dset_id, chan_q_fspace_id, error)
      call h5sselect_hyperslab_f(chan_q_fspace_id, H5S_SELECT_SET_F, 
     &     h_offset, chan_q_fsubset_dims, error) 
      call h5dread_f(chan_q_dset_id,H5T_NATIVE_REAL, QChan, chan_q_mdata_dims, 
     &     error, chan_q_memspace, chan_q_fspace_id)
      call VerifyHDF5(error,"Channel flow read")
      call h5sclose_f (chan_q_fspace_id, error)  
      call VerifyHDF5(error,"Channel flow dataspace closed")


           ! Read AChan
      call h5dget_space_f (chan_a_dset_id, chan_a_fspace_id, error)
      call h5sselect_hyperslab_f(chan_a_fspace_id, H5S_SELECT_SET_F, 
     &     h_offset, chan_a_fsubset_dims, error) 
      call h5dread_f(chan_a_dset_id,H5T_NATIVE_REAL, AChan, chan_a_mdata_dims, 
     &     error, chan_a_memspace, chan_a_fspace_id)
      call VerifyHDF5(error,"Channel area read")
      call h5sclose_f (chan_a_fspace_id, error)
      call VerifyHDF5(error,"Channel area dataspace closed")



      h_offset(1) = 0
      h_offset(2) = hdf5point
      h_offset(3) = 0

           ! Read Avg AChan
      call h5dget_space_f (chan_aa_dset_id, chan_aa_fspace_id, error)
      call h5sselect_hyperslab_f(chan_aa_fspace_id, H5S_SELECT_SET_F, 
     &     h_offset, chan_aa_fsubset_dims, error) 
      call h5dread_f(chan_aa_dset_id,H5T_NATIVE_REAL, AChan_Avg, chan_aa_mdata_dims, 
     &     error, chan_aa_memspace, chan_aa_fspace_id)
      call VerifyHDF5(error,"Channel avg area read")
      call h5sclose_f (chan_aa_fspace_id, error)  
      call VerifyHDF5(error,"Channel avg area dataspace closed")

      if (mod(hdf5point,24*10) .eq. 1) call h5garbage_collect_f(error)

      return
      end

***********************************************************************
***********************************************************************

      subroutine ReadReservoirDataFromHDF5()

      use HDF5                  ! HDF5 This module contains all necessary modules 
      use hdfvars
      use inclvars
      use common_tide
      implicit none

      integer(HSIZE_T), dimension(3) :: h_offset
      integer        :: error   ! HDF5 Error flag

      integer::i

           ! Creation of hyperslab
c-----call h5dget_space_f(res_dset_id, res_fspace_id, error)
      h_offset(1) = 0
      h_offset(2) = hdf5point
      h_offset(3) = 0

           ! Read Reservoir Height
      call h5dget_space_f (res_h_dset_id, res_h_fspace_id, error)
      call h5sselect_hyperslab_f(res_h_fspace_id, H5S_SELECT_SET_F, 
     &     h_offset, res_h_fsubset_dims, error) 
      call h5dread_f(res_h_dset_id,H5T_NATIVE_REAL, EResv, res_h_mdata_dims, 
     &     error, res_h_memspace, res_h_fspace_id)
      call VerifyHDF5(error,"Reservoir height read")
      call h5sclose_f (res_h_fspace_id, error)  
      call VerifyHDF5(error,"Reservoir height dataspace closed")



      h_offset(1) = 0
      h_offset(2) = 0
      h_offset(3) = hdf5point

           ! Read Reservoir Flow
      call h5dget_space_f (res_q_dset_id, res_q_fspace_id, error)
      call h5sselect_hyperslab_f(res_q_fspace_id, H5S_SELECT_SET_F, 
     &     h_offset, res_q_fsubset_dims, error)
      call h5dread_f(res_q_dset_id,H5T_NATIVE_REAL, QResv, res_q_mdata_dims, 
     &     error, res_q_memspace, res_q_fspace_id)
      call VerifyHDF5(error,"Reservoir flow read")
      call h5sclose_f (res_q_fspace_id, error)  
      call VerifyHDF5(error,"Reservoir flow dataspace closed")


      return
      end

***********************************************************************
***********************************************************************

      subroutine ReadTransferFlowFromHDF5()

      use HDF5                  ! HDF5 This module contains all necessary modules 
      use hdfvars
      use inclvars
      use grid_data

      implicit none
      integer        :: error   ! HDF5 Error flag

      integer(HSIZE_T), dimension(2) :: h_offset
      real*4, dimension(max_obj2obj) :: objavg
      integer::i
      continue
      if (nobj2obj .le. 0) return ! no transfer flows
                                ! Creation of hyperslab
      h_offset(1) = 0
      h_offset(2) = hdf5point

      call h5dget_space_f (transfer_dset_id, transfer_fspace_id, error)
      call h5sselect_hyperslab_f(transfer_fspace_id, H5S_SELECT_SET_F, 
     &     h_offset, transfer_fsubset_dims, error) 
      call h5dread_f(transfer_dset_id,H5T_NATIVE_REAL, objavg, transfer_mdata_dims, 
     &     error, transfer_memspace, transfer_fspace_id)
      call VerifyHDF5(error,"Transfer flow read")
      call h5sclose_f (transfer_fspace_id, error)  
      call VerifyHDF5(error,"Transfer flow dataspace closed")


      do i=1,max_obj2obj
         obj2obj(i).flow_avg=objavg(i)
      end do

      return
      end

***********************************************************************
***********************************************************************

      subroutine ReadQExtDataFromHDF5()

      use HDF5                  ! HDF5 This module contains all necessary modules 
      use hdfvars
      use inclvars
      use common_tide

      implicit none
      
      integer        :: error   ! HDF5 Error flag

      integer(HSIZE_T), dimension(2) :: h_offset
                                ! Dataset rank
      real*4, dimension(max_qext) :: qextavg

      integer::i
	integer::ipoint
	integer,save :: startpoint = miss_val_i
	integer,save :: last_loaded_tidefile=miss_val_i

      ! startpoint will be zero at the start of the simulation
	
      if (current_tidefile .ne. last_loaded_tidefile) then 
	  startpoint =0
	  last_loaded_tidefile = current_tidefile
	else
	  startpoint = hdf5point
	end if

      h_offset(1) = 0 
      
	!do ipoint=startpoint,hdf5point
      ipoint=hdf5point ! added
        h_offset(2) = ipoint
        call h5dget_space_f (qext_change_dset_id, qext_fspace_id, error)
        call h5sselect_hyperslab_f(qext_fspace_id, H5S_SELECT_SET_F, 
     &     h_offset, qext_fsubset_dims, error) 
        call h5dread_f(qext_change_dset_id,H5T_NATIVE_REAL, qextavg, qext_mdata_dims, 
     &     error, qext_memspace, qext_fspace_id)
        call VerifyHDF5(error,"Qext flow read")
        call h5sclose_f (qext_fspace_id, error)  

        !if (ipoint .eq. 0) then
        !   do i=1,nqext
        !      qext(i).avg = qextavg(i)
        !   end do
        !else
           do i=1,nqext
              qext(i).avg = qextavg(i) !qext(i).prev_avg + qextavg(i)
           end do
        !endif
        do i=1,nqext
           qext(i).prev_avg = qext(i).avg
        end do
      !end do
	startpoint=hdf5point
      return
      end

***********************************************************************
***********************************************************************

      subroutine ReadQExtHDF5()

      use HDF5
      use hdfvars
      use qextvars
      use inclvars
      use grid_data
      implicit none

      integer ::   error        ! Error flag
      integer(HSIZE_T), dimension(7) :: data_dims 
      integer(HSIZE_T), dimension(7) :: h_data_dims 
      integer(HSIZE_T), dimension(1) :: h_offset
      integer(HID_T) :: filespace ! Dataspace identifier 
      integer(HID_T) :: memspace ! memspace identifier 
      integer     ::    rank = 1 ! Dataset rank

      integer::i

      data_dims(1) = max_qext
      h_data_dims(1) = 1

                                !! TODO Add nqext as an attribute

      call h5dopen_f(geom_id, "qext", qext_dset_id, error)

      do i = 1,max_qext

         h_offset(1) = i - 1
         call h5dget_space_f(qext_dset_id, filespace, error)
         call h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, 
     &        h_offset, h_data_dims, error) 
         call h5screate_simple_f(rank, h_data_dims, memspace, error)

                                ! Select Hyperslab 
         call h5dread_f(qext_dset_id, q_name_tid, qext(i).name, data_dims, error, 
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dread_f(qext_dset_id, q_flow_tid, qext(i).flow, data_dims, error, 
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dread_f(qext_dset_id, q_prev_flow_tid, qext(i).prev_flow, data_dims, error, 
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dread_f(qext_dset_id, q_avg_tid, qext(i).avg, data_dims, error, 
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dread_f(qext_dset_id, q_prev_flow_tid, qext(i).prev_avg, data_dims, error, 
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dread_f(qext_dset_id, q_dsrc_type_tid, qext(i).datasource.source_type, data_dims, error, 
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dread_f(qext_dset_id, q_dsrc_idx_tid, qext(i).datasource.indx_ptr, data_dims, error, 
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dread_f(qext_dset_id, q_dsrc_val_tid, qext(i).datasource.value, data_dims, error, 
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dread_f(qext_dset_id, q_chng_idx_tid, qext(i).changed_ndx, data_dims, error, 
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dread_f(qext_dset_id, q_obj_name_tid, qext(i).obj_name, data_dims, error, 
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dread_f(qext_dset_id, q_attach_id_tid, qext(i).attach_obj_type, data_dims, error, 
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dread_f(qext_dset_id, q_attach_name_tid, qext(i).attach_obj_name, data_dims, error, 
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dread_f(qext_dset_id, q_attach_num_tid, qext(i).attach_obj_no, data_dims, error, 
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dread_f(qext_dset_id, q_grp_idx_tid, qext(i).group_ndx, data_dims, error, 
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dread_f(qext_dset_id, q_mass_frac_tid, qext(i).mass_frac, data_dims, error,
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
      end do


      return
      END
