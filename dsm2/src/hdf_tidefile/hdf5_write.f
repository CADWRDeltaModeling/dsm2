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

***********************************************************************
***********************************************************************

      subroutine Write1DStringArray(dest_id,name,arr,strlen,nstr)

      use HDF5
      use hdfvars
      use inclvars

      implicit none
      integer(HID_T),intent(in) :: dest_id ! Destination group identifier      
      character*(*) :: name       ! Name of array
	integer       :: strlen     ! Length of string element
	integer       :: nstr       ! Dimension of string array
	character(LEN=strlen),dimension(nstr) :: arr

      integer(HID_T) :: dspace_id ! Dataspace identifier
      integer(HID_T) :: dtype_id  ! Dataspace identifier
      integer(HID_T) :: dset_id  ! Dataspace identifier
      integer,parameter  ::    drank = 1 ! Dataset rank


      integer,parameter  :: arank = 1  ! Attribute rank
      integer     :: error      ! HDF5 Error flag
      integer(HSIZE_T), dimension(arank) :: data_dims

	data_dims(1)=nstr
      call h5tcopy_f(H5T_NATIVE_CHARACTER, dtype_id, error)
      call h5tset_size_f(dtype_id, strlen, error)
	call h5screate_simple_f(drank, data_dims, dspace_id, error)
	call h5dcreate_f(dest_id,trim(name),dtype_id,
     &     dspace_id, dset_id, error)
	call h5dwrite_f(dset_id,dtype_id, 
     &     arr(1), data_dims, error)
	call h5tclose_f(dtype_id,error)
	call h5sclose_f(dspace_id,error)
	call h5dclose_f(dset_id,error)
      return
	end subroutine




***********************************************************************
***********************************************************************

      subroutine WriteChannelAreaToHDF5()

      use HDF5                  ! HDF5 This module contains all necessary modules
      use hdfvars
      use inclvars
      use common_tide

      implicit none

      integer(HSIZE_T), dimension(3) :: h_offset = (/0,0,0/)
      integer        :: error   ! HDF5 Error flag
      integer :: flag
      if (mod(hdf5point,24*10) .eq. 1) call h5garbage_collect_f(error)


          ! Write out channel stage
      h_offset(1) = 0
      h_offset(2) = 0
      h_offset(3) = hdf5point
  

      call h5dget_space_f (chan_z_dset_id, chan_z_fspace_id, error)
      call h5sselect_hyperslab_f(chan_z_fspace_id, H5S_SELECT_SET_F,
     &     h_offset, chan_z_fsubset_dims, error)
      call h5dwrite_f(chan_z_dset_id,H5T_NATIVE_REAL, YChan, chan_z_mdata_dims,
     &     error, chan_z_memspace, chan_z_fspace_id)
	call VerifyHDF5(error,"Channel stage write")
      call h5sclose_f (chan_z_fspace_id, error)
   

          ! Write out AChan
      call h5dget_space_f (chan_a_dset_id, chan_a_fspace_id, error)
      call h5sselect_hyperslab_f(chan_a_fspace_id, H5S_SELECT_SET_F,
     &     h_offset, chan_a_fsubset_dims, error)
      call h5dwrite_f(chan_a_dset_id,H5T_NATIVE_REAL, AChan, chan_a_mdata_dims,
     &     error, chan_a_memspace, chan_a_fspace_id)
	call VerifyHDF5(error,"Channel area write")
      call h5sclose_f (chan_a_fspace_id, error)


      h_offset(1) = 0
      h_offset(3) = 0
      h_offset(2) = hdf5point
          ! Write out AChan avg
      call h5dget_space_f (chan_aa_dset_id, chan_aa_fspace_id, error)
      call h5sselect_hyperslab_f(chan_aa_fspace_id, H5S_SELECT_SET_F,
     &     h_offset, chan_aa_fsubset_dims, error)
      call h5dwrite_f(chan_aa_dset_id,H5T_NATIVE_REAL, AChan_Avg, chan_aa_mdata_dims,
     &     error, chan_aa_memspace, chan_aa_fspace_id)
	call VerifyHDF5(error,"Channel avg area write")
      call h5sclose_f (chan_aa_fspace_id, error)   

      return
      end

***********************************************************************
***********************************************************************

      subroutine WriteChannelFlowToHDF5()

      use HDF5                  ! HDF5 This module contains all necessary modules
      use hdfvars
      use inclvars
      use common_tide

      implicit none

      integer(HSIZE_T), dimension(3) :: h_offset

      integer        :: error   ! HDF5 Error flag

      h_offset(1) = 0
      h_offset(2) = 0
      h_offset(3) = hdf5point   ! represents the time index, which can be
	                          ! is increased by one for each write

      call h5dget_space_f (chan_q_dset_id, chan_q_fspace_id, error)
      call h5sselect_hyperslab_f(chan_q_fspace_id, H5S_SELECT_SET_F,
     &     h_offset, chan_q_fsubset_dims, error)
      call h5dwrite_f(chan_q_dset_id,H5T_NATIVE_REAL, QChan, chan_q_mdata_dims,
     &     error, chan_q_memspace, chan_q_fspace_id)
	call VerifyHDF5(error,"Channel flow write")
      call h5sclose_f (chan_q_fspace_id, error)

      return
      end

***********************************************************************
***********************************************************************

      subroutine WriteReservoirFlowToHDF5()

      use HDF5                  ! HDF5 This module contains all necessary modules 
      use hdfvars
      use inclvars
      use common_tide

      implicit none

      integer(HSIZE_T), dimension(3) :: h_offset

      integer        :: error   ! HDF5 Error flag
      integer :: i



      h_offset(1) = 0
      h_offset(2) = 0
      h_offset(3) = hdf5point

      call h5dget_space_f (res_q_dset_id, res_q_fspace_id, error)
      call h5sselect_hyperslab_f(res_q_fspace_id, H5S_SELECT_SET_F, 
     &     h_offset, res_q_fsubset_dims, error)
                                !fixme: this may have to be inverted
      call h5dwrite_f(res_q_dset_id,H5T_NATIVE_REAL, 
     &     QResv, res_q_mdata_dims, 
     &     error, res_q_memspace, res_q_fspace_id)
	call VerifyHDF5(error,"Reservoir flow write")
      call h5sclose_f (res_q_fspace_id, error)      

      return
      end

***********************************************************************
***********************************************************************

      subroutine WriteReservoirHeightToHDF5()

      use HDF5                  ! HDF5 This module contains all necessary modules 
      use hdfvars
      use inclvars
      use common_tide

      implicit none

      integer(HSIZE_T), dimension(3) :: h_offset
      integer        :: error   ! HDF5 Error flag

      h_offset(1) = 0
      h_offset(2) = hdf5point

      call h5dget_space_f (res_h_dset_id, res_h_fspace_id, error)
      call h5sselect_hyperslab_f(res_h_fspace_id, H5S_SELECT_SET_F, 
     &     h_offset, res_h_fsubset_dims, error) 
      call h5dwrite_f(res_h_dset_id,H5T_NATIVE_REAL, EResv, res_h_mdata_dims, 
     &     error, res_h_memspace, res_h_fspace_id)
	call VerifyHDF5(error,"Reservoir height write")
      call h5sclose_f (res_h_fspace_id, error)      



      return
      end

***********************************************************************
***********************************************************************

      subroutine WriteTransferFlowToHDF5()

      use HDF5                  ! HDF5 This module contains all necessary modules 
      use hdfvars
      use inclvars
      use common_tide

      implicit none

      integer(HSIZE_T), dimension(2) :: h_offset

      integer        :: error   ! HDF5 Error flag
      real(kind=4), dimension(max_qext) :: objavg = 0.

      integer::i

      do i=1,nobj2obj
         objavg(i) = obj2obj(i).flow_avg
      end do

      h_offset(1) = 0
      h_offset(2) = hdf5point
      call h5dget_space_f (transfer_dset_id, transfer_fspace_id, error)
      call h5sselect_hyperslab_f(transfer_fspace_id, H5S_SELECT_SET_F, 
     &     h_offset, transfer_fsubset_dims, error) 
      call h5dwrite_f(transfer_dset_id,H5T_NATIVE_REAL, objavg, transfer_mdata_dims, 
     &     error, transfer_memspace, transfer_fspace_id)
	call VerifyHDF5(error,"Transfer flow write")
      call h5sclose_f (transfer_fspace_id, error)

      return
      end

***********************************************************************
***********************************************************************

      subroutine WriteQExtChangedToHDF5()

      use HDF5                  ! HDF5 This module contains all necessary modules 
      use hdfvars
      use inclvars
      use common_tide

      implicit none

      integer(HSIZE_T), dimension(2) :: h_offset

      integer        :: error   ! HDF5 Error flag
      real(kind=4), dimension(max_qext) :: qextavg = 0.

      integer::i

c-----Preprocess the values to changes
! fixme: should this be done here?
      !if (hdf5point .eq. 0) then
      !   do i=1,max_qext
      !      qextavg(i) = qext(i).avg
      !   end do
      !else
         do i=1,max_qext
            qextavg(i) = qext(i).avg !- qext(i).prev_avg 
         end do
      !endif
      do i=1,max_qext
         qext(i).prev_avg = qext(i).avg
      end do

                                ! Creation of hyperslab
      h_offset(1) = 0
      h_offset(2) = hdf5point
      call h5dget_space_f (qext_change_dset_id, qext_fspace_id, error)
      call h5sselect_hyperslab_f(qext_fspace_id, H5S_SELECT_SET_F, 
     &     h_offset, qext_fsubset_dims, error) 
      call h5dwrite_f(qext_change_dset_id,H5T_NATIVE_REAL, qextavg, qext_mdata_dims, 
     &     error, qext_memspace, qext_fspace_id)
	call VerifyHDF5(error,"Qextchanged write")
      call h5sclose_f (qext_fspace_id, error)

      return
      end

***********************************************************************
***********************************************************************

      subroutine WriteObj2ObjAttribute()

      use HDF5
      use hdfvars
      use objvars
      use inclvars
      use grid_data

      implicit none

      integer ::   error        ! Error flag
      integer(HSIZE_T), dimension(1) :: data_dims 
      integer(HSIZE_T), dimension(1) :: h_data_dims 
      integer(HSIZE_T), dimension(1) :: h_offset
      integer(HID_T) :: filespace ! Dataspace identifier 
      integer(HID_T) :: memspace ! memspace identifier 
      integer     ::    rank = 1 ! Dataset rank

      integer::i

      data_dims(1) = max_obj2obj
      h_data_dims(1) = 1

      do i = 1,max_obj2obj

         h_offset(1) = i - 1
         call h5dget_space_f(obj2obj_dset_id, filespace, error)
         call h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, 
     &        h_offset, h_data_dims, error) 
         call h5screate_simple_f(rank, h_data_dims, memspace, error)
         call h5dwrite_f(obj2obj_dset_id, o_name_tid, obj2obj(i).name, data_dims, error, 
     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(obj2obj_dset_id, fo_id_tid, obj2obj(i).from_obj.obj_type, data_dims, error,
     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(obj2obj_dset_id, fo_name_tid, obj2obj(i).from_obj.obj_name, data_dims, error, 
     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(obj2obj_dset_id, fo_num_tid, obj2obj(i).from_obj.obj_no, data_dims, error, 
     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(obj2obj_dset_id, fo_hychan_tid, obj2obj(i).from_obj.hydrochan, data_dims, error, 
     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(obj2obj_dset_id, fo_massfrac_tid, obj2obj(i).from_obj.mass_frac, data_dims, error, 
     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(obj2obj_dset_id, fo_coeff_tid, obj2obj(i).from_obj.coeff, data_dims, error, 
     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(obj2obj_dset_id, to_id_tid, obj2obj(i).to_obj.obj_type, data_dims, error, 
     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(obj2obj_dset_id, to_name_tid, obj2obj(i).to_obj.obj_name, data_dims, error, 
     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(obj2obj_dset_id, to_num_tid, obj2obj(i).to_obj.obj_no, data_dims, error, 
     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(obj2obj_dset_id, to_hychan_tid, obj2obj(i).to_obj.hydrochan, data_dims, error, 
     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(obj2obj_dset_id, to_massfrac_tid, obj2obj(i).to_obj.mass_frac, data_dims, error, 
     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(obj2obj_dset_id, to_coeff_tid, obj2obj(i).to_obj.coeff, data_dims, error, 
     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(obj2obj_dset_id, constval_tid, obj2obj(i).constant_value, data_dims, error, 
     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(obj2obj_dset_id, datasrc_type_tid,obj2obj(i).datasource.source_type, data_dims, error, 
     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(obj2obj_dset_id, datasrc_idx_tid, obj2obj(i).datasource.indx_ptr, data_dims, error, 
     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)   
c         call h5dwrite_f(obj2obj_dset_id, datasrc_val_tid, obj2obj(i).datasource.value, data_dims, error, 
c     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(obj2obj_dset_id, curQ_tid, obj2obj(i).flow, data_dims, error, 
     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(obj2obj_dset_id, prevQ_tid, obj2obj(i).prev_flow, data_dims, error, 
     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(obj2obj_dset_id, avgQ_tid, obj2obj(i).flow_avg, data_dims, error, 
     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(obj2obj_dset_id, conc_tid, obj2obj(i).constituent_conc, data_dims, error, 
     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)
      end do

      return
      end

***********************************************************************
***********************************************************************

      subroutine WriteQExt()

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

      if (rank .eq. 1) then
!         return
      endif

                                !! TODO Add nqext as an attribute  !! done in init??

      do i = 1,max_qext

         h_offset(1) = i - 1
         call h5dget_space_f(qext_dset_id, filespace, error)
         call h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F,
     &        h_offset, h_data_dims, error)
         call h5screate_simple_f(rank, h_data_dims, memspace, error)

                                ! Select Hyperslab
         call h5dwrite_f(qext_dset_id, q_name_tid, qext(i).name, data_dims, error,
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(qext_dset_id, q_flow_tid, qext(i).flow, data_dims, error,
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(qext_dset_id, q_prev_flow_tid, qext(i).prev_flow, data_dims, error,
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(qext_dset_id, q_avg_tid, qext(i).avg, data_dims, error,
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(qext_dset_id, q_prev_flow_tid, qext(i).prev_avg, data_dims, error,
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(qext_dset_id, q_dsrc_type_tid, qext(i).datasource.source_type, data_dims, error,
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(qext_dset_id, q_dsrc_idx_tid, qext(i).datasource.indx_ptr, data_dims, error,
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
c        todo: URGENT breaks compatibility with hdf5 1.8.3     
c         call h5dwrite_f(qext_dset_id, q_dsrc_val_tid, qext(i).datasource.value, data_dims, error,
c     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(qext_dset_id, q_chng_idx_tid, qext(i).changed_ndx, data_dims, error,
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(qext_dset_id, q_obj_name_tid, qext(i).obj_name, data_dims, error,
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(qext_dset_id, q_attach_id_tid, qext(i).attach_obj_type, data_dims, error,
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(qext_dset_id, q_attach_name_tid, qext(i).attach_obj_name, data_dims, error,
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(qext_dset_id, q_attach_num_tid, qext(i).attach_obj_no, data_dims, error,
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(qext_dset_id, q_grp_idx_tid, qext(i).group_ndx, data_dims, error,
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(qext_dset_id, q_mass_frac_tid, qext(i).mass_frac, data_dims, error,
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
      end do

      return
      end

***********************************************************************
***********************************************************************
