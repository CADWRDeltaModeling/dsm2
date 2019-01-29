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
      call h5dwrite_f(chan_z_dset_id,H5T_NATIVE_REAL, HChan(:,1:nchans), chan_z_mdata_dims,
     &     error, chan_z_memspace, chan_z_fspace_id)
      call VerifyHDF5(error,"Channel stage write")
      call h5sclose_f (chan_z_fspace_id, error)
   

          ! Write out AChan
      call h5dget_space_f (chan_a_dset_id, chan_a_fspace_id, error)
      call h5sselect_hyperslab_f(chan_a_fspace_id, H5S_SELECT_SET_F,
     &     h_offset, chan_a_fsubset_dims, error)
      call h5dwrite_f(chan_a_dset_id,H5T_NATIVE_REAL, AChan(:,1:nchans), chan_a_mdata_dims,
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
      call h5dwrite_f(chan_aa_dset_id,H5T_NATIVE_REAL, AChan_Avg(1:nchans), chan_aa_mdata_dims,
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
      call h5dwrite_f(chan_q_dset_id,H5T_NATIVE_REAL, QChan(:,1:nchans), chan_q_mdata_dims,
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
      if (nres_connect .eq. 0) return


      h_offset(1) = 0
      h_offset(2) = hdf5point

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
      end subroutine

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
      end subroutine

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
      end subroutine

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
      end subroutine

***********************************************************************
***********************************************************************


