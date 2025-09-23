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

!***********************************************************************
!***********************************************************************

subroutine Write1DStringArray(dest_id,name,arr,strlen,nstr)

      use HDF5
      use hdfvars
      use inclvars

      implicit none
      integer(HID_T),intent(in) :: dest_id ! Destination group identifier
      character*(*) :: name       ! Name of array
      integer(SIZE_T) :: strlen     ! Length of string element
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
      call h5dcreate_f(dest_id,trim(name),dtype_id, &
          dspace_id, dset_id, error)
      call h5dwrite_f(dset_id,dtype_id,  &
          arr(1), data_dims, error)
      call h5tclose_f(dtype_id,error)
      call h5sclose_f(dspace_id,error)
      call h5dclose_f(dset_id,error)
      return
end subroutine




!***********************************************************************
!***********************************************************************

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
      call h5sselect_hyperslab_f(chan_z_fspace_id, H5S_SELECT_SET_F, &
          h_offset, chan_z_fsubset_dims, error)
      call h5dwrite_f(chan_z_dset_id,H5T_NATIVE_REAL, HChan(:,1:nchans), chan_z_mdata_dims, &
          error, chan_z_memspace, chan_z_fspace_id)
      call VerifyHDF5(error,"Channel stage write")
      call h5sclose_f (chan_z_fspace_id, error)


          ! Write out AChan
      call h5dget_space_f (chan_a_dset_id, chan_a_fspace_id, error)
      call h5sselect_hyperslab_f(chan_a_fspace_id, H5S_SELECT_SET_F, &
          h_offset, chan_a_fsubset_dims, error)
      call h5dwrite_f(chan_a_dset_id,H5T_NATIVE_REAL, AChan(:,1:nchans), chan_a_mdata_dims, &
          error, chan_a_memspace, chan_a_fspace_id)
      call VerifyHDF5(error,"Channel area write")
      call h5sclose_f (chan_a_fspace_id, error)


      h_offset(1) = 0
      h_offset(3) = 0
      h_offset(2) = hdf5point
          ! Write out AChan avg
      call h5dget_space_f (chan_aa_dset_id, chan_aa_fspace_id, error)
      call h5sselect_hyperslab_f(chan_aa_fspace_id, H5S_SELECT_SET_F, &
          h_offset, chan_aa_fsubset_dims, error)
      call h5dwrite_f(chan_aa_dset_id,H5T_NATIVE_REAL, AChan_Avg(1:nchans), chan_aa_mdata_dims, &
          error, chan_aa_memspace, chan_aa_fspace_id)
      call VerifyHDF5(error,"Channel avg area write")
      call h5sclose_f (chan_aa_fspace_id, error)

      return
end

!***********************************************************************
!***********************************************************************

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
      call h5sselect_hyperslab_f(chan_q_fspace_id, H5S_SELECT_SET_F, &
          h_offset, chan_q_fsubset_dims, error)
      call h5dwrite_f(chan_q_dset_id,H5T_NATIVE_REAL, QChan(:,1:nchans), chan_q_mdata_dims, &
          error, chan_q_memspace, chan_q_fspace_id)
      call VerifyHDF5(error,"Channel flow write")
      call h5sclose_f (chan_q_fspace_id, error)

      return
end


subroutine WriteCompPointToHDF5()

      use HDF5                  ! HDF5 This module contains all necessary modules
      use hdfvars
      use inclvars
      use common_tide
      use chnlcomp, only: TotalCompLocations   ! to obtain the value for TotalComputations

      implicit none

      integer(HSIZE_T), dimension(2) :: h_offset

      integer        :: error   ! HDF5 Error flag

      ! Write out flow and stage at computation points
      h_offset(1) = 0
      h_offset(2) = hdf5point
          ! Write out flow
      call h5dget_space_f (cp_q_dset_id, cp_q_fspace_id, error)
      call h5sselect_hyperslab_f(cp_q_fspace_id, H5S_SELECT_SET_F, &
          h_offset, cp_q_fsubset_dims, error)
      call h5dwrite_f(cp_q_dset_id,H5T_NATIVE_REAL, Qcp(1:TotalCompLocations), cp_q_mdata_dims, &
          error, cp_q_memspace, cp_q_fspace_id)
      call VerifyHDF5(error,"Computational point flow write")
      call h5sclose_f (cp_q_fspace_id, error)

          ! Write out stage
      call h5dget_space_f (cp_z_dset_id, cp_z_fspace_id, error)
      call h5sselect_hyperslab_f(cp_z_fspace_id, H5S_SELECT_SET_F, &
          h_offset, cp_z_fsubset_dims, error)
      call h5dwrite_f(cp_z_dset_id,H5T_NATIVE_REAL, Zcp(1:TotalCompLocations), cp_z_mdata_dims, &
          error, cp_z_memspace, cp_z_fspace_id)
      call VerifyHDF5(error,"Computational point water surface write")
      call h5sclose_f (cp_z_fspace_id, error)

      return
end subroutine

!***********************************************************************
!***********************************************************************

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

      ! write out theta average value
      call h5dget_space_f (res_q_dset_id, res_q_fspace_id, error)
      call h5sselect_hyperslab_f(res_q_fspace_id, H5S_SELECT_SET_F,  &
          h_offset, res_q_fsubset_dims, error)
                                !fixme: this may have to be inverted
      call h5dwrite_f(res_q_dset_id,H5T_NATIVE_REAL,  &
          QResv, res_q_mdata_dims,  &
          error, res_q_memspace, res_q_fspace_id)
      call VerifyHDF5(error,"Reservoir flow write")
      call h5sclose_f (res_q_fspace_id, error)

      if (output_inst) then
        ! write out instantaneous value
          call h5dget_space_f (inst_res_q_dset_id, inst_res_q_fspace_id, error)
          call h5sselect_hyperslab_f(inst_res_q_fspace_id, H5S_SELECT_SET_F,  &
            h_offset, inst_res_q_fsubset_dims, error)
                                !fixme: this may have to be inverted
          call h5dwrite_f(inst_res_q_dset_id,H5T_NATIVE_REAL,  &
            inst_QResv, inst_res_q_mdata_dims,  &
            error, inst_res_q_memspace, inst_res_q_fspace_id)
          call VerifyHDF5(error,"Instantaneous Reservoir flow write")
          call h5sclose_f (inst_res_q_fspace_id, error)
      end if
      return
end subroutine

!***********************************************************************
!***********************************************************************

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
      call h5sselect_hyperslab_f(res_h_fspace_id, H5S_SELECT_SET_F,  &
          h_offset, res_h_fsubset_dims, error)
      call h5dwrite_f(res_h_dset_id,H5T_NATIVE_REAL, EResv, res_h_mdata_dims,  &
          error, res_h_memspace, res_h_fspace_id)
      call VerifyHDF5(error,"Reservoir height write")
      call h5sclose_f (res_h_fspace_id, error)

      return
end subroutine

!***********************************************************************
!***********************************************************************

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

      ! write out the theta average value
      call h5dget_space_f (transfer_dset_id, transfer_fspace_id, error)
      call h5sselect_hyperslab_f(transfer_fspace_id, H5S_SELECT_SET_F,  &
          h_offset, transfer_fsubset_dims, error)
      call h5dwrite_f(transfer_dset_id,H5T_NATIVE_REAL, objavg, transfer_mdata_dims,  &
          error, transfer_memspace, transfer_fspace_id)
      call VerifyHDF5(error,"Transfer flow write")
      call h5sclose_f (transfer_fspace_id, error)

      if (output_inst) then
        ! write out the instantaneous value
        call h5dget_space_f (inst_transfer_dset_id, inst_transfer_fspace_id, error)
        call h5sselect_hyperslab_f(inst_transfer_fspace_id, H5S_SELECT_SET_F,  &
            h_offset, inst_transfer_fsubset_dims, error)
        call h5dwrite_f(inst_transfer_dset_id,H5T_NATIVE_REAL, inst_obj2obj, inst_transfer_mdata_dims,  &
            error, inst_transfer_memspace, inst_transfer_fspace_id)
        call VerifyHDF5(error,"Instantaneous Transfer flow write")
        call h5sclose_f (inst_transfer_fspace_id, error)
      end if

      return
end subroutine

!***********************************************************************
!***********************************************************************

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

!-----Preprocess the values to changes
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

      ! write out the theta average value
      call h5dget_space_f (qext_change_dset_id, qext_fspace_id, error)
      call h5sselect_hyperslab_f(qext_fspace_id, H5S_SELECT_SET_F,  &
          h_offset, qext_fsubset_dims, error)
      call h5dwrite_f(qext_change_dset_id,H5T_NATIVE_REAL, qextavg, qext_mdata_dims,  &
          error, qext_memspace, qext_fspace_id)
      call VerifyHDF5(error,"Qextchanged write")
      call h5sclose_f (qext_fspace_id, error)

      if (output_inst) then
        ! write out the instantaneous value
        call h5dget_space_f (inst_qext_change_dset_id, inst_qext_fspace_id, error)
        call h5sselect_hyperslab_f(inst_qext_fspace_id, H5S_SELECT_SET_F,  &
            h_offset, inst_qext_fsubset_dims, error)
        call h5dwrite_f(inst_qext_change_dset_id,H5T_NATIVE_REAL, inst_qext, inst_qext_mdata_dims,  &
            error, inst_qext_memspace, inst_qext_fspace_id)
        call VerifyHDF5(error,"Instantaneous Qextchanged write")
        call h5sclose_f (inst_qext_fspace_id, error)
      end if

      return
end subroutine


subroutine write_gates_to_hdf5()
    !! Write gate status to the tide file
    use hdf5
    use inclvars
    use hdfvars
    use gates, only: nGate
    use common_tide, only: inst_device_flow

    implicit none

    integer(HSIZE_T), dimension(3) :: h_offset

    integer        :: error   ! HDF5 Error flag
!   real(kind=4), dimension(max_gates) :: gateposavg = 0.

    integer::i

    ! do i=1,ngates
    !     gateposavg(i) = gatepos(i).avg
    ! end do

    h_offset(1) = 0
    h_offset(2) = 0
    h_offset(3) = hdf5point

    ! write out the instantaneous value
    if (output_inst) then
        call h5dget_space_f(inst_deviceflow_dset_id, inst_deviceflow_fspace_id, error)
        call h5sselect_hyperslab_f(inst_deviceflow_fspace_id, H5S_SELECT_SET_F,  &
            h_offset, inst_deviceflow_fsubset_dims, error)
        call h5dwrite_f(inst_deviceflow_dset_id, H5T_NATIVE_REAL, &
            inst_device_flow(1:max(1, nGate), :), &
            inst_deviceflow_mdata_dims, error, inst_deviceflow_memspace, &
            inst_deviceflow_fspace_id)
        call VerifyHDF5(error,"Instantaneous Gates position write")
        call h5sclose_f (inst_deviceflow_fspace_id, error)
    end if

    return
end subroutine
