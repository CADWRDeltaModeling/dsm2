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


!***********************************************************************
!***********************************************************************


subroutine ReadDataFromHDF5(tidetime)
      implicit none
      integer tidetime
      integer index
      integer SetHDF5ToTime
      index = SetHDF5ToTime(tidetime)
      call LagStageVariables
      call ReadChannelDataFromHDF5()
      call ReadReservoirDataFromHDF5()
      call ReadQExtDataFromHDF5()
      call ReadTransferFlowFromHDF5()
      
      return
end subroutine

!***********************************************************************
!***********************************************************************


subroutine LagStageVariables()
      use inclvars
      use common_tide
      implicit none
      AChanPrev=Achan
      HChanPrev=HChan
      return
end subroutine

!***********************************************************************
!***********************************************************************

subroutine ReadChannelDataFromHDF5()

      use HDF5 
      use hdfvars
      use inclvars
      use common_tide

      implicit none

      integer(HSIZE_T), dimension(3) :: h_offset
      integer        :: error   ! HDF5 Error flag
      integer :: ichan
      h_offset(1) = 0
      h_offset(2) = 0
      h_offset(3) = hdf5point

           ! Read channel z area
      call h5dget_space_f (chan_z_dset_id, chan_z_fspace_id, error)
      call h5sselect_hyperslab_f(chan_z_fspace_id, H5S_SELECT_SET_F, &
          h_offset, chan_z_fsubset_dims, error) 
      call h5dread_f(chan_z_dset_id,H5T_NATIVE_REAL, HChan(:,1:nchans), chan_z_mdata_dims, &
          error, chan_z_memspace, chan_z_fspace_id)
      
      !do ichan=1,nchans
      !   HChan(1,ichan)=ZChan(1,ichan)-chan_geom(ichan).bottomelev(1)
      !   HChan(2,ichan)=ZChan(2,ichan)-chan_geom(ichan).bottomelev(2)
      !end do
      do ichan=1,nchans
          ZChan(1,ichan)=HChan(1,ichan)+chan_geom(ichan).bottomelev(1)
          ZChan(2,ichan)=HChan(2,ichan)+chan_geom(ichan).bottomelev(2)
      end do
      call VerifyHDF5(error,"Channel stage read")
      call h5sclose_f (chan_z_fspace_id, error)  
      call VerifyHDF5(error,"Channel stage dataspace closed")


           ! Read Channel Flow
      call h5dget_space_f (chan_q_dset_id, chan_q_fspace_id, error)
      call h5sselect_hyperslab_f(chan_q_fspace_id, H5S_SELECT_SET_F, &
          h_offset, chan_q_fsubset_dims, error) 
      call h5dread_f(chan_q_dset_id,H5T_NATIVE_REAL, QChan(:,1:nchans), chan_q_mdata_dims, &
          error, chan_q_memspace, chan_q_fspace_id)
      call VerifyHDF5(error,"Channel flow read")
      call h5sclose_f (chan_q_fspace_id, error)  
      call VerifyHDF5(error,"Channel flow dataspace closed")


           ! Read AChan
      call h5dget_space_f (chan_a_dset_id, chan_a_fspace_id, error)
      call h5sselect_hyperslab_f(chan_a_fspace_id, H5S_SELECT_SET_F, &
          h_offset, chan_a_fsubset_dims, error) 
      call h5dread_f(chan_a_dset_id,H5T_NATIVE_REAL, AChan(:,1:nchans), chan_a_mdata_dims, &
          error, chan_a_memspace, chan_a_fspace_id)
      call VerifyHDF5(error,"Channel area read")
      call h5sclose_f (chan_a_fspace_id, error)
      call VerifyHDF5(error,"Channel area dataspace closed")



      h_offset(1) = 0
      h_offset(2) = hdf5point
      h_offset(3) = 0

           ! Read Avg AChan
      call h5dget_space_f (chan_aa_dset_id, chan_aa_fspace_id, error)
      call h5sselect_hyperslab_f(chan_aa_fspace_id, H5S_SELECT_SET_F, &
          h_offset, chan_aa_fsubset_dims, error) 
      call h5dread_f(chan_aa_dset_id,H5T_NATIVE_REAL, AChan_Avg(1:nchans), chan_aa_mdata_dims, &
          error, chan_aa_memspace, chan_aa_fspace_id)
      call VerifyHDF5(error,"Channel avg area read")
      call h5sclose_f (chan_aa_fspace_id, error)  
      call VerifyHDF5(error,"Channel avg area dataspace closed")

      if (mod(hdf5point,24*10) .eq. 1) call h5garbage_collect_f(error)

      return
end

!***********************************************************************
!***********************************************************************

subroutine ReadReservoirDataFromHDF5()

      use HDF5                  ! HDF5 This module contains all necessary modules 
      use hdfvars
      use inclvars
      use grid_data
      use common_tide
      use chconnec, only: QRes
      implicit none

      integer(HSIZE_T), dimension(3) :: h_offset
      integer        :: error   ! HDF5 Error flag

      integer:: i,j,kk
      integer:: iconnect

           ! Creation of hyperslab
!-----call h5dget_space_f(res_dset_id, res_fspace_id, error)
      if (nreser .ne. 0)then
          h_offset(1) = 0
          h_offset(2) = hdf5point
          h_offset(3) = 0

           ! Read Reservoir Height
          call h5dget_space_f (res_h_dset_id, res_h_fspace_id, error)
          call h5sselect_hyperslab_f(res_h_fspace_id, H5S_SELECT_SET_F, &
              h_offset, res_h_fsubset_dims, error) 
          call h5dread_f(res_h_dset_id,H5T_NATIVE_REAL, EResv, res_h_mdata_dims, &
              error, res_h_memspace, res_h_fspace_id)
          call VerifyHDF5(error,"Reservoir height read")
          call h5sclose_f (res_h_fspace_id, error)  
          call VerifyHDF5(error,"Reservoir height dataspace closed")

          h_offset(1) = 0
          h_offset(2) = hdf5point

           ! Read Reservoir Flow
          call h5dget_space_f (res_q_dset_id, res_q_fspace_id, error)
          call h5sselect_hyperslab_f(res_q_fspace_id, H5S_SELECT_SET_F, &
              h_offset, res_q_fsubset_dims, error)
          call h5dread_f(res_q_dset_id,H5T_NATIVE_REAL, QResv, res_q_mdata_dims, &
              error, res_q_memspace, res_q_fspace_id)
          call VerifyHDF5(error,"Reservoir flow read")
          call h5sclose_f (res_q_fspace_id, error)  
          call VerifyHDF5(error,"Reservoir flow dataspace closed")

!--------assign flows and concentrations to objects
!--------reservoirs
         iconnect = 0
         do j=1, nreser
            do kk=1, res_geom(j).nnodes
               iconnect = iconnect+1
               qres(j,kk)=dble(qresv(iconnect))
            enddo
         enddo
         
          
      end if    
      return
      end subroutine

!***********************************************************************
!***********************************************************************

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
      call h5sselect_hyperslab_f(transfer_fspace_id, H5S_SELECT_SET_F, &
          h_offset, transfer_fsubset_dims, error) 
      call h5dread_f(transfer_dset_id,H5T_NATIVE_REAL, objavg, transfer_mdata_dims, &
          error, transfer_memspace, transfer_fspace_id)
      call VerifyHDF5(error,"Transfer flow read")
      call h5sclose_f (transfer_fspace_id, error)  
      call VerifyHDF5(error,"Transfer flow dataspace closed")


      do i=1,max_obj2obj
         obj2obj(i).flow_avg=objavg(i)
      end do

      return
end

!***********************************************************************
!***********************************************************************

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
      call h5sselect_hyperslab_f(qext_fspace_id, H5S_SELECT_SET_F, &
          h_offset, qext_fsubset_dims, error) 
      call h5dread_f(qext_change_dset_id,H5T_NATIVE_REAL, qextavg, qext_mdata_dims, &
          error, qext_memspace, qext_fspace_id)
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

!***********************************************************************
!***********************************************************************

subroutine ReadQExtHDF5()

      use HDF5
      use hdfvars
      use qextvars
      use inclvars
      use grid_data
      use common_tide
      use dsm2_tidefile_input_storage_fortran
      implicit none
      integer ::   error        ! Error flag
      integer::i
      character*32 :: name
      if (nqext .eq. 0) then
         ! don't try to read
         return
      endif
                                !! TODO Add nqext as an attribute  !! done in init??
      call qext_clear_buffer()
      call qext_read_buffer_from_hdf5(geom_id,error)
      call VerifyHDF5(error,"Read qext attributes from buffer")
      !nqext=qext_buffer_size() 
      !todo This causes a weird memory bug
      call qext_number_rows_hdf5(geom_id, nqext,error)
      call VerifyHDF5(error,"Query size of qext in hdf5")

      call qext_read_buffer_from_hdf5(geom_id,error)
      do i = 1,nqext
          call qext_query_from_buffer(i,&
                                      name,&
                                      qext(i).attach_obj_name,&
                                      qext(i).attach_obj_type,&
                                      qext(i).attach_obj_no,&
                                      error)
      
          qext(i).name = trim(name)

      end do
      call qext_clear_buffer()

      return
END
