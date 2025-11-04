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
module hdf_tidefile
    use mod_tide_time
    use mod_hdf5_init
    use constants, only: miss_val_i
    implicit none

    abstract interface
        subroutine abs_process_tide(new_tidefile, &
            first_used_tidefile, &
            tidefile_ndx)
            logical :: new_tidefile
            integer :: first_used_tidefile
            integer :: tidefile_ndx
        end subroutine abs_process_tide
    end interface
contains
 subroutine read_mult_tide(process_tide)

!-----Read multiple Hydro tidefiles.  Each tidefile can have multiple
!-----tides.  A tide is flow data averaged between the last and
!-----this timestamp for each tide.
!-----Determine if data is available in current or new tidefile;
!-----read tidefile if necessary and store data.
      use io_units
      use logging
      use common_tide
      use runtime_data
      use iopath_data

      implicit none
    procedure(abs_process_tide), pointer, intent(in) :: process_tide

!-----local variables
      character &
          filenm*150, &           ! current tidefile name
          jmin2cdt*14         ! julian minute to char function

      integer &
          i                    ! loop indices


      integer, save :: prev_read_tidetime = miss_val_i
      integer, save :: first_used_tidefile = miss_val_i
      integer, save :: prev_tidefile = miss_val_i

      logical &
          new_tidefile, &         ! true if new tidefile
          foundtime



      external jmin2cdt

      foundtime = .false.
      do i=max(current_tidefile,1),nintides
         if (julmin .ge. tide_files(i).start_julmin .and. &
             julmin .le. tide_files(i).end_julmin) then
              new_tidefile=current_tidefile .ne. i
	      if (new_tidefile .and. current_tidefile .ne. miss_val_i) &
              call CloseHDF5()
              prev_tidefile=current_tidefile
              current_tidefile=i
              if (first_used_tidefile .eq. miss_val_i) then
                  first_used_tidefile=i
              end if
              foundtime = .true.
	      exit
         endif
      enddo
      if (.not. foundtime) then
610     format(/'Unable to find a tidefile for current time: ',a)
        write(unit_error,610) current_date
        call exit(2)
      end if

      filenm=tide_files(current_tidefile).filename
      if (new_tidefile) then
	   !@todo: why do we read_tide_head? We want some specific dated info,
	   !       but mostly this is just dangerous.
	   !       It would be better to have a smaller read of any
	   !       info that actually changes between tidefiles.
         call read_tide_head(filenm, .true.)
         if (print_level.ge.1) then
            write(unit_screen,922) trim(filenm),  current_date
            write(unit_output,922) trim(filenm), current_date
 922        format(/'Opened a new tidefile: ',/a &
                /' model time: ',a)
         endif
         ! When a transition occurs at t0, the old tidefile is read for the
	   ! time step that reads t0. Here, we are about to read/calculate
	   ! t1. Need to reload the data from t0 as if it came from the new
	   ! tidefile, and then reconcile any differences in ProcessTide.
         call ReadDataFromHDF5(tide_files(current_tidefile).start_julmin)
         call process_tide(new_tidefile,first_used_tidefile,current_tidefile)
         prev_read_tidetime = getCurrentTideTime() ! forces a second read based on julmin

      endif
      new_tidefile = .false.
!-----read tide flows
	if (julmin .gt. prev_read_tidetime ) then
	  ! flow values must be updated, rather than reused
	   call ReadDataFromHDF5(julmin)
	   prev_read_tidetime = getCurrentTideTime()
       call process_tide(new_tidefile,first_used_tidefile,current_tidefile)
    end if


	return
end subroutine

subroutine ReadDataFromHDF5(tidetime)
      implicit none
      integer tidetime
      integer index
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

subroutine hdf5_read_attributes()

      use HDF5                  ! HDF5 This module contains all necessary modules
      use h5lt
      use hdfvars
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

      real(kind=4), allocatable :: bottom_el1(:), bottom_el2(:)

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
      call h5aopen_name_f(hydro_id,"Number of quadrature points",attr_id,error)
      call h5aread_f(attr_id, atype_id, hdf5length, a_data_dims, error)
!     call h5aclose_f(attr_id, error)
      call h5aopen_name_f(hydro_id,"Hydro theta",attr_id,error)
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

      if(not(allocated(bottom_el1)))allocate(bottom_el1(n_chan_tf))

      call h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, &
          h_offset, cg_data_dims, error)
      call h5screate_simple_f(cg_rank, cg_data_dims, memspace, error)
      call h5dread_f(cg_dset_id,H5T_NATIVE_REAL, bottom_el1, cg_data_dims, &
          error, memspace, filespace)


      if(not(allocated(bottom_el2)))allocate(bottom_el2(n_chan_tf))

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
      
      deallocate (bottom_el1, bottom_el2)

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

subroutine read_tide_head(filenm, check_headers)

!-----read in header info from DSM2-Hydro binary tidefile
!-----or HDF5 tidefile.
!-----if check_headers is true, also check headers
      use IO_Units
      use groups_data, only: ngroup
      use hdfvars
      use common_tide
      use runtime_data
      use iopath_data
      use grid_data
      use qual_param
      use bltm
      implicit none


!-----local variables

      integer unit_tide, &        ! binary tide file unit number
          lnblnk, &              ! intrisic last non blank function
          i,j,n,p               ! loop indices

      logical check_headers, &    ! [INPUT]
          file_exists		      ! TRUE if file exists

      character(LEN=*) :: filenm ! [INPUT]

      integer objType            ! type of object, channel, reservoir,...
      integer getReservoirId, getStageId, getExternalId, xgetInternalId
      integer getGroupIndex
      integer aIndex
      character*130 ctmp         ! temporary storage for string
!      character*21 chead        ! header (same length as 'hydro version '+dsm2_version)
      logical updated_pathoutput, &
          constituent_found     ! true if this constituent found for qext
      data updated_pathoutput /.false./
      common /tide_l/ updated_pathoutput

 810  format(/'Note: Blank accounting name in ', &
          a, ' flow (from tidefile)' &
          /' for ',a,' ',a)

 815  format(/'Warning: multiple concentrations assigned to '/ &
          a, ' ',a,' external flow (from tidefile), last one used.')

 820  format(/'Error: could not find ', a, ' flow (from tidefile)' &
          /' accounting name ',a,' in input account names.')

 825  format(/'Too many individual constituents, increase size of' &
          /' max_conpth:',i2)

      tidefile_version=' '



!-----open tidefile and check version

      hdf5_hydrofile=trim(filenm)
      inquire (file=hdf5_hydrofile, exist=file_exists)
      if (.not. file_exists) goto 900
      ! Opens the file and groups for DSM2
      call OpenHDF5()
      ! Initialize memory spaces for reading/writing
      call hdf5_read_attributes()
      if (chead(:5) .ne. 'Hydro') then
          tidefile_version=' '
      else
          tidefile_version=chead(15:)
      endif


      do i=1,nqext
!--------fix incorrect mass_frac value from old tidefiles
         if (qext(i).mass_frac .eq. miss_val_r) then
            qext(i).mass_frac=1.0
         endif
      enddo



!-----Assign input path constituents to external flows for qual.
!-----An input path of the same object, object number, and group
!-----is considered to belong to the external flow.
!-----Trap multiple concentrations to same flow
      if (dsm2_module .eq. qual .and. check_headers) then
         do i=1,nqext
            n_conqext(i)=0
            do p=1,ninpaths
               if (pathinput(p).name .eq. qext(i).name) then
!-----------------matched external flow and this input path
                  constituent_found=.false.
                  do n=1,n_conqext(i)
!--------------------check for same constituent multiple times to same flow
!--------------------compare constituent type
                     if (pathinput(p).variable .eq. &
                         pathinput(const_qext(i,n)).variable) then ! same constituent
                        constituent_found=.true.
                        write(unit_error,815) &
                            trim(obj_names(qext(i).attach_obj_type)), &
                            trim(qext(i).attach_obj_name)
                        const_qext(i,n)=p
                     endif
                  enddo
                  if (.not. constituent_found) then
!--------------------first input path of this constituent to this flow
                     n_conqext(i)=n_conqext(i)+1
                     if (n_conqext(i) .gt. max_conqext) then
                        write(unit_error,825) max_conqext
                        call exit(2)
                     endif
                     const_qext(i,n_conqext(i))=p
                  endif
               endif
            enddo
         enddo
610      format(/'Incorrect ',a,' ',a,' in tidefile:'/a &
             /'tidefile was ',i4,', Qual uses ',i4)

        if (dim_res_tf .ne. maxnumres) then
            write(unit_error,610) 'reservoir', 'dimension', &
                filenm(:lnblnk(filenm)),dim_res_tf,maxnumres
            call exit(2)
        else if (dim_chan_tf .ne. nobr) then
            write(unit_error,610) 'channel', 'dimension', &
                filenm(:lnblnk(filenm)),dim_chan_tf,nobr
            call exit(2)
        else if (n_res_tf .ne. nreser) then
            write(unit_error,610) 'reservoir', 'number', &
                filenm(:lnblnk(filenm)),n_res_tf,nreser
            call exit(2)
        else if (n_chan_tf .ne. nbrch) then
            write(unit_error,610) 'channel', 'number', &
                filenm(:lnblnk(filenm)),n_chan_tf,nbrch
            call exit(2)
        endif
      endif                     ! qual module

      return

 900  continue                  ! error on opening tide file
      write(unit_error,920) filenm(:lnblnk(filenm))
 920  format(/'Could not open input tide file:' &
          /a)
      call exit(2)

end

end module hdf_tidefile
