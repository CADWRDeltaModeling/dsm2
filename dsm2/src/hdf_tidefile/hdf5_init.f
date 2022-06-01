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

	subroutine OpenHDF5()

	use HDF5		! HDF5 This module contains all necessary modules
	use hdfvars

	implicit none

	CHARACTER(LEN=8), PARAMETER :: filename = "z:\\test.h5" ! HDF5
      integer(HID_T) :: fapl
	integer(SIZE_T) :: rddc_nelmts
	integer(SIZE_T) :: rddc_nbytes
	integer  nelmts
	real rdcc_w0
	integer        :: error	! HDF5 Error flag

	inquire (file=hdf5_hydrofile, exist=h5_file_exists)

      call h5pcreate_f(H5P_FILE_ACCESS_F,fapl,error)
	call VerifyHDF5(error,"Property list creation")

	call h5pget_cache_f( fapl, nelmts, rddc_nelmts, rddc_nbytes, rdcc_w0,error)
	rddc_nbytes = 8000000
	call h5pset_cache_f( fapl, nelmts, rddc_nelmts, rddc_nbytes, rdcc_w0,error)
	call VerifyHDF5(error,"Cache set")

	call h5fopen_f(hdf5_hydrofile,H5F_ACC_RDONLY_F, file_id, error, fapl)
	call VerifyHDF5(error,"File open")

	call h5gopen_f(file_id, "hydro", hydro_id, error)
	call h5gopen_f(hydro_id, "geometry", geom_id, error)
	call h5gopen_f(hydro_id, "data", data_id, error)

	call h5dopen_f(data_id, "channel stage", chan_z_dset_id, error)
	call VerifyHDF5(error,"Channel stage dataset open")

	call h5dopen_f(data_id, "channel area", chan_a_dset_id, error)
	call VerifyHDF5(error,"Channel area dataset open")

	call h5dopen_f(data_id, "channel flow", chan_q_dset_id, error)
	call VerifyHDF5(error,"Channel flow dataset open")

	call h5dopen_f(data_id, "channel avg area", chan_aa_dset_id, error)
	call VerifyHDF5(error,"Channel avg area dataset open")

	call h5dopen_f(data_id, "reservoir height", res_h_dset_id, error)
	call VerifyHDF5(error,"Reservoir height dataset open")

	call h5dopen_f(data_id, "reservoir flow", res_q_dset_id, error)
	call VerifyHDF5(error,"Reservoir flow dataset open")

	call h5dopen_f(data_id, "qext flow", qext_change_dset_id, error)
	call VerifyHDF5(error,"Qext dataset open")

	call h5dopen_f(data_id, "transfer flow", transfer_dset_id, error)
	call VerifyHDF5(error,"Transfer flow dataset open")

      if (output_inst) then
	   call h5dopen_f(data_id, "inst flow", cp_q_dset_id, error)
	   call VerifyHDF5(error,"Computation point flow dataset open")

	   call h5dopen_f(data_id, "inst water surface", cp_z_dset_id, error)
	   call VerifyHDF5(error,"Computation point stage dataset open")

	   call h5dopen_f(data_id, "inst reservoir flow", inst_res_q_dset_id, error)
	   call VerifyHDF5(error,"Inst Reservoir flow dataset open")

	   call h5dopen_f(data_id, "inst qext flow", inst_qext_change_dset_id, error)
	   call VerifyHDF5(error,"Inst Qext dataset open")

    	   call h5dopen_f(data_id, "inst transfer flow", inst_transfer_dset_id, error)
 	   call VerifyHDF5(error,"Inst Transfer flow dataset open")
	end if

	return
	end subroutine

***********************************************************************
***********************************************************************
      !> Create a hydro tidefile.
      !> h5open must have been called by this point.
	subroutine InitHDF5File()

	use HDF5		! HDF5 This module contains all necessary modules
	use hdfvars
	use inclvars
	use IO_Units
	use logging
	use runtime_data

	implicit none

	integer(HID_T) :: access_plist ! Dataset trasfer property

	integer getHDF5IndexForTideTime
	external getHDF5IndexForTideTime
	external attach_hydro_dimscales

	integer(SIZE_T) :: rddc_nelmts
	integer(SIZE_T) :: rddc_nbytes
	integer  nelmts
	real rdcc_w0

	integer        :: error	! HDF5 Error flag


      ! check to see if file exists
	inquire (file=hdf5_hydrofile, exist=h5_file_exists)

	if (h5_file_exists) then ! file already exists
	   write(unit_error,920) trim(hdf5_hydrofile)
 920	   format(' File already exists... deleting existing file :: ', a )
	endif

				! set up stdio to allow for buffered read/write
	call H5Pcreate_f(H5P_FILE_ACCESS_F, access_plist, error)

	call h5pget_cache_f(access_plist, nelmts,
     &                    rddc_nelmts, rddc_nbytes, rdcc_w0,error)
	rddc_nbytes = 8000000
	call h5pset_cache_f(access_plist, nelmts, rddc_nelmts,
     &                    rddc_nbytes, rdcc_w0,error)
	call VerifyHDF5(error,"Cache set")

	! start up garbage collection
	!call h5pset_gc_references_f(access_plist_id,1,error)

	if (print_level .gt. 1) then
	   write(unit_screen,*)"Creating new HDF5 file"
	end if
	call h5fcreate_f(hdf5_hydrofile, H5F_ACC_TRUNC_F, file_id, error,
     &                H5P_DEFAULT_F, access_plist)



				! create group hydro
	call h5gcreate_f(file_id, "hydro", hydro_id, error)
	call h5gcreate_f(hydro_id, "geometry", geom_id, error)
	call h5gcreate_f(hydro_id, "data", data_id, error)

      !todo: does this create weird dependencies?
      call write_input_buffers_hdf5(hydro_id)

				! initialize attributes and datasets
	call hdf5_write_attributes()
	call InitHDF5MemoryDims
	call InitChannelsHDF5
	call InitReservoirsHDF5
	call InitTransferHDF5
	call InitQExtChangeHDF5
      call attach_hydro_dimscales(file_id);
      ! Calculate starting index for reading/writing
	h5_time_start = tf_start_julmin
	hdf5point = getHDF5IndexForTideTime(tf_start_julmin)


	return
	end subroutine


***********************************************************************
***********************************************************************

	subroutine CloseHDF5()

	use HDF5		! HDF5 This module contains all necessary modules
	use hdfvars
	use io_units, only: unit_error,unit_screen
      use logging
	implicit none

				! Called in fourpoint.f to close out the HDF5 file properly

	integer        :: error	! HDF5 Error flag

c-------Close the datasets corresponding to model states
      if (print_level .gt.2) write(unit_screen,*)"Closing HDF5 data sets"

	call h5dclose_f(chan_z_dset_id,error)
	if (error .ne. 0) then
	   write(unit_error,*)"HDF5 error closing channel stage data set: ",error
	end if

	call h5dclose_f(chan_aa_dset_id,error)
	if (error .ne. 0) then
	   write(unit_error,*)"HDF5 error closing channel avg area data set: ",error
	end if

	call h5dclose_f(chan_q_dset_id,error)
	if (error .ne. 0) then
	   write(unit_error,*)"HDF5 error closing channel flow data set: ",error
	end if

	call h5dclose_f(chan_a_dset_id,error)
	if (error .ne. 0) then
	   write(unit_error,*)"HDF5 error closing channel area data set: ",error
	end if


	call h5dclose_f(res_h_dset_id,error)
	call h5dclose_f(res_q_dset_id,error)
	if (error .ne. 0) then
	   write(unit_error,*)"HDF5 error closing reservoir data set: ",error
	end if

	call h5dclose_f(qext_change_dset_id,error)
	if (error .ne. 0) then
	   write(unit_error,*)"HDF5 error closing qext data set: ",error
	end if

	call h5dclose_f(transfer_dset_id,error)
	if (error .ne. 0) then
	   write(unit_error,*)"HDF5 error closing transfer data set: ",error
	end if

	if (output_inst) then

		 call h5dclose_f(cp_q_dset_id,error)
	   if (error .ne. 0) then
	      write(unit_error,*)"HDF5 error closing computation point flow data set: ",error
	   end if
		 call h5dclose_f(cp_z_dset_id,error)
	   if (error .ne. 0) then
	      write(unit_error,*)"HDF5 error closing computation point stage data set: ",error
	   end if

	   call h5dclose_f(inst_res_q_dset_id,error)
	   if (error .ne. 0) then
	      write(unit_error,*)"HDF5 error closing inst reservoir data set: ",error
	   end if

	   call h5dclose_f(inst_qext_change_dset_id,error)
	   if (error .ne. 0) then
	      write(unit_error,*)"HDF5 error closing inst qext data set: ",error
	   end if

	   call h5dclose_f(inst_transfer_dset_id,error)
  	   if (error .ne. 0) then
	       write(unit_error,*)"HDF5 error closing inst transfer data set: ",error
	   end if
	end if

c-------Close the groups in the dataset

      if (print_level .gt.2) write(unit_screen,*)"Closing HDF5 data groups"
	call h5gclose_f(geom_id, error)
	if (error .ne. 0) then
	   write(unit_error,*)"HDF5 error closing geometry group: ",error
	end if

	call h5gclose_f(data_id, error)
	if (error .ne. 0) then
	   write(unit_error,*)"HDF5 error closing data group: ",error
	end if

	call h5gclose_f(hydro_id, error)
	if (error .ne. 0) then
	   write(unit_error,*)"HDF5 error closing hydro group: ",error
	end if

c-------Close the file
 333  if (print_level .gt.1) write(unit_screen,*)"Closing HDF5 file"
	call h5fclose_f(file_id, error)
	if (error .ne. 0) then
	   write(unit_error,*)"HDF5 error closing hdf file: ",error
	end if

      if (print_level .gt.2) write(unit_screen,*)"Closing HDF5"
	call h5close_f(error)
	if (error .ne. 0) then
	   write(unit_error,*)"HDF5 error closing hdf5: ",error
	end if
      if (print_level .gt.2) write(unit_screen,*)"Closed HDF5"

	return
	end subroutine




***********************************************************************
***********************************************************************

	subroutine InitHDF5MemoryDims()
c-------Create the memory space describing dimensions in memory of objects
c-------involved in reading/writing time-varying model data

	use HDF5		! HDF5 This module contains all necessary modules
	use hdfvars
	use inclvars
	use grid_data
	use common_tide  ! todo: this is only to allocate qresv
	use chnlcomp, only: TotalCompLocations   ! to obtain the value for TotalComputations
	implicit none
	integer error
	integer getHDF5NumberOfTimeIntervals

       call alloc_reservoir_connections(.true.)
      !@todo: This is done everytime read_tide_head is called:
	!       1. This is twice (which is bad) and 2. No need to do this more than once
	!       Prefer we fix this with better program structure, not "if (firsttime)"

c-------Channel stage(io for channels is one variable at at time)
	chan_z_fdata_dims(1) = 2
	chan_z_fdata_dims(2) = nchans	! QChan:2,YChan:2,AChan:2,AChan_Avg:1
	chan_z_fdata_dims(3) = getHDF5NumberOfTimeIntervals()

	chan_z_fsubset_dims(1) = chan_z_fdata_dims(1) ! Number of channels
	chan_z_fsubset_dims(2) = chan_z_fdata_dims(2)
	chan_z_fsubset_dims(3) = 1 ! One step

	chan_z_mdata_dims(1) = 2
	chan_z_mdata_dims(2) = nchans

	call H5Screate_simple_f(chan_z_mdata_rank,
     &                        chan_z_mdata_dims,chan_z_memspace,error)

c-------Channel flow (io for channels is one variable at at time)
	chan_q_fdata_dims(1) = 2
	chan_q_fdata_dims(2) = nchans	! QChan:2,YChan:2,AChan:2,AChan_Avg:1
	chan_q_fdata_dims(3) = getHDF5NumberOfTimeIntervals()

	chan_q_fsubset_dims(1) = chan_q_fdata_dims(1) ! Number of channels
	chan_q_fsubset_dims(2) = chan_q_fdata_dims(2) ! one variable
	chan_q_fsubset_dims(3) = 1 ! One step

	chan_q_mdata_dims(1) = 2
	chan_q_mdata_dims(2) = nchans

	call H5Screate_simple_f(chan_q_mdata_rank,
     &                        chan_q_mdata_dims,chan_q_memspace,error)

c-------Channel area
	chan_a_fdata_dims(1) = 2
	chan_a_fdata_dims(2) = nchans
	chan_a_fdata_dims(3) = getHDF5NumberOfTimeIntervals()

	chan_a_fsubset_dims(1) = chan_a_fdata_dims(1) ! Number of channels
	chan_a_fsubset_dims(2) = chan_a_fdata_dims(2)
	chan_a_fsubset_dims(3) = 1 ! One step

	chan_a_mdata_dims(1) = chan_a_fdata_dims(1)
	chan_a_mdata_dims(2) = chan_a_fdata_dims(2)

	call H5Screate_simple_f(chan_a_mdata_rank,
     &                        chan_a_mdata_dims,chan_a_memspace,error)


c-------Channel avg area(io for channels is one variable at at time)
	chan_aa_fdata_dims(1) = nchans
	chan_aa_fdata_dims(2) = getHDF5NumberOfTimeIntervals()

	chan_aa_fsubset_dims(1) = chan_aa_fdata_dims(1) ! Number of channels
	chan_aa_fsubset_dims(2) = 1 ! One step

	chan_aa_mdata_dims(1) = nchans
	call H5Screate_simple_f(chan_aa_mdata_rank,
     &                        chan_aa_mdata_dims,chan_aa_memspace,error);

c-------Reservoir

	res_h_fdata_dims(1) = max(1,nreser)
	res_h_fdata_dims(2) = getHDF5NumberOfTimeIntervals()

      ! todo: gotta be a better place to do this
	res_q_fdata_dims(1) = max(1,nres_connect)
	res_q_fdata_dims(2) = getHDF5NumberOfTimeIntervals()

	res_h_fsubset_dims(1) = res_h_fdata_dims(1)
	res_h_fsubset_dims(2) = 1

	res_q_fsubset_dims(1) = res_q_fdata_dims(1)
	res_q_fsubset_dims(2) = 1


	res_h_mdata_dims(1) = max(1,NReser)
	call H5Screate_simple_f(res_h_mdata_rank,
     &                        res_h_mdata_dims,res_h_memspace,error);

	res_q_mdata_dims(1) = max(1,nres_connect)
	call H5Screate_simple_f(res_q_mdata_rank,res_q_mdata_dims,res_q_memspace,error);

c-------Qext

	qext_fdata_dims(1) = max(1,nqext)
	qext_fdata_dims(2) = getHDF5NumberOfTimeIntervals()
	qext_fsubset_dims(1) = qext_fdata_dims(1)
	qext_fsubset_dims(2) = 1

	qext_mdata_dims(1) = qext_fdata_dims(1)
	call H5Screate_simple_f(qext_mdata_rank,qext_mdata_dims,qext_memspace,error);
      call VerifyHDF5(error,"qext dataspace")
c-------Transfer (obj2obj)
	transfer_fdata_dims(1) = max(1,nobj2obj) !nqext
	transfer_fdata_dims(2) = getHDF5NumberOfTimeIntervals()

	transfer_fsubset_dims(1) = transfer_fdata_dims(1)
	transfer_fsubset_dims(2) = 1

	transfer_mdata_dims(1) = transfer_fdata_dims(1)
	call H5Screate_simple_f(transfer_mdata_rank,
     &                        transfer_mdata_dims,transfer_memspace,error);
      call VerifyHDF5(error,"transfer dataspace")


      if (output_inst) then
c-------Computation point water surface
	   cp_z_fdata_dims(1) = TotalCompLocations
	   cp_z_fdata_dims(2) = getHDF5NumberOfTimeIntervals()

	   cp_z_fsubset_dims(1) = cp_z_fdata_dims(1) ! Number of computation points
	   cp_z_fsubset_dims(2) = 1 ! One step

	   cp_z_mdata_dims(1) = TotalCompLocations
	   call H5Screate_simple_f(cp_z_mdata_rank,
     &                        cp_z_mdata_dims,cp_z_memspace,error);

c-------Computation point flow
	   cp_q_fdata_dims(1) = TotalCompLocations
	   cp_q_fdata_dims(2) = getHDF5NumberOfTimeIntervals()

	   cp_q_fsubset_dims(1) = cp_q_fdata_dims(1) ! Number of computation points
	   cp_q_fsubset_dims(2) = 1 ! One step

	   cp_q_mdata_dims(1) = TotalCompLocations
	   call H5Screate_simple_f(cp_q_mdata_rank,
     &                        cp_q_mdata_dims,cp_q_memspace,error);

c-------Instantaneous Reservoir

         ! todo: gotta be a better place to do this
	   inst_res_q_fdata_dims(1) = max(1,nres_connect)
   	   inst_res_q_fdata_dims(2) = getHDF5NumberOfTimeIntervals()

	   inst_res_q_fsubset_dims(1) = inst_res_q_fdata_dims(1)
	   inst_res_q_fsubset_dims(2) = 1

	   inst_res_q_mdata_dims(1) = max(1,nres_connect)
	   call H5Screate_simple_f(inst_res_q_mdata_rank,inst_res_q_mdata_dims,inst_res_q_memspace,error);
         call VerifyHDF5(error,"inst reservoir dataspace")
c-------Instantaneous Qext

	   inst_qext_fdata_dims(1) = max(1,nqext)
	   inst_qext_fdata_dims(2) = getHDF5NumberOfTimeIntervals()
	   inst_qext_fsubset_dims(1) = inst_qext_fdata_dims(1)
	   inst_qext_fsubset_dims(2) = 1

	   inst_qext_mdata_dims(1) = inst_qext_fdata_dims(1)
	   call H5Screate_simple_f(inst_qext_mdata_rank,inst_qext_mdata_dims,inst_qext_memspace,error);
         call VerifyHDF5(error,"inst qext dataspace")
c-------Instantaneous Transfer (obj2obj)
	   inst_transfer_fdata_dims(1) = max(1,nobj2obj) !nqext
	   inst_transfer_fdata_dims(2) = getHDF5NumberOfTimeIntervals()

	   inst_transfer_fsubset_dims(1) = inst_transfer_fdata_dims(1)
	   inst_transfer_fsubset_dims(2) = 1

	   inst_transfer_mdata_dims(1) = inst_transfer_fdata_dims(1)
	   call H5Screate_simple_f(inst_transfer_mdata_rank,
     &                           inst_transfer_mdata_dims,inst_transfer_memspace,error);
         call VerifyHDF5(error,"inst transfer dataspace")

      end if

	return
	end subroutine

***********************************************************************
***********************************************************************

	subroutine CloseMemoryDims()
c-------Close the memory space describing dimensions in memory of objects
c-------involved in reading/writing time-varying model data

	use HDF5		! HDF5 This module contains all necessary modules
	use hdfvars
	implicit none
	integer error
      call alloc_reservoir_connections(.false.)
	call H5Sclose_f(chan_z_memspace, error)
	call H5Sclose_f(chan_q_memspace, error)
	call H5Sclose_f(chan_aa_memspace, error)
	call H5Sclose_f(res_h_memspace, error)
	call H5Sclose_f(res_q_memspace, error)
	call H5Sclose_f(qext_memspace, error)
	call H5Sclose_f(transfer_memspace, error)
      if (output_inst) then
	   call H5Sclose_f(cp_q_memspace, error)
	   call H5Sclose_f(cp_z_memspace, error)
	   call H5Sclose_f(inst_res_q_memspace, error)
	   call H5Sclose_f(inst_qext_memspace, error)
	   call H5Sclose_f(inst_transfer_memspace, error)
      endif
      call alloc_reservoir_connections(.false.)

	return
	end subroutine



***********************************************************************
***********************************************************************

	subroutine AddTimeSeriesAttributes(dset_id, ts_start, ts_interval)

	use HDF5
	use hdfvars
	use inclvars
	use runtime_data
	use iopath_data
	use utilities, only: jmin2iso
      implicit none
      integer :: ts_start
      integer :: ts_interval
      character*16 :: cinterval
	integer(HID_T) :: dset_id ! Attribute identifier
	integer(HID_T) :: aspace_id ! Attribute Dataspace identifier
	integer(HID_T) :: atype_id
	integer(HID_T) :: attr_id
	integer        :: error	! HDF5 Error flag
	integer nlen

      character(LEN=32) :: buffer
	integer(HSIZE_T), dimension(1) :: a_dims = (/1/)
      integer     ::   arank = 1                      ! Attribure rank

	integer*4, parameter :: ISO_LEN = 19  ! includes null termination
	character(LEN=ISO_LEN) :: iso_datetime


      write(cinterval,"(i,'min')")ts_interval
      cinterval=adjustl(cinterval)

      call h5screate_simple_f(arank, a_dims, aspace_id, error)
      call VerifyHDF5(error,"time series attributes dataspace")
      call h5tcopy_f(H5T_NATIVE_CHARACTER, atype_id, error)
	buffer = 'TIMESERIES'
	nlen = len_trim(buffer)
      call h5tset_size_f(atype_id, nlen, error)
	call h5acreate_f(dset_id, "CLASS",
     &    atype_id, aspace_id, attr_id, error)
	call h5awrite_f(attr_id, atype_id, trim(buffer),
     &                a_dims, error)
	call h5aclose_f(attr_id,error)

	iso_datetime=jmin2iso(ts_start)
      call h5tset_size_f(atype_id, ISO_LEN, error)
	call h5acreate_f(dset_id, "start_time",
     &    atype_id, aspace_id, attr_id, error)
 	call h5awrite_f(attr_id, atype_id, iso_datetime(1:19),
     &     a_dims, error)
	call h5aclose_f(attr_id,error)

      nlen=len_trim(cinterval)
      call h5tset_size_f(atype_id, nlen, error)
	call h5acreate_f(dset_id, "interval",
     &    atype_id, aspace_id, attr_id, error)
	call h5awrite_f(attr_id, atype_id,
     &    cinterval,
     &    a_dims, error)
	call h5aclose_f(attr_id,error)

	nlen = 5
      call h5tset_size_f(atype_id, nlen, error)
	call h5acreate_f(dset_id, "model",
     &    atype_id, aspace_id, attr_id, error)
	call h5awrite_f(attr_id, atype_id, dsm2_name
     &    ,a_dims, error)
	call h5aclose_f(attr_id,error)

	nlen = 3
      call h5tset_size_f(atype_id, nlen, error)
	call h5acreate_f(dset_id, "model_version",
     &    atype_id, aspace_id, attr_id, error)
	call h5awrite_f(attr_id, atype_id, dsm2_version
     &    ,a_dims, error)
	call h5aclose_f(attr_id,error)

      call h5tclose_f(atype_id,error)
	call h5sclose_f(aspace_id,error)


      return
	end subroutine






***********************************************************************
***********************************************************************

	subroutine InitChannelsHDF5()

	use HDF5		! HDF5 This module contains all necessary modules
	use hdfvars
	use inclvars
	use runtime_data
	use common_tide

	implicit none

	integer(HSIZE_T), dimension(3) :: chan_z_chunk_dims = 0 ! Dataset dimensions
	integer(HSIZE_T), dimension(3) :: chan_q_chunk_dims = 0 ! Dataset dimensions
	integer(HSIZE_T), dimension(3) :: chan_a_chunk_dims = 0 ! Dataset dimensions
	integer(HSIZE_T), dimension(2) :: chan_aa_chunk_dims = 0 ! Dataset dimensions
	integer(HSIZE_T), dimension(3) :: cp_z_chunk_dims = 0   ! Dataset dimensions
	integer(HSIZE_T), dimension(3) :: cp_q_chunk_dims = 0   ! Dataset dimensions
	integer(HID_T) :: cparms !dataset creatation property identifier
	integer        :: error	! HDF5 Error flag

	integer(HID_T) :: attr_id ! Attribute identifier
	integer(HID_T) :: aspace_id ! Attribute Dataspace identifier
	integer(HID_T) :: atype_id ! Attribute Dataspace identifier
	integer(HSIZE_T), dimension(1) :: adims = (/1/) ! Attribute dimension
	integer     ::   arank = 1 ! Attribure rank
	integer(HSIZE_T), dimension(7) :: a_data_dims

	integer getHDF5NumberOfTimeIntervals
	character*14,external :: jmin2cdt

	call h5screate_simple_f(arank, adims, aspace_id, error)
	call h5tcopy_f(H5T_NATIVE_INTEGER, atype_id, error)

	call h5acreate_f(data_id, "Start Time",
	1    atype_id, aspace_id, attr_id, error)
	call h5awrite_f(attr_id, atype_id, tf_start_julmin, a_data_dims, error)
	call h5acreate_f(data_id, "Time Interval",
	1    atype_id, aspace_id, attr_id, error)
	call h5awrite_f(attr_id, atype_id, TideFileWriteInterval, a_data_dims, error)
	call h5aclose_f(attr_id,error)


	chan_z_chunk_dims(1) = chan_z_fdata_dims(1)
	chan_z_chunk_dims(2) = chan_z_fdata_dims(2)
	chan_z_chunk_dims(3) = min(TIME_CHUNK,chan_z_fdata_dims(3))

		! Create channel stage data set
		! Add chunking and compression
      cparms=0
	call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, error)
	if (getHDF5NumberOfTimeIntervals() .gt. MIN_STEPS_FOR_CHUNKING) then
	  call h5pset_chunk_f(cparms, chan_z_fdata_rank, chan_z_chunk_dims, error)
	  call H5Pset_szip_f (cparms, H5_SZIP_NN_OM_F,
     &                    HDF_SZIP_PIXELS_PER_BLOCK, error);

      end if


	call h5screate_simple_f(chan_z_fdata_rank,
     &                        chan_z_fdata_dims, chan_z_fspace_id, error)
	call h5dcreate_f(data_id, "channel stage", H5T_NATIVE_REAL,
	1    chan_z_fspace_id, chan_z_dset_id, error, cparms)
	call VerifyHDF5(error,"Channel stage dataset creation")
      call AddTimeSeriesAttributes(chan_z_dset_id,tf_start_julmin,TideFileWriteInterval)

		! Create channel flow data set
	chan_q_chunk_dims(1) = chan_q_fdata_dims(1)
	chan_q_chunk_dims(2) = chan_q_fdata_dims(2)
	chan_q_chunk_dims(3) = min(TIME_CHUNK,chan_q_fdata_dims(3))
	cparms=0
		! Add chunking and compression
	call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, error)
	if (getHDF5NumberOfTimeIntervals() .gt. MIN_STEPS_FOR_CHUNKING) then
	   call h5pset_chunk_f(cparms,chan_q_fdata_rank,chan_q_chunk_dims, error)
	   call H5Pset_szip_f (cparms, H5_SZIP_NN_OM_F,
     &                    HDF_SZIP_PIXELS_PER_BLOCK, error);
	end if
	call h5screate_simple_f(chan_q_fdata_rank, chan_q_fdata_dims,
     &                        chan_q_fspace_id, error)
	call h5dcreate_f(data_id, "channel flow", H5T_NATIVE_REAL,
     &    chan_q_fspace_id, chan_q_dset_id, error, cparms)
	call VerifyHDF5(error,"Channel flow dataset creation")
      call AddTimeSeriesAttributes(chan_q_dset_id,tf_start_julmin,TideFileWriteInterval)


		! Create channel area data set
	chan_a_chunk_dims(1) = chan_a_fdata_dims(1)
	chan_a_chunk_dims(2) = chan_a_fdata_dims(2)
	chan_a_chunk_dims(3) = min(TIME_CHUNK,chan_a_fdata_dims(3))
      cparms=0
		! Add chunking and compression
	call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, error)
	if (getHDF5NumberOfTimeIntervals() .gt. MIN_STEPS_FOR_CHUNKING) then
          call h5pset_chunk_f(cparms, chan_a_fdata_rank, chan_a_chunk_dims, error)
	    call H5Pset_szip_f (cparms, H5_SZIP_NN_OM_F,
     &                    HDF_SZIP_PIXELS_PER_BLOCK, error);

	end if

	call h5screate_simple_f(chan_a_fdata_rank, chan_a_fdata_dims,
     &                        chan_a_fspace_id, error)
	call h5dcreate_f(data_id, "channel area", H5T_NATIVE_REAL,
     &    chan_a_fspace_id, chan_a_dset_id, error, cparms)
	call VerifyHDF5(error,"Channel area dataset creation")
      call AddTimeSeriesAttributes(chan_a_dset_id,tf_start_julmin,TideFileWriteInterval)


		! Create channel avg area data set
	chan_aa_chunk_dims(1) = chan_aa_fdata_dims(1)
	chan_aa_chunk_dims(2) = min(TIME_CHUNK,chan_aa_fdata_dims(2))
	cparms=0
			! Add chunking and compression
	call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, error)
      if (getHDF5NumberOfTimeIntervals() .gt. MIN_STEPS_FOR_CHUNKING) then
          call h5pset_chunk_f(cparms, chan_aa_fdata_rank,
     &                        chan_aa_chunk_dims, error)
	    call H5Pset_szip_f (cparms, H5_SZIP_NN_OM_F,
     &                    HDF_SZIP_PIXELS_PER_BLOCK, error);
	end if

	call h5screate_simple_f(chan_aa_fdata_rank, chan_aa_fdata_dims,
     &                        chan_aa_fspace_id, error)
	call h5dcreate_f(data_id, "channel avg area", H5T_NATIVE_REAL,
     &	    chan_aa_fspace_id, chan_aa_dset_id, error, cparms)
	call VerifyHDF5(error,"Channel avg area dataset creation")
      call AddTimeSeriesAttributes(chan_aa_dset_id,tf_start_julmin,TideFileWriteInterval)
      if (output_inst) then
	 ! Create computation point stage data set
	  cp_z_chunk_dims(1) = cp_z_fdata_dims(1)
	  cp_z_chunk_dims(2) = min(TIME_CHUNK,cp_z_fdata_dims(2))
	  cparms=0
		  	  ! Add chunking and compression
	  call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, error)
        if (getHDF5NumberOfTimeIntervals() .gt. MIN_STEPS_FOR_CHUNKING) then
            call h5pset_chunk_f(cparms, cp_z_fdata_rank,
     &                          cp_z_chunk_dims, error)
	      call H5Pset_szip_f (cparms, H5_SZIP_NN_OM_F,
     &                    HDF_SZIP_PIXELS_PER_BLOCK, error);
	  end if

	  call h5screate_simple_f(cp_z_fdata_rank, cp_z_fdata_dims,
     &                          cp_z_fspace_id, error)
	  call h5dcreate_f(data_id, "inst water surface", H5T_NATIVE_REAL,
     &	      cp_z_fspace_id, cp_z_dset_id, error, cparms)
	  call VerifyHDF5(error,"Computation point stage dataset creation")
        call AddTimeSeriesAttributes(cp_z_dset_id,tf_start_julmin,TideFileWriteInterval)

		! Create computation point flow data set
	  cp_q_chunk_dims(1) = cp_q_fdata_dims(1)
	  cp_q_chunk_dims(2) = min(TIME_CHUNK,cp_q_fdata_dims(2))
	  cparms=0
			  ! Add chunking and compression
	  call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, error)
        if (getHDF5NumberOfTimeIntervals() .gt. MIN_STEPS_FOR_CHUNKING) then
            call h5pset_chunk_f(cparms, cp_q_fdata_rank,
     &                          cp_q_chunk_dims, error)
	      call H5Pset_szip_f (cparms, H5_SZIP_NN_OM_F,
     &                    HDF_SZIP_PIXELS_PER_BLOCK, error);
	  end if

	  call h5screate_simple_f(cp_q_fdata_rank, cp_q_fdata_dims,
     &                          cp_q_fspace_id, error)
	  call h5dcreate_f(data_id, "inst flow", H5T_NATIVE_REAL,
     &	      cp_q_fspace_id, cp_q_dset_id, error, cparms)
	  call VerifyHDF5(error,"Computation point flow dataset creation")
        call AddTimeSeriesAttributes(cp_q_dset_id,tf_start_julmin,TideFileWriteInterval)

      end if

	return
	end subroutine

	subroutine InitReservoirsHDF5()

	use HDF5		! HDF5 This module contains all necessary modules
	use hdfvars
	use inclvars
      use runtime_data
      use common_tide
	implicit none

	integer(HID_T) :: cparms !dataset creation property identifier
	integer        :: error	! HDF5 Error flag
	integer(HSIZE_T), dimension(res_q_fdata_rank) ::
     &	res_q_chunk_dims = 0 ! Dataset dimensions
	integer(HSIZE_T), dimension(res_h_fdata_rank) ::
     &    res_h_chunk_dims = 0 ! Dataset dimensions
	integer(HSIZE_T), dimension(inst_res_q_fdata_rank) ::
     &	inst_res_q_chunk_dims = 0 ! Dataset dimensions
	integer getHDF5NumberOfTimeIntervals

c-------Create the datasets

	res_h_chunk_dims(1) = res_h_fdata_dims(1)
	res_h_chunk_dims(2) = min(TIME_CHUNK,res_h_fdata_dims(2))


		! Add chunking and compression
	call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, error)
	if (getHDF5NumberOfTimeIntervals() .gt. MIN_STEPS_FOR_CHUNKING) then
	   call h5pset_chunk_f(cparms, res_h_fdata_rank, res_h_chunk_dims, error)
	   call H5Pset_szip_f (cparms, H5_SZIP_NN_OM_F,
     &                    HDF_SZIP_PIXELS_PER_BLOCK, error);
      end if

	call h5screate_simple_f(res_h_fdata_rank, res_h_fdata_dims, res_h_fspace_id, error)
	call h5dcreate_f(data_id, "reservoir height", H5T_NATIVE_REAL,
     &    res_h_fspace_id, res_h_dset_id, error, cparms)
      call AddTimeSeriesAttributes(res_h_dset_id,tf_start_julmin,TideFileWriteInterval)



	res_q_chunk_dims(1) = res_q_fdata_dims(1)
	res_q_chunk_dims(2) = min(TIME_CHUNK,res_q_fdata_dims(2))


				! Add chunking and compression
	call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, error)
	if (getHDF5NumberOfTimeIntervals() .gt. MIN_STEPS_FOR_CHUNKING) then
	    call h5pset_chunk_f(cparms, res_q_fdata_rank, res_q_chunk_dims, error)
	    call H5Pset_szip_f (cparms, H5_SZIP_NN_OM_F,
     &                    HDF_SZIP_PIXELS_PER_BLOCK, error);
	end if


	call h5screate_simple_f(res_q_fdata_rank, res_q_fdata_dims, res_q_fspace_id, error)
	call h5dcreate_f(data_id, "reservoir flow", H5T_NATIVE_REAL,
     &	    res_q_fspace_id, res_q_dset_id, error, cparms)
      call AddTimeSeriesAttributes(res_q_dset_id,tf_start_julmin,TideFileWriteInterval)


      if (output_inst) then
        ! instantaneous
	  inst_res_q_chunk_dims(1) = inst_res_q_fdata_dims(1)
  	  inst_res_q_chunk_dims(2) = min(TIME_CHUNK,inst_res_q_fdata_dims(2))

				! Add chunking and compression
	  call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, error)
	  if (getHDF5NumberOfTimeIntervals() .gt. MIN_STEPS_FOR_CHUNKING) then
	      call h5pset_chunk_f(cparms, inst_res_q_fdata_rank, inst_res_q_chunk_dims, error)
	      call H5Pset_szip_f (cparms, H5_SZIP_NN_OM_F,
     &                    HDF_SZIP_PIXELS_PER_BLOCK, error);
	  end if

	  call h5screate_simple_f(inst_res_q_fdata_rank, inst_res_q_fdata_dims, inst_res_q_fspace_id, error)
	  call h5dcreate_f(data_id, "inst reservoir flow", H5T_NATIVE_REAL,
     &  	    inst_res_q_fspace_id, inst_res_q_dset_id, error, cparms)
        call AddTimeSeriesAttributes(inst_res_q_dset_id,tf_start_julmin,TideFileWriteInterval)
      end if

	return
	end subroutine

***********************************************************************
***********************************************************************

	subroutine InitTransferHDF5()

	use HDF5		! HDF5 This module contains all necessary modules
	use hdfvars
	use inclvars
	use grid_data
      use runtime_data
      use common_tide

	implicit none

	integer        :: error	! HDF5 Error flag
	integer(HID_T) :: cparms !dataset creatation property identifier
	integer(HID_T) :: attr_id ! Attribute identifier
	integer(HID_T) :: aspace_id ! Attribute Dataspace identifier
	integer(HID_T) :: atype_id ! Attribute Dataspace identifier
	integer(HSIZE_T), dimension(1) :: adims = (/1/) ! Attribute dimension
	integer     ::   arank = 1 ! Attribure rank
	integer(HSIZE_T), dimension(7) :: a_data_dims
	integer(HSIZE_T), dimension(transfer_fdata_rank) ::
     &    transfer_chunk_dims = 0 ! Dataset dimensions
	integer(HSIZE_T), dimension(inst_transfer_fdata_rank) ::
     &    inst_transfer_chunk_dims = 0 ! Dataset dimensions
	integer getHDF5NumberOfTimeIntervals

	transfer_chunk_dims(1) = transfer_fdata_dims(1)
	transfer_chunk_dims(2) = min(TIME_CHUNK,transfer_fdata_dims(2))

      cparms = 0
				! Add chunking and compression
	call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, error)

	if (getHDF5NumberOfTimeIntervals() .gt. MIN_STEPS_FOR_CHUNKING) then
	   call h5pset_chunk_f(cparms, transfer_fdata_rank, transfer_chunk_dims, error)
	   call H5Pset_szip_f (cparms, H5_SZIP_NN_OM_F,
     &                    HDF_SZIP_PIXELS_PER_BLOCK, error);
      end if


	call h5screate_simple_f(transfer_fdata_rank, transfer_fdata_dims,
     &       transfer_fspace_id, error)
      call VerifyHDF5(error,"transfer dataspace")
	call h5dcreate_f(data_id, "transfer flow", H5T_NATIVE_REAL,
     &       transfer_fspace_id, transfer_dset_id, error, cparms)
	call VerifyHDF5(error,"Flow transfer dataset creation")
      call AddTimeSeriesAttributes(transfer_dset_id,tf_start_julmin,TideFileWriteInterval)

	call h5screate_simple_f(arank, adims, aspace_id, error)
	call h5tcopy_f(H5T_NATIVE_INTEGER, atype_id, error)
	call h5acreate_f(hydro_id, "Number of flow transfers",
	1    atype_id, aspace_id, attr_id, error)
	call h5awrite_f(attr_id, atype_id, nobj2obj, a_data_dims, error)
	call h5acreate_f(hydro_id, "Max Number of flow transfers",
	1    atype_id, aspace_id, attr_id, error)
	call h5awrite_f(attr_id, atype_id, max_obj2obj, a_data_dims, error)
	call h5aclose_f(attr_id,error)

	if (output_inst) then
	  ! instantaneous
	  inst_transfer_chunk_dims(1) = inst_transfer_fdata_dims(1)
	  inst_transfer_chunk_dims(2) = min(TIME_CHUNK,inst_transfer_fdata_dims(2))

        cparms = 0
				! Add chunking and compression
	  call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, error)

	  if (getHDF5NumberOfTimeIntervals() .gt. MIN_STEPS_FOR_CHUNKING) then
	     call h5pset_chunk_f(cparms, inst_transfer_fdata_rank, inst_transfer_chunk_dims, error)
	     call H5Pset_szip_f (cparms, H5_SZIP_NN_OM_F,
     &                    HDF_SZIP_PIXELS_PER_BLOCK, error);
        end if

	  call h5screate_simple_f(inst_transfer_fdata_rank, inst_transfer_fdata_dims,
     &       inst_transfer_fspace_id, error)
        call VerifyHDF5(error,"inst transfer dataspace")
	  call h5dcreate_f(data_id, "inst transfer flow", H5T_NATIVE_REAL,
     &       inst_transfer_fspace_id, inst_transfer_dset_id, error, cparms)
	  call VerifyHDF5(error,"Flow transfer dataset creation")
        call AddTimeSeriesAttributes(inst_transfer_dset_id,tf_start_julmin,TideFileWriteInterval)
      end if

	return
	end subroutine

***********************************************************************
***********************************************************************

	subroutine InitQExtChangeHDF5()

	use HDF5		! HDF5 This module contains all necessary modules
	use hdfvars
	use inclvars
	use grid_data
      use runtime_data
      use common_tide
	implicit none

	integer(HID_T) :: cparms !dataset creatation property identifier
	integer        :: error	! HDF5 Error flag

	integer(HID_T) :: attr_id ! Attribute identifier
	integer(HID_T) :: aspace_id ! Attribute Dataspace identifier
	integer(HID_T) :: atype_id ! Attribute Dataspace identifier
	integer(HSIZE_T), dimension(1) :: adims = (/1/) ! Attribute dimension
	integer     ::   arank = 1 ! Attribure rank
	integer(HSIZE_T), dimension(7) :: a_data_dims

	integer getHDF5NumberOfTimeIntervals
	integer(HSIZE_T), dimension(qext_fdata_rank) ::
     &    qext_chunk_dims = 0 ! Dataset dimensions
	integer(HSIZE_T), dimension(inst_qext_fdata_rank) ::
     &    inst_qext_chunk_dims = 0 ! Dataset dimensions

	qext_chunk_dims(1) = qext_fdata_dims(1)
	qext_chunk_dims(2) = min(TIME_CHUNK,qext_fdata_dims(2))

				! Add chunking and compression
	call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, error)
	if (getHDF5NumberOfTimeIntervals() .gt. MIN_STEPS_FOR_CHUNKING) then
	   call h5pset_chunk_f(cparms, qext_fdata_rank, qext_chunk_dims, error)
	   call H5Pset_szip_f (cparms, H5_SZIP_NN_OM_F,
     &                    HDF_SZIP_PIXELS_PER_BLOCK, error);
      end if


	call h5screate_simple_f(qext_fdata_rank, qext_fdata_dims, qext_fspace_id, error)
	call h5dcreate_f(data_id, "qext flow", H5T_NATIVE_REAL,
	1    qext_fspace_id, qext_change_dset_id, error, cparms)
      call AddTimeSeriesAttributes(qext_change_dset_id,tf_start_julmin,TideFileWriteInterval)

	call h5screate_simple_f(arank, adims, aspace_id, error)
	call h5tcopy_f(H5T_NATIVE_INTEGER, atype_id, error)
	call h5acreate_f(hydro_id, "Number of QExt",
	1    atype_id, aspace_id, attr_id, error)
	call h5awrite_f(attr_id, atype_id, nqext, a_data_dims, error)
	call h5acreate_f(hydro_id, "Max Number of QExt",
	1    atype_id, aspace_id, attr_id, error)
	call h5awrite_f(attr_id, atype_id, max_qext, a_data_dims, error)
	call h5aclose_f(attr_id,error)

	if (output_inst) then
	  ! instantaneous
	  inst_qext_chunk_dims(1) = inst_qext_fdata_dims(1)
	  inst_qext_chunk_dims(2) = min(TIME_CHUNK,inst_qext_fdata_dims(2))

				! Add chunking and compression
	  call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, error)
	  if (getHDF5NumberOfTimeIntervals() .gt. MIN_STEPS_FOR_CHUNKING) then
	     call h5pset_chunk_f(cparms, inst_qext_fdata_rank, inst_qext_chunk_dims, error)
	     call H5Pset_szip_f (cparms, H5_SZIP_NN_OM_F,
     &                    HDF_SZIP_PIXELS_PER_BLOCK, error);
        end if

	  call h5screate_simple_f(inst_qext_fdata_rank, inst_qext_fdata_dims, inst_qext_fspace_id, error)
	  call h5dcreate_f(data_id, "inst qext flow", H5T_NATIVE_REAL,
     &       inst_qext_fspace_id, inst_qext_change_dset_id, error, cparms)
        call AddTimeSeriesAttributes(inst_qext_change_dset_id,tf_start_julmin,TideFileWriteInterval)
      end if

	return
	end subroutine

***********************************************************************
***********************************************************************
