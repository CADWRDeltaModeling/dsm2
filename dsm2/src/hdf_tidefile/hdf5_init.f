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

	call h5open_f (error)
	inquire (file=hdf5_hydrofile, exist=h5_file_exists)

      call h5pcreate_f(H5P_FILE_ACCESS_F,fapl,error)
	call VerifyHDF5(error,"Property list creation")

	call h5pget_cache_f( fapl, nelmts, rddc_nelmts, rddc_nbytes, rdcc_w0,error)
	rddc_nbytes = 8000000
	call h5pset_cache_f( fapl, nelmts, rddc_nelmts, rddc_nbytes, rdcc_w0,error)
	call VerifyHDF5(error,"Cache set")

	call h5fopen_f(hdf5_hydrofile,H5F_ACC_RDONLY_F, file_id, fapl, error)
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

	call h5dopen_f(data_id, "qext changed", qext_change_dset_id, error)
	call VerifyHDF5(error,"Qext dataset open")

	call h5dopen_f(data_id, "transfer flow", transfer_dset_id, error)
	call VerifyHDF5(error,"Transfer flow dataset open")	


	return
	end subroutine

***********************************************************************
***********************************************************************

	subroutine InitHDF5File()

	use HDF5		! HDF5 This module contains all necessary modules
	use hdfvars
	use inclvars
	use IO_Units
	use common_tide
	use runtime_data

	implicit none

	integer(HID_T) :: access_plist ! Dataset trasfer property

	integer getHDF5IndexForTideTime
	external getHDF5IndexForTideTime


	integer(SIZE_T) :: rddc_nelmts
	integer(SIZE_T) :: rddc_nbytes
	integer  nelmts
	real rdcc_w0

	integer        :: error	! HDF5 Error flag

				! initialize HDF5 library
	call h5open_f (error)

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

	print*, "Creating new HDF5 file"
	call h5fcreate_f(hdf5_hydrofile, H5F_ACC_TRUNC_F, file_id, error,
     &                H5P_DEFAULT_F, access_plist)

				! create group hydro
	call h5gcreate_f(file_id, "hydro", hydro_id, error)
	call h5gcreate_f(hydro_id, "geometry", geom_id, error)
	call h5gcreate_f(hydro_id, "data", data_id, error)

				! initialize attributes and datasets
	call WriteAttributesToHDF5()
	call InitHDF5MemoryDims()
	call InitChannelsHDF5()
	call InitReservoirsHDF5()
	call InitTransferHDF5()
	call InitQExtChangeHDF5()
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

	implicit none

				! Called in fourpoint.f to close out the HDF5 file properly

	integer        :: error	! HDF5 Error flag

c-------Close the datasets corresponding to model states

      write(unit_screen,*)"Closing HDF5 data sets"

	call h5dclose_f(chan_z_dset_id,error)
	if (error .ne. 0)then
	   write(unit_error,*)"HDF5 error closing channel stage data set: ",error
	end if

	call h5dclose_f(chan_aa_dset_id,error)
	if (error .ne. 0)then
	   write(unit_error,*)"HDF5 error closing channel avg area data set: ",error
	end if

	call h5dclose_f(chan_q_dset_id,error)
	if (error .ne. 0)then
	   write(unit_error,*)"HDF5 error closing channel flow data set: ",error
	end if

	call h5dclose_f(chan_a_dset_id,error)
	if (error .ne. 0)then
	   write(unit_error,*)"HDF5 error closing channel area data set: ",error
	end if


	call h5dclose_f(res_h_dset_id,error)
	call h5dclose_f(res_q_dset_id,error)
	if (error .ne. 0)then
	   write(unit_error,*)"HDF5 error closing reservoir data set: ",error
	end if

	call h5dclose_f(qext_change_dset_id,error)
	if (error .ne. 0)then
	   write(unit_error,*)"HDF5 error closing qext data set: ",error
	end if

	call h5dclose_f(transfer_dset_id,error)
	if (error .ne. 0)then
	   write(unit_error,*)"HDF5 error closing transfer data set: ",error
	end if

c-------Close the groups in the dataset
  
	write(unit_screen,*)"Closing HDF5 data groups"
	call h5gclose_f(geom_id, error)
	if (error .ne. 0)then
	   write(unit_error,*)"HDF5 error closing geometry group: ",error
	end if

	call h5gclose_f(data_id, error)
	if (error .ne. 0)then
	   write(unit_error,*)"HDF5 error closing data group: ",error
	end if

	call h5gclose_f(hydro_id, error)
	if (error .ne. 0)then
	   write(unit_error,*)"HDF5 error closing hydro group: ",error
	end if

c-------Close the file
 333  write(unit_screen,*)"Closing HDF5 file"
	call h5fclose_f(file_id, error)
	if (error .ne. 0)then
	   write(unit_error,*)"HDF5 error closing hdf file: ",error
	end if

	write(unit_screen,*)"Closing HDF5"
	call h5close_f(error)
	if (error .ne. 0)then
	   write(unit_error,*)"HDF5 error closing hdf5: ",error
	end if
	write(unit_screen,*)"Closed HDF5"      

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
	implicit none
	integer error
	integer getHDF5NumberOfTimeIntervals
      

      !@todo: This is done everytime read_tide_head is called:
	!       1. This is twice (which is bad) and 2. No need to do this more than once
	!       Prefer we fix this with better program structure, not "if (firsttime)"

c-------Channel stage(io for channels is one variable at at time)
	chan_z_fdata_dims(1) = MaxChannels
	chan_z_fdata_dims(2) = 2	! QChan:2,YChan:2,AChan:2,AChan_Avg:1
	chan_z_fdata_dims(3) = getHDF5NumberOfTimeIntervals()

	chan_z_fsubset_dims(1) = chan_z_fdata_dims(1) ! Number of channels
	chan_z_fsubset_dims(2) = 2
	chan_z_fsubset_dims(3) = 1 ! One step

	chan_z_mdata_dims(1) = MaxChannels
	chan_z_mdata_dims(2) = 2

	call H5Screate_simple_f(chan_z_mdata_rank,
     &                        chan_z_mdata_dims,chan_z_memspace,error)

c-------Channel flow (io for channels is one variable at at time)
	chan_q_fdata_dims(1) = MaxChannels 
	chan_q_fdata_dims(2) = 2	! QChan:2,YChan:2,AChan:2,AChan_Avg:1
	chan_q_fdata_dims(3) = getHDF5NumberOfTimeIntervals()

	chan_q_fsubset_dims(1) = chan_q_fdata_dims(1) ! Number of channels
	chan_q_fsubset_dims(2) = 2 ! one variable
	chan_q_fsubset_dims(3) = 1 ! One step

	chan_q_mdata_dims(1) = MaxChannels
	chan_q_mdata_dims(2) = 2

	call H5Screate_simple_f(chan_q_mdata_rank,
     &                        chan_q_mdata_dims,chan_q_memspace,error)

c-------Channel area
	chan_a_fdata_dims(1) = MaxChannels
	chan_a_fdata_dims(2) = 2
	chan_a_fdata_dims(3) = getHDF5NumberOfTimeIntervals()

	chan_a_fsubset_dims(1) = chan_a_fdata_dims(1) ! Number of channels
	chan_a_fsubset_dims(2) = 2
	chan_a_fsubset_dims(3) = 1 ! One step

	chan_a_mdata_dims(1) = MaxChannels
	chan_a_mdata_dims(2) = 2

	call H5Screate_simple_f(chan_a_mdata_rank,
     &                        chan_a_mdata_dims,chan_a_memspace,error)



c-------Channel avg area(io for channels is one variable at at time)
	chan_aa_fdata_dims(1) = MaxChannels
	chan_aa_fdata_dims(2) = getHDF5NumberOfTimeIntervals() 

	chan_aa_fsubset_dims(1) = chan_aa_fdata_dims(1) ! Number of channels
	chan_aa_fsubset_dims(2) = 1 ! One step

	chan_aa_mdata_dims(1) = MaxChannels
	call H5Screate_simple_f(chan_aa_mdata_rank,
     &                        chan_aa_mdata_dims,chan_aa_memspace,error); 





c-------Reservoir

	res_h_fdata_dims(1) = MaxNRes
	res_h_fdata_dims(2) = getHDF5NumberOfTimeIntervals()


	res_q_fdata_dims(1) = MaxNRes
	res_q_fdata_dims(2) = Maxresnodes ! Variabless EResv:1,QResv:Maxresnodes
	res_q_fdata_dims(3) = getHDF5NumberOfTimeIntervals()

	res_h_fsubset_dims(1) = MaxNRes
	res_h_fsubset_dims(2) = 1 ! Variabless EResv:1,QResv:Maxresnodes

	res_q_fsubset_dims(1) = MaxNRes ! Variables EResv:1,QResv:Maxresnodes
	res_q_fsubset_dims(2) = MaxResNodes
	res_q_fsubset_dims(3) = 1


	res_h_mdata_dims(1) = MaxNRes
	call H5Screate_simple_f(res_h_mdata_rank,
     &                        res_h_mdata_dims,res_h_memspace,error);   

	res_q_mdata_dims(1) = MaxNRes
	res_q_mdata_dims(2) = maxresnodes
	call H5Screate_simple_f(res_q_mdata_rank,res_q_mdata_dims,res_q_memspace,error);   

c-------Qext

	qext_fdata_dims(1) = max(1,nqext) !nqext
	qext_fdata_dims(2) = getHDF5NumberOfTimeIntervals() 
	qext_fsubset_dims(1) = max(1,nqext) !nqext
	qext_fsubset_dims(2) = 1
	

	qext_mdata_dims(1) = max(1,nqext)
	call H5Screate_simple_f(qext_mdata_rank,qext_mdata_dims,qext_memspace,error); 

c-------Transfer (obj2obj)
	transfer_fdata_dims(1) = max(1,nobj2obj) !nqext
	transfer_fdata_dims(2) = getHDF5NumberOfTimeIntervals()

	transfer_fsubset_dims(1) = transfer_fdata_dims(1)
	transfer_fsubset_dims(2) = 1

	transfer_mdata_dims(1) = max(1,nobj2obj)
	call H5Screate_simple_f(transfer_mdata_rank,
     &                        transfer_mdata_dims,transfer_memspace,error);

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

	call H5Sclose_f(chan_z_memspace, error)
	call H5Sclose_f(chan_q_memspace, error)
	call H5Sclose_f(chan_aa_memspace, error)

	call H5Sclose_f(res_h_memspace, error)
	call H5Sclose_f(res_q_memspace, error)
	call H5Sclose_f(qext_memspace, error)
	call H5Sclose_f(transfer_memspace, error)

	return
	end subroutine



***********************************************************************
***********************************************************************

	subroutine AddTimeSeriesAttributes(dset_id)

	use HDF5
	use hdfvars
	use inclvars
	use runtime_data
	use iopath_data
      implicit none
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
      character*19,external ::  jmin2iso  ! format function

      call h5screate_simple_f(arank, a_dims, aspace_id, error)

      call h5tcopy_f(H5T_NATIVE_CHARACTER, atype_id, error)
	buffer = 'TIMESERIES'
	nlen = len_trim(buffer)
      call h5tset_size_f(atype_id, nlen, error)
	call h5acreate_f(dset_id, "CLASS", 
     &    atype_id, aspace_id, attr_id, error)
	call h5awrite_f(attr_id, atype_id, trim(buffer),
     &                a_dims, error)
	call h5aclose_f(attr_id,error)

	iso_datetime=jmin2iso(tf_start_julmin)
      call h5tset_size_f(atype_id, ISO_LEN, error)
	call h5acreate_f(dset_id, "start_time", 
     &    atype_id, aspace_id, attr_id, error)
 	call h5awrite_f(attr_id, atype_id, iso_datetime(1:19),
     &     a_dims, error)
	call h5aclose_f(attr_id,error)

      nlen=len_trim(io_files(hydro,io_hdf5,io_write).interval)
      call h5tset_size_f(atype_id, nlen, error)
	call h5acreate_f(dset_id, "interval", 
     &    atype_id, aspace_id, attr_id, error)
	call h5awrite_f(attr_id, atype_id, 
     &    trim(io_files(hydro,io_hdf5,io_write).interval),
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

	integer(HID_T) :: cf_dspace_id ! Dataspace identifier
	integer     ::    cf_rank = 3 ! Dataset rank
	integer(HSIZE_T), dimension(3) :: chan_z_chunk_dims = 0 ! Dataset dimensions
	integer(HSIZE_T), dimension(3) :: chan_q_chunk_dims = 0 ! Dataset dimensions
	integer(HSIZE_T), dimension(3) :: chan_a_chunk_dims = 0 ! Dataset dimensions
	integer(HSIZE_T), dimension(2) :: chan_aa_chunk_dims = 0 ! Dataset dimensions
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
	if (getHDF5NumberOfTimeIntervals() .gt. MIN_STEPS_FOR_CHUNKING)then
	  call h5pset_chunk_f(cparms, chan_z_fdata_rank, chan_z_chunk_dims, error)
	  call H5Pset_szip_f (cparms, H5_SZIP_NN_OM_F,
     &                    HDF_SZIP_PIXELS_PER_BLOCK, error);

      end if

	
	call h5screate_simple_f(chan_z_fdata_rank, 
     &                        chan_z_fdata_dims, chan_z_fspace_id, error)
	call h5dcreate_f(data_id, "channel stage", H5T_NATIVE_REAL,
	1    chan_z_fspace_id, chan_z_dset_id, error, cparms)
	call VerifyHDF5(error,"Channel stage dataset creation")
      call AddTimeSeriesAttributes(chan_z_dset_id)

		! Create channel flow data set
	chan_q_chunk_dims(1) = chan_q_fdata_dims(1)
	chan_q_chunk_dims(2) = chan_q_fdata_dims(2)
	chan_q_chunk_dims(3) = min(TIME_CHUNK,chan_q_fdata_dims(3))
	cparms=0
		! Add chunking and compression
	call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, error)
	if (getHDF5NumberOfTimeIntervals() .gt. MIN_STEPS_FOR_CHUNKING)then
	   call h5pset_chunk_f(cparms,chan_q_fdata_rank,chan_q_chunk_dims, error)
	   call H5Pset_szip_f (cparms, H5_SZIP_NN_OM_F,
     &                    HDF_SZIP_PIXELS_PER_BLOCK, error);
	end if
	call h5screate_simple_f(chan_q_fdata_rank, chan_q_fdata_dims,
     &                        chan_q_fspace_id, error)
	call h5dcreate_f(data_id, "channel flow", H5T_NATIVE_REAL,
     &    chan_q_fspace_id, chan_q_dset_id, error, cparms)
	call VerifyHDF5(error,"Channel flow dataset creation")
      call AddTimeSeriesAttributes(chan_q_dset_id)


		! Create channel area data set
	chan_a_chunk_dims(1) = chan_a_fdata_dims(1)
	chan_a_chunk_dims(2) = chan_a_fdata_dims(2)
	chan_a_chunk_dims(3) = min(TIME_CHUNK,chan_a_fdata_dims(3))
      cparms=0
		! Add chunking and compression
	call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, error)
	if (getHDF5NumberOfTimeIntervals() .gt. MIN_STEPS_FOR_CHUNKING)then
          call h5pset_chunk_f(cparms, chan_a_fdata_rank, chan_a_chunk_dims, error)
	    call H5Pset_szip_f (cparms, H5_SZIP_NN_OM_F,
     &                    HDF_SZIP_PIXELS_PER_BLOCK, error);

	end if
	
	call h5screate_simple_f(chan_a_fdata_rank, chan_a_fdata_dims,
     &                        chan_a_fspace_id, error)
	call h5dcreate_f(data_id, "channel area", H5T_NATIVE_REAL,
     &    chan_a_fspace_id, chan_a_dset_id, error, cparms)
	call VerifyHDF5(error,"Channel area dataset creation")
      call AddTimeSeriesAttributes(chan_a_dset_id)


		! Create channel avg area data set
	chan_aa_chunk_dims(1) = chan_aa_fdata_dims(1)
	chan_aa_chunk_dims(2) = min(TIME_CHUNK,chan_aa_fdata_dims(2))
	cparms=0
			! Add chunking and compression
	call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, error)
      if (getHDF5NumberOfTimeIntervals() .gt. MIN_STEPS_FOR_CHUNKING)then
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
      call AddTimeSeriesAttributes(chan_aa_dset_id)

	return
	end subroutine

	subroutine InitReservoirsHDF5()

	use HDF5		! HDF5 This module contains all necessary modules 
	use hdfvars
	use inclvars

	implicit none

	integer(HID_T) :: res_dspace_id ! Dataspace identifier
	integer(HID_T) :: cparms !dataset creation property identifier 
	integer        :: error	! HDF5 Error flag
	integer(HSIZE_T), dimension(res_q_fdata_rank) :: 
     &	res_q_chunk_dims = 0 ! Dataset dimensions
	integer(HSIZE_T), dimension(res_h_fdata_rank) :: 
     &    res_h_chunk_dims = 0 ! Dataset dimensions
	integer getHDF5NumberOfTimeIntervals

c-------Create the datasets

	res_h_chunk_dims(1) = res_h_fdata_dims(1)
	res_h_chunk_dims(2) = min(TIME_CHUNK,res_h_fdata_dims(2))


		! Add chunking and compression
	call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, error)
	if (getHDF5NumberOfTimeIntervals() .gt. MIN_STEPS_FOR_CHUNKING)then
	   call h5pset_chunk_f(cparms, res_h_fdata_rank, res_h_chunk_dims, error)
	   call H5Pset_szip_f (cparms, H5_SZIP_NN_OM_F, 
     &                    HDF_SZIP_PIXELS_PER_BLOCK, error);
      end if

	call h5screate_simple_f(res_h_fdata_rank, res_h_fdata_dims, res_h_fspace_id, error)
	call h5dcreate_f(data_id, "reservoir height", H5T_NATIVE_REAL,
     &    res_h_fspace_id, res_h_dset_id, error, cparms)
      call AddTimeSeriesAttributes(res_h_dset_id)



	res_q_chunk_dims(1) = res_q_fdata_dims(1) 
	res_q_chunk_dims(2) = res_q_fdata_dims(2)
	res_q_chunk_dims(3) = min(TIME_CHUNK,res_q_fdata_dims(3))


				! Add chunking and compression
	call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, error)
	if (getHDF5NumberOfTimeIntervals() .gt. MIN_STEPS_FOR_CHUNKING)then
	    call h5pset_chunk_f(cparms, res_q_fdata_rank, res_q_chunk_dims, error)
	    call H5Pset_szip_f (cparms, H5_SZIP_NN_OM_F,
     &                    HDF_SZIP_PIXELS_PER_BLOCK, error);
	end if


	call h5screate_simple_f(res_q_fdata_rank, res_q_fdata_dims, res_q_fspace_id, error)
	call h5dcreate_f(data_id, "reservoir flow", H5T_NATIVE_REAL,
     &	    res_q_fspace_id, res_q_dset_id, error, cparms)
      call AddTimeSeriesAttributes(res_q_dset_id)

	return
	end subroutine

***********************************************************************
***********************************************************************

	subroutine InitTransferHDF5()

	use HDF5		! HDF5 This module contains all necessary modules 
	use hdfvars
	use inclvars
	use grid_data

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
	1    transfer_chunk_dims = 0 ! Dataset dimensions

	integer getHDF5NumberOfTimeIntervals

	transfer_chunk_dims(1) = transfer_fdata_dims(1)
	transfer_chunk_dims(2) = min(TIME_CHUNK,transfer_fdata_dims(2))

      cparms = 0
				! Add chunking and compression
	call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, error)

	if (getHDF5NumberOfTimeIntervals() .gt. MIN_STEPS_FOR_CHUNKING)then
	   call h5pset_chunk_f(cparms, transfer_fdata_rank, transfer_chunk_dims, error)
	   call H5Pset_szip_f (cparms, H5_SZIP_NN_OM_F,
     &                    HDF_SZIP_PIXELS_PER_BLOCK, error);
      end if

	
	call h5screate_simple_f(transfer_fdata_rank, transfer_fdata_dims, 
     &       transfer_fspace_id, error)
	call h5dcreate_f(data_id, "transfer flow", H5T_NATIVE_REAL,
     &       transfer_fspace_id, transfer_dset_id, error, cparms)
	call VerifyHDF5(error,"Flow transfer dataset creation")
      call AddTimeSeriesAttributes(transfer_dset_id)

	call h5screate_simple_f(arank, adims, aspace_id, error)
	call h5tcopy_f(H5T_NATIVE_INTEGER, atype_id, error)
	call h5acreate_f(hydro_id, "Number of flow transfers", 
	1    atype_id, aspace_id, attr_id, error)
	call h5awrite_f(attr_id, atype_id, nobj2obj, a_data_dims, error)
	call h5acreate_f(hydro_id, "Max Number of flow transfers", 
	1    atype_id, aspace_id, attr_id, error)
	call h5awrite_f(attr_id, atype_id, max_obj2obj, a_data_dims, error)
	call h5aclose_f(attr_id,error)

	return
	end subroutine

***********************************************************************
***********************************************************************

	subroutine InitQExtChangeHDF5()

	use HDF5		! HDF5 This module contains all necessary modules 
	use hdfvars
	use inclvars
	use grid_data

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
	1    qext_chunk_dims = 0 ! Dataset dimensions


	qext_chunk_dims(1) = qext_fdata_dims(1)
	qext_chunk_dims(2) = min(TIME_CHUNK,qext_fdata_dims(2))

				! Add chunking and compression
	call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, error)
	if (getHDF5NumberOfTimeIntervals() .gt. MIN_STEPS_FOR_CHUNKING)then
	   call h5pset_chunk_f(cparms, qext_fdata_rank, qext_chunk_dims, error)
	   call H5Pset_szip_f (cparms, H5_SZIP_NN_OM_F,
     &                    HDF_SZIP_PIXELS_PER_BLOCK, error);
      end if


	call h5screate_simple_f(qext_fdata_rank, qext_fdata_dims, qext_fspace_id, error)
	call h5dcreate_f(data_id, "qext changed", H5T_NATIVE_REAL,
	1    qext_fspace_id, qext_change_dset_id, error, cparms)
      call AddTimeSeriesAttributes(qext_change_dset_id)

	call h5screate_simple_f(arank, adims, aspace_id, error)
	call h5tcopy_f(H5T_NATIVE_INTEGER, atype_id, error)
	call h5acreate_f(hydro_id, "Number of QExt", 
	1    atype_id, aspace_id, attr_id, error)
	call h5awrite_f(attr_id, atype_id, nqext, a_data_dims, error)
	call h5acreate_f(hydro_id, "Max Number of QExt", 
	1    atype_id, aspace_id, attr_id, error)
	call h5awrite_f(attr_id, atype_id, max_qext, a_data_dims, error)

	call h5aclose_f(attr_id,error)

	return
	end subroutine

***********************************************************************
***********************************************************************

	subroutine InitObj2ObjType()

	use HDF5
	use hdfvars
	use objvars
	use inclvars
	use grid_data

	implicit none

	integer(HID_T) :: dspace_id ! Dataspace identifier
	integer(HID_T) :: dtc10_id ! Memory datatype identifier 
	integer(HID_T) :: dtc32_id ! Memory datatype identifier 

	integer(HSIZE_T), dimension(1) :: dims ! Dataset dimensions
	integer ::   rank = 1	! Dataset rank
	integer ::   error	! Error flag
	integer(SIZE_T) :: typesize
	integer(SIZE_T)     ::   oset ! Member's offset

				! Create Object Type

				!dims(1) = nobj2obj
	dims(1) = max_obj2obj

	call h5pcreate_f(H5P_DATASET_XFER_F, obj_plist_id, error)
	call h5pset_preserve_f(obj_plist_id, .true., error)

	call h5screate_simple_f(rank, dims, dspace_id, error)
	
	call h5tcopy_f(H5T_NATIVE_CHARACTER, dtc10_id, error)
	typesize = 10
	call h5tset_size_f(dtc10_id, typesize, error)
	call h5tget_size_f(dtc10_id, type_sizec10, error)
	call h5tcopy_f(H5T_NATIVE_CHARACTER, dtc32_id, error)
	typesize = 32
	call h5tset_size_f(dtc32_id, typesize, error)
	call h5tget_size_f(dtc32_id, type_sizec32, error)
	call h5tget_size_f(H5T_NATIVE_INTEGER, type_sizei, error)
	call h5tget_size_f(H5T_NATIVE_REAL, type_sizer, error)
	type_size = loc(obj2obj(2)) - loc(obj2obj(1))
c-------print*,"Obj2Obj type size (1): ", type_size
	type_size = (2 * type_sizec10) + (4 * type_sizec32) + (10 * type_sizei) + ((9 + max_constituent) * type_sizer)
	call h5tcreate_f(H5T_COMPOUND_F, type_size, obj2obj_type_id, error)
c-------print*,"Obj2Obj type size: (2)", type_size
	oset = 0
	call h5tinsert_f(obj2obj_type_id, "name",           oset, dtc32_id, error)
	oset = oset + type_sizec32 
	call h5tinsert_f(obj2obj_type_id, "from_id",        oset, H5T_NATIVE_INTEGER, error)
	oset = oset + type_sizei
	call h5tinsert_f(obj2obj_type_id, "from_name",      oset, dtc32_id, error)
	oset = oset + type_sizec32
	call h5tinsert_f(obj2obj_type_id, "from_number",    oset, H5T_NATIVE_INTEGER, error)
	oset = oset + type_sizei
	call h5tinsert_f(obj2obj_type_id, "from_hydrochan", oset, H5T_NATIVE_INTEGER, error)
	oset = oset + type_sizei
	call h5tinsert_f(obj2obj_type_id, "from_acctname",  oset, dtc10_id, error)
	oset = oset + type_sizec10
	call h5tinsert_f(obj2obj_type_id, "from_acctidx",   oset, H5T_NATIVE_INTEGER, error)
	oset = oset + type_sizei
	call h5tinsert_f(obj2obj_type_id, "from_massfract", oset, H5T_NATIVE_REAL, error)
	oset = oset + type_sizer
	call h5tinsert_f(obj2obj_type_id, "from_coeff",     oset, H5T_NATIVE_REAL, error)
	oset = oset + type_sizer
	call h5tinsert_f(obj2obj_type_id, "to_id",          oset, H5T_NATIVE_INTEGER, error)
	oset = oset + type_sizei
	call h5tinsert_f(obj2obj_type_id, "to_name",        oset, dtc32_id, error)
	oset = oset + type_sizec32
	call h5tinsert_f(obj2obj_type_id, "to_number",      oset, H5T_NATIVE_INTEGER, error)
	oset = oset + type_sizei
	call h5tinsert_f(obj2obj_type_id, "to_hydrochan",   oset, H5T_NATIVE_INTEGER, error)
	oset = oset + type_sizei
	call h5tinsert_f(obj2obj_type_id, "to_acctname",    oset, dtc10_id, error)
	oset = oset + type_sizec10
	call h5tinsert_f(obj2obj_type_id, "to_acctidx",     oset, H5T_NATIVE_INTEGER, error)
	oset = oset + type_sizei
	call h5tinsert_f(obj2obj_type_id, "to_massfract",   oset, H5T_NATIVE_REAL, error)
	oset = oset + type_sizer
	call h5tinsert_f(obj2obj_type_id, "to_coeff",       oset, H5T_NATIVE_REAL, error)
	oset = oset + type_sizer
	call h5tinsert_f(obj2obj_type_id, "constantval",    oset, H5T_NATIVE_REAL, error)
	oset = oset + type_sizer
	call h5tinsert_f(obj2obj_type_id, "datasrc_type",   oset, H5T_NATIVE_INTEGER, error)
	oset = oset + type_sizei
	call h5tinsert_f(obj2obj_type_id, "datasrc_index",  oset, H5T_NATIVE_INTEGER, error)
	oset = oset + type_sizei
	call h5tinsert_f(obj2obj_type_id, "datasrc_value",  oset, H5T_NATIVE_REAL, error)
	oset = oset + type_sizer
	call h5tinsert_f(obj2obj_type_id, "current_flow",   oset, H5T_NATIVE_REAL, error)
	oset = oset + type_sizer
	call h5tinsert_f(obj2obj_type_id, "previous_flow",  oset, H5T_NATIVE_REAL, error)
	oset = oset + type_sizer
	call h5tinsert_f(obj2obj_type_id, "average_flow",   oset, H5T_NATIVE_REAL, error)
	oset = oset + type_sizer
	call h5tinsert_f(obj2obj_type_id, "concentrations", oset, H5T_NATIVE_REAL, error)
				! Concentrations needs to be extended as an array !!TODO
	
	call h5dcreate_f(geom_id, "obj2obj", obj2obj_type_id, dspace_id, obj2obj_dset_id, error)

	call InitObj2ObjType2()

	return
	end subroutine

***********************************************************************
***********************************************************************

	subroutine InitObj2ObjType2()

	use HDF5
	use hdfvars
	use objvars
	use inclvars
	use grid_data

	implicit none

	integer(HID_T) :: dspace_id ! Dataspace identifier
	integer(HID_T) :: dtc10_id ! Memory datatype identifier 
	integer(HID_T) :: dtc32_id ! Memory datatype identifier 

	integer(HSIZE_T), dimension(1) :: dims ! Dataset dimensions
	integer ::   rank = 1	! Dataset rank
	integer ::   error	! Error flag
	integer(SIZE_T) :: typesize
	integer(SIZE_T)     ::   oset ! Member's offset

				! Create Object Type

				!dims(1) = nobj2obj
	dims(1) = max_obj2obj
	
	call h5tcopy_f(H5T_NATIVE_CHARACTER, dtc10_id, error)
	typesize = 10
	call h5tset_size_f(dtc10_id, typesize, error)
	call h5tget_size_f(dtc10_id, type_sizec10, error)
	call h5tcopy_f(H5T_NATIVE_CHARACTER, dtc32_id, error)
	typesize = 32
	call h5tset_size_f(dtc32_id, typesize, error)
	call h5tget_size_f(dtc32_id, type_sizec32, error)
	call h5tget_size_f(H5T_NATIVE_INTEGER, type_sizei, error)
	call h5tget_size_f(H5T_NATIVE_REAL, type_sizer, error)
	type_size = (2 * type_sizec10) + (4 * type_sizec32) + (10 * type_sizei) + ((9 + max_constituent) * type_sizer)
	call h5tcreate_f(H5T_COMPOUND_F, type_size, obj2obj_type_id, error)

				!! TODO : Add nobj2obj as an attribute

	oset = 0
	call h5tcreate_f(H5T_COMPOUND_F,  type_sizec32,     o_name_tid, error)
	call h5tinsert_f(o_name_tid,      "name",           oset, dtc32_id, error)

	call h5tcreate_f(H5T_COMPOUND_F,  type_sizei,       fo_id_tid, error)
	call h5tinsert_f(fo_id_tid,       "from_id",        oset, H5T_NATIVE_INTEGER, error)

	call h5tcreate_f(H5T_COMPOUND_F,  type_sizec32,     fo_name_tid, error)
	call h5tinsert_f(fo_name_tid,     "from_name",      oset, dtc32_id, error)

	call h5tcreate_f(H5T_COMPOUND_F,  type_sizei,       fo_num_tid, error)
	call h5tinsert_f(fo_num_tid,      "from_number",    oset, H5T_NATIVE_INTEGER, error)

	call h5tcreate_f(H5T_COMPOUND_F,  type_sizei,       fo_hychan_tid, error)
	call h5tinsert_f(fo_hychan_tid,   "from_hydrochan", oset, H5T_NATIVE_INTEGER, error)

	call h5tcreate_f(H5T_COMPOUND_F,  type_sizec10,     fo_accname_tid, error)
	call h5tinsert_f(fo_accname_tid,  "from_acctname",  oset, dtc10_id, error)

	call h5tcreate_f(H5T_COMPOUND_F,  type_sizei,       fo_accidx_tid, error)
	call h5tinsert_f(fo_accidx_tid,   "from_acctidx",   oset, H5T_NATIVE_INTEGER, error)

	call h5tcreate_f(H5T_COMPOUND_F,  type_sizer,       fo_massfrac_tid, error)
	call h5tinsert_f(fo_massfrac_tid, "from_massfract", oset, H5T_NATIVE_REAL, error)

	call h5tcreate_f(H5T_COMPOUND_F,  type_sizer,       fo_coeff_tid, error)
	call h5tinsert_f(fo_coeff_tid,    "from_coeff",     oset, H5T_NATIVE_REAL, error)

	call h5tcreate_f(H5T_COMPOUND_F,  type_sizei,       to_id_tid, error)
	call h5tinsert_f(to_id_tid,       "to_id",          oset, H5T_NATIVE_INTEGER, error)

	call h5tcreate_f(H5T_COMPOUND_F,  type_sizec32,     to_name_tid, error)
	call h5tinsert_f(to_name_tid,     "to_name",        oset, dtc32_id, error)

	call h5tcreate_f(H5T_COMPOUND_F,  type_sizei,       to_num_tid, error)
	call h5tinsert_f(to_num_tid,      "to_number",      oset, H5T_NATIVE_INTEGER, error)

	call h5tcreate_f(H5T_COMPOUND_F,  type_sizei,       to_hychan_tid, error)
	call h5tinsert_f(to_hychan_tid,   "to_hydrochan",   oset, H5T_NATIVE_INTEGER, error)

	call h5tcreate_f(H5T_COMPOUND_F,  type_sizec10,     to_accname_tid, error)
	call h5tinsert_f(to_accname_tid,  "to_acctname",    oset, dtc10_id, error)

	call h5tcreate_f(H5T_COMPOUND_F,  type_sizei,       to_accidx_tid, error)
	call h5tinsert_f(to_accidx_tid,   "to_acctidx",     oset, H5T_NATIVE_INTEGER, error)

	call h5tcreate_f(H5T_COMPOUND_F,  type_sizer,       to_massfrac_tid, error)
	call h5tinsert_f(to_massfrac_tid, "to_massfract",   oset, H5T_NATIVE_REAL, error)

	call h5tcreate_f(H5T_COMPOUND_F,  type_sizer,       to_coeff_tid, error)
	call h5tinsert_f(to_coeff_tid,    "to_coeff",       oset, H5T_NATIVE_REAL, error)

	call h5tcreate_f(H5T_COMPOUND_F,  type_sizer,       constval_tid, error)
	call h5tinsert_f(constval_tid,    "constantval",    oset, H5T_NATIVE_REAL, error)

	call h5tcreate_f(H5T_COMPOUND_F,  type_sizei,       datasrc_type_tid, error)
	call h5tinsert_f(datasrc_type_tid,"datasrc_type",   oset, H5T_NATIVE_INTEGER, error)

	call h5tcreate_f(H5T_COMPOUND_F,  type_sizei,       datasrc_idx_tid, error)
	call h5tinsert_f(datasrc_idx_tid, "datasrc_index",  oset, H5T_NATIVE_INTEGER, error)

	call h5tcreate_f(H5T_COMPOUND_F,  type_sizer,       datasrc_val_tid, error)
	call h5tinsert_f(datasrc_val_tid, "datasrc_value",  oset, H5T_NATIVE_REAL, error)

	call h5tcreate_f(H5T_COMPOUND_F,  type_sizer,       curQ_tid, error)
	call h5tinsert_f(curQ_tid,        "current_flow",   oset, H5T_NATIVE_REAL, error)

	call h5tcreate_f(H5T_COMPOUND_F,  type_sizer,       prevQ_tid, error)
	call h5tinsert_f(prevQ_tid,       "previous_flow",  oset, H5T_NATIVE_REAL, error)

	call h5tcreate_f(H5T_COMPOUND_F,  type_sizer,       avgQ_tid, error)
	call h5tinsert_f(avgQ_tid,        "average_flow",   oset, H5T_NATIVE_REAL, error)

	call h5tcreate_f(H5T_COMPOUND_F,  type_sizer,       conc_tid, error)
	call h5tinsert_f(conc_tid,        "concentrations", oset, H5T_NATIVE_REAL, error)

				! Create Obj2Obj Type (uses Object Type)

	return
	end subroutine

***********************************************************************
***********************************************************************

	subroutine InitQExtType()

	use HDF5
	use hdfvars
	use qextvars
	use inclvars
	use grid_data

	implicit none

	integer(HID_T) :: dspace_id ! Dataspace identifier
	integer(HID_T) :: dtc10_id ! Memory datatype identifier 
	integer(HID_T) :: dtc32_id ! Memory datatype identifier 

	integer(HSIZE_T), dimension(1) :: dims ! Dataset dimensions
	integer ::   rank = 1	! Dataset rank
	integer ::   error	! Error flag
	integer(SIZE_T) :: typesize
	integer(SIZE_T)     ::   oset ! Member's offset

	dims(1) = max_qext

	call h5pcreate_f(H5P_DATASET_XFER_F, qext_plist_id, error)
	call h5pset_preserve_f(qext_plist_id, .true., error)

	call h5screate_simple_f(rank, dims, dspace_id, error)
	
	call h5tcopy_f(H5T_NATIVE_CHARACTER, dtc10_id, error)
	typesize = 10
	call h5tset_size_f(dtc10_id, typesize, error)
	call h5tget_size_f(dtc10_id, type_sizec10, error)
	call h5tcopy_f(H5T_NATIVE_CHARACTER, dtc32_id, error)
	typesize = 32
	call h5tset_size_f(dtc32_id, typesize, error)
	call h5tget_size_f(dtc32_id, type_sizec32, error)
	call h5tget_size_f(H5T_NATIVE_INTEGER, type_sizei, error)
	call h5tget_size_f(H5T_NATIVE_REAL, type_sizer, error)
	type_size = (0 * type_sizec10) + (3 * type_sizec32) + (6 * type_sizei) + (6 * type_sizer)

	call h5tcreate_f(H5T_COMPOUND_F, type_size, qext_type_id, error)

	oset = 0
	call h5tinsert_f(qext_type_id, "name",          oset, dtc32_id, error)
	oset = oset + type_sizec32 
	call h5tinsert_f(qext_type_id, "flow",          oset, H5T_NATIVE_REAL, error)
	oset = oset + type_sizer
	call h5tinsert_f(qext_type_id, "prev_flow",     oset, H5T_NATIVE_REAL, error)
	oset = oset + type_sizer
	call h5tinsert_f(qext_type_id, "average",       oset, H5T_NATIVE_REAL, error)
	oset = oset + type_sizer
	call h5tinsert_f(qext_type_id, "prev_average",  oset, H5T_NATIVE_REAL, error)
	oset = oset + type_sizer
	call h5tinsert_f(qext_type_id, "datasrc_type",  oset, H5T_NATIVE_INTEGER, error)
	oset = oset + type_sizei
	call h5tinsert_f(qext_type_id, "datasrc_index", oset, H5T_NATIVE_INTEGER, error)
	oset = oset + type_sizei
	call h5tinsert_f(qext_type_id, "datasrc_value", oset, H5T_NATIVE_REAL, error)
	oset = oset + type_sizer
	call h5tinsert_f(qext_type_id, "changed_idx",   oset, H5T_NATIVE_INTEGER, error)
	oset = oset + type_sizei
	call h5tinsert_f(qext_type_id, "object_name",   oset, dtc32_id, error)
	oset = oset + type_sizec32
	call h5tinsert_f(qext_type_id, "attach_id",     oset, H5T_NATIVE_INTEGER, error)
	oset = oset + type_sizei
	call h5tinsert_f(qext_type_id, "attach_name",   oset, dtc32_id, error)
	oset = oset + type_sizec32
	call h5tinsert_f(qext_type_id, "attach_number", oset, H5T_NATIVE_INTEGER, error)
	oset = oset + type_sizei
	call h5tinsert_f(qext_type_id, "group_idx",     oset, H5T_NATIVE_INTEGER, error)
	oset = oset + type_sizei
	call h5tinsert_f(qext_type_id, "mass_fraction", oset, H5T_NATIVE_REAL, error)
	
	
	call h5dcreate_f(geom_id, "qext", qext_type_id, dspace_id, qext_dset_id, error)

	call InitQExtType2()

	return
	end subroutine

***********************************************************************
***********************************************************************

	subroutine InitQExtType2()

	use HDF5
	use hdfvars
	use qextvars
	use inclvars
	use grid_data

	implicit none

	integer(HID_T) :: dspace_id ! Dataspace identifier
	integer(HID_T) :: dtc10_id ! Memory datatype identifier 
	integer(HID_T) :: dtc32_id ! Memory datatype identifier 

	integer(HSIZE_T), dimension(1) :: dims ! Dataset dimensions
	integer ::   rank = 1	! Dataset rank
	integer ::   error	! Error flag
	integer(SIZE_T) :: typesize
	integer(SIZE_T)     ::   oset ! Member's offset

	dims(1) = max_qext

	call h5tcopy_f(H5T_NATIVE_CHARACTER, dtc10_id, error)
	typesize = 10
	call h5tset_size_f(dtc10_id, typesize, error)
	call h5tget_size_f(dtc10_id, type_sizec10, error)
	call h5tcopy_f(H5T_NATIVE_CHARACTER, dtc32_id, error)
	typesize = 32
	call h5tset_size_f(dtc32_id, typesize, error)
	call h5tget_size_f(dtc32_id, type_sizec32, error)
	call h5tget_size_f(H5T_NATIVE_INTEGER, type_sizei, error)
	call h5tget_size_f(H5T_NATIVE_REAL, type_sizer, error)

	oset = 0
	call h5tcreate_f(H5T_COMPOUND_F,  type_sizec32,   q_name_tid, error)
	call h5tinsert_f(q_name_tid,       "name",           oset, dtc32_id, error)

	call h5tcreate_f(H5T_COMPOUND_F,  type_sizer,     q_flow_tid, error)
	call h5tinsert_f(q_flow_tid,        "flow",          oset, H5T_NATIVE_REAL, error)

	call h5tcreate_f(H5T_COMPOUND_F,  type_sizer,     q_prev_flow_tid, error)
	call h5tinsert_f(q_prev_flow_tid,   "prev_flow",     oset, H5T_NATIVE_REAL, error)

	call h5tcreate_f(H5T_COMPOUND_F,  type_sizer,     q_avg_tid, error)
	call h5tinsert_f(q_avg_tid,         "average",       oset, H5T_NATIVE_REAL, error)

	call h5tcreate_f(H5T_COMPOUND_F,  type_sizer,     q_prev_avg_tid, error)
	call h5tinsert_f(q_prev_avg_tid,    "prev_avg",      oset, H5T_NATIVE_REAL, error)

	call h5tcreate_f(H5T_COMPOUND_F,  type_sizei,     q_dsrc_type_tid, error)
	call h5tinsert_f(q_dsrc_type_tid,   "datasrc_type",  oset, H5T_NATIVE_INTEGER, error)

	call h5tcreate_f(H5T_COMPOUND_F,  type_sizei,     q_dsrc_idx_tid, error)
	call h5tinsert_f(q_dsrc_idx_tid,    "datasrc_index", oset, H5T_NATIVE_INTEGER, error)

	call h5tcreate_f(H5T_COMPOUND_F,  type_sizer,     q_dsrc_val_tid, error)
	call h5tinsert_f(q_dsrc_val_tid,    "datasrc_value", oset, H5T_NATIVE_REAL, error)

	call h5tcreate_f(H5T_COMPOUND_F,  type_sizei,     q_chng_idx_tid, error)
	call h5tinsert_f(q_chng_idx_tid,    "changed_idx",   oset, H5T_NATIVE_INTEGER, error)

	call h5tcreate_f(H5T_COMPOUND_F,  type_sizec32,   q_obj_name_tid, error)
	call h5tinsert_f(q_obj_name_tid,    "object_name",   oset, dtc32_id, error)

	call h5tcreate_f(H5T_COMPOUND_F,  type_sizei,     q_attach_id_tid, error)
	call h5tinsert_f(q_attach_id_tid,   "attach_id",     oset, H5T_NATIVE_INTEGER, error)

	call h5tcreate_f(H5T_COMPOUND_F,  type_sizec32,   q_attach_name_tid, error)
	call h5tinsert_f(q_attach_name_tid, "attach_name",   oset, dtc32_id, error)

	call h5tcreate_f(H5T_COMPOUND_F,  type_sizei,     q_attach_num_tid, error)
	call h5tinsert_f(q_attach_num_tid,  "attach_number", oset, H5T_NATIVE_INTEGER, error)

	call h5tcreate_f(H5T_COMPOUND_F,  type_sizei,     q_grp_idx_tid, error)
	call h5tinsert_f(q_grp_idx_tid,     "group_idx",     oset, H5T_NATIVE_INTEGER, error)

	call h5tcreate_f(H5T_COMPOUND_F,  type_sizer,     q_mass_frac_tid, error)
	call h5tinsert_f(q_mass_frac_tid,   "mass_fraction", oset, H5T_NATIVE_REAL, error)

	return
	end subroutine




***********************************************************************
***********************************************************************
