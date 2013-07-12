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

module gtm_hdf_output

    use HDF5, only: HID_T, HSIZE_T
    use misc_vars
      
    type gtm_hdf_t 
         character*128 file_name
         integer        :: write_interval
         integer        :: start_julmin
         integer        :: time_index
         integer(hid_t) :: file_id
         integer(hid_t) :: gtm_id
         integer(hid_t) :: data_id
         integer(hid_t) :: chan_conc_id
         integer(hid_t) :: chan_ave_conc_id          
         integer(hid_t) :: res_conc_id
         integer(hsize_t) :: conc_dim
         integer(hsize_t) :: channel_dim
         integer(hsize_t) :: res_dim
         integer(hsize_t) :: time_dim
    end type  
    type(gtm_hdf_t) :: gtm_hdf
    
    logical :: using_gtm_hdf = .false.
    integer,parameter :: HDF_SZIP_PIXELS_PER_BLOCK = 16
    integer,parameter :: TIME_CHUNK = HDF_SZIP_PIXELS_PER_BLOCK
	integer,parameter :: MIN_STEPS_FOR_CHUNKING = TIME_CHUNK
	
    contains

    !< Initialize the GTM output file
    !< Assumes that the hdf5 fortran api (h5open) was opened already
    subroutine InitGtmHdf(hdf_file,   &
                          hdf_name,   &
                          nchannel,   &
                          nres,       &
                          nconc,      &
                          sim_start,  &
                          sim_end,    &
                          hdf_interval_char)

	  use HDF5		! HDF5 This module contains all necessary modules
	  use IO_Units
	  use runtime_data
	  use constants
	  implicit none
      !----- args
      type(gtm_hdf_t), intent(inout) ::hdf_file ! persistent info about file and datasets
      character*128, intent(in) :: hdf_name    ! name of qual hdf5 file
      integer :: sim_start                     ! first write time
      integer :: sim_end                       ! last write time
      character*16 :: hdf_interval_char             ! interval
      integer :: nchannel
      integer :: nres
      integer :: nconc
            
      !----- locals      
      integer :: hdf_start
      integer :: hdf_end
      integer :: hdf_interval
      integer :: ntime   ! number of time points in hdf5 file

	  integer(HID_T) :: access_plist ! Dataset trasfer property
	  integer(SIZE_T) :: rddc_nelmts
	  integer(SIZE_T) :: rddc_nbytes
	  integer  nelmts
	  real rdcc_w0
      
      logical :: h5_file_exists
  	  integer :: error	! HDF5 Error flag

      integer :: incr_intvl

	  ! check to see if file exists
	  inquire (file=hdf_name, exist=h5_file_exists)

	  if (h5_file_exists) then ! file already exists
	      write(unit_error,920) trim(hdf_name)
 920	  format(' File already exists... deleting existing file :: ', a )
	  endif

      hdf_file.file_name = hdf_name
 	  ! set up stdio to allow for buffered read/write
	  call H5Pcreate_f(H5P_FILE_ACCESS_F, access_plist, error)
      
	  call h5pget_cache_f(access_plist, nelmts,                        &
                          rddc_nelmts, rddc_nbytes, rdcc_w0,error)
	  rddc_nbytes = 8000000
	  call h5pset_cache_f(access_plist, nelmts, rddc_nelmts,       &
                          rddc_nbytes, rdcc_w0,error)
	  call VerifyHDF5(error,"Cache set")

	  ! start up garbage collection 
  	  !call h5pset_gc_references_f(access_plist_id,1,error)

      ! Create the HDF5 file
	  if (print_level .gt. 1) then
	      write(unit_screen,*)"Creating new HDF5 file"
	  end if
	  call h5fcreate_f(hdf_name, H5F_ACC_TRUNC_F, hdf_file.file_id, error,  &
                       H5P_DEFAULT_F, access_plist)

      !todo: does this create unwanted project dependence?
      call write_input_buffers_hdf5(hdf_file.file_id)
      
	  ! create group for output
	  call h5gcreate_f(hdf_file.file_id, "output", hdf_file.data_id, error)
	
	  hdf_interval = incr_intvl(0,hdf_interval_char,TO_BOUNDARY)
	  gtm_hdf.write_interval = hdf_interval
	  if (hdf_interval < time_step) then
	      write(unit_error,*)"HDF write interval is finer than the simulation time step"
	      call exit(-3)
	  end if
	
	  ! This would be more complex if time averages were stored
      hdf_start=incr_intvl(sim_start,hdf_interval_char, NEAREST_BOUNDARY)
      gtm_hdf.start_julmin = hdf_start

      ! todo: is this "1+" always right? It wasn't in the original code      
      ! record the dimensions of the simulation
	  ntime = 1+(sim_end - hdf_start)/hdf_interval
      hdf_end = hdf_start + (ntime-1)*hdf_interval

      hdf_file.channel_dim = nchannel
      hdf_file.conc_dim = nconc
      hdf_file.time_dim = ntime
	  hdf_file.res_dim = nres
	  hdf_file.time_index = 1
	  hdf_file.time_dim = ntime
	
	  call WriteDimensions(hdf_file.data_id)
	
	  ! create the data sets for time-varying output
	  call InitChannelQualHDF5(hdf_file,nchannel,nconc,ntime)
	  if (hdf_file.res_dim .gt. 0)then
	      call InitReservoirQualHDF5(hdf_file, nres, nconc, ntime)
	  end if
	
      ! initialize attributes and datasets
      ! call write_qual_attributes_hdf5()
      call attach_qual_dimscales(hdf_file.file_id)

      using_gtm_hdf = .true.
	  return
	end subroutine      

!==============================================================
!==============================================================

	subroutine InitChannelGtmHDF5(hdf_file, nchannel, nconc, ntime)

	  use hdf5
	  use runtime_data
	  implicit none	  
      type(gtm_hdf_t), intent(out) ::hdf_file
      
	  integer(HID_T) :: cparms !dataset creatation property identifier 
  	  integer        :: error	! HDF5 Error flag
	  integer(HID_T) :: attr_id    ! Attribute identifier 
	  integer(HID_T) :: aspace_id  ! Attribute dataspace identifier 
	  integer(HID_T) :: atype_id   ! Attribute type identifier 
	  integer(HSIZE_T), dimension(1) :: adims = (/1/) ! Attribute dimension
	  integer     ::   arank = 1   ! Attribute rank
	  integer(HSIZE_T), dimension(7) :: a_data_dims
      integer     ::   chan_rank = 0
	  integer(HSIZE_T), dimension(4) :: chan_file_dims  = 0 ! Data size on file
	  integer(HSIZE_T), dimension(4) :: chan_chunk_dims = 0 ! Dataset chunk dimensions

	  integer(HID_T) :: fspace_id   ! File space identifier

	  integer ntime
	  integer nchannel
	  integer nconc
	
	  character*14,external :: jmin2cdt

  	  call h5screate_simple_f(arank, adims, aspace_id, error)
	  call h5tcopy_f(H5T_NATIVE_INTEGER, atype_id, error)
	  
      chan_rank = 4
	  chan_file_dims(1) = 2
	  chan_file_dims(2) = nchannel
	  chan_file_dims(3) = nconc
	  chan_file_dims(4) = ntime

	  chan_chunk_dims(1) = 2
	  chan_chunk_dims(2) = nchannel
	  chan_chunk_dims(3) = nconc
	  chan_chunk_dims(4) = min(TIME_CHUNK,ntime)
	
	  ! Create channel data set, one data point at top and bottom of channel
	  ! Add chunking and compression
      
      cparms=0
	  call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, error)
	  if (ntime .gt. MIN_STEPS_FOR_CHUNKING) then
	      call h5pset_chunk_f(cparms, chan_rank, chan_chunk_dims, error)
	      call H5Pset_szip_f (cparms, H5_SZIP_NN_OM_F,                 &
                              HDF_SZIP_PIXELS_PER_BLOCK, error);
      end if

	  call h5screate_simple_f(chan_rank,         &
                              chan_file_dims,    &
                              fspace_id,         &
                              error)
     
	  call h5dcreate_f(hdf_file.data_id,         &
                       "channel concentration",  &
                       H5T_NATIVE_REAL,          &
                       fspace_id,                &
                       hdf_file.chan_conc_id,    &
                       error,                    &
                       cparms)
	  call VerifyHDF5(error,"Channel concentration dataset creation")
      call AddTimeSeriesAttributes(hdf_file.chan_conc_id,                            &
                                   hdf_file.start_julmin, hdf_file.write_interval)
                                 
      ! Create channel avg area data set, one data point per channel
      chan_rank = 3
	  chan_file_dims(1) = nchannel
	  chan_file_dims(2) = nconc
	  chan_file_dims(3) = ntime
      
	  chan_chunk_dims(1) = nchannel
	  chan_chunk_dims(2) = nconc
	  chan_chunk_dims(3) = min(TIME_CHUNK,ntime)
	
	  cparms = 0
	  ! Add chunking and compression
	  call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, error)
      if (ntime.gt. MIN_STEPS_FOR_CHUNKING) then
          call h5pset_chunk_f(cparms,            &
                              chan_rank,         &
                              chan_chunk_dims,   &
                              error)
	      call H5Pset_szip_f (cparms, H5_SZIP_NN_OM_F,              &
                              HDF_SZIP_PIXELS_PER_BLOCK, error);
	  end if

	  call h5screate_simple_f(chan_rank,             &
                              chan_file_dims,        &
                              fspace_id,             &
                              error)
	  call h5dcreate_f(hdf_file.data_id,             &   
                       "channel avg concentration",  &
                       H5T_NATIVE_REAL,              &
      	               fspace_id,                    &
                       hdf_file.chan_ave_conc_id,    &
                       error,                        &
                       cparms)
	  call VerifyHDF5(error,"Channel avg conc dataset creation")
      call AddTimeSeriesAttributes(hdf_file.chan_ave_conc_id,        &
                                   hdf_file.start_julmin, hdf_file.write_interval)

	  return
	end subroutine

    subroutine WriteDimensions(loc_id)
      use hdf5
      use runtime_data
      !use grid_data
      use common_gtm
      implicit none
      integer (hid_t) :: loc_id      
      integer(HSIZE_T), dimension(1) :: in_dims != (/0/) ! Dataset dimensions
      integer(HID_T) :: in_dspace_id ! Dataspace identifier
      integer(HID_T) :: in_dset_id ! Dataspace identifier

      integer     ::    in_rank = 1 ! Dataset rank

      integer(HID_T) :: cparms  !dataset creatation property identifier
      integer(HID_T) :: cg_dspace_id ! Dataspace identifier
      integer     ::    cg_rank = 2 ! Dataset rank
      integer(HSIZE_T), dimension(7) :: cg_data_dims
      integer(HID_T) :: memspace ! memspace identifier

      integer, parameter :: label_len = 12
  	  integer, parameter :: name_len=32
      character(LEN=label_len),dimension(2) :: chan_location              &
                                         = (/"upstream","downstream"/)
      character(LEN=name_len),dimension(:), allocatable :: names
      integer :: i
      integer :: ierror
      in_dims(1) = nchans
      
      ! Write out channel geometry
      call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, ierror)      
 
      ! Write out external channel numbers int2ext
      call h5screate_simple_f(in_rank, in_dims, in_dspace_id, ierror)
      call h5dcreate_f(loc_id, "channel_number", H5T_NATIVE_INTEGER,      &
                       in_dspace_id, in_dset_id, ierror, cparms)
      call h5dwrite_f(in_dset_id,H5T_NATIVE_INTEGER,                      &
                      int2ext(1), in_dims, ierror)
      call h5sclose_f (in_dspace_id, ierror)
      call h5dclose_f(in_dset_id,ierror)

      ! Write channel up/down labels
	  in_dims(1) = 2
	  call Write1DStringArray(loc_id,"channel_location",                  &
                              chan_location,label_len,2)
      
	  ! Write reservoir names
	  in_dims(1) = max(1,nreser)
      allocate(names(max(1,nreser)))
	  names = ' '
	  do i=1,max(1,nreser)
	      names(i)=res_geom(i).name 
      end do
 	  call Write1DStringArray(loc_id,"reservoir_names",names,             &
                              name_len,max(1,nreser))
      deallocate(names)     

      ! Write external flow names
	  in_dims(1) = max(1,nqext)
      allocate(names(max(1,nqext)))
	  names = ' '
	  do i=1,max(1,nqext)
	      names(i)=qext(i).name 
      end do
	  call Write1DStringArray(loc_id,"external_flow_names",names,         &
                              name_len,max(1,nqext))
      deallocate(names) 

      ! Write constituent names
	  in_dims(1)=max(1,no_of_constituent)
      allocate(names(max(1,no_of_constituent)))
	  names = ' '
	  do i=1,max(1,no_of_constituent)
	     names(i)=constituents(i).name 
      end do 
	  call Write1DStringArray(loc_id,"constituent_names",names,           &
                             name_len,max(1,no_of_constituent))
      deallocate(names) 
      return
    end subroutine
  
!==================================================   
      
    subroutine WriteQualHDF5(hdf_file,       &
                             chan_conc,      &
                             chan_ave_conc,  &    
                             res_conc,       &
                             nchan,          &
                             nres,           &
                             nconc,          &
                             time_index)
      use hdf5
      implicit none
      type(gtm_hdf_t) :: hdf_file
      integer nchan, nres, nconc
      real*4, intent(in) ::  chan_conc(2,nchan,nconc) ! chan data to write
      real*4, intent(in) ::  chan_ave_conc(nchan,nconc) ! chan data to write
      real*4, intent(in) ::  res_conc(nres,nconc)    ! res data to write
      integer time_index
      integer :: chan_rank
      integer :: res_rank
      integer(HID_T) :: fspace_id
      integer(HID_T) :: memspace_id
      integer(HSIZE_T), dimension(4) :: mdata_dims  = 0   ! Dims of data in memory
      integer(HSIZE_T), dimension(4) :: subset_dims  = 0  ! Dims of subset for time step
	  integer(HSIZE_T), dimension(4) :: h_offset = (/0,0,0,0/)
      integer :: error   ! HDF5 Error flag
      if (mod(time_index,24*10) .eq. 1) call h5garbage_collect_f(error)
      
      !-----chan conc
      h_offset(1) = 0
      h_offset(2) = 0
      h_offset(3) = 0
      h_offset(4) = time_index

	  subset_dims(1) = 2
	  subset_dims(2) = nchan
	  subset_dims(3) = nconc
	  subset_dims(4) = 1

	  mdata_dims(1) = 2
	  mdata_dims(2) = nchan
	  mdata_dims(3) = nconc
      chan_rank = 3
      call H5Screate_simple_f(chan_rank,   &
                              mdata_dims,  &
                              memspace_id, &
                              error);   
      call h5dget_space_f (hdf_file.chan_conc_id,  &
                           fspace_id,              &
                           error)
      call h5sselect_hyperslab_f(fspace_id,         &
                                 H5S_SELECT_SET_F,  &
                                 h_offset,          &
                                 subset_dims,       &
                                 error)
      call h5dwrite_f(hdf_file.chan_conc_id,   &
                      H5T_NATIVE_REAL,         &
                      chan_conc,               &
                      mdata_dims,              &
                      error,                   &
                      memspace_id,             &
                      fspace_id)
	  call VerifyHDF5(error,"Channel concentration write")
      call h5sclose_f (fspace_id, error)
      call h5sclose_f (memspace_id, error)
      
      !-----chan ave conc
      h_offset(1) = 0
      h_offset(2) = 0
      h_offset(3) = time_index

	  subset_dims(1) = nchan
	  subset_dims(2) = nconc
	  subset_dims(3) = 1

	  mdata_dims(1) = nchan
	  mdata_dims(2) = nconc
      chan_rank = 2
      call H5Screate_simple_f(chan_rank,   &
                              mdata_dims,  &
                              memspace_id, &
                              error);  
      call h5dget_space_f (hdf_file.chan_ave_conc_id, &
                           fspace_id,                 &
                           error)
      call h5sselect_hyperslab_f(fspace_id,           &
                                 H5S_SELECT_SET_F,    &
                                 h_offset,            &
                                 subset_dims,         &
                                 error)
      call h5dwrite_f(hdf_file.chan_ave_conc_id,      &
                      H5T_NATIVE_REAL,                &
                      chan_ave_conc,                  &
                      mdata_dims,                     &
                      error,                          &
                      memspace_id,                    &
                      fspace_id)
	  call VerifyHDF5(error,"Channel concentration write")
      call h5sclose_f (fspace_id, error)
      call h5sclose_f (memspace_id, error)      
      
      if (nres .gt. 0)then
      
      h_offset(1) = 0
      h_offset(2) = 0
      h_offset(3) = time_index
	  subset_dims(1) = nres
	  subset_dims(2) = nconc
  	  subset_dims(3) = 1
  	  mdata_dims(1) = nres
	  mdata_dims(2) = nconc
	  res_rank = 2
      call H5Screate_simple_f(res_rank,     &
                              mdata_dims,   &
                              memspace_id,  &
                              error); 
      call h5dget_space_f (hdf_file.res_conc_id,  &
                           fspace_id,             &
                           error)
      call h5sselect_hyperslab_f(fspace_id,         &
                                 H5S_SELECT_SET_F,  &
                                 h_offset,          &
                                 subset_dims,       &
                                 error) 
      call h5dwrite_f(hdf_file.res_conc_id,         &
                      H5T_NATIVE_REAL,              &
                      res_conc,                     &
                      mdata_dims,                   &
                      error,                        &
                      memspace_id,                  &
                      fspace_id)
	  call VerifyHDF5(error,"Reservoir concentration write")
      call h5sclose_f (fspace_id, error)
      call h5sclose_f (memspace_id, error)      
      end if
      return
      end subroutine

!=====================================================================
    !> Close out the HDF5 file properly, leaves HDF5 API open
    subroutine close_qual_hdf(hdf_file)
  	  use HDF5
	  use io_units, only: unit_error,unit_screen
	  implicit none
      type(gtm_hdf_t) :: hdf_file
	  integer        :: error	! HDF5 Error flag
      call h5close_f(error)
      return
     !-------Close the datasets corresponding to model states
      if (print_level .gt.2) write(unit_screen,*) "Closing HDF5 data sets"

	  call h5dclose_f(hdf_file.chan_conc_id,error)
	  if (error .ne. 0) then
	      write(unit_error,*)"HDF5 error closing channel stage data set: ",error
	  end if

	  call h5dclose_f(hdf_file.chan_ave_conc_id,error)
	  if (error .ne. 0) then
	      write(unit_error,*)"HDF5 error closing channel avg area data set: ",error
	  end if

	  call h5dclose_f(hdf_file.res_conc_id,error)
	  if (error .ne. 0) then
	      write(unit_error,*)"HDF5 error closing channel flow data set: ",error
	  end if

      !-----Close the groups in the dataset. Only the data group should be open
      !     on an ongoing basis  
	  call h5gclose_f(hdf_file.data_id, error)
	  if (error .ne. 0) then
	      write(unit_error,*)"HDF5 error closing data group: ",error
	  end if

      !-------Close the file
 333  if (print_level .gt.1) write(unit_screen,*)"Closing HDF5 file"
 	  call h5fclose_f(hdf_file.file_id, error)
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

!=========================================================
      
    !> Consolidates the GTM state in the form used in hdf5 file
    subroutine GtmStateForHDF5(chan_conc,chan_ave_conc,res_conc,nchan,nres,nconc)
      use common_gtm
      implicit none
            
      !include 'param.inc'
      !include 'bltm1.inc'
      
      integer :: nchan,nres,nconc
      real*4 chan_conc(2,nchan,nconc)
      real*4 chan_ave_conc(nchan,nconc)
      real*4 res_conc(nres,nconc)
      integer ichan,ires
      integer iconc,iparcel
      real*8 mass,tvol,vol

      do iconc = 1,nconc
          do ichan = 1,nchan
              chan_conc(1,ichan,iconc) = gpt(iconc,1,ichan)
              chan_conc(2,ichan,iconc) = gpt(iconc,ns(ichan),ichan)
              mass = 0.d0
              tvol = 0.d0
              do iparcel = 1,ns(ichan)
                 vol = gpv(ichan,iparcel)
                 tvol = tvol + vol
                 mass = mass + vol*gpt(iconc,iparcel,ichan)
              end do
              chan_ave_conc(ichan,iconc) = sngl(mass/tvol)
          end do
      end do
      
      do iconc = 1,nconc
          do ires = 1,nres
              res_conc(ires,iconc) = cres(ires,iconc)
          end do
      end do
      return
    end subroutine
      
!====================================================================
!     Query if it is time to write Qual state to HDF5
      
    logical function IsQualHDFWriteInterval(julmin)
      implicit none
      integer julmin

      IsQualHDFWriteInterval = ( julmin .ge. qual_hdf.start_julmin .and.   &
                                 mod(julmin, qual_hdf.write_interval) .eq. 0)
      return
    end function
        
!====================================================================
!     Write GTM state to HDF5
!     This routine includes most coupling between gtm and tidefile code
    subroutine WriteGtmHDF(julmin)
        use io_units
        use common_gtm
        implicit none
            
        !include 'param.inc'
        !include 'bltm1.inc'
            
        integer, intent(in) :: julmin
        integer time_index
        real*4 chan_conc(2,nchans,neq)
        real*4 chan_ave_conc(nchans,neq)
        real*4 res_conc(nreser,neq)
        if (.not. IsQualHDFWriteInterval(julmin)) then
            return
        end if
        time_index = (julmin - qual_hdf.start_julmin)/qual_hdf.write_interval
        call QualStateForHDF5(chan_conc,chan_ave_conc,res_conc,nchans,nreser,neq)
        call WriteQualHDF5(gtm_hdf,           &
                           chan_conc,         &
                           chan_ave_conc,     &
                           res_conc,          &
                           nchans,            &
                           nreser,            &
                           neq,               &
                           time_index)
        return
    end subroutine
      
end module