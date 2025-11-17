!<license>
!    Copyright (C) 2017 State of California,
!    Department of Water Resources.
!    This file is part of DSM2-GTM.
!
!    The Delta Simulation Model 2 (DSM2) - General Transport Model (GTM)
!    is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    DSM2 is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
!</license>
!> Module to write GTM simulation results to HDF5 file.
!> This module was mainly borrowed from DSM2-Qual with several
!> modifications to make it stand-alone, testable and in
!> fortran 90 style.
!>@ingroup process_io
module gtm_hdf_ts_write

    use hdf5, only: HID_T, HSIZE_T
    use constants
    use error_handling

    type gtm_hdf_t
        character*128 file_name
        real(gtm_real) :: write_interval
        real(gtm_real) :: start_julmin
        real(gtm_real) :: time_index
        integer(HID_T) :: file_id
        integer(HID_T) :: qual_id
        integer(HID_T) :: data_id
        integer(HID_T) :: cell_conc_id
        integer(HID_T) :: chan_conc_id
        integer(HID_T) :: resv_conc_id
        integer(HID_T) :: chan_budget_id
        integer(HID_T) :: cell_flow_id
        integer(HID_T) :: cell_area_id
        integer(HID_T) :: cell_cfl_id
        integer(HSIZE_T) :: conc_dim
        integer(HSIZE_T) :: cell_dim
        integer(HSIZE_T) :: chan_dim
        integer(HSIZE_T) :: resv_dim
        integer(HSIZE_T) :: time_dim
    end type
    type(gtm_hdf_t) :: gtm_hdf

    logical :: using_gtm_hdf = .false.
    integer, parameter :: HDF_SZIP_PIXELS_PER_BLOCK = 16
    integer, parameter :: TIME_CHUNK = HDF_SZIP_PIXELS_PER_BLOCK
    integer, parameter :: MIN_STEPS_FOR_CHUNKING = TIME_CHUNK

    contains

    !< Initialize the qual output file, assume HDF5 interface is opne
    subroutine init_gtm_hdf(hdf_file,          &
                            hdf_name,          &
                            ncell,             &
                            nchan,             &
                            nresv,             &
                            nconc,             &
                            sim_start,         &
                            sim_end,           &
                            hdf_interval_char, &
                            cal_budget)

   	    use hdf5		! HDF5 This module contains all necessary modules
        use gtm_vars, only: unit_error, unit_screen, gtm_time_interval, debug_print
        use dsm2_time_utils, only: incr_intvl
        use common_gtm_vars, only: NEAREST_BOUNDARY, TO_BOUNDARY, print_level, hdf_out
  	    implicit none
        type(gtm_hdf_t), intent(inout) :: hdf_file    !< persistent info about file and datasets
        character*128, intent(in) :: hdf_name         !< name of gtm hdf5 file
        integer, intent(in) :: ncell                  !< number of cells
        integer, intent(in) :: nchan                  !< number of channels
        integer, intent(in) :: nresv                  !< number of reservoirs
        integer, intent(in) :: nconc                  !< number of constituents
        integer, intent(in) :: sim_start              !< first write time
        integer, intent(in) :: sim_end                !< last write time
        logical, intent(in) :: cal_budget             !< calculate budget if true
        character*16 :: hdf_interval_char             !< interval

        !----- locals
        integer :: hdf_start
        integer :: hdf_end
        integer :: hdf_interval
        integer :: time_intv
        integer :: ntime                            ! number of time points in hdf5 file
        real(gtm_real) :: time_step                 ! gtm simulation time step
  	    integer(HID_T) :: access_plist              ! Dataset trasfer property
	    integer(SIZE_T) :: rddc_nelmts
	    integer(SIZE_T) :: rddc_nbytes
	    integer :: nelmts
	    real :: rdcc_w0
        logical :: h5_file_exists
	    integer :: error	                        ! HDF5 Error flag

        time_step = gtm_time_interval

	    ! check to see if file exists
     	inquire (file=hdf_name, exist=h5_file_exists)

    	if (h5_file_exists) then
	         write(unit_error,920) trim(hdf_name)
 920         format(' File already exists... deleting existing file :: ', a )
	    endif

        hdf_file%file_name = hdf_name
	    ! set up stdio to allow for buffered read/write
	    call H5Pcreate_f(H5P_FILE_ACCESS_F, access_plist, error)

	    call h5pget_cache_f(access_plist, nelmts,   &
                            rddc_nelmts, rddc_nbytes, rdcc_w0,error)
  	    rddc_nbytes = 8000000
	    call h5pset_cache_f(access_plist, nelmts, rddc_nelmts,    &
                            rddc_nbytes, rdcc_w0,error)
	    call verify_error(error, "Cache set")

	    ! start up garbage collection
	    !call h5pset_gc_references_f(access_plist_id,1,error)

        ! Create the HDF5 file
	    if (print_level .gt. 1) then
	         write(unit_screen,*) "Creating new HDF5 file"
	    end if
	    call h5fcreate_f(hdf_name, H5F_ACC_TRUNC_F, hdf_file%file_id, error, &
                         H5P_DEFAULT_F, access_plist)

        ! create group for output
	    call h5gcreate_f(hdf_file%file_id, "output", hdf_file%data_id, error)

	    call incr_intvl(hdf_interval, 0,hdf_interval_char,TO_BOUNDARY)
	    gtm_hdf%write_interval = hdf_interval
	    if (hdf_interval < time_step) then
	        write(unit_error,*) "HDF write interval is finer than the simulation time step"
	        call exit(-3)
	    end if

	    ! This would be more complex if time averages were stored
        call incr_intvl(hdf_start, sim_start, hdf_interval_char, NEAREST_BOUNDARY)
        gtm_hdf%start_julmin = hdf_start

        ! todo: is this "1+" always right? It wasn't in the original code
        ! record the dimensions of the simulation
	    ntime = 1+(sim_end - hdf_start)/hdf_interval
        hdf_end = hdf_start + (ntime-1)*hdf_interval

        hdf_file%cell_dim = ncell
        hdf_file%chan_dim = nchan
        hdf_file%conc_dim = nconc
        hdf_file%time_dim = ntime
	    hdf_file%resv_dim = nresv
	    hdf_file%time_index = 1

	    call write_dimensions(hdf_file%data_id, nchan, nresv, nconc)

	    ! create the data sets for time-varying output
	    if (trim(hdf_out).eq.'channel') then
	        call init_chan_gtm_hdf5(hdf_file, nchan, nconc, ntime, cal_budget)
	    else
	        call init_cell_gtm_hdf5(hdf_file, ncell, nconc, ntime)
	    endif
	    if (hdf_file%resv_dim .gt. 0)then
	        call init_reservoir_gtm_hdf5(hdf_file, nresv, nconc, ntime)
	    end if
	    ! for debug print only
	    if (debug_print .eq. .true.) then
	        call init_cell_gtm_hdf5_debug(hdf_file, ncell, ntime)
	    end if

        using_gtm_hdf = .true.

	    return
	end subroutine


    !> Write out lookup information for cells, reservoirs, and constituents.
    subroutine write_dimensions(loc_id, nchan, nresv, nconc)
        use hdf5
        use gtm_vars, only: chan_geom, resv_geom, constituents, hdf_out
        implicit none
        integer (HID_T), intent(in) :: loc_id              !< hdf file data ID
        integer, intent(in) :: nchan                       !< nbumber of channels
        integer, intent(in) :: nresv                       !< number of reservoirs
        integer, intent(in) :: nconc                       !< number of constituents
        integer(HSIZE_T), dimension(1) :: in_dims != (/0/) ! Dataset dimensions
        integer(HID_T) :: in_dspace_id                     ! Dataspace identifier
        integer(HID_T) :: in_dset_id                       ! Dataspace identifier
        integer     ::    in_rank = 1                      ! Dataset rank
        integer(HID_T) :: cparms                           ! dataset creatation property identifier
        integer(HID_T) :: cg_dspace_id                     ! Dataspace identifier
        integer     ::    cg_rank = 2                      ! Dataset rank
        integer(HSIZE_T), dimension(7) :: cg_data_dims
        integer(HID_T) :: memspace                         ! memspace identifier

        integer, parameter :: label_len = 12
	    integer, parameter :: name_len = 32
        character(LEN=name_len),dimension(:), allocatable :: names
        real(gtm_real) :: tmp
        integer :: i
        integer :: ierror

        ! Write channel up/down labels
        if (hdf_out .eq. 'channel') then
            in_dims(1) = 2
            allocate(names(2))
            names(1) = 'upstream'
            names(2) = 'downstream'
            call write_1D_string_array(loc_id,"channel_location", names,  &
                                       name_len,2)
            deallocate(names)

            ! Write out external channel numbers int2ext
            call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, ierror)
            in_dims(1) = nchan
            call h5screate_simple_f(in_rank, in_dims, in_dspace_id, ierror)
            call h5dcreate_f(loc_id, "channel_number", H5T_NATIVE_INTEGER,   &
                             in_dspace_id, in_dset_id, ierror, cparms)
            call h5dwrite_f(in_dset_id,H5T_NATIVE_INTEGER, chan_geom%channel_num, in_dims, ierror)
            call h5sclose_f (in_dspace_id, ierror)
            call h5dclose_f(in_dset_id,ierror)
        end if

  	    ! Write reservoir names
  	    if (nresv.gt.0) then
            in_dims(1) = nresv
            allocate(names(nresv))
	        names = ' '
	        do i = 1, nresv
	            names(i) = resv_geom(i)%name
            end do
	        call write_1D_string_array(loc_id,"reservoir_names",names,    &
                                       name_len, nresv)
            deallocate(names)
        end if

        ! Write constituent names
        if (nconc.gt.0) then
  	        in_dims(1) = max(1,nconc)
            allocate(names(max(1,nconc)))
	        names = ' '
	        do i = 1,max(1,nconc)
	            names(i) = constituents(i)%name
            end do
	        call write_1D_string_array(loc_id,"constituent_names", names,  &
                                       name_len,max(1,nconc))
            deallocate(names)
        end if
        return
    end subroutine

    !> Initialize qual tide file for cell time series (ncell)
	subroutine init_cell_gtm_hdf5_debug(hdf_file, ncell, ntime)

	    use hdf5
	    use time_utilities, only: jmin2cdt
	    implicit none

        type(gtm_hdf_t), intent(inout) :: hdf_file       !< hdf file properties
	    integer, intent(in) :: ntime                      !< number of time steps
	    integer, intent(in) :: ncell                      !< number of cells
   	    integer(HID_T) :: attr_id                         ! Attribute identifier
        integer(HID_T) :: aspace_id                       ! Attribute dataspace identifier
        integer(HID_T) :: atype_id                        ! Attribute type identifier
        integer(HSIZE_T), dimension(1) :: adims = (/1/)   ! Attribute dimension
        integer     ::   arank = 1                        ! Attribute rank
        integer(HSIZE_T), dimension(7) :: a_data_dims
        integer     ::   cell_rank = 0
   	    integer(HSIZE_T), dimension(2) :: cell_file_dims  = 0 ! Data size on file
	    integer(HSIZE_T), dimension(2) :: cell_chunk_dims = 0 ! Dataset chunk dimensions

	    integer(HID_T) :: fspace_flow_id                  ! File space identifier
	    integer(HID_T) :: fspace_area_id                  ! File space identifier
	    integer(HID_T) :: fspace_cfl_id                   ! File space identifier

	    integer(HID_T) :: cparms                          ! dataset creatation property identifier
	    integer        :: error                           ! HDF5 Error flag

	    call h5screate_simple_f(arank, adims, aspace_id, error)
	    call h5tcopy_f(H5T_NATIVE_INTEGER, atype_id, error)

		! Create cell data set, one data point per cell
        cell_rank = 2
        cell_file_dims(1) = ncell
	    cell_file_dims(2) = ntime

	    cell_chunk_dims(1) = ncell
	    cell_chunk_dims(2) = min(TIME_CHUNK,ntime)

	    cparms = 0
		! Add chunking and compression
	    call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, error)
        if (ntime.gt. MIN_STEPS_FOR_CHUNKING) then
          call h5pset_chunk_f(cparms,                   &
                              cell_rank,                &
                              cell_chunk_dims,          &
                              error)
	      call H5Pset_szip_f (cparms, H5_SZIP_NN_OM_F,  &
                              HDF_SZIP_PIXELS_PER_BLOCK, error);
	    end if

        ! initialize table for cell flow
	    call h5screate_simple_f(cell_rank,              &
                                cell_file_dims,         &
                                fspace_flow_id,         &
                                error)
	    call h5dcreate_f(hdf_file%data_id,              &
                         "cell flow",                   &
                         H5T_NATIVE_DOUBLE,             &
                         fspace_flow_id,                &
                         hdf_file%cell_flow_id,         &
                         error,                         &
                         cparms)
	    call verify_error(error,"Cell flow dataset creation")
        call add_timeseries_attributes(hdf_file%cell_flow_id,   &
                                       hdf_file%start_julmin,   &
                                       hdf_file%write_interval)

        ! initialize table for cell area
	    call h5screate_simple_f(cell_rank,              &
                                cell_file_dims,         &
                                fspace_area_id,         &
                                error)
	    call h5dcreate_f(hdf_file%data_id,              &
                         "cell area",                   &
                         H5T_NATIVE_DOUBLE,             &
                         fspace_area_id,                &
                         hdf_file%cell_area_id,         &
                         error,                         &
                         cparms)
	    call verify_error(error,"Cell area dataset creation")
        call add_timeseries_attributes(hdf_file%cell_area_id,   &
                                       hdf_file%start_julmin,   &
                                       hdf_file%write_interval)

        ! initialize table for cell cfl number
	    call h5screate_simple_f(cell_rank,              &
                                cell_file_dims,         &
                                fspace_cfl_id,          &
                                error)
	    call h5dcreate_f(hdf_file%data_id,              &
                         "cell cfl",                    &
                         H5T_NATIVE_DOUBLE,             &
                         fspace_cfl_id,                 &
                         hdf_file%cell_cfl_id,          &
                         error,                         &
                         cparms)
	    call verify_error(error,"Cell CFL dataset creation")
        call add_timeseries_attributes(hdf_file%cell_cfl_id,    &
                                       hdf_file%start_julmin,   &
                                       hdf_file%write_interval)
	    return
	end subroutine


    !> Initialize qual tide file for cell time series (ncell, nvar)
	subroutine init_cell_gtm_hdf5(hdf_file, ncell, nconc, ntime)

	    use hdf5
	    use time_utilities, only: jmin2cdt
	    implicit none

        type(gtm_hdf_t), intent(inout) :: hdf_file       !< hdf file properties
	    integer, intent(in) :: ntime                      !< number of time steps
	    integer, intent(in) :: ncell                      !< number of cells
        integer, intent(in) :: nconc	                  !< number of constituents
   	    integer(HID_T) :: attr_id                         ! Attribute identifier
        integer(HID_T) :: aspace_id                       ! Attribute dataspace identifier
        integer(HID_T) :: atype_id                        ! Attribute type identifier
        integer(HSIZE_T), dimension(1) :: adims = (/1/)   ! Attribute dimension
        integer     ::   arank = 1                        ! Attribute rank
        integer(HSIZE_T), dimension(7) :: a_data_dims
        integer     ::   cell_rank = 0
   	    integer(HSIZE_T), dimension(3) :: cell_file_dims  = 0 ! Data size on file
	    integer(HSIZE_T), dimension(3) :: cell_chunk_dims = 0 ! Dataset chunk dimensions

	    integer(HID_T) :: fspace_id                       ! File space identifier
	    integer(HID_T) :: cparms                          ! dataset creatation property identifier
	    integer        :: error                           ! HDF5 Error flag

	    call h5screate_simple_f(arank, adims, aspace_id, error)
	    call h5tcopy_f(H5T_NATIVE_INTEGER, atype_id, error)

		! Create cell data set, one data point per cell
        cell_rank = 3
        cell_file_dims(1) = ncell
        cell_file_dims(2) = nconc
	    cell_file_dims(3) = ntime

        cell_chunk_dims(1) = ncell
	    cell_chunk_dims(2) = nconc
	    cell_chunk_dims(3) = min(TIME_CHUNK,ntime)

	    cparms = 0
		! Add chunking and compression
	    call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, error)
        if (ntime.gt. MIN_STEPS_FOR_CHUNKING) then
          call h5pset_chunk_f(cparms,                   &
                              cell_rank,                &
                              cell_chunk_dims,          &
                              error)
	      call H5Pset_szip_f (cparms, H5_SZIP_NN_OM_F,  &
                              HDF_SZIP_PIXELS_PER_BLOCK, error);
	    end if

        ! initialize table for cell concentration
	    call h5screate_simple_f(cell_rank,              &
                                cell_file_dims,         &
                                fspace_id,              &
                                error)
	    call h5dcreate_f(hdf_file%data_id,              &
                         "cell concentration",          &
                         H5T_NATIVE_DOUBLE,             &
                         fspace_id,                     &
                         hdf_file%cell_conc_id,         &
                         error,                         &
                         cparms)
	    call verify_error(error,"Cell concentration dataset creation")
        call add_timeseries_attributes(hdf_file%cell_conc_id,   &
                                       hdf_file%start_julmin,   &
                                       hdf_file%write_interval)
	    return
	end subroutine


    !> Initialize qual tide file for channel time series (nchan, nvar)
	subroutine init_chan_gtm_hdf5(hdf_file, nchan, nconc, ntime, budget)

	    use hdf5
	    use time_utilities, only: jmin2cdt
	    implicit none

        type(gtm_hdf_t), intent(inout) :: hdf_file        !< hdf file properties
	    integer, intent(in) :: ntime                      !< number of time steps
	    integer, intent(in) :: nchan                      !< number of channels
        integer, intent(in) :: nconc	                  !< number of constituents
        logical, intent(in) :: budget                     !< calculate budget if true
   	    integer(HID_T) :: attr_id                         ! Attribute identifier
        integer(HID_T) :: aspace_id                       ! Attribute dataspace identifier
        integer(HID_T) :: atype_id                        ! Attribute type identifier
        integer(HSIZE_T), dimension(1) :: adims = (/1/)   ! Attribute dimension
        integer     ::   arank = 1                        ! Attribute rank
        integer(HSIZE_T), dimension(7) :: a_data_dims
        integer     ::   chan_rank = 0
   	    integer(HSIZE_T), dimension(4) :: chan_file_dims  = 0 ! Data size on file
	    integer(HSIZE_T), dimension(4) :: chan_chunk_dims = 0 ! Dataset chunk dimensions
	    integer(HID_T) :: fspace_id                       ! File space identifier
	    integer(HID_T) :: cparms                          ! dataset creatation property identifier

   	    integer(HID_T) :: attr_budget_id                         ! Attribute identifier
        integer(HID_T) :: aspace_budget_id                       ! Attribute dataspace identifier
        integer(HID_T) :: atype_budget_id                        ! Attribute type identifier
        integer(HSIZE_T), dimension(1) :: adims_budget = (/1/)   ! Attribute dimension
        integer     ::   arank_budget = 1                        ! Attribute rank
        integer     ::   chan_budget_rank = 0
   	    integer(HSIZE_T), dimension(3) :: chan_budget_file_dims  = 0 ! Data size on file
	    integer(HSIZE_T), dimension(3) :: chan_budget_chunk_dims = 0 ! Dataset chunk dimensions
	    integer(HID_T) :: fspace_budget_id                       ! File space identifier
	    integer(HID_T) :: cparms_budget                          ! dataset creatation property identifier

	    integer        :: error                                  ! HDF5 Error flag

	    call h5screate_simple_f(arank, adims, aspace_id, error)
	    call h5tcopy_f(H5T_NATIVE_INTEGER, atype_id, error)

		! Create cell data set, one data point per cell
        chan_rank = 4
        chan_file_dims(1) = 2
        chan_file_dims(2) = nchan
	    chan_file_dims(3) = nconc
	    chan_file_dims(4) = ntime

        chan_chunk_dims(1) = 2
	    chan_chunk_dims(2) = nchan
	    chan_chunk_dims(3) = nconc
	    chan_chunk_dims(4) = min(TIME_CHUNK,ntime)

	    cparms = 0
		! Add chunking and compression
	    call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, error)
        if (ntime.gt. MIN_STEPS_FOR_CHUNKING) then
          call h5pset_chunk_f(cparms,                   &
                              chan_rank,                &
                              chan_chunk_dims,          &
                              error)
	      call H5Pset_szip_f (cparms, H5_SZIP_NN_OM_F,  &
                              HDF_SZIP_PIXELS_PER_BLOCK, error);
	    end if

        ! initialize table for channel concentration
	    call h5screate_simple_f(chan_rank,              &
                                chan_file_dims,         &
                                fspace_id,              &
                                error)
	    call h5dcreate_f(hdf_file%data_id,              &
                         "channel concentration",       &
                         H5T_NATIVE_REAL,               &
                         fspace_id,                     &
                         hdf_file%chan_conc_id,         &
                         error,                         &
                         cparms)
	    call verify_error(error,"Channel concentration dataset creation")
        call add_timeseries_attributes(hdf_file%chan_conc_id,   &
                                       hdf_file%start_julmin,   &
                                       hdf_file%write_interval)

        if (budget) then
	        call h5screate_simple_f(arank_budget, adims_budget, aspace_budget_id, error)
	        call h5tcopy_f(H5T_NATIVE_INTEGER, atype_budget_id, error)

		    ! Create cell data set, one data point per cell
            chan_budget_rank = 3
            chan_budget_file_dims(1) = nchan
    	    chan_budget_file_dims(2) = nconc
	        chan_budget_file_dims(3) = ntime

	        chan_budget_chunk_dims(1) = nchan
	        chan_budget_chunk_dims(2) = nconc
	        chan_budget_chunk_dims(3) = min(TIME_CHUNK,ntime)

	        cparms_budget = 0
		    ! Add chunking and compression
	        call h5pcreate_f(H5P_DATASET_CREATE_F, cparms_budget, error)
            if (ntime.gt. MIN_STEPS_FOR_CHUNKING) then
              call h5pset_chunk_f(cparms_budget,                    &
                                  chan_budget_rank,                 &
                                  chan_budget_chunk_dims,           &
                                  error)
	          call H5Pset_szip_f (cparms_budget, H5_SZIP_NN_OM_F,   &
                                  HDF_SZIP_PIXELS_PER_BLOCK, error)
	        end if

            ! initialize table for channel budget
	        call h5screate_simple_f(chan_budget_rank,               &
                                    chan_budget_file_dims,          &
                                    fspace_budget_id,               &
                                    error)
	        call h5dcreate_f(hdf_file%data_id,                      &
                             "channel budget",                      &
                             H5T_NATIVE_REAL,                       &
                             fspace_budget_id,                      &
                             hdf_file%chan_budget_id,               &
                             error,                                 &
                             cparms_budget)
	        call verify_error(error,"Channel budget dataset creation")
            call add_timeseries_attributes(hdf_file%chan_budget_id,   &
                                           hdf_file%start_julmin,     &
                                           hdf_file%write_interval)
        end if
	    return
	end subroutine


	!> Initialize qual tide file for reservoir time series
	subroutine init_reservoir_gtm_hdf5(hdf_file, nresv, nconc, ntime)
	    use hdf5
        implicit none
        type(gtm_hdf_t), intent(inout) :: hdf_file
        integer(HID_T) :: cparms          ! dataset creation property identifier
        integer        :: error	          ! HDF5 Error flag
        integer        :: res_rank = 3
        integer(HSIZE_T), dimension(3) :: chunk_dims = 0 ! Dataset dimensions
        integer        :: ntime           ! number of time points in tidefile
        integer        :: nresv
        integer        :: nconc
        integer(HSIZE_T), dimension(3) :: file_dims  = 0 ! Data size on file
	    integer(HID_T) :: fspace_id       ! File space identifier

        !-------Create the datasets
        res_rank = 3
	    file_dims(1) = nresv
        file_dims(2) = nconc
	    file_dims(3) = ntime

        chunk_dims(1) = nresv
	    chunk_dims(2) = nconc
	    chunk_dims(3) = min(TIME_CHUNK,ntime)

		! Add chunking and compression
	    call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, error)
	    if (ntime .gt. MIN_STEPS_FOR_CHUNKING) then
	        call h5pset_chunk_f(cparms, res_rank, chunk_dims, error)
	        call H5Pset_szip_f (cparms, H5_SZIP_NN_OM_F,           &
                                HDF_SZIP_PIXELS_PER_BLOCK, error);
        end if

	    call h5screate_simple_f(res_rank,                       &
                                file_dims,                      &
                                fspace_id,                      &
                                error)
	    call h5dcreate_f(hdf_file%data_id,                      &
                         "reservoir concentration",             &
                         H5T_NATIVE_DOUBLE,                     &
                         fspace_id,                             &
                         hdf_file%resv_conc_id,                 &
                         error,                                 &
                         cparms)
        call add_timeseries_attributes(hdf_file%resv_conc_id,   &
                                       hdf_file%start_julmin,   &
                                       hdf_file%write_interval)

        return
	end subroutine


    !> Write flow/area time series data to Qual tidefile (dimension cell)
    subroutine write_gtm_hdf_ts(data_id,       &
                                cell_ts,       &
                                ncell,         &
                                time_index)
        use hdf5
        implicit none
        integer(HID_T), intent(in) :: data_id                   !< data id
        integer, intent(in) :: ncell                            !< number of cells
        real(gtm_real), intent(in) :: cell_ts(ncell)            !< cell data from transport module
        integer, intent(in) :: time_index                       !< time index to write the data
        integer :: cell_rank
        integer(HID_T) :: fspace_id
        integer(HID_T) :: memspace_id
     	integer(HSIZE_T), dimension(1) :: mdata_dims  = 0       ! Dims of data in memory
      	integer(HSIZE_T), dimension(2) :: subset_dims  = 0      ! Dims of subset for time step
    	integer(HSIZE_T), dimension(2) :: h_offset = (/0,0/)
    	integer :: i, j
        integer :: error                                        ! HDF5 Error flag
        if (mod(time_index,24*10) .eq. 1) call h5garbage_collect_f(error)

        if (ncell .ne. 0) then
            !-----cell conc
            h_offset(1) = 0
            h_offset(2) = time_index

	        subset_dims(1) = ncell
	        subset_dims(2) = 1

	        mdata_dims(1) = ncell
            cell_rank =1
            call H5Screate_simple_f(cell_rank,           &
                                    mdata_dims,          &
                                    memspace_id,         &
                                    error);
            call h5dget_space_f (data_id,                &
                                 fspace_id,              &
                                 error)
            call h5sselect_hyperslab_f(fspace_id,        &
                                       H5S_SELECT_SET_F, &
                                       h_offset,         &
                                       subset_dims,      &
                                       error)
            call h5dwrite_f(data_id,                     &
                            H5T_NATIVE_DOUBLE,           &   ! This was H5T_NATIVE_REAL in old DSM2-Qual. Leaving it as REAL will introduce errors.
                            cell_ts,                     &
                            mdata_dims,                  &
                            error,                       &
                            memspace_id,                 &
                            fspace_id)
	        call verify_error(error,"Cell time series write (write_gtm_hdf_ts)")
            call h5sclose_f (fspace_id, error)
            call h5sclose_f (memspace_id, error)
        end if
        return
    end subroutine


    !> Write time series data to Qual tidefile (dimension cell)
    subroutine write_gtm_hdf(hdf_file,      &
                             cell_conc,     &
                             ncell,         &
                             nconc,         &
                             time_index)
        use hdf5
        implicit none
        type(gtm_hdf_t), intent(in) :: hdf_file                !< hdf file structure
        integer, intent(in) :: ncell                            !< number of cells
        integer, intent(in) :: nconc                            !< number of constituents
        real(gtm_real), intent(in) :: cell_conc(ncell, nconc)   !< cell data from transport module
        integer, intent(in) :: time_index                       !< time index to write the data
        integer :: cell_rank
        integer(HID_T) :: fspace_id
        integer(HID_T) :: memspace_id
     	integer(HSIZE_T), dimension(2) :: mdata_dims  = 0       ! Dims of data in memory
      	integer(HSIZE_T), dimension(3) :: subset_dims  = 0      ! Dims of subset for time step
    	integer(HSIZE_T), dimension(3) :: h_offset = (/0,0,0/)
    	integer :: i, j
        integer :: error                                        ! HDF5 Error flag
        if (mod(time_index,24*10) .eq. 1) call h5garbage_collect_f(error)

        if (ncell .ne. 0) then
            !-----cell conc
            h_offset(1) = 0
            h_offset(2) = 0
            h_offset(3) = time_index

	        subset_dims(1) = ncell
	        subset_dims(2) = nconc
	        subset_dims(3) = 1

	        mdata_dims(1) = ncell
	        mdata_dims(2) = nconc
            cell_rank = 2
            call H5Screate_simple_f(cell_rank,           &
                                    mdata_dims,          &
                                    memspace_id,         &
                                    error);
            call h5dget_space_f(hdf_file%cell_conc_id,   &
                                fspace_id,               &
                                error)
            call h5sselect_hyperslab_f(fspace_id,        &
                                       H5S_SELECT_SET_F, &
                                       h_offset,         &
                                       subset_dims,      &
                                       error)
            call h5dwrite_f(hdf_file%cell_conc_id,       &
                            H5T_NATIVE_DOUBLE,           &   ! This was H5T_NATIVE_REAL in old DSM2-Qual. Leaving it as REAL will introduce errors.
                            cell_conc,                   &
                            mdata_dims,                  &
                            error,                       &
                            memspace_id,                 &
                            fspace_id)
	        call verify_error(error,"Cell concentration write")
            call h5sclose_f(fspace_id, error)
            call h5sclose_f(memspace_id, error)
        end if
        return
    end subroutine


    !> Write time series data to Qual tidefile (dimension channel)
    subroutine write_gtm_chan_hdf(hdf_file,      &
                                  flow_lo,       &
                                  flow_hi,       &
                                  conc,          &
                                  nchan,         &
                                  ncell,         &
                                  nconc,         &
                                  out_hdf_dt,    &
                                  prev_flow_lo,  &
                                  prev_flow_hi,  &
                                  prev_conc,     &
                                  time_index,    &
                                  budget)
        use hdf5
        use gtm_vars, only: hdf_out, chan_geom
        implicit none
        real(gtm_real), parameter :: hydro_theta = 0.6d0           !< hydro theta value
        type(gtm_hdf_t), intent(in) :: hdf_file                    !< hdf file structure
        integer, intent(in) :: nchan                               !< number of channels
        integer, intent(in) :: ncell                               !< number of cells
        integer, intent(in) :: nconc                               !< number of constituents
        real(gtm_real), intent(in) :: flow_lo(ncell)               !< flow_lo
        real(gtm_real), intent(in) :: flow_hi(ncell)               !< flow_hi
        real(gtm_real), intent(in) :: conc(ncell, nconc)           !< cell data from transport module
        real(gtm_real), intent(in) :: out_hdf_dt                   !< output time interval for GTM hdf5 file
        real(gtm_real), intent(in) :: prev_flow_lo(ncell)          !< flow at low face from previous time step
        real(gtm_real), intent(in) :: prev_flow_hi(ncell)          !< flow at high face from previous time step
        real(gtm_real), intent(in) :: prev_conc(ncell, nconc)      !< concentration from previous time step
        real(gtm_real) :: chan_conc(2, nchan, nconc)               !< channel data from transport module
        real(gtm_real) :: prev_chan_conc(2, nchan, nconc)          !< channel data from transport module
        real(gtm_real) :: chan_budget(nchan, nconc)                !< channel budget
        integer, intent(in) :: time_index                          !< time index to write the data
        integer :: chan_rank
        integer :: chan_budget_rank
        integer(HID_T) :: fspace_id
        integer(HID_T) :: memspace_id
     	integer(HSIZE_T), dimension(3) :: mdata_dims  = 0          ! Dims of data in memory
      	integer(HSIZE_T), dimension(4) :: subset_dims  = 0         ! Dims of subset for time step
    	integer(HSIZE_T), dimension(4) :: h_offset = (/0,0,0,0/)
        integer(HID_T) :: fspace_budget_id
        integer(HID_T) :: memspace_budget_id
     	integer(HSIZE_T), dimension(2) :: mdata_budget_dims  = 0   ! Dims of data in memory
      	integer(HSIZE_T), dimension(3) :: subset_budget_dims  = 0  ! Dims of subset for time step
    	integer(HSIZE_T), dimension(3) :: h_budget_offset = (/0,0,0/)
    	integer :: ichan
        integer :: error                                           ! HDF5 Error flag
        logical, intent(in) :: budget                              !< print out the budget

        if (mod(time_index,24*10) .eq. 1) call h5garbage_collect_f(error)

        if (nchan .ne. 0) then
            !-----channel conc
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

            do ichan = 1, nchan
                chan_conc(1,ichan,:) = conc(chan_geom(ichan)%start_cell,:)-(conc(chan_geom(ichan)%end_cell,:)-conc(chan_geom(ichan)%start_cell,:)) &
                                       /(chan_geom(ichan)%end_cell-chan_geom(ichan)%start_cell)*half
                chan_conc(2,ichan,:) = conc(chan_geom(ichan)%end_cell,:)+(conc(chan_geom(ichan)%end_cell,:)-conc(chan_geom(ichan)%start_cell,:)) &
                                       /(chan_geom(ichan)%end_cell-chan_geom(ichan)%start_cell)*half
                where (chan_conc(1,ichan,:)<zero) chan_conc(1,ichan,:) = zero
                where (chan_conc(2,ichan,:)<zero) chan_conc(2,ichan,:) = zero
            end do

            call H5Screate_simple_f(chan_rank,           &
                                    mdata_dims,          &
                                    memspace_id,         &
                                    error)
            call h5dget_space_f(hdf_file%chan_conc_id,   &
                                fspace_id,               &
                                error)
            call h5sselect_hyperslab_f(fspace_id,        &
                                       H5S_SELECT_SET_F, &
                                       h_offset,         &
                                       subset_dims,      &
                                       error)
            call h5dwrite_f(hdf_file%chan_conc_id,       &
                            H5T_NATIVE_REAL,             &
                            real(chan_conc),             &
                            mdata_dims,                  &
                            error,                       &
                            memspace_id,                 &
                            fspace_id)
	        call verify_error(error,"Channel concentration write")
            call h5sclose_f(fspace_id, error)
            call h5sclose_f(memspace_id, error)

            if (budget) then

                do ichan = 1, nchan
                    prev_chan_conc(1,ichan,:) = prev_conc(chan_geom(ichan)%start_cell,:)-(prev_conc(chan_geom(ichan)%end_cell,:)-prev_conc(chan_geom(ichan)%start_cell,:)) &
                                           /(chan_geom(ichan)%end_cell-chan_geom(ichan)%start_cell)*half
                    prev_chan_conc(2,ichan,:) = prev_conc(chan_geom(ichan)%end_cell,:)+(prev_conc(chan_geom(ichan)%end_cell,:)-prev_conc(chan_geom(ichan)%start_cell,:)) &
                                           /(chan_geom(ichan)%end_cell-chan_geom(ichan)%start_cell)*half
                    where (prev_chan_conc(1,ichan,:)<zero) prev_chan_conc(1,ichan,:) = zero
                    where (prev_chan_conc(2,ichan,:)<zero) prev_chan_conc(2,ichan,:) = zero
                end do

                do ichan = 1, nchan
                    chan_budget(ichan,:) = hydro_theta * (flow_lo(chan_geom(ichan)%start_cell)*chan_conc(1,ichan,:) -                 &
                                           flow_hi(chan_geom(ichan)%end_cell)*chan_conc(2,ichan,:))*out_hdf_dt*sixty +                &
                                           (one-hydro_theta) * (prev_flow_lo(chan_geom(ichan)%start_cell)*prev_chan_conc(1,ichan,:) - &
                                           prev_flow_hi(chan_geom(ichan)%end_cell)*prev_chan_conc(2,ichan,:))*out_hdf_dt*sixty
                enddo

                h_budget_offset(1) = 0
                h_budget_offset(2) = 0
                h_budget_offset(3) = time_index

	            subset_budget_dims(1) = nchan
	            subset_budget_dims(2) = nconc
	            subset_budget_dims(3) = 1

	            mdata_budget_dims(1) = nchan
	            mdata_budget_dims(2) = nconc
                chan_budget_rank = 2

                call H5Screate_simple_f(chan_budget_rank,     &
                                        mdata_budget_dims,    &
                                        memspace_budget_id,   &
                                        error)
                call h5dget_space_f(hdf_file%chan_budget_id,  &
                                    fspace_budget_id,         &
                                    error)
                call h5sselect_hyperslab_f(fspace_budget_id,  &
                                           H5S_SELECT_SET_F,  &
                                           h_budget_offset,   &
                                           subset_budget_dims,&
                                           error)
                call h5dwrite_f(hdf_file%chan_budget_id,      &
                                H5T_NATIVE_REAL,              &
                                real(chan_budget),            &
                                mdata_budget_dims,            &
                                error,                        &
                                memspace_budget_id,           &
                                fspace_budget_id)
	            call verify_error(error,"Channel budget write")
                call h5sclose_f(fspace_budget_id, error)
                call h5sclose_f(memspace_budget_id, error)
            end if
        end if
        return
    end subroutine


    !> Write time series data to Qual tidefile (dimension reservoir)
    subroutine write_gtm_hdf_resv(hdf_file,      &
                                  resv_conc,     &
                                  nresv,         &
                                  nconc,         &
                                  time_index)
        use hdf5
        implicit none
        type(gtm_hdf_t), intent(in) :: hdf_file                 !< hdf file structure
        integer, intent(in) :: nresv                            !< number of reservoirs
        integer, intent(in) :: nconc                            !< number of constituents
        real(gtm_real), intent(in) :: resv_conc(nresv, nconc)   !< resv data from transport module
        integer, intent(in) :: time_index                       !< time index to write the data
        integer :: resv_rank
        integer(HID_T) :: fspace_id
        integer(HID_T) :: memspace_id
     	integer(HSIZE_T), dimension(2) :: mdata_dims  = 0       ! Dims of data in memory
      	integer(HSIZE_T), dimension(3) :: subset_dims  = 0      ! Dims of subset for time step
    	integer(HSIZE_T), dimension(3) :: h_offset = (/0,0,0/)
    	integer :: i, j
        integer :: error                                        ! HDF5 Error flag
        if (mod(time_index,24*10) .eq. 1) call h5garbage_collect_f(error)

        if (nresv .ne. 0) then

            !-----reservoir conc
            h_offset(1) = 0
            h_offset(2) = 0
            h_offset(3) = time_index
            subset_dims(1) = nresv
	        subset_dims(2) = nconc
	        subset_dims(3) = 1
   	        mdata_dims(1) = nresv
	        mdata_dims(2) = nconc
	        resv_rank = 2
            call H5Screate_simple_f(resv_rank,           &
                                    mdata_dims,          &
                                    memspace_id,         &
                                    error);
            call h5dget_space_f(hdf_file%resv_conc_id,   &
                                fspace_id,               &
                                error)
            call h5sselect_hyperslab_f(fspace_id,        &
                                       H5S_SELECT_SET_F, &
                                       h_offset,         &
                                       subset_dims,      &
                                       error)
            call h5dwrite_f(hdf_file%resv_conc_id,       &
                            H5T_NATIVE_DOUBLE,           &
                            resv_conc,                   &
                            mdata_dims,                  &
                            error,                       &
                            memspace_id,                 &
                            fspace_id)
	        call verify_error(error,"Reservoir concentration write")
            call h5sclose_f (fspace_id, error)
            call h5sclose_f (memspace_id, error)
        end if
        return
    end subroutine


    !> Close out the HDF5 file properly, leaves HDF5 API open
    subroutine close_gtm_hdf(hdf_file)
        use hdf5
        use gtm_vars, only: unit_error,unit_screen, hdf_out
        use common_gtm_vars, only: print_level

        implicit none
        type(gtm_hdf_t) :: hdf_file

        integer        :: error	! HDF5 Error flag

        !-------Close the datasets corresponding to model states
        if (print_level .gt.2) write(unit_screen,*)"Closing HDF5 data sets"

        if (hdf_out .eq. 'channel') then
	        call h5dclose_f(hdf_file%chan_conc_id,error)
            if (error .ne. 0) then
	            write(unit_error,*)"HDF5 error closing chan conc data set: ",error
	        end if
	        if (hdf_file%chan_budget_id.ne.0) then
	            call h5dclose_f(hdf_file%chan_budget_id,error)
                if (error .ne. 0) then
	                write(unit_error,*)"HDF5 error closing chan budget data set: ",error
	            end if
	        end if
        else
	        call h5dclose_f(hdf_file%cell_conc_id,error)
            if (error .ne. 0) then
	            write(unit_error,*)"HDF5 error closing cell conc data set: ",error
	        end if
	    end if

        if (hdf_file%resv_dim.gt.0) then
	        call h5dclose_f(hdf_file%resv_conc_id,error)
	        if (error .ne. 0) then
	            write(unit_error,*)"HDF5 error closing reservoir conc data set: ",error
	        end if
	    end if

        !-----Close the groups in the dataset. Only the data group should be open
        !     on an ongoing basis
        call h5gclose_f(hdf_file%data_id, error)
        if (error .ne. 0) then
            write(unit_error,*)"HDF5 error closing data group: ",error
        end if

        !-------Close the file
 333    if (print_level .gt.1) write(unit_screen,*)"Closing HDF5 file"
	    call h5fclose_f(hdf_file%file_id, error)
        if (error .ne. 0) then
	       write(unit_error,*)"HDF5 error closing hdf file: ",error
        end if

	    return
    end subroutine


    !> Query if it is time to write Qual state to HDF5
    logical function is_gtm_hdf_write_interval(julmin)
        implicit none
        real(gtm_real), intent(in) :: julmin
        is_gtm_hdf_write_interval = ( julmin .ge. gtm_hdf.start_julmin .and.     &
                                     mod(julmin, gtm_hdf.write_interval) .eq. 0)
        return
    end function


    !> Write one dimensional string array to dataset
    subroutine write_1D_string_array(dest_id, name, arr, strlen, nstr)

        use hdf5
        implicit none
        integer(HID_T),intent(in) :: dest_id    !< Destination group identifier
        character*(*), intent(in) :: name       !< Name of array
  	    integer(HSIZE_T), intent(in)       :: strlen   !< Length of string element
	    integer(HSIZE_T), intent(in)       :: nstr     !< Dimension of string array
	    character(len=strlen),dimension(nstr), intent(in) :: arr    !< string array

        integer(HID_T) :: dspace_id     ! Dataspace identifier
        integer(HID_T) :: dtype_id      ! Dataspace identifier
        integer(HID_T) :: dset_id       ! Dataspace identifier
        integer, parameter :: drank = 1 ! Dataset rank
        integer, parameter :: arank = 1 ! Attribute rank
        integer :: error                ! HDF5 Error flag
        integer(HSIZE_T), dimension(arank) :: data_dims

	    data_dims(1) = nstr
        call h5tcopy_f(H5T_NATIVE_CHARACTER, dtype_id, error)
        call h5tset_size_f(dtype_id, strlen, error)
	    call h5screate_simple_f(drank, data_dims, dspace_id, error)
	    call h5dcreate_f(dest_id,trim(name),dtype_id, dspace_id, dset_id, error)
        call h5dwrite_f(dset_id,dtype_id, arr(1), data_dims, error)
        call h5tclose_f(dtype_id,error)
        call h5sclose_f(dspace_id,error)
        call h5dclose_f(dset_id,error)

        return
	end subroutine


    !> add time series attributes to tidefile
	subroutine add_timeseries_attributes(dset_id, ts_start, ts_interval)

	    use hdf5
        use time_utilities, only: jmin2iso
        implicit none
        real(gtm_real) :: ts_start
        real(gtm_real) :: ts_interval
        character*30 :: cinterval
        character*30 :: cinterval_f
   	    integer(HID_T) :: dset_id ! Attribute identifier
        integer(HID_T) :: aspace_id ! Attribute Dataspace identifier
	    integer(HID_T) :: atype_id
	    integer(HID_T) :: attr_id
	    integer        :: error	! HDF5 Error flag
	    integer :: nlen
        character(LEN=32) :: buffer
	    integer(HSIZE_T), dimension(1) :: a_dims = (/1/)
        integer     ::   arank = 1                      ! Attribure rank
        character*5:: dsm2_name
        character*5:: dsm2_version
	    integer*4, parameter :: ISO_LEN = 19  ! includes null termination
        character(LEN=ISO_LEN) :: iso_datetime

        dsm2_name = 'GTM'
        dsm2_version = '8.2'

        write(cinterval,"(i,'min')") int(ts_interval)
        !write(cinterval_f,"(f,'min')") ts_interval
        cinterval=adjustl(cinterval)

        call h5screate_simple_f(arank, a_dims, aspace_id, error)
        call verify_error(error,"time series attributes dataspace")
        call h5tcopy_f(H5T_NATIVE_CHARACTER, atype_id, error)
  	    buffer = 'TIMESERIES'
	    nlen = len_trim(buffer)
        call h5tset_size_f(atype_id, nlen, error)
	    call h5acreate_f(dset_id, "CLASS",                      &
                         atype_id, aspace_id, attr_id, error)
	    call h5awrite_f(attr_id, atype_id, trim(buffer),        &
                        a_dims, error)
	    call h5aclose_f(attr_id,error)

	    iso_datetime=jmin2iso(int(ts_start))
        call h5tset_size_f(atype_id, ISO_LEN, error)
	    call h5acreate_f(dset_id, "start_time",                 &
                         atype_id, aspace_id, attr_id, error)
 	    call h5awrite_f(attr_id, atype_id, iso_datetime(1:19),  &
                        a_dims, error)
	    call h5aclose_f(attr_id,error)

        nlen = 5
        call h5tset_size_f(atype_id, nlen, error)
        call h5acreate_f(dset_id, "model", atype_id, aspace_id, attr_id, error)
        call h5awrite_f(attr_id, atype_id, dsm2_name, a_dims, error)
        call h5aclose_f(attr_id,error)

        nlen = 3
        call h5tset_size_f(atype_id, nlen, error)
        call h5acreate_f(dset_id, "model_version", atype_id, aspace_id, attr_id, error)
        call h5awrite_f(attr_id, atype_id, dsm2_version, a_dims, error)
        call h5aclose_f(attr_id,error)

        nlen = len_trim(cinterval)
        call h5tset_size_f(atype_id, nlen, error)
	    call h5acreate_f(dset_id, "interval",                   &
                         atype_id, aspace_id, attr_id, error)
	    call h5awrite_f(attr_id, atype_id,                      &
                        cinterval, a_dims, error)
	    call h5aclose_f(attr_id,error)

        call h5tclose_f(atype_id,error)
	    call h5sclose_f(aspace_id,error)

        return
	end subroutine

end module