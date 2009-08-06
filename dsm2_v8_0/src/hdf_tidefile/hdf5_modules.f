	module inclvars
	  use type_defs
	  ! Module that loads include variables from DSM2
        include '../hydrolib/network.inc'
        include '../hydrolib/chconnec.inc'
        include '../hydrolib/chnlcomp.inc'
        include '../hydrolib/chstatus.inc'
        include '../hydrolib/chcxtbl.inc'
        include '../timevar/dss.inc'
        include '../timevar/readdss.inc'
	end module inclvars

***********************************************************************
***********************************************************************

	module hdfvars
				! Module that contains shared variables for HDF5 writing
	use hdf5
	integer		   :: hdf5point	! index representing time step
	integer		   :: hdf5length ! time length of file
      integer(HID_T),save :: file_id ! HDF5 File ID
      integer(HID_T),save :: hydro_id ! Group identifier 
      integer(HID_T),save :: geom_id ! Group identifier 
      integer(HID_T),save :: data_id ! Group identifier 
	integer(HID_T) :: in_dset_id ! int2ext Dataset identifier 

      integer(HSIZE_T), dimension(1) :: in_dims != (/0/) ! Dataset dimensions
	integer(HID_T) :: cg_dset_id ! Channel Geometry Dataset identifier 
      integer(HSIZE_T), dimension(2) :: cg_dims = (/0,0/) ! Dataset dimensions
	integer(HID_T) :: ng_dset_id ! Node Geometry Dataset identifier 
      integer(HSIZE_T), dimension(2) :: ng_dims = (/0,0/) ! Dataset dimensions
	integer(HID_T) :: rg_dset_id ! Reservoir Geometry Dataset identifier 
      integer(HSIZE_T), dimension(2) :: rg_dims = (/0,0/) ! Dataset dimensions
	integer(HID_T) :: bname_dset_id	! Boundary Name Dataset identifier 
      integer(HSIZE_T), dimension(1) :: bname_dims = (/0/) ! Dataset dimensions
	integer(HID_T) :: bnode_dset_id	! Boundary Node Dataset identifier 
      integer(HSIZE_T), dimension(1) :: bnode_dims = (/0/) ! Dataset dimensions

	integer(HSIZE_T),parameter :: chanFlowIdx = 0
	integer(HSIZE_T),parameter :: chanStageIdx = 2
	integer(HSIZE_T),parameter :: chanXSectIdx = 4
	integer(HSIZE_T),parameter :: chanAvgXSectIdx = 6
	
	integer(HSIZE_T) :: resHeightIdx = 0
	integer(HSIZE_T) :: resFlowIdx = 1 ! Variable dimension

	integer(HSIZE_T) :: int2extIdx = 0
	integer(HSIZE_T) :: bottom_elIdx = 0 ! Variable dimension

	character(LEN=150) :: hdf5_hydrofile ! HDF5
	integer :: h5_time_start
	integer :: h5_time_interval
	logical :: h5_file_exists = .FALSE.

      integer,parameter :: HDF_SZIP_PIXELS_PER_BLOCK = 16
      integer,parameter :: TIME_CHUNK = HDF_SZIP_PIXELS_PER_BLOCK
	integer,parameter :: MIN_STEPS_FOR_CHUNKING = TIME_CHUNK

	integer(HID_T) :: t_filespace ! Dataspace identifier 
	integer(HID_T) :: t_memspace ! memspace identifier 


	integer(HID_T) :: chan_z_dset_id ! Channel File Dataset identifier
	integer(HID_T) :: chan_z_fspace_id ! Dataspace identifier 
	integer(HID_T) :: chan_q_dset_id ! Channel File Dataset identifier
	integer(HID_T) :: chan_q_fspace_id ! Dataspace identifier
	integer(HID_T) :: chan_a_dset_id ! Channel File Dataset identifier
	integer(HID_T) :: chan_a_fspace_id ! Dataspace identifier
	integer(HID_T) :: chan_aa_dset_id ! Channel File Dataset identifier
	integer(HID_T) :: chan_aa_fspace_id ! Dataspace identifier

	integer :: chan_aa_mdata_rank = 1 ! Single channel variable in memory for one time step
	integer :: chan_a_mdata_rank = 2 ! Single channel variable in memory for one time step
	integer :: chan_q_mdata_rank = 2 ! Single channel variable in memory for one time step
	integer :: chan_z_mdata_rank = 2 ! Single channel variable in memory for one time step
	integer :: chan_aa_fdata_rank = 2 ! Channel dataset in file is [time x variable x chan]
	integer :: chan_z_fdata_rank = 3 ! Channel dataset in file is [time x variable x chan]
	integer :: chan_a_fdata_rank = 3 ! Channel dataset in file is [time x variable x chan]
	integer :: chan_q_fdata_rank = 3 ! Channel dataset in file is [time x variable x chan]
	integer(HSIZE_T), dimension(1) :: chan_aa_mdata_dims  = 0  ! Chan data in memory
	integer(HSIZE_T), dimension(2) :: chan_a_mdata_dims  = 0  ! Chan data in memory
	integer(HSIZE_T), dimension(2) :: chan_z_mdata_dims  = 0  ! Chan data in memory
	integer(HSIZE_T), dimension(2) :: chan_q_mdata_dims  = 0  ! Chan data in memory
      integer(HSIZE_T), dimension(2) :: chan_aa_fdata_dims = 0  ! Chan data in file
      integer(HSIZE_T), dimension(2) :: chan_aa_fsubset_dims = 0  ! Chan data in file
      integer(HSIZE_T), dimension(3) :: chan_a_fdata_dims = 0  ! Chan data in file
      integer(HSIZE_T), dimension(3) :: chan_a_fsubset_dims = 0  ! Chan data in file
      integer(HSIZE_T), dimension(3) :: chan_z_fdata_dims = 0  ! Chan data in file
      integer(HSIZE_T), dimension(3) :: chan_z_fsubset_dims = 0  ! Chan data in file
      integer(HSIZE_T), dimension(3) :: chan_q_fdata_dims = 0  ! Chan data in file
      integer(HSIZE_T), dimension(3) :: chan_q_fsubset_dims = 0  ! Chan data in file 
	integer(HID_T) :: chan_aa_memspace ! Memspace identifier
	integer(HID_T) :: chan_a_memspace ! Memspace identifier	   
	integer(HID_T) :: chan_z_memspace ! Memspace identifier
	integer(HID_T) :: chan_q_memspace ! Memspace identifier


	integer(HID_T) :: res_h_dset_id = 0   ! Reservoir Dataset identifier
	integer(HID_T) :: res_q_dset_id = 0   ! Reservoir Dataset identifier 
	integer(HID_T) :: res_h_fspace_id = 0 ! Dataspace identifier
	integer(HID_T) :: res_q_fspace_id = 0 ! Dataspace identifier	 
	integer(HID_T) :: res_h_memspace  = 0 ! memspace identifier for height
	integer(HID_T) :: res_q_memspace  = 0 ! memspace identifier for flow
	integer,parameter :: res_h_fdata_rank = 2 ! time by nres
	integer,parameter :: res_q_fdata_rank = 3 ! time by nres by nconnect
	integer,parameter :: res_h_mdata_rank = 1  ! values for one time step for just height
	integer,parameter :: res_q_mdata_rank = 2  ! values for one time step for flow (qres(i,j))
	integer(HSIZE_T), dimension(res_h_fdata_rank) :: res_h_fdata_dims = (/0,0/) !fixme
	integer(HSIZE_T), dimension(res_q_fdata_rank) :: res_q_fdata_dims = (/0,0,0/) !fixme
	integer(HSIZE_T), dimension(res_h_fdata_rank) :: res_h_fsubset_dims = (/0,0/) !fixme
	integer(HSIZE_T), dimension(res_q_fdata_rank) :: res_q_fsubset_dims = (/0,0,0/) !fixme
	integer(HSIZE_T), dimension(res_h_mdata_rank) :: res_h_mdata_dims = (/0/)
	integer(HSIZE_T), dimension(res_q_mdata_rank) :: res_q_mdata_dims = (/0,0/)


	integer(HID_T) :: qext_change_dset_id ! QExt change Dataset identifier 
	integer(HID_T) :: qext_fspace_id ! Dataspace identifier 
	integer(HID_T) :: qext_memspace ! memspace identifier for height
	integer,parameter :: qext_fdata_rank = 2   ! time by scalar (ave/diff of qext)
	integer,parameter :: qext_mdata_rank = 1 ! values for one time step
	integer(HSIZE_T), dimension(qext_fdata_rank) :: qext_fdata_dims = 0
	integer(HSIZE_T), dimension(qext_fdata_rank) :: qext_fsubset_dims = 0
	integer(HSIZE_T), dimension(qext_mdata_rank) :: qext_mdata_dims = 0


	integer(HID_T) :: transfer_dset_id ! obj2obj changed Dataset identifier 
	integer(HID_T) :: transfer_fspace_id ! Dataspace identifier 
	integer(HID_T) :: transfer_memspace ! memspace identifier for height
	integer,parameter :: transfer_fdata_rank = 2    ! scalar by time
	integer,parameter :: transfer_mdata_rank = 1  ! values for one time step
	integer(HSIZE_T), dimension(transfer_fdata_rank) :: 
     &                   transfer_fdata_dims = 0 
	integer(HSIZE_T), dimension(transfer_fdata_rank) :: 
     &                   transfer_fsubset_dims = 0 
	integer(HSIZE_T), dimension(transfer_mdata_rank) :: 
     &                   transfer_mdata_dims = 0

	end module hdfvars

***********************************************************************
***********************************************************************

	module objvars
				! Module that contains shared variables for Obj2Obj HDF5 writing
	use HDF5

        integer(HID_T) :: obj2obj_dset_id ! Dataset identifier 
        integer(HID_T) :: obj_plist_id ! Dataset trasfer property
	
        integer(HID_T) :: obj2obj_type_id ! Compound datatype identifier
        integer(HID_T) :: o_name_tid ! Memory datatype identifier 
        integer(HID_T) :: fo_id_tid ! Memory datatype identifier 
        integer(HID_T) :: fo_name_tid ! Memory datatype identifier 
        integer(HID_T) :: fo_num_tid ! Memory datatype identifier 
        integer(HID_T) :: fo_hychan_tid	! Memory datatype identifier 
        integer(HID_T) :: fo_accname_tid ! Memory datatype identifier 
        integer(HID_T) :: fo_accidx_tid	! Memory datatype identifier 
        integer(HID_T) :: fo_massfrac_tid ! Memory datatype identifier 
        integer(HID_T) :: fo_coeff_tid ! Memory datatype identifier 
        integer(HID_T) :: to_id_tid ! Memory datatype identifier 
        integer(HID_T) :: to_name_tid ! Memory datatype identifier 
        integer(HID_T) :: to_num_tid ! Memory datatype identifier 
        integer(HID_T) :: to_hychan_tid	! Memory datatype identifier 
        integer(HID_T) :: to_accname_tid ! Memory datatype identifier 
        integer(HID_T) :: to_accidx_tid	! Memory datatype identifier 
        integer(HID_T) :: to_massfrac_tid ! Memory datatype identifier 
        integer(HID_T) :: to_coeff_tid ! Memory datatype identifier 
        integer(HID_T) :: constval_tid ! Memory datatype identifier 
        integer(HID_T) :: datasrc_type_tid ! Memory datatype identifier 
        integer(HID_T) :: datasrc_idx_tid ! Memory datatype identifier 
        integer(HID_T) :: datasrc_val_tid ! Memory datatype identifier 
        integer(HID_T) :: curQ_tid ! Memory datatype identifier 
        integer(HID_T) :: prevQ_tid ! Memory datatype identifier 
        integer(HID_T) :: avgQ_tid ! Memory datatype identifier 
        integer(HID_T) :: conc_tid ! Memory datatype identifier 

        integer(SIZE_T) :: type_size ! Size of the datatype
        integer(SIZE_T) :: type_sizec10 ! Size of the character*10 datatype 
        integer(SIZE_T) :: type_sizec32 ! Size of the character*32 datatype 
        integer(SIZE_T) :: type_sizei ! Size of the integer datatype
        integer(SIZE_T) :: type_sizer ! Size of the real datatype

	end module objvars

***********************************************************************
***********************************************************************

	module qextvars
				! Module that contains shared variables for Obj2Obj HDF5 writing
	use HDF5

        integer(HID_T) :: qext_dset_id ! Dataset identifier 
        integer(HID_T) :: qext_plist_id	! Dataset trasfer property
	
        integer(HID_T) :: qext_type_id ! Compound datatype identifier
        integer(HID_T) :: q_name_tid ! Memory datatype identifier 
        integer(HID_T) :: q_flow_tid ! Memory datatype identifier 
        integer(HID_T) :: q_prev_flow_tid ! Memory datatype identifier 
        integer(HID_T) :: q_avg_tid ! Memory datatype identifier 
        integer(HID_T) :: q_prev_avg_tid ! Memory datatype identifier
        integer(HID_T) :: q_dsrc_type_tid ! Memory datatype identifier 
        integer(HID_T) :: q_dsrc_idx_tid ! Memory datatype identifier 
        integer(HID_T) :: q_dsrc_val_tid ! Memory datatype identifier 
        integer(HID_T) :: q_chng_idx_tid ! Memory datatype identifier 
        integer(HID_T) :: q_obj_name_tid ! Memory datatype identifier 
        integer(HID_T) :: q_attach_id_tid ! Memory datatype identifier 
        integer(HID_T) :: q_attach_name_tid ! Memory datatype identifier 
        integer(HID_T) :: q_attach_num_tid ! Memory datatype identifier 
        integer(HID_T) :: q_grp_idx_tid	! Memory datatype identifier 
        integer(HID_T) :: q_mass_frac_tid ! Memory datatype identifier 

        integer(SIZE_T) :: type_size ! Size of the datatype
        integer(SIZE_T) :: type_sizec10 ! Size of the character*10 datatype 
        integer(SIZE_T) :: type_sizec32 ! Size of the character*32 datatype 
        integer(SIZE_T) :: type_sizei ! Size of the integer datatype
        integer(SIZE_T) :: type_sizer ! Size of the real datatype

	end module qextvars



