      subroutine read_grid_from_tidefile()
      use hdf5
      use common_tide
      use input_storage_fortran
      use runtime_data
      implicit none
      
      character(len=11) :: group_name = "hydro", subgroup_name = "input"
      integer(HID_T) :: file_id 
      integer(HID_T) :: group_id, subgroup_id
      integer :: ierror = 0


      call h5open_f (ierror)
      call h5fopen_f(trim(tide_files(1).filename), H5F_ACC_RDONLY_F, file_id, ierror)
      call h5gopen_f (file_id, "hydro",    group_id,   ierror)  ! open group instead of create
	call h5gopen_f(group_id, "input", subgroup_id, ierror)
      
      call read_buffer_profile_from_hdf5("Grid",subgroup_id,ierror)  ! Do the actual read
      
      call verify_error(ierror,"Error writing echoed input to hdf5")
      call h5gclose_f (subgroup_id, ierror)
      call h5gclose_f (group_id, ierror)
      call h5fclose_f(file_id,ierror)
      return
      end subroutine