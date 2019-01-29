program test_parser
use hdf5
use input_storage_fortran
implicit none
integer :: nchan, nxsect
integer :: error
character(len=15), PARAMETER :: filename = "fortran_test.h5" ! File name
integer(HID_T) :: file_id                            ! File identifier
logical :: ext


call init_file_reader(error)
call clear_all_buffers(error)
call init_file_reader(error)

! Read, collect and process the "ENVVAR" section used for 
! text substitution
call set_user_substitution_enabled(.false.,error)    ! don't try to substitute now
call set_active_profile("ENVVAR",error)        ! read only ENVVAR blocks
call read_buffer_from_text("example.txt",error) ! read starting from this file

call process_text_substitution(error)
! so that envvars are not loaded redundantly 
call envvar_clear_buffer()

call set_active_profile("all",error)
call set_user_substitution_enabled(.true.,error)    ! substitute now
call read_buffer_from_text("test.txt",error)

call channel_prioritize_buffer(error)
call xsect_prioritize_buffer(error)

nchan = channel_buffer_size()
print *,"Number of channels: ", nchan

inquire(file=filename, exist=ext)
if (ext)then
 call unlink(filename,error)
end if

call h5open_f (error)
call h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)
if (error .ne. 0) then
   print*,"Could not open file, hdf error: ", error
   print*,"Check if it already exists and delete if so -- failure to replace seems to be an HDF5 bug"
   call exit(2)
end if
print*,"invert"

call xsect_write_buffer_to_hdf5(file_id,error)
call channel_write_buffer_to_hdf5(file_id,error)
call envvar_write_buffer_to_hdf5(file_id,error)
call channel_write_buffer_to_text("testout.txt",.true.,error)

!
!    Close file and  FORTRAN interface.
!
call h5fclose_f(file_id, error)
print *, "file close status: ", error
call h5close_f(error)
print*, "hdf5 shutdown status: ", error



end program
