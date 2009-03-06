program test_parser
use hdf5
use input_storage_fortran
implicit none
integer :: nchan, nxsect
integer :: error
character(len=18), PARAMETER :: filename="fortran_example.h5" ! File name
integer(HID_T) :: file_id                            ! File identifier
logical :: ext


call clear_all_buffers()
call init_file_reader()


! Read, collect and process the "ENVVAR" section used for 
! text substitution
call set_substitution_enabled(.false.)    ! don't try to substitute now
call set_active_profile("ENVVAR")        ! read only ENVVAR blocks
call read_buffer_from_text("example.txt") ! read starting from this file

!
! process the results
!
call process_text_substitution("example.txt")
call set_substitution_enabled(.true.)    ! substitute now
! clear the buffer so that envvars are not loaded redundantly 
call envvar_clear_buffer()


!
! set the active profile to "all". Now all items will be read.
!
call set_active_profile("all")
call read_buffer_from_text("example.txt")
!
! check for redundancies and prioritize according to source file
!
call channel_prioritize_buffer()
call xsect_prioritize_buffer()

!
! do something with the data...
!
nchan = channel_buffer_size()
print *,"Number of channels: ", nchan

! inquire(file=filename, exist=ext)   ! this was for cygwin
! if (ext)then
! call unlink(filename,error)
! end if

! Now write it out to hdf5
call h5open_f (error)
call h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)
if (error .ne. 0) then
   print*,"Could not open file, hdf error: ", error
   print*,"Check if it already exists and delete if so -- failure to replace seems to be an HDF5 bug"
   call exit(2)
end if
print*,"invert"

error= xsect_write_buffer_to_hdf5(file_id)
error= channel_write_buffer_to_hdf5(file_id)
error= envvar_write_buffer_to_hdf5(file_id)
call channel_write_buffer_to_text("example_fortran_out.txt",.true.)

!
!    Close file and  FORTRAN interface.
!
call h5fclose_f(file_id, error)
print *, "file close status: ", error
call h5close_f(error)
print*, "hdf5 shutdown status: ", error



end program
