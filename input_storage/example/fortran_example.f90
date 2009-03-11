program test_parser
use hdf5
use input_storage_fortran
implicit none
integer :: nchan, nxsect
integer :: error
character(len=18), PARAMETER :: filename="fortran_example.h5" ! File name
integer(HID_T) :: file_id                            ! File identifier
logical :: ext
integer :: ierror = 0


call clear_all_buffers(ierror)
call init_file_reader(ierror)


! Read, collect and process the "ENVVAR" section used for 
! text substitution
call set_substitution_enabled(.false.,ierror)    ! don't try to substitute now

print*, "Setting active profile to envvar"
call set_active_profile("envvar",ierror)         ! read only ENVVAR blocks
call verify_error(ierror)

print*, "Reading from text, envvar only"
call read_buffer_from_text("example.txt",ierror) ! read starting from this file
call verify_error(ierror)
!
! process the results
!
print*,"Processing text substitution"
call process_text_substitution(ierror)
call verify_error(ierror)

print*,"Enable text substitution"
call set_substitution_enabled(.true.,ierror)  ! substitute now
! clear the buffer so that envvars are not loaded redundantly 
call envvar_clear_buffer()


!
! set the active profile to "all". Now all items will be read.
!
print*,"Reading all items"
call set_active_profile("all", ierror)
call read_buffer_from_text("example.txt",ierror)
call verify_error(ierror)
!
! check for redundancies and prioritize according to source file
!
print*,"Prioritize buffer"
call channel_prioritize_buffer(ierror)
call verify_error(ierror)
call xsect_prioritize_buffer(ierror)
call verify_error(ierror)

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

call xsect_write_buffer_to_hdf5(file_id,ierror)
call channel_write_buffer_to_hdf5(file_id,ierror)
call envvar_write_buffer_to_hdf5(file_id,ierror)
call channel_write_buffer_to_text("example_fortran_out.txt",.true.,ierror)

!
!    Close file and  FORTRAN interface.
!
call h5fclose_f(file_id, error)
print *, "file close status: ", error
call h5close_f(error)
print*, "hdf5 shutdown status: ", error



end program

subroutine verify_error(error)
implicit none
integer error
if (error .ne. 0)then
  write(*,*)"Input storage error"
  call exit(error)
end if
end subroutine