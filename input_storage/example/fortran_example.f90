program test_parser
use hdf5
use input_storage_fortran_data
implicit none
integer :: nchan, nxsect
integer :: error
character(len=18), PARAMETER :: filename="fortran_example.h5" ! File name
integer(HID_T) :: file_id                            ! File identifier
logical :: ext


call clear_all_buffers()

call init_text_substitution("INCLUDE")
call process_text_substitution("example.txt")
call envvar_clear_buffer()


call init_file_reader()

call read_buffer_from_text("example.txt")

call channel_prioritize_buffer()
call xsect_prioritize_buffer()

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

error= xsect_write_buffer_to_hdf5(file_id)
error= channel_write_buffer_to_hdf5(file_id)
error= envvar_write_buffer_to_hdf5(file_id)
call channel_write_buffer_to_text("testout.txt",.true.)

!
!    Close file and  FORTRAN interface.
!
    call h5fclose_f(file_id, error)
    print *, "file close status: ", error
    call h5close_f(error)
    print*, "hdf5 shutdown status: ", error



end program
