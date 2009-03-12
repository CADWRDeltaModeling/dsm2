
c=======================================================================
      subroutine input_text(filename)
!     Read in all text starting from input filename
      use hdf5
      use input_storage_fortran
      use envvar
      use runtime_data
      implicit none
      integer :: ierror = 0
      character*(*) filename
      call clear_all_buffers(ierror)


      call init_file_reader(ierror)
      call set_initial_context_profile(dsm2_name)


c-----Do a first pass reading the input, activating only ENVVARS for use in later text substitution
      call init_file_reader(ierror)                   ! Prepare for a read of everything
      call set_substitution_enabled(.false.,ierror)   ! don't try to substitute now      
      call set_active_profile("envvar",ierror)        ! read only ENVVAR blocks
      call verify_error(ierror,"Error setting active profile")
      call read_buffer_from_text(filename,ierror)            ! read starting from this file
      call verify_error(ierror,"Error reading from text (envvar pass)")
      !
      ! process the results
      !
      call process_text_substitution(ierror)
      call set_substitution_enabled(.true.,ierror)    ! substitute now
      ! clear the buffer so that envvars are not loaded redundantly 

      call envvar_clear_buffer()                  ! Clear the envvar buffer
      print*,"Read and processed text substitution (ENVVARS), reading all data from text"

c-----Do a second pass on all the input, making use of the text substitution we just prepped
      call set_active_profile(dsm2_name,ierror)          ! activate all keywords for the model
      call read_buffer_from_text(filename,ierror)        ! Perform the read into buffers
      call verify_error(ierror,"Error reading from text (full pass)")
      print*,"Read text into buffers"
      print*,"No of layers=",xsect_layer_buffer_size()
      call prioritize_all_buffers(ierror)                ! Enforce the "layering"
      call verify_error(ierror,"Error prioritizing buffers, sorting layers")
      print*,"Prioritized buffer"
 
      return
      end subroutine
c==================================================================

      subroutine write_input_buffers()
!     Writes in all text starting from input filename
      use hdf5
      use input_storage_fortran
      use runtime_data
      use envvar
      implicit none
      character(LEN=32) :: hdf_filename
      integer(HID_T) :: file_id
      integer :: ierror = 0
      logical, parameter :: append_text=.TRUE.
      logical :: ext
c-----Write all buffers to text in the order they were defined
      call write_all_buffers_to_text("testout.txt",append_text,ierror)
      call verify_error(ierror,"Error writing echoed text")
      print*, "text written"

c-----Write all buffers to hdf5
      ! for the moment we are deleting and recreating the file, but this is an open question
      write(hdf_filename,"(a,'_echo.h5')")dsm2_name
      inquire(file=hdf_filename, exist=ext)
      if (ext)then
      call unlink(hdf_filename,ierror)
      end if

      call h5open_f (ierror)
      call h5fcreate_f(hdf_filename, H5F_ACC_TRUNC_F, file_id, ierror)
      if (ierror .ne. 0) then      
      print*,"Could not open file, hdf error: ", ierror
      print*,"Check if it already exists and delete if so -- failure to replace seems to be an HDF5 bug"
      call exit(2)
      end if

      call write_all_buffers_to_hdf5(file_id,ierror)  ! Do the actual write
      call verify_error(ierror,"Error writing echoed input to hdf5")
      call h5fclose_f(file_id, ierror)                 ! Close down ! todo: this is too verbose
      print *, "file close status: ", ierror
      call h5close_f(ierror)
      print*, "hdf5 shutdown status: ", ierror

 
      return
      end subroutine



c====================================================================
      subroutine process_initial_text
      
      use hdf5
      use input_storage_fortran
      use envvar
      implicit none
      integer :: nitem
      integer :: icount
      character*(32) name,value
      character*(32) envname
      character*(128) envval
      integer :: ierror = 0
      nitem = envvar_buffer_size()
      do icount = 1,nitem
           call envvar_query_from_buffer(icount,envname,envval,ierror)
           call add_envvar(envname,envval)
      end do
      print *,"Number of envvar: ", nitem

      nitem = scalar_buffer_size()
      do icount = 1,nitem
           call scalar_query_from_buffer(icount,name,value,ierror)
           call process_scalar(name,value)
      end do
      print *,"Number of scalars: ", nitem
       
      return
      end subroutine



      subroutine verify_error(ierror,message)
      use io_units
      use constants
      implicit none
      integer,intent(in) :: ierror
      character(len=*),intent(in) :: message
      if (ierror .eq. 0) return
      write(unit_error,"(/,a,/,'[FATAL]',1x,i)") message,ierror
      call exit(ierror)
      end subroutine

