
c=======================================================================
      subroutine input_text(filename)
!     Read in all text starting from input filename
      use hdf5
      use input_storage_fortran
      use envvar

      implicit none
      character*(*) filename

c-----Do a first pass reading the input, activating only ENVVARS for use in later text substitution
      call clear_all_buffers()
      !todo: testing whether this is optional
      call init_text_substitution("PARAMETER")    ! Get ready for the pass
      call process_text_substitution(filename)    ! Do the reading and prep the environment variables
      call envvar_clear_buffer()                  ! Clear the envvar buffer
      print*,"Read and processed text substitution (ENVVARS)"

c-----Do a second pass on all the input, making use of the text substitution we just prepped
      call init_file_reader()                     ! Prepare for a read of everything
      call read_buffer_from_text(filename)        ! Perform the read into buffers
      print*,"Read text into buffers"
      print*,"No of layers=",xsect_layer_buffer_size()
      call prioritize_all_buffers()               ! Enforce the "layering"
      print*,"Prioritized buffer"
 
      return
      end subroutine
c==================================================================

      subroutine write_input_buffers()
!     Writes in all text starting from input filename
      use hdf5
      use input_storage_fortran
      use envvar
      implicit none
      character(LEN=7),parameter :: hdf_filename = "echo.h5" 
      integer(HID_T) :: file_id
      integer :: error
      logical, parameter :: append_text=.TRUE.
      logical :: ext
c-----Write all buffers to text in the order they were defined
      call write_all_buffers_to_text("testout.txt",append_text)
      print*, "text written"

c-----Write all buffers to hdf5
      ! for the moment we are deleting and recreating the file, but this is an open question
      inquire(file=hdf_filename, exist=ext)
      if (ext)then
      call unlink(hdf_filename,error)
      end if

      call h5open_f (error)
      call h5fcreate_f(hdf_filename, H5F_ACC_TRUNC_F, file_id, error)
      if (error .ne. 0) then
      print*,"Could not open file, hdf error: ", error
      print*,"Check if it already exists and delete if so -- failure to replace seems to be an HDF5 bug"
      call exit(2)
      end if

      call write_all_buffers_to_hdf5(file_id)      ! Do the actual write

      call h5fclose_f(file_id, error)              ! Close down ! todo: this is too verbose
      print *, "file close status: ", error
      call h5close_f(error)
      print*, "hdf5 shutdown status: ", error

 
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
      integer err
      nitem = envvar_buffer_size()
      do icount = 1,nitem
           err=envvar_query_from_buffer(icount,envname,envval)
           call add_envvar(envname,envval)
      end do
      print *,"Number of envvar: ", nitem

      nitem = scalar_buffer_size()
      do icount = 1,nitem
           err=scalar_query_from_buffer(icount,name,value)
           call process_scalar(name,value)
      end do
      print *,"Number of scalars: ", nitem
       
      return
      end subroutine






