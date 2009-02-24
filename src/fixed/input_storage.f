      subroutine input_text(filename)
      use hdf5
      use input_storage_fortran
      use envvar
      implicit none
      integer :: nitem
      integer :: error
      character*(*) filename
      character(LEN=7),parameter :: hdf_filename = "echo.h5" 
      integer(HID_T) :: file_id
      logical :: ext
      integer :: icount
      character*(16) :: sdate,edate
      character*(32) name,value
      character*(32) envname
      character*(128) envval
      character*8,model,filetype,io
      character*16 interval
      character*128 iofile
      integer err
      logical, parameter :: append_text=.TRUE.

      call clear_all_buffers()
      
      !todo: testing whether this is optional
      call init_text_substitution("INCLUDE")
      call process_text_substitution(filename)
      
      !todo: this is annoyint to have to clear
      call envvar_clear_buffer()
      call init_file_reader()
      call read_buffer_from_text(filename)
      call prioritize_all_buffers()


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

      nitem = io_file_buffer_size()
      do icount = 1,nitem
         err=io_file_query_from_buffer(icount,model,filetype,io,interval,iofile)
         call process_io_file(model,filetype,io,interval,iofile)
      end do
      print *,"Number of iofiles: ", nitem


      nitem = tidefile_buffer_size()
      do icount = 1,nitem
         err=tidefile_query_from_buffer(icount,sdate,edate,iofile)
         call process_tidefile(model,sdate,edate,iofile)
      end do
      print *,"Number of tidefiles: ", nitem


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

      error= envvar_write_buffer_to_hdf5(file_id)
      error= scalar_write_buffer_to_hdf5(file_id)
      error= io_file_write_buffer_to_hdf5(file_id) 
      !error= tidefile_write_buffer_to_hdf5(file_id) !todo: need to handle the empty case


      call h5fclose_f(file_id, error)
      print *, "file close status: ", error
      call h5close_f(error)
      print*, "hdf5 shutdown status: ", error

      call envvar_write_buffer_to_text("testout.txt",.false.)
      call scalar_write_buffer_to_text("testout.txt",append_text)
      call io_file_write_buffer_to_text("testout.txt",append_text)
      call tidefile_write_buffer_to_text("testout.txt",append_text)
      print*, "text written"
 
      return
      end subroutine



