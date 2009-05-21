
      subroutine buffer_input_tidefile()      
      use input_storage_fortran
      use constants
      use io_units
      
      implicit none
      integer :: nitem
      integer :: icount
      character*128 iofile
      integer :: ierror = 0

      character*(16) :: sdate,edate
      integer*4, external :: obj_type_code


      nitem = tidefile_buffer_size()
      do icount = 1,nitem
         call tidefile_query_from_buffer(icount,sdate,edate,iofile,ierror)
         call process_tidefile(sdate,edate,iofile)
      end do
      print *,"Number of tidefiles: ", nitem

      end subroutine