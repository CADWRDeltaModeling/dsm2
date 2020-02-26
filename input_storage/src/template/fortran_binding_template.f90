       !>@ingroup fortran
       !>@defgroup @TABLEOBJ @TABLEOBJ
       !>@{
        
       !> Clear the buffer of all stored data of type @TABLEOBJ 
       subroutine @TABLEOBJ_clear_buffer()
         !DEC$ IF DEFINED(_WIN32)
         !DEC$ ATTRIBUTES ALIAS:'_@TABLEOBJ_clear_buffer_f' :: @TABLEOBJ_clear_buffer_f
	 !DEC$ ELSE
         !DEC$ ATTRIBUTES ALIAS:'@TABLEOBJ_clear_buffer_f' :: @TABLEOBJ_clear_buffer_f
	 !DEC$ END IF
         implicit none
         call @TABLEOBJ_clear_buffer_f()
         return
       end subroutine

       !> Query the number of records in storage buffer of type @TABLEOBJ
       integer function @TABLEOBJ_buffer_size()
         !DEC$ IF DEFINED(_WIN32)
         !DEC$ ATTRIBUTES ALIAS:'_@TABLEOBJ_buffer_size_f' :: @TABLEOBJ_buffer_size_f
         !DEC$ ELSE
         !DEC$ ATTRIBUTES ALIAS:'@TABLEOBJ_buffer_size_f' :: @TABLEOBJ_buffer_size_f
         !DEC$ END IF
          implicit none
          integer, external :: @TABLEOBJ_buffer_size_f
          @TABLEOBJ_buffer_size = @TABLEOBJ_buffer_size_f()
          return
       end function
       
       !> write @TABLEOBJ data to buffer
       subroutine @TABLEOBJ_append_to_buffer(@FORTRAN_SIGNATURE,ierror)
         !DEC$ IF DEFINED(_WIN32)
         !DEC$ ATTRIBUTES ALIAS:'_@TABLEOBJ_append_to_buffer_f' :: @TABLEOBJ_append_to_buffer_f
         !DEC$ ELSE
         !DEC$ ATTRIBUTES ALIAS:'@TABLEOBJ_append_to_buffer_f' :: @TABLEOBJ_append_to_buffer_f
         !DEC$ END IF
         implicit none
         integer, intent(out) :: ierror         
         @FORTRAN_DECL_IN
         call @TABLEOBJ_append_to_buffer_f(@FORTRAN_SIGNATURE,ierror)
         return
       end subroutine
       
       !> Query the number of records in hdf5 of type @TABLEOBJ
       subroutine @TABLEOBJ_number_rows_hdf5(file_id, nrecords,ierror)
         !DEC$ IF DEFINED(_WIN32)
         !DEC$ ATTRIBUTES ALIAS:'_@TABLEOBJ_number_rows_hdf5_f' :: @TABLEOBJ_number_rows_hdf5_f
         !DEC$ ELSE
         !DEC$ ATTRIBUTES ALIAS:'@TABLEOBJ_number_rows_hdf5_f' :: @TABLEOBJ_number_rows_hdf5_f
         !DEC$ END IF
          use hdf5, only: HID_T,HSIZE_T
          implicit none
          integer :: nrecords
          integer(HSIZE_T) :: nrec = 0
          integer(HID_T),intent(in)::file_id
          integer, intent(out) :: ierror
          call @TABLEOBJ_number_rows_hdf5_f(file_id,nrec,ierror)
          nrecords = nrec
          return
       end subroutine



       !> Query row of data from buffer of type @TABLEOBJ
       subroutine @TABLEOBJ_query_from_buffer(row, @FORTRAN_SIGNATURE,ierror)
         !DEC$ IF DEFINED(_WIN32)
         !DEC$ ATTRIBUTES ALIAS:'_@TABLEOBJ_query_from_buffer_f' :: @TABLEOBJ_query_from_buffer_f
         !DEC$ ELSE
         !DEC$ ATTRIBUTES ALIAS:'@TABLEOBJ_query_from_buffer_f' :: @TABLEOBJ_query_from_buffer_f
         !DEC$ END IF
          implicit none
          @FORTRAN_DECL_OUT
          integer row
          integer, intent(out) :: ierror
          call @TABLEOBJ_query_from_buffer_f(row, &
                    @FORTRAN_SIGNATURE,ierror)
          return
       end subroutine

       !> Prioritize buffer by layers, delete unused items and sort
       subroutine @TABLEOBJ_prioritize_buffer(ierror)
         !DEC$ IF DEFINED(_WIN32)
         !DEC$ ATTRIBUTES ALIAS:'_@TABLEOBJ_prioritize_buffer_f' :: @TABLEOBJ_prioritize_buffer_f
         !DEC$ ELSE
         !DEC$ ATTRIBUTES ALIAS:'@TABLEOBJ_prioritize_buffer_f' :: @TABLEOBJ_prioritize_buffer_f
         !DEC$ END IF
	      implicit none
          integer, intent(out) :: ierror          
          call @TABLEOBJ_prioritize_buffer_f(ierror)
       end subroutine

       !> Write a buffer of @TABLEOBJ data to buffer 
       !! @param file_id handle to hdf5 file
       subroutine @TABLEOBJ_write_buffer_to_hdf5(file_id, ierror)
         !DEC$ IF DEFINED(_WIN32)
         !DEC$ ATTRIBUTES ALIAS:'_@TABLEOBJ_write_buffer_to_hdf5_f' :: @TABLEOBJ_write_buffer_to_hdf5_f
         !DEC$ ELSE
         !DEC$ ATTRIBUTES ALIAS:'@TABLEOBJ_write_buffer_to_hdf5_f' :: @TABLEOBJ_write_buffer_to_hdf5_f
         !DEC$ END IF
         use hdf5, only: HID_T
         implicit none
         integer(HID_T), intent(in) :: file_id
         integer, intent(out) :: ierror         
         call @TABLEOBJ_write_buffer_to_hdf5_f(file_id,ierror)
         return
       end subroutine

       !> Read a buffer of @TABLEOBJ data from hdf5 to buffer 
       subroutine @TABLEOBJ_read_buffer_from_hdf5(file_id,ierror)
         !DEC$ IF DEFINED(_WIN32)
         !DEC$ ATTRIBUTES ALIAS:'_@TABLEOBJ_read_buffer_from_hdf5_f' :: @TABLEOBJ_read_buffer_from_hdf5_f
         !DEC$ ELSE
         !DEC$ ATTRIBUTES ALIAS:'@TABLEOBJ_read_buffer_from_hdf5_f' :: @TABLEOBJ_read_buffer_from_hdf5_f
         !DEC$ END IF
         use hdf5, only: HID_T
         implicit none
         integer(HID_T), intent(in) :: file_id
         integer, intent(out) :: ierror         
         call @TABLEOBJ_read_buffer_from_hdf5_f(file_id,ierror)
         return
       end subroutine

       subroutine @TABLEOBJ_write_buffer_to_text(filename,append,ierror)
         !DEC$ IF DEFINED(_WIN32)
         !DEC$ ATTRIBUTES ALIAS:'_@TABLEOBJ_write_buffer_to_text_f' :: @TABLEOBJ_write_buffer_to_text_f
         !DEC$ ELSE
         !DEC$ ATTRIBUTES ALIAS:'@TABLEOBJ_write_buffer_to_text_f' :: @TABLEOBJ_write_buffer_to_text_f
         !DEC$ END IF
         implicit none
         integer, intent(out) :: ierror
         character*(*) filename
         logical append
         call @TABLEOBJ_write_buffer_to_text_f(filename, append,ierror)
         return
       end subroutine     
       !@}



