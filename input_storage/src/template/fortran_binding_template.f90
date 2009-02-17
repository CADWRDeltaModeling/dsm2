       !>@ingroup fortran
       !>@defgroup @TABLEOBJ @TABLEOBJ
       !>@{
        
       !> write @TABLEOBJ data to buffer
       subroutine @TABLEOBJ_append_to_buffer(@FORTRAN_SIGNATURE)
         !DEC$ ATTRIBUTES ALIAS:'_@TABLEOBJ_append_to_buffer_f' :: @TABLEOBJ_append_to_buffer_f
         implicit none
         @FORTRAN_DECL_IN
         call @TABLEOBJ_append_to_buffer_f(@FORTRAN_SIGNATURE)
         return
       end subroutine

       !> Clear the buffer of all stored data of type @TABLEOBJ 
       subroutine @TABLEOBJ_clear_buffer()
         !DEC$ ATTRIBUTES ALIAS:'_@TABLEOBJ_clear_buffer_f' :: @TABLEOBJ_clear_buffer_f
         implicit none
         call @TABLEOBJ_clear_buffer_f()
         return
       end subroutine


       !> Query the number of records in hdf5 of type @TABLEOBJ
       integer function @TABLEOBJ_number_rows_hdf5(file_id, nrecords)
         !DEC$ ATTRIBUTES ALIAS:'_@TABLEOBJ_number_rows_hdf5_f' :: @TABLEOBJ_number_rows_hdf5_f
          use hdf5, only: HID_T
          implicit none
          integer :: nrecords
          integer, external :: @TABLEOBJ_number_rows_hdf5_f
          integer(HID_T),intent(in)::file_id
          @TABLEOBJ_number_rows_hdf5 = & 
              @TABLEOBJ_number_rows_hdf5_f(file_id,nrecords)
          return
       end function

       !> Query the number of records in storage buffer of type @TABLEOBJ
       integer function @TABLEOBJ_buffer_size()
         !DEC$ ATTRIBUTES ALIAS:'_@TABLEOBJ_buffer_size_f' :: @TABLEOBJ_buffer_size_f
          implicit none
          integer, external :: @TABLEOBJ_buffer_size_f
          @TABLEOBJ_buffer_size = @TABLEOBJ_buffer_size_f()
          return
       end function

       !> Query row of data from buffer of type @TABLEOBJ
       integer function @TABLEOBJ_query_from_buffer(row, @FORTRAN_SIGNATURE)
         !DEC$ ATTRIBUTES ALIAS:'_@TABLEOBJ_query_from_buffer_f' :: @TABLEOBJ_query_from_buffer_f
          implicit none
          @FORTRAN_DECL_OUT
          integer row
          integer, external :: @TABLEOBJ_query_from_buffer_f
          @TABLEOBJ_query_from_buffer= @TABLEOBJ_query_from_buffer_f(row, &
                    @FORTRAN_SIGNATURE)
          return
       end function

       !> Prioritize buffer by layers, delete unused items and sort
       subroutine @TABLEOBJ_prioritize_buffer
         !DEC$ ATTRIBUTES ALIAS:'_@TABLEOBJ_prioritize_buffer_f' :: @TABLEOBJ_prioritize_buffer_f
	      implicit none
          call @TABLEOBJ_prioritize_buffer_f
       end subroutine

       !> Write a buffer of @TABLEOBJ data to buffer 
       !! @param file_id handle to hdf5 file
       integer function @TABLEOBJ_write_buffer_to_hdf5(file_id)
         !DEC$ ATTRIBUTES ALIAS:'_@TABLEOBJ_write_buffer_to_hdf5_f' :: @TABLEOBJ_write_buffer_to_hdf5_f
         use hdf5, only: HID_T
         implicit none
         integer(HID_T), intent(in) :: file_id
         integer, external :: @TABLEOBJ_write_buffer_to_hdf5_f
         @TABLEOBJ_write_buffer_to_hdf5= &
            @TABLEOBJ_write_buffer_to_hdf5_f(file_id)
         return
       end function

       !> Read a buffer of @TABLEOBJ data from hdf5 to buffer 
       integer function @TABLEOBJ_read_buffer_from_hdf5(file_id)
         !DEC$ ATTRIBUTES ALIAS:'_@TABLEOBJ_read_buffer_from_hdf5_f' :: @TABLEOBJ_read_buffer_from_hdf5_f
         use hdf5, only: HID_T
         implicit none
         integer(HID_T), intent(in) :: file_id
         integer, external :: @TABLEOBJ_read_buffer_from_hdf5_f
         @TABLEOBJ_read_buffer_from_hdf5=@TABLEOBJ_read_buffer_from_hdf5_f(file_id)
         return
       end function

       subroutine @TABLEOBJ_write_buffer_to_text(filename,append)
         !DEC$ ATTRIBUTES ALIAS:'_@TABLEOBJ_write_buffer_to_text_f' :: @TABLEOBJ_write_buffer_to_text_f
         implicit none
         character*(*) filename
         logical append
         call @TABLEOBJ_write_buffer_to_text_f(filename, append)
         return
       end subroutine     
       !@}



