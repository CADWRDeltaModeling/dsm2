C!<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    The Delta Simulation Model 2 (DSM2) is free software: 
C!    you can redistribute it and/or modify
C!    it under the terms of the GNU General Public License as published by
C!    the Free Software Foundation, either version 3 of the License, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public License for more details.

C!    You should have received a copy of the GNU General Public License
C!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
C!</license>
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

      call init_file_reader(ierror)        ! Prepare for a read of everything
      call set_initial_context_profile(dsm2_name)


c-----Do a first pass reading the input, activating only ENVVARS for use in later text substitution
      call set_user_substitution_enabled(.false.,ierror)   ! don't try to substitute now      
      call set_substitution_not_found_is_error(.false.,ierror)
      call set_active_profile("envvar",ierror)        ! read only ENVVAR blocks
      call verify_error(ierror,"Error setting active profile")
      call read_buffer_from_text(filename,ierror)            ! read starting from this file
      call verify_error(ierror,"Error reading from text (envvar pass)")
      !
      ! process the results
      !
      call process_text_substitution(ierror)
      call set_user_substitution_enabled(.true.,ierror)    ! substitute now
      call set_substitution_not_found_is_error(.true.,ierror)
      ! clear the buffer so that envvars are not loaded redundantly 

      call clear_all_buffers(ierror)          ! Clear the envvar buffer
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
      use input_storage_fortran
      use iopath_data
      use runtime_data
      use envvar
      implicit none
      integer :: ierror = 0
      logical :: append_text=.false.
c-----Write all buffers to text in the order they were defined
      if (io_files(dsm2_module,io_echo,io_write).use)then
      append_text=.false.
      call write_buffer_profile_to_text(trim(dsm2_name),
     &                                  io_files(dsm2_module,
     &                                          io_echo,
     &                                          io_write).filename,
     &                                  append_text,ierror)
      call verify_error(ierror,"Error writing echoed text")
      print*, "text written"
      end if

      return
      end subroutine
c============================================================
      subroutine write_input_buffers_hdf5(loc_id)
!     Writes in all text starting from input filename
      use hdf5
      use input_storage_fortran
      use iopath_data
      use runtime_data
      use envvar
      implicit none
      character(len=5) :: group_name = "input"
      integer(HID_T) :: loc_id 
      integer(HID_T) :: group_id

      integer :: ierror = 0

c-----Write all buffers to hdf5
      call h5gcreate_f (loc_id, group_name, group_id, ierror)
      call write_buffer_profile_to_hdf5(dsm2_name,group_id,ierror)  ! Do the actual write
      call verify_error(ierror,"Error writing echoed input to hdf5")
      call h5gclose_f (group_id, ierror)
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
      character*(32) name
      character*(64) value
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

