!<license>
!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
!    Department of Water Resources.
!    This file is part of DSM2.

!    The Delta Simulation Model 2 (DSM2) is free software: 
!    you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.

!    DSM2 is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.

!    You should have received a copy of the GNU General Public License
!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
!</license>

module buffer_gtm_input_common

    contains
    
    !> 
    subroutine buffer_input_common()      
      use input_storage_fortran
      !use groups, only:convertgrouppatternstomembers
      use common_dsm2_vars
      use process_gtm_scalar
      use process_gtm_io_file
      
      implicit none
      integer :: nitem
      character*(32) name
      character*(64) value     
      character*(32) envname
      character*(128) envval
      character*(128) filename
      integer :: icount
      character*8,model,filetype,io
      character*16 interval
      character*128 iofile
      integer :: ierror = 0

      character*32 groupname
      character*16 member_type
      character*32 pattern
      integer*4 obj_type
      integer*4, external :: obj_type_code

      ! process envvar block
      nitem = envvar_buffer_size()
      do icount = 1,nitem
           call envvar_query_from_buffer(icount, envname, envval, ierror)
           call add_envvar(envname,envval)
      end do
      print *,"Number of envvar: ", nitem
      
      ! process scalar block
      nitem = scalar_buffer_size()
      do icount = 1,nitem
           call scalar_query_from_buffer(icount,name,value,ierror)
           call process_scalar(name,value)
      end do
      print *,"Number of scalars: ", nitem
      
      ! process io_file block
      nitem = io_file_buffer_size()
      do icount = 1,nitem
         call io_file_query_from_buffer(icount, model, filetype, io, interval, iofile, ierror)
         call process_io_file(model, filetype, io, interval, iofile)
      end do
      print *,"Number of iofiles: ", nitem

      ! process tidefile
      ! todo: do we want to work with multi tidefiles?

      ! process group block
      !nitem = group_buffer_size()
      !do icount = 1,nitem
      !   call group_query_from_buffer(icount, name, ierror)
      !   call  process_group(name, icount)
      !end do
      !print *,"Number of groups processed: ", nitem

      ! process group_member block
      !nitem = group_member_buffer_size()
      !do icount = 1,nitem
      !   call group_member_query_from_buffer(icount,         &
      !                                       groupname,      &
      !                                       member_type,    &
      !                                       pattern,        &
      !                                       ierror)
      !   obj_type = obj_type_code(member_type)
      !   call  process_group_member(groupname,               &
      !                              obj_type,                &
      !                              pattern)
      !end do
      !print *,"Number of group members processed: ", nitem
      !! convert group members from patterns to actual objects&indexes
      !! This must come after tidefile is loaded
      !call ConvertGroupPatternsToMembers

      return
    end subroutine
      
end module      