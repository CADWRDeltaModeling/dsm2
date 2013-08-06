!<license>
!    Copyright (C) 2013 State of California,
!    Department of Water Resources.
!    This file is part of DSM2-GTM.
!
!    The Delta Simulation Model 2 (DSM2) - General Transport Model (GTM) 
!    is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    DSM2 is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
!</license>

!>@ingroup process_io
module buffer_gtm_input_common

    contains
    
    !> Process blocks (ENVVAR, SCALAR, IO_FILES, TIDEFILE) and variables in common_dsm2_vars
    subroutine buffer_input_common()      
      use input_storage_fortran
      !use groups, only:convertgrouppatternstomembers
      use common_variables
      use common_dsm2_vars
      use process_gtm_scalar
      use process_gtm_io_file
      use process_gtm_tidefile
      use time_utilities
      
      implicit none
      integer :: nitem
      character(len=32) :: name
      character(len=64) :: value     
      character(len=32) :: envname
      character(len=128) :: envval
      character(len=128) :: filename, iofile
      character(len=8) :: model,filetype,io
      character(len=16) :: interval
      character(len=128) :: hydro_tidefile
      character(len=16) :: sdate, edate
      integer :: icount      
      integer :: ierror = 0

      character*32 groupname
      character*16 member_type
      character*32 pattern
      integer*4 obj_type
      integer*4, external :: obj_type_code

      print*,"Parsing input text file..."
      
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
      call fill_gtm_io_type()
      print *,"Number of iofiles: ", nitem

      ! process tidefile - process_gtm_tidefile() has been modified for single tidefile input only. 
      nitem = tidefile_buffer_size()
      do icount = 1, nitem
          call tidefile_query_from_buffer(icount, sdate, edate, hydro_tidefile, ierror)
          call process_tidefile(hydro_tidefile)
      enddo
      print *,"Number of tidefiles: ", nitem    

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

      call get_npartition_t(npartition_t, hydro_time_interval, gtm_time_interval)

      return
    end subroutine
    
    !> Routine to fill in gtm_io(:,:)
    subroutine fill_gtm_io_type()
        use common_dsm2_vars
        use common_variables
        implicit none
        !(1) 1:restart, 2:echo, 3:hdf (2) 1:in, 2:out
        gtm_io(1,2)%filename = io_files(4,1,2)%filename 
        gtm_io(2,2)%filename = io_files(4,2,2)%filename
        gtm_io(3,2)%filename = io_files(4,7,2)%filename
        gtm_io(3,2)%interval = io_files(4,7,2)%interval
        return
    end subroutine    
      
end module      