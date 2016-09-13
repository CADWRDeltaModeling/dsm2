!<license>
!    Copyright (C) 2015 State of California,
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
      use process_gtm_groups
      use process_gtm_output_channel
      use process_gtm_output_reservoir
      use process_gtm_suspended_sediment
      use time_utilities
      
      implicit none
      integer :: nitem
      integer :: nitem_chan, nitem_resv
      character(len=32) :: name
      character(len=64) :: value     
      character(len=32) :: envname
      character(len=128) :: envval
      character(len=128) :: filename, iofile
      character(len=8) :: model,filetype,io
      character(len=16) :: interval
      character(len=128) :: hydro_tidefile
      character(len=16) :: sdate, edate
      !--output channel
      character(len=8) :: distance
      character(len=16) :: variable, perop
      integer :: idistance, chan_length
      integer :: channo   
      !--output reservoir
      character(len=32) :: res_name
      character(len=8) :: node_str
      integer :: node, resvno   
      !--suspended sediment
      character(len=16) :: method
      character(len=16) :: composition
      real(gtm_real) :: grain_size, percent
      
      integer :: icount      
      integer :: ierror = 0

      character(len=32) :: groupname, sourcegroup
      character(len=16) :: member_type
      character(len=256) :: pattern
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

      ! process suspended sediment table
      nitem = suspended_sediment_type_buffer_size()
      allocate(sediment(nitem))
      do icount = 1, nitem  
          call suspended_sediment_type_query_from_buffer(icount, composition, ierror)
          call process_suspended_sediment_type(composition)
      enddo
      print *,"Number of sediments: ", nitem         
      
      nitem = suspended_sediment_boundary_buffer_size()
      allocate(sediment_bc(nitem))
      do icount = 1, nitem  
          call suspended_sediment_boundary_query_from_buffer(icount, name, composition, percent, ierror)
          call process_suspended_sediment_boundary(name, composition, percent)
      enddo
      
      ! process output channel block
      nitem_chan = output_channel_buffer_size()
      nitem_resv = output_reservoir_buffer_size()
      allocate(pathoutput(nitem_chan+nitem_resv))
      nitem = output_channel_buffer_size()
      do icount = 1,nitem
          call output_channel_query_from_buffer(icount, name, channo, distance, variable,  &
                                                interval, perop, filename,ierror)
          sourcegroup = ""
          call locase(distance)
          if (distance(:6) .eq. "length") then 
             idistance = LARGEINT
          else 
             read(distance,'(i)') idistance
          end if
          call process_output_channel(name, channo, idistance, variable, interval, &
                                      perop, sourcegroup, filename)
      end do
      print *,"Number of channel output requests: ", nitem      

      ! process output reservoir block
      nitem = output_reservoir_buffer_size()
      do icount = 1,nitem
          call output_reservoir_query_from_buffer(icount, name, res_name, node_str, variable,  &
                                                  interval, perop, filename, ierror)                                               
          !sourcegroup = ""
          !if (node_str .eq. "none") then
          !    node = miss_val_i
          !else
          !    node = miss_val_i
          !    read(node_str,'(i)') node
          !end if 
          call process_output_reservoir(res_name, resvno, node, variable, interval, &
                                        perop, sourcegroup, filename)
      end do
      print *,"Number of reservoir output requests: ", nitem    

      ! process group block
      nitem = group_buffer_size()
      n_group = nitem
      
      call allocate_group
      
      do icount = 1,nitem
         call group_query_from_buffer(icount, name, ierror)
         call process_gtm_group(name, icount)
      end do
      print *,"Number of groups processed: ", nitem

      ! process group_member block
      nitem = group_member_buffer_size()
      n_group_member = nitem
      allocate(group_member(n_group_member))
      do icount = 1,nitem
         call group_member_query_from_buffer(icount,         &
                                             groupname,      &
                                             member_type,    &
                                             pattern,        &
                                             ierror)
         call obj_type_to_code(obj_type, member_type)
         call  process_gtm_group_member(groupname,           &
                                        obj_type,            &
                                        pattern,             &
                                        icount)
      end do
      print *,"Number of group members processed: ", nitem
      call process_members_detail
      !! convert group members from patterns to actual objects&indexes
      !! This must come after tidefile is loaded
      !call ConvertGroupPatternsToMembers

      call get_npartition_t(npartition_t, hydro_time_interval, gtm_time_interval)

      allocate(outdssfiles(n_outdssfiles))
      outdssfiles = outfilenames
      allocate(ifltab_out(600, n_outdssfiles))

      return
    end subroutine
    
    !> Convert member pattern name to obj code
    subroutine obj_type_to_code(obj_type,     &
                                member_type)
        use common_variables
        implicit none
        integer, intent(out) :: obj_type
        character*16, intent(in) :: member_type
        if (trim(member_type).eq."channel") then
            obj_type = 1
        elseif(trim(member_type).eq."node") then
            obj_type = 2
        elseif(trim(member_type).eq."reservoir") then
            obj_type = 3
        elseif(trim(member_type).eq."gate") then
            obj_type = 4
        elseif(trim(member_type).eq."qext") then
            obj_type = 5
        elseif(trim(member_type).eq."transfer") then
            obj_type = 6
        elseif(trim(member_type).eq."stage") then
            obj_type = 7
        elseif(trim(member_type).eq."flow_boundary") then
            obj_type = 8
        elseif(trim(member_type).eq."source_sink") then
            obj_type = 9
        elseif(trim(member_type).eq."flux") then
            obj_type = 10
        elseif(trim(member_type).eq."group") then
            obj_type = 11                                                               
        elseif(trim(member_type).eq."climate") then
            obj_type = 12   
        elseif(trim(member_type).eq."oprule") then
            obj_type = 13   
        elseif(trim(member_type).eq."filter") then
            obj_type = 14   
        else
            obj_type = 99  
        end if
        return
    end subroutine
    
    !> Routine to fill in gtm_io(:,:)
    subroutine fill_gtm_io_type()
        use common_dsm2_vars
        use common_variables
        use error_handling
        implicit none
        logical :: file_exists
        !(1) 1:restart, 2:echo, 3:hdf (2) 1:in, 2:out
        gtm_io(1,1)%filename = io_files(4,1,1)%filename
        gtm_io(1,2)%filename = io_files(4,1,2)%filename 
        gtm_io(2,2)%filename = io_files(4,2,2)%filename
        gtm_io(3,2)%filename = io_files(4,7,2)%filename
        gtm_io(3,2)%interval = io_files(4,7,2)%interval
        
        if (len_trim(gtm_io(1,1)%filename)==0) then
            inquire(file=gtm_io(1,1)%filename, exist=file_exists)
            if (file_exists==.false.) then 
                call gtm_fatal(trim(gtm_io(1,1)%filename)//" does not exist!!")
            endif             
        end if    
        return
    end subroutine    
      
end module      